#' CORE-OBS-001: MDV and DV consistency
#'
#' Flags records where MDV and DV disagree: MDV = 0 (not missing) but DV is
#' missing, or an observation (EVID 0) marked MDV = 1 yet carrying a non-zero
#' DV.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_obs_mdv_dv <- function(data, thresholds) {
  evid <- suppressWarnings(as.numeric(data$EVID))
  mdv <- suppressWarnings(as.numeric(data$MDV))
  dv <- suppressWarnings(as.numeric(data$DV))

  bad_missing <- mdv == 0 & is.na(dv)
  bad_observed <- mdv == 1 & evid == 0 & !is.na(dv) & dv != 0
  flagged <- which(bad_missing | bad_observed)

  if (length(flagged) == 0) {
    return(result_pass("MDV and DV are consistent."))
  }
  result_flag(
    message = paste0(length(flagged), " record(s) with inconsistent MDV/DV."),
    n_flagged = length(flagged),
    flagged_records = tibble::as_tibble(data[flagged, , drop = FALSE])
  )
}

#' CORE-OBS-004: Predose concentration descriptive review
#'
#' Flags quantifiable predose observations (TIME <= 0) whose concentration is
#' above the dataset median quantifiable concentration, a possible carryover or
#' timing signal for review.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @importFrom stats median
#' @noRd
check_obs_predose <- function(data, thresholds) {
  time <- suppressWarnings(as.numeric(data$TIME))
  evid <- suppressWarnings(as.numeric(data$EVID))
  mdv <- suppressWarnings(as.numeric(data$MDV))
  dv <- suppressWarnings(as.numeric(data$DV))

  quant <- evid == 0 & mdv == 0 & !is.na(dv) & dv > 0
  predose <- which(quant & !is.na(time) & time <= 0)
  if (length(predose) == 0) {
    return(result_pass("No quantifiable predose concentrations."))
  }
  med <- median(dv[quant])
  flagged <- predose[dv[predose] > med]
  if (length(flagged) == 0) {
    return(result_pass("Predose concentrations are not unusually high."))
  }
  result_flag(
    message = paste0(
      length(flagged), " predose record(s) with relatively high concentration."
    ),
    n_flagged = length(flagged),
    flagged_records = tibble::as_tibble(data[flagged, , drop = FALSE])
  )
}

#' CORE-OBS-005: Concentration outlier detection
#'
#' Flags quantifiable observations whose log-concentration is a robust
#' (MAD-based) outlier, using `thresholds$outlier_nmad`.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list; uses `outlier_nmad`.
#' @return A partial check result.
#' @noRd
check_obs_outliers <- function(data, thresholds) {
  evid <- suppressWarnings(as.numeric(data$EVID))
  mdv <- suppressWarnings(as.numeric(data$MDV))
  dv <- suppressWarnings(as.numeric(data$DV))

  obs <- which(evid == 0 & mdv == 0 & !is.na(dv) & dv > 0)
  if (length(obs) < 3) {
    return(result_skip(
      "Too few quantifiable observations for outlier detection."
    ))
  }

  flagged <- obs[robust_outliers(log(dv[obs]), thresholds$outlier_nmad)]
  if (length(flagged) == 0) {
    return(result_pass("No concentration outliers detected."))
  }
  result_flag(
    message = paste0(
      length(flagged), " concentration outlier(s) (robust |z| > ",
      thresholds$outlier_nmad, ")."
    ),
    n_flagged = length(flagged),
    flagged_records = tibble::as_tibble(data[flagged, , drop = FALSE]),
    subject_list = tibble::tibble(ID = unique(data$ID[flagged]))
  )
}

#' CORE-OBS-002: BLQ/CENS/LLOQ internal consistency
#'
#' When a BLQ or censoring flag is present, flags records marked below the limit
#' of quantification whose DV is nonetheless above the reported LLOQ.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_obs_blq_consistency <- function(data, thresholds) {
  flag_col <- detect_col(names(data), "^BLQ|^CENS$")
  if (is.na(flag_col)) {
    return(result_skip(
      "No BLQ/CENS variable present; covered by MDV/DV checks."
    ))
  }
  is_blq <- is_positive_flag(data[[flag_col]])
  if (!any(is_blq)) {
    return(result_pass("No records are flagged BLQ/censored."))
  }
  lloq_col <- detect_col(names(data), "LLOQ")
  if (is.na(lloq_col)) {
    return(result_pass(
      paste0(sum(is_blq), " BLQ record(s); no LLOQ variable to cross-check.")
    ))
  }
  dv <- suppressWarnings(as.numeric(data$DV))
  lloq <- suppressWarnings(as.numeric(data[[lloq_col]]))
  flagged <- which(is_blq & !is.na(dv) & !is.na(lloq) & dv > lloq)
  if (length(flagged) == 0) {
    return(result_pass("BLQ records are consistent with the reported LLOQ."))
  }
  result_flag(
    message = paste0(length(flagged), " BLQ record(s) with DV above LLOQ."),
    n_flagged = length(flagged),
    flagged_records = tibble::as_tibble(data[flagged, , drop = FALSE])
  )
}

#' CORE-OBS-003: BLQ in the middle of a profile
#'
#' Flags BLQ/censored observations that have quantifiable observations both
#' before and after them in time within the same subject.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_obs_blq_middle <- function(data, thresholds) {
  flag_col <- detect_col(names(data), "^BLQ|^CENS$")
  if (is.na(flag_col)) {
    return(result_skip("No BLQ/CENS variable present."))
  }
  time <- suppressWarnings(as.numeric(data$TIME))
  evid <- suppressWarnings(as.numeric(data$EVID))
  is_blq <- is_positive_flag(data[[flag_col]])

  flagged <- integer(0)
  for (idx in split(seq_len(nrow(data)), data$ID)) {
    obs <- idx[evid[idx] == 0]
    obs <- obs[order(time[obs])]
    if (length(obs) < 3) next
    quant <- !is_blq[obs]
    for (i in seq_along(obs)) {
      before <- if (i > 1) any(quant[seq_len(i - 1)]) else FALSE
      after <- if (i < length(obs)) any(quant[(i + 1):length(obs)]) else FALSE
      if (is_blq[obs[i]] && before && after) {
        flagged <- c(flagged, obs[i])
      }
    }
  }
  if (length(flagged) == 0) {
    return(result_pass("No BLQ records surrounded by quantifiable records."))
  }
  result_flag(
    message = paste0(
      length(flagged), " BLQ record(s) in the middle of a profile."
    ),
    n_flagged = length(flagged),
    flagged_records = tibble::as_tibble(data[sort(flagged), , drop = FALSE])
  )
}

#' CORE-OBS-006: Minimum quantifiable PK records per subject or occasion
#'
#' Flags subjects (or subject-occasions) with fewer than
#' `thresholds$min_quantifiable_n` quantifiable observations.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list; uses `min_quantifiable_n`.
#' @return A partial check result.
#' @noRd
check_obs_min_records <- function(data, thresholds) {
  evid <- suppressWarnings(as.numeric(data$EVID))
  mdv <- suppressWarnings(as.numeric(data$MDV))
  dv <- suppressWarnings(as.numeric(data$DV))
  quant <- evid == 0 & mdv == 0 & !is.na(dv)
  grp <- if ("OCC" %in% names(data)) {
    paste(data$ID, data$OCC, sep = "_")
  } else {
    as.character(data$ID)
  }

  counts <- tapply(quant, grp, sum)
  n <- thresholds$min_quantifiable_n
  bad <- names(counts)[counts < n]
  if (length(bad) == 0) {
    return(result_pass(paste0(
      "All subjects/occasions have at least ", n, " quantifiable records."
    )))
  }
  result_flag(
    message = paste0(
      length(bad), " subject/occasion(s) with fewer than ", n,
      " quantifiable records."
    ),
    n_flagged = length(bad),
    subject_list = tibble::tibble(
      group = bad, n_quantifiable = as.integer(counts[bad])
    )
  )
}
