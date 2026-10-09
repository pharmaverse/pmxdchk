#' CORE-OBS-001: MDV and DV consistency
#'
#' Flags records where MDV and DV disagree: MDV = 0 (not missing) but DV is
#' missing, an observation (EVID 0) marked MDV = 1 yet carrying a non-zero DV,
#' and dose records (EVID 1 or 4) that carry a non-zero DV or MDV = 0.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_obs_mdv_dv <- function(data, thresholds) {
  evid <- suppressWarnings(as.numeric(data$EVID))
  mdv <- suppressWarnings(as.numeric(data$MDV))
  dv <- suppressWarnings(as.numeric(data$DV))
  is_dose <- evid %in% c(1, 4)
  has_dv <- !is.na(dv) & dv != 0

  issue <- rep(NA_character_, nrow(data))
  issue[which(mdv == 1 & evid == 0 & has_dv)] <- "MDV = 1 with a DV value"
  issue[which(is_dose & mdv == 0)] <- "dose record with MDV = 0"
  issue[which(is_dose & has_dv)] <- "dose record with a DV value"
  issue[which(mdv == 0 & is.na(dv))] <- "MDV = 0 with missing DV"
  flagged <- which(!is.na(issue))

  if (length(flagged) == 0) {
    return(result_pass("MDV and DV are consistent."))
  }
  records <- tibble::as_tibble(data[flagged, , drop = FALSE])
  records$issue <- issue[flagged]
  result_flag(
    message = paste0(
      length(flagged), " record(s) with inconsistent MDV/DV: ",
      paste(unique(issue[flagged]), collapse = "; "), "."
    ),
    n_flagged = length(flagged),
    flagged_records = records
  )
}

#' CORE-OBS-004: Predose concentration descriptive review
#'
#' Flags quantifiable observations taken at or before the subject's first dose
#' (TIME <= 0 for subjects without a dose record) whose concentration exceeds
#' `thresholds$predose_cmax_frac` of that subject's highest post-dose
#' concentration for the same analyte / compartment.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list; uses `predose_cmax_frac`.
#' @return A partial check result.
#' @noRd
check_obs_predose <- function(data, thresholds) {
  time <- suppressWarnings(as.numeric(data$TIME))
  evid <- suppressWarnings(as.numeric(data$EVID))
  mdv <- suppressWarnings(as.numeric(data$MDV))
  dv <- suppressWarnings(as.numeric(data$DV))
  id <- as.character(data$ID)

  is_dose <- evid %in% c(1, 4) & !is.na(time)
  first_dose <- tapply(time[is_dose], id[is_dose], min)[id]
  first_dose[is.na(first_dose)] <- 0
  quant <- which(evid == 0 & mdv == 0 & !is.na(dv) & dv > 0 & !is.na(time))
  predose <- quant[time[quant] <= first_dose[quant]]
  if (length(predose) == 0) {
    return(result_pass("No quantifiable predose concentrations."))
  }

  key <- paste(id, obs_group(data))
  post <- setdiff(quant, predose)
  cmax <- tapply(dv[post], key[post], max)[key[predose]]
  ratio <- dv[predose] / as.numeric(cmax)
  hit <- which(ratio > thresholds$predose_cmax_frac)
  if (length(hit) == 0) {
    return(result_pass(paste0(
      "No predose concentration above ",
      100 * thresholds$predose_cmax_frac, "% of the subject's Cmax."
    )))
  }
  records <- tibble::as_tibble(data[predose[hit], , drop = FALSE])
  records$pct_of_cmax <- round(100 * ratio[hit], 1)
  result_flag(
    message = paste0(
      length(hit), " predose record(s) above ",
      100 * thresholds$predose_cmax_frac, "% of the subject's Cmax."
    ),
    n_flagged = length(hit),
    flagged_records = records
  )
}

#' CORE-OBS-005: Concentration outlier detection
#'
#' Flags quantifiable observations whose log-concentration is a robust
#' (MAD-based) outlier among observations of the same analyte / compartment and
#' nominal time (analyte / compartment only when no nominal time is mapped),
#' using `thresholds$outlier_nmad`. Groups with fewer than five observations are
#' not assessed.
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
  if (length(obs) < 5) {
    return(result_skip(
      "Too few quantifiable observations for outlier detection."
    ))
  }

  by_time <- "NTIME" %in% names(data)
  grp <- obs_group(data)
  if (by_time) {
    grp <- paste(grp, data$NTIME)
  }
  flagged <- unlist(lapply(split(obs, grp[obs]), function(i) {
    if (length(i) < 5) {
      return(integer())
    }
    i[robust_outliers(log(dv[i]), thresholds$outlier_nmad)]
  }), use.names = FALSE)
  flagged <- sort(flagged)

  scope <- if (by_time) "their nominal time point" else "the dataset"
  if (length(flagged) == 0) {
    return(result_pass(paste0(
      "No concentration outliers relative to ", scope, "."
    )))
  }
  result_flag(
    message = paste0(
      length(flagged), " concentration outlier(s) relative to ", scope,
      " (robust |z| > ", thresholds$outlier_nmad, ")."
    ),
    n_flagged = length(flagged),
    flagged_records = tibble::as_tibble(data[flagged, , drop = FALSE]),
    subject_list = tibble::tibble(ID = unique(data$ID[flagged]))
  )
}

#' CORE-OBS-002: BLQ/CENS/LLOQ internal consistency
#'
#' When a BLQ or censoring flag and an LLOQ variable are present, flags records
#' marked below the limit of quantification whose DV is above the LLOQ, and
#' unmarked observations with MDV = 0 whose DV is below it.
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
  lloq_col <- detect_col(names(data), "LLOQ")
  if (is.na(lloq_col)) {
    return(result_pass(
      paste0(sum(is_blq), " BLQ record(s); no LLOQ variable to cross-check.")
    ))
  }
  dv <- suppressWarnings(as.numeric(data$DV))
  mdv <- suppressWarnings(as.numeric(data$MDV))
  lloq <- suppressWarnings(as.numeric(data[[lloq_col]]))

  issue <- rep(NA_character_, nrow(data))
  issue[which(is_blq & dv > lloq)] <- "marked BLQ but DV above LLOQ"
  issue[
    which(!is_blq & mdv == 0 & dv < lloq)
  ] <- "DV below LLOQ but not marked BLQ"
  flagged <- which(!is.na(issue))
  if (length(flagged) == 0) {
    return(result_pass("BLQ records are consistent with the reported LLOQ."))
  }
  records <- tibble::as_tibble(data[flagged, , drop = FALSE])
  records$issue <- issue[flagged]
  result_flag(
    message = paste0(
      length(flagged), " record(s) where ", flag_col, ", DV, and ", lloq_col,
      " disagree."
    ),
    n_flagged = length(flagged),
    flagged_records = records
  )
}

#' CORE-OBS-003: BLQ in the middle of a profile
#'
#' Flags BLQ/censored observations that have quantifiable observations both
#' before and after them within the same subject, analyte / compartment, and
#' dosing interval. A BLQ trough followed by the next dose is therefore not
#' flagged.
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
  evid <- if ("EVID" %in% names(data)) {
    suppressWarnings(as.numeric(data$EVID))
  } else {
    rep(0, nrow(data))
  }
  is_blq <- is_positive_flag(data[[flag_col]])
  grp <- obs_group(data)

  flagged <- integer(0)
  for (idx in split(seq_len(nrow(data)), data$ID)) {
    idx <- idx[order(time[idx])]
    interval <- cumsum(evid[idx] %in% c(1, 4))
    is_obs <- evid[idx] %in% 0
    key <- paste(grp[idx], interval)[is_obs]
    for (obs in split(idx[is_obs], key)) {
      quant <- which(!is_blq[obs])
      if (length(quant) < 2) next
      inside <- seq_along(obs) > min(quant) & seq_along(obs) < max(quant)
      flagged <- c(flagged, obs[inside & is_blq[obs]])
    }
  }
  if (length(flagged) == 0) {
    return(result_pass("No BLQ records surrounded by quantifiable records."))
  }
  result_flag(
    message = paste0(
      length(flagged),
      " BLQ record(s) between quantifiable records in a dosing interval."
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
  quant <- evid %in% 0 & mdv %in% 0 & !is.na(dv)
  keys <- intersect(c("ID", "OCC"), names(data))

  counts <- unique(data[, keys, drop = FALSE])
  grp <- do.call(paste, c(data[, keys, drop = FALSE], sep = "_"))
  n_quant <- tapply(quant, grp, sum)
  counts$n_quantifiable <- as.integer(
    n_quant[do.call(paste, c(counts[, keys, drop = FALSE], sep = "_"))]
  )
  n <- thresholds$min_quantifiable_n
  bad <- counts[counts$n_quantifiable < n, ]
  if (nrow(bad) == 0) {
    return(result_pass(paste0(
      "All subjects/occasions have at least ", n, " quantifiable records."
    )))
  }
  result_flag(
    message = paste0(
      nrow(bad), " subject/occasion(s) with fewer than ", n,
      " quantifiable records."
    ),
    n_flagged = nrow(bad),
    subject_list = tibble::as_tibble(bad)
  )
}
