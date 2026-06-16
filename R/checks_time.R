#' CORE-TIME-001: TIME non-decreasing within subject
#'
#' Within each subject (and occasion when present), flags records where TIME
#' decreases relative to the previous record in dataset row order.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_time_nondecreasing <- function(data, thresholds) {
  time <- suppressWarnings(as.numeric(data$TIME))
  grp <- if ("OCC" %in% names(data)) {
    paste(data$ID, data$OCC, sep = "_")
  } else {
    as.character(data$ID)
  }

  flagged <- unlist(lapply(split(seq_along(time), grp), function(idx) {
    t <- time[idx]
    idx[c(FALSE, diff(t) < 0)]
  }), use.names = FALSE)
  flagged <- sort(flagged)

  if (length(flagged) == 0) {
    return(result_pass("TIME is non-decreasing within each subject."))
  }
  result_flag(
    message = paste0(
      length(flagged), " record(s) where TIME decreases within a subject."
    ),
    n_flagged = length(flagged),
    flagged_records = tibble::as_tibble(data[flagged, , drop = FALSE])
  )
}

#' CORE-TIME-002: Same-time event pattern review
#'
#' Flags multiple dose records (EVID 1 or 4) that share the same subject and
#' TIME, a pattern that usually warrants review.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_time_same_time_events <- function(data, thresholds) {
  time <- suppressWarnings(as.numeric(data$TIME))
  evid <- suppressWarnings(as.numeric(data$EVID))
  key <- paste(data$ID, time, sep = "_")
  is_dose <- evid %in% c(1, 4)

  doses_per_key <- tapply(is_dose, key, sum)
  bad_keys <- names(doses_per_key)[doses_per_key > 1]
  flagged <- which(key %in% bad_keys & is_dose)

  if (length(flagged) == 0) {
    return(result_pass("No multiple dose records share a subject-time."))
  }
  result_flag(
    message = paste0(
      length(flagged), " dose record(s) share a subject-time with another dose."
    ),
    n_flagged = length(flagged),
    flagged_records = tibble::as_tibble(data[flagged, , drop = FALSE])
  )
}

#' CORE-TIME-003: Negative TIME review
#'
#' Flags negative TIME on dose records (EVID 1 or 4), which is suspicious.
#' Negative TIME on observations is common for predose samples, so it is
#' reported descriptively rather than flagged.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_time_negative <- function(data, thresholds) {
  time <- suppressWarnings(as.numeric(data$TIME))
  evid <- suppressWarnings(as.numeric(data$EVID))
  neg <- !is.na(time) & time < 0
  dose_neg <- which(neg & evid %in% c(1, 4))
  obs_neg <- sum(neg & !(evid %in% c(1, 4)))

  if (length(dose_neg) == 0) {
    if (sum(neg) == 0) {
      return(result_pass("No negative TIME values."))
    }
    return(result_pass(paste0(
      sum(neg), " negative TIME record(s), all on non-dose events ",
      "(often valid predose)."
    )))
  }
  result_flag(
    message = paste0(
      length(dose_neg), " dose record(s) with negative TIME",
      if (obs_neg > 0) {
        paste0(" (plus ", obs_neg, " non-dose negative times).")
      } else {
        "."
      }
    ),
    n_flagged = length(dose_neg),
    flagged_records = tibble::as_tibble(data[dose_neg, , drop = FALSE])
  )
}

#' CORE-TIME-004: Actual vs nominal time descriptive difference
#'
#' When both actual (TIME) and nominal (NTIME) times are present, flags records
#' whose absolute difference is a robust (MAD-based) outlier.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list; uses `outlier_nmad`.
#' @return A partial check result.
#' @noRd
check_time_actual_nominal <- function(data, thresholds) {
  if (!"NTIME" %in% names(data)) {
    return(result_skip("No nominal time variable present."))
  }
  actual <- suppressWarnings(as.numeric(data$TIME))
  nominal <- suppressWarnings(as.numeric(data$NTIME))
  idx <- which(!is.na(actual) & !is.na(nominal))
  if (length(idx) < 3) {
    return(result_skip("Too few records with both actual and nominal time."))
  }

  diff <- abs(actual[idx] - nominal[idx])
  flagged <- idx[robust_outliers(diff, thresholds$outlier_nmad)]
  if (length(flagged) == 0) {
    return(result_pass("Actual-nominal time differences are unremarkable."))
  }
  result_flag(
    message = paste0(
      length(flagged), " record(s) with outlying actual-nominal time difference."
    ),
    n_flagged = length(flagged),
    flagged_records = tibble::as_tibble(data[flagged, , drop = FALSE])
  )
}
