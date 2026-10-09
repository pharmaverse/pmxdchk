#' CORE-TIME-001: TIME non-decreasing within subject
#'
#' Within each subject (and occasion when present), flags records where TIME
#' decreases relative to the previous record in dataset row order. Reset records
#' (EVID 3 or 4) may restart the clock and are not flagged.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_time_nondecreasing <- function(data, thresholds) {
  time <- suppressWarnings(as.numeric(data$TIME))
  reset <- if ("EVID" %in% names(data)) {
    suppressWarnings(as.numeric(data$EVID)) %in% c(3, 4)
  } else {
    rep(FALSE, nrow(data))
  }
  grp <- if ("OCC" %in% names(data)) {
    paste(data$ID, data$OCC, sep = "_")
  } else {
    as.character(data$ID)
  }

  flagged <- unlist(lapply(split(seq_along(time), grp), function(idx) {
    t <- time[idx]
    idx[c(FALSE, diff(t) < 0) & !reset[idx]]
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
#' Flags multiple dose records (EVID 1 or 4) that share the same subject, TIME,
#' and compartment (when CMT is present), a pattern that usually warrants
#' review.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_time_same_time_events <- function(data, thresholds) {
  time <- suppressWarnings(as.numeric(data$TIME))
  evid <- suppressWarnings(as.numeric(data$EVID))
  key <- paste(data$ID, time, sep = "_")
  if ("CMT" %in% names(data)) {
    key <- paste(key, data$CMT, sep = "_")
  }
  is_dose <- evid %in% c(1, 4) & !is.na(time)

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
#' Summarizes records with a negative TIME. Flags those on dose records and,
#' when VISIT is mapped, those recorded at a different visit than the subject's
#' first dose. Other negative times are predose samples and are kept as they
#' are, reported descriptively.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_time_negative <- function(data, thresholds) {
  time <- suppressWarnings(as.numeric(data$TIME))
  evid <- if ("EVID" %in% names(data)) {
    suppressWarnings(as.numeric(data$EVID))
  } else {
    rep(NA_real_, nrow(data))
  }
  neg <- which(!is.na(time) & time < 0)
  if (length(neg) == 0) {
    return(result_pass("No negative TIME values."))
  }
  is_dose <- evid %in% c(1, 4)
  assessment <- ifelse(is_dose[neg], "dose record", "predose (expected)")
  by <- tibble::tibble(assessment = assessment)
  if ("VISIT" %in% names(data)) {
    visit <- as.character(data$VISIT)
    id <- as.character(data$ID)
    first <- which(is_dose & !duplicated(ifelse(is_dose, id, NA)))
    dose_visit <- visit[first][match(id[neg], id[first])]
    off_visit <- !is_dose[neg] & !is.na(dose_visit) & visit[neg] != dose_visit
    assessment[off_visit] <- "not at the first-dose visit"
    by <- tibble::tibble(assessment = assessment, VISIT = visit[neg])
  }
  summary <- dplyr::count(by, dplyr::across(dplyr::everything()), name = "n")
  flagged <- neg[assessment != "predose (expected)"]
  if (length(flagged) == 0) {
    return(result_pass(
      paste0(
        length(neg), " negative TIME record(s), all predose records before ",
        "the first dose."
      ),
      summary_table = summary
    ))
  }
  records <- tibble::as_tibble(data[flagged, , drop = FALSE])
  records$issue <- assessment[assessment != "predose (expected)"]
  result_flag(
    message = paste0(
      length(flagged), " of ", length(neg),
      " negative TIME record(s) need review: ",
      paste(unique(records$issue), collapse = "; "), "."
    ),
    n_flagged = length(flagged),
    flagged_records = records,
    summary_table = summary
  )
}

#' CORE-TIME-004: Actual vs nominal time descriptive difference
#'
#' When both actual (TIME) and nominal (NTIME) times are present, compares the
#' actual-minus-nominal difference among observations sharing a nominal time
#' (and analyte / compartment) and flags robust (MAD-based) outliers.
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
  is_obs <- if ("EVID" %in% names(data)) {
    suppressWarnings(as.numeric(data$EVID)) %in% 0
  } else {
    rep(TRUE, nrow(data))
  }
  idx <- which(is_obs & !is.na(actual) & !is.na(nominal))
  if (length(idx) < 3) {
    return(result_skip("Too few records with both actual and nominal time."))
  }

  diff <- actual - nominal
  grp <- paste(obs_group(data), nominal)
  flagged <- unlist(lapply(split(idx, grp[idx]), function(i) {
    if (length(i) < 3) {
      return(integer())
    }
    i[robust_outliers(diff[i], thresholds$outlier_nmad)]
  }), use.names = FALSE)
  flagged <- sort(flagged)
  if (length(flagged) == 0) {
    return(result_pass("Actual-nominal time differences are unremarkable."))
  }
  records <- tibble::as_tibble(data[flagged, , drop = FALSE])
  records$time_difference <- diff[flagged]
  result_flag(
    message = paste0(
      length(flagged),
      " observation(s) whose actual-nominal time difference is unusual for ",
      "their nominal time point."
    ),
    n_flagged = length(flagged),
    flagged_records = records
  )
}
