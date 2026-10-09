#' CORE-DOSE-001: Dose AMT validity
#'
#' Flags dose records (EVID 1 or 4) with missing, negative, or zero AMT, and
#' observation records (EVID 0) with a non-zero AMT.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_dose_amt_validity <- function(data, thresholds) {
  evid <- suppressWarnings(as.numeric(data$EVID))
  amt <- suppressWarnings(as.numeric(data$AMT))

  bad_dose <- evid %in% c(1, 4) & (is.na(amt) | amt <= 0)
  bad_obs <- evid == 0 & !is.na(amt) & amt != 0
  flagged <- which(bad_dose | bad_obs)

  if (length(flagged) == 0) {
    return(result_pass("Dose AMT values are valid."))
  }
  result_flag(
    message = paste0(
      length(flagged), " record(s) with suspicious AMT ",
      "(invalid dose amount or non-zero on an observation)."
    ),
    n_flagged = length(flagged),
    flagged_records = tibble::as_tibble(data[flagged, , drop = FALSE])
  )
}

#' CORE-DOSE-002: Dose amount distribution
#'
#' Lists the distinct dose amounts and flags dose records whose log AMT is a
#' robust (MAD-based) outlier, catching likely unit or pooling errors. The
#' spread is floored at a 1.5-fold difference so that ordinary dose levels in a
#' dataset dominated by one dose are not flagged.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list; uses `outlier_nmad`.
#' @return A partial check result.
#' @importFrom stats mad median
#' @noRd
check_dose_distribution <- function(data, thresholds) {
  evid <- suppressWarnings(as.numeric(data$EVID))
  amt <- suppressWarnings(as.numeric(data$AMT))
  dose <- which(evid %in% c(1, 4) & !is.na(amt) & amt > 0)
  if (length(dose) < 3) {
    return(result_skip("Too few dose records for distribution review."))
  }

  log_amt <- log(amt[dose])
  z <- abs(log_amt - median(log_amt)) / max(mad(log_amt), log(1.5))
  flagged <- dose[z > thresholds$outlier_nmad]
  n_doses <- table(amt[dose])
  levels <- tibble::tibble(
    AMT = as.numeric(names(n_doses)), n_doses = as.integer(n_doses)
  )
  levels$flagged <- levels$AMT %in% amt[flagged]

  if (length(flagged) == 0) {
    return(result_pass(
      paste0("No dose amount outliers among ", nrow(levels), " dose level(s)."),
      summary_table = levels
    ))
  }
  result_flag(
    message = paste0(
      length(flagged), " dose record(s) with an outlying amount (",
      paste(sort(unique(amt[flagged])), collapse = ", "), ")."
    ),
    n_flagged = length(flagged),
    flagged_records = tibble::as_tibble(data[flagged, , drop = FALSE]),
    summary_table = levels
  )
}

#' CORE-DOSE-003: Subjects with observations but no dose
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_dose_obs_no_dose <- function(data, thresholds) {
  evid <- suppressWarnings(as.numeric(data$EVID))
  obs_ids <- unique(data$ID[evid == 0])
  dose_ids <- unique(data$ID[evid %in% c(1, 4)])
  bad <- setdiff(obs_ids, dose_ids)

  if (length(bad) == 0) {
    return(result_pass("All subjects with observations have dose records."))
  }
  result_flag(
    message = paste0(length(bad), " subject(s) with observations but no dose."),
    n_flagged = length(bad),
    subject_list = tibble::tibble(ID = bad)
  )
}

#' CORE-DOSE-004: Subjects with dose but no quantifiable observation
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_dose_dose_no_obs <- function(data, thresholds) {
  evid <- suppressWarnings(as.numeric(data$EVID))
  mdv <- suppressWarnings(as.numeric(data$MDV))
  dv <- suppressWarnings(as.numeric(data$DV))
  dose_ids <- unique(data$ID[evid %in% c(1, 4)])
  qobs_ids <- unique(data$ID[evid == 0 & mdv == 0 & !is.na(dv)])
  bad <- setdiff(dose_ids, qobs_ids)

  if (length(bad) == 0) {
    return(result_pass("All dosed subjects have a quantifiable observation."))
  }
  result_flag(
    message = paste0(
      length(bad), " subject(s) with dose but no quantifiable observation."
    ),
    n_flagged = length(bad),
    subject_list = tibble::tibble(ID = bad)
  )
}

#' CORE-DOSE-005: ADDL and II basic validity
#'
#' When ADDL is present, flags negative or non-integer ADDL, positive ADDL on a
#' non-dose record, and (where ADDL is positive) a missing or non-positive II.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_dose_addl_ii <- function(data, thresholds) {
  if (!"ADDL" %in% names(data)) {
    return(result_skip("No ADDL variable present."))
  }
  addl <- suppressWarnings(as.numeric(data$ADDL))
  ii <- if ("II" %in% names(data)) {
    suppressWarnings(as.numeric(data$II))
  } else {
    rep(NA_real_, nrow(data))
  }
  evid <- suppressWarnings(as.numeric(data$EVID))
  have <- !is.na(addl)
  bad_addl <- have & (addl < 0 | addl != round(addl))
  bad_nondose <- have & addl > 0 & !(evid %in% c(1, 4))
  bad_ii <- have & addl > 0 & (is.na(ii) | ii <= 0)
  flagged <- which(bad_addl | bad_nondose | bad_ii)

  if (length(flagged) == 0) {
    return(result_pass("ADDL and II values are valid."))
  }
  result_flag(
    message = paste0(length(flagged), " record(s) with invalid ADDL/II."),
    n_flagged = length(flagged),
    flagged_records = tibble::as_tibble(data[flagged, , drop = FALSE])
  )
}

#' CORE-DOSE-006: Infusion RATE and DUR basic validity
#'
#' When RATE or DUR are present, flags negative RATE other than the NONMEM
#' control values (-1: rate modeled, -2: duration modeled), non-zero RATE on a
#' non-dose record, missing DUR on a RATE = -2 record when the dataset carries a
#' DUR column, and AMT / RATE that disagrees with DUR by more than 5%.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_dose_rate_dur <- function(data, thresholds) {
  if (!"RATE" %in% names(data) && !"DUR" %in% names(data)) {
    return(result_skip("No RATE/DUR variable present."))
  }
  evid <- suppressWarnings(as.numeric(data$EVID))
  rate <- if ("RATE" %in% names(data)) {
    suppressWarnings(as.numeric(data$RATE))
  } else {
    rep(NA_real_, nrow(data))
  }
  dur <- if ("DUR" %in% names(data)) {
    suppressWarnings(as.numeric(data$DUR))
  } else {
    rep(NA_real_, nrow(data))
  }
  amt <- suppressWarnings(as.numeric(data$AMT))
  is_dose <- evid %in% c(1, 4)
  issue <- rep(NA_character_, nrow(data))
  issue[
    is_dose & "DUR" %in% names(data) & rate %in% -2 & is.na(dur)
  ] <- "RATE = -2 without DUR"
  mismatch <- is_dose & rate > 0 & dur > 0 & abs(amt / rate - dur) > 0.05 * dur
  issue[which(mismatch)] <- "AMT / RATE differs from DUR"
  issue[!is_dose & !is.na(rate) & rate != 0] <- "RATE on a non-dose record"
  issue[
    !is.na(rate) & rate < 0 & !(rate %in% c(-1, -2))
  ] <- "negative RATE other than -1 or -2"
  flagged <- which(!is.na(issue))

  if (length(flagged) == 0) {
    return(result_pass("Infusion RATE/DUR values are valid."))
  }
  records <- tibble::as_tibble(data[flagged, , drop = FALSE])
  records$issue <- issue[flagged]
  result_flag(
    message = paste0(
      length(flagged), " record(s) with invalid RATE/DUR: ",
      paste(unique(issue[flagged]), collapse = "; "), "."
    ),
    n_flagged = length(flagged),
    flagged_records = records
  )
}

#' CORE-DOSE-007: SS flag basic validity
#'
#' When SS is present, flags non-binary SS values, SS = 1 on non-dose records,
#' and SS = 1 without a positive II (when II is available).
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_dose_ss <- function(data, thresholds) {
  if (!"SS" %in% names(data)) {
    return(result_skip("No SS variable present."))
  }
  evid <- suppressWarnings(as.numeric(data$EVID))
  ss <- suppressWarnings(as.numeric(data$SS))
  has_ii <- "II" %in% names(data)
  ii <- if (has_ii) {
    suppressWarnings(as.numeric(data$II))
  } else {
    rep(NA_real_, nrow(data))
  }
  is_dose <- evid %in% c(1, 4)

  bad_val <- !is.na(ss) & !(ss %in% c(0, 1))
  bad_nondose <- !is_dose & !is.na(ss) & ss == 1
  bad_noii <- is_dose & !is.na(ss) & ss == 1 & has_ii & (is.na(ii) | ii <= 0)
  flagged <- which(bad_val | bad_nondose | bad_noii)

  if (length(flagged) == 0) {
    return(result_pass("SS flag values are valid."))
  }
  result_flag(
    message = paste0(length(flagged), " record(s) with invalid SS pattern."),
    n_flagged = length(flagged),
    flagged_records = tibble::as_tibble(data[flagged, , drop = FALSE])
  )
}
