#' CORE-CMP-001: CMT basic consistency
#'
#' When CMT is present and mostly populated, flags records with a missing CMT.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_cmp_cmt <- function(data, thresholds) {
  if (!"CMT" %in% names(data)) {
    return(result_skip("No CMT variable present."))
  }
  missing <- is.na(data$CMT) | trimws(as.character(data$CMT)) == ""
  n <- sum(missing)
  if (n == 0) {
    return(result_pass("CMT is populated on all records."))
  }
  if (n == nrow(data)) {
    return(result_skip("CMT is entirely missing."))
  }
  result_flag(
    message = paste0(
      n, " record(s) with missing CMT while CMT is otherwise populated."
    ),
    n_flagged = n,
    flagged_records = tibble::as_tibble(data[which(missing), , drop = FALSE])
  )
}

#' CORE-CMP-002: DVID and analyte internal consistency
#'
#' When DVID and an analyte label are present, flags DVID values that map to
#' more than one analyte label.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_cmp_dvid_analyte <- function(data, thresholds) {
  if (!"DVID" %in% names(data)) {
    return(result_skip("No DVID variable present."))
  }
  analyte_col <- detect_col(names(data), "^PARAM$|PARAMCD|ANALYTE")
  if (is.na(analyte_col)) {
    return(result_skip("No analyte label variable present."))
  }
  d <- data.frame(
    DVID = as.character(data$DVID),
    label = as.character(data[[analyte_col]]),
    stringsAsFactors = FALSE
  )
  d <- d[!is.na(d$DVID) & !is.na(d$label), ]
  labels_per_dvid <- tapply(d$label, d$DVID, function(x) length(unique(x)))
  bad <- names(labels_per_dvid)[labels_per_dvid > 1]

  if (length(bad) == 0) {
    return(result_pass("Each DVID maps to a single analyte label."))
  }
  summary <- dplyr::distinct(d[d$DVID %in% bad, ])
  result_flag(
    message = paste0(
      length(bad), " DVID value(s) mapping to multiple analyte labels."
    ),
    n_flagged = length(bad),
    summary_table = tibble::as_tibble(summary)
  )
}

#' CORE-CMP-003: Concentration magnitude review across DVID
#'
#' Compares median log-concentration across DVID groups and flags when the
#' groups differ by more than ~1000-fold, for scientist review.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @importFrom stats median
#' @noRd
check_cmp_magnitude <- function(data, thresholds) {
  if (!"DVID" %in% names(data)) {
    return(result_skip("No DVID variable present."))
  }
  evid <- suppressWarnings(as.numeric(data$EVID))
  mdv <- suppressWarnings(as.numeric(data$MDV))
  dv <- suppressWarnings(as.numeric(data$DV))
  quant <- evid == 0 & mdv == 0 & !is.na(dv) & dv > 0
  if (sum(quant) < 3 || length(unique(data$DVID[quant])) < 2) {
    return(result_skip("Fewer than two DVID groups with quantifiable data."))
  }

  med <- tapply(log(dv[quant]), data$DVID[quant], median)
  summary <- tibble::tibble(
    DVID = names(med), median_log_dv = round(as.numeric(med), 3)
  )
  if (diff(range(med)) > log(1000)) {
    return(result_flag(
      message = "Concentration magnitude differs > 1000x across DVID groups.",
      n_flagged = length(med),
      summary_table = summary
    ))
  }
  result_pass(
    "Concentration magnitudes across DVID groups are within ~1000x.",
    summary_table = summary
  )
}
