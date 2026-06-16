#' Infer likely study type(s) from an NMPK dataset
#'
#' Data-driven inference for `CORE-STUDYTYPE-001`, following the *Study Type Map*
#' sheet of the QC checklist. Operates on a dataset already mapped to canonical
#' variable names. The result is informational: the app presents it for user
#' confirmation before checks are selected.
#'
#' @param data A data frame with canonical variable names.
#' @return A tibble with columns `study_type`, `applicable` (logical), `reason`.
#' @export
#' @examples
#' df <- data.frame(
#'   ID = c(1, 1, 2, 2), TIME = c(0, 1, 0, 1), EVID = c(1, 0, 1, 0),
#'   AMT = c(100, 0, 100, 0), ADDL = c(5, NA, 5, NA), II = c(24, NA, 24, NA)
#' )
#' infer_study_type(df)
infer_study_type <- function(data) {
  nms <- names(data)
  has <- function(v) all(v %in% nms)

  dose_rows <- if (has("EVID")) data$EVID %in% c(1, 4) else rep(FALSE, nrow(data))
  doses_per_id <- if (has("ID") && any(dose_rows)) {
    max(table(data$ID[dose_rows]))
  } else {
    NA_integer_
  }

  multiple_signals <- c(
    if (any(c("ADDL", "II", "SS") %in% nms)) "ADDL/II/SS present",
    if (any(c("OCC", "DOSNO", "PERIOD") %in% nms)) "occasion/dose-number present",
    if (!is.na(doses_per_id) && doses_per_id > 1) "repeated dose records per subject"
  )
  is_multiple <- length(multiple_signals) > 0
  is_single <- !is_multiple && !is.na(doses_per_id) && doses_per_id <= 1

  iv_signals <- c(
    if (any(c("RATE", "DUR") %in% nms)) "RATE/DUR present",
    if (has("RATE") && any(data$RATE == -2, na.rm = TRUE)) "RATE = -2 convention"
  )
  multi_signals <- if (has("DVID") && dplyr::n_distinct(data$DVID) > 1) {
    "multiple DVID groups"
  } else {
    character()
  }
  integ_signals <- if (has("STUDYID") && dplyr::n_distinct(data$STUDYID) > 1) {
    "multiple STUDYID values"
  } else {
    character()
  }

  reason_or <- function(signals) {
    if (length(signals) > 0) paste(signals, collapse = "; ") else "Not indicated."
  }

  tibble::tibble(
    study_type = c(
      "All", "Single Dose", "Multiple Dose", "IV", "Multi-analyte", "Integrated"
    ),
    applicable = c(
      TRUE, is_single, is_multiple,
      length(iv_signals) > 0, length(multi_signals) > 0, length(integ_signals) > 0
    ),
    reason = c(
      "Always applicable to a readable dataset.",
      if (is_single) "At most one dose record per subject." else "Not indicated.",
      reason_or(multiple_signals),
      reason_or(iv_signals),
      reason_or(multi_signals),
      reason_or(integ_signals)
    )
  )
}

#' CORE-STUDYTYPE-001: Study type inference and user confirmation
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result carrying the inference table.
#' @noRd
check_study_type <- function(data, thresholds) {
  inferred <- infer_study_type(data)
  applicable <- inferred$study_type[inferred$applicable]
  result_pass(
    message = paste0(
      "Inferred study type(s): ", paste(applicable, collapse = ", "), "."
    ),
    summary_table = inferred
  )
}
