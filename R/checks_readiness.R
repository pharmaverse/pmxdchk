#' CORE-MR-001: Exclusion flag internal consistency
#'
#' When an exclusion-flag-like variable is present, flags excluded records that
#' lack a reason when a reason variable is available; otherwise summarizes the
#' excluded record count.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_mr_exclusion_flags <- function(data, thresholds) {
  flag_col <- grep("^EXCL(FL)?$|EXCLFL", names(data),
    ignore.case = TRUE, value = TRUE
  )[1]
  if (is.na(flag_col)) {
    return(result_skip("No exclusion flag variable detected."))
  }

  excl <- as.character(data[[flag_col]])
  is_excl <- !is.na(excl) & toupper(trimws(excl)) %in% c("Y", "1", "TRUE")
  reason_col <- grep("EXCLRSN|EXREAS|EXCLREAS|REASON", names(data),
    ignore.case = TRUE, value = TRUE
  )[1]

  if (is.na(reason_col)) {
    return(result_pass(
      paste0(
        sum(is_excl), " excluded record(s); no reason variable to cross-check."
      ),
      summary_table = tibble::tibble(
        flag = flag_col, n_excluded = sum(is_excl)
      )
    ))
  }

  reason <- as.character(data[[reason_col]])
  has_reason <- !is.na(reason) & trimws(reason) != ""
  issue <- rep(NA_character_, nrow(data))
  issue[is_excl & !has_reason] <- "excluded without a reason"
  issue[!is_excl & has_reason] <- "reason given but not excluded"
  bad <- which(!is.na(issue))
  if (length(bad) == 0) {
    return(result_pass("Exclusion flags and reasons are consistent."))
  }
  records <- tibble::as_tibble(data[bad, , drop = FALSE])
  records$issue <- issue[bad]
  result_flag(
    message = paste0(
      length(bad), " record(s) where ", flag_col, " and ", reason_col,
      " disagree."
    ),
    n_flagged = length(bad),
    flagged_records = records
  )
}

#' CORE-MR-002: Imputation traceability signals
#'
#' Detects imputation-flag-like variables and counts the records each marks.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_mr_imputation <- function(data, thresholds) {
  imp_cols <- grep("IMPUT|IMPFL|^IMP$", names(data),
    ignore.case = TRUE, value = TRUE
  )
  if (length(imp_cols) == 0) {
    return(result_skip("No imputation flag variables detected."))
  }
  result_pass(
    message = paste0(
      "Imputation flag variable(s) detected: ",
      paste(imp_cols, collapse = ", "), "."
    ),
    summary_table = tibble::tibble(
      variable = imp_cols,
      n_imputed = vapply(
        imp_cols, function(v) sum(is_positive_flag(data[[v]])), integer(1)
      )
    )
  )
}

#' CORE-MR-003: Plot-ready flagged subject review
#'
#' Lists every subject flagged by at least one other check, with the checks that
#' flagged them, for review in the individual profile browser.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @param results The check results accumulated so far by the runner.
#' @return A partial check result.
#' @noRd
check_mr_plot_ready <- function(data, thresholds, results = list()) {
  flagged <- Filter(function(r) r$status == "flag", results)
  hits <- dplyr::bind_rows(lapply(flagged, function(r) {
    ids <- result_subject_ids(r)
    tibble::tibble(ID = ids, check_id = rep(r$check_id, length(ids)))
  }))
  if (nrow(hits) == 0) {
    return(result_pass("No subjects were flagged by other checks."))
  }
  checks <- tapply(hits$check_id, hits$ID, paste, collapse = ", ")
  subjects <- tibble::tibble(
    ID = names(checks),
    n_checks = as.integer(table(hits$ID)[names(checks)]),
    checks = as.character(checks)
  )
  subjects <- subjects[order(-subjects$n_checks), ]
  result_pass(
    message = paste0(
      nrow(subjects), " subject(s) flagged by at least one check; ",
      "review them in Profiles."
    ),
    subject_list = subjects
  )
}
