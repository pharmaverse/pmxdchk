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
  bad <- which(is_excl & (is.na(reason) | trimws(reason) == ""))
  if (length(bad) == 0) {
    return(result_pass("All excluded records carry a reason."))
  }
  result_flag(
    message = paste0(length(bad), " excluded record(s) without a reason."),
    n_flagged = length(bad),
    flagged_records = tibble::as_tibble(data[bad, , drop = FALSE])
  )
}

#' CORE-MR-002: Imputation traceability signals
#'
#' Detects imputation-flag-like variables and summarizes them for review.
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
    summary_table = tibble::tibble(variable = imp_cols)
  )
}

#' CORE-MR-003: Plot-ready flagged subject review
#'
#' Lists subjects that have quantifiable concentration data and can therefore be
#' reviewed in the individual profile browser. The flagged-subject overlay is
#' provided interactively by the profile browser.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_mr_plot_ready <- function(data, thresholds) {
  evid <- suppressWarnings(as.numeric(data$EVID))
  mdv <- suppressWarnings(as.numeric(data$MDV))
  dv <- suppressWarnings(as.numeric(data$DV))
  ids <- unique(data$ID[evid == 0 & mdv == 0 & !is.na(dv)])
  if (length(ids) == 0) {
    return(result_skip("No quantifiable observations to plot."))
  }
  result_pass(
    message = paste0(
      length(ids), " subject(s) have plot-ready concentration data."
    ),
    summary_table = tibble::tibble(ID = ids)
  )
}
