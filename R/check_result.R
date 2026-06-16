#' Construct a standardized check result
#'
#' Internal constructor for the object every check returns once finalized by the
#' runner. The UI and reporting layers consume only this structure.
#'
#' @param check_id Character scalar, the check identifier
#'   (e.g. `"CORE-STRUCT-001"`).
#' @param title Character scalar, human-readable check title.
#' @param domain Character scalar, the check domain (e.g. `"structure"`).
#' @param severity One of `"Critical"`, `"High"`, `"Medium"`.
#' @param status One of `"pass"`, `"flag"`, `"skip"`, `"error"`.
#' @param message Character scalar, one-line human-readable conclusion.
#' @param n_flagged Integer, number of flagged records or subjects.
#' @param summary_table,flagged_records,subject_list,plot_data Optional tibbles
#'   carrying the check's structured output.
#'
#' @return An object of class `pmxdchk_check_result`.
#' @noRd
new_check_result <- function(check_id, title, domain, severity, status, message,
                             n_flagged = 0L,
                             summary_table = NULL,
                             flagged_records = NULL,
                             subject_list = NULL,
                             plot_data = NULL) {
  stopifnot(
    rlang::is_string(check_id),
    rlang::is_string(status),
    status %in% c("pass", "flag", "skip", "error"),
    severity %in% c("Critical", "High", "Medium")
  )
  structure(
    list(
      check_id = check_id,
      title = title,
      domain = domain,
      severity = severity,
      status = status,
      message = message,
      n_flagged = as.integer(n_flagged),
      summary_table = summary_table,
      flagged_records = flagged_records,
      subject_list = subject_list,
      plot_data = plot_data
    ),
    class = "pmxdchk_check_result"
  )
}

#' Partial result helpers for check functions
#'
#' Check functions return the *logic* of a result (status, message, outputs)
#' without repeating metadata; the runner attaches `check_id`, `title`,
#' `domain`, and `severity` from the registry. These helpers build that partial
#' list.
#'
#' @param message One-line conclusion.
#' @param n_flagged Number of flagged records or subjects.
#' @param summary_table,flagged_records,subject_list,plot_data Optional output
#'   tibbles.
#'
#' @return A list consumed by [finalize_result()].
#' @noRd
result_pass <- function(message, summary_table = NULL, plot_data = NULL) {
  list(
    status = "pass", message = message, n_flagged = 0L,
    summary_table = summary_table, plot_data = plot_data
  )
}

#' @noRd
result_flag <- function(message, n_flagged, flagged_records = NULL,
                        summary_table = NULL, subject_list = NULL,
                        plot_data = NULL) {
  list(
    status = "flag", message = message, n_flagged = as.integer(n_flagged),
    flagged_records = flagged_records, summary_table = summary_table,
    subject_list = subject_list, plot_data = plot_data
  )
}

#' @noRd
result_skip <- function(message) {
  list(status = "skip", message = message, n_flagged = 0L)
}
