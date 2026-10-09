#' Default check thresholds
#'
#' Conservative, data-driven defaults for checks that take tunable cutoffs.
#' Passed into every check by [run_nmpk_checks()]; override individual elements
#' to retune without touching check code.
#'
#' @return A named list of thresholds.
#' @export
#' @examples
#' th <- default_thresholds()
#' th$min_quantifiable_n <- 3
default_thresholds <- function() {
  list(
    outlier_nmad = 5,        # robust (MAD-based) cutoff for continuous outliers
    outlier_iqr_k = 3,       # boxplot fence multiplier for covariate outliers
    min_quantifiable_n = 2,  # minimum quantifiable PK records per subject
    predose_cmax_frac = 0.05 # predose cutoff as a fraction of Cmax
  )
}

#' Run NONMEM dataset quality checks
#'
#' Headless entry point and the same path the Shiny app uses. Applies the
#' variable mapping, orders checks by their prerequisites, evaluates skip
#' conditions, runs each check, and returns a tidy findings tibble.
#'
#' @param data A data frame of a NONMEM-style (NMPK) dataset.
#' @param mapping Optional named character vector mapping canonical variable
#'   names to user column names (`c(ID = "USUBJID", TIME = "TAFD")`). When
#'   `NULL`, columns are assumed to already use canonical names.
#' @param thresholds A list of thresholds; see [default_thresholds()]. Elements
#'   not supplied fall back to their defaults.
#' @param study_type Character vector of confirmed study types used to gate
#'   conditional checks. Defaults to `"All"`.
#'
#' @return A tibble with one row per check: `check_id`, `title`, `domain`,
#'   `severity`, `status`, `n_flagged`, `message`. The full
#'   [new_check_result()] objects are
#'   attached as the `"results"` attribute for the app and reporting layers.
#' @export
#' @examples
#' df <- data.frame(
#'   ID = c(1, 1), TIME = c(0, 1), EVID = c(1, 0),
#'   MDV = c(1, 0), DV = c(0, 5), AMT = c(100, 0)
#' )
#' run_nmpk_checks(df)
run_nmpk_checks <- function(data, mapping = NULL,
                            thresholds = default_thresholds(),
                            study_type = "All") {
  defaults <- default_thresholds()
  defaults[names(thresholds)] <- thresholds
  thresholds <- defaults
  data <- apply_mapping(data, mapping)
  data[[".rowid"]] <- seq_len(nrow(data))
  specs <- order_checks(check_registry())

  results <- list()
  for (spec in specs) {
    if (!check_applies(spec, study_type)) next
    results[[spec$id]] <- run_one_check(spec, data, results, thresholds)
  }

  findings <- results_to_tibble(results)
  attr(findings, "results") <- results
  findings
}

#' Apply a variable mapping to a dataset
#'
#' Renames user columns to canonical names. A fuller mapping UI arrives in
#' Phase 1; this handles the rename so the engine is usable headless now.
#'
#' @inheritParams run_nmpk_checks
#' @return A tibble with canonical column names where mapped.
#' @importFrom stats setNames
#' @noRd
apply_mapping <- function(data, mapping = NULL) {
  data <- tibble::as_tibble(data)
  if (is.null(mapping) || length(mapping) == 0) {
    return(data)
  }
  present <- mapping[mapping %in% names(data)]
  if (length(present) == 0) {
    return(data)
  }
  dplyr::rename(data, !!!stats::setNames(unname(present), names(present)))
}

#' Order checks so prerequisites run first
#'
#' Stable topological sort on the `requires` edges. Prerequisites that are not
#' registered (e.g. checks from a later phase) are ignored.
#'
#' @param registry The check registry.
#' @return The registry as an ordered list.
#' @noRd
order_checks <- function(registry) {
  ids <- names(registry)
  visit <- function(id, ordered) {
    if (id %in% ordered) {
      return(ordered)
    }
    for (dep in intersect(registry[[id]]$requires, ids)) {
      ordered <- visit(dep, ordered)
    }
    c(ordered, id)
  }
  ordered <- character()
  for (id in ids) ordered <- visit(id, ordered)
  registry[ordered]
}

#' Whether a check applies to the confirmed study type
#'
#' @param spec A registry entry.
#' @param study_type The confirmed study type.
#' @return `TRUE` if the check should run.
#' @noRd
check_applies <- function(spec, study_type) {
  "All" %in% spec$study_types || any(study_type %in% spec$study_types)
}

#' Run a single check, handling skip and error states
#'
#' @param spec A registry entry.
#' @param data The mapped dataset.
#' @param results Results accumulated so far (for prerequisite evaluation).
#' @param thresholds The thresholds list.
#' @return A [new_check_result()] object.
#' @noRd
run_one_check <- function(spec, data, results, thresholds) {
  blocked <- intersect(spec$requires, names(results))
  blocked <- blocked[vapply(
    results[blocked], function(r) r$status %in% c("skip", "error"), logical(1)
  )]
  if (length(blocked) > 0) {
    return(finalize_result(spec, result_skip(paste0(
      "Skipped: prerequisite check(s) not satisfied (",
      paste(blocked, collapse = ", "), ")."
    ))))
  }

  missing_vars <- setdiff(spec$min_vars, names(data))
  if (length(missing_vars) > 0) {
    return(finalize_result(spec, result_skip(paste0(
      "Skipped: required variable(s) absent (",
      paste(missing_vars, collapse = ", "), ")."
    ))))
  }

  args <- list(data, thresholds)
  if ("results" %in% names(formals(spec$fn))) {
    args$results <- results
  }
  partial <- tryCatch(
    do.call(spec$fn, args),
    error = function(e) {
      list(status = "error", message = conditionMessage(e), n_flagged = 0L)
    }
  )
  finalize_result(spec, partial)
}

#' Attach registry metadata to a partial result
#'
#' @param spec A registry entry.
#' @param partial A partial result from a check function or helper.
#' @return A [new_check_result()] object.
#' @importFrom rlang %||%
#' @noRd
finalize_result <- function(spec, partial) {
  new_check_result(
    check_id = spec$id,
    title = spec$title,
    domain = spec$domain,
    severity = spec$severity,
    status = partial$status,
    message = partial$message,
    n_flagged = partial$n_flagged %||% 0L,
    summary_table = partial$summary_table,
    flagged_records = partial$flagged_records,
    subject_list = partial$subject_list,
    plot_data = partial$plot_data,
    rule = spec$rule,
    guidance = spec$guidance
  )
}

#' Flatten check results into a tidy findings tibble
#'
#' @param results A list of [new_check_result()] objects.
#' @return One row per check.
#' @noRd
results_to_tibble <- function(results) {
  if (length(results) == 0) {
    return(tibble::tibble(
      check_id = character(), title = character(), domain = character(),
      severity = character(),
      status = character(), n_flagged = integer(), message = character()
    ))
  }
  purrr::map_dfr(results, function(r) {
    tibble::tibble(
      check_id = r$check_id,
      title = r$title,
      domain = r$domain,
      severity = r$severity,
      status = r$status,
      n_flagged = r$n_flagged,
      message = r$message
    )
  })
}
