#' CORE-INT-001: Subject ID uniqueness across studies
#'
#' When STUDYID is present, flags subject IDs that appear in more than one
#' study, a possible ID collision in a pooled dataset.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_int_id_uniqueness <- function(data, thresholds) {
  if (!"STUDYID" %in% names(data)) {
    return(result_skip("No STUDYID variable present."))
  }
  d <- unique(data.frame(
    ID = as.character(data$ID), STUDYID = as.character(data$STUDYID),
    stringsAsFactors = FALSE
  ))
  studies_per_id <- tapply(d$STUDYID, d$ID, function(x) length(unique(x)))
  bad <- names(studies_per_id)[studies_per_id > 1]

  if (length(bad) == 0) {
    return(result_pass("Subject IDs do not collide across studies."))
  }
  result_flag(
    message = paste0(
      length(bad), " subject ID(s) appear in multiple studies (possible collision)."
    ),
    n_flagged = length(bad),
    subject_list = tibble::tibble(ID = bad)
  )
}

#' CORE-INT-002: Cross-study categorical coding consistency
#'
#' When STUDYID is present, flags categorical covariates whose value
#' vocabularies are disjoint between two studies, suggesting divergent coding.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_int_categorical <- function(data, thresholds) {
  if (!"STUDYID" %in% names(data)) {
    return(result_skip("No STUDYID variable present."))
  }
  cats <- intersect(c("SEX", "RACE", "ROUTE"), names(data))
  if (length(cats) == 0) {
    return(result_skip("No categorical covariates to compare across studies."))
  }

  bad <- character()
  for (cv in cats) {
    sets <- tapply(
      as.character(data[[cv]]), data$STUDYID,
      function(x) unique(x[!is.na(x)])
    )
    sets <- sets[vapply(sets, length, integer(1)) > 0]
    if (length(sets) < 2) next
    disjoint <- FALSE
    for (i in seq_len(length(sets) - 1)) {
      for (j in (i + 1):length(sets)) {
        if (length(intersect(sets[[i]], sets[[j]])) == 0) disjoint <- TRUE
      }
    }
    if (disjoint) bad <- c(bad, cv)
  }

  if (length(bad) == 0) {
    return(result_pass("Categorical covariate coding is consistent across studies."))
  }
  result_flag(
    message = paste0(
      length(bad), " covariate(s) with divergent cross-study coding: ",
      paste(bad, collapse = ", "), "."
    ),
    n_flagged = length(bad),
    summary_table = tibble::tibble(variable = bad)
  )
}

#' CORE-INT-003: Cross-study numeric distribution review
#'
#' When STUDYID is present (3+ studies), flags numeric variables whose per-study
#' median is a robust outlier among studies, a possible unit/harmonization issue.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list; uses `outlier_nmad`.
#' @return A partial check result.
#' @importFrom stats median
#' @noRd
check_int_numeric <- function(data, thresholds) {
  if (!"STUDYID" %in% names(data)) {
    return(result_skip("No STUDYID variable present."))
  }
  if (length(unique(data$STUDYID)) < 3) {
    return(result_skip("Fewer than three studies for distribution comparison."))
  }
  nums <- intersect(c("WT", "BMI", "AGE", "AMT"), names(data))
  if (length(nums) == 0) {
    return(result_skip("No numeric variables to compare across studies."))
  }

  bad <- list()
  for (cv in nums) {
    x <- suppressWarnings(as.numeric(data[[cv]]))
    med <- tapply(x, data$STUDYID, function(v) median(v, na.rm = TRUE))
    med <- med[!is.na(med)]
    if (length(med) < 3) next
    out <- robust_outliers(as.numeric(med), thresholds$outlier_nmad)
    if (any(out)) {
      bad[[cv]] <- tibble::tibble(
        variable = cv, STUDYID = names(med)[out],
        median = round(as.numeric(med)[out], 2)
      )
    }
  }

  if (length(bad) == 0) {
    return(result_pass("Numeric distributions are comparable across studies."))
  }
  tab <- dplyr::bind_rows(bad)
  result_flag(
    message = paste0(
      nrow(tab), " study-level numeric distribution outlier(s)."
    ),
    n_flagged = nrow(tab),
    summary_table = tab
  )
}
