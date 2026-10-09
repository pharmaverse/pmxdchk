#' CORE-INT-001: Subject ID uniqueness across studies
#'
#' The NONMEM `ID` must identify exactly one subject. Flags an `ID` that maps to
#' more than one `USUBJID` or appears in more than one study, and a `USUBJID`
#' that maps to more than one `ID`.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_int_id_uniqueness <- function(data, thresholds) {
  keys <- intersect(c("STUDYID", "USUBJID"), names(data))
  if (length(keys) == 0) {
    return(result_skip("No STUDYID or USUBJID variable present."))
  }
  id <- as.character(data$ID)
  n_per <- function(x, by) tapply(x, by, function(v) length(unique(v)))

  bad <- list()
  for (key in keys) {
    n <- n_per(as.character(data[[key]]), id)
    hit <- names(n)[n > 1]
    if (length(hit) > 0) {
      bad[[key]] <- tibble::tibble(
        ID = hit, issue = paste0("ID maps to ", n[hit], " ", key, " values")
      )
    }
  }
  if ("USUBJID" %in% keys) {
    n <- n_per(id, as.character(data$USUBJID))
    hit <- names(n)[n > 1]
    ids <- unique(id[data$USUBJID %in% hit])
    if (length(ids) > 0) {
      bad[["split"]] <- tibble::tibble(
        ID = ids, issue = "USUBJID maps to more than one ID"
      )
    }
  }

  if (length(bad) == 0) {
    return(result_pass(paste0(
      "ID is unique with respect to ", paste(keys, collapse = " and "), "."
    )))
  }
  tab <- dplyr::bind_rows(bad)
  result_flag(
    message = paste0(
      length(unique(tab$ID)), " ID(s) do not identify a single subject."
    ),
    n_flagged = length(unique(tab$ID)),
    summary_table = tab,
    subject_list = tibble::tibble(ID = unique(tab$ID))
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
  cats <- intersect(c("SEX", "RACE", "ETHNIC", "ROUTE"), names(data))
  if (length(cats) == 0) {
    return(result_skip("No categorical covariates to compare across studies."))
  }

  bad <- list()
  for (cv in cats) {
    sets <- tapply(
      as.character(data[[cv]]), data$STUDYID,
      function(x) sort(unique(x[!is.na(x)]))
    )
    sets <- sets[vapply(sets, length, integer(1)) > 0]
    if (length(sets) < 2) next
    disjoint <- FALSE
    for (i in seq_len(length(sets) - 1)) {
      for (j in (i + 1):length(sets)) {
        if (length(intersect(sets[[i]], sets[[j]])) == 0) disjoint <- TRUE
      }
    }
    if (disjoint) {
      bad[[cv]] <- tibble::tibble(
        variable = cv, STUDYID = names(sets),
        values = vapply(sets, paste, character(1), collapse = ", ")
      )
    }
  }

  if (length(bad) == 0) {
    return(result_pass(
      "Categorical covariate coding is consistent across studies."
    ))
  }
  result_flag(
    message = paste0(
      length(bad), " covariate(s) with divergent cross-study coding: ",
      paste(names(bad), collapse = ", "), "."
    ),
    n_flagged = length(bad),
    summary_table = dplyr::bind_rows(bad)
  )
}

#' CORE-INT-003: Cross-study numeric distribution review
#'
#' When two or more studies are pooled, tabulates the per-study median of each
#' continuous covariate and of AMT, and flags covariates whose highest and
#' lowest study medians differ more than 2-fold, a possible unit or
#' harmonization issue.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @importFrom stats median
#' @noRd
check_int_numeric <- function(data, thresholds) {
  if (!"STUDYID" %in% names(data)) {
    return(result_skip("No STUDYID variable present."))
  }
  if (length(unique(data$STUDYID)) < 2) {
    return(result_skip("Only one study present."))
  }
  covs <- continuous_covariates(data)
  nums <- c(covs, intersect("AMT", names(data)))
  if (length(nums) == 0) {
    return(result_skip("No numeric variables to compare across studies."))
  }

  tab <- dplyr::bind_rows(lapply(nums, function(cv) {
    x <- suppressWarnings(as.numeric(data[[cv]]))
    x[x <= 0] <- NA
    med <- tapply(x, data$STUDYID, function(v) median(v, na.rm = TRUE))
    med <- med[!is.na(med)]
    tibble::tibble(
      variable = cv, STUDYID = names(med), median = signif(as.numeric(med), 4),
      flagged = cv %in% covs && length(med) > 1 && max(med) / min(med) > 2
    )
  }))

  bad <- unique(tab$variable[tab$flagged])
  if (length(bad) == 0) {
    return(result_pass(
      "Covariate medians are within 2-fold across studies.",
      summary_table = tab
    ))
  }
  result_flag(
    message = paste0(
      length(bad), " covariate(s) whose median differs more than 2-fold ",
      "between studies: ", paste(bad, collapse = ", "), "."
    ),
    n_flagged = length(bad),
    summary_table = tab
  )
}
