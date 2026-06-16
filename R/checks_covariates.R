#' CORE-COV-001: Fixed covariate consistency within subject
#'
#' Flags subjects whose supposedly fixed covariates (SEX, RACE, baseline AGE /
#' WT / BMI) take more than one distinct value across their records.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_cov_fixed_consistency <- function(data, thresholds) {
  covs <- intersect(c("SEX", "RACE", "AGE", "WT", "BMI"), names(data))
  if (length(covs) == 0 || !"ID" %in% names(data)) {
    return(result_skip("No fixed covariates available to check."))
  }

  flagged <- list()
  for (cv in covs) {
    n_distinct <- tapply(
      data[[cv]], data$ID, function(x) length(unique(x[!is.na(x)]))
    )
    bad_ids <- names(n_distinct)[!is.na(n_distinct) & n_distinct > 1]
    if (length(bad_ids) > 0) {
      flagged[[cv]] <- tibble::tibble(ID = bad_ids, variable = cv)
    }
  }

  if (length(flagged) == 0) {
    return(result_pass("Fixed covariates are constant within each subject."))
  }
  tab <- dplyr::bind_rows(flagged)
  result_flag(
    message = paste0(
      nrow(tab), " subject-covariate(s) with inconsistent fixed values."
    ),
    n_flagged = nrow(tab),
    summary_table = tab,
    subject_list = tibble::tibble(ID = unique(tab$ID))
  )
}

#' CORE-COV-004: Continuous covariate outlier detection
#'
#' At the subject level (first record per subject), flags robust (MAD-based)
#' outliers among common continuous covariates, using `thresholds$outlier_nmad`.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list; uses `outlier_nmad`.
#' @return A partial check result.
#' @noRd
check_cov_outliers <- function(data, thresholds) {
  covs <- intersect(c("AGE", "WT", "BMI"), names(data))
  if (length(covs) == 0 || !"ID" %in% names(data)) {
    return(result_skip("No continuous covariates available to review."))
  }

  first <- data[!duplicated(data$ID), c("ID", covs), drop = FALSE]
  flagged <- list()
  for (cv in covs) {
    x <- suppressWarnings(as.numeric(first[[cv]]))
    if (sum(!is.na(x)) < 3) next
    out <- which(robust_outliers(x, thresholds$outlier_nmad))
    if (length(out) > 0) {
      flagged[[cv]] <- tibble::tibble(
        ID = first$ID[out], variable = cv, value = x[out]
      )
    }
  }

  if (length(flagged) == 0) {
    return(result_pass("No continuous covariate outliers detected."))
  }
  tab <- dplyr::bind_rows(flagged)
  result_flag(
    message = paste0(
      nrow(tab), " covariate outlier(s) across ",
      length(flagged), " variable(s)."
    ),
    n_flagged = nrow(tab),
    summary_table = tab,
    subject_list = tibble::tibble(ID = unique(tab$ID))
  )
}

#' CORE-COV-005: Implausible common covariate values
#'
#' Conservative public-health plausibility checks for common covariates
#' (negative or extreme age, weight, or BMI).
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_cov_implausible <- function(data, thresholds) {
  rules <- list(
    AGE = function(x) x < 0 | x > 120,
    WT = function(x) x <= 0 | x > 500,
    BMI = function(x) x < 5 | x > 100
  )
  present <- intersect(names(rules), names(data))
  if (length(present) == 0 || !"ID" %in% names(data)) {
    return(result_skip("No covariates with plausibility rules present."))
  }

  flagged <- list()
  for (cv in present) {
    x <- suppressWarnings(as.numeric(data[[cv]]))
    bad <- which(!is.na(x) & rules[[cv]](x))
    if (length(bad) > 0) {
      flagged[[cv]] <- tibble::tibble(
        ID = data$ID[bad], variable = cv, value = x[bad]
      )
    }
  }

  if (length(flagged) == 0) {
    return(result_pass("No implausible covariate values detected."))
  }
  tab <- dplyr::distinct(dplyr::bind_rows(flagged))
  result_flag(
    message = paste0(nrow(tab), " implausible covariate value(s)."),
    n_flagged = nrow(tab),
    summary_table = tab,
    subject_list = tibble::tibble(ID = unique(tab$ID))
  )
}

#' CORE-COV-002: Character-numeric mapping consistency
#'
#' For paired character/numeric variables detected by the `X` / `XN` naming
#' convention (e.g. SEX/SEXN), flags pairs whose label-to-code mapping is not
#' one-to-one.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_cov_char_numeric <- function(data, thresholds) {
  cols <- names(data)
  pairs <- cols[paste0(cols, "N") %in% cols]
  if (length(pairs) == 0) {
    return(result_skip("No paired character/numeric covariates detected."))
  }

  bad <- character()
  for (lab in pairs) {
    code <- paste0(lab, "N")
    d <- unique(data.frame(
      l = as.character(data[[lab]]), c = as.character(data[[code]]),
      stringsAsFactors = FALSE
    ))
    d <- d[!is.na(d$l) & !is.na(d$c), ]
    if (nrow(d) == 0) next
    l2c <- tapply(d$c, d$l, function(x) length(unique(x)))
    c2l <- tapply(d$l, d$c, function(x) length(unique(x)))
    if (any(l2c > 1) || any(c2l > 1)) bad <- c(bad, paste0(lab, "/", code))
  }

  if (length(bad) == 0) {
    return(result_pass("Paired character/numeric covariates map one-to-one."))
  }
  result_flag(
    message = paste0(
      length(bad), " covariate pair(s) with inconsistent mapping: ",
      paste(bad, collapse = ", "), "."
    ),
    n_flagged = length(bad),
    summary_table = tibble::tibble(pair = bad)
  )
}

#' CORE-COV-003: Possible character truncation
#'
#' Flags character variables where a large share of values share the maximum
#' string length, a possible sign of truncation at a transport limit.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_cov_truncation <- function(data, thresholds) {
  char_cols <- names(data)[vapply(
    data, function(x) is.character(x) || is.factor(x), logical(1)
  )]
  char_cols <- grep("^\\.", char_cols, value = TRUE, invert = TRUE)

  bad <- list()
  for (cv in char_cols) {
    x <- as.character(data[[cv]])
    x <- x[!is.na(x) & x != ""]
    if (length(x) == 0) next
    lens <- nchar(x)
    max_len <- max(lens)
    if (max_len < 20) next
    frac_at_max <- mean(lens == max_len)
    if (frac_at_max >= 0.2 && length(unique(x[lens == max_len])) >= 2) {
      bad[[cv]] <- tibble::tibble(
        variable = cv, max_length = max_len,
        pct_at_max = round(100 * frac_at_max, 1)
      )
    }
  }

  if (length(bad) == 0) {
    return(result_pass("No signs of character truncation."))
  }
  result_flag(
    message = paste0(
      length(bad), " character variable(s) with possible truncation."
    ),
    n_flagged = length(bad),
    summary_table = dplyr::bind_rows(bad)
  )
}

#' CORE-COV-006: Time-varying covariate change review
#'
#' For covariates that vary within a subject (weight, BMI), flags large
#' short-interval relative changes (> 50%) between adjacent records.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_cov_time_varying <- function(data, thresholds) {
  cand <- intersect(c("WT", "BMI"), names(data))
  if (length(cand) == 0 || !"ID" %in% names(data) || !"TIME" %in% names(data)) {
    return(result_skip("No time-varying covariates available to review."))
  }
  time <- suppressWarnings(as.numeric(data$TIME))

  flagged <- list()
  for (cv in cand) {
    x <- suppressWarnings(as.numeric(data[[cv]]))
    varies <- any(tapply(x, data$ID, function(v) {
      length(unique(v[!is.na(v)])) > 1
    }), na.rm = TRUE)
    if (!isTRUE(varies)) next
    for (idx in split(seq_len(nrow(data)), data$ID)) {
      o <- idx[order(time[idx])]
      xv <- x[o]
      rel <- abs(diff(xv)) / pmax(abs(xv[-length(xv)]), 1e-9)
      big <- which(!is.na(rel) & rel > 0.5)
      if (length(big) > 0) {
        flagged[[length(flagged) + 1]] <- tibble::tibble(
          ID = data$ID[o[big + 1]], variable = cv,
          rel_change = round(rel[big], 2)
        )
      }
    }
  }

  if (length(flagged) == 0) {
    return(result_pass("No large short-interval covariate changes."))
  }
  tab <- dplyr::bind_rows(flagged)
  result_flag(
    message = paste0(nrow(tab), " large within-subject covariate change(s)."),
    n_flagged = nrow(tab),
    summary_table = tab,
    subject_list = tibble::tibble(ID = unique(tab$ID))
  )
}
