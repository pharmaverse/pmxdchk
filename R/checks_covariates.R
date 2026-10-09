#' CORE-COV-001: Fixed covariate consistency within subject
#'
#' Flags subjects whose fixed covariates take more than one distinct value
#' across their records. Fixed covariates are SEX, RACE, ETHNIC, COUNTRY, and
#' every continuous covariate that is not time-varying by design (see
#' [time_varying_covariates()]).
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_cov_fixed_consistency <- function(data, thresholds) {
  covs <- c(
    intersect(c("SEX", "RACE", "ETHNIC", "COUNTRY"), names(data)),
    setdiff(continuous_covariates(data), time_varying_covariates(data))
  )
  if (length(covs) == 0 || !"ID" %in% names(data)) {
    return(result_skip("No fixed covariates available to check."))
  }

  flagged <- list()
  for (cv in covs) {
    values <- tapply(
      as.character(data[[cv]]), as.character(data$ID),
      function(x) paste(sort(unique(x[!is.na(x)])), collapse = " | ")
    )
    bad <- grep(" | ", values, fixed = TRUE)
    if (length(bad) > 0) {
      flagged[[cv]] <- tibble::tibble(
        ID = names(values)[bad], variable = cv,
        values = unname(as.character(values[bad]))
      )
    }
  }

  if (length(flagged) == 0) {
    return(result_pass("Fixed covariates are constant within each subject."))
  }
  tab <- dplyr::bind_rows(flagged)
  result_flag(
    message = paste0(
      nrow(tab), " subject-covariate(s) with inconsistent fixed values (",
      paste(unique(tab$variable), collapse = ", "), ")."
    ),
    n_flagged = nrow(tab),
    summary_table = tab,
    subject_list = tibble::tibble(ID = unique(tab$ID))
  )
}

#' CORE-COV-004: Continuous covariate outlier detection
#'
#' At the subject level (first record per subject), flags continuous covariates
#' outside the boxplot fences, using `thresholds$outlier_iqr_k`.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list; uses `outlier_iqr_k`.
#' @return A partial check result.
#' @noRd
check_cov_outliers <- function(data, thresholds) {
  covs <- continuous_covariates(data)
  if (length(covs) == 0 || !"ID" %in% names(data)) {
    return(result_skip("No continuous covariates available to review."))
  }

  first <- data[!duplicated(data$ID), c("ID", covs), drop = FALSE]
  flagged <- list()
  for (cv in covs) {
    x <- suppressWarnings(as.numeric(first[[cv]]))
    if (sum(!is.na(x)) < 5) next
    out <- which(tukey_outliers(x, thresholds$outlier_iqr_k))
    if (length(out) > 0) {
      flagged[[cv]] <- tibble::tibble(
        ID = as.character(first$ID[out]), variable = cv, value = x[out]
      )
    }
  }

  if (length(flagged) == 0) {
    return(result_pass(paste0(
      "No outliers among ", length(covs), " continuous covariate(s)."
    )))
  }
  tab <- dplyr::bind_rows(flagged)
  result_flag(
    message = paste0(
      nrow(tab), " covariate value(s) outside the boxplot fences (",
      paste(names(flagged), collapse = ", "), ")."
    ),
    n_flagged = nrow(tab),
    summary_table = tab,
    subject_list = tibble::tibble(ID = unique(tab$ID))
  )
}

#' CORE-COV-005: Implausible common covariate values
#'
#' Flags covariate values outside the fixed physiological limits in
#' [cov_plausibility_rules()].
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_cov_implausible <- function(data, thresholds) {
  rules <- cov_plausibility_rules()
  rules$variable <- vapply(
    rules$pattern, function(p) detect_col(names(data), p), character(1),
    USE.NAMES = FALSE
  )
  rules <- rules[!is.na(rules$variable), ]
  if (nrow(rules) == 0 || !"ID" %in% names(data)) {
    return(result_skip("No covariates with plausibility rules present."))
  }

  flagged <- list()
  for (i in seq_len(nrow(rules))) {
    x <- suppressWarnings(as.numeric(data[[rules$variable[i]]]))
    bad <- which(x < rules$lower[i] | x > rules$upper[i])
    if (length(bad) > 0) {
      flagged[[i]] <- tibble::tibble(
        ID = as.character(data$ID[bad]), variable = rules$variable[i],
        value = x[bad],
        plausible_range = paste0(
          rules$lower[i], " - ", rules$upper[i], " ", rules$unit[i]
        )
      )
    }
  }

  if (length(flagged) == 0) {
    return(result_pass("No implausible covariate values detected."))
  }
  tab <- dplyr::distinct(dplyr::bind_rows(flagged))
  result_flag(
    message = paste0(
      nrow(tab), " implausible covariate value(s) (",
      paste(unique(tab$variable), collapse = ", "), ")."
    ),
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

  bad <- list()
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
    conflict <- d$l %in% names(l2c)[l2c > 1] | d$c %in% names(c2l)[c2l > 1]
    if (any(conflict)) {
      bad[[lab]] <- tibble::tibble(
        pair = paste0(lab, "/", code),
        label = d$l[conflict], code = d$c[conflict]
      )
    }
  }

  if (length(bad) == 0) {
    return(result_pass("Paired character/numeric covariates map one-to-one."))
  }
  tab <- dplyr::bind_rows(bad)
  result_flag(
    message = paste0(
      length(bad), " covariate pair(s) with inconsistent mapping: ",
      paste(unique(tab$pair), collapse = ", "), "."
    ),
    n_flagged = length(bad),
    summary_table = tab
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
#' For covariates that are time-varying by design (see
#' [time_varying_covariates()]), flags records outside the overall boxplot
#' fences (`thresholds$outlier_iqr_k`) and relative changes above 50% between
#' consecutive records of a subject.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list; uses `outlier_iqr_k`.
#' @return A partial check result.
#' @noRd
check_cov_time_varying <- function(data, thresholds) {
  cand <- time_varying_covariates(data)
  if (length(cand) == 0) {
    return(result_skip("No time-varying covariates detected."))
  }
  time <- suppressWarnings(as.numeric(data$TIME))
  ord <- order(as.character(data$ID), time)
  same_id <- c(FALSE, data$ID[ord][-1] == data$ID[ord][-length(ord)])

  flagged <- list()
  for (cv in cand) {
    x <- suppressWarnings(as.numeric(data[[cv]]))
    xo <- x[ord]
    previous <- c(NA, xo[-length(xo)])
    jump <- rep(FALSE, nrow(data))
    jump[ord] <- same_id & abs(xo - previous) > 0.5 * abs(previous)
    reason <- rep(NA_character_, nrow(data))
    reason[which(jump)] <- "change above 50% from previous record"
    reason[
      tukey_outliers(x, thresholds$outlier_iqr_k)
    ] <- "outside overall boxplot fences"
    hit <- which(!is.na(reason))
    if (length(hit) > 0) {
      flagged[[cv]] <- tibble::tibble(
        ID = as.character(data$ID[hit]), TIME = time[hit], variable = cv,
        value = x[hit], reason = reason[hit]
      )
    }
  }

  if (length(flagged) == 0) {
    return(result_pass(paste0(
      "No unusual values in time-varying covariate(s): ",
      paste(cand, collapse = ", "), "."
    )))
  }
  tab <- dplyr::bind_rows(flagged)
  result_flag(
    message = paste0(
      nrow(tab), " unusual time-varying covariate value(s) (",
      paste(unique(tab$variable), collapse = ", "), ")."
    ),
    n_flagged = nrow(tab),
    summary_table = tab,
    subject_list = tibble::tibble(ID = unique(tab$ID))
  )
}
