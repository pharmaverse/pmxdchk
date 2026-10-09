#' Robust (MAD-based) outlier flags
#'
#' Marks robust outliers in a numeric vector. When the MAD is zero (the bulk of
#' values are identical), any value differing from the median is treated as an
#' outlier, so a lone extreme value among constants is still caught.
#'
#' @param x Numeric vector.
#' @param nmad Cutoff in MAD units.
#' @return A logical vector the length of `x`; `NA` entries are `FALSE`.
#' @importFrom stats mad median
#' @noRd
robust_outliers <- function(x, nmad) {
  center <- median(x, na.rm = TRUE)
  scale <- mad(x, na.rm = TRUE)
  out <- if (is.na(scale) || scale == 0) {
    !is.na(x) & x != center
  } else {
    !is.na(x) & abs(x - center) / scale > nmad
  }
  out[is.na(out)] <- FALSE
  out
}

#' Detect the first column name matching a pattern
#'
#' @param cols Character vector of column names.
#' @param pattern A regular expression (matched case-insensitively).
#' @return The first matching column name, or `NA_character_`.
#' @noRd
detect_col <- function(cols, pattern) {
  hit <- grep(pattern, cols, ignore.case = TRUE, value = TRUE)
  if (length(hit) == 0) NA_character_ else hit[1]
}

#' Whether a vector indicates a positive flag
#'
#' Treats numeric 1 and the strings "Y"/"1"/"TRUE" (any case) as positive.
#'
#' @param x A vector.
#' @return A logical vector the length of `x`; `NA` becomes `FALSE`.
#' @noRd
is_positive_flag <- function(x) {
  out <- if (is.numeric(x)) {
    x == 1
  } else {
    toupper(trimws(as.character(x))) %in% c("Y", "1", "TRUE")
  }
  out[is.na(out)] <- FALSE
  out
}

#' Tukey (boxplot) outlier flags
#'
#' Marks values beyond the boxplot fences `Q1 - k * IQR` and `Q3 + k * IQR`.
#'
#' @param x Numeric vector.
#' @param k Fence multiplier (1.5 is the conventional boxplot whisker).
#' @return A logical vector the length of `x`; `NA` entries are `FALSE`.
#' @importFrom stats quantile
#' @noRd
tukey_outliers <- function(x, k) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  iqr <- q[2] - q[1]
  !is.na(x) & (x < q[1] - k * iqr | x > q[2] + k * iqr)
}

#' Observation grouping label
#'
#' Labels each record with the analyte / matrix it belongs to, using `DVID` and
#' `CMT` only when they take more than one value among observation records.
#' Distribution-based checks compare like with like within this group.
#'
#' @param data A mapped NMPK dataset.
#' @return A character vector the length of `data`; `""` when ungrouped.
#' @noRd
obs_group <- function(data) {
  obs <- if ("EVID" %in% names(data)) {
    suppressWarnings(as.numeric(data$EVID)) %in% 0
  } else {
    rep(TRUE, nrow(data))
  }
  grp <- rep("", nrow(data))
  for (v in intersect(c("DVID", "CMT"), names(data))) {
    x <- data[[v]]
    if (length(unique(x[obs & !is.na(x)])) > 1) {
      grp <- paste0(grp, v, "=", x, " ")
    }
  }
  trimws(grp)
}

#' Continuous covariates available in a dataset
#'
#' The canonical `AGE`, `WT`, `BMI` plus numeric columns named like common
#' baseline covariates (height, BSA, renal and hepatic labs, with or without a
#' `BL` suffix).
#'
#' @param data A mapped NMPK dataset.
#' @return A character vector of column names.
#' @noRd
continuous_covariates <- function(data) {
  other <- grep(
    "^(HT|HEIGHT|BSA|CRCL|EGFR|CREAT|SCR|ALB|ALT|AST|TBIL|BILI)(BL)?$",
    names(data), ignore.case = TRUE, value = TRUE
  )
  other <- other[vapply(data[other], is.numeric, logical(1))]
  union(intersect(c("AGE", "WT", "BMI"), names(data)), other)
}

#' Continuous covariates that are time-varying by design
#'
#' A covariate is treated as time-varying when it takes more than one value in
#' at least half of the subjects. Columns with a `BL` suffix are baseline by
#' definition. Covariates that vary in only a few subjects are treated as fixed,
#' so those subjects are reported as inconsistencies.
#'
#' @param data A mapped NMPK dataset.
#' @return A character vector of column names.
#' @noRd
time_varying_covariates <- function(data) {
  covs <- grep("BL$", continuous_covariates(data), value = TRUE, invert = TRUE)
  if (!"ID" %in% names(data)) {
    return(character())
  }
  varies <- vapply(covs, function(cv) {
    n <- tapply(data[[cv]], data$ID, function(x) length(unique(x[!is.na(x)])))
    isTRUE(mean(n > 1, na.rm = TRUE) >= 0.5)
  }, logical(1))
  covs[varies]
}

#' Plausibility ranges for common covariates
#'
#' Conservative physiological limits used by `CORE-COV-005`. Values below
#' `lower` or above `upper` are implausible in any human population.
#'
#' @return A tibble with columns `covariate`, `pattern` (regular expression
#'   matched against column names), `lower`, `upper`, and `unit`.
#' @noRd
cov_plausibility_rules <- function() {
  tibble::tibble(
    covariate = c("Age", "Body weight", "BMI", "Height", "CrCL", "eGFR"),
    pattern = c(
      "^AGE$", "^(WT|WEIGHT)(BL)?$", "^BMI(BL)?$", "^(HT|HEIGHT)(BL)?$",
      "^CRCL(BL)?$", "^EGFR(BL)?$"
    ),
    lower = c(0, 0.2, 5, 20, 0, 0),
    upper = c(120, 500, 100, 250, 500, 300),
    unit = c("years", "kg", "kg/m2", "cm", "mL/min", "mL/min/1.73m2")
  )
}

#' Subjects implicated by a check result
#'
#' Collects subject IDs from the flagged records, the subject list, and the
#' summary table of a result.
#'
#' @param result A `pmxdchk_check_result` object.
#' @return A character vector of unique subject IDs.
#' @noRd
result_subject_ids <- function(result) {
  ids <- c(
    as.character(result$flagged_records[["ID"]]),
    as.character(result$subject_list[["ID"]]),
    as.character(result$summary_table[["ID"]])
  )
  unique(ids[!is.na(ids)])
}
