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
