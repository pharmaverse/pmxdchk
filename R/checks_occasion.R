#' CORE-OCC-001: Occasion and period internal consistency
#'
#' When an occasion / dose-number / period variable is present, flags records
#' where the occasion number decreases as TIME increases within a subject.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_occ_consistency <- function(data, thresholds) {
  occ_col <- intersect(c("OCC", "DOSNO", "PERIOD"), names(data))[1]
  if (is.na(occ_col)) {
    return(result_skip("No occasion/dose-number/period variable present."))
  }
  time <- suppressWarnings(as.numeric(data$TIME))
  occ <- suppressWarnings(as.numeric(data[[occ_col]]))

  flagged <- integer(0)
  for (idx in split(seq_len(nrow(data)), data$ID)) {
    o <- idx[order(time[idx])]
    dec <- c(FALSE, diff(occ[o]) < 0)
    dec[is.na(dec)] <- FALSE
    flagged <- c(flagged, o[dec])
  }

  if (length(flagged) == 0) {
    return(result_pass(paste0(
      occ_col, " is non-decreasing with time within each subject."
    )))
  }
  result_flag(
    message = paste0(
      length(flagged), " record(s) where ", occ_col,
      " decreases as time increases."
    ),
    n_flagged = length(flagged),
    flagged_records = tibble::as_tibble(data[sort(flagged), , drop = FALSE])
  )
}
