#' CORE-STRUCT-001: Core NONMEM variable presence
#'
#' Checks whether the core modeling variables are present. When one is absent it
#' is reported so dependent checks can be skipped by the runner.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused; kept for a uniform signature).
#' @return A partial check result.
#' @noRd
check_struct_core_variables <- function(data, thresholds) {
  core <- c("ID", "TIME", "EVID", "MDV", "DV", "AMT")
  missing <- setdiff(core, names(data))
  if (length(missing) == 0) {
    return(result_pass(
      "All core NONMEM variables present (ID, TIME, EVID, MDV, DV, AMT)."
    ))
  }
  result_flag(
    message = paste0(
      "Missing core variable(s): ", paste(missing, collapse = ", "), "."
    ),
    n_flagged = length(missing),
    summary_table = tibble::tibble(variable = missing, status = "missing")
  )
}

#' CORE-STRUCT-005: Duplicate records
#'
#' Flags records that duplicate a common key (`ID`, `TIME`, `EVID`, plus
#' `CMT`/`DVID`/`OCC`/`DOSNO` when present). Without a data specification these
#' are reported as potential duplicates for review.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_struct_duplicate_records <- function(data, thresholds) {
  keys <- intersect(
    c("ID", "TIME", "EVID", "CMT", "DVID", "OCC", "DOSNO"), names(data)
  )
  key_tbl <- data[, keys, drop = FALSE]
  dup <- duplicated(key_tbl) | duplicated(key_tbl, fromLast = TRUE)
  n <- sum(dup)
  if (n == 0) {
    return(result_pass("No duplicate keys detected."))
  }
  result_flag(
    message = paste0(
      n, " record(s) share a duplicate key (",
      paste(keys, collapse = " + "), ")."
    ),
    n_flagged = n,
    flagged_records = tibble::as_tibble(data[dup, , drop = FALSE])
  )
}

#' CORE-STRUCT-002: Variable type and numeric parsability
#'
#' Flags expected-numeric variables that contain values which cannot be parsed
#' as numbers.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_struct_numeric_parsable <- function(data, thresholds) {
  expected <- intersect(
    c("TIME", "EVID", "MDV", "DV", "AMT", "RATE", "II", "ADDL", "CMT", "DVID"),
    names(data)
  )
  n_bad <- vapply(expected, function(v) {
    x <- data[[v]]
    if (is.numeric(x)) {
      return(0L)
    }
    chr <- as.character(x)
    chr <- chr[!is.na(chr) & trimws(chr) != ""]
    suppressWarnings(sum(is.na(as.numeric(chr))))
  }, integer(1))

  bad <- n_bad[n_bad > 0]
  if (length(bad) == 0) {
    return(result_pass("All expected numeric variables are parsable as numeric."))
  }
  result_flag(
    message = paste0(
      "Non-numeric values in: ", paste(names(bad), collapse = ", "), "."
    ),
    n_flagged = sum(bad),
    summary_table = tibble::tibble(
      variable = names(bad), n_unparsable = unname(bad)
    )
  )
}

#' CORE-STRUCT-003: EVID value validity
#'
#' Flags records whose EVID is outside the standard set 0, 1, 2, 3, 4.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_struct_evid_validity <- function(data, thresholds) {
  evid <- suppressWarnings(as.numeric(data$EVID))
  invalid <- !is.na(evid) & !(evid %in% c(0, 1, 2, 3, 4))
  n <- sum(invalid)
  if (n == 0) {
    return(result_pass("EVID contains only standard values (0-4)."))
  }
  result_flag(
    message = paste0(n, " record(s) with invalid EVID value(s)."),
    n_flagged = n,
    flagged_records = tibble::as_tibble(data[invalid, , drop = FALSE])
  )
}

#' CORE-STRUCT-004: Observed record and dose record counts
#'
#' Descriptive record-count summary that feeds the Overview dashboard.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_struct_record_counts <- function(data, thresholds) {
  evid <- suppressWarnings(as.numeric(data$EVID))
  summary <- tibble::tibble(
    metric = c("subjects", "records", "observations", "doses"),
    value = c(
      dplyr::n_distinct(data$ID),
      nrow(data),
      sum(evid == 0, na.rm = TRUE),
      sum(evid %in% c(1, 4), na.rm = TRUE)
    )
  )
  result_pass(
    message = paste0(
      summary$value[1], " subjects, ", nrow(data), " records (",
      summary$value[3], " observations, ", summary$value[4], " doses)."
    ),
    summary_table = summary
  )
}

#' CORE-STRUCT-006: Missingness by variable
#'
#' Descriptive missingness summary that feeds the Overview dashboard. Internal
#' bookkeeping columns (prefixed with `.`) are excluded.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_struct_missingness <- function(data, thresholds) {
  cols <- grep("^\\.", names(data), value = TRUE, invert = TRUE)
  n_missing <- vapply(cols, function(v) sum(is.na(data[[v]])), integer(1))
  tab <- tibble::tibble(
    variable = cols,
    n_missing = unname(n_missing),
    pct_missing = round(100 * unname(n_missing) / nrow(data), 1)
  )
  tab <- tab[tab$n_missing > 0, ]
  result_pass(
    message = paste0(nrow(tab), " variable(s) contain missing values."),
    summary_table = tab
  )
}

#' CORE-STRUCT-007: EVID=4 uniqueness within subject and period
#'
#' When reset-dose records (EVID = 4) are present, flags subjects (or
#' subject-periods, when an occasion/period/dose-number variable exists) that
#' carry more than one reset record, for review.
#'
#' @param data A mapped NMPK dataset.
#' @param thresholds A thresholds list (unused).
#' @return A partial check result.
#' @noRd
check_struct_evid4_uniqueness <- function(data, thresholds) {
  evid <- suppressWarnings(as.numeric(data$EVID))
  resets <- which(evid == 4)
  if (length(resets) == 0) {
    return(result_skip("No EVID=4 reset-dose records present."))
  }

  period_var <- intersect(c("OCC", "PERIOD", "DOSNO"), names(data))[1]
  key <- if (is.na(period_var)) {
    as.character(data$ID[resets])
  } else {
    paste(data$ID[resets], data[[period_var]][resets], sep = "_")
  }

  counts <- table(key)
  dup_keys <- names(counts)[counts > 1]
  if (length(dup_keys) == 0) {
    return(result_pass(
      "Each subject/period has at most one EVID=4 reset record."
    ))
  }
  flagged <- resets[key %in% dup_keys]
  result_flag(
    message = paste0(
      length(flagged),
      " EVID=4 record(s) in subjects/periods with multiple resets."
    ),
    n_flagged = length(flagged),
    flagged_records = tibble::as_tibble(data[flagged, , drop = FALSE])
  )
}
