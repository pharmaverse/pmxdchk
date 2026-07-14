#' Example ADPPK dataset (clean)
#'
#' A copy of the ADPPK analysis dataset from \pkg{pharmaverseadam}, provided as a
#' built-in clean example for trying the app and the checks. It uses CDISC ADPPK
#' variable names (e.g. `USUBJIDN`, `AFRLT`, `NFRLT`, `WTBL`, `PARAMN`), which
#' the app's variable mapping resolves to canonical NONMEM names.
#'
#' @format A data frame with one row per event record and CDISC ADPPK columns
#'   including `USUBJIDN`, `STUDYIDN`, `AFRLT`, `NFRLT`, `EVID`, `MDV`, `DV`,
#'   `AMT`, `CMT`, `II`, `SS`, `PARAMN`, and baseline covariates.
#' @source \code{pharmaverseadam::adppk}
"adppk_example"

#' Example ADPPK dataset with injected data-quality issues
#'
#' A variant of [adppk_example] with known problems injected across several
#' domains so the checks and the profile flag overlay have something to detect.
#' Injected issues include an invalid EVID, a zero and an extreme dose amount, a
#' decreasing time, a high predose and an extreme concentration, an MDV/DV
#' inconsistency, an inconsistent within-subject SEX, an implausible baseline
#' weight, a subject with observations but no dose, a duplicate record, and an
#' exclusion flag without a reason (via an added `EXCLFL` column).
#'
#' @format The same structure as [adppk_example] plus an `EXCLFL` column.
#' @source Derived from \code{pharmaverseadam::adppk}; see
#'   \code{data-raw/corrupted_examples.R}.
"adppk_corrupted"
