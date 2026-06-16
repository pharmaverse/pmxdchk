#' Canonical NMPK variable dictionary
#'
#' Maps canonical NONMEM/ADPPK variable names to common aliases (including CDISC
#' ADPPK names) used to auto-guess the variable mapping. Matching is
#' case-insensitive.
#'
#' @return A named list; each element is a character vector of aliases for the
#'   canonical name given by the element's name.
#' @noRd
nmpk_var_dictionary <- function() {
  list(
    ID      = c("ID", "USUBJIDN", "USUBJID", "SUBJIDN", "SUBJID"),
    TIME    = c("TIME", "AFRLT", "TAFD", "APRLT", "RELTIME"),
    NTIME   = c("NTIME", "NFRLT", "NPRLT", "TADNOM"),
    EVID    = c("EVID"),
    MDV     = c("MDV"),
    DV      = c("DV", "DVOR", "AVAL"),
    AMT     = c("AMT", "DOSEA"),
    CMT     = c("CMT"),
    RATE    = c("RATE"),
    DUR     = c("DUR"),
    II      = c("II"),
    ADDL    = c("ADDL"),
    SS      = c("SS"),
    DVID    = c("DVID", "PARAMN", "PARAMCD"),
    OCC     = c("OCC", "OCC1"),
    DOSNO   = c("DOSNO", "DOSENO"),
    PERIOD  = c("PERIOD", "APERIOD"),
    STUDYID = c("STUDYID", "STUDYIDN"),
    ROUTE   = c("ROUTE", "ROUTEN"),
    AGE     = c("AGE"),
    WT      = c("WT", "WTBL", "WEIGHT", "BW"),
    BMI     = c("BMI", "BMIBL"),
    SEX     = c("SEX", "SEXN"),
    RACE    = c("RACE", "RACEN")
  )
}

#' Human-readable labels for canonical variables
#'
#' Short descriptions shown next to each canonical name in the mapping UI so the
#' role of a variable is clear without knowing NONMEM conventions.
#'
#' @return A named character vector: names are canonical variables, values are
#'   descriptions.
#' @noRd
nmpk_var_labels <- function() {
  c(
    ID      = "Subject identifier",
    TIME    = "Actual time after first dose",
    NTIME   = "Nominal (planned) time",
    EVID    = "Event type (0 obs, 1 dose, 2 other, 3 reset, 4 reset+dose)",
    MDV     = "Missing dependent variable flag",
    DV      = "Dependent variable (observed concentration)",
    AMT     = "Dose amount",
    CMT     = "Compartment number",
    RATE    = "Infusion rate",
    DUR     = "Infusion duration",
    II      = "Interdose interval",
    ADDL    = "Number of additional doses",
    SS      = "Steady-state flag",
    DVID    = "Observation / analyte identifier",
    OCC     = "Occasion",
    DOSNO   = "Dose number",
    PERIOD  = "Study period",
    STUDYID = "Study identifier",
    ROUTE   = "Route of administration",
    AGE     = "Age",
    WT      = "Body weight",
    BMI     = "Body mass index",
    SEX     = "Sex",
    RACE    = "Race"
  )
}

#' Auto-guess a canonical-to-user-column mapping
#'
#' For each canonical variable, returns the first alias present in `columns`
#' (case-insensitive). Datasets that already use canonical names resolve to a
#' near-identity mapping, supporting the frictionless path in the upload module.
#'
#' @param columns Character vector of column names in the uploaded dataset.
#' @return A named character vector: names are canonical variables, values the
#'   matched user column. Only resolved variables are included.
#' @noRd
guess_mapping <- function(columns) {
  dict <- nmpk_var_dictionary()
  lower <- tolower(columns)
  out <- character()
  for (canon in names(dict)) {
    for (alias in dict[[canon]]) {
      m <- which(lower == tolower(alias))
      if (length(m) > 0) {
        out[[canon]] <- columns[m[1]]
        break
      }
    }
  }
  out
}
