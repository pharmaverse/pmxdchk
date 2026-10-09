#' Register a check in the package registry
#'
#' Each entry mirrors the columns of `docs/NMPK_QC_Checklist.xlsx`: the function
#' implementing the check, its minimum variables, applicable study types,
#' severity, and prerequisite check IDs (the dependency graph the runner sorts
#' on).
#'
#' @param id Character scalar, the check identifier.
#' @param fn The check function, with signature `function(data, thresholds)`
#'   returning a partial result (see [result_pass()]).
#' @param title Human-readable check title.
#' @param domain Check domain (e.g. `"structure"`).
#' @param severity One of `"Critical"`, `"High"`, `"Medium"`.
#' @param min_vars Character vector of canonical variables required; the
#'   check is skipped if any is absent. Use `character()` to always run.
#' @param study_types Character vector of applicable study types
#'   (e.g. `"All"`, `"Multiple Dose"`).
#' @param requires Character vector of prerequisite check IDs.
#' @param rule Plain-language statement of what is flagged, including any
#'   cutoff, shown to the reviewer next to each finding.
#' @param guidance What the reviewer should do when the check flags.
#'
#' @return Invisibly, the registered entry.
#' @noRd
register_check <- function(id, fn, title, domain, severity,
                           min_vars = character(),
                           study_types = "All",
                           requires = character(),
                           rule = "", guidance = "") {
  the$registry[[id]] <- list(
    id = id,
    fn = fn,
    title = title,
    domain = domain,
    severity = severity,
    min_vars = min_vars,
    study_types = study_types,
    requires = requires,
    rule = rule,
    guidance = guidance
  )
  invisible(the$registry[[id]])
}

#' Access the check registry
#'
#' @return A named list of registered check entries.
#' @noRd
check_registry <- function() {
  the$registry
}

#' Register all built-in Public Core checks
#'
#' Called from `.onLoad()`. As new check domains are implemented, register them
#' here.
#'
#' @return Invisibly `NULL`.
#' @noRd
register_builtin_checks <- function() {
  the$registry <- list()

  register_check(
    id = "CORE-STUDYTYPE-001",
    fn = check_study_type,
    title = "Study Type Inference and User Confirmation",
    domain = "studytype",
    severity = "Critical",
    min_vars = character(),
    study_types = "All",
    rule = paste0(
      "Infers study type from the variables present: ADDL/II/SS, an ",
      "occasion variable, or repeated dose records indicate multiple ",
      "dose; RATE/DUR indicate IV; more than one DVID among observation ",
      "records indicates multi-analyte; more than one STUDYID indicates ",
      "an integrated dataset."
    ),
    guidance = paste0(
      "Confirm or correct the study type on the Data tab. It decides ",
      "which conditional checks run."
    )
  )

  register_check(
    id = "CORE-STRUCT-001",
    fn = check_struct_core_variables,
    title = "Core NONMEM Variable Presence",
    domain = "structure",
    severity = "Critical",
    min_vars = character(),
    study_types = "All",
    requires = "CORE-STUDYTYPE-001",
    rule = paste0(
      "ID, TIME, EVID, MDV, DV, and AMT must all be present after ",
      "variable mapping."
    ),
    guidance = paste0(
      "Map the missing variable on the Data tab, or add it to the ",
      "dataset. Checks that need it are skipped until then."
    )
  )

  register_check(
    id = "CORE-STRUCT-002",
    fn = check_struct_numeric_parsable,
    title = "Variable Type and Numeric Parsability",
    domain = "structure",
    severity = "Critical",
    min_vars = character(),
    study_types = "All",
    requires = "CORE-STUDYTYPE-001",
    rule = paste0(
      "ID, TIME, EVID, MDV, DV, AMT, RATE, II, ADDL, CMT, and DVID must ",
      "contain only values that parse as numbers. NONMEM cannot read ",
      "character values in these items."
    ),
    guidance = paste0(
      "Look for text placeholders such as 'BLQ' or '<LLOQ' and for ",
      "character subject identifiers. Recode them to numeric values; map ",
      "a numeric ID rather than USUBJID."
    )
  )

  register_check(
    id = "CORE-STRUCT-003",
    fn = check_struct_evid_validity,
    title = "EVID Value Validity",
    domain = "structure",
    severity = "Critical",
    min_vars = "EVID",
    study_types = "All",
    requires = "CORE-STUDYTYPE-001",
    rule = paste0(
      "EVID must be 0 (observation), 1 (dose), 2 (other), 3 (reset), or 4 ",
      "(reset and dose). Missing EVID is also flagged."
    ),
    guidance = paste0(
      "Correct the event type in the dataset programming; other values ",
      "make NONMEM stop or misread the record."
    )
  )

  register_check(
    id = "CORE-STRUCT-004",
    fn = check_struct_record_counts,
    title = "Observed Record and Dose Record Counts",
    domain = "structure",
    severity = "Medium",
    min_vars = c("ID", "EVID"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003"),
    rule = paste0(
      "Descriptive count of subjects, records, observations (EVID 0), and ",
      "doses (EVID 1 or 4). Never flags."
    ),
    guidance = "Compare the counts with the analysis plan and the source data."
  )

  register_check(
    id = "CORE-STRUCT-005",
    fn = check_struct_duplicate_records,
    title = "Duplicate Records",
    domain = "structure",
    severity = "High",
    min_vars = c("ID", "TIME", "EVID"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001"),
    rule = paste0(
      "Flags records sharing the same ID, TIME, EVID and, when present, ",
      "CMT, DVID, OCC, DOSNO. Each is labelled as an exact duplicate row ",
      "or as the same key with different values (for example a different ",
      "DV or AMT)."
    ),
    guidance = paste0(
      "Remove exact duplicates. For records with the same key but ",
      "different values, check the source: usually a repeated assay ",
      "result or a merge error, and only one should remain."
    )
  )

  register_check(
    id = "CORE-STRUCT-006",
    fn = check_struct_missingness,
    title = "Missingness by Event Type",
    domain = "structure",
    severity = "Medium",
    min_vars = c("EVID", "MDV"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003"),
    rule = paste0(
      "Summarizes missing values per variable and event type. Flags only ",
      "missingness that is not expected: ID, TIME, EVID, or MDV on any ",
      "record, AMT on dose records, and covariates on any record. Missing ",
      "DV on dose records and missing AMT on observation records are ",
      "expected."
    ),
    guidance = paste0(
      "Fill or impute unexpected missing values before modeling; NONMEM ",
      "reads a blank covariate as zero."
    )
  )

  register_check(
    id = "CORE-TIME-001",
    fn = check_time_nondecreasing,
    title = "TIME Non-Decreasing Within Subject",
    domain = "time",
    severity = "Critical",
    min_vars = c("ID", "TIME"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-002"),
    rule = paste0(
      "Reads records in dataset row order, without sorting, within each ",
      "subject (and OCC when present). Flags a record whose TIME is lower ",
      "than the previous record, unless it is a reset record (EVID 3 or ",
      "4)."
    ),
    guidance = paste0(
      "Sort the dataset by ID and TIME. NONMEM stops when time decreases ",
      "within an individual."
    )
  )

  register_check(
    id = "CORE-DOSE-001",
    fn = check_dose_amt_validity,
    title = "Dose AMT Validity",
    domain = "dosing",
    severity = "Critical",
    min_vars = c("EVID", "AMT"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003"),
    rule = paste0(
      "Flags dose records (EVID 1 or 4) with missing, zero, or negative ",
      "AMT, and observation records (EVID 0) with a non-zero AMT."
    ),
    guidance = paste0(
      "Correct the dose amount or the event type. A dose record without a ",
      "positive amount doses nothing."
    )
  )

  register_check(
    id = "CORE-OBS-001",
    fn = check_obs_mdv_dv,
    title = "MDV and DV Consistency",
    domain = "observations",
    severity = "Critical",
    min_vars = c("EVID", "MDV", "DV"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001"),
    rule = paste0(
      "Flags MDV = 0 with a missing DV, observation records with MDV = 1 ",
      "that still carry a non-zero DV, and dose records (EVID 1 or 4) ",
      "that carry a non-zero DV or MDV = 0."
    ),
    guidance = paste0(
      "Set MDV = 1 wherever DV must be ignored, and remove DV from dose ",
      "records. NONMEM requires MDV = 1 on dose records."
    )
  )

  register_check(
    id = "CORE-OBS-005",
    fn = check_obs_outliers,
    title = "Concentration Outlier Detection",
    domain = "observations",
    severity = "High",
    min_vars = c("EVID", "MDV", "DV"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-OBS-001"),
    rule = paste0(
      "Among quantifiable observations with the same nominal time and ",
      "analyte or compartment (or analyte only when no nominal time is ",
      "mapped), flags log concentrations more than the outlier cutoff ",
      "(robust |z|) from the group median."
    ),
    guidance = paste0(
      "Open the subject in Profiles. Check units, sample swaps, and ",
      "dosing history before excluding anything; an outlier is a prompt ",
      "for review, not proof of error."
    )
  )

  register_check(
    id = "CORE-COV-004",
    fn = check_cov_outliers,
    title = "Continuous Covariate Outlier Detection",
    domain = "covariates",
    severity = "Medium",
    min_vars = "ID",
    study_types = "All",
    requires = "CORE-STUDYTYPE-001",
    rule = paste0(
      "Using one value per subject, flags continuous covariates outside ",
      "the boxplot fences Q1 - k x IQR and Q3 + k x IQR, where k is the ",
      "boxplot fence set in Settings (default 3, Tukey's far-out limit)."
    ),
    guidance = paste0(
      "Verify flagged values against the source. Extreme but real values ",
      "can stay; unit errors such as lb versus kg must be corrected."
    )
  )

  register_check(
    id = "CORE-STRUCT-007",
    fn = check_struct_evid4_uniqueness,
    title = "EVID=4 Uniqueness Within Subject and Period",
    domain = "structure",
    severity = "High",
    min_vars = c("ID", "EVID"),
    study_types = c("Multiple Dose", "Integrated"),
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003"),
    rule = paste0(
      "Flags subjects, or subject-periods when OCC, PERIOD, or DOSNO ",
      "exists, with more than one EVID = 4 record."
    ),
    guidance = paste0(
      "Confirm each reset is intended, for example one per crossover ",
      "period. Unintended resets wipe the amounts in all compartments."
    )
  )

  register_check(
    id = "CORE-TIME-002",
    fn = check_time_same_time_events,
    title = "Same-Time Event Pattern Review",
    domain = "time",
    severity = "Medium",
    min_vars = c("ID", "TIME", "EVID"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-002"),
    rule = paste0(
      "Flags two or more dose records (EVID 1 or 4) with the same ID, ",
      "TIME, and CMT."
    ),
    guidance = paste0(
      "Check for duplicated dosing or a merge error. Split doses into ",
      "different compartments at the same time are not flagged."
    )
  )

  register_check(
    id = "CORE-TIME-003",
    fn = check_time_negative,
    title = "Negative TIME Review",
    domain = "time",
    severity = "Medium",
    min_vars = "TIME",
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-002"),
    rule = paste0(
      "Negative TIME is kept as it is and summarized. Flags negative TIME ",
      "on a dose record and, when VISIT is mapped, on a record whose visit ",
      "differs from the visit of the subject's first dose. Other negative ",
      "times are predose samples and are not flagged."
    ),
    guidance = paste0(
      "Correct negative dose times. For samples outside the first-dose ",
      "visit, check the reference dose used to derive TIME."
    )
  )

  register_check(
    id = "CORE-TIME-004",
    fn = check_time_actual_nominal,
    title = "Actual vs Nominal Time Descriptive Difference",
    domain = "time",
    severity = "Medium",
    min_vars = "TIME",
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-002"),
    rule = paste0(
      "Among observations with the same nominal time (and analyte or ",
      "compartment), flags records whose actual-minus-nominal difference ",
      "is more than the outlier cutoff (robust |z|) away from the median ",
      "difference for that time point."
    ),
    guidance = paste0(
      "Check the sample date and time in the source. A large deviation ",
      "can indicate a wrong date or a sample assigned to the wrong ",
      "nominal time."
    )
  )

  register_check(
    id = "CORE-DOSE-002",
    fn = check_dose_distribution,
    title = "Dose Amount Distribution",
    domain = "dosing",
    severity = "High",
    min_vars = c("EVID", "AMT"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003"),
    rule = paste0(
      "Lists the distinct dose amounts. Flags amounts whose log value is ",
      "more than the outlier cutoff (robust |z|) from the median, with a ",
      "minimum spread equal to a 1.5-fold difference so that ordinary ",
      "dose levels are not flagged."
    ),
    guidance = paste0(
      "Check flagged amounts for unit errors (mg versus ug) or data entry ",
      "errors against the exposure source data."
    )
  )

  register_check(
    id = "CORE-DOSE-003",
    fn = check_dose_obs_no_dose,
    title = "Subjects with Observations but No Dose",
    domain = "dosing",
    severity = "Critical",
    min_vars = c("ID", "EVID"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003"),
    rule = paste0(
      "Flags subjects with at least one observation record (EVID 0) and ",
      "no dose record (EVID 1 or 4)."
    ),
    guidance = paste0(
      "Check whether dosing records were lost in a merge or the subject ",
      "was never dosed (for example placebo). Add the doses or exclude ",
      "the subject."
    )
  )

  register_check(
    id = "CORE-DOSE-004",
    fn = check_dose_dose_no_obs,
    title = "Subjects with Dose but No Quantifiable Observation",
    domain = "dosing",
    severity = "Medium",
    min_vars = c("ID", "EVID", "MDV", "DV"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-OBS-001"),
    rule = paste0(
      "Flags subjects with a dose record and no observation having MDV = ",
      "0 and a non-missing DV."
    ),
    guidance = paste0(
      "These subjects do not inform the model. Confirm that all their ",
      "samples are missing or BLQ and decide whether to keep them."
    )
  )

  register_check(
    id = "CORE-OBS-004",
    fn = check_obs_predose,
    title = "Predose Concentration Descriptive Review",
    domain = "observations",
    severity = "Medium",
    min_vars = c("ID", "TIME", "EVID", "MDV", "DV"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-OBS-001"),
    rule = paste0(
      "Flags a quantifiable observation taken at or before the subject's ",
      "first dose whose concentration exceeds the predose cutoff (default ",
      "5%) of that subject's highest post-dose concentration."
    ),
    guidance = paste0(
      "Check for carryover from a previous period, a wrong sample time, ",
      "or a sample swap. Bioequivalence guidance uses the same 5% of Cmax ",
      "criterion."
    )
  )

  register_check(
    id = "CORE-COV-001",
    fn = check_cov_fixed_consistency,
    title = "Fixed Covariate Consistency Within Subject",
    domain = "covariates",
    severity = "High",
    min_vars = "ID",
    study_types = "All",
    requires = "CORE-STUDYTYPE-001",
    rule = paste0(
      "SEX, RACE, ETHNIC, COUNTRY and baseline covariates must have one ",
      "value per subject. A continuous covariate counts as baseline when ",
      "its name ends in BL or when it changes in fewer than half of the ",
      "subjects."
    ),
    guidance = paste0(
      "Correct the inconsistent records. If the covariate is meant to be ",
      "time-varying, the flagged subjects are the exception worth ",
      "checking."
    )
  )

  register_check(
    id = "CORE-COV-005",
    fn = check_cov_implausible,
    title = "Implausible Common Covariate Values",
    domain = "covariates",
    severity = "High",
    min_vars = "ID",
    study_types = "All",
    requires = "CORE-STUDYTYPE-001",
    rule = paste0(
      "Fixed physiological limits, not data-driven: age 0-120 years, ",
      "weight 0.2-500 kg, BMI 5-100 kg/m2, height 20-250 cm, CrCL 0-500 ",
      "mL/min, eGFR 0-300 mL/min/1.73m2. Values outside are flagged."
    ),
    guidance = paste0(
      "These values are impossible in any population. Check for unit ",
      "errors and missing-value codes such as -99."
    )
  )

  register_check(
    id = "CORE-MR-001",
    fn = check_mr_exclusion_flags,
    title = "Exclusion Flag Internal Consistency",
    domain = "readiness",
    severity = "High",
    min_vars = character(),
    study_types = "All",
    requires = "CORE-STUDYTYPE-001",
    rule = paste0(
      "When an exclusion flag exists, flags excluded records without a ",
      "reason and non-excluded records that carry a reason."
    ),
    guidance = paste0(
      "Complete or clear the exclusion reason so that every exclusion is ",
      "traceable."
    )
  )

  register_check(
    id = "CORE-MR-002",
    fn = check_mr_imputation,
    title = "Imputation Traceability Signals",
    domain = "readiness",
    severity = "Medium",
    min_vars = character(),
    study_types = "All",
    requires = "CORE-STUDYTYPE-001",
    rule = paste0(
      "Lists variables named like imputation flags (IMPUT, IMPFL, IMP) ",
      "with the number of records marked. Never flags."
    ),
    guidance = paste0(
      "Confirm that every imputed value is documented in the analysis ",
      "plan."
    )
  )

  register_check(
    id = "CORE-OBS-002",
    fn = check_obs_blq_consistency,
    title = "BLQ/CENS/LLOQ Internal Consistency",
    domain = "observations",
    severity = "Critical",
    min_vars = c("DV", "MDV"),
    study_types = c("All", "Single Dose", "Multiple Dose"),
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-OBS-001"),
    rule = paste0(
      "BLQ or CENS marks a record as below the limit of quantification ",
      "(CENS is the censoring indicator used by likelihood-based BLQ ",
      "methods such as M3). Flags marked records whose DV is above LLOQ, ",
      "and unmarked observations with MDV = 0 whose DV is below LLOQ."
    ),
    guidance = paste0(
      "Correct the BLQ flag, DV, or LLOQ so they agree with the ",
      "bioanalytical report."
    )
  )

  register_check(
    id = "CORE-OBS-003",
    fn = check_obs_blq_middle,
    title = "BLQ in Middle of Profile",
    domain = "observations",
    severity = "Medium",
    min_vars = c("ID", "TIME", "DV"),
    study_types = c("All", "Single Dose", "Multiple Dose"),
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-OBS-001"),
    rule = paste0(
      "Flags a BLQ observation that has a quantifiable observation both ",
      "before and after it within the same subject, analyte, and dosing ",
      "interval. Trough samples before the next dose are not flagged."
    ),
    guidance = paste0(
      "Review the profile. An isolated BLQ between quantifiable samples ",
      "may be a sample swap or assay issue; decide how to handle it and ",
      "document the decision."
    )
  )

  register_check(
    id = "CORE-OBS-006",
    fn = check_obs_min_records,
    title = "Minimum Quantifiable PK Records Per Subject or Occasion",
    domain = "observations",
    severity = "Medium",
    min_vars = c("ID", "EVID", "MDV", "DV"),
    study_types = c("Single Dose", "Multiple Dose"),
    requires = c("CORE-STUDYTYPE-001", "CORE-OBS-001"),
    rule = paste0(
      "Flags subjects, or subject-occasions when OCC exists, with fewer ",
      "quantifiable observations (MDV = 0, non-missing DV) than the ",
      "minimum set in Settings."
    ),
    guidance = paste0(
      "Decide whether these subjects carry enough information to keep in ",
      "the analysis."
    )
  )

  register_check(
    id = "CORE-DOSE-005",
    fn = check_dose_addl_ii,
    title = "ADDL and II Basic Validity",
    domain = "dosing",
    severity = "High",
    min_vars = "EVID",
    study_types = "Multiple Dose",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003"),
    rule = paste0(
      "Flags negative or non-integer ADDL, ADDL above zero on a non-dose ",
      "record, and ADDL above zero with a missing or non-positive II."
    ),
    guidance = paste0(
      "ADDL needs a positive dosing interval II on the same dose record. ",
      "Correct the pair."
    )
  )

  register_check(
    id = "CORE-DOSE-006",
    fn = check_dose_rate_dur,
    title = "Infusion RATE and DUR Basic Validity",
    domain = "dosing",
    severity = "Critical",
    min_vars = c("EVID", "AMT"),
    study_types = "IV",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003"),
    rule = paste0(
      "Flags negative RATE other than -1 (rate estimated) and -2 ",
      "(duration estimated), a non-zero RATE on a non-dose record, a ",
      "missing DUR on a RATE = -2 record when the dataset has a DUR ",
      "column, and AMT / RATE differing from DUR by more than 5%. ",
      "Placeholders such as -99 are not valid NONMEM values."
    ),
    guidance = paste0(
      "Replace placeholder values and reconcile AMT, RATE, and DUR ",
      "against the infusion records."
    )
  )

  register_check(
    id = "CORE-DOSE-007",
    fn = check_dose_ss,
    title = "SS Flag Basic Validity",
    domain = "dosing",
    severity = "High",
    min_vars = "EVID",
    study_types = "Multiple Dose",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-003"),
    rule = paste0(
      "Flags SS values other than 0 or 1, SS = 1 on a non-dose record, ",
      "and SS = 1 with a missing or non-positive II."
    ),
    guidance = paste0(
      "A steady-state dose needs SS = 1 together with a positive II on ",
      "the dose record."
    )
  )

  register_check(
    id = "CORE-OCC-001",
    fn = check_occ_consistency,
    title = "Occasion and Period Internal Consistency",
    domain = "occasion",
    severity = "High",
    min_vars = c("ID", "TIME"),
    study_types = c("Multiple Dose", "Integrated"),
    requires = c("CORE-STUDYTYPE-001", "CORE-TIME-001"),
    rule = paste0(
      "Orders each subject's records by TIME and flags a record whose ",
      "occasion (OCC, DOSNO, or PERIOD) is lower than the record before ",
      "it. This also detects overlapping occasion time ranges."
    ),
    guidance = paste0(
      "Correct the occasion assignment or the time of the flagged ",
      "records."
    )
  )

  register_check(
    id = "CORE-CMP-001",
    fn = check_cmp_cmt,
    title = "CMT Basic Consistency",
    domain = "compartment",
    severity = "Medium",
    min_vars = "EVID",
    study_types = "Multi-analyte",
    requires = "CORE-STUDYTYPE-001",
    rule = paste0(
      "When CMT is populated on some records, flags records where it is ",
      "missing. The summary shows CMT by event type."
    ),
    guidance = paste0(
      "Populate CMT on every record; NONMEM otherwise applies the default ",
      "compartment."
    )
  )

  register_check(
    id = "CORE-CMP-002",
    fn = check_cmp_dvid_analyte,
    title = "DVID and Analyte Internal Consistency",
    domain = "compartment",
    severity = "High",
    min_vars = "DVID",
    study_types = "Multi-analyte",
    requires = c("CORE-STUDYTYPE-001", "CORE-OBS-001"),
    rule = paste0(
      "Each DVID must map to exactly one analyte label (PARAM, PARAMCD, ",
      "or ANALYTE)."
    ),
    guidance = paste0(
      "Correct the DVID coding so that one identifier never mixes ",
      "analytes."
    )
  )

  register_check(
    id = "CORE-CMP-003",
    fn = check_cmp_magnitude,
    title = "Concentration Magnitude Review Across DVID",
    domain = "compartment",
    severity = "Medium",
    min_vars = c("DVID", "DV", "MDV"),
    study_types = "Multi-analyte",
    requires = c("CORE-STUDYTYPE-001", "CORE-CMP-002", "CORE-OBS-005"),
    rule = paste0(
      "Compares the median concentration between DVID groups and flags a ",
      "difference of more than 1000-fold."
    ),
    guidance = paste0(
      "Large differences can be real (parent versus metabolite) or a unit ",
      "mismatch between analytes. Confirm the units."
    )
  )

  register_check(
    id = "CORE-INT-001",
    fn = check_int_id_uniqueness,
    title = "Subject ID Uniqueness Across Studies",
    domain = "integrated",
    severity = "Critical",
    min_vars = "ID",
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-003"),
    rule = paste0(
      "The numeric ID used by NONMEM must identify one subject: flags an ",
      "ID that maps to more than one USUBJID or appears in more than one ",
      "study, and a USUBJID that maps to more than one ID."
    ),
    guidance = paste0(
      "Re-derive ID so that it is unique across the pooled dataset. ",
      "NONMEM would otherwise merge different subjects into one ",
      "individual."
    )
  )

  register_check(
    id = "CORE-INT-002",
    fn = check_int_categorical,
    title = "Cross-Study Categorical Coding Consistency",
    domain = "integrated",
    severity = "High",
    min_vars = "STUDYID",
    study_types = "Integrated",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-003"),
    rule = paste0(
      "Flags SEX, RACE, ETHNIC, or ROUTE when two studies share no value ",
      "at all, for example M/F in one study and Male/Female in another. ",
      "Code-to-label mapping is covered by CORE-COV-002."
    ),
    guidance = "Harmonize the coding across studies before pooling."
  )

  register_check(
    id = "CORE-INT-003",
    fn = check_int_numeric,
    title = "Cross-Study Numeric Distribution Review",
    domain = "integrated",
    severity = "High",
    min_vars = "STUDYID",
    study_types = "Integrated",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-003"),
    rule = paste0(
      "Compares the per-study median of each continuous covariate and ",
      "flags a covariate whose highest and lowest study medians differ ",
      "more than 2-fold. Dose amounts are listed but not flagged."
    ),
    guidance = paste0(
      "Check for unit differences between studies (kg versus lb, cm ",
      "versus inch). Different populations, such as pediatric and adult, ",
      "can explain a real difference."
    )
  )

  register_check(
    id = "CORE-COV-002",
    fn = check_cov_char_numeric,
    title = "Character-Numeric Mapping Consistency",
    domain = "covariates",
    severity = "High",
    min_vars = character(),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-003"),
    rule = paste0(
      "For each label / code pair named X and XN (for example SEX and ",
      "SEXN), every label must map to one code and every code to one ",
      "label."
    ),
    guidance = paste0(
      "Fix the decode in the dataset programming so that the numeric ",
      "covariate used in the model matches its label."
    )
  )

  register_check(
    id = "CORE-COV-003",
    fn = check_cov_truncation,
    title = "Possible Character Truncation",
    domain = "covariates",
    severity = "Medium",
    min_vars = character(),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003"),
    rule = paste0(
      "Flags character variables at least 20 characters long where 20% or ",
      "more of the values have exactly the maximum length, which suggests ",
      "truncation at a transport limit."
    ),
    guidance = paste0(
      "Compare with the source values and increase the variable length if ",
      "they were cut."
    )
  )

  register_check(
    id = "CORE-COV-006",
    fn = check_cov_time_varying,
    title = "Time-Varying Covariate Change Review",
    domain = "covariates",
    severity = "Medium",
    min_vars = c("ID", "TIME"),
    study_types = c("All", "Multiple Dose"),
    requires = c("CORE-STUDYTYPE-001", "CORE-TIME-001", "CORE-COV-004"),
    rule = paste0(
      "For covariates that change within most subjects, flags records ",
      "outside the overall boxplot fences (k from Settings) and changes ",
      "of more than 50% between consecutive records of a subject."
    ),
    guidance = paste0(
      "Check flagged values against the source vital signs or laboratory ",
      "data."
    )
  )

  # Registered last so it can summarize every other result.
  register_check(
    id = "CORE-MR-003",
    fn = check_mr_plot_ready,
    title = "Plot-Ready Flagged Subject Review",
    domain = "readiness",
    severity = "Medium",
    min_vars = c("ID", "EVID", "MDV", "DV"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-OBS-001"),
    rule = paste0(
      "Lists every subject flagged by at least one other check, with the ",
      "checks that flagged them. Never flags."
    ),
    guidance = "Review these subjects in the Profiles tab."
  )

  invisible(NULL)
}

#' Catalogue of available checks
#'
#' Lists every check the package runs, with the rule it applies and what to do
#' when it flags.
#'
#' @return A tibble with one row per check: `check_id`, `title`, `domain`,
#'   `severity`, `study_types`, `rule`, `guidance`.
#' @export
#' @examples
#' check_catalogue()
check_catalogue <- function() {
  purrr::map_dfr(check_registry(), function(spec) {
    tibble::tibble(
      check_id = spec$id,
      title = spec$title,
      domain = spec$domain,
      severity = spec$severity,
      study_types = paste(spec$study_types, collapse = ", "),
      rule = spec$rule,
      guidance = spec$guidance
    )
  })
}
