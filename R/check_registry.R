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
#'
#' @return Invisibly, the registered entry.
#' @noRd
register_check <- function(id, fn, title, domain, severity,
                           min_vars = character(),
                           study_types = "All",
                           requires = character()) {
  the$registry[[id]] <- list(
    id = id,
    fn = fn,
    title = title,
    domain = domain,
    severity = severity,
    min_vars = min_vars,
    study_types = study_types,
    requires = requires
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
    study_types = "All"
  )

  register_check(
    id = "CORE-STRUCT-001",
    fn = check_struct_core_variables,
    title = "Core NONMEM Variable Presence",
    domain = "structure",
    severity = "Critical",
    min_vars = character(),
    study_types = "All",
    requires = "CORE-STUDYTYPE-001"
  )

  register_check(
    id = "CORE-STRUCT-002",
    fn = check_struct_numeric_parsable,
    title = "Variable Type and Numeric Parsability",
    domain = "structure",
    severity = "Critical",
    min_vars = character(),
    study_types = "All",
    requires = "CORE-STUDYTYPE-001"
  )

  register_check(
    id = "CORE-STRUCT-003",
    fn = check_struct_evid_validity,
    title = "EVID Value Validity",
    domain = "structure",
    severity = "Critical",
    min_vars = "EVID",
    study_types = "All",
    requires = "CORE-STUDYTYPE-001"
  )

  register_check(
    id = "CORE-STRUCT-004",
    fn = check_struct_record_counts,
    title = "Observed Record and Dose Record Counts",
    domain = "structure",
    severity = "Medium",
    min_vars = c("ID", "EVID"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003")
  )

  register_check(
    id = "CORE-STRUCT-005",
    fn = check_struct_duplicate_records,
    title = "Duplicate Records",
    domain = "structure",
    severity = "High",
    min_vars = c("ID", "TIME", "EVID"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001")
  )

  register_check(
    id = "CORE-STRUCT-006",
    fn = check_struct_missingness,
    title = "Missingness by Variable",
    domain = "structure",
    severity = "Medium",
    min_vars = c("EVID", "MDV"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003")
  )

  register_check(
    id = "CORE-TIME-001",
    fn = check_time_nondecreasing,
    title = "TIME Non-Decreasing Within Subject",
    domain = "time",
    severity = "Critical",
    min_vars = c("ID", "TIME"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-002")
  )

  register_check(
    id = "CORE-DOSE-001",
    fn = check_dose_amt_validity,
    title = "Dose AMT Validity",
    domain = "dosing",
    severity = "Critical",
    min_vars = c("EVID", "AMT"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003")
  )

  register_check(
    id = "CORE-OBS-001",
    fn = check_obs_mdv_dv,
    title = "MDV and DV Consistency",
    domain = "observations",
    severity = "Critical",
    min_vars = c("EVID", "MDV", "DV"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001")
  )

  register_check(
    id = "CORE-OBS-005",
    fn = check_obs_outliers,
    title = "Concentration Outlier Detection",
    domain = "observations",
    severity = "High",
    min_vars = c("EVID", "MDV", "DV"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-OBS-001")
  )

  register_check(
    id = "CORE-COV-004",
    fn = check_cov_outliers,
    title = "Continuous Covariate Outlier Detection",
    domain = "covariates",
    severity = "Medium",
    min_vars = "ID",
    study_types = "All",
    requires = "CORE-STUDYTYPE-001"
  )

  register_check(
    id = "CORE-STRUCT-007",
    fn = check_struct_evid4_uniqueness,
    title = "EVID=4 Uniqueness Within Subject and Period",
    domain = "structure",
    severity = "High",
    min_vars = c("ID", "EVID"),
    study_types = c("Multiple Dose", "Integrated"),
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003")
  )

  register_check(
    id = "CORE-TIME-002",
    fn = check_time_same_time_events,
    title = "Same-Time Event Pattern Review",
    domain = "time",
    severity = "Medium",
    min_vars = c("ID", "TIME", "EVID"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-002")
  )

  register_check(
    id = "CORE-TIME-003",
    fn = check_time_negative,
    title = "Negative TIME Review",
    domain = "time",
    severity = "Medium",
    min_vars = "TIME",
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-002")
  )

  register_check(
    id = "CORE-TIME-004",
    fn = check_time_actual_nominal,
    title = "Actual vs Nominal Time Descriptive Difference",
    domain = "time",
    severity = "Medium",
    min_vars = "TIME",
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-002")
  )

  register_check(
    id = "CORE-DOSE-002",
    fn = check_dose_distribution,
    title = "Dose Amount Distribution",
    domain = "dosing",
    severity = "High",
    min_vars = c("EVID", "AMT"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003")
  )

  register_check(
    id = "CORE-DOSE-003",
    fn = check_dose_obs_no_dose,
    title = "Subjects with Observations but No Dose",
    domain = "dosing",
    severity = "Critical",
    min_vars = c("ID", "EVID"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003")
  )

  register_check(
    id = "CORE-DOSE-004",
    fn = check_dose_dose_no_obs,
    title = "Subjects with Dose but No Quantifiable Observation",
    domain = "dosing",
    severity = "Medium",
    min_vars = c("ID", "EVID", "MDV", "DV"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-OBS-001")
  )

  register_check(
    id = "CORE-OBS-004",
    fn = check_obs_predose,
    title = "Predose Concentration Descriptive Review",
    domain = "observations",
    severity = "Medium",
    min_vars = c("TIME", "EVID", "MDV", "DV"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-OBS-001")
  )

  register_check(
    id = "CORE-COV-001",
    fn = check_cov_fixed_consistency,
    title = "Fixed Covariate Consistency Within Subject",
    domain = "covariates",
    severity = "High",
    min_vars = "ID",
    study_types = "All",
    requires = "CORE-STUDYTYPE-001"
  )

  register_check(
    id = "CORE-COV-005",
    fn = check_cov_implausible,
    title = "Implausible Common Covariate Values",
    domain = "covariates",
    severity = "High",
    min_vars = "ID",
    study_types = "All",
    requires = "CORE-STUDYTYPE-001"
  )

  register_check(
    id = "CORE-MR-001",
    fn = check_mr_exclusion_flags,
    title = "Exclusion Flag Internal Consistency",
    domain = "readiness",
    severity = "High",
    min_vars = character(),
    study_types = "All",
    requires = "CORE-STUDYTYPE-001"
  )

  register_check(
    id = "CORE-MR-002",
    fn = check_mr_imputation,
    title = "Imputation Traceability Signals",
    domain = "readiness",
    severity = "Medium",
    min_vars = character(),
    study_types = "All",
    requires = "CORE-STUDYTYPE-001"
  )

  register_check(
    id = "CORE-MR-003",
    fn = check_mr_plot_ready,
    title = "Plot-Ready Flagged Subject Review",
    domain = "readiness",
    severity = "Medium",
    min_vars = c("ID", "EVID", "MDV", "DV"),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-OBS-001")
  )

  register_check(
    id = "CORE-OBS-002",
    fn = check_obs_blq_consistency,
    title = "BLQ/CENS/LLOQ Internal Consistency",
    domain = "observations",
    severity = "Critical",
    min_vars = c("DV", "MDV"),
    study_types = c("All", "Single Dose", "Multiple Dose"),
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-OBS-001")
  )

  register_check(
    id = "CORE-OBS-003",
    fn = check_obs_blq_middle,
    title = "BLQ in Middle of Profile",
    domain = "observations",
    severity = "Medium",
    min_vars = c("ID", "TIME", "DV"),
    study_types = c("All", "Single Dose", "Multiple Dose"),
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-OBS-001")
  )

  register_check(
    id = "CORE-OBS-006",
    fn = check_obs_min_records,
    title = "Minimum Quantifiable PK Records Per Subject or Occasion",
    domain = "observations",
    severity = "Medium",
    min_vars = c("ID", "EVID", "MDV", "DV"),
    study_types = c("Single Dose", "Multiple Dose"),
    requires = c("CORE-STUDYTYPE-001", "CORE-OBS-001")
  )

  register_check(
    id = "CORE-DOSE-005",
    fn = check_dose_addl_ii,
    title = "ADDL and II Basic Validity",
    domain = "dosing",
    severity = "High",
    min_vars = "EVID",
    study_types = "Multiple Dose",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003")
  )

  register_check(
    id = "CORE-DOSE-006",
    fn = check_dose_rate_dur,
    title = "Infusion RATE and DUR Basic Validity",
    domain = "dosing",
    severity = "Critical",
    min_vars = c("EVID", "AMT"),
    study_types = "IV",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003")
  )

  register_check(
    id = "CORE-DOSE-007",
    fn = check_dose_ss,
    title = "SS Flag Basic Validity",
    domain = "dosing",
    severity = "High",
    min_vars = "EVID",
    study_types = "Multiple Dose",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-003")
  )

  register_check(
    id = "CORE-OCC-001",
    fn = check_occ_consistency,
    title = "Occasion and Period Internal Consistency",
    domain = "occasion",
    severity = "High",
    min_vars = c("ID", "TIME"),
    study_types = c("Multiple Dose", "Integrated"),
    requires = c("CORE-STUDYTYPE-001", "CORE-TIME-001")
  )

  register_check(
    id = "CORE-CMP-001",
    fn = check_cmp_cmt,
    title = "CMT Basic Consistency",
    domain = "compartment",
    severity = "Medium",
    min_vars = "EVID",
    study_types = "Multi-analyte",
    requires = "CORE-STUDYTYPE-001"
  )

  register_check(
    id = "CORE-CMP-002",
    fn = check_cmp_dvid_analyte,
    title = "DVID and Analyte Internal Consistency",
    domain = "compartment",
    severity = "High",
    min_vars = "DVID",
    study_types = "Multi-analyte",
    requires = c("CORE-STUDYTYPE-001", "CORE-OBS-001")
  )

  register_check(
    id = "CORE-CMP-003",
    fn = check_cmp_magnitude,
    title = "Concentration Magnitude Review Across DVID",
    domain = "compartment",
    severity = "Medium",
    min_vars = c("DVID", "DV", "MDV"),
    study_types = "Multi-analyte",
    requires = c("CORE-STUDYTYPE-001", "CORE-CMP-002", "CORE-OBS-005")
  )

  register_check(
    id = "CORE-INT-001",
    fn = check_int_id_uniqueness,
    title = "Subject ID Uniqueness Across Studies",
    domain = "integrated",
    severity = "Critical",
    min_vars = "ID",
    study_types = "Integrated",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-003")
  )

  register_check(
    id = "CORE-INT-002",
    fn = check_int_categorical,
    title = "Cross-Study Categorical Coding Consistency",
    domain = "integrated",
    severity = "High",
    min_vars = "STUDYID",
    study_types = "Integrated",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-003")
  )

  register_check(
    id = "CORE-INT-003",
    fn = check_int_numeric,
    title = "Cross-Study Numeric Distribution Review",
    domain = "integrated",
    severity = "High",
    min_vars = "STUDYID",
    study_types = "Integrated",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-003")
  )

  register_check(
    id = "CORE-COV-002",
    fn = check_cov_char_numeric,
    title = "Character-Numeric Mapping Consistency",
    domain = "covariates",
    severity = "High",
    min_vars = character(),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-003")
  )

  register_check(
    id = "CORE-COV-003",
    fn = check_cov_truncation,
    title = "Possible Character Truncation",
    domain = "covariates",
    severity = "Medium",
    min_vars = character(),
    study_types = "All",
    requires = c("CORE-STUDYTYPE-001", "CORE-STRUCT-001", "CORE-STRUCT-003")
  )

  register_check(
    id = "CORE-COV-006",
    fn = check_cov_time_varying,
    title = "Time-Varying Covariate Change Review",
    domain = "covariates",
    severity = "Medium",
    min_vars = c("ID", "TIME"),
    study_types = c("All", "Multiple Dose"),
    requires = c("CORE-STUDYTYPE-001", "CORE-TIME-001", "CORE-COV-004")
  )

  invisible(NULL)
}
