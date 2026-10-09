# pmxdchk 0.0.0.9000

## Checks

* Every check now carries a plain-language rule and guidance on what to do when
  it flags; `check_catalogue()` lists them and the app shows them next to each
  finding.
* `run_nmpk_checks()` returns a `title` column, and partial `thresholds` lists
  fall back to `default_thresholds()`. New thresholds: `outlier_iqr_k` and
  `predose_cmax_frac`.
* New findings: DV or MDV = 0 on dose records (CORE-OBS-001), DV below LLOQ
  without a BLQ flag (CORE-OBS-002), non-numeric `ID` (CORE-STRUCT-002), missing
  `EVID` (CORE-STRUCT-003), `ID` versus `USUBJID` uniqueness (CORE-INT-001),
  exclusion reason on a record that is not excluded (CORE-MR-001), `ADDL` on
  non-dose records (CORE-DOSE-005), and placeholder `RATE` values or
  `AMT` / `RATE` / `DUR` disagreement (CORE-DOSE-006).
* Fewer false positives: outliers are compared within nominal time point and
  analyte (CORE-OBS-005, CORE-TIME-004), BLQ troughs before the next dose are
  not "mid-profile" (CORE-OBS-003), ordinary dose levels are not outliers
  (CORE-DOSE-002), `TIME` may restart on reset records (CORE-TIME-001), and
  time-varying covariates are no longer reported as inconsistent fixed
  covariates (CORE-COV-001, CORE-COV-006).
* Changed rules: predose concentrations are compared with 5% of the subject's
  Cmax (CORE-OBS-004); covariate outliers use boxplot fences and cover more
  covariates (CORE-COV-004); missingness is reported by event type and flags
  only unexpected gaps (CORE-STRUCT-006); duplicates are split into exact rows
  and conflicting values (CORE-STRUCT-005); cross-study medians are compared
  from two studies upwards (CORE-INT-003); CORE-MR-003 lists the subjects
  flagged by other checks.
* `USUBJID` and `VISIT` are canonical variables; `USUBJID` is no longer preferred
  over a numeric identifier for `ID`.

## App

* Built-in example datasets can be loaded from the Data tab.
* Uploads accept comma-, semicolon-, tab-, and space-delimited text and read
  `.`, the NONMEM null value, as missing.
* Findings is the main workspace: it lists the checks to review with triage
  progress, and for the selected check shows the rule, guidance, the profile of
  each affected subject, all result tables, and the triage decision, which is
  saved as it is entered.
* Overview describes the dataset before any check runs: all concentration-time
  profiles (by time since first dose or after the last dose), dose levels, and
  missing values by event type.
* The sidebar is replaced by a status line with the dataset, the state of the
  last run, and a run button; thresholds moved to the Data tab.
* Profiles can be filtered by check; new Check library tab.
* Results are cleared when another dataset is confirmed; a column can no longer
  be mapped to two variables.
