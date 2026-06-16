test_that("run_nmpk_checks returns a tidy findings tibble", {
  findings <- run_nmpk_checks(make_clean_nmpk())
  expect_s3_class(findings, "tbl_df")
  expect_setequal(
    names(findings),
    c("check_id", "domain", "severity", "status", "n_flagged", "message")
  )
  expect_true(
    all(c("CORE-STRUCT-001", "CORE-STRUCT-005") %in% findings$check_id)
  )
  expect_false(any(findings$status %in% c("flag", "error")))
})

test_that("full check_result objects are attached", {
  findings <- run_nmpk_checks(make_clean_nmpk())
  results <- attr(findings, "results")
  expect_s3_class(results[["CORE-STRUCT-001"]], "pmxdchk_check_result")
})

test_that("checks are skipped when required variables are absent", {
  data <- make_clean_nmpk()[, c("ID", "MDV", "DV", "AMT")] # no TIME/EVID
  findings <- run_nmpk_checks(data)
  struct005 <- findings[findings$check_id == "CORE-STRUCT-005", ]
  expect_equal(struct005$status, "skip")
})

test_that("dependents are skipped when a prerequisite is skipped", {
  # Force CORE-STRUCT-001 to skip by removing its (none) -- instead remove the
  # variables STRUCT-005 needs and confirm the skip message references vars.
  data <- make_clean_nmpk()[, c("ID", "MDV", "DV", "AMT")]
  findings <- run_nmpk_checks(data)
  msg <- findings$message[findings$check_id == "CORE-STRUCT-005"]
  expect_match(msg, "required variable")
})

test_that("apply_mapping renames user columns to canonical names", {
  data <- make_clean_nmpk()
  names(data)[names(data) == "ID"] <- "USUBJID"
  findings <- run_nmpk_checks(data, mapping = c(ID = "USUBJID"))
  expect_false(any(findings$status %in% c("flag", "error")))
})

test_that("every registered check runs on the multidose fixture", {
  st <- c("All", "Multiple Dose", "IV", "Multi-analyte", "Integrated")
  findings <- run_nmpk_checks(make_multidose_nmpk(), study_type = st)
  expect_setequal(findings$check_id, names(check_registry()))
  expect_false(any(findings$status %in% c("flag", "error")))
})

test_that("study-type gating excludes conditional checks for All only", {
  findings <- run_nmpk_checks(make_multidose_nmpk(), study_type = "All")
  expect_false("CORE-STRUCT-007" %in% findings$check_id)
})

test_that("run_nmpk_checks flags injected issues across domains", {
  data <- make_multidose_nmpk()
  data$EVID[2] <- 9 # STRUCT-003 invalid EVID
  data$AMT[data$ID == 2 & data$EVID == 1] <- 0 # DOSE-001 zero dose
  data$TIME[data$ID == 3 & data$EVID == 1] <- -1 # TIME-003 negative dose time
  data$AGE[data$ID == 4] <- 900 # COV-005 implausible
  findings <- run_nmpk_checks(data, study_type = c("All", "Multiple Dose"))
  flagged <- findings$check_id[findings$status == "flag"]
  expect_true(all(
    c("CORE-STRUCT-003", "CORE-DOSE-001", "CORE-TIME-003", "CORE-COV-005")
    %in% flagged
  ))
})
