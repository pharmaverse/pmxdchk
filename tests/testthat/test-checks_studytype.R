test_that("infer_study_type flags multiple-dose from ADDL/II", {
  data <- make_clean_nmpk()
  data$ADDL <- ifelse(data$EVID == 1, 5, NA)
  data$II <- ifelse(data$EVID == 1, 24, NA)
  inf <- infer_study_type(data)
  expect_true(inf$applicable[inf$study_type == "Multiple Dose"])
  expect_false(inf$applicable[inf$study_type == "Single Dose"])
})

test_that("infer_study_type flags single dose for one dose per subject", {
  inf <- infer_study_type(make_clean_nmpk())
  expect_true(inf$applicable[inf$study_type == "Single Dose"])
})

test_that("infer_study_type detects IV and multi-analyte", {
  data <- make_clean_nmpk()
  data$RATE <- 0
  data$DVID <- rep(c(1, 2), length.out = nrow(data))
  inf <- infer_study_type(data)
  expect_true(inf$applicable[inf$study_type == "IV"])
  expect_true(inf$applicable[inf$study_type == "Multi-analyte"])
})

test_that("All is always applicable", {
  inf <- infer_study_type(make_clean_nmpk())
  expect_true(inf$applicable[inf$study_type == "All"])
})

test_that("CORE-STUDYTYPE-001 runs through the engine as a prerequisite", {
  findings <- run_nmpk_checks(make_clean_nmpk())
  expect_true("CORE-STUDYTYPE-001" %in% findings$check_id)
  expect_equal(
    findings$status[findings$check_id == "CORE-STUDYTYPE-001"], "pass"
  )
})

applicable_types <- function(data) {
  inf <- infer_study_type(data)
  inf$study_type[inf$applicable]
}

test_that("infer_study_type identifies the IV builder", {
  expect_true("IV" %in% applicable_types(make_iv_nmpk()))
})

test_that("infer_study_type identifies the multi-analyte builder", {
  expect_true("Multi-analyte" %in% applicable_types(make_multianalyte_nmpk()))
})

test_that("infer_study_type identifies the integrated builder", {
  expect_true("Integrated" %in% applicable_types(make_integrated_nmpk()))
})

test_that("infer_study_type identifies the multiple-dose builder", {
  expect_true("Multiple Dose" %in% applicable_types(make_multidose_nmpk()))
})
