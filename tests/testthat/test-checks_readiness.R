test_that("CORE-MR-001 skips without an exclusion flag variable", {
  res <- check_mr_exclusion_flags(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-MR-001 flags excluded records without a reason", {
  data <- make_clean_nmpk()
  data$EXCLFL <- c("Y", rep("", nrow(data) - 1))
  data$EXCLRSN <- ""
  res <- check_mr_exclusion_flags(data, default_thresholds())
  expect_equal(res$status, "flag")
  expect_equal(res$n_flagged, 1L)
})

test_that("CORE-MR-001 passes when exclusions carry a reason", {
  data <- make_clean_nmpk()
  data$EXCLFL <- c("Y", rep("", nrow(data) - 1))
  data$EXCLRSN <- c("outlier", rep("", nrow(data) - 1))
  res <- check_mr_exclusion_flags(data, default_thresholds())
  expect_equal(res$status, "pass")
})

test_that("CORE-MR-002 detects imputation flag variables", {
  data <- make_clean_nmpk()
  data$IMPFL <- "N"
  res <- check_mr_imputation(data, default_thresholds())
  expect_equal(res$status, "pass")
  expect_true("IMPFL" %in% res$summary_table$variable)
})

test_that("CORE-MR-002 skips without imputation variables", {
  res <- check_mr_imputation(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-MR-003 lists subjects flagged by other checks", {
  data <- make_clean_nmpk()
  data$AMT[data$ID == 2 & data$EVID == 1] <- 0 # DOSE-001 on subject 2
  findings <- run_nmpk_checks(data)
  res <- attr(findings, "results")[["CORE-MR-003"]]
  expect_equal(res$status, "pass")
  expect_equal(res$subject_list$ID, "2")
  expect_match(res$subject_list$checks, "CORE-DOSE-001")
})

test_that("CORE-MR-003 reports no subjects on a clean dataset", {
  findings <- run_nmpk_checks(make_clean_nmpk())
  res <- attr(findings, "results")[["CORE-MR-003"]]
  expect_null(res$subject_list)
})

test_that("CORE-MR-001 flags a reason on a record that is not excluded", {
  data <- make_clean_nmpk()
  data$EXCLFL <- "N"
  data$EXCLRSN <- ""
  data$EXCLRSN[3] <- "outlier"
  res <- check_mr_exclusion_flags(data, default_thresholds())
  expect_equal(res$flagged_records$issue, "reason given but not excluded")
})
