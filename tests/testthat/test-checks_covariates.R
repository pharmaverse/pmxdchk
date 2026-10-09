test_that("CORE-COV-004 passes when covariates are unremarkable", {
  res <- check_cov_outliers(make_cov_nmpk(), default_thresholds())
  expect_equal(res$status, "pass")
})

test_that("CORE-COV-004 flags an extreme covariate value", {
  data <- make_cov_nmpk()
  data$AGE[1] <- 900
  res <- check_cov_outliers(data, default_thresholds())
  expect_equal(res$status, "flag")
  expect_true("AGE" %in% res$summary_table$variable)
})

test_that("CORE-COV-004 skips when no continuous covariates present", {
  res <- check_cov_outliers(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-COV-001 flags inconsistent fixed covariate within subject", {
  data <- make_multidose_nmpk()
  data$SEX[data$ID == 1][2] <- 1 - data$SEX[data$ID == 1][1]
  res <- check_cov_fixed_consistency(data, default_thresholds())
  expect_equal(res$status, "flag")
  expect_true("SEX" %in% res$summary_table$variable)
})

test_that("CORE-COV-001 passes when fixed covariates are constant", {
  res <- check_cov_fixed_consistency(
    make_multidose_nmpk(), default_thresholds()
  )
  expect_equal(res$status, "pass")
})

test_that("CORE-COV-005 flags implausible covariate values", {
  data <- make_cov_nmpk()
  data$AGE[1] <- -5
  res <- check_cov_implausible(data, default_thresholds())
  expect_equal(res$status, "flag")
  expect_true("AGE" %in% res$summary_table$variable)
})

test_that("CORE-COV-005 passes on plausible values", {
  res <- check_cov_implausible(make_cov_nmpk(), default_thresholds())
  expect_equal(res$status, "pass")
})

test_that("CORE-COV-002 flags inconsistent label/code mapping", {
  data <- make_clean_nmpk()
  data$SEX <- c("M", "M", "F", "F", "M", "M", "F", "F")
  data$SEXN <- c(1, 1, 2, 2, 1, 1, 2, 9) # F maps to 2 and 9
  res <- check_cov_char_numeric(data, default_thresholds())
  expect_equal(res$status, "flag")
})

test_that("CORE-COV-002 skips without paired variables", {
  res <- check_cov_char_numeric(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-COV-003 flags possible character truncation", {
  data <- make_clean_nmpk()
  data$NOTE <- c(
    strrep("A", 30), strrep("B", 30), strrep("C", 30), "short",
    strrep("D", 30), "x", "y", "z"
  )
  res <- check_cov_truncation(data, default_thresholds())
  expect_equal(res$status, "flag")
})

test_that("CORE-COV-006 flags a large within-subject covariate change", {
  data <- make_multidose_nmpk()
  data$WT <- data$WT + data$TIME / 10 # weight drifts in every subject
  expect_equal(
    check_cov_time_varying(data, default_thresholds())$status, "pass"
  )
  data$WT[data$ID == 1][5] <- 140
  res <- check_cov_time_varying(data, default_thresholds())
  expect_equal(res$status, "flag")
  expect_equal(unique(res$summary_table$ID), "1")
})

test_that("a covariate changing in few subjects is fixed, not time-varying", {
  data <- make_multidose_nmpk()
  data$WT[data$ID == 1][5] <- 140
  expect_equal(
    check_cov_time_varying(data, default_thresholds())$status, "skip"
  )
  res <- check_cov_fixed_consistency(data, default_thresholds())
  expect_equal(res$status, "flag")
  expect_equal(res$summary_table$variable, "WT")
})

test_that("CORE-COV-005 reports the plausible range for extra covariates", {
  data <- make_cov_nmpk()
  data$CRCLBL <- c(90, 100, 110, -99, 95, 105)
  res <- check_cov_implausible(data, default_thresholds())
  expect_equal(res$summary_table$variable, "CRCLBL")
  expect_match(res$summary_table$plausible_range, "0 - 500")
})

test_that("CORE-COV-006 skips without time-varying covariates", {
  res <- check_cov_time_varying(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})
