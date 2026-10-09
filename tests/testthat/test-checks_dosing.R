test_that("CORE-DOSE-001 passes on valid AMT", {
  res <- check_dose_amt_validity(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "pass")
})

test_that("CORE-DOSE-001 flags zero dose amount", {
  data <- make_clean_nmpk()
  data$AMT[data$EVID == 1][1] <- 0
  res <- check_dose_amt_validity(data, default_thresholds())
  expect_equal(res$status, "flag")
  expect_equal(res$n_flagged, 1L)
})

test_that("CORE-DOSE-001 flags non-zero AMT on an observation", {
  data <- make_clean_nmpk()
  data$AMT[data$EVID == 0][1] <- 50
  res <- check_dose_amt_validity(data, default_thresholds())
  expect_equal(res$status, "flag")
})

test_that("CORE-DOSE-002 flags a dose amount outlier", {
  data <- make_multidose_nmpk()
  data$AMT[data$EVID == 1][1] <- 1e6
  res <- check_dose_distribution(data, default_thresholds())
  expect_equal(res$status, "flag")
})

test_that("CORE-DOSE-003 flags subjects with observations but no dose", {
  data <- make_multidose_nmpk()
  data <- data[!(data$ID == 1 & data$EVID == 1), ]
  res <- check_dose_obs_no_dose(data, default_thresholds())
  expect_equal(res$status, "flag")
  expect_true(1 %in% res$subject_list$ID)
})

test_that("CORE-DOSE-004 flags dosed subjects without quantifiable obs", {
  data <- make_multidose_nmpk()
  data$MDV[data$ID == 1 & data$EVID == 0] <- 1
  res <- check_dose_dose_no_obs(data, default_thresholds())
  expect_equal(res$status, "flag")
  expect_true(1 %in% res$subject_list$ID)
})

test_that("CORE-DOSE-005 flags negative ADDL", {
  data <- make_multidose_nmpk()
  data$ADDL[data$EVID == 1][1] <- -2
  res <- check_dose_addl_ii(data, default_thresholds())
  expect_equal(res$status, "flag")
})

test_that("CORE-DOSE-005 skips without ADDL", {
  res <- check_dose_addl_ii(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-DOSE-006 flags an invalid negative RATE", {
  data <- make_iv_nmpk()
  data$RATE[data$EVID == 1][1] <- -5
  res <- check_dose_rate_dur(data, default_thresholds())
  expect_equal(res$status, "flag")
})

test_that("CORE-DOSE-006 skips without RATE/DUR", {
  res <- check_dose_rate_dur(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-DOSE-007 flags SS=1 on a non-dose record", {
  data <- make_multidose_nmpk()
  data$SS[data$EVID == 0][1] <- 1
  res <- check_dose_ss(data, default_thresholds())
  expect_equal(res$status, "flag")
})

test_that("CORE-DOSE-007 skips without SS", {
  res <- check_dose_ss(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-DOSE-002 does not flag ordinary dose levels", {
  data <- make_multidose_nmpk()
  data$AMT[data$EVID == 1] <- c(100, 100, 100, 100, 200, 50)
  res <- check_dose_distribution(data, default_thresholds())
  expect_equal(res$status, "pass")
  expect_equal(nrow(res$summary_table), 3L)
})

test_that("CORE-DOSE-006 rejects placeholder RATE and inconsistent DUR", {
  data <- make_iv_nmpk()
  data$RATE[data$ID == 1 & data$EVID == 1] <- -99
  data$DUR[data$ID == 2 & data$EVID == 1] <- 3 # the implied duration is 10
  res <- check_dose_rate_dur(data, default_thresholds())
  expect_setequal(
    res$flagged_records$issue,
    c("negative RATE other than -1 or -2", "AMT / RATE differs from DUR")
  )
})

test_that("CORE-DOSE-006 accepts RATE = -2 when no DUR column exists", {
  data <- make_iv_nmpk()
  data$DUR <- NULL
  data$RATE[data$EVID == 1] <- -2
  expect_equal(check_dose_rate_dur(data, default_thresholds())$status, "pass")
})

test_that("CORE-DOSE-005 flags ADDL on a non-dose record", {
  data <- make_multidose_nmpk()
  data$ADDL[2] <- 1
  data$II[2] <- 24
  expect_equal(check_dose_addl_ii(data, default_thresholds())$n_flagged, 1L)
})
