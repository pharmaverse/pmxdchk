test_that("CORE-OBS-001 passes on consistent MDV/DV", {
  res <- check_obs_mdv_dv(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "pass")
})

test_that("CORE-OBS-001 flags MDV=0 with missing DV", {
  data <- make_clean_nmpk()
  data$DV[data$EVID == 0][1] <- NA
  res <- check_obs_mdv_dv(data, default_thresholds())
  expect_equal(res$status, "flag")
})

test_that("CORE-OBS-005 flags a concentration outlier", {
  data <- make_clean_nmpk()
  data$DV[data$EVID == 0][1] <- 1e6
  res <- check_obs_outliers(data, default_thresholds())
  expect_equal(res$status, "flag")
  expect_true(res$n_flagged >= 1L)
})

test_that("CORE-OBS-005 skips when too few observations", {
  data <- make_clean_nmpk()[1:3, ] # one dose + two obs
  res <- check_obs_outliers(data, default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-OBS-004 flags a high predose concentration", {
  data <- make_multidose_nmpk()
  # add a quantifiable predose record for subject 1 with a high concentration
  predose <- data[data$ID == 1 & data$EVID == 0, ][1, ]
  predose$TIME <- -0.5
  predose$DV <- 50
  data <- dplyr::bind_rows(data, predose)
  res <- check_obs_predose(data, default_thresholds())
  expect_equal(res$status, "flag")
})

test_that("CORE-OBS-004 passes without quantifiable predose records", {
  res <- check_obs_predose(make_multidose_nmpk(), default_thresholds())
  expect_equal(res$status, "pass")
})

test_that("CORE-OBS-002 skips without a BLQ/CENS variable", {
  res <- check_obs_blq_consistency(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-OBS-002 flags a BLQ record with DV above LLOQ", {
  data <- make_clean_nmpk()
  data$BLQFL <- "N"
  data$ALLOQ <- 1
  data$BLQFL[2] <- "Y" # DV here is 10, above LLOQ 1
  res <- check_obs_blq_consistency(data, default_thresholds())
  expect_equal(res$status, "flag")
})

test_that("CORE-OBS-003 flags a BLQ record in the middle of a profile", {
  data <- make_clean_nmpk()
  data$BLQFL <- "N"
  data$BLQFL[3] <- "Y" # subject 1 middle observation
  res <- check_obs_blq_middle(data, default_thresholds())
  expect_equal(res$status, "flag")
})

test_that("CORE-OBS-006 flags a subject below the minimum record count", {
  data <- make_clean_nmpk()
  data <- data[!(data$ID == 1 & data$TIME %in% c(2, 4)), ]
  res <- check_obs_min_records(data, default_thresholds())
  expect_equal(res$status, "flag")
})
