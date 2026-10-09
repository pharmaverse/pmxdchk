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

test_that("CORE-OBS-001 flags a DV value on a dose record", {
  data <- make_clean_nmpk()
  data$DV[1] <- 5 # row 1 is a dose record
  res <- check_obs_mdv_dv(data, default_thresholds())
  expect_equal(res$flagged_records$issue, "dose record with a DV value")
})

test_that("CORE-OBS-002 flags an unmarked DV below LLOQ", {
  data <- make_clean_nmpk()
  data$BLQ <- 0
  data$LLOQ <- 6
  res <- check_obs_blq_consistency(data, default_thresholds())
  expect_equal(res$n_flagged, 1L) # DV = 5 in subject 1
  expect_equal(res$flagged_records$issue, "DV below LLOQ but not marked BLQ")
})

test_that("CORE-OBS-003 does not flag a BLQ trough before the next dose", {
  data <- make_clean_nmpk()
  data$BLQ <- 0
  second <- data[data$ID == 1, ]
  second$TIME <- second$TIME + 24
  data <- dplyr::bind_rows(data[data$ID == 1, ], second)
  data$BLQ[4] <- 1 # last sample of the first dosing interval
  expect_equal(check_obs_blq_middle(data, default_thresholds())$status, "pass")
  data$BLQ[4] <- 0
  data$BLQ[3] <- 1 # between two quantifiable samples
  expect_equal(check_obs_blq_middle(data, default_thresholds())$n_flagged, 1L)
})

test_that("CORE-OBS-004 uses the subject's own Cmax", {
  data <- make_clean_nmpk()
  predose <- data[data$EVID == 0, ][1, ]
  predose$TIME <- -0.5
  predose$DV <- 0.2 # 2% of Cmax = 10
  res <- check_obs_predose(
    dplyr::bind_rows(predose, data), default_thresholds()
  )
  expect_equal(res$status, "pass")
  predose$DV <- 2 # 20% of Cmax
  res <- check_obs_predose(
    dplyr::bind_rows(predose, data), default_thresholds()
  )
  expect_equal(res$flagged_records$pct_of_cmax, 20)
})

test_that("CORE-OBS-005 compares within a nominal time point", {
  data <- make_multidose_nmpk()
  data$DV[data$EVID == 0] <- data$DV[data$EVID == 0] *
    rep(c(0.9, 0.95, 1, 1.05, 1.1, 1.15), each = 5)
  expect_equal(check_obs_outliers(data, default_thresholds())$status, "pass")
  data$DV[data$ID == 1 & data$NTIME == 8] <- 500
  res <- check_obs_outliers(data, default_thresholds())
  expect_equal(res$n_flagged, 1L)
})
