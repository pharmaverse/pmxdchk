test_that("CORE-TIME-001 passes on ordered TIME", {
  res <- check_time_nondecreasing(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "pass")
})

test_that("CORE-TIME-001 flags a decreasing TIME within a subject", {
  data <- make_clean_nmpk()
  data$TIME[3] <- 0.5 # subject 1: 0, 1, 0.5, 4 -> decrease at row 3
  res <- check_time_nondecreasing(data, default_thresholds())
  expect_equal(res$status, "flag")
  expect_equal(res$n_flagged, 1L)
})

test_that("CORE-TIME-001 evaluates per subject, not across subjects", {
  # subject 2 starting at TIME 0 after subject 1 ended at 4 is not a decrease
  res <- check_time_nondecreasing(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "pass")
})

test_that("CORE-TIME-002 flags duplicate doses at the same subject-time", {
  data <- make_clean_nmpk()
  extra <- data[data$EVID == 1, ][1, ]
  data <- dplyr::bind_rows(data, extra) # second dose for subject 1 at TIME 0
  res <- check_time_same_time_events(data, default_thresholds())
  expect_equal(res$status, "flag")
})

test_that("CORE-TIME-003 flags negative TIME on a dose record", {
  data <- make_clean_nmpk()
  data$TIME[1] <- -0.5 # row 1 is a dose record
  res <- check_time_negative(data, default_thresholds())
  expect_equal(res$status, "flag")
  expect_equal(res$n_flagged, 1L)
})

test_that("CORE-TIME-003 treats negative predose obs as descriptive", {
  data <- make_clean_nmpk()
  data$TIME[2] <- -0.5 # row 2 is an observation
  res <- check_time_negative(data, default_thresholds())
  expect_equal(res$status, "pass")
})

test_that("CORE-TIME-004 skips without nominal time", {
  res <- check_time_actual_nominal(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-TIME-004 flags an actual-nominal outlier", {
  data <- make_multidose_nmpk()
  data$TIME[data$EVID == 0][1] <- data$NTIME[data$EVID == 0][1] + 100
  res <- check_time_actual_nominal(data, default_thresholds())
  expect_equal(res$status, "flag")
})
