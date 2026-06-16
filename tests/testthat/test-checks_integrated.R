test_that("CORE-INT-001 skips without STUDYID", {
  res <- check_int_id_uniqueness(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-INT-001 flags an ID appearing in multiple studies", {
  data <- make_integrated_nmpk()
  extra <- data[data$ID == 1, ][1, ]
  extra$STUDYID <- "S2"
  data <- dplyr::bind_rows(data, extra)
  res <- check_int_id_uniqueness(data, default_thresholds())
  expect_equal(res$status, "flag")
})

test_that("CORE-INT-002 flags divergent cross-study coding", {
  data <- make_integrated_nmpk()
  data$SEX <- ifelse(data$STUDYID == "S1", "M", "1")
  res <- check_int_categorical(data, default_thresholds())
  expect_equal(res$status, "flag")
})

test_that("CORE-INT-003 skips with fewer than three studies", {
  res <- check_int_numeric(make_integrated_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-INT-003 flags a study-level numeric outlier", {
  data <- make_integrated_nmpk()
  data$STUDYID <- rep(c("A", "B", "C"), length.out = nrow(data))
  data$WT[data$STUDYID == "C"] <- data$WT[data$STUDYID == "C"] * 1000
  res <- check_int_numeric(data, default_thresholds())
  expect_equal(res$status, "flag")
})
