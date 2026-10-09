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

test_that("CORE-INT-003 passes when study medians are comparable", {
  res <- check_int_numeric(make_integrated_nmpk(), default_thresholds())
  expect_equal(res$status, "pass")
})

test_that("CORE-INT-003 skips with a single study", {
  data <- make_integrated_nmpk()
  data$STUDYID <- "S1"
  expect_equal(check_int_numeric(data, default_thresholds())$status, "skip")
})

test_that("CORE-INT-001 flags an ID shared by two USUBJIDs", {
  data <- make_multidose_nmpk()
  data$USUBJID <- paste0("S1-", data$ID)
  expect_equal(
    check_int_id_uniqueness(data, default_thresholds())$status, "pass"
  )
  data$USUBJID[data$ID == 2] <- "S2-1"
  data$ID[data$ID == 2] <- 1
  res <- check_int_id_uniqueness(data, default_thresholds())
  expect_equal(res$status, "flag")
  expect_equal(res$subject_list$ID, "1")
})

test_that("CORE-INT-001 flags a USUBJID split across two IDs", {
  data <- make_multidose_nmpk()
  data$USUBJID <- paste0("S1-", pmin(data$ID, 5)) # IDs 5 and 6 share one
  res <- check_int_id_uniqueness(data, default_thresholds())
  expect_setequal(res$subject_list$ID, c("5", "6"))
})

test_that("CORE-INT-003 flags a study-level numeric outlier", {
  data <- make_integrated_nmpk()
  data$STUDYID <- rep(c("A", "B", "C"), length.out = nrow(data))
  data$WT[data$STUDYID == "C"] <- data$WT[data$STUDYID == "C"] * 1000
  res <- check_int_numeric(data, default_thresholds())
  expect_equal(res$status, "flag")
})
