test_that("CORE-STRUCT-001 passes when all core variables are present", {
  res <- check_struct_core_variables(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "pass")
})

test_that("CORE-STRUCT-001 flags missing core variables", {
  data <- make_clean_nmpk()[, c("ID", "TIME", "EVID", "MDV", "DV")]
  res <- check_struct_core_variables(data, default_thresholds())
  expect_equal(res$status, "flag")
  expect_equal(res$n_flagged, 1L)
  expect_equal(res$summary_table$variable, "AMT")
})

test_that("CORE-STRUCT-005 passes on unique keys", {
  res <- check_struct_duplicate_records(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "pass")
})

test_that("CORE-STRUCT-005 flags duplicate keys", {
  res <- check_struct_duplicate_records(
    make_duplicate_nmpk(), default_thresholds()
  )
  expect_equal(res$status, "flag")
  expect_equal(res$n_flagged, 2L)
  expect_s3_class(res$flagged_records, "tbl_df")
})

test_that("CORE-STRUCT-002 flags non-numeric values in numeric variables", {
  data <- make_clean_nmpk()
  data$DV <- as.character(data$DV)
  data$DV[3] <- "BLQ"
  res <- check_struct_numeric_parsable(data, default_thresholds())
  expect_equal(res$status, "flag")
  expect_equal(res$summary_table$variable, "DV")
})

test_that("CORE-STRUCT-003 flags invalid EVID", {
  data <- make_clean_nmpk()
  data$EVID[2] <- 9
  res <- check_struct_evid_validity(data, default_thresholds())
  expect_equal(res$status, "flag")
  expect_equal(res$n_flagged, 1L)
})

test_that("CORE-STRUCT-004 reports record counts", {
  res <- check_struct_record_counts(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "pass")
  expect_equal(res$summary_table$value[res$summary_table$metric == "subjects"], 2L)
})

test_that("CORE-STRUCT-006 summarizes missingness and ignores .rowid", {
  data <- make_clean_nmpk()
  data$DV[1] <- NA
  data$.rowid <- seq_len(nrow(data))
  res <- check_struct_missingness(data, default_thresholds())
  expect_equal(res$status, "pass")
  expect_false(".rowid" %in% res$summary_table$variable)
  expect_true("DV" %in% res$summary_table$variable)
})

test_that("CORE-STRUCT-007 skips without EVID=4 records", {
  res <- check_struct_evid4_uniqueness(make_multidose_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-STRUCT-007 flags multiple resets within a subject-period", {
  data <- make_multidose_nmpk()
  reset <- data[data$ID == 1 & data$EVID == 1, ][1, ]
  reset$EVID <- 4
  data <- dplyr::bind_rows(data, reset, reset) # two EVID=4 for ID 1, OCC 1
  res <- check_struct_evid4_uniqueness(data, default_thresholds())
  expect_equal(res$status, "flag")
})
