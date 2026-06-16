test_that("CORE-OCC-001 skips without an occasion variable", {
  res <- check_occ_consistency(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-OCC-001 passes when occasion is non-decreasing with time", {
  res <- check_occ_consistency(make_multidose_nmpk(), default_thresholds())
  expect_equal(res$status, "pass")
})

test_that("CORE-OCC-001 flags decreasing occasion within a subject", {
  data <- make_multidose_nmpk()
  data$OCC[data$ID == 1] <- 6:1 # decreases as time increases
  res <- check_occ_consistency(data, default_thresholds())
  expect_equal(res$status, "flag")
})
