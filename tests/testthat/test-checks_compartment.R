test_that("CORE-CMP-001 skips without CMT", {
  res <- check_cmp_cmt(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-CMP-001 flags a missing CMT among populated ones", {
  data <- make_multianalyte_nmpk()
  data$CMT <- 2
  data$CMT[1] <- NA
  res <- check_cmp_cmt(data, default_thresholds())
  expect_equal(res$status, "flag")
})

test_that("CORE-CMP-002 flags a DVID mapping to multiple analyte labels", {
  data <- make_multianalyte_nmpk()
  data$PARAM <- ifelse(is.na(data$DVID), NA,
    ifelse(data$DVID == 1, "Parent", "Metabolite")
  )
  data$PARAM[which(data$DVID == 1)[1]] <- "Other"
  res <- check_cmp_dvid_analyte(data, default_thresholds())
  expect_equal(res$status, "flag")
})

test_that("CORE-CMP-002 skips without DVID", {
  res <- check_cmp_dvid_analyte(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-CMP-003 flags large magnitude differences across DVID", {
  data <- make_multianalyte_nmpk()
  data$DV[data$DVID == 2 & data$EVID == 0] <- 1e-4
  res <- check_cmp_magnitude(data, default_thresholds())
  expect_equal(res$status, "flag")
})
