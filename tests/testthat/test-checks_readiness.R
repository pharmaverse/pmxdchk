test_that("CORE-MR-001 skips without an exclusion flag variable", {
  res <- check_mr_exclusion_flags(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-MR-001 flags excluded records without a reason", {
  data <- make_clean_nmpk()
  data$EXCLFL <- c("Y", rep("", nrow(data) - 1))
  data$EXCLRSN <- ""
  res <- check_mr_exclusion_flags(data, default_thresholds())
  expect_equal(res$status, "flag")
  expect_equal(res$n_flagged, 1L)
})

test_that("CORE-MR-001 passes when exclusions carry a reason", {
  data <- make_clean_nmpk()
  data$EXCLFL <- c("Y", rep("", nrow(data) - 1))
  data$EXCLRSN <- c("outlier", rep("", nrow(data) - 1))
  res <- check_mr_exclusion_flags(data, default_thresholds())
  expect_equal(res$status, "pass")
})

test_that("CORE-MR-002 detects imputation flag variables", {
  data <- make_clean_nmpk()
  data$IMPFL <- "N"
  res <- check_mr_imputation(data, default_thresholds())
  expect_equal(res$status, "pass")
  expect_true("IMPFL" %in% res$summary_table$variable)
})

test_that("CORE-MR-002 skips without imputation variables", {
  res <- check_mr_imputation(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "skip")
})

test_that("CORE-MR-003 lists plot-ready subjects", {
  res <- check_mr_plot_ready(make_clean_nmpk(), default_thresholds())
  expect_equal(res$status, "pass")
  expect_setequal(res$summary_table$ID, c(1, 2))
})

test_that("CORE-MR-003 skips when no quantifiable observations", {
  data <- make_clean_nmpk()
  data$MDV <- 1
  res <- check_mr_plot_ready(data, default_thresholds())
  expect_equal(res$status, "skip")
})
