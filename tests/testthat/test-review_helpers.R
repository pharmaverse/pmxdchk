test_that("time_after_dose restarts at each dose record", {
  tad <- time_after_dose(
    id = c(1, 1, 1, 1, 2),
    time = c(-1, 0, 5, 24, 3),
    is_dose = c(FALSE, TRUE, FALSE, TRUE, FALSE)
  )
  expect_equal(tad, c(NA, 0, 5, 0, NA))
})

test_that("plot_subject_profile returns NULL without observations", {
  sub <- add_review_columns(make_clean_nmpk())
  sub <- sub[sub$ID == 1, ]
  sub$flagged <- FALSE
  expect_s3_class(plot_subject_profile(sub, 1), "ggplot")
  expect_null(plot_subject_profile(sub[sub$EVID == 1, ], 1))
})

test_that("plot_population_profile handles each time axis and colouring", {
  data <- make_multidose_nmpk()
  expect_s3_class(plot_population_profile(data), "ggplot")
  expect_s3_class(
    plot_population_profile(data, time_axis = "last", color_by = "dose"),
    "ggplot"
  )
  expect_s3_class(plot_population_profile(data, color_by = "SEX"), "ggplot")
  expect_null(plot_population_profile(data[data$EVID == 1, ]))
})

test_that("findings_list shows the triage state of findings to review", {
  data <- make_clean_nmpk()
  data$AMT[1] <- 0
  findings <- run_nmpk_checks(data)
  rows <- findings_list(findings, c("CORE-DOSE-001" = "reject"))
  flagged <- findings$check_id == "CORE-DOSE-001"
  expect_equal(rows$triage[flagged], "not an issue")
  expect_equal(rows$result[flagged], "1 flagged")
  expect_true(all(rows$triage[findings$status == "pass"] == ""))
})

test_that("finding_has_profile is limited to record or PK subject findings", {
  data <- make_multidose_nmpk()
  data$AMT[data$ID == 1 & data$EVID == 1] <- 0
  data$AGE[data$ID == 2] <- 900
  results <- attr(run_nmpk_checks(data), "results")
  expect_true(finding_has_profile(results[["CORE-DOSE-001"]]))
  expect_false(finding_has_profile(results[["CORE-COV-005"]]))
  expect_false(finding_has_profile(results[["CORE-STRUCT-001"]]))
})

test_that("signif_doubles leaves dates, integers, and text untouched", {
  data <- data.frame(
    x = 1.23456789, n = 2L, when = as.POSIXct("2026-01-01 10:00", tz = "UTC"),
    day = as.Date("2026-01-01"), label = "a"
  )
  out <- signif_doubles(data, 3)
  expect_equal(out$x, 1.23)
  expect_identical(out[-1], data[-1])
})

test_that("read_nmpk_file guesses the delimiter and reads '.' as missing", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  for (sep in c(",", ";", "\t")) {
    writeLines(c(
      paste(c("ID", "TIME", "DV"), collapse = sep),
      paste(c("1", "0", "."), collapse = sep),
      paste(c("1", "1", "5.5"), collapse = sep)
    ), path)
    data <- read_nmpk_file(path)
    expect_named(data, c("ID", "TIME", "DV"))
    expect_equal(data$DV, c(NA, 5.5))
  }
})
