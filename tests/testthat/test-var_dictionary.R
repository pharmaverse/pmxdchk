test_that("guess_mapping resolves canonical names directly", {
  m <- guess_mapping(c("ID", "TIME", "EVID", "MDV", "DV", "AMT"))
  expect_equal(unname(m[c("ID", "TIME", "AMT")]), c("ID", "TIME", "AMT"))
})

test_that("guess_mapping resolves CDISC ADPPK aliases case-insensitively", {
  m <- guess_mapping(c("usubjidn", "AFRLT", "EVID", "DV", "WTBL"))
  expect_equal(unname(m["ID"]), "usubjidn")
  expect_equal(unname(m["TIME"]), "AFRLT")
  expect_equal(unname(m["WT"]), "WTBL")
})

test_that("guess_mapping omits unresolved variables", {
  m <- guess_mapping(c("foo", "bar"))
  expect_length(m, 0)
})
