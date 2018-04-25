context("test-create_case_when.R")

test_that("x argument is a character vector", {
  expect_error(create_case_when(TRUE ~ as.character(x), vars = 1))
})
