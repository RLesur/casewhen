context("test-create_case_when.R")

test_that("x argument is a character vector", {
  expect_error(create_case_when(TRUE ~ as.character(x), vars = 1))
})

vars <- c("x", "y", "z")
cw <- create_case_when(TRUE ~ as.character(x), vars = vars)

test_that("returned value is a closure", {
  expect_type(cw, "closure")
})

test_that("returned function has correct formals with missing args", {
  expect_equal(rlang::fn_fmls_names(cw), vars)
  lapply(rlang::fn_fmls(cw), function(x) expect_equal(x, rlang::missing_arg()))
})
