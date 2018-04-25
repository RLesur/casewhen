context("test-create_case_when.R")

vars <- c("x", "y", "z")

test_that("x argument is a character vector", {
  expect_error(create_case_when(TRUE ~ as.character(x), vars = 1))
})

test_that("returned function has correct formals", {
  expect_equal(
    rlang::fn_fmls_names(
      create_case_when(TRUE ~ as.character(x), vars = vars)
    ),
    vars
  )
  expect_length(
    rlang::fn_fmls(create_case_when(TRUE ~ as.character(x), vars = vars)),
    length(vars)
  )
  lapply(
    rlang::fn_fmls(create_case_when(TRUE ~ as.character(x), vars = vars)),
    function(x) expect_equal(x, rlang::missing_arg())
  )
})
