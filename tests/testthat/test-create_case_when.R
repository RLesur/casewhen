context("test-create_case_when.R")

test_that("x argument is a character vector", {
  expect_error(create_case_when(TRUE ~ as.character(x), vars = 1))
})

test_that("returned function has correct formals", {
  expect_equal(
    rlang::fn_fmls_names(
      create_case_when(TRUE ~ as.character(x),
                       vars = c("x", "y", "z")
      )
    ),
    c("x", "y", "z")
  )
})
