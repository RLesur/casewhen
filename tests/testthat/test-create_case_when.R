context("test-create_case_when.R")

test_that("x argument is a character vector", {
  expect_error(create_case_when(TRUE ~ x, vars = 1))
})

test_that("dots arguments are formulas", {
  expect_error(create_case_when(1))
})

vars <- c("x", "y", "z")
cw <- create_case_when(TRUE ~ x, vars = vars)

test_that("returned value is a closure", {
  expect_type(cw, "closure")
  expect_s3_class(cw, "function")
  expect_s3_class(cw, "case_when")
})

test_that("returned function has correct formals with missing args", {
  expect_equal(rlang::fn_fmls_names(cw), vars)
  lapply(rlang::fn_fmls(cw), function(x) expect_equal(x, rlang::missing_arg()))
})


test_that("returned function is a case_when", {
  cw <- create_case_when(x == 1 ~ "a",
                         x == 2 ~ "b",
                         TRUE ~ "z")
  expect_equal(cw(1:3), c("a", "b", "z"))
})

test_that("returned function works with mutate", {
  people <-
    dplyr::tribble(
      ~name, ~sex,
      "Mary", "F",
      "Henry", "M"
    )
  cw_sex <- create_case_when(x == "F" ~ "Woman",
                             x == "M" ~ "Man",
                             TRUE ~ as.character(x),
                             vars = "x")
  expect_equal(people %>% mutate(label = cw_sex(sex)),
               dplyr::tribble(
                 ~name, ~sex, ~label,
                 "Mary", "F", "Woman",
                 "Henry", "M", "Man"
               )
  )
})

test_that("formulas method returns a list of formula", {
  expect_type(formulas(cw), "list")
  lapply(formulas(cw), function(x) expect_type(x, "language"))
  lapply(formulas(cw), function(x) expect_s3_class(x, "formula"))
})

test_that("print method for case_when functions", {
  expect_output(print(cw), "<CASE WHEN>")
})
