context("test-create_case_when.R")

library(dplyr)

test_that("x argument is a character vector", {
  expect_error(create_case_when(TRUE ~ x, vars = 1))
  expect_error(create_sql_case_when(TRUE ~ x, vars = 1))
})

test_that("dots arguments are formulas", {
  expect_error(create_case_when(1))
  expect_error(create_sql_case_when(1))
})

vars <- c("x", "y", "z")
cw <- create_case_when(TRUE ~ x, vars = vars)
cw_sql <- create_sql_case_when(TRUE ~ x, vars = vars)

test_that("returned value is a closure", {
  expect_type(cw, "closure")
  expect_type(cw_sql, "closure")
  expect_s3_class(cw, "function")
  expect_s3_class(cw_sql, "function")
})

test_that("returned value class", {
  expect_s3_class(cw, "case_when")
  expect_s3_class(cw_sql, "case_when")
  expect_s3_class(cw, "dplyr_case_when")
  expect_s3_class(cw_sql, "sql_case_when")
})

test_that("returned function has correct formals with missing args", {
  expect_equal(rlang::fn_fmls_names(cw), vars)
  expect_equal(rlang::fn_fmls_names(cw_sql), vars)
  lapply(rlang::fn_fmls(cw), function(x) expect_equal(x, rlang::missing_arg()))
  lapply(rlang::fn_fmls(cw_sql), function(x) expect_equal(x, rlang::missing_arg()))
})

test_that("is_case_when detects case_when functions", {
  expect_equal(is_case_when(cw), TRUE)
  expect_equal(is_case_when(cw_sql), TRUE)
  expect_equal(is_case_when(mean), FALSE)
})

test_that("formulas method returns a list of formula", {
  expect_type(formulas(cw), "list")
  expect_type(formulas(cw_sql), "list")
  lapply(formulas(cw), function(x) expect_type(x, "language"))
  lapply(formulas(cw_sql), function(x) expect_type(x, "language"))
  lapply(formulas(cw), function(x) expect_s3_class(x, "formula"))
  lapply(formulas(cw_sql), function(x) expect_s3_class(x, "formula"))
})

test_that("create_case_when returns a function that is a dplyr::case_when()", {
  cw <- create_case_when(x == 1 ~ "a",
                         x == 2 ~ "b",
                         TRUE ~ "z")
  expect_equal(cw(1:3), c("a", "b", "z"))
})

test_that("create_case_when returns a function that works with mutate", {
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
  people_label <- people %>% mutate(label = cw_sex(sex))
  expect_equal(people_label,
               dplyr::tribble(
                 ~name, ~sex, ~label,
                 "Mary", "F", "Woman",
                 "Henry", "M", "Man"
               )
  )
})

test_that("variable.names returns the vars argument", {
  vars <- c("x", "y", "z")
  cw <- create_case_when(TRUE ~ x, vars = vars)
  cw_sql <- create_sql_case_when(TRUE ~ x, vars = vars)
  expect_equal(vars, variable.names(cw))
})

test_that("print method for case_when functions", {
  expect_output(print(cw), "<CASE WHEN>")
  expect_output(print(cw_sql), "<CASE WHEN>")
})
