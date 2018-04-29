context("test-add_sql_translate.R")

library(dplyr)
library(dbplyr)

test_that("translate sql works", {
  cw <- create_case_when(x == 1 ~ TRUE)
  add_sql_translate(cw)
  expect_equal(translate_sql(case_when(y == 1 ~ TRUE)), translate_sql(cw(y)))
})
