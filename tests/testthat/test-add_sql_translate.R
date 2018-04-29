context("test-add_sql_translate.R")

library(dplyr)
library(dbplyr)

test_that("translate sql works with con NULL", {
  cw <- create_case_when(x == 1 ~ TRUE)
  add_sql_translate(cw)
  expect_equal(translate_sql(case_when(y == 1 ~ TRUE)), translate_sql(cw(y)))
})

test_that("translate sql works with SQLite", {
  cw2 <- create_case_when(y == 2 ~ TRUE, vars = "y")
  con <- structure(
    list(),
    class = c("SQLiteConnection", "DBIConnection")
  )
  add_sql_translate(cw2, con = con)
  expect_equal(translate_sql(case_when(z == 2 ~ TRUE), con = con),
               translate_sql(cw2(z), con = con))
})
