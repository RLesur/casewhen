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
  con <- simulate_sqlite()
  add_sql_translate(cw2, con = con)
  expect_equal(translate_sql(case_when(z == 2 ~ TRUE), con = con),
               translate_sql(cw2(z), con = con))

  df <- memdb_frame(x = c(1, 2), y = c(3, 4))
  cw3 <- create_case_when(y == 1 ~ TRUE, TRUE ~ FALSE, vars = "y")
  add_sql_translate(cw3, con = con)
  expect_equal(
    capture.output(df %>% mutate(z = case_when(x == 1 ~ TRUE, TRUE ~ FALSE)) %>% show_query()),
    capture.output(df %>% mutate(z = cw3(x)) %>% show_query())
  )
})
