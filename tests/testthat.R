library(testthat)
library(casewhen)

people <-
  dplyr::tribble(
    ~name, ~sex, ~seek,
    "Mary", "F", "M",
    "Henry", "M", "F"
  )

test_check("casewhen")
