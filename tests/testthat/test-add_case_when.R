context("test-add_case_when.R")

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

teardown(DBI::dbDisconnect(con))

people <-
  tribble(
    ~name, ~sex, ~seek,
    "Mary", "F", "M",
    "Henry", "M", "F"
  )

cw_sex <- create_case_when(x == "F" ~ "Woman",
                           x == "M" ~ "Man",
                           TRUE ~ as.character(x),
                           vars = "x")

test_that("add_case_when works without dots argument", {
  expect_warning(con2 <- add_case_when(con))
  expect_equal(con, con2)
})

test_that("new connection object class", {
  expect_s4_class(add_case_when(con, cw_sex), class(con))
  expect_s4_class(add_case_when(con, cw_sex), "CustomTranslation")
})

test_that("request with new conn leads to the same result than local computation", {
  local_result <-
    people %>%
    mutate(label = case_when(
      sex == "F" ~ "Woman",
      sex == "M" ~ "Man",
      TRUE ~ as.character(sex)
    ))
  remote_result <-
    con %>%
    add_case_when(cw_sex) %>%
    copy_to(people, overwrite = TRUE) %>%
    mutate(label = cw_sex(sex)) %>%
    collect()

  expect_equal(local_result, remote_result)
})
