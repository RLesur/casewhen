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

test_that("Non case_when arguments are thrown by add_case_when", {
  expect_warning(con2 <- add_case_when(con, sum, pi), regexp = "case_when")
  expect_equal(con, con2)

  expect_warning(con2 <- add_case_when(con, sum, cw_sex), regexp = "case_when")
  expect_equal(length(get_case_when_funs(con2)), 1)
})

test_that("SQL reserved functions are thrown by add_case_when", {
  expect_warning(con2 <- add_case_when(con, mean = cw_sex, mean = cw_sex), regexp = "reserved")
  expect_equal(con, con2)

  expect_warning(con2 <- add_case_when(con, cw_sex, mean = cw_sex), regexp = "reserved")
  expect_equal(length(get_case_when_funs(con2)), 1)
})

test_that("new connection object class", {
  expect_s4_class(add_case_when(con, cw_sex), class(con))
  expect_s4_class(add_case_when(con, cw_sex), "CustomTranslation")
})

test_that("A previously sealed formal class does not lead to error", {
  expect_silent(con2 <- add_case_when(con, cw_sex))
  expect_silent(con3 <- add_case_when(con, cw_sex))
  expect_equivalent(con2, con3)
})

test_that("One can reuse the new connection object", {
  expect_silent(con2 <- add_case_when(con, cw_sex))
  expect_silent(con3 <- add_case_when(con2, cw_sex2 = cw_sex))
  expect_equal(length(get_case_when_funs(con3)), 2)
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

test_that("show method throws output", {
  expect_silent(con2 <- add_case_when(con, cw_sex))
  expect_output(show(con2), regexp = "<SQLiteConnection>")
})
