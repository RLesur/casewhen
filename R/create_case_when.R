#' @import rlang
#' @importFrom assertthat assert_that
#' @importFrom purrr map walk
#' @importFrom dplyr case_when
#' @importFrom crayon cyan magenta green
#' @importFrom utils capture.output
NULL

#' A `case_when` factory
#'
#' This function allows to create reusable `dplyr::case_when()` functions.
#'
#' @inheritDotParams dplyr::case_when
#' @param vars A character vector.
#' @return A function, usable in place of `dplyr::case_when`.
#' @export
#' @examples
#' library(dplyr)
#'
#' people <- tribble(
#'   ~name, ~sex, ~seek,
#'   "Mary", "F", "M",
#'   "Henry", "M", "F"
#' )
#'
#' cw_sex <- create_case_when(x == "F" ~ "Woman",
#'                            x == "M" ~ "Man",
#'                            TRUE ~ as.character(x),
#'                            vars = c("x"))
#'
#' people %>%
#'   mutate(sex_label = cw_sex(sex), seek_label = cw_sex(seek))
create_case_when <- function(..., vars = "x") {
  assertthat::assert_that(is.character(vars))
  fun_fmls <- purrr::map(rlang::set_names(vars), ~ rlang::missing_arg())
  fun_body <- substitute({
    for (name in var) {
      symb <- rlang::eval_bare(rlang::sym(name))
      var <- rlang::eval_tidy(rlang::enquo(symb))
      assign(name, var)
    }
    forms <- purrr::map(formulas, rlang::`f_env<-`, value = environment())
    do.call(dplyr::case_when, forms)
  })
  formulas <- rlang::dots_list(...)
  var <- vars
  structure(
    rlang::new_function(fun_fmls, fun_body),
    class = c("case_when", "function")
  )
}

#' Get formulas
#'
#' `formulas` retrieves formulas from an object that depends on.
#'
#' @export
#' @param x An object used to select a method.
#' @param ... Other arguments passed to methods.
#' @return A list of `formula` objects.
formulas <- function(x, ...) UseMethod("formulas")

#' @export
formulas.case_when <- function(x, ...) get("formulas", envir = environment(x))

#' @export
print.case_when <- function(x, ...) {
  formulas <- formulas(x)
  n <- length(formulas)
  out <- utils::capture.output(purrr::walk(formulas, print, showEnv = FALSE))
  out <- c(crayon::cyan("<CASE WHEN>"),
           crayon::magenta(paste(n, "conditions:")),
           crayon::green(paste("->", out)), "")
  cat(paste0(out, collapse = "\n"))
  invisible(x)
}
