#' @import rlang methods
#' @importFrom assertthat assert_that
#' @importFrom pryr modify_lang
#' @importFrom purrr map walk
#' @importFrom dplyr case_when
#' @importFrom crayon cyan magenta green
#' @importFrom utils capture.output
#' @importFrom stats variable.names
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
  formulas <- rlang::dots_list(...)
  structure(
    .create_case_when(!!! formulas, vars = vars),
    class = c("dplyr_case_when", "case_when", "function")
  )
}
setOldClass("case_when")

#' Create a SQL translation of a case_when function
#'
#' @param ... A sequence of thwo sided formulas. The left hand side (LHS)
#'     determines which values match this case.The right hand side (RHS)
#'     provides the replacement value.
#'
#' The LHS must evaluate to a logical vector. Each logical vector can
#'     either have length 1 or a common length. All RHSs must evaluate to
#'     the same type of vector.
#'
#' These dots are evaluated with [explicit splicing](dplyr::tidy-dots).
#' @inheritParams create_case_when
#' @inheritParams dplyr::sql_translate_env
#' @keywords internal
#' @export
create_sql_case_when <- function(..., vars = "x", con = NULL) {
  formulas <- rlang::dots_list(...)
  case_when_con <- dplyr::sql_translate_env(con = con)$scalar$case_when
  args <- c(formulas, list(vars = vars, fn = case_when_con))
  structure(
    do.call(.create_case_when, args),
    class = c("sql_case_when", "case_when", "function")
  )
}

#' Get formulas
#'
#' `formulas` retrieves formulas from an object that depends on.
#'
#' @export
#' @param x An object used to select a method.
#' @param ... Other arguments passed on to methods.
#' @return A list of `formula` objects.
formulas <- function(x, ...) UseMethod("formulas")

#' @export
formulas.case_when <- function(x, ...) get("formulas", envir = environment(x))

#' @export
variable.names.case_when <- function(object, ...) get("vars", envir = environment(object))

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

.create_case_when <- function(..., vars, fn = dplyr::case_when) {
  assertthat::assert_that(is.character(vars))
  fun_fmls <- purrr::map(rlang::set_names(vars), ~ rlang::missing_arg())
  fun_body <- substitute({
    match_call <- match.call()
    args_call <- as.list(match_call[-1])
    modify_vars <- function(x) {
      if (is.name(x)) {
        if (as.character(x) %in% names(args_call))
          return(args_call[[as.character(x)]])
      }
      x
    }
    n <- length(formulas)
    new_formulas <- vector("list", n)
    for (i in seq_len(n)) {
      lhs <- rlang::f_lhs(formulas[[i]])
      rhs <- rlang::f_rhs(formulas[[i]])
      new_lhs <- pryr::modify_lang(lhs, modify_vars)
      new_rhs <- pryr::modify_lang(rhs, modify_vars)
      new_formulas[[i]] <- rlang::new_formula(new_lhs, new_rhs, env = parent.frame())
    }
    do.call(fn, new_formulas)
  })
  formulas <- rlang::dots_list(...)
  purrr::walk(formulas,
              ~ assertthat::assert_that(rlang::is_formula(.x),
                                        msg = "An argument is not a formula."
              )
  )
  rlang::new_function(fun_fmls, fun_body)
}

.translate_to_sql <- function(cw_fn, con) {
  if (is.list(cw_fn)) {
    return(lapply(cw_fn, .translate_to_sql, con = con))
  } else {
    formulas <- formulas(cw_fn)
    vars <- variable.names(cw_fn)
    create_sql_case_when(!!! formulas, vars = vars, con = con)
  }
}
