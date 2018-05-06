#' @include utils.R
#' @import rlang methods
#' @importFrom assertthat assert_that
#' @importFrom pryr modify_lang
#' @importFrom purrr map walk
#' @importFrom dplyr case_when
#' @importFrom crayon cyan magenta green
#' @importFrom utils capture.output
#' @importFrom stats variable.names
NULL

#' A case_when factory
#'
#' `create_case_when` allows to create reusable [dplyr::case_when()] functions.
#'  It returns a function that can be used in place of
#'     [dplyr::case_when()]. The arguments of the returned function are
#'     determined by the `vars` argument of `create_case_when()`.
#'
#' The returned function is of class `case_when`.
#'
#' @inheritParams dplyr::case_when
#' @param vars A character vector that determined the names of the arguments
#'     of the returned function.
#' @return A function, usable in place of [dplyr::case_when()].
#' @export
#' @examples
#' x <- 1:50
#' y <- 51:100
#'
#' cw_fb <- create_case_when(
#'   number %% 35 == 0 ~ "fizz buzz",
#'   number %% 5 == 0 ~ "fizz",
#'   number %% 7 == 0 ~ "buzz",
#'   TRUE ~ as.character(number),
#'   vars = "number"
#' )
#'
#' cw_fb(number = x)
#' cw_fb(number = y)
#'
#' # Formulas and variable names can be extracted
#' patterns <- formulas(cw_fb)
#' var_name <- variable.names(cw_fb)
#'
#' # Dots support splicing
#' create_case_when(!!! patterns, vars = var_name)
create_case_when <- function(..., vars = "x") {
  formulas <- rlang::dots_list(...)
  structure(
    .create_case_when(!!! formulas, vars = vars),
    class = c("dplyr_case_when", "case_when", "function")
  )
}

#' Create a reusable SQL case_when function
#'
#' @inheritParams create_case_when
#' @inheritParams dplyr::sql_translate_env
#' @keywords internal
#' @export
#' @examples
#' library(dplyr)
#' library(dbplyr)
#'
#' con <- structure(
#'   list(),
#'   class = c("TestCon", "DBITestConnection", "DBIConnection")
#' )
#'
#' cw_fb <- create_sql_case_when(
#'   number %% 35 == 0 ~ "fizz buzz",
#'   number %% 5 == 0 ~ "fizz",
#'   number %% 7 == 0 ~ "buzz",
#'   TRUE ~ as.character(number),
#'   vars = "number",
#'   con = con
#' )
#'
#' testcon_var <- sql_variant(
#'   sql_translator(
#'     cw_fb = cw_fb,
#'     .parent = sql_translate_env(con)$scalar
#'   ),
#'   sql_translate_env(con)$aggregate,
#'   sql_translate_env(con)$window
#' )
#'
#' sql_translate_env.TestCon <- function(x) testcon_var
#'
#' translate_sql(cw_fb(x), con = con)
create_sql_case_when <- function(..., vars = "x", con = NULL) {
  formulas <- rlang::dots_list(...)
  case_when_con <- dplyr::sql_translate_env(con = con)$scalar$case_when
  args <- c(formulas, list(vars = vars, fn = case_when_con))
  structure(
    do.call(.create_case_when, args),
    class = c("sql_case_when", "case_when", "function")
  )
}

is_case_when <- function(x) {
  if (is.list(x)) return(vapply(x, is_case_when, FUN.VALUE = logical(1)))
  inherits(x, "case_when")
}

#' Get formulas
#'
#' `formulas` retrieves formulas from an object that depends on.
#'
#' @export
#' @param x An object used to select a method.
#' @param ... Other arguments passed on to methods.
#' @keywords internal
#' @return A list of `formula` objects.
formulas <- function(x, ...) UseMethod("formulas")

#' @describeIn create_case_when Get the formulas of a `case_when` function.
#' @param x,object A `case_when` function, created by `create_case_when`.
#' @export
formulas.case_when <- function(x, ...) get("formulas", envir = environment(x))

#' @describeIn create_case_when Get the variable names of a `case_when` function.
#' @export
variable.names.case_when <- function(object, ...) get("vars", envir = environment(object))

#' @describeIn create_case_when Print informations about a `case_when` function.
#' @export
print.case_when <- function(x, ...) {
  formulas <- formulas(x)
  var_names <- variable.names(x)
  n_forms <- length(formulas)
  n_vars <- length(var_names)
  out <- utils::capture.output(purrr::walk(formulas, print, showEnv = FALSE))
  out <- c(
    crayon::cyan("<CASE WHEN>"),
    crayon::magenta(n_vars, paste0("variable", plural(var_names), ":"),
                    paste(var_names, collapse = ", ")
                    ),
    crayon::magenta(n_forms, paste0("condition", plural(formulas), ":")),
    crayon::green(paste("->", out)), ""
  )
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
