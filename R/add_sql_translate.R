#' @include create_case_when.R
#' @importFrom dplyr sql_translate_env
NULL

#' Register a function in a SQL translator
#'
#' @param fn A function to register.
#' @param con A database connection.
#' @param ... Other arguments passed on to method.
#'
#' @export
add_sql_translate <-
  function(fn, con = NULL, ...) UseMethod("add_sql_translate")

add_sql_translate.sql_case_when <- function(fn, con = NULL, name, ...) {
  scalar_env_con <- dplyr::sql_translate_env(con = con)[["scalar"]]
  assign(name, fn, envir = scalar_env_con)
}

#' @describeIn add_sql_translate Register a case_when function in a SQL
#'   translate environment
#' @export
add_sql_translate.case_when <- function(fn, con = NULL, ...) {
  formulas <- formulas(fn)
  vars <- variable.names(fn)
  name <- deparse(substitute(fn))
  cw_sql <- .create_sql_case_when(!!! formulas, vars = vars, con = con)
  add_sql_translate(cw_sql, con = NULL, name = name, ...)
  add_sql_translate(cw_sql, con = con, name = name, ...)
}

.create_sql_case_when <- function(..., vars = "x", con = NULL) {
  formulas <- rlang::dots_list(...)
  case_when_con <- dplyr::sql_translate_env(con = con)[["scalar"]]$case_when
  args <- c(formulas, list(vars = vars, fn = case_when_con))
  cw <- do.call(.create_case_when, args)
  structure(
    do.call(.create_case_when, args),
    class = c("sql_case_when", "function")
  )
}
