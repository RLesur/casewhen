#' @include create_case_when.R
#' @importFrom dplyr sql_translate_env
NULL

#' Register a function in a SQL translator
#'
#' @param fn A function to register.
#' @param con A connection.
#' @param ... Other arguments passed on to method.
#'
#' @export
add_sql_translate2 <-
  function(fn, con = NULL, ...)
    UseMethod("add_sql_translate2")

add_sql_translate2.sql_case_when <- function(fn, con = NULL, name, ...) {
  scalar_env_con <- dplyr::sql_translate_env(con = con)[["scalar"]]
  assign(name, fn, envir = scalar_env_con)
}

#' Register a case_when function
#'
#' @export
add_sql_translate2.case_when <- function(fn, con = NULL, ...) {
  formulas <- formulas(fn)
  vars <- variable.names(fn)
  name <- deparse(substitute(fn))
  cw_sql <- .create_sql_case_when(!!! formulas, vars = vars, con = con)
  add_sql_translate2(cw_sql, con = con, name = name, ...)
}

#' Register a case_when function in a SQL translator
#'
#' @param cw A case_when function.
#' @param con A connection.
#'
#' @export
add_sql_translate <- function(cw, con = NULL) {
  scalar_env_con <- dplyr::sql_translate_env(con = con)[["scalar"]]
  formulas <- formulas(cw)
  vars <- variable.names(cw)
  name <- deparse(substitute(cw))
  cw_sql <- .create_sql_case_when(!!! formulas, vars = vars, con = con)
  assign(name, cw_sql, envir = scalar_env_con)
  invisible(cw_sql)
}

.create_sql_case_when <- function(..., vars = "x", con = NULL) {
  formulas <- rlang::dots_list(...)
  case_when_con <- dplyr::sql_translate_env(con = con)[["scalar"]]$case_when
  args <- c(formulas, list(vars = vars, fn = case_when_con))
  cw <- do.call(.create_case_when, args)
  class(cw) <- c("sql_case_when", "function")
  cw
}
