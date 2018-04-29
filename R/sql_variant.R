#' @include create_case_when.R
NULL

#' Register a case_when function in a SQL translator
#'
#' @param cw A case_when function.
#' @param con A connection.
#'
#' @export
add_sql_variant <- function(cw, con = NULL) {
  variant <- dplyr::sql_translate_env(con = con)
  scalar <- variant[["scalar"]]
  case_when <- scalar$case_when
  cw_name <- deparse(substitute(cw))
  vars <- variable.names(cw)
  formulas <- formulas(cw)
  args <- c(formulas, list(vars = vars, fn = case_when))
  cw_sql <- do.call(.create_case_when, args)
  assign(cw_name, cw_sql, envir = scalar)
  invisible(cw_sql)
}
