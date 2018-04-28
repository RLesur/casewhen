#' @export
add_sql_variant <- function(con = NULL, cw) {
  #sql_translate_env <- utils::getAnywhere("sql_translate_env.NULL")$objs[[1]]
  variant <- dplyr::sql_translate_env(con = con)
  scalar <- variant[["scalar"]]
  case_when <- scalar$case_when
  cw_name <- deparse(substitute(cw))
  vars <- variable.names(cw)
  formulas <- formulas(cw)
  list(vars = vars, fn = case_when)
  cw_sql <- .create_case_when(!!! formulas, vars = vars, fn = case_when)
  return(cw_sql)
  assign(cw_name, cw_sql, envir = scalar)
}
