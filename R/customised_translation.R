#' @include create_case_when.R
#' @importFrom dbplyr sql_variant sql_translator
NULL

setOldClass("sql_variant")

cacheClasses <- new.env(parent = emptyenv())

reserved_names <- names(dplyr::sql_translate_env(NULL))

setClass("CustomTranslation", slots = list(sql_translate_env = "sql_variant"))

#' @export
sql_translate_env.CustomTranslation <- function(con) {
  con@sql_translate_env
}

#' Add a customised translation to an connection
#' @param con A database connection.
#' @param ... Other arguments.
#' @export
add_case_when <-
  function(con, ...) UseMethod("add_case_when")

#' Add a custom SQL translation to a DBI connection
#'
#' @return A new DBIConnection object.
#' @param con A DBIConnection object.
#' @param ... Not used.
#' @export
add_case_when.DBIConnection <- function(con, ...) {
  fn_list <- list(...)
  lapply(fn_list, function(x) assertthat::assert_that(inherits(x, "case_when")))

  fn_names <- names(fn_list)
  # Ensure that each element of fn_names has a name
  dots_args <- as.list(match.call()[-(1:2)])
  if (is.null(fn_names)) {
    names(fn_list) <- as.character(dots_args)
  }
  else {
    names(fn_list) <- ifelse(nzchar(fn_names), fn_names, as.character(dots_args))
  }

  # Throw an error if a function's name is reserved
  if (any(forbidden <- names(fn_list) %in% reserved_names))
    stop(paste(names(fn_list)[forbidden], collapse = ", "), ": reserved word(s).")

  if (!inherits(con, "CustomTranslation")) {
    # Here, con is a pure DBIConnection
    connection_class <- class(con)
    new_class <- paste0("CustomisedTranslation", connection_class)
    if (!isClass(new_class))
      setClass(new_class,
               contains = c("CustomTranslation", connection_class),
               where = cacheClasses
      )
  } else {
    new_class <- class(con)
  }

  translate_env <- dplyr::sql_translate_env(con)

  variant_translate_env <- dbplyr::sql_variant(
    scalar = dbplyr::sql_translator(
      .parent = translate_env$scalar,
      .funs = .translate_to_sql(fn_list, con = con)
    ),
    aggregate = translate_env$aggregate,
    window = translate_env$window
  )

  custom_translation <- new("CustomTranslation",
                            sql_translate_env = variant_translate_env
                            )
  new(new_class, custom_translation, con)
}
