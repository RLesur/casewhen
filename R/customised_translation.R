#' @include create_case_when.R
NULL

setOldClass("sql_variant")

cacheClasses <- new.env(parent = emptyenv())

setClass("CustomTranslation", slots = list(sql_translate_env = "sql_variant"))

#' Add a customised translation to an object
#' @param object An object.
#' @param ... Other arguments.
#' @export
add_custom_translation <-
  function(object, ...) UseMethod("add_custom_translation")

#' Add a custom SQL translation to a DBI connection
#'
#' @return A new DBIConnection object.
#' @param object A DBIConnection object.
#' @param ... Not used.
#' @export
add_custom_translation.DBIConnection <- function(object, ...) {
  connection_class <- class(object)
  new_class <- paste0("CustomisedTranslation", connection_class)
  setClass(new_class,
           contains = c("CustomTranslation", connection_class),
           where = cacheClasses,
           sealed = FALSE
  )
  custom_translation <- new("CustomTranslation",
                            sql_translate_env = dplyr::sql_translate_env(object)
                            )
  new(new_class, custom_translation, object)
}
