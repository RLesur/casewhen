#' @include create_case_when.R
#' @importFrom dplyr sql_translate_env
#' @importFrom dbplyr sql_variant sql_translator
NULL

cacheClasses <- new.env(parent = emptyenv())

setOldClass("sql_variant")
setClass("CustomTranslation", slots = list(sql_translate_env = "sql_variant"))

#' @export
sql_translate_env.CustomTranslation <- function(con) {
  con@sql_translate_env
}

original_sql_functions <- function(con) {
  translate_env <- dplyr::sql_translate_env(con)
  fn_list <- c(as.list(translate_env$scalar),
               as.list(translate_env$aggregate),
               as.list(translate_env$window)
               )
  names(fn_list[!is_case_when(fn_list)])
}

get_case_when_funs <- function(con) {
  translate_env <- dplyr::sql_translate_env(con)
  fn_list <- c(as.list(translate_env$scalar),
               as.list(translate_env$aggregate),
               as.list(translate_env$window)
  )
  fn_list[is_case_when(fn_list)]
}

#' Add a customised translation to a connection
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
  fn_names <- names(fn_list)

  # Ensure that each element of fn_names is named
  dots_args <- as.list(match.call()[-(1:2)])
  if (is.null(fn_names)) {
    names(fn_list) <- as.character(dots_args)
  }
  else {
    names(fn_list) <- ifelse(nzchar(fn_names), fn_names, as.character(dots_args))
  }

  # Drop non case_when functions with warning
  is_cw <- is_case_when(fn_list)
  if (any(!is_cw)) {
    warning(paste(names(fn_list[!is_cw]), collapse = ", "),
            " dropped: not case_when function(s).\n"
            )
    fn_list <- fn_list[is_cw]
  }

  # Drop functions corresponding to a SQL name with warning
  reserved_names <- original_sql_functions(con)
  forbidden <- names(fn_list) %in% reserved_names
  if (any(forbidden)) {
    warning(paste(names(fn_list)[forbidden], collapse = ", "),
            " dropped: reserved SQL function(s).\n")
    fn_list <- fn_list[!forbidden]
  }

  if (length(fn_list) == 0) {
    warning("No function to add. Returning original connection object...\n")
    return(con)
  }

  # Create a new class and a new connection object with custom translation
  # These operations are embedded in a tryCatch():
  #   if an error occurs, the original con object is returned
  # This tryCatch() ensures a safe use of add_case_when with a pipe
  tryCatch(
    {
      # Test if con is an original connection
      if (!inherits(con, "CustomTranslation")) {
        connection_class <- class(con)
        new_class <- paste0("CustomisedTranslation", connection_class)
        # Create a new formal class
        if (!isClass(new_class, where = cacheClasses))
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
    },
    error = function(e) {
      warning("An error has occurred.\nError message:\n", e$message,
              "\nReturning original connection object...\n"
             )
      con
  })
}

#' @param object A customised connection.
#' @rdname add_case_when.DBIConnection
#' @export
setMethod("show", "CustomTranslation", function(object) {
  con_class <- sub("CustomisedTranslation", "", class(object))
  getMethod("show", con_class)(object)
  print(get_case_when_funs(object))
})
