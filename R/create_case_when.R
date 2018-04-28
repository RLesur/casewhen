#' @import rlang
#' @importFrom assertthat assert_that
#' @importFrom pryr modify_lang
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
    # for (name in var) {
    #   symb <- rlang::eval_bare(rlang::sym(name))
    #   var <- rlang::eval_tidy(rlang::enquo(symb))
    #   assign(name, var)
    # }
    # forms <- purrr::map(formulas, rlang::`f_env<-`, value = environment())
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
      new_formulas[[i]] <- rlang::new_formula(new_lhs, new_rhs, env = rlang::caller_env())
    }
    do.call(dplyr::case_when, new_formulas)
  })
  formulas <- rlang::dots_list(...)
  purrr::walk(formulas,
              ~ assertthat::assert_that(rlang::is_formula(.x),
                                        msg = "An argument is not a formula."
              )
  )
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
#' @param ... Other arguments passed on to methods.
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

add_sql_variant.NULL <- function(cw) {
  sql_translate_env <- getAnywhere("sql_translate_env.NULL")$objs[[1]]
  variant <- sql_translate_env()
  scalar <- variant[["scalar"]]
  case_when <- scalar$case_when
  cw_name <- deparse(substitute(cw))
  assign(cw_name, case_when, envir = scalar)
}
