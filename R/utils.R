#' @include casewhen-package.R
NULL

plural <- function(x, plural = "s") {
  if (length(x) > 1)
    return(plural)
  else
    ""
}
