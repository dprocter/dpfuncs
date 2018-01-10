#' @title Window apply
#' @description Apply across a window,
#' @param x the variable to apply with a window
#' @param width  window width
#' @param by how often to apply the window
#' @param FUN function to apply
#'
#' @details An apply function for windows, similar to rollapply in the zoo package
#' @export

wapply <- function(x, width, by = NULL, FUN = NULL, ...)
{
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width

  lenX <- length(x)
  SEQ1 <- seq(1, lenX - width + 1, by = by)
  SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))

  OUT <- lapply(SEQ2, function(a) FUN(x[a], ...))
  OUT <- base:::simplify2array(OUT, higher = TRUE)
  return(OUT)
}
