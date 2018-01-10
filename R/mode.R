#' @title mode (average)
#' @description returns the most frequently occuring value in a variable
#' @param x a variable
#' @export
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
