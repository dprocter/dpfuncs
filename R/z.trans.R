#' @title z transformation
#' @param x the variable to transform
#' @details returns a z transformed variable, with mean 0 and sd 1
#'
#' @examples
#' set.seed(204)
#' x<-rpois(30, 10)
#' z.trans(x)
#'
#' @export

z.trans<-function(x){
  z.transformed<-(x-(mean(x)))/sd(x)
  return(z.transformed)
}
