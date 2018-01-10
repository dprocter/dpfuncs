#' @title box cox transformation
#' @description returns a box cox transformed variable,
#' @param x the variable to transform
#' @param scale the scaling power you want to use
#' @details Detects whether there are zeros in data and adds a constant of 1 if there are
#'
#'
#' @examples
#' set.seed(204)
#' x<-rpois(30, 10)
#' box.cox(x)
#'
#' @export

box.cox<-function(x,scale){
  if (min(x)<0){
    stop("box cox transform requires positive data")
  }
  if (min(x)==0){
    c<-1
  } else {
    c<-0
  }
  if (scale==0){
    bcnorm<-log(x+c)
  } else{
    bcnorm<-((x+c)^scale-1)/(scale)
  }
  return(bcnorm)
}
