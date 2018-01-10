#' @title Min max scaling
#' @param x the variable to scale
#' @details returns a variable scaled to between 0 and 1
#'
#' @examples
#' set.seed(204)
#' x<-rpois(30, 10)
#' minmax.scale(x)
#'
#' @export
#'

minmax.scale<-function(x){
  scaled<-(x-min(x))/(max(x)-min(x))
  return(scaled)
}
