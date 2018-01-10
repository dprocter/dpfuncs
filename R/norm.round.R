#' @title Alternative to round
#' @description An alternative round that deals with the fact that 0.5 rounds to either 0 or 1
#' , the round would always round to 1
#' @param x the number/variable to round
#' @param digits  the number of digits to round to

#'
#' @details
#' a different version of round, shamelessly stolen from here:
#' https://stackoverflow.com/questions/12688717/round-up-from-5-in-r
#' this deals with the fact that the standard version of round in r sometimes rounds 0.5
#' up and sometimes down, for good reasons but reasons that do not apply in some circumstances

#' @export

norm.round = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5
  z = trunc(z)
  z = z/10^digits
  z*posneg
}
