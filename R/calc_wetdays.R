#' @title Wet Days
#'
#' @description Calculation of number of wet days per year able to support soil microbial respiration, from Ritchie 2014.
#' @param RAIN Mean annual precipitation (mm/year).
#' @param Gdays Number of days in growing season (days). Default is 240. 
#' @export

calc_wetdays = function(RAIN, Gdays) {
  wetdays = (0.00044*RAIN-0.025)*Gdays
  return(wetdays)
}