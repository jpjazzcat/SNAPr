#' @title Actual Belowground Net Primary Production
#'
#' @description Calculation of belowground net primary production for a given site (g/m2), from Ritchie 2014.
#' @param RAIN Mean annual precipitation (mm/year).
#' @export

calc_BNPPest = function(RAIN) {
  BNPPest = 917.4-0.763*RAIN
  return(BNPPest)
}