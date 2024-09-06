#' @title Maximum Potential Above-ground Net Primary Production
#'
#' @description Calculation of the parameter ANPPmax, the maximum potential grass production for a given site, from Ritchie 2014.
#' @param RAIN Mean annual precipitation (mm/year).
#' @param SAND Sand fraction in the top 30 cm of soil (0 - 1).
#' @export

calc_ANPPmax = function(RAIN, SAND) {
  ANPPmax = (0.84*RAIN-27.5)*(1.33-0.0075*SAND)
  return(ANPPmax)
}