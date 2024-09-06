#' @title Actual Aboveground Net Primary Production
#'
#' @description Calculation of aboveground net primary production under the specified grazing management system (g/m2), from Ritchie 2014.
#' @param ANPPmax Maximum potential aboveground net primary production (g/m2).
#' @param LAI Leaf area index per unit ground area.
#' @export

calc_ANPPest = function(ANPPmax, LAI) {
  ANPPest = ANPPmax*LAI
  return(ANPPest)
}