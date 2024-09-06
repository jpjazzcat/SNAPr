#' @title Leaf Area Index
#'
#' @description Calculation of the parameter LAI, the leaf area index per unit ground area, from Ritchie 2014.
#' @param GI Grazing intensity (proportion of aboveground biomass removed per year, 0 - 1).
#' @param LEAFFRAC Leaf fraction (the proportion of aboveground biomass that is leaf tissue, 0 - 1)
#' @export

calc_LAI = function(GI, LEAFFRAC) {
  LAI = (LEAFFRAC/0.6)-0.015*exp(4.6*GI)
  return(LAI)
}