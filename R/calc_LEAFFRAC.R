#' @title Proportion of Aboveground Biomass as Leaf
#'
#' @description Calculation of the parameter LEAFFRAC, the fraction of aboveground plant biomass that is leaf tissue, from Ritchie 2014.
#' @param GI Grazing intensity (proportion of aboveground biomass removed per year, 0 - 1).
#' @export

calc_LEAFFRAC = function(GI) {
  LEAFFRAC = 0.597+0.24*GI
  return(LEAFFRAC)
}