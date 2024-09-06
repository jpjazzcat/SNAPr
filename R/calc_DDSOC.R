#' @title Dung-derived Soil Organic Carbon Inputs 
#'
#' @description Calculation of dung-derived soil organic carbon inputs under the specified grazing management system (g/m2), from Ritchie 2014.
#' @param LIGCELL Lignin and cellulose fraction of plant biomass (0 - 1).
#' @param ANPPest Actual aboveground net primary production (g/m2).
#' @param GI Grazing intensity (proportion of aboveground biomass removed per year, 0 - 1).
#' @export

calc_DDSOC = function(LIGCELL, ANPPest, GI) {
  DDSOC = 0.45*LIGCELL*ANPPest*GI
  return(DDSOC)
}