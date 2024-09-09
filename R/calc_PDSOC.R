#' @title Plant-derived Soil Organic Carbon Inputs 
#'
#' @description Calculation of plant-derived soil organic carbon inputs under the specified grazing management system (g/m2), from Ritchie 2014.
#' @param LIGCELL Lignin and cellulose fraction of aboveground plant biomass (0 - 1).
#' @param ANPPest Actual aboveground net primary production (g/m2).
#' @param GI Grazing intensity (proportion of aboveground biomass removed per year, 0 - 1).
#' @param FIRE Proportion of aboveground biomass removed by fire per year (0 - 1).
#' @param BNPPest Belowground net primary production (g/m2).
#' @export

calc_PDSOC = function(LIGCELL, ANPPest, GI, FIRE, BNPPest) {
  PDSOC = 0.45*(LIGCELL*ANPPest*(1-GI)*(1-FIRE)+(LIGCELL+0.05)*BNPPest)
  return(PDSOC)
}