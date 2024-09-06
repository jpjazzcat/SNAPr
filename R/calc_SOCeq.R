#' @title Equilibrium SOC Stock 
#'
#' @description Calculation of the SOC stock (tSOC/ha) at equilibrium under the specified grazing management system, from Ritchie 2014. This is the point at which SOC inputs from plants and dung are offset by SOC losses due to microbial respiration (dsoc = 0).
#' @param PDSOC Plant-derived soil organic carbon inputs (g/m2).
#' @param DDSOC Dung-derived soil organic carbon inputs (g/m2).
#' @param wetdays Number of days wet enough to support soil microbial respiration (days).
#' @param SAND Sand fraction in the top 30 cm of soil (0 - 1).
#' @export

calc_SOCeq = function(PDSOC, DDSOC, wetdays, SAND) {
  SOCeq = ((PDSOC+DDSOC+(wetdays*0.579)*(0.7+0.3*SAND))/((0.00044*wetdays)*(0.7+0.3*SAND)))/100
  return(SOCeq)
}