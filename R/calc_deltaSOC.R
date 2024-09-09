#' @title Delta SOC
#'
#' @description Calculation of the change in SOC stock (tSOC/ha) for year t using plant-derived soil organic carbon (PDSOCt), dung-derived soil organic carbon (DDSOC), and microbial respiration.
#' @param PDSOC Plant-derived soil organic carbon inputs (g/m2).
#' @param DDSOC Dung-derived soil organic carbon inputs (g/m2).
#' @param wetdays Number of days wet enough to support soil microbial respiration (days).
#' @param SAND Sand fraction in the top 40 cm of soil (0 - 1).
#' @param SOC Initial soil organic carbon stocks to 40 cm depth (tSOC/ha).
#' @export

calc_deltaSOC = function(PDSOC, DDSOC, wetdays, SAND, SOC) {
  MRESP = wetdays*(0.7+0.3*SAND)*(0.00044*(SOC*100)-0.579)
  deltaSOC = (PDSOC+DDSOC-MRESP)/100
  
  return(deltaSOC)
}