#' @title SNAP  delta Wrapper Function 
#'
#' @description A wrapper function around all SNAP model equations to calculate delta SOC over a given number of years based on the below input parameters.
#' @param RAIN Mean annual precipitation (mm/yr)
#' @param GI Grazing intensity (proportion of aboveground biomass removed per year, 0 - 1).
#' @param FIRE Proportion of aboveground biomass removed by fire per year (0 - 1).
#' @param LIGCELL Lignin and cellulose fraction of aboveground plant biomass (0 - 1).
#' @param SAND Sand fraction in the top 40 cm of soil (0 - 1).
#' @param SOC Initial soil organic carbon stocks to 40 cm depth (tSOC/ha).
#' @param Gdays Number of days in growing season (days). Default is 240.
#' @param years Number of years over which to run the simulation.
#' @export

SNAP_delta = function(RAIN, GI, FIRE, LIGCELL, SAND, SOC, Gdays = 240, years) {
  # Add input validation and defaults here
  if(RAIN<0|RAIN>10000){
    stop("ERROR: RAIN must be between 0 and 10000")
  } else if(GI<0|GI>1){
    stop("ERROR: GI mus be between 0 and 1")
  } else if(FIRE<0|FIRE>1){
    stop("ERROR: FIRE must be between 0 and 1")
  } else if(LIGCELL<0|LIGCELL>1){
    stop("ERROR: LIGCELL must be between 0 and 1")
  } else if(SAND<0|SAND>1){
    stop("ERROR: SAND must be between 0 and 1")
  } else if(Gdays<0|Gdays>365){
    stop("ERROR: Gdays must be between 0 and 365")
  } else if(years<1){
    stop("ERROR: years must be greater than 1")
  }
  
  if(is.na(Gdays)){
    Gdays = 240
  }
  
  # Plant productivity
  ANPPmax = calc_ANPPmax(RAIN, SAND)
  LEAFFRAC = calc_LEAFFRAC(GI)
  LAI = calc_LAI(GI, LEAFFRAC)
  ANPPest = calc_ANPPest(ANPPmax, LAI)
  BNPPest = calc_BNPPest(RAIN)
  
  # Soil carbon cycling
  PDSOCt = calc_PDSOC(LIGCELL, ANPPest, GI, FIRE, BNPPest)
  DDSOCt = calc_DDSOC(LIGCELL, ANPPest, GI)
  wetdays = calc_wetdays(RAIN, Gdays)
  
  # SOC over time
  soc_dataframe = data.frame(year = 0:years, soc = NA)
  
  for(i in 0:years){
    if(i==0){
      soc_dataframe[[2]][1] = SOC
    } else{
      x = soc_dataframe[[2]][i]
      deltaSOC = calc_deltaSOC(PDSOCt, DDSOCt, wetdays, SAND, SOC = x)
      SOCi = x+deltaSOC
      soc_dataframe[[2]][i+1] = SOCi
    }
  }

  return(soc_dataframe)
  
}