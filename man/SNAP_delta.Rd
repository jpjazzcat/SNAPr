% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SNAP_delta.R
\name{SNAP_delta}
\alias{SNAP_delta}
\title{SNAP  delta Wrapper Function}
\usage{
SNAP_delta(RAIN, GI, FIRE, LIGCELL, SAND, SOC, Gdays = 240, years)
}
\arguments{
\item{RAIN}{Mean annual precipitation (mm/yr)}

\item{GI}{Grazing intensity (proportion of aboveground biomass removed per year, 0 - 1).}

\item{FIRE}{Proportion of aboveground biomass removed by fire per year (0 - 1).}

\item{LIGCELL}{Lignin and cellulose fraction of aboveground plant biomass (0 - 1).}

\item{SAND}{Sand fraction in the top 40 cm of soil (0 - 1).}

\item{SOC}{Initial soil organic carbon stocks to 40 cm depth (tSOC/ha).}

\item{Gdays}{Number of days in growing season (days). Default is 240.}

\item{years}{Number of years over which to run the simulation.}
}
\description{
A wrapper function around all SNAP model equations to calculate delta SOC over a given number of years based on the below input parameters.
}
