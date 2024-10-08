% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_deltaSOC.R
\name{calc_deltaSOC}
\alias{calc_deltaSOC}
\title{Delta SOC}
\usage{
calc_deltaSOC(PDSOC, DDSOC, wetdays, SAND, SOC)
}
\arguments{
\item{PDSOC}{Plant-derived soil organic carbon inputs (g/m2).}

\item{DDSOC}{Dung-derived soil organic carbon inputs (g/m2).}

\item{wetdays}{Number of days wet enough to support soil microbial respiration (days).}

\item{SAND}{Sand fraction in the top 40 cm of soil (0 - 1).}

\item{SOC}{Initial soil organic carbon stocks to 40 cm depth (tSOC/ha).}
}
\description{
Calculation of the change in SOC stock (tSOC/ha) for year t using plant-derived soil organic carbon (PDSOCt), dung-derived soil organic carbon (DDSOC), and microbial respiration.
}
