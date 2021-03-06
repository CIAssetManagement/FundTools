% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fixedincome.R
\name{VaR_BondPriceChange}
\alias{VaR_BondPriceChange}
\title{Generates a Price Path for a Bond that is used to calculate VaR.}
\usage{
VaR_BondPriceChange(id, dates)
}
\arguments{
\item{id}{is the id for which the path is constructed.}

\item{dates}{is a vector of dates for the path.}
}
\value{
the price of the Bond at the calculation date
}
\description{
Calculates the price of a Bond with the maturity date, calculation date, coupon rate and YTM of the Bond
}
