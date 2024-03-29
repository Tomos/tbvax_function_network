% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TBVx-output-query-fncs-v5.R
\name{exclude.not.alive}
\alias{exclude.not.alive}
\title{helper function called from calc.targets to exclude non alive (i.e. states ending in 'dead' or 'count') from stocks}
\usage{
exclude.not.alive(z)
}
\arguments{
\item{z}{a data frame (either numerator or denominator targets)}
}
\value{
the modified data frame with non alive stocks removed from the columns VXa, SES, RISK, HIV, TB
}
\description{
helper function called from calc.targets to exclude non alive (i.e. states ending in 'dead' or 'count') from stocks
}
