% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TBVx-derivs-v27.R
\name{run.deSolve}
\alias{run.deSolve}
\title{called from run.model()}
\usage{
run.deSolve(params = NULL)
}
\arguments{
\item{params}{an environment with initialized model parameters}
}
\value{
a \code{list[3]} of \code{double[] times}, \code{list matrix[nVXaSESRISKHIVTB,nAGES] state}, \code{list vector[nAGES] dHIVTBx}
}
\description{
within run.deSolve the deSolve function rk() is called with derivs.deSolve() as the argument that provides a reference to the derivs function that produces the derivatives at a specific time point.
}
