% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TBVx-data-reading-fncs-v13.R
\name{read.incidence}
\alias{read.incidence}
\title{Reads a tab delimited incidence file}
\usage{
read.incidence(parms, fname = NA, countrycode = NA, proportions = F)
}
\arguments{
\item{parms}{an environemnt with initialized parameters}

\item{fname}{the name of the .txt file}

\item{countrycode}{3 letter ISO country code}

\item{proportions}{a logical to indicate if the numbers in the file are proportions or rates (the default)}
}
\value{
a list with dim, from, to, VXa, RISK, SES, HIV, TB, and inci ; see the vignette for more details on incidence files
}
\description{
Reads a tab delimited incidence file
}
