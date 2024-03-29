% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TBVx-initialization-fncs-v20.R
\name{fractions.by.stage.at.start}
\alias{fractions.by.stage.at.start}
\title{Sets the initial fractions of the population (by state and age group)}
\usage{
fractions.by.stage.at.start(p, df = NULL)
}
\arguments{
\item{p}{params environment}

\item{df}{data frame from XML seeded infections for TB}
}
\value{
a matrix of fractions by state and age group
}
\description{
Sets the fractions of people in each state and age group using the XML fraction.at.birth attribute of 
the VXa, SES, RISK and HIV dimensions and the TB <seeded.infections> XML element.
This distribution is used to initialize the population with a prespecified distribution over the various states by age
}
