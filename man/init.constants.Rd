% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TBVx-initialization-fncs-v20.R
\name{init.constants}
\alias{init.constants}
\title{Initializes constant parameters from XML}
\usage{
init.constants(p, xml)
}
\arguments{
\item{row}{a  named vector with names  'parameter', 'times', 'values' that will be parsed}

\item{df}{an approxfun f(t)}
}
\value{
an environment of model parameters to be passed to the next stage in parameter initialization
}
\description{
Called from read.model.parameters()
For example: 
<multiplier name="etamul" times="1960:2020" values="1/(1+exp(-sh*(x-midx)))"/>
}
