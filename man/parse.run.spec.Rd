% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TBVx-parsing-xml-fncs-v13.R
\name{parse.run.spec}
\alias{parse.run.spec}
\title{reads the XML file and parses parameters unrelated to the model itself (i.e. dimensions, progression etc etc)}
\usage{
parse.run.spec(xmlfile = NA)
}
\arguments{
\item{xmlfile}{the XML file}
}
\value{
an environment the will be part of the initialized parameters environment as run.params
}
\description{
reads the XML file and parses parameters unrelated to the model itself (i.e. dimensions, progression etc etc)
}
