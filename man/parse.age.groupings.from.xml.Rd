% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TBVx-parsing-xml-fncs-v13.R
\name{parse.age.groupings.from.xml}
\alias{parse.age.groupings.from.xml}
\title{parses an XML node list of <age.group name="A0"/> into a named integer vector of age.groups}
\usage{
parse.age.groupings.from.xml(xmldoc, xmlpath)
}
\arguments{
\item{xmldoc}{the XML document}

\item{xmlpath}{the path to the XML element with <age.groups><age.group/>s}
}
\value{
a named ('A0,'A15',..) integer vector (0,15,..)of age groups
}
\description{
parses an XML node list of <age.group name="A0"/> into a named integer vector of age.groups
}
