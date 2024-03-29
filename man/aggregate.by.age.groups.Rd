% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TBVx-age-related-fncs-v14.R
\name{aggregate.by.age.groups}
\alias{aggregate.by.age.groups}
\title{Aggregates columns with single year age groups into age groups defined by the lower age limits
with options to}
\usage{
\method{aggregate}{by.age.groups}(Y, lower.age.limits, sumcols = NA, avg = NA, weights = NA)
}
\arguments{
\item{Y}{a matrix with single year age groups 0..99 in columns}

\item{lower.age.limits}{new age group lower limits}

\item{sumcols}{logical to indicate if the summed columns will be returned}

\item{avg}{logical to indicate if the average of the rows in an age group will be returned}

\item{weights}{an optional matrix with weights (of the same dimensions as Y)}
}
\value{
the result of the aggregation of columns of matrix Y into the appropriate age groups, optionally summed into a vector and optionally averaged and/or weighted
}
\description{
Aggregates columns with single year age groups into age groups defined by the lower age limits
with options to
}
