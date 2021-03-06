% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prop.unequal.R
\name{prop.unequal}
\alias{prop.unequal}
\title{Proportion of unequal elements}
\usage{
prop.unequal(x, y, na.rm = FALSE)
}
\arguments{
\item{x}{First vector to be compared}

\item{y}{Second vector to be compared}

\item{na.rm}{a logical value indicating positions should be excluded if they
are \code{NA} in at least one of the vectors.}
}
\value{
numeric value that is proportion of corresponding elements that are
not equal.
}
\description{
Compares the first element in vector \code{x} to the first element in vector
\code{y}, the second element in \code{x} to the second element in \code{y},
and so forth, and returns the proportion of elements which are not equal.
}
\examples{
prop.unequal(c(1,2,3), c(1,3,3))
prop.unequal(c(1,2,3), c(1,3,NA))
prop.unequal(c(1,2,3), c(1,3,NA), na.rm=TRUE)
}
