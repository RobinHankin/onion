\name{sum}
\alias{sum}
\alias{min}
\alias{max}
\alias{prod}
\alias{range}
\alias{Summary,onion-method}
\alias{sum,onion-method}
\alias{prod,quaternion-method}
\alias{prod,octonion-method}
\alias{sum,octonion-method}
\alias{str_onion}
\alias{str,onion-method}
\alias{onion_allsum}
\alias{quaternion_allprod}
\title{Various summary statistics for onions}
\description{Various summary statistics for onions}
\usage{
onion_allsum(x)
\S4method{sum}{onion}(x)
\S4method{sum}{octonion}(x)
\S4method{prod}{quaternion}(x)
\S4method{prod}{octonion}(x)
\S4method{str}{onion}(object, ...)
str_onion(object, vec.len = 4, ...)
onion_allsum(x)
quaternion_allprod(x)
}

\arguments{
  \item{x,object,\dots}{Objects of class \code{onion}}
  \item{vec.len}{number of elements to display}
}
\details{
  For a \code{onion} object
}
\author{Robin K. S. Hankin}
\note{
no notes here
}
\examples{
3+5
}
\keyword{math}
