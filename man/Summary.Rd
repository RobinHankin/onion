\name{sum}
\alias{sum}
\alias{min}
\alias{max}
\alias{prod}
\alias{range}
\alias{Summary,onion-method}
\alias{sum,onion-method}
\alias{sum,quaternion-method}
\alias{sum,octonion-method}
\alias{sum,onionmat-method}
\alias{prod,quaternion-method}
\alias{prod,octonion-method}
\alias{str_onion}
\alias{str,onion-method}
\alias{onion_allsum}
\alias{onionmat_allsum}
\alias{quaternion_allprod}
\title{Various summary statistics for onions}
\description{Various summary statistics for onions}
\usage{
onion_allsum(x)
\S4method{sum}{onion}(x)
\S4method{prod}{quaternion}(x)
\S4method{sum}{octonion}(x)
\S4method{sum}{onionmat}(x)
\S4method{prod}{octonion}(x)
\S4method{str}{onion}(object, ...)
str_onion(object, vec.len = 4, ...)
onion_allsum(x)
onionmat_allsum(x)
quaternion_allprod(x)
}

\arguments{
  \item{x,object,\dots}{Objects of class \code{onion}}
  \item{vec.len}{number of elements to display}
}
\details{For a \code{onion} object, return the sum or product accordingly}
\value{Return an onion}
\author{Robin K. S. Hankin}
\note{Function \code{str()} uses functionality from \code{condense()}.}
\examples{

sum(roct())
str(roct())
}
\keyword{math}
