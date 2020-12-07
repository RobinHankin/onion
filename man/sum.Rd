\name{sum.onion}
\alias{sum.onion}
\alias{sum.quaternion}
\alias{sum.octonion}
\alias{onion_allsum}
\alias{onion_allprod}
\alias{onion_cumsum}
\title{Sum of onionic vectors}
\description{
The sum or product of a vector
}
\usage{
\method{sum}{onion}(..., na.rm = FALSE)
onion_allsum(x)
onion_allprod(x)
onion_cumsum(x)
onion_cumprod(x)
}
\arguments{
  \item{x,...}{Onionic vectors}
  \item{na.rm}{Boolean with default \code{FALSE} meaning to sum with
    \code{NA} values in place and \code{TRUE} meaning to ignore them.}
    
}
\author{Robin K. S. Hankin}
\examples{
sum(roct(5))
sum(Oil,Oj,O1)
sum(roct(5),100*Ok)
}
\keyword{array}
