\name{sum.onion}
\alias{sum.onion}
\alias{sum.quaternion}
\alias{sum.octonion}
\title{Sum of onionic vectors}
\description{
Returns the onionic sum of all values present in its arguments.
}
\usage{
\method{sum}{onion}(..., na.rm = FALSE)
}
\arguments{
  \item{\dots}{Onionic vectors}
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
