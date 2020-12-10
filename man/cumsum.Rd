\name{cumsum}
\alias{cumsum}
\alias{onion_cumsum}
\alias{onion_cumprod}
\title{Cumulative sums and products of onions}
\description{
Cumulative sums and products of onions
}
\usage{
onion_cumsum(x)
onion_cumprod(x)
}
\arguments{\item{x}{onion}}
\value{An onion}
\author{Robin K. S. Hankin}
\note{
The octonions are nonassociative but \code{cumprod()} operates
left-associatively, as in \code{((a[1]*a[2])*a[3])*a[4]} etc. 
}
\examples{

cumsum(as.quaternion(matrix(runif(20),4,5)))
cumsum(roct(5))

cumprod(rquat(7))
}
\keyword{array}
