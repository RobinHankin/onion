\name{length}
\alias{length}
\alias{length.onion}
\alias{length<-}
\alias{length<-.onion}
\alias{length,onion-method}
\alias{length<-,onion-method}
\title{Length of an octonionic vector}
\description{Get or set the length of onions}
\usage{\S4method{length}{onion}(x)}
\arguments{\item{x}{An onion}}
\value{integer}
\details{Operates on the columns of the matrix as expected.}
\author{Robin K. S. Hankin}
\examples{
a <- roct(5)
length(a)

}
\keyword{array}
