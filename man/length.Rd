\name{length}
\alias{length}
\alias{length.onion}
\alias{length<-}
\alias{length<-.onion}
\alias{length,onion-method}
\alias{length<-,onion-method}
\title{Length of an octonionic vector}
\description{
  Get the length of octonionic vectors.
}
\usage{
\S4method{length}{onion}(x)
\S4method{length<-}{onion}(x,value)
}
\arguments{
  \item{x}{An onionic vector}
  \item{value}{length of vector}
}
\details{
  Operates on the columns of the matrix as expected.
}
\author{Robin K. S. Hankin}
\examples{
a <- roct(5)
length(a)

}
\keyword{array}
