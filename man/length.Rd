\name{length.onion}
\alias{length.onion}
\alias{length.onion<-}
\alias{length}
\alias{length<-}
\alias{length<-.onion}
\title{Length of an octonionic vector}
\description{
  Get or set the length of octonionic vectors.
}
\usage{
\method{length}{onion}(x)
\method{length}{onion}(x) <-  value
}
\arguments{
  \item{x}{An onionic vector}
  \item{value}{An integer}
}
\details{
  Operates on the columns of the matrix as expected.

  In \code{length(x) <- value}, if \code{value>length(x)}, the onion is
  padded with \code{NA}s.
  
}
\author{Robin K. S. Hankin}
\examples{
a <- roct(5)
length(a)
length(a) <- 3
a
length(a) <- 10
a

}
\keyword{array}
