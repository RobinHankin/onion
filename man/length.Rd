\name{length.onion}
\alias{length.onion}
\alias{length}
\title{Length of an octonionic vector}
\description{
  Get the length of octonionic vectors.
}
\usage{
\method{length}{onion}(x)
}
\arguments{
  \item{x}{An onionic vector}
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
