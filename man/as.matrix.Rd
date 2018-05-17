\name{as.matrix.onion}
\alias{as.matrix.onion}
\title{Coerces an onionic vector into a matrix}
\description{
Coerces an onionic vector into a matrix.
}
\usage{
\method{as.matrix}{onion}(x, ...)
}
\arguments{
  \item{x}{Onionic vector}
  \item{...}{Further arguments (currently ignored)}
}
\details{
  The matrix returned has 4 rows for a quaternion, 8 for an octonion
}
\author{Robin K. S. Hankin}
\examples{
det(as.matrix(roct(8)))
}
\keyword{array}
