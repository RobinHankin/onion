\name{Conj.onion}
\alias{Conj.onion}
\title{Onionic conjugation}
\description{
Returns the conjugate of an onionic vector
}
\usage{
\method{Conj}{onion}(z)
}
\arguments{
  \item{z}{An onionic vector}
}
\details{
  The method is common to all onions: \code{Im(x) <- -Im(x)}
  }
\author{Robin K. S. Hankin}
\examples{
a <- roct(5)
Conj(a)
}
\keyword{array}
