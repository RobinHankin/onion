\name{t.onion}
\alias{t.onion}
\title{Onion transpose}
\description{
  Takes the transpose of an onionic vector
}
\usage{
\method{t}{onion}(x)
}
\arguments{
  \item{x}{Onionic vector}
}
\details{
  Returns the transpose of the eight-row or four-row matrix
}
\author{Robin K. S. Hankin}
\examples{
a <- roct(5)
t(a)
}
\keyword{array}
