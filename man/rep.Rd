\name{rep}
\alias{rep}
\alias{rep.onion}
\alias{rep,onion-method}
\title{Replicate elements of onionic vectors}
\description{
  Replicate elements of onionic vectors
}
\usage{
\S4method{rep}{onion}(x, ...)
}
\arguments{
  \item{x}{Onionic vector}
  \item{...}{Further arguments passed to \code{seq.default()}}
}
\author{Robin K. S. Hankin}
\examples{
a <- roct(3)
rep(a,2) + a[1]
rep(a,each=2)
rep(a,length.out=5)
}
\keyword{array}
