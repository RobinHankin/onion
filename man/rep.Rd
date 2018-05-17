\name{rep.onion}
\alias{rep.onion}
\title{Replicate elements of onionic vectors}
\description{
  Replicate elements of onionic vectors
}
\usage{
\method{rep}{onion}(x, ...)
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
