\name{names}
\alias{names.onion}
\alias{names.onion<-}
\alias{names<-}
\alias{names<-.onion}
\alias{names,onion-method}
\alias{names<-,onion-method}
\title{Names of an onionic vector}
\description{Functions to get or set the names of an onion}
\usage{
\S4method{names}{onion}(x)
\S4method{names<-}{onion}(x)
}
\arguments{
  \item{x}{onion}
}
\details{
  Names attributes refers to colnames of the internal matrix, which
  are retrieved or set using \code{colnames()} or \code{colnames<-()}.
}
\author{Robin K. S. Hankin}
\examples{
a <- roct(5)
names(a) <- letters[1:5]
}
\keyword{array}
