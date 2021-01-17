\name{names}
\alias{names.onion}
\alias{names.onion<-}
\alias{names<-}
\alias{names<-.onion}
\alias{names,onion-method}
\alias{names<-,onion-method}
\alias{names,onionmat-method}
\alias{names<-,onionmat-method}
\alias{rownames}
\alias{rownames,onionmat-method}
\alias{rownames<-,onionmat-method}
\alias{colnames}
\alias{colnames,onion-method}
\alias{colnames,onionmat-method}
\alias{colnames<-,onionmat-method}
\alias{dim}
\alias{dim<-}
\alias{dim,onionmat-method}
\alias{dim<-,onionmat-method}
\alias{dimnames,onionmat-method}
\alias{dimnames<-,onionmat-method}
\alias{nrow,onionmat-method}
\alias{ncol,onionmat-method}
\title{Names of an onionic vector}
\description{Functions to get or set the names of an onion}
\usage{
\S4method{names}{onion}(x)
\S4method{rownames}{onionmat}(x)
\S4method{colnames}{onionmat}(x)
\S4method{dimnames}{onionmat}(x)
\S4method{dim}{onionmat}(x)
}
\arguments{\item{x}{onion}}
\details{
  Names attributes refers to colnames of the internal matrix, which
  are retrieved or set using \code{colnames()} or \code{colnames<-()}.
}
\author{Robin K. S. Hankin}
\examples{
a <- roct(5)
names(a) <- letters[1:5]

b <- romat()
dimnames(b) <- list(month = month.abb[1:5], location=names(islands)[1:6])

}
\keyword{array}
