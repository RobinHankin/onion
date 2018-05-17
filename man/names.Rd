\name{names.onion}
\alias{names.onion}
\alias{names.onion<-}
\alias{names<-.onion}
\title{Names of an onionic vector}
\description{
 Functions to get or set the names of an onionic vector.
}
\usage{
\method{names}{onion}(x)
\method{names}{onion}(x) <- value
}
\arguments{
  \item{x}{Onionic vector}
  \item{value}{a character vector of  the same length as
    \sQuote{\code{x}}, or \code{NULL}}
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
