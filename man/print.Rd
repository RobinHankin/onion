\name{print.onion}
\alias{print.quaternion}
\alias{print.octonion}
\alias{print.onion}
\title{Print method for onions}
\description{
  Prints an onion
}
\usage{
\method{print}{quaternion}(x, h=getOption("horiz"), ...)
\method{print}{octonion}(x, h=getOption("horiz"), ...)
}
\arguments{
  \item{x}{Onionic vector}
  \item{h}{Boolean, with default \code{FALSE} meaning to print
    horizontally and \code{TRUE} meaning to print by columns.}
  \item{...}{Further arguments (currently ignored)}
}
\details{
  If \code{options("horiz")} is \code{TRUE}, then print by rows rather
  than columns (provided that the default value of argument \code{h} is
  not overridden).  The default behaviour is to print by columns; do
  this by setting \code{horiz} to anything other than \code{TRUE}, including
  leaving it unset.
  }
  
\author{Robin K. S. Hankin}
\examples{
roct(4)
}
\keyword{array}
