\name{show}
\alias{show}
\alias{print}
\alias{print.quaternion}
\alias{print.octonion}
\alias{print.onion}
\alias{print.onionmat}
\alias{print,onion-method}
\alias{onion_show}
\alias{show,onion-method}
\alias{onionmat_show}
\title{Print method for onions}
\description{Show methods for onions}
\usage{
\S4method{show}{onion}(object)
onion_show(x,h=getOption("show_onions_horizontally"))
}
\arguments{
  \item{x,object}{Onions}
  \item{h}{Boolean, with default \code{FALSE} meaning to print
    horizontally and \code{TRUE} meaning to print by columns.}
}
\details{
  If \code{options("horiz")} is \code{TRUE}, then print by rows rather
  than columns (provided that the default value of argument \code{h} is
  not overridden).  The default behaviour is to print by columns; do
  this by setting \code{horiz} to anything other than \code{TRUE}, including
  leaving it unset.
  }
  
\author{Robin K. S. Hankin}
\note{Print method for \code{onionmat} objects is also sensitive to
  this option.}
\examples{
roct(4)
}
\keyword{array}
