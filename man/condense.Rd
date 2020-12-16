\name{condense}
\alias{condense}
\alias{condense.onion}
\title{Condense an onionic vector into a short form}
\description{
 Condense an onion into a string vector showing whether the
 elements are positive, zero or negative. 
}
\usage{
condense(x,as.vector=FALSE)
}
\arguments{
  \item{x}{An onionic vector}
  \item{as.vector}{Boolean, indicating whether to return a vector or
    matrix}
}
\value{
  If \code{as.vector} is \code{TRUE}, return a string vector of the same
  length as \code{x} whose elements are length 4 or 8 strings for
  quaternions or octonions respectively.  If \code{FALSE}, return a
  matrix with these columns.

  The characters are \dQuote{\code{+}} for a positive, \dQuote{\code{-}}
  for a negative, and \dQuote{\code{0}} for a zero, element.

}
\author{Robin K. S. Hankin}
\examples{
condense(roct(3))
condense(roct(3),as.vector=TRUE)
}
\keyword{array}
