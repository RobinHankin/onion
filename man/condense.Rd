\name{condense.onion}
\alias{condense.onion}
\alias{condense}
\title{Condense an onionic vector into a short form}
\description{
 Condense an onionic vector into a string vector showing whether the
 elements are positive, zero or negative. 
}
\usage{
\method{condense}{onion}(x)
}
\arguments{
  \item{x}{An onionic vector}
}
\value{
  Returns a string vector of the same length as \code{x} whose elements
  are length 4 or 8 strings for quaternions or octonions respectively.  
  The characters are \dQuote{\code{+}} for a positive, \dQuote{\code{-}}
  for a negative, and \dQuote{\code{0}} for a zero, element.
}
\author{Robin K. S. Hankin}
\examples{
condense(roct(3))
}
\keyword{array}
