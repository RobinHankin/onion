\name{str}
\alias{str}
\alias{str.onion}
\title{Compactly display an onion}
\description{
A very basic implementation of \code{str()} for onions
}
\usage{
\S4method{str}{onion}(object, vec.len=4, ...)
}
\arguments{
  \item{object}{Onionic vector}
  \item{vec.len}{Number of elements to display}
  \item{\dots}{Futher arguments (currently ignored)}
}
\details{
  Displays the first \code{vec.len} elements of an onionic vector using
  \code{condense}.
}
\value{
Returns \code{NULL}.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{condense}}}
\examples{
str(roct(100))
}
\keyword{array}
