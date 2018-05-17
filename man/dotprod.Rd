\name{dotprod}
\alias{dotprod}
\alias{\%.\%}
\title{Euclidean even product}
\description{
  Euclidean even product; dot product of two onions
}
\usage{
dotprod(x, y)
}

\arguments{
  \item{x,y}{Onionic vectors}
}
\details{
  Returns the Euclidean even product of two onionic vectors.  That is,
  if \code{x} and \code{y} are eight-element vectors of the components
  of two onions, return \code{sum(x*y)}.

  Note that the returned value is a numeric vector (compare \code{\%<.>\%},
  \code{e.even()}, which return onionic vectors with zero imaginary part).
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{prods}}}
\examples{
a <- roct(5)
b <- roct(1)

a \%.\% b
a \%.\% (a+3)

}
\keyword{array}
