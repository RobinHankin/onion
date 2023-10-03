\name{drop}
\alias{drop}
\alias{drop.onion}
\alias{drop,onion-method}
\alias{drop,onionmat-method}
\title{Drop zero imaginary parts of an onionic vector}
\description{If an onion has zero imaginary part, drop it}
\usage{
\S4method{drop}{onion}(x)
}
\arguments{\item{x}{onion}}
\details{

  Generally, \dQuote{drop} means coercion of an object to a less general
  type without loss of information.  In many contexts, function
  \code{drop()} means to lose redundant information.  This is not done
  by default (doing so would result in unexpected coercions).

  Methods are given for \code{onion} and \code{onionmat} objects.

}
\author{Robin K. S. Hankin}
\examples{
a <- rsoct()
a
a-Im(a)
drop(a-Im(a))
}
\keyword{array}
