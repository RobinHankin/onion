\name{roct}
\alias{rquat}
\alias{roct}
\title{Random onionic vector}
\description{
  Returns a random onionic vector of arbitrary length
}
\usage{
roct(n, x=1:8, replace=TRUE, rand="sample", ...)
rquat(n, x=1:4, replace=TRUE, rand="sample", ...)
}
\arguments{
  \item{n}{Length of onionic vector returned}
  \item{x}{Argument \code{x} as passed to \code{sample()};
    only matters if \code{rand} takes its default value of
    \dQuote{\code{sample}}.}
  \item{replace}{Argument \code{replace} as passed to \code{sample()};
    only matters if \code{rand} takes its default value of
    \dQuote{\code{sample}}.}
  \item{rand}{String, with name being that of the distribution
    intended.  Currently implemented values are \dQuote{sample}
    (default), \dQuote{norm}, \dQuote{unif}, \dQuote{binom} and
    \dQuote{pois}.  Add an \dQuote{r} to get the name of the function
    used; thus \dQuote{unif} means to call \code{runif()}.}
  \item{...}{Further arguments passed to the random number generator
    (such as \code{mean} and \code{sd}, which would be passed to
    \code{rnorm()})}
}
\details{
  Function \code{rquat()} returns a quaternionic vector and function
  \code{roct()} returns an octonionic vector.
}

\note{Arguments \code{x} and \code{replace} are there (and have the
  default values they do) in order to make \code{roct(n)} return a
  \dQuote{get you going} random onion that prints relatively compactly.}
\author{Robin K. S. Hankin}
\examples{
roct(3)
plot(roct(30))
}
\keyword{array}
