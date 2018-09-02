\name{roct}
\alias{rquat}
\alias{roct}
\title{Random onionic vector}
\description{
  Random quaternion or octonion vectors
}
\usage{
rquat(n, sphere=TRUE)
roct(n)
}
\arguments{
  \item{n}{Length of random vector returned}
  \item{sphere}{Boolean with default \code{TRUE} meaning to return
    unit quaternions that correspond to uniform distribution over \eqn{SO_3}{SO3}}
}
\details{
  Function \code{rquat()} returns a quaternionic vector and function
  \code{roct()} returns an octonionic vector.
}

\note{
  These functions are give a quick  \dQuote{get you going} random onion
  to play with.
\author{Robin K. S. Hankin}
\examples{
rquat(3)
roct(3)
plot(roct(30))
}
