\name{roct}
\alias{rquat}
\alias{roct}
\title{Random onionic vector}
\description{
  Random quaternion or octonion vectors
}
\usage{
rquat(n=5)
roct(n=5)
}
\arguments{
  \item{n}{Length of random vector returned}
}
\details{
  Function \code{rquat()} returns a quaternionic vector and function
  \code{roct()} returns an octonionic vector.
}

\note{
  These functions give a quick  \dQuote{get you going} random onion
  to play with.
}
\author{Robin K. S. Hankin}
\references{
  K. Shoemake 1992. \dQuote{Uniform random rotations}. 
  In D. Kirk, editor, \emph{Graphics Gems III} pages 129-130. Academic,
  New York.
}
\examples{
rquat(3)
roct(3)
plot(roct(30))
}
