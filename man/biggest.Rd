\name{biggest}
\alias{biggest}
\title{Returns the biggest type of a set of onions}
\description{
Returns the biggest type of a set of onions; useful for
\dQuote{promoting} a set of onions to the most general type.
}
\usage{
biggest(...)
}
\arguments{
  \item{\dots}{Onionic vectors}
}
\details{
  If any argument passed to \code{biggest()} is an octonion, then return
  the string \dQuote{octonion}.  Failing that, if any argument is a quaternion,
  return the string \dQuote{quaternion},  and failing that, return
  \dQuote{scalar}.
}
\author{Robin K. S. Hankin}
\examples{
biggest(O1,rquat(100),1:4)
}
\keyword{math}
