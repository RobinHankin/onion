\name{c.onion}
\alias{c.onion}
\title{Combine onionic vectors into a single vector}
\description{
Combines its arguments to form an onionic vector.
}
\usage{
\method{c}{onion}(...)
}
\arguments{
  \item{\dots}{onionic vectors}
}
\details{
  Returns an onionic vector of type \code{biggest()} of the arguments.

  Names are inherited from the behaviour of  \code{cbind()}, not
  \code{c()}. 
  
  }
\author{Robin K. S. Hankin}
\examples{
a <- roct(3)
b <- seq(from=Oil,to=Oj,len=6)
c(a,b)

c(rquat(3),H1,H0,Him)

c(Hi,Oil,1:2)
}
\keyword{array}
