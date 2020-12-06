\name{c.onion}
\alias{c.onion}
\alias{concatenate.onion}
\alias{c_onionpair}
\alias{c,onion-method}
\title{Combine onionic vectors into a single vector}
\description{
Combines its arguments to form an onionic vector.
}
\usage{
c_onionpair(x,y)
\S4method{c}{onion}(x,...)
}
\arguments{
  \item{x,y,\dots}{onions}
}
\details{
  Returns an onionic vector of type \code{biggest()} of the arguments.

  Names are inherited from the behaviour of  \code{cbind()}, not
  \code{c()}. 
  
  }
\author{Robin K. S. Hankin}
\examples{
a <- roct(3)
b <- seq_onion(from=Oil,to=Oj,len=6)
c(a,b)

c(rquat(3),H1,H0,Him)
}
\keyword{array}
