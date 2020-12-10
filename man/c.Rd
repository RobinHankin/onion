\name{c}
\alias{c}
\alias{c.onion}
\alias{concatenate.onion}
\alias{c_onionpair}
\alias{c,onion-method}
\title{Concatenation}
\description{
Combines its arguments to form a single onion.
}
\usage{
c_onionpair(x,y)
\S4method{c}{onion}(x,...)
}
\arguments{\item{x,y,\dots}{onions}}
\details{
  Returns an onion of the same type as its arguments.  Names are
  inherited from the behaviour of \code{cbind()}, not \code{c()}.
}
\value{An onion}
\author{Robin K. S. Hankin}
\note{
  
  The method is not perfect; it will not, for example, coerce its
  arguments to the \code{biggest()} type, so \code{c(rquat(),roct())}
  will fail.  You will have to coerce the arguments by hand.

  Dispatch is based on the class of the first argument, so
  \code{c(1,rquat())} will return a list (not an onion), and
  \code{c(rquat(),1)} will fail.
}

\examples{
a <- roct(3)
b <- seq_onion(from=Oil,to=Oj,len=6)
c(a,b)

c(rquat(3),H1,H0,Him)
}
\keyword{array}
