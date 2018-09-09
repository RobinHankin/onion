\name{onion-package}
\alias{onion-package}
\docType{package}
\title{
\packageTitle{onion}
}
\description{
\packageDescription{onion}
}
\details{

\packageDESCRIPTION{onion}
\packageIndices{onion}

There are precisely four normed division algebras over the reals: the
reals themselves, the complex numbers, the quaternions, and the
octonions.  The \R system is well equipped to deal with the first two:
the \pkg{onion} package provides some functionality for the third and
fourth.  
}
\author{
\packageAuthor{onion}

Maintainer: \packageMaintainer{onion}
}
\references{
R. K. S. Hankin 2006. \dQuote{Normed division algebras in R:
  introducing the onion package}.  \emph{R News}, Volume 6, number 2
}
\keyword{ package }
\examples{
rquat(10)   # random quaternions

Ok + (Oi + Ojl)/(Oj-Oil)  # basic octonions

x <- roct(10)
y <- roct(10)
z <- roct(10)

x*(y*z) - (x*y)*z   # nonassociative!


}
