\name{onion-class}
\docType{class}
\alias{onion-class}
\alias{quaternion-class}
\alias{octonion-class}
\alias{onionmat-class}
\alias{index-class}
\title{Class \dQuote{onion}}
\description{The formal \proglang{S4} class for onion and onionmat objects}
\section{Objects from the Class}{

  Class \dfn{onion} is a virtual \proglang{S4} class extending classes
  \dfn{quaternion} and \dfn{octonion}.  In package documentation,
  \dQuote{\dfn{onion}} means an \R object that behaves as a vector of
  quaternions or octonions, stored as a four- or eight- row numeric
  matrix.

  Class \dfn{onionmat} is the \proglang{S4} class for matrices whose
  elements are quaternions or octonions.  An onionmat is stored as a
  two-element list, the first being an onion and the second an integer
  matrix which holds structural matrix attributes such as dimensions and
  dimnames.  Most standard arithmetic \R idiom for matrices should work
  for onionmats.

  Class \dfn{index} is taken from the excellent \pkg{Matrix} package and
  is a \code{setClassUnion()} of classes \code{numeric}, \code{logical},
  and \code{character}, which mean that it is an arity-one matrix index.


}
\author{Robin K. S. Hankin}
\examples{

as.octonion(1:8,single=TRUE)
as.quaternion(matrix(runif(20),nrow=4))

H <- matrix(rquat(21),3,7)
dimnames(H) <- list(foo=letters[1:3],bar=state.abb[1:7])

i(H) <- 0.1

I <- matrix(rquat(14),7,2)
dimnames(I) <- list(foo=state.abb[1:7],baz=LETTERS[1:2])
H \%*\% I

}
\keyword{classes}
