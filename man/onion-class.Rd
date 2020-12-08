\name{onion-class}
\docType{class}
\alias{onion-class}
\alias{quaternion-class}
\alias{octonion-class}
\alias{onionmat-class}
\title{Class \dQuote{onion}}
\description{
  The formal S4 class for onions.}
\section{Objects from the Class}{
Objects \emph{can} be created by calls of the form \code{new("onion",
  ...)} but this is not encouraged.  Use functions \code{octonion()} or
\code{quaternion} and, especially, \code{as.onion()} instead.
}
\section{Slots}{
  Classes \code{quaternion} and \code{octonion} have a single slot
  \describe{
    \item{x}{Numeric matrix of four or eight rows for
      quaternions or octonions respectively.  Column names may be used.}
  }
}
\author{Robin K. S. Hankin}
\examples{

M <- matrix(runif(24),8)
colnames(M) <- letters[1:3]
new("octonion",x=M)          # in practice, use octonion() or as.onion()

}
\keyword{classes}
