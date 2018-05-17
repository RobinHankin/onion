\name{seq.onion}
\alias{seq.onion}
\alias{slerp}
\alias{SLERP}
\title{seq method for onions}
\description{
  Rough equivalent of \code{seq()} for onions. 
}
\usage{
\method{seq}{onion}(from = 1, to = 1, by = ((to - 
    from)/(length.out - 1)), length.out = NULL, slerp = FALSE, ...)
}
\arguments{
  \item{from}{Onion for start of sequence}
  \item{to}{Onion for end of sequence}
  \item{by}{Onion for interval}
  \item{length.out}{Length of vector returned}
  \item{slerp}{Boolean, with default \code{FALSE} meaning to use linear
    interpolation and \code{TRUE} meaning to use spherical linear
    interpolation (useful for animating 3D rotation)}
  \item{...}{Further arguments (currently ignored)}
}
\author{Robin K. S. Hankin}
\examples{
seq(from=O1,to=Oil,length.out=6)
seq(from=H1,to=(Hi+Hj)/2,len=10,slerp=TRUE)
}
\keyword{array}
