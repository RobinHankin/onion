\name{round}
\alias{round}
\alias{round,onion-method}
\alias{round,onionmat-method}
\title{Rounding of onions}
\description{Round elements of an onion}
\usage{
   \S4method{round}{onion}(x,digits=0)
   \S4method{round}{onionmat}(x,digits=0)
}
\arguments{
  \item{x}{Object of class \code{onion}}
  \item{digits}{number of digits to round to}
}
\details{

  For \code{onion}s, coerce to a matrix, round, then coerce back to an
  \code{onion}.  For \code{onionmat}s, coerce to an \code{onion}, round,
  then coerce back to an \code{onionmat}.
}
\value{Return an onion}
\author{Robin K. S. Hankin}
\examples{

round(rquat()*100)
round(rquat()*100,3)
}
\keyword{math}
