\name{Compare-methods}
\docType{methods}
\alias{Compare-methods}
\alias{Compare,onion,onion-method}
\alias{Compare,onion,numeric-method}
\alias{Compare,numeric,onion-method}
\alias{Compare,onionmat,onionmat-method}
\alias{Compare,onionmat,ANY-method}
\alias{Compare,ANY,onionmat-method}
\alias{onion_compare}
\alias{onionmat_equal_onionmat}
\alias{onionmat_equal_single}
\alias{onionmat_compare_onionmat}
\alias{onionmat_compare_single}
\alias{single_compare_onionmat}
\title{Methods for compare S4 group}
\description{Methods for comparison (equal to, greater than, etc) of
  onions.  Only equality makes sense.}
\value{Return a boolean}
\examples{
# roct() > 0 # meaningless and returns an error



x <- as.octonion(matrix(sample(0:1,800,TRUE,p=c(9,1)),nrow=8))
y <- as.octonion(matrix(sample(0:1,800,TRUE,p=c(9,1)),nrow=8))
x==y

matrix(as.quaternion(100+1:12),3,4) == 102



  }
\keyword{methods}
\keyword{math}
