\name{threeform}
\alias{threeform}
\alias{associator}
\alias{commutator}
\title{Various non-field diagnostics}
\description{
  Diagnostics of non-field behaviour: threeform, associator, commutator
}
\usage{
threeform(x1, x2, x3)
associator(x1, x2, x3)
commutator(x1, x2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1,x2,x3}{onionic vectors}
}
\details{
  The threeform is defined as \code{Re(x1 * (Conj(x2) * x3) - x3
    * (Conj(x2) * x1))/2};

  the associator is \code{(x1 * x2) * x3 - x1 * (x2 * x3)};

  the commutator is \code{x1 * x2 - x2 * x1}.
}
\value{
Returns an octonionic vector.
}
\author{Robin K. S. Hankin}
\examples{
x <- roct(7) ; y <- roct(7) ; z <- roct(7)
associator(x,y,z)
}
\keyword{array}
