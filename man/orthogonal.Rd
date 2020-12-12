\name{orthogonal}
\alias{orthogonal}
\alias{as.orthogonal}
\alias{is_orthogonal}
\alias{matrix2quaternion}

\title{Orthogonal matrix equivalents}
\description{
Convert a quaternion to and from an equivalent orthogonal matrix
}
\usage{
matrix2quaternion(M)
as.orthogonal(Q)
}
\arguments{
  \item{M}{A three-by-three orthogonal matrix}
  \item{Q}{A vector of quaternions}
}
\value{
  Function \code{matrix2quaternion()} returns a quaternion.

  Function \code{as.orthogonal()} returns either a \eqn{3\times 3}{3*3}
  matrix or a \eqn{3\times3\times n}{3*3*n} array of orthogonal matrices
}
\note{
  Function \code{matrix2quaternion()} is low-level; use
  \code{as.quaternion()} to convert arrays.
  }
\author{Robin K. S. Hankin}
\seealso{\code{\link{rotate}}}
\examples{

as.orthogonal(rquat(1))

o <- function(w){diag(3)-2*outer(w,w)/sum(w^2)}  # Householder
matrix2quaternion(o(1:3))   # Booorrrriiinnnggg
matrix2quaternion(o(1:3) \%*\% o(3:1))

Q <- rquat(7)
Q <- Q/abs(Q)
as.quaternion(as.orthogonal(Q))   # +/- Q


A <- replicate(7,o(rnorm(3)) \%*\% o(rnorm(3)))
max(abs(as.orthogonal(as.quaternion(A))-A))

}
\keyword{misc}
