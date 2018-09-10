\name{rotate}
\alias{rotate}
\title{Rotates 3D vectors using quaternions}
\description{
  Rotates a three-column matrix whose rows are vectors in 3D space,
  using quaternions
}
\usage{
rotate(x, H)
}
\arguments{
  \item{x}{A matrix of three columns whose rows are points in 3D space}
  \item{H}{A quaternion.  Does not need to have unit modulus}
}
\value{
Returns a matrix of the same size as \code{x}
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{orthogonal}}}
\examples{
data(bunny)
par(mfrow=c(2,2))
par(mai=rep(0,4))
p3d(rotate(bunny,Hi),box=FALSE)
p3d(rotate(bunny,H1-Hi+Hj),box=FALSE)
p3d(rotate(bunny,Hk),box=FALSE)
p3d(rotate(bunny,Hall),box=FALSE)

o <- function(w){diag(3)-2*outer(w,w)/sum(w^2)}  # Householder
O <- o(1:3) \%*\% o(3:1)

rotate(bunny,as.quaternion(O))
bunny \%*\% t(O)    # should be the same; note transpose

}
\keyword{misc}
