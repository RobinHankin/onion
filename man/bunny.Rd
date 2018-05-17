\name{bunny}
\alias{bunny}
\docType{data}
\title{The Stanford Bunny}
\description{
  A set of 3D points in the shape of a rabbit (the Stanford Bunny)
}
\usage{data(bunny)}
\format{
A three column matrix with 35947 rows.  Each row is the Cartesian
coordinates of a point on the
surface of the bunny.
}
\source{
  \url{http://graphics.stanford.edu/data/3Dscanrep/}
}
\examples{
data(bunny)
p3d(rotate(bunny,Hk))
}
\keyword{datasets}
