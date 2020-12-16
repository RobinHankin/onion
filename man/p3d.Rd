\name{p3d}
\alias{p3d}
\title{Three dimensional plotting}
\description{
  Three dimensional plotting of points.  Produces a nice-looking 3D
  scatterplot with greying out of further points givin a visual depth cue
}
\usage{
p3d(x, y, z, xlim = NULL, ylim = NULL, zlim = NULL, d0 = 0.2, h = 1, ...)
}
\arguments{
  \item{x,y,z}{vector of \eqn{x,y,z} coordinates to be plotted.  If
    \code{x} is a matrix, interpret the rows as 3D Cartesian coordinates}
  \item{xlim,ylim,zlim}{Limits of plot in the \eqn{x,y,z} directions,
    with default \code{NULL} meaning to use \code{range()}}
  \item{d0}{E-folding distance for graying out (depths are standardized
    to be between 0 and 1)}
  \item{h}{The hue for the points, with default value of 1 corresponding
    to red.   If \code{NULL}, produce black points greying to white}
  \item{\dots}{Further arguments passed to \code{persp()} and
    \code{points()}}
}
\value{
Value returned is that given by function \code{trans3d()}.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{bunny}}}
\examples{
data(bunny)
p3d(bunny,theta=3,phi=104,box=FALSE)
}
\keyword{hplot}
