\name{BarryGoldman}
\alias{BarryGoldman}
\title{Barry-Goldman quaternions spline}
\usage{
BarryGoldman(keyRotors, keyTimes = NULL, times, n_intertimes)
}
\arguments{
\item{keyRotors}{a vector of unit quaternions (rotors) to be interpolated;
it is automatically appended with the first quaternion}

\item{keyTimes}{the times corresponding to the key rotors; must be an
increasing vector of length \code{length(keyRotors)+1}; if \code{NULL},
it is set to \code{c(1, 2, ..., length(keyRotors)+1)}}

\item{times}{the times of interpolation; each time must lie within the
range of the key times; this parameter can be missing if \code{keyTimes}
is \code{NULL} and \code{n_intertimes} is not missing}

\item{n_intertimes}{should be missing if \code{times} is given; otherwise,
\code{keyTimes} should be \code{NULL} and \code{times} is constructed by
linearly interpolating the automatic key times such that there are
\code{n_intertimes - 1} between two key times (so the times are the key
times if \code{n_intertimes = 1})}
}
\value{
A vector of unit quaternions with the same length as \code{times}.
}
\description{
Constructs a spline of unit quaternions by the Barry-Goldman
  method.
}
\note{
The function does not check whether the quaternions given in
  \code{keyRotors} are unit quaternions.
}
\examples{
library(onion)
# We will use a Barry-Goldman quaternions spline to construct a spherical
#   curve interpolating some key points on the sphere of radius 5.

# helper function: spherical to Cartesian coordinates
sph2cart <- function(rho, theta, phi){
  return(c(
    rho * cos(theta) * sin(phi),
    rho * sin(theta) * sin(phi),
    rho * cos(phi)
  ))
}

# construction of the key points on the sphere
keyPoints <- matrix(nrow = 0L, ncol = 3L)
theta_ <- seq(0, 2*pi, length.out = 9L)[-1L]
phi <- 1
for(theta in theta_){
  keyPoints <- rbind(keyPoints, sph2cart(5, theta, phi))
  phi = pi - phi
}
n_keyPoints <- nrow(keyPoints)

# construction of the key rotors; the first key rotor is the identity
#   quaternion and rotor i sends the first key point to the key point i
keyRotors <- quaternion(length.out = n_keyPoints)
rotor <- keyRotors[1L] <- H1
for(i in seq_len(n_keyPoints - 1L)){
  keyRotors[i+1L] <- rotor <-
    quaternionFromTo(keyPoints[i, ]/5, keyPoints[i+1L, ]/5) * rotor
}

# Barry-Goldman quaternions spline
rotors <- BarryGoldman(keyRotors, n_intertimes = 10L)

# construction of the interpolating points on the sphere
points <- matrix(nrow = 0L, ncol = 3L)
keyPoint1 <- rbind(keyPoints[1L, ])
for(i in seq_along(rotors)){
  points <- rbind(points, rotate(keyPoint1, rotors[i]))
}

# visualize the result with the 'rgl' package
library(rgl)
spheres3d(0, 0, 0, radius = 5, color = "lightgreen")
spheres3d(points, radius = 0.2, color = "midnightblue")
spheres3d(keyPoints, radius = 0.25, color = "red")
}
