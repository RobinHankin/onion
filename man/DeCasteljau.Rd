\name{DeCasteljau}
\alias{DeCasteljau}
\title{Spline using the De Casteljau algorithm}
\usage{
DeCasteljau(segments, keyTimes = NULL, times, constantSpeed = FALSE)
}
\arguments{
\item{segments}{a list of vectors of unit quaternions; each segment must
contain at least two quaternions}

\item{keyTimes}{the times corresponding to the segment boundaries, an
increasing vector of length \code{length(segments)+1}; if \code{NULL},
it is set to \code{1, 2, ..., length(segments)+1}}

\item{times}{the interpolating times, they must lie within the range of
\code{keyTimes}; ignored if \code{constantSpeed=TRUE}}

\item{constantSpeed}{Boolean, whether to re-parameterize the spline to
have constant speed; in this case, \code{"times"} is ignored and a
function is returned, with an attribute \code{"times"}, the vector of
new times corresponding to the key rotors}
}
\value{
A vector of quaternions having the same length as \code{times},
  or a function if \code{constantSpeed=TRUE}.
}
\description{
Constructs a quaternions spline using the De Casteljau
  algorithm.
}
\note{
This algorithm is rather for internal purpose. It serves for example
  as a base for the \link[=KochanekBartels]{Kochanek-Bartels} algorithm.
}
\author{
Stéphane Laurent. Original Python code by Matthias Geier.
}
