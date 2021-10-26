\name{DeCasteljau}
\alias{DeCasteljau}
\title{Spline using the De Casteljau algorithm}
\usage{
DeCasteljau(segments, keyTimes = NULL, times)
}
\arguments{
\item{segments}{a list of vectors of unit quaternions; each segment must
contain at least two quaternions}

\item{keyTimes}{the times corresponding to the segment boundaries, an
increasing vector of length \code{length(segments)+1}; if \code{NULL},
it is set to \code{1, 2, ..., length(segments)+1}}

\item{times}{the interpolating times, they must lie within the range of
\code{keyTimes}}
}
\value{
A vector of quaternions having the same length as \code{times}.
}
\description{
Constructs a quaternions spline using the De Casteljau
  algorithm.
}
\note{
This algorithm is rather for internal purpose. It serves for example
  as a base for the \link[=KochanekBartels]{Konachek-Bartels} algorithm.
}
