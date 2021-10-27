\name{interpolateTimes}
\alias{interpolateTimes}
\title{Interpolate a vector of times}
\usage{
interpolateTimes(times, n)
}
\arguments{
\item{times}{increasing vector of times}

\item{n}{integer, controls the number of interpolations: there will be
\code{n-1} time values between two consecutive original times}
}
\value{
A vector, a refinement of the \code{times} vector.
}
\description{
Linearly interpolate an increasing vector of times. This is
  useful to deal with the quaternions splines.
}
\examples{
interpolateTimes(1:4, n = 3)
interpolateTimes(c(1, 2, 4), n = 3)
}
