\name{quaternionFromTo}
\alias{quaternionFromTo}
\title{Quaternion between two vectors}
\usage{
quaternionFromTo(u, v)
}
\arguments{
\item{u, v}{two unit 3D vectors}
}
\value{
A unit quaternion whose corresponding rotation transforms \code{u}
  to \code{v}.
}
\description{
Get a unit quaternion whose corresponding rotation sends
  \code{u} to \code{v}; the vectors \code{u} and \code{v} must be normalized.
}
\examples{
u <- c(1, 1, 1) / sqrt(3)
v <- c(1, 0, 0)
q <- quaternionFromTo(u, v)
rotate(rbind(u), q) # this should be v
}
