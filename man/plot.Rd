\name{plot.onion}
\alias{plot.onion}
\title{Plot onions}
\description{
  Plotting method for onionic vectors
}
\usage{
\method{plot}{onion}(x, ...)
}
\arguments{
  \item{x}{Onionic vector}
  \item{\dots}{Further arguments passed to \code{plot.default()}}
}
\details{
  The function is \code{plot(Re(x), Mod(Im(x)), ...)}, and thus
    behaves similarly to \code{plot()} when called with a complex vector.
}
\author{Robin K. S. Hankin}
\examples{
plot(roct(30))
}
\keyword{array}
