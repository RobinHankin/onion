\name{plot}
\alias{plot}
\alias{plot.onion}
\alias{plot,onion-method}
\title{Plot onions}
\description{
  Plotting method for onionic vectors
}
\usage{
\S4method{plot}{onion}(x,y, ...)
}
\arguments{
  \item{x,y}{Onions}
  \item{\dots}{Further arguments passed to \code{plot.default()}}
}
\details{
  The function is \code{plot(Re(x), Mod(Im(x)), ...)}, and thus
  behaves similarly to \code{plot()} when called with a complex vector.
}
\value{Called for its side-effect of plotting a diagram}
\author{Robin K. S. Hankin}
\examples{
plot(roct(30))
}
\keyword{array}
