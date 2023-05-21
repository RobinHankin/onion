\name{roct}
\alias{rquat}
\alias{roct}
\alias{romat}
\alias{rsquat}
\alias{rsoct}
\alias{rsomat}
\alias{ronionmat}
\title{Random onionic vectors}
\description{
  Random quaternion or octonion vectors and matrices
}
\usage{
rquat(n=5)
roct(n=5)
rsquat(n=11,s=12)
rsoct(n=11,s=12)
romat(type="quaternion", nrow=5, ncol=6, ...)
rsomat(type="quaternion", nrow=5, ncol=6, ...)
}
\arguments{
  \item{n}{Length of random vector returned}
  \item{nrow,ncol,...}{Further arguments specifying properties of the
    returned matrix}
  \item{s}{In the sparse functions \code{rsquat()} and \code{rsoct()},
    an integer specifying the level of sparsity, with higher values
    meaning to return sparser onions}
  \item{type}{string specifying type of elements}
}
\details{

  Function \code{rquat()} returns a quaternionic vector, \code{roct()}
  returns an octonionic vector, and \code{romat()} a quaternionic
  matrix.

  Functions \code{rquat()} and \code{roct()} give a quick \dQuote{get
  you going} random onion to play with.  Function \code{romat()} gives a
  simple onionmat, although arguably \code{matrix(roct(4),2,2)} is as
  convenient.

  The \dQuote{sparse} functions \code{rsquat()} and \code{rsoct()} and
  \code{rsomat()} return onions that have many zero entries; non-zero
  entries are small integers.  They showcase the print method for the
  case when \code{show_onions_compactly} is set.

}
\author{Robin K. S. Hankin}
\references{
  K. Shoemake 1992. \dQuote{Uniform random rotations}. 
  In D. Kirk, editor, \emph{Graphics Gems III} pages 129-130. Academic,
  New York.
}
\examples{
rquat(3)
roct(3)
plot(roct(30))

romat()


rsquat()
rsoct()

}
