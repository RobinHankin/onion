\name{Re}
\alias{Re}
\alias{Im}
\alias{i}
\alias{j}
\alias{k}
\alias{l}
\alias{il}
\alias{jl}
\alias{kl}
\alias{Re.octonion}
\alias{Im.octonion}
\alias{i.octonion}
\alias{j.octonion}
\alias{k.octonion}
\alias{l.octonion}
\alias{il.octonion}
\alias{jl.octonion}
\alias{kl.octonion}
\alias{Re.quaternion}
\alias{Im.quaternion}
\alias{i.quaternion}
\alias{j.quaternion}
\alias{k.quaternion}
\alias{Re<-}
\alias{Im<-}
\alias{i<-}
\alias{j<-}
\alias{k<-}
\alias{l<-}
\alias{il<-}
\alias{jl<-}
\alias{kl<-}
\alias{Re<-.octonion}
\alias{Im<-.octonion}
\alias{i<-.octonion}
\alias{j<-.octonion}
\alias{k<-.octonion}
\alias{l<-.octonion}
\alias{il<-.octonion}
\alias{jl<-.octonion}
\alias{kl<-.octonion}
\alias{Re<-.quaternion}
\alias{Im<-.quaternion}
\alias{i<-.quaternion}
\alias{j<-.quaternion}
\alias{k<-.quaternion}
\alias{Im.default}
\alias{Re.default}
\alias{get.comp}
\alias{get.comp.onion}
\alias{set.comp<-}
\alias{set.comp<-.onion}
\alias{set.comp<-.octonion}
\alias{set.comp<-.quaternion}
\title{Octonion components}
\description{Get or set each component of an onionic vector}
\usage{
\method{Re}{octonion}(z)
\method{Im}{octonion}(z)
\method{i}{octonion}(x)
\method{j}{octonion}(x)
\method{k}{octonion}(x)
\method{l}{octonion}(x)
\method{il}{octonion}(x)
\method{jl}{octonion}(x)
\method{kl}{octonion}(x)
\method{Re}{octonion}(x) <- value
\method{Im}{octonion}(x) <- value
\method{i}{octonion}(x) <- value
\method{j}{octonion}(x) <- value
\method{k}{octonion}(x) <- value
\method{l}{octonion}(x) <- value
\method{il}{octonion}(x) <- value
\method{jl}{octonion}(x) <- value
\method{kl}{octonion}(x) <- value
\method{Re}{quaternion}(z)
\method{Im}{quaternion}(z)
\method{i}{quaternion}(x)
\method{j}{quaternion}(x)
\method{k}{quaternion}(x)
\method{Re}{quaternion}(x) <- value
\method{Im}{quaternion}(x) <- value
\method{i}{quaternion}(x) <- value
\method{j}{quaternion}(x) <- value
\method{k}{quaternion}(x) <- value
\method{get.comp}{onion}(x,i)
\method{set.comp}{onion}(x,i) <- value
}
\arguments{
  \item{x,z}{An onionic vector}
  \item{value}{A real vector (or, in the case of \code{Im<-()} and
    \code{set.comp<-()}, an appropriately sized matrix)}
  \item{i}{In functions \code{get.comp()} and \code{set.comp<-()}, an
    integer between 1 and \eqn{2^n} where \eqn{n} depends on the
    type of onion}
}
\value{
  All return an onion of the appropriate class.
}
\author{Robin K. S. Hankin}
\note{
  In the case of \code{Im<-} methods, if \code{value} has the special
  value \code{0}, then all the imaginary parts will be set to zero, as
  though one had typed \code{Im(a) <- Im(a)*0}.  Note that setting value
  to \code{rep(0,length(x))} will \emph{not} work; neither will
  \code{Im(x) <- 3} (say).

  These functions are all specific to their algebra; there is no onionic
  generalization.  This is because the code is more structured.
  It also makes it easier to change the names of the bases.

  Functions \code{get.comp()} and the various methods for
  \code{set.comp<-()} are not really intended for the end-user. It is
  better to use idioms such as \code{il(x)} and \code{i(x) <- 4}
}
\seealso{\code{\link{octonion}}}
\examples{
x <- octonion(Re=1,il=1:3,j=3:1)
Re(x)
kl(x) <- 1000
}
\keyword{misc}
