\name{Complex}
\alias{Re}
\alias{Im}
\alias{Re<-}
\alias{Im<-}
\alias{Mod}
\alias{Conj}
\alias{Norm}
\alias{Norm.onion}
\alias{Mod,onion-method}
\alias{Conj,onion-method}
\alias{Re,onion-method}
\alias{Im,onion-method}
\alias{Re<-,onion-method}
\alias{Im<-,onion-method}
\alias{Conj,onion-method}
\alias{Norm,onion-method}
\alias{Mod,onionmat-method}
\alias{Conj,onionmat-method}
\alias{Re,onionmat-method}
\alias{Im,onionmat-method}
\alias{Re<-,onionmat-method}
\alias{Im<-,onionmat-method}
\alias{Conj,onionmat-method}
\alias{Norm,onionmat-method}
\alias{onion_complex}
\alias{onion_imag}
\alias{onion_mod}
\alias{onion_re}
\alias{onion_abs}
\alias{sign,onion-method}
\alias{onion_conjugate}
\title{Complex functionality for onions}
\description{
  Functionality in the Complex group.

  The \dfn{norm} \code{Norm(O)} of onion \eqn{O} is the product of
  \eqn{O} with its conjugate: \eqn{|O|=OO^*} but a more efficient
  numerical method is used (see \code{dotprod()}).

  The \dfn{Mod} \code{Mod(O)} of onion \eqn{O} is the square root of its
  norm.

  The \dfn{sign} of onion \eqn{O} is the onion with the same direction
  as \eqn{O} but with unit Norm: \code{sign(O)=O/Mod(O)}.

  Function \code{Im()} sets the real component of its argument to zero
  and returns that; \code{Conj()} flips the sign of its argument's
  non-real components.  Function \code{Re()} returns the real component
  (first row) of its argument as a numeric vector.  If \code{x} is an
  onion, then \code{x == Re(x) + Im(x)}.
}
\usage{
\S4method{Re}{onion}(z)
\S4method{Im}{onion}(z)
Re(z) <- value
Im(x) <- value
\S4method{Conj}{onion}(z)
\S4method{Mod}{onion}(z)
onion_abs(x)
onion_conjugate(z)
\S4method{sign}{onion}(x)
}
\arguments{
  \item{x,z}{Object of class onion or glub}
  \item{value}{replacement value}
}
\value{
  All functions documented here return a numeric vector or matrix of the
  same dimensions as their argument, apart from functions \code{Im()}
  and \code{Conj()}, which return an object of the same class as its
  argument.
}
\author{Robin K. S. Hankin}
\note{
  If \code{x} is a numeric vector and \code{y} an onion, one might
  expect typing \code{x[1] <- y} to result in \code{x} being a onion.
  This is impossible, according to John Chambers.

  Extract and set methods for components such as \code{i,j,k} are
  documented at \code{Extract.Rd}

  Compare \code{clifford::Conj()}, which is more complicated.
}
\seealso{\code{\link{Extract}}}

\examples{

a <- rquat()
Re(a)
Re(a) <- j(a)

Im(a)

b <- romat()

A <- romat()
Im(A) <- Im(A)*10

}
\keyword{math}
