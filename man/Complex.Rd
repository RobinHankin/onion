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
\alias{onion_complex}
\alias{onion_imag}
\title{Extract or Replace Parts of onions or glubs}
\description{
  Functionality in the Complex group.

  The \dfn{norm} of onionic vector \eqn{O} is the product of \eqn{O} with
  its conjugate: \eqn{|O|=OO^*} but a more efficient numerical method is
  used (see \code{dotprod()}).

  The \dfn{Mod} of  onionic vector \eqn{O} is the square root of its
  norm.

  The \dfn{sign} of onionic \eqn{O} is the onion with the same
  direction as \eqn{O} but with unit Norm: \code{sign(O)=O/Mod(O)}.

   
 }
 \usage{
\S4method{Re}{onion}(z)
\S4method{Im}{onion}(z)
Re(z) <- value
Im(x) <- value
\S4method{Conj}{onion}(z)
\S4method{Mod}{onion}(z)
}
\arguments{
  \item{x,z}{Object of class onion or glub}
  \item{value}{replacement value}
}
\value{
  Always returns an object of the same class as \code{x}.
}
\author{Robin K. S. Hankin}
\note{
  If \code{x} is a numeric vector and \code{y} a onion, one might expect
  typing \code{x[1] <- y} to result in \code{x} being a onion.  This is
  impossible, according to John Chambers.
}
\examples{
2+2

}
\keyword{math}
