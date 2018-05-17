\name{Norm}
\alias{Norm}
\alias{Norm.onion}
\alias{norm.onion}
\alias{norm}
\alias{Mod}
\alias{Mod<-}
\alias{Mod.onion}
\alias{Mod<-.onion}
\alias{sign}
\alias{Sign}
\alias{sign.onion}
\title{Norm and modulus of an onionic vector}
\description{
Norm of an onionic vector: the  modulus and its square
}
\usage{
\method{Norm}{onion}(z)
\method{Mod}{onion}(z)
\method{Mod}{onion}(z) <- value
\method{sign}{onion}(x)
}
\arguments{
  \item{x,z}{Onionic vector}
  \item{value}{A real vector}
}
\author{Robin K. S. Hankin}
\note{

  The \dfn{norm} of onionic vector \eqn{O} is the product of \eqn{O} with
  its conjugate: \eqn{|O|=OO^*} but a more efficient numerical method is
  used (see \code{dotprod()}).

  The \dfn{Mod} of  onionic vector \eqn{O} is the square root of its
  norm.

  The \dfn{sign} of onionic \eqn{O} is the onion with the same
  direction as \eqn{O} but with unit Norm: \code{sign(O)=O/Mod(O)}.


}
\examples{
a <- roct(5)
Norm(a)
Mod(a)
}
\keyword{array}
