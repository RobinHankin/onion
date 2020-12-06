\name{Extract}
\alias{i}
\alias{j}
\alias{k}
\alias{l}
\alias{il}
\alias{jl}
\alias{kl}
\alias{i<-}
\alias{j<-}
\alias{k<-}
\alias{l<-}
\alias{il<-}
\alias{jl<-}
\alias{kl<-}
\alias{i.octonion}
\alias{j.octonion}
\alias{k.octonion}
\alias{l.octonion}
\alias{il.octonion}
\alias{jl.octonion}
\alias{kl.octonion}
\alias{i.quaternion}
\alias{j.quaternion}
\alias{k.quaternion}
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
\alias{[.onion}
\alias{[<-.onion}
\alias{[,onion-method}
\alias{[<-,onion-method}
\alias{[,onion,ANY,ANY-method}
\alias{[<-,onion,ANY,ANY-method}
\alias{i,onion-method}
\alias{j,onion-method}
\alias{k,onion-method}
\alias{il,octonion-method}
\alias{jl,octonion-method}
\alias{kl,octonion-method}
\alias{l,octonion-method}
\alias{i<-,onion-method}
\alias{j<-,onion-method}
\alias{k<-,onion-method}
\alias{il<-,octonion-method}
\alias{jl<-,octonion-method}
\alias{kl<-,octonion-method}
\title{Extract or Replace Parts of onions or glubs}
\description{
   Methods for \code{"["} and  \code{"[<-"}, i.e., extraction or
   subsetting of onions.
 }
 \usage{
\S4method{i}{onion}(z)
\S4method{j}{onion}(z)
\S4method{k}{onion}(z)
\S4method{il}{octonion}(z)
\S4method{jl}{octonion}(z)
\S4method{kl}{octonion}(z)
i(x) <- value
j(x) <- value
k(x) <- value
l(x) <- value
il(x) <- value
jl(x) <- value
kl(x) <- value
}
\arguments{
  \item{x}{Object of class onion or glub}
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
