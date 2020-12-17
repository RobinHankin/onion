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
\alias{[,onion,index,ANY-method}
\alias{[,onion,index,missing-method}
\alias{[<-,onion-method}
\alias{[,onion,ANY,ANY-method}
\alias{[<-,onion,ANY,ANY-method}
\alias{i,onion-method}
\alias{j,onion-method}
\alias{k,onion-method}
\alias{l,onion-method}
\alias{l,octonion-method}
\alias{il,octonion-method}
\alias{jl,octonion-method}
\alias{kl,octonion-method}
\alias{i,onionmat-method}
\alias{j,onionmat-method}
\alias{k,onionmat-method}
\alias{l,onionmat-method}
\alias{il,octonion-method}
\alias{jl,octonion-method}
\alias{kl,octonion-method}
\alias{l,octonion-method}
\alias{i<-,onion-method}
\alias{j<-,onion-method}
\alias{k<-,onion-method}
\alias{l<-,octonion-method}
\alias{il<-,octonion-method}
\alias{jl<-,octonion-method}
\alias{kl<-,octonion-method}
\alias{il,onionmat-method}
\alias{jl,onionmat-method}
\alias{kl,onionmat-method}
\alias{i<-,onionmat-method}
\alias{j<-,onionmat-method}
\alias{k<-,onionmat-method}
\alias{l<-,onionmat-method}
\alias{il<-,onionmat-method}
\alias{jl<-,onionmat-method}
\alias{kl<-,onionmat-method}
\alias{[,onionmat,ANY,ANY-method}
\alias{[,onionmat,index,index-method}
\alias{[,onionmat,index,missing-method}
\alias{[,onionmat,matrix,missing-method}
\alias{[,onionmat,missing,index-method}
\alias{[,onionmat,missing,missing-method}
\alias{[,onionmat,ANY,ANY-method}
\alias{[<-,onion,index,ANY,ANY-method}
\alias{[<-,onion,index,missing,ANY-method}
\alias{[<-,onion,index,missing,numeric-method}
\alias{[<-,onion,index,missing,onion-method}
\alias{[<-,onion,missing,missing,numeric-method}
\alias{[<-,onion,missing,missing,onion-method}
\alias{[<-,onionmat,ANY,missing,numeric-method}
\alias{[<-,onionmat,ANY,missing,onion-method}
\alias{[<-,onionmat,index,index,numeric-method}
\alias{[<-,onionmat,index,index,onion-method}
\alias{[<-,onionmat,index,missing,numeric-method}
\alias{[<-,onionmat,index,missing,onion-method}
\alias{[<-,onionmat,missing,index,numeric-method}
\alias{[<-,onionmat,missing,index,onion-method}
\alias{[,onion,index,ANY,ANY-method}
\alias{[,onion,index,missing,ANY-method}
\alias{[,onionmat,ANY,ANY,ANY-method}
\alias{[,onionmat,index,index,ANY-method}
\alias{[,onionmat,index,missing,ANY-method}
\alias{[,onionmat,index,missing,missing-method}
\alias{[,onionmat,matrix,missing,ANY-method}
\alias{[,onionmat,missing,index,ANY-method}
\alias{[,onionmat,missing,missing,ANY-method}
\alias{[}
\alias{[<-}
\title{Extract or Replace Parts of onions or glubs}
\description{
   Methods for \code{"["} and  \code{"[<-"}, i.e., extraction or
   subsetting of onions.
 }
 \usage{
\S4method{i}{onion}(z)
\S4method{j}{onion}(z)
\S4method{k}{onion}(z)
\S4method{l}{octonion}(z)
\S4method{il}{octonion}(z)
\S4method{jl}{octonion}(z)
\S4method{kl}{octonion}(z)
\S4method{i}{onionmat}(z)
\S4method{j}{onionmat}(z)
\S4method{k}{onionmat}(z)
\S4method{il}{onionmat}(z)
\S4method{jl}{onionmat}(z)
\S4method{kl}{onionmat}(z)
i(x) <- value
j(x) <- value
k(x) <- value
l(x) <- value
il(x) <- value
jl(x) <- value
kl(x) <- value
}
\arguments{
  \item{x,z}{Object of class onion}
  \item{value}{replacement value}
}
\value{
  Extraction and methods return an onion or onionmat.
 Replacement methods return an object of the same class as \code{x}.  
}
\author{Robin K. S. Hankin}
\note{
  If \code{x} is a numeric vector and \code{y} a onion, one might expect
  typing \code{x[1] <- y} to result in \code{x} being a onion.  This is
  impossible, according to John Chambers.
}
\examples{

a <- roct(9)
il(a)
Re(a) <- 1:9

j(a) <- l(a)
a
}
\keyword{math}
