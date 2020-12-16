\name{onion}
\alias{onion}
\alias{octonion}
\alias{Octonion}
\alias{quaternion}
\alias{Quaternion}
\alias{is.onion}
\alias{is.octonion}
\alias{as.octonion}
\alias{is.quaternion}
\alias{as.quaternion}
\alias{as.onion}
\alias{quaternion_to_octonion}
\alias{octonion_to_quaternion}
\alias{as.matrix}
\alias{as.matrix,onion-method}
\alias{as.numeric,onion-method}
\alias{octonion_to_quaternion}
\alias{type}
\title{
Basic onion functions
}
\description{
Construct, coerce to, test for, and print onions
}
\usage{
octonion(length.out = NULL, Re = 0, i = 0, j = 0, 
    k = 0, l = 0, il = 0, jl = 0, kl = 0)
as.octonion(x, single = FALSE)
is.octonion(x)
quaternion(length.out = NULL,Re = 0, i = 0, j = 0, k = 0)
as.quaternion(x, single = FALSE)
is.quaternion(x)
is.onion(x)
as.onion(x,type,single=FALSE)
quaternion_to_octonion(from)
octonion_to_quaternion(from)
\S4method{as.matrix}{onion}(x)
\S4method{as.numeric}{onion}(x)
}
\arguments{
  \item{length.out}{In functions \code{quaternion()} and
    \code{octonion()}, the length of the onionic vector returned}
  \item{Re}{The real part of the onionic vector returned}
  \item{i,j,k}{In functions \code{quaternion()} and \code{octonion()},
    component \eqn{i,j,k} respectively of the returned onion}
  \item{l,il,jl,kl}{In function \code{octonion()}, component
    \eqn{l,il,jl,kl} respectively of the returned octonion} 
  \item{x,from}{Onion to be tested or printed}
  \item{single}{In functions \code{as.octonion()} and
    \code{as.quaternion()}, Boolean  with
    default \code{FALSE} meaning to interpret \code{x} as a vector of
    reals to be coerced into an onion  with zero imaginary
    part; and \code{TRUE} meaning to interpret \code{x} as a length 4
    (or length 8) vector and return the corresponding single onion.}
  \item{type}{In function \code{as.onion()} a string either
    \dQuote{\code{quaternion}} or \dQuote{\code{octonion}} denoting the
    algebra to be forced into}
}
\details{
  Functions \code{quaternion()} and \code{octonion()} use standard
  recycling where possible; \code{rbind()} is used.

  Functions \code{as.quaternion()} and \code{as.octonion()} coerce to
  quaternions and octonions respectively.  If given a complex vector,
  the real and imaginary components are interpreted
  as \code{Re} and \code{i} respectively.
    
  The output of \code{type()} is accepted as the \code{type} argument of
  function \code{as.onion()}; thus \code{as.onion(out,type=type(x))} works as
  expected.
}
\value{Generally return onions}
\note{
  An \dfn{onion} is any algebra (over the reals) created by an iterated
  Cayley-Dickson process.  Examples include quaternions, octonions, and
  sedenions.  There does not appear to be a standard generic term for
  such objects (I have seen n-ion, anion and others.  But \dQuote{onion}
  is pronouncable and a bona fide English word).

  Creating further onions---such as the sedenions---is intended to be
  straightforward.

  There is a nice example of the onion package in use in the
  \code{permutations} package, under \code{cayley.Rd}.  This also shows
  the quaternion group Q8, but from a different perspective.
}
\author{Robin K. S. Hankin}
\examples{

x <- octonion(Re=1,il=1:3)
x
kl(x) <- 100
x

as.quaternion(diag(4))


# Cayley table for the quaternion group Q8:
a <- c(H1,-H1,Hi,-Hi,Hj,-Hj,Hk,-Hk)
names(a) <- c("+1","-1","+i","-i","+j","-j","+k","-k")

f <- Vectorize(function(x,y){names(a)[a==a[x]*a[y]]})
X <- noquote(outer(1:8,1:8, f))
rownames(X) <- names(a)
colnames(X) <- names(a)
X

}
\keyword{misc}
