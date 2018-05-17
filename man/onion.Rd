\name{onion}
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
\alias{type}
\title{
Basic onion functions
}
\description{
Construct, coerce to, test for, and print onions
}
\usage{
octonion(length.out = NULL, names = NULL, Re = 0, i = 0, j = 0, 
    k = 0, l = 0, il = 0, jl = 0, kl = 0)
as.octonion(x, single = FALSE, names=NULL)
is.octonion(x)
quaternion(length.out = NULL, names = NULL, Re = 0, i = 0, j = 0, k = 0)
as.quaternion(x, single = FALSE, names=NULL)
is.quaternion(x)
is.onion(x)
as.onion(x,type,names=NULL,single=FALSE)
type(x)
}
\arguments{
  \item{length.out}{In functions \code{quaternion()} and
    \code{octonion()}, the length of the onionic vector returned.}
  \item{names}{In functions \code{quaternion()} and \code{octonion()},
    the names of the octonionic vector returned.}
  \item{Re}{The real part of the onionic vector returned.}
  \item{i}{Component \eqn{i} of the onionic vector returned.}
  \item{j}{Component \eqn{j} of the onionic vector returned.}
  \item{k}{In function \code{octonion()}, component \eqn{k} of the
    octonionic vector returned.}
  \item{l}{In function \code{octonion()}, component \eqn{l} of the
    octonionic vector returned.}
  \item{il}{In function \code{octonion()}, component \eqn{il} of the
    octonionic vector returned.}
  \item{jl}{In function \code{octonion()}, component \eqn{jl} of the
    octonionic vector returned.}
  \item{kl}{In function \code{octonion()}, component \eqn{kl} of the
    octonionic vector returned.}
  \item{x}{Onion to be tested or printed}
  \item{single}{In functions \code{as.octonion()} and
    \code{as.quaternion()}, a Boolean variable with
    default \code{FALSE} meaning to interpret \code{x} as a vector of
    reals to be coerced into an onionic vector with zero imaginary
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
\note{
  An \dfn{onion} is any algebra (over the reals) created by an iterated
  Cayley-Dickson process.  Examples include quaternions, octonions, and
  sedenions.  There does not appear to be a standard terminology for
  such objects (I have seen n-ion, anion and others.  But \dQuote{onion}
  is pronouncable and a bona fide English word).

  Creating further onions is intended to be straightforward; the
  following steps show how to add the sedenions but any number of
  onions may be added the same way.
  \itemize{
    \item Add functions \code{sedenion_prod_single()} and
    \code{sedenion_prod()} to \code{src/onion.c}.
    \item Update the following functions:
    \itemize{
      \item \code{type()}
      \item \code{harmonize()}
      \item \code{as.onion()}
      \item \code{AprodA()}
    }  This should be reasonably straightforward.
    \item create the following functions:
    \itemize{
      \item \code{rsed()} (by analogy with \code{roct()} and \code{rquat()})
      \item \code{SprodS()} (by analogy with \code{OprodO()} and
      \code{HprodH()})
      \item \code{R_SprodS()} (by analogy with \code{R_OprodO()} and
      \code{R_HprodH()})
      \item \code{sedenion()} and \code{is.sedenion()} (by analogy
      with \code{octonion()} and \code{is.octonion()})
      \item \code{as.sedenion()} by analogy with \code{as.octonion()}.
      \item a whole bunch of functions (with appropriate names) to get
      and set individual onion components, by
      analogy with \code{i.quaternion()} and \code{i<-.quaternion()}.
      Sedenions have at least two different naming conventions.
      }
    }
Note that function \code{Ops.onion()} need not be changed, as it copes
with generic onions.
    
  }
\author{Robin K. S. Hankin}
\examples{

x <- octonion(Re=1,il=1:3)
x
kl(x) <- 100
x

as.quaternion(diag(4))
}
\keyword{misc}
