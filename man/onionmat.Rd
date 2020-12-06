\name{onionmat}
\alias{onionmat}
\alias{newonionmat}
\alias{OMprodOM}
\alias{OMneg}
\alias{OMpowerOM}
\alias{OMprodS}
\alias{OMpowerS}
\alias{OMplusOM}
\alias{OMequalOM}
\alias{OMplusS}
\alias{OMpowerOM}
\alias{OMpowerS}
\alias{OMprodOM}
\alias{OMquotientOM}
\alias{SpowerOM}
\alias{dim.onionmat}
\alias{Conj.onionmat}
\alias{Ops.onionmat}
\alias{rownames}
\alias{colnames}
\alias{colnames-methods}
\alias{colnames,ANY-method}
\alias{colnames.onionmat}
\alias{colnames<--methods}
\alias{colnames<-,ANY-method}
\alias{colnames<-.onionmat}
\alias{dimnames.onionmat}
\alias{dimnames<-.onionmat}
\alias{getM}
\alias{getd}
\alias{herm_onion_mat}
\alias{ncol-methods}
\alias{ncol,ANY-method}
\alias{ncol.onionmat}
\alias{newonionmat}
\alias{nrow-methods}
\alias{nrow,ANY-method}
\alias{nrow.onionmat}
\alias{onionmat}
\alias{onionmatprod}
\alias{rownames-methods}
\alias{rownames,ANY-method}
\alias{rownames.onionmat}
\alias{rownames<--methods}
\alias{rownames<-,ANY-method}
\alias{rownames<-.onionmat}
\alias{t.onionmat}
\alias{\%*\%}
\alias{\%*\%.default}
\alias{[.onionmat}
\alias{[<-.onionmat}
\alias{nrow}
\alias{ncol}
\alias{colnames<-}
\alias{rownames<-}
\alias{print.onionmat}
\alias{romat}
\alias{as.octonionmat}
\alias{as.quaternionmat}
\alias{cprod,onionmat,onionmat-method}
\alias{cprod,ANY,onionmat-method}
\alias{cprod,onionmat,ANY-method}
\alias{cprod,onionmat,missing-method}
\alias{cprod,ANY,ANY-method}
\alias{cprod,ANY,ANY-method}
\alias{om_cprod}
\alias{om_ht}
\alias{om_prod}
\alias{om_tcprod}
\alias{onionmat_complex}
\alias{onionmat_conjugate}
\alias{onionmat_imag}
\alias{onionmat_re}
\alias{onionmat_mod}
\title{Onionic matrices}
\description{
  Some simple functionality for quaternionic and octonionic matrices,
  intended for use in the \code{jordan} package.
}  
\usage{
newonionmat(d, M)
onionmat(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)
OMneg(e1)
OMprodOM(e1,e2)
OMpowerOM(...)
OMprodS(e1,e2)
OMpowerS(e1,e2)
OMplusOM(e1,e2)
colnames.onionmat(x)
rownames.onionmat(x)
\method{Conj}{onionmat}(z)
\method{print}{onionmat}(x,...)
nrow(x)
ncol(x)
herm_onion_mat(real_diagonal, onions)
romat(type="quaternion", nrow=5,ncol=6,...)
onionmat_complex(z)
onionmat_conjugate(z)
onionmat_imag(z)
onionmat_re(z)
onionmat_mod(z)
}
\arguments{
  \item{d,M}{data and matrix index}
  \item{x,z,e1,e2}{Objects of class \code{onionmat}}
  \item{data,nrow,ncol,byrow,dimnames}{In functions
    \code{onionmat()} and \code{romat()}, as for \code{matrix()}}
  \item{...}{In functions like \code{OMpowerOM()}, which are designed to
    return informative errors; and \code{print()}, ignored further
    arguments}
  \item{real_diagonal, onions}{In function \code{herm_onion_mat()}, on-
    and  off- diagonal elements of an Hermitian matrix}
  \item{type}{In \code{romat()}, type of output}
}
\details{

  The functionality is intended to match base R's matrix functionality
  in the sense that standard R idiom just goes through for onionic
  matrices.  Determinants are not well-defined for quaternionic or
  octonionic matrices, and matrix inverses are not implemented.

  An object of class \code{onionmat} is a two-element list, the first of
  which is typically an onionic vector of elements, and the second an
  integer matrix used for tracking attributes such as dimnames, and
  also for the extraction method.

  Functions such as \code{OMpowerOM()} are low-level helper functions,
  not really designed for the end-user.

}
\author{Robin K. S. Hankin}
\examples{

newonionmat(rquat(28),matrix(0,4,7))

}
