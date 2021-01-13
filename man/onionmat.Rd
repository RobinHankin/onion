\name{onionmat}
\alias{onionmat}
\alias{as.onionmat}
\alias{newonionmat}
\alias{onionmat_prod_onionmat}
\alias{onionmat_neg}
\alias{onionmat_inv}
\alias{onionmat_unary}
\alias{onionmat_power_onionmat}
\alias{onionmat_prod_single}
\alias{single_prod_onionmat}
\alias{onionmat_power_single}
\alias{onionmat_plus_onionmat}
\alias{onionmat_plus_single}
\alias{onionmat_plus_matrix}
\alias{matrix_plus_onionmat}
\alias{onionmat_power_single}
\alias{onionmat_power_matrix}
\alias{onionmat_prod_onionmat}
\alias{onionmat_prod_matrix}
\alias{single_power_onionmat}
\alias{Ops.onionmat}
\alias{getM}
\alias{getd}
\alias{herm_onion_mat}
\alias{drop}
\alias{drop,onionmat-method}
\alias{matrix,onion-method}
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
\alias{diag.onion}
\alias{diag}
\alias{diag.onionmat}
\alias{diag,onionmat-method}
\alias{diag,onion-method}
\alias{diag<-.onionmat}
\alias{diag<-,onionmat-method}
\alias{t,onionmat-method}
\alias{t,onion-method}
\alias{t.onionmat}
\alias{t.onion}
\alias{\%*\%}
\alias{\%*\%,onionmat,onionmat-method}
\alias{\%*\%,onionmat,onion-method}
\alias{\%*\%,numeric,onion-method}
\alias{\%*\%,numeric,onionmat-method}
\alias{\%*\%,onion,onionmat-method}
\alias{\%*\%,onionmat,numeric-method}
\alias{[.onionmat}
\alias{[<-.onionmat}
\alias{nrow}
\alias{ncol}
\alias{colnames<-}
\alias{rownames<-}
\alias{as.octonionmat}
\alias{as.quaternionmat}
\alias{as.octonionmat}
\alias{ht}
\alias{ht,onionmat-method}
\alias{ht,onion-method}
\alias{cprod}
\alias{cprod,onionmat,onionmat-method}
\alias{cprod,ANY,onionmat-method}
\alias{cprod,onionmat,ANY-method}
\alias{cprod,onionmat,missing-method}
\alias{cprod,ANY,ANY-method}
\alias{cprod,ANY,ANY-method}
\alias{cprod,ANY,missing-method}
\alias{cprod,onion,missing-method}
\alias{cprod,onion,onion-method}
\alias{cprod,onion,onionmat-method}
\alias{cprod,onionmat,onion-method}
\alias{tcprod}
\alias{tcprod,onionmat,onionmat-method}
\alias{tcprod,ANY,onionmat-method}
\alias{tcprod,onionmat,ANY-method}
\alias{tcprod,onionmat,missing-method}
\alias{tcprod,ANY,ANY-method}
\alias{tcprod,ANY,missing-method}
\alias{tcprod,onion,missing-method}
\alias{tcprod,onion,onion-method}
\alias{tcprod,onion,onionmat-method}
\alias{tcprod,onionmat,onion-method}
\alias{+,onion,missing-method}
\alias{-,onion,missing-method}
\alias{+,onionmat,missing-method}
\alias{-,onionmat,missing-method}
\alias{om_cprod}
\alias{om_tcprod}
\alias{om_ht}
\alias{om_prod}
\alias{om_tcprod}
\alias{onionmat_complex}
\alias{onionmat_conjugate}
\alias{onionmat_imag}
\alias{onionmat_re}
\alias{onionmat_mod}
\alias{single_arith_onionmat}
\alias{onionmat_arith_onionmat}
\alias{onionmat_arith_onion}
\alias{onionmat_arith_single}
\alias{onionmat_inverse}
\alias{onionmat_negative}
\alias{numeric_arith_onionmat}
\alias{onionmat_arith_onion}
\alias{onion_arith_onionmat}
\alias{onion_arith_single}
\alias{single_arith_onionmat}
\alias{matrix_arith_onionmat}
\alias{onionmat_arith_matrix}
\alias{onionmat_matrixprod_onionmat}
\alias{onionmat_matrixprod_onion}
\alias{onionmat_matrixprod_numeric}
\alias{numeric_matrixprod_onionmat}
\alias{onion_matrixprod_onionmat}
\alias{is.onionmat}
\alias{matrix_arith_onion}
\alias{onion_arith_matrix}
\alias{matrix_plus_onion}
\alias{matrix_prod_onion}
\alias{onion_power_matrix}
\title{Onionic matrices}
\description{

  Simple functionality for quaternionic and octonionic matrices,
  intended for use in the \code{jordan} package.  Use idiom like
  \code{matrix(Him,4,5)} or \code{matrix(roct(6),2,3)} to create an
  \code{onionmat} object, a matrix of onions.

  The package is intended to match base \R's matrix functionality in the
  sense that standard \R idiom just goes through for onionic matrices.
  Determinants are not well-defined for quaternionic or octonionic
  matrices, and matrix inverses are not implemented.

}  
\usage{
newonionmat(d, M)
onionmat(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)
as.onionmat(x)
is.onionmat(x)
onionmat_negative(e1)
onionmat_inverse(e1)
onionmat_prod_onionmat(e1,e2)
onionmat_power_onionmat(...)
onionmat_prod_single(x,y)
onionmat_power_single(e1,e2)
onionmat_plus_onionmat(e1,e2)
matrix_arith_onion(e1,e2)
onion_arith_matrix(e1,e2)
matrix_plus_onion(e1,e2)
matrix_prod_onion(e1,e2)
drop(x)
\S4method{cprod}{onionmat,onionmat}(x,y)
\S4method{cprod}{onionmat,missing}(x,y)
\S4method{cprod}{onionmat,ANY}(x,y)
\S4method{cprod}{ANY,ANY}(x,y)
\S4method{cprod}{onion,missing}(x,y)
\S4method{cprod}{onion,onion}(x,y)
\S4method{cprod}{onion,onionmat}(x,y)
\S4method{cprod}{onionmat,onion}(x,y)
\S4method{tcprod}{onionmat,onionmat}(x,y)
\S4method{tcprod}{onionmat,missing}(x,y)
\S4method{tcprod}{onionmat,ANY}(x,y)
\S4method{tcprod}{ANY,ANY}(x,y)
\S4method{cprod}{onion,missing}(x,y)
\S4method{cprod}{onion,onion}(x,y)
\S4method{cprod}{onion,onionmat}(x,y)
\S4method{cprod}{onionmat,onion}(x,y)
\S4method{t}{onionmat}(x)
\S4method{t}{onion}(x)
\S4method{ht}{onionmat}(x)
\S4method{ht}{onion}(x)
nrow(x)
ncol(x)
herm_onion_mat(real_diagonal, onions)
onionmat_complex(z)
onionmat_conjugate(z)
onionmat_imag(z)
onionmat_re(z)
onionmat_mod(z)
onionmat_matrixprod_onionmat(x,y)
onion_matrixprod_onionmat(x,y)
onionmat_matrixprod_numeric(x,y)
onionmat_matrixprod_onion(x,y)
}
\arguments{
  \item{d,M}{data and matrix index}
  \item{x,y,z,e1,e2}{Objects of class \code{onionmat}}
  \item{data,nrow,ncol,byrow,dimnames}{In function
    \code{onionmat()}, as for \code{matrix()}}
  \item{...}{Further arguments (currently ignored)}
  \item{real_diagonal, onions}{In function \code{herm_onion_mat()}, on-
    and  off- diagonal elements of an Hermitian matrix}
}
\details{

  An object of class \code{onionmat} is a two-element list, the first of
  which is an onion, and the second an integer matrix used for tracking
  attributes such as dimensions and dimnames.  This device makes the
  extraction and replacement methods easy.  

  The S4 method for \code{matrix()} simply dispatches to
  \code{onionmat()}, which is a drop-in replacement for \code{matrix()}.

  Function \code{newonionmat()} is lower-level: it also creates onionmat
  objects, but takes two arguments: an onion and a matrix; the matrix
  argument can be used to specify additional attributes via
  \code{attr()}, but this ability is not currently used in the package.

  Functions such as \code{onionmat_plus_onionmat()} are low-level helper
  functions, not really designed for the end-user.

  Vignette \code{onionmat} shows some use-cases.

}
\author{Robin K. S. Hankin}
\examples{

matrix(rquat(28),4,7)

M <- onionmat(rquat(10),2,5)
cprod(M) 

Re(M)
Re(M) <- 0.3

romat() \%*\% rquat(6)

}
