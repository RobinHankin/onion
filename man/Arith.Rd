\name{Arith-methods}
\docType{methods}
\alias{Arith-methods}
\alias{Arith,onion,missing-method}
\alias{Arith,onion,ANY-method}
\alias{Arith,ANY,onion-method}
\alias{Arith,onion,onion-method}
\alias{Arith,onion,numeric-method}
\alias{onion_negative}
\alias{onion_inverse}
\alias{onion_arith_onion}
\alias{onion_arith_numeric}
\alias{numeric_arith_onion}
\alias{harmonize_oo}
\alias{harmonize_on}
\alias{onion_plus_onion}
\alias{onion_plus_numeric}
\alias{onion_prod_onion}
\alias{octonion_prod_octonion}
\alias{quaternion_prod_quaternion}
\alias{onion_prod_numeric}
\alias{onion_power_singlenumber}
\alias{onion_power_numeric}
\title{Methods for Function Arith in package Oniondingnag}
\description{
Methods for Arithmetic functions for onions: \code{+},
\code{-}, \code{*}, \code{/}, \code{^}
}
\usage{
onion_negative(z)
onion_inverse(z)
onion_arith_onion(e1,e2)
onion_arith_numeric(e1,e2)
numeric_arith_onion(e1,e2)
harmonize_oo(a,b)
harmonize_on(a,b)
onion_plus_onion(a,b)
onion_plus_numeric(a,b)
onion_prod_onion(e1,e2)
octonion_prod_octonion(o1,o2)
quaternion_prod_quaternion(q1,q2)
onion_prod_numeric(a,b)
onion_power_singlenumber(o,n)
onion_power_numeric(a,b)
}
\arguments{
  \item{z,e1,e2,a,b,o,o1,o2,n,q1,q2}{onions or numerics}
}
\author{Robin K. S. Hankin}
\note{

  The binary arithmetic functions coerce \code{numeric} <op>
  \code{onion}, and \code{onion} <op> \code{numeric} to onions.

}
\examples{
3+3
}
\keyword{methods}
\keyword{math}
