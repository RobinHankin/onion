\name{Arith}
\docType{methods}
\alias{Arith}
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
\alias{onion_power_singleinteger}
\alias{onion_power_numeric}
\title{Methods for Function Arith in package Onion}
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
onion_power_singleinteger(o,n)
onion_power_numeric(o,p)
}
\arguments{
  \item{z,e1,e2,a,b,o,o1,o2,n,q1,q2,p}{onions or numeric vectors}
}
\details{

  The package implements the \code{Arith} group of \code{S4} generics so
  that idiom like \code{A + B*C} works as expected with onions.

  Functions like \code{onion_inverse()} and \code{onion_plus_onion()}
  are low-level helper functions.  The only really interesting operation
  is multiplication; functions \code{octonion_prod_octonion()} and
  \code{quaternion_prod_quaternion()} dispatch to \proglang{C}.

  Names are implemented and the rules are inherited (via
  \code{harmonize_oo()} and \code{harmonize_on()}) from \code{rbind()}.

}
\value{generally return an onion}
\author{Robin K. S. Hankin}
\note{
  Previous versions of the package included the option to use native \R
  rather than the faster compiled \proglang{C} code used here.  But this
  was very slow and is now discontinued.

}
\examples{
a <- rquat()
b <- rquat()
a
Re(a)
j(a) <- 0.2
a*b
b*a  # quaternions are noncommutative


x <- as.octonion(matrix(rnorm(40),nrow=8))
y <- roct()
z <- roct()


x*(y*z) - (x*y)*z  # octonions are nonassociative [use associator()]






}
\keyword{methods}
\keyword{math}
