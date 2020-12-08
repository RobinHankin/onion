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
\alias{onion_power_singleinteger}
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
onion_power_singleinteger(o,n)
onion_power_numeric(o,p)
}
\arguments{
  \item{z,e1,e2,a,b,o,o1,o2,n,q1,q2,p}{onions or numerics}
}
\details{

  The function \code{Ops.onion()} passes unary and binary arithmetic
  operators (\dQuote{\code{+}}, \dQuote{\code{-}}, \dQuote{\code{*}}, and
  \dQuote{\code{/}}) to the appropriate specialist function.

  The most interesting operator is \dQuote{\code{*}}, which is passed to
  \code{AprodA()}.  This function is sensitive to the value of option
  \code{use.R}.  If this is \code{TRUE}, then arguments are passed, via
  \code{Amassage()}, to either \code{R_HprodH()} (for quaternions), or
  \code{R_OprodO()} (for octonions).  If option \code{use.R} is
  anything other than \code{TRUE} (including being unset, which is the
  default), the massaged arguments are passed to
  \code{HprodH()} or \code{OprodO()}.  This is what the user usually
  wants: it is much faster than using the \code{R_} functions.

  The relative performance of, say, \code{OprodO()} vs
  \code{R_OprodO()}, will be system dependent but on my little Linux
  system (Fedora; 256MB) \code{OprodO()} runs more than three hundred
  times faster than \code{R_OprodO()}.  Your mileage may vary; see
  examples section for using \code{options()} to set argument \code{ur}.



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
