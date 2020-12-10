\name{Math}
\alias{log}
\alias{exp}
\alias{cosh}
\alias{sinh}
\alias{acos}
\alias{acosh}
\alias{asin}
\alias{asinh}
\alias{atan}
\alias{atanh}
\alias{cos}
\alias{sin}
\alias{tan}
\alias{tanh}
\alias{cos}
\alias{sin}
\alias{tan}
\alias{tanh}
\alias{onion_log}
\alias{onion_sign}
\alias{onion_sqrt}
\alias{onion_exp}
\alias{onion_cosh}
\alias{onion_sinh}
\alias{onion_acos}
\alias{onion_acosh}
\alias{onion_asin}
\alias{onion_asinh}
\alias{onion_atan}
\alias{onion_atanh}
\alias{onion_cos}
\alias{onion_sin}
\alias{onion_tan}
\alias{onion_tanh}
\alias{onion_cos}
\alias{onion_sin}
\alias{onion_tan}
\alias{onion_tanh}
\alias{abs,onion-method}
\alias{log,onion-method}
\alias{exp,onion-method}
\alias{cosh,onion-method}
\alias{sinh,onion-method}
\alias{acos,onion-method}
\alias{acosh,onion-method}
\alias{asin,onion-method}
\alias{asinh,onion-method}
\alias{atan,onion-method}
\alias{atanh,onion-method}
\alias{cos,onion-method}
\alias{sin,onion-method}
\alias{tan,onion-method}
\alias{tanh,onion-method}
\alias{cos,onion-method}
\alias{sin,onion-method}
\alias{tan,onion-method}
\alias{tanh,onion-method}
\alias{sqrt}
\title{Various logarithmic and circular functions for onions}
\description{
Various elementary functions for onions
}
\usage{
onion_log(x,base=exp(1))
onion_exp(x)
onion_sign(x)
onion_sqrt(x)
onion_cosh(x)
onion_sinh(x)
onion_acos(x)
onion_acosh(x)
onion_asin(x)
onion_asinh(x)
onion_atan(x)
onion_atanh(x)
onion_cos(x)
onion_sin(x)
onion_tan(x)
onion_tanh(x)
onion_cos(x)
onion_sin(x)
onion_tan(x)
onion_tanh(x)
}
\arguments{
  \item{x}{Object of class onion}
  \item{base}{In function \code{log()}, the base of the logarithm}
}
\details{
Standard math stuff.  I am not convinced that the trig functions
(\code{sin()} etc) have any value. 

}
\author{Robin K. S. Hankin}  
\examples{
x <- roct()
exp(x+x) - exp(x)*exp(x) # zero to numerical precision

jj <- exp(log(x)/2)      # use sqrt() here
jj*jj-x                  #  also small


y <- roct()
exp(x+y) - exp(x)*exp(y) # some rules do not operate for onions



max(Mod(c(sin(asin(x))-x,asin(sin(x))-x)))   # zero to numerical precision



}
\keyword{math}
