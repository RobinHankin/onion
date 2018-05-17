\name{exp}
\alias{exp}
\alias{log}
\alias{ln}
\alias{sin}
\alias{cos}
\alias{tan}
\alias{asin}
\alias{acos}
\alias{atan}
\alias{sinh}
\alias{cosh}
\alias{tanh}
\alias{asinh}
\alias{acosh}
\alias{atanh}
\alias{sqrt}
\alias{exp.octonion}
\alias{log.octonion}
\alias{ln.octonion}
\alias{sin.octonion}
\alias{cos.octonion}
\alias{tan.octonion}
\alias{asin.octonion}
\alias{acos.octonion}
\alias{atan.octonion}
\alias{sinh.octonion}
\alias{cosh.octonion}
\alias{tanh.octonion}
\alias{asinh.octonion}
\alias{acosh.octonion}
\alias{atanh.octonion}
\alias{sqrt.octonion}
\alias{exp.onion}
\alias{log.onion}
\alias{ln.onion}
\alias{sin.onion}
\alias{cos.onion}
\alias{tan.onion}
\alias{asin.onion}
\alias{acos.onion}
\alias{atan.onion}
\alias{sinh.onion}
\alias{cosh.onion}
\alias{tanh.onion}
\alias{asinh.onion}
\alias{acosh.onion}
\alias{atanh.onion}
\alias{sqrt.onion}
\title{Elementary transcendental functions}
\description{
  Elementary transcendental functions: exponential and trig
}
\usage{
\method{exp}{onion}(x)
\method{log}{onion}(x,base=exp(1))
\method{sin}{onion}(x)
\method{cos}{onion}(x)
\method{tan}{onion}(x)
\method{asin}{onion}(x)
\method{acos}{onion}(x)
\method{atan}{onion}(x)
\method{sinh}{onion}(x)
\method{cosh}{onion}(x)
\method{tanh}{onion}(x)
\method{asinh}{onion}(x)
\method{acosh}{onion}(x)
\method{atanh}{onion}(x)
\method{sqrt}{onion}(x)
}
\arguments{
  \item{x}{An onionic vector}
  \item{base}{In \code{log()}, the base of the logarithm}
  }
\details{
  Trig and exponential functions, and a square root.  \strong{Warning:}
  these functions do not obey all the identities that one might expect;
  quaternions are not commutative, and octonions are not associative.
  The examples section illustrates this.
}
\author{Robin K. S. Hankin}
\examples{
x <- roct(3)/10
sin(x)^2 + cos(x)^2  #should be close to O1

a <- rquat(5)
b <- roct(5)

log(a*b) -log(a) -log(b)  #zero for real or complex a & b, but not quaternions
log(b*a) -log(a) -log(b)  #different (and still nonzero)
}
\keyword{array}
