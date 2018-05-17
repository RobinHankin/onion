\name{onion-package}
\alias{onion-package}
\alias{onion}
\docType{package}
\title{
Manipulation of quaternions and octonions
}
\description{
There are precisely four normed division algebras over the reals: the
reals themselves, the complex numbers, the quaternions, and the
octonions.  The \R system is well equipped to deal with the first two:
the \pkg{onion} package provides some functionality for the third and
fourth.  
}
\details{
\tabular{ll}{
Package: \tab onion\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2007-05-01\cr
License: \tab GPL
}
The package is intended to provide transparent access to quaternions and
octonions.

Package currently S3 but will use S4 methods shortly
}
\author{
Robin K. S. Hankin

Maintainer: hankin.robin@gmail.com
}
\references{
R News article
}
\keyword{ package }
\examples{

as.quaternion(1:10)  # quaternionic vector with zero imaginary components

1:10 + Hj  # Simple nontrivial quaternion; note appropriate behaviour of '+'

1:10 + Oil   # simple octonionic vector ('Oil' is one of the octonionic bases). 

a <- rquat(5) 
b <- rquat(5)  #Quaternionic vectors with random integer components

a*b - b*a     # Nonzero!  (quaternions are not commutative)

Re(a)  #  Re() extracts the real component

i(a) <- 1000 ; a  # individual components may be manipulated intuitively

as.octonion(a)  # 'upgrades' to octonion

x <- roct(5)  # random octonionic vector with integer components
y <- roct(5)
z <- roct(5)

(x*y)*z - z*(y*z)  # Nonzero!  (octonions are not associative)

Norm(x)
Mod(x)  # Modulus and Norm work as expected

# Now some plotting:

a <- as.octonion(c(7,8,3,3,7,1,3,3),single=TRUE)
b <- as.octonion(c(8,4,2,8,3,7,3,7),single=TRUE)
plot(exp(seq(from=a,to=b,len=50)))
# Note operation of seq(), exp(), and plot()
}
