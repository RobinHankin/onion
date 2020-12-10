\name{O1}
\alias{H0}
\alias{H1}
\alias{Hi}
\alias{Hj}
\alias{Hk}
\alias{Hall}
\alias{Him}
\alias{O0}
\alias{O1}
\alias{Oi}
\alias{Oj}
\alias{Ok}
\alias{Ol}
\alias{Oil}
\alias{Ojl}
\alias{Okl}
\alias{Oim}
\alias{Oall}
\docType{data}
\title{Unit onions}
\description{
	Each of the eight unit quaternions and octonions 
}
\usage{
H1
Hi
Hj
Hk
H0
Him
Hall
O1
Oi
Oj
Ok
Ol
Oil
Ojl
O0
Oim
Oall
}
\format{Each one is an onionic vector of length one.}
\value{A length-one onion, either a quaternion or an octonion}
\details{
Try \code{Hi} (=\code{quaternion(i=1)}) to get the pattern for the first
four.  The next ones are the zero quaternion, the pure imaginary
quaternion with all components 1, and the quaternion with all components
1.  The ones beginning with \dQuote{O} follow a similar pattern.

These are just variables that may be overwritten and thus resemble
\code{T} and \code{F} whose value may be changed.  
}
\examples{
Oall
seq_onion(from=O1,to=Oil,len=6)

stopifnot(Hj*Hk ==  Hi)
stopifnot(Okl*Oil == -Oj )  # See tests/test_aaa.R for the full set

}
\keyword{datasets}
