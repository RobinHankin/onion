\name{prods}
\alias{prods}
\alias{\%<*>\%}
\alias{\%>*<\%}
\alias{\%<.>\%}
\alias{\%>.<\%}
\alias{\%.\%}
\alias{\%<*>\%.onion}
\alias{\%>*<\%.onion}
\alias{\%<.>\%.onion}
\alias{\%>.<\%.onion}
\alias{onion_g_even}
\alias{onion_g_odd}
\alias{onion_e_even}
\alias{onion_e_odd}
\alias{g_even.onion}
\alias{g_odd.onion}
\alias{e_even.onion}
\alias{e_odd.onion}
\alias{dotprod}
\title{Various products of two onions}
\description{
Returns various inner and outer products of two onionic vectors.
}
\usage{
x \%<*>\% y
x \%>*<\% y
x \%<.>\% y
x \%>.<\% y
x \%.\% y
onion_g_even(x,y)
onion_g_odd (x,y)
onion_e_even(x,y)
onion_e_odd (x,y)
dotprod(x,y)
}
\arguments{
  \item{x,y}{onions}
}
\details{

  This page documents an attempt at a consistent notation for onionic
  products.  The default product for onions (viz \dQuote{\code{*}}) is
  sometimes known as the \dQuote{Grassman product}.  There is another
  product known as the Euclidean product defined by \eqn{E(p,q)=p'q}
  where \eqn{x'} is the conjugate of \eqn{x}.

  Each of these products separates into an \dQuote{even} and an
  \dQuote{odd} part, here denoted by functions \code{g_even()} and
  \code{g_odd()} for the Grassman product, and \code{e_even()} and
  \code{e_odd()} for the Euclidean product.  These are defined as
  follows:

  \itemize{
    \item \code{g_even(x,y)=(xy+yx)/2}
    \item \code{g_odd(x,y)=(xy-yx)/2}
    \item \code{e_even(x,y)=(x'y+y'x)/2} 
    \item \code{e_odd(x,y)=(x'y-y'x)/2}
  }
  
These functions have an equivalent binary operator.

The Grassman operators have a \dQuote{\code{*}}; they are
\dQuote{\code{\%<*>\%}} for the even Grassman product and
\dQuote{\code{\%>*<\%}} for the odd product.

The Euclidean operators have a \dQuote{\code{.}}; they are
\dQuote{\code{\%<.>\%}} for the even Euclidean product and
\dQuote{\code{\%>.<\%}} for the odd product.

Function \code{dotprod()} returns the Euclidean even product of two
onionic vectors.  That is, if \code{x} and \code{y} are eight-element
vectors of the components of two onions, return \code{sum(x*y)}.

Note that the returned value is a numeric vector (compare
\code{\%<.>\%}, \code{e.even()}, which return onionic vectors with zero
imaginary part).

There is no binary operator for the ordinary Euclidean product (it seems
to be rarely needed in practice).  For \code{Conj(x)*x}, \code{Norm(x)}
is much more efficient and accurate.

Function \code{prod()} is documented at \code{Summary.Rd}.

}
\author{Robin K. S. Hankin}
\note{Frankly if you find yourself using these operators you might be
  better off using the \pkg{clifford} package, which has an extensive
  and consistent suite of product operators.
}
\examples{
Oj \%<.>\% Oall
}
\keyword{array}
