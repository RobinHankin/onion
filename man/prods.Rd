\name{prods}
\alias{prod}
\alias{prods}
\alias{\%<*>\%}
\alias{\%>*<\%}
\alias{\%<.>\%}
\alias{\%>.<\%}
\alias{\%<*>\%.onion}
\alias{\%>*<\%.onion}
\alias{\%<.>\%.onion}
\alias{\%>.<\%.onion}
\alias{g.even}
\alias{g.odd}
\alias{e.even}
\alias{e.odd}
\alias{g.even.onion}
\alias{g.odd.onion}
\alias{e.even.onion}
\alias{e.odd.onion}
\title{Various products of two onionic vectors}
\description{
Returns the various inner and outer  products of two onionic vectors.
}
\usage{
x \%<*>\% y
x \%>*<\% y
x \%<.>\% y
x \%>.<\% y
\method{g.even}{onion}(x,y)
\method{g.odd}{onion}(x,y)
\method{e.even}{onion}(x,y)
\method{e.odd}{onion}(x,y)
}
\arguments{
  \item{x}{Onionic vector}
  \item{y}{Onionic vector}
}
\details{
  This page documents an attempt at a consistent notation for onionic
  products.  The product used by \code{Ops.octonion()}  and
  \code{Ops.quaternion()} (viz
  \dQuote{\code{*}}) is sometimes known as the \dQuote{Grassman
    product}.  There is another product known as the Euclidean product
  defined by \eqn{E(p,q)=p'q} where \eqn{x'} is the conjugate of \eqn{x}.

  Each of these products separates into an \dQuote{even} and an
  \dQuote{odd} part, here denoted by functions \code{g.even()} and
  \code{g.odd()} for the Grassman product, and \code{e.even()} and
  \code{e.odd()} for the Euclidean product.  These are defined as
  follows:

  \itemize{
    \item \code{g.even(x,y)=(xy+yx)/2}
    \item \code{g.odd(x,y)=(xy-yx)/2}
    \item \code{e.even(x,y)=(x'y+y'x)/2} 
    \item \code{e.odd(x,y)=(x'y-y'x)/2}
  }
  
These functions have an equivalent binary operator.

The Grassman operators have a \dQuote{\code{*}}; they are
\dQuote{\code{\%<*>\%}} for the even Grassman product and
\dQuote{\code{\%>*<\%}} for the odd product.

The Euclidean operators have a \dQuote{\code{.}}; they are
\dQuote{\code{\%<.>\%}} for the even Euclidean product and
\dQuote{\code{\%>.<\%}} for the odd product.

There is no binary operator for the ordinary Euclidean product (it is
not defined because there is no natural, consistent notation available;
and it seems to be rarely needed in practice).  Use \code{Conj(x)*y}.
}
\author{Robin K. S. Hankin}
\examples{
Oj \%<.>\% Oall
}
\keyword{array}
