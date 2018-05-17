\name{Extract.onion}
\alias{[.onion}
\alias{[<-.onion}
\title{Extract or Replace Parts of an onion}
\description{
  Extract or replace subsets of onions.
}
\arguments{
  \item{x}{An onionic vector}
  \item{index}{elements to extract or replace}
  \item{value}{replacement value}
}
\details{
These methods work as expected: an octonionic vector is a eight-row
matrix and element selection/replacement operate, on a per-column basis,
on objects coerced to class \dQuote{matrix}.
}
\value{
  Always returns an onion of the same class as \code{x}.
}
\examples{
a <- octonion(Re=1,j=1:10)
a[1:3]  <- Oil
a[3:5]

a[2:4] <- a[1]
a[2:3] <- c(10,11)


}
\keyword{array}
