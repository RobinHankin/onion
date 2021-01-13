\name{zapsmall}
\alias{zapsmall}
\alias{zap}
\alias{zapsmall,onion-method}
\alias{zapsmall,onionmat-method}
\title{Concatenation}
\description{Zapping small components to zero}
\usage{
\S4method{zapsmall}{onion}(x,digits=getOption("digits"))
\S4method{zapsmall}{onionmat}(x,digits=getOption("digits"))
}
\arguments{
  \item{x}{An onion or onionmat}
  \item{digits}{integer indicating the precision to be used as in \code{base::zapsmall()}}
}
\details{
Uses \code{base::zapsmall()} to zap small elements to zero.
}
\value{An onion}
\author{Robin K. S. Hankin}
\examples{

zapsmall(as.octonion(0.01^(1:8),single=TRUE))


a <- roct(7)
x <- a^1/a
x
zapsmall(x)

}
