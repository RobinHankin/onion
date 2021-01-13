\name{bind}
\alias{bind}
\alias{rbind}
\alias{cbind}
\alias{bind_onion}
\alias{bind_onion_matrix}
\alias{bind_onion_onion}
\alias{bind_onion_onionmat}
\alias{bind_onionmat_onion}
\alias{bind_onionmat_onionmat}
\alias{rbind2,onion,onion-method}
\alias{cbind2,onion,onion-method}
\alias{rbind2,onionmat,onionmat-method}
\alias{cbind2,onionmat,onionmat-method}
\alias{rbind2,onion,onionmat-method}
\alias{cbind2,onion,onionmat-method}
\alias{rbind2,onionmat,onion-method}
\alias{cbind2,onionmat,onion-method}
\alias{rbind2,onion,numeric-method}
\alias{cbind2,onion,numeric-method}
\alias{rbind2,numeric,onion-method}
\alias{cbind2,numeric,onion-method}
\alias{rbind2,onionmat,numeric-method}
\alias{cbind2,onionmat,numeric-method}
\alias{rbind2,numeric,onionmat-method}
\alias{cbind2,numeric,onionmat-method}
\alias{rbind2,onion,matrix-method}
\alias{cbind2,onion,matrix-method}
\alias{rbind2,matrix,onion-method}
\alias{cbind2,matrix,onion-method}
\alias{rbind2,onionmat,matrix-method}
\alias{cbind2,onionmat,matrix-method}
\alias{rbind2,matrix,onionmat-method}
\alias{cbind2,matrix,onionmat-method}
\title{Binding of onionmats}
\description{
  Methods for \code{rbind()} and \code{cbind()} of onionmats.  These are
  implemented by specifying methods for \code{rbind2()} and
  \code{cbind2()}.
}
\usage{
bind_onion(x,bind,...)
bind_onion_onion(x,y,bind,...)
bind_onion_onionmat(x,y,bind,...)
bind_onionmat_onion(x,y,bind,...)
}
\arguments{
  \item{x,y}{Onions or onionmats}
  \item{bind}{Either \code{rbind} or \code{cbind} as appropriate}
  \item{...}{Further arguments}
}
\value{Return onionmats}
\author{Robin K. S. Hankin}
\examples{
rbind(rquat(3),rquat(3))

cbind(diag(5),roct(1))


cbind(matrix(Oil,4,2),matrix(roct(12),4,3))

}
