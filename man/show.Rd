\name{show}
\alias{show}
\alias{print}
\alias{print.quaternion}
\alias{print.octonion}
\alias{print.onion}
\alias{print.onionmat}
\alias{print,onion-method}
\alias{onion_show}
\alias{show,onion-method}
\alias{onionmat_show}
\alias{onion_to_string_lowlevel}
\alias{onion_to_string}
\title{Print method for onions}
\description{Show methods for onions}
\usage{
\S4method{show}{onion}(object)
onion_show(x,comp=getOption("show_onions_compactly"),h=getOption("show_onions_horizontally"))
}
\arguments{
  \item{x,object}{Onions}
  \item{comp}{Boolean, with \code{TRUE} meaning to print onions
    compactly and any other value to print in matrix form} 
  \item{h}{Boolean, with \code{TRUE} meaning to print by columns and any
    other value  meaning to print horizontally}
}
\details{

  Default behaviour is to print by rows.   To print by columns, set
  option \code{show_onions_horizontally} to \code{TRUE}:

  \code{options("show_onions_horizontally" = TRUE)}

  Any non-\code{TRUE} value (including \code{NULL} and its being unset)
  will restore the default.

  Similarly, to show onions compactly,  set option
  \code{show_onions_compactly} to \code{TRUE}:
  
  \code{options("show_onions_compactly" = TRUE)}
  
}
  
\author{Robin K. S. Hankin}
\note{The print method for \code{onionmat} objects is also sensitive to
  this option.}
\examples{


x <- roct(15)
x  #default

options("show_onions_horizontally" = TRUE)
roct(4) 

options("show_onions_horizontally" = FALSE)  # restore default

options("show_onions_compactly" = TRUE)
x <- as.quaternion(matrix(sample(c(0,0,0,-1,1),80,replace=TRUE),4,20))
options("show_onions_compactly" = FALSE) # restore default

}
\keyword{array}
