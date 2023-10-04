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
\alias{comp_names}
\title{Print method for onions}
\description{Show methods for onions}
\usage{
\S4method{show}{onion}(object)
onion_show(x,
     comp = getOption("show_onions_compactly"),
     h    = getOption("show_onions_horizontally")
)
comp_names(x)
}
\arguments{
  \item{x,object}{Onions}
  \item{comp}{Boolean, with \code{TRUE} meaning to print onions
    compactly and any other value to print in matrix form} 
  \item{h}{Boolean, with \code{TRUE} meaning to print by columns and any
    other value  meaning to print horizontally}
}
\details{

  Default behaviour is to print by rows.  To print by columns, set
  option \code{show_onions_horizontally} to \code{TRUE}:

  \code{options("show_onions_horizontally" = TRUE)}

  Any non-\code{TRUE} value (including \code{NULL} and its being unset)
  will restore the default.

  Similarly, to show onions compactly,  set option
  \code{show_onions_compactly} to \code{TRUE}:
  
  \code{options("show_onions_compactly" = TRUE)}

  This option works best for simple onions with integer entries (or at
  least values with few decimal places), and especially if there are
  many zero entries.

  Function \code{onion_show()} is a helper function, not really intended
  for the end-user.

  The \dQuote{names} of the components of an onion (viz \code{Re},
  \code{i}, \code{j}, \code{k} for quaternions and \code{Re}, \code{i},
  \code{j}, \code{k}, \code{l},\code{il},\code{jl},\code{kl} for
  octonions) are given by function \code{comp_names()} which takes
  either a character string or an onion.
  
}
  
\author{Robin K. S. Hankin}
\note{The print method for \code{onionmat} objects is also sensitive to
  these options.}
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
