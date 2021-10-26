#' @description Cross product of two 3D vectors.
#' @noRd
.crossProduct <- function(v, w){
  c(
    v[2L] * w[3L] - v[3L] * w[2L],
    v[3L] * w[1L] - v[1L] * w[3L],
    v[1L] * w[2L] - v[2L] * w[1L]
  )
}

#' @title Quaternion between two vectors
#' @description Get a unit quaternion whose corresponding rotation sends
#'   \code{u} to \code{v}; the vectors \code{u} and \code{v} must be normalized.
#' @param u,v two unit 3D vectors
#' @return A unit quaternion whose corresponding rotation transforms \code{u}
#'   to \code{v}.
#' @export
#' @examples
#' u <- c(1, 1, 1) / sqrt(3)
#' v <- c(1, 0, 0)
#' q <- quaternionFromTo(u, v)
#' rotate(rbind(u), q) # this should be v
quaternionFromTo <- function(u, v){
  re <- sqrt((1 + sum(u*v))/2)
  w <- .crossProduct(u, v) / 2 / re
  as.quaternion(c(re, w), single = TRUE)
}
