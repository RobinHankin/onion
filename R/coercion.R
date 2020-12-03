`quaternion_to_octonion` <- function(from){
  stopifnot(is.quaternion(from))
  as.octonion(rbind(as.matrix(from),matrix(0,4,length(from))))
}

`octonion_to_quaternion` <- function(from){
  stopifnot(is.octonion(from))
  as.quaternion(as.matrix(from)[1:4,,drop=FALSE])
}

setAs("quaternion","octonion",quaternion_to_octonion)
setAs("octonion","quaternion",octonion_to_quaternion)
