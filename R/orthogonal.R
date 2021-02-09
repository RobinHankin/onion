#' @details https://www.euclideanspace.com/maths/geometry/rotations/conversions/quaternionToMatrix/index.htm
#' @details https://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/index.htm


`as.orthogonal` <- function(Q){  # converts a quaternion to an orthogonal matrix
  s <- 1/Norm(Q)
  r <- Re(Q)
  i <- i(Q)
  j <- j(Q)
  k <- k(Q)

  drop(aperm(array(c(
      1-2*s*(j^2+k^2) ,   2*s*(i*j-k*r) ,   2*s*(i*k+j*r),
      2*s*(i*j+k*r)   , 1-2*s*(i^2+k^2) ,   2*s*(j*k-i*r),
      2*s*(i*k-j*r)   ,   2*s*(j*k+i*r) , 1-2*s*(i^2+j^2)
  ),c(length(Q),3,3)), 3:1))
}



`matrix2quaternion` <- function(M){  # converts an orthogonal matrix to a quaternion
  stopifnot(all(abs(crossprod(M)-diag(3)) < 1e-6))

  if (M[1,1]+M[2,2]+M[3,3] > 0) { 
    S <- 2*sqrt(1 + M[1,1] + M[2,2] + M[3,3])
    qw <- S/4
    qx <- (M[3,2] - M[2,3]) / S
    qy <- (M[1,3] - M[3,1]) / S 
    qz <- (M[2,1] - M[1,2]) / S 
  } else if ((M[1,1] > M[2,2]) && (M[1,1] > M[3,3])) { 
    S <- 2*sqrt(1 + M[1,1] - M[2,2] - M[3,3])
    qw <- (M[3,2] - M[2,3]) / S
    qx <- S/4
    qy <- (M[1,2] + M[2,1]) / S 
    qz <- (M[1,3] + M[3,1]) / S
  } else if (M[2,2] > M[3,3]) { 
    S <- 2*sqrt(1 + M[2,2] - M[1,1] - M[3,3])
    qw <- (M[1,3] - M[3,1]) / S
    qx <- (M[1,2] + M[2,1]) / S
    qy <- S/4
    qz <- (M[2,3] + M[3,2]) / S
  } else { 
    S <- 2*sqrt(1 + M[3,3] - M[1,1] - M[2,2])
    qw <- (M[2,1] - M[1,2]) / S
    qx <- (M[1,3] + M[3,1]) / S
    qy <- (M[2,3] + M[3,2]) / S
    qz <- S/4
  }
  return(as.quaternion(c(qw,qx,qy,qz),single=TRUE))
}

