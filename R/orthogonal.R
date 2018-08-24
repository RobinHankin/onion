#' @details http://www.euclideanspace.com/maths/geometry/rotations/conversions/quaternionToMatrix/index.htm
#' @details http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/index.htm


`as.orthogonal` <- function(q){  # converts a quaternion to an orthogonal matrix
  s <- 1/Norm(q)
  r <- Re(q)
  i <- i(q)
  j <- j(q)
  k <- k(q)
  
  drop(array(matrix(c(
      1-2*s*(j^2+k^2)  ,   2*s*(i*j-k*r) ,   2*s*(i*k+j*r),
        2*s*(i*j+k*r)  , 1-2*s*(i^2+k^2) ,   2*s*(j*k-i*r),
        2*s*(i*k-j*r)  ,   2*s*(j*k+i*r) , 1-2*s*(i^2+j^2)
  ),nrow=9,byrow=TRUE),c(3,3,length(q))))

}

`is_orthogonal` <- function(M,tol=1e-7){ max(abs(crossprod(M)-diag(3))) < tol }

`matrix2quaternion` <- function(M){  # converts an orthogonal matrix to a quaternion
  if(!is_orthogonal(M)){stop("matrix not orthogonal")}

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
