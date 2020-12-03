`rquat` <- function(n=5){ as.quaternion(matrix(rnorm(n*4),4,n))}
`roct` <- function(n=5){ as.octonion(matrix(rnorm(n*8),8,n))}

