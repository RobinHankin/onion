`rquat` <- function(n=5){ as.quaternion(matrix(rnorm(n*4),4,n))}
`roct` <- function(n=5){ as.octonion(matrix(rnorm(n*8),8,n))}

`rsquat` <- function(n=11,s=12){ as.quaternion(matrix(sample(c(1:2,-(1:2),rep(0,s)),n*4,replace=TRUE),nrow=4))}
`rsoct`  <- function(n=11,s=12){ as.octonion  (matrix(sample(c(1:2,-(1:2),rep(0,s)),n*8,replace=TRUE),nrow=8))}





