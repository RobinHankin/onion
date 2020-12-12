test_that("Test suite aai.R, functionality in orthogonal.R",{


testequal <- function(x,y){testzero(x-y)}
testzero <- function(x, TOL= 1e-8){expect_true(max(Mod(x))<TOL)}

n <- 4  # number of times to check

checkerA <- function(a){testequal(cprod(as.orthogonal(a)),diag(nrow=3))}
checkerB <- function(Q){
    Q <- Q/abs(Q)
    LHS <-Q
    RHS <- as.quaternion(as.orthogonal(Q))
    testzero(abs(LHS/RHS)-1)   # LHS = +/- RHS
}
for(i in seq_len(n)){
    checkerA(rquat(1))
    checkerB(rquat(7))
}

})
