test_that("Test suite aal.R, functionality in math.R",{


testequal <- function(x,y){testzero(x-y)}
testzero <- function(x, TOL= 1e-8){expect_true(max(Mod(x))<TOL)}

n <- 4  # number of times to check
checker1  <- function(a){
    testequal(a,cumsum(a))
    testequal(a,cumprod(a))
}

checker3  <- function(a){
    testequal(c(a[1],a[1]*a[2],(a[1]*a[2])*a[3]),cumprod(a))
    testequal(c(a[1],a[1]+a[2],(a[1]+a[2])+a[3]), cumsum(a))
}


for(i in seq_len(n)){
    checker1(rquat(1))
    checker1(roct(1))
    checker3(rquat(3))
    checker3(roct(3))

}
})
