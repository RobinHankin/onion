test_that("Test suite aab.R, arithmetic ops on onionmats",{


testequal <- function(x,y){testzero(x-y)}
testzero <- function(x, TOL= 1e-8){expect_true(max(Mod(x))<TOL)}

n <- 4  # number of times to check
checkerA <- function(a){  # following are pointwise operations
  testequal(a,a)
  testzero(a + (-1)*a)
  testzero((-1)*a + a)
  testequal(a+a,2*a)
  testequal(a+a,a*2)
  testequal(a+a+a, 3*a)
  testzero(a+1-a-1)
  testequal(a/2+a/2,a)
  testequal(a*a    , a^2)
  testequal(a*a*a  , a^3) # octonions are *power* associative
  testequal(a*a*a*a, a^4)
}

checkerB <- function(a,x){
  testequal(a*x,x*a)
  testequal(a+x,x+a)
}

checkerC <- function(a,x,y){
  testequal(a*(x+y),a*x + a*y)
  testequal((x+y)*a,a*x + a*y)
  testequal((x+y)*a,x*a + y*a)
}

checkerD <- function(a,b){
  testequal( (a+b) -(b+a))
  testequal((a/b)*a - b)
}


for(i in 1:10){
  checkerA(romat(3,4))
  checkerA(romat(3,5))
  checkerA(romat(3,4,"octonion"))
  checkerA(romat(3,5,"octonion"))

  checkerB(romat(3,4),rnorm(1))
  checkerB(romat(3,4,"octonion"),rnorm(1))
  checkerB(romat(3,4),rquat(1))
  checkerB(romat(3,4,"octonion"),roct(1))

  checkerC(romat(3,4),runif(1),runif(1))
  checkerC(romat(3,4),runif(1),rquat(1))
  checkerC(romat(3,4),rquat(1),rquat(1))
}  



} )
