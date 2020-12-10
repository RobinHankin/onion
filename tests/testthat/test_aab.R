test_that("Test suite aab.R, arithmetic ops on onions, e.g. roct() and rquat()",{


testequal <- function(x,y){testzero(x-y)}
testzero <- function(x, TOL= 1e-8){expect_true(max(Mod(x))<TOL)}

n <- 4  # number of times to check
checkerA <- function(a){
  testequal(a,a)
  testequal(a,+a)
  testzero(a + (-1)*a)
  testzero((-1)*a + a)
  testequal(a+a,2*a)
  testequal(a+a,a*2)
  testequal(a+a+a, 3*a)
  testzero(a+1-a-1)
  testequal(a/2+a/2,a)

  testequal(a/a,1)
  testequal(1/a, a^(-1))
  testequal(1/a^2, a^(-2))
  testequal(a/a^2, a^(-1))  

  testequal(a*a    , a^2)
  testequal(a*a*a  , a^3) # octonions are *power* associative
  testequal(a*a*a*a, a^4)

  testequal(onion_power_singleinteger(a,0),1)
  testequal(onion_power_singleinteger(a,1),a)
  testequal(onion_power_singleinteger(a,2),a^2)
  testequal(onion_power_singleinteger(a,3),a^3)

  testequal(onion_power_singleinteger(a,-1),a^(-1))
  testequal(onion_power_singleinteger(a,-2),a^(-2))

  expect_error(a^a)
  expect_error(1^a)
  expect_error(a%%1)
  expect_error(1%%a)
  expect_error(a%%a)

  expect_error(a & a)
  expect_error(a & H1)
  expect_error(H1 & a)
  expect_error(a & 1)
  expect_error(1 & a)


}

checkerB <- function(a,x){
  testequal(a*x,x*a)
  testequal(a+x,x+a)
  testequal(a-x,-x+a)
  testequal(x-a,-a+x)
}

checkerC <- function(a,x,y){
  testequal(a*(x+y),a*x + a*y)
  testequal((x+y)*a,a*x + a*y)
  testequal((x+y)*a,x*a + y*a)
}

checkerD <- function(a,b){
  testequal(a+b,b+a)

  
}


for(i in 1:2){
  checkerA(rquat(3))
  checkerA(roct(3))
  checkerB(rquat(3),rnorm(1))
  checkerB(rquat(3),rnorm(3))
  checkerB(roct(3),rnorm(1))
  checkerB(roct(3),rnorm(3))
  checkerC(rquat(3),runif(3),runif(1))
  checkerC(roct(3),runif(3),runif(1))

  checkerD(rquat(3),rquat(3))
  checkerD(roct(3),roct(3))

}  



} )
