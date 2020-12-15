test_that("Test suite aal.R, functionality in math.R",{


testequal <- function(x,y){testzero(x-y)}
testzero <- function(x, TOL= 1e-8){expect_true(max(Mod(x))<TOL)}

n <- 4  # number of times to check
checker1  <- function(a){
    stopifnot(length(a)==1)
    testequal(a,cumsum(a))
    testequal(a,cumprod(a))
}


checker1a <- function(a){
  testequal(a,asin(sin(a)))
  testequal(a,atan(tan(a)))
  testzero(Mod(acos(cos(a))/a)-1)

  testequal(a,asinh(sinh(a)))
  testequal(a,atanh(tanh(a)))
  testzero(Mod(acosh(cosh(a))/a)-1)

  testzero(Mod(sign(a))-1)


  testequal(log(a,base=exp(1)),log(a))
  }


checker3  <- function(a){
    stopifnot(length(a) == 3)
    testequal(c(a[1],a[1]*a[2],(a[1]*a[2])*a[3]),cumprod(a))
    testequal(c(a[1],a[1]+a[2],(a[1]+a[2])+a[3]), cumsum(a))
    testequal(a[1]+a[2]+a[3],sum(a))

    if(is.quaternion(a)){testequal(a[1]*a[2]*a[3],prod(a))}
    if(is.octonion(a)){expect_error(prod(a))}
    expect_error(digamma(a))
    expect_error(digamma(as.onionmat(a)))
    expect_error(Arg(a))
    expect_error(Arg(as.onionmat(a)))
    expect_error(Complex(a))
    expect_error(Complex(as.onionmat(a)))
    testequal(Norm(a),Mod(a)^2)
    testequal(Norm(as.onionmat(a)),Mod(as.onionmat(a))^2)

} # checker3() closes


jj <- romat()
testequal(jj,Conj(Conj(jj)))
expect_true(all(Mod(jj) >= 0))


for(i in seq_len(n)){
    checker1(rquat(1))
    checker1(roct(1))

    checker1a(rquat(3)/31)
    checker1a(roct(3)/31)
    checker3(rquat(3))
    checker3(roct(3))
    

}
})
