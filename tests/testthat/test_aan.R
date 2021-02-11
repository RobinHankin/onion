test_that("Test suite aan.R, functionality in matrix.R",{


testequal <- function(x,y){testzero(x-y)}
testzero <- function(x, TOL= 1e-8){expect_true(max(Mod(x))<TOL)}

n <- 2  # number of times to check


checker1 <- function(a){
  stopifnot(is.onionmat(a))
  stopifnot(nrow(a) == 3)
  stopifnot(ncol(a) == 4)
  
  expect_true(is.onionmat(a))
  expect_true(is.onionmat(as.onionmat(a)))
  expect_true(is.onionmat(as.onionmat(as.onionmat(a))))
  expect_true(is.onionmat(as.onionmat(getd(a))))
  expect_true(all(t(a) == onionmat(getd(a),4,3,byrow=TRUE)))

  testequal(sum(a),sum(a[1,]+a[2,]+a[3,]))
  testequal(sum(a),sum(a[,1]+a[,2]+a[,3]+a[,4]))

  expect_error(a^a)
  expect_error(2^a)
  testequal(a,1/(1/a))
  testequal(a/a,1)
  testequal(1-a,-a+1)
  testequal(a*5,a/0.2)

  testequal(a, -(-a))
  testequal(a, +(+a))

  expect_error(a %% a)
  expect_error(1 %% a)
  expect_error(a %% 1)

  testequal(cprod(a),ht(a) %*% a)
  testequal(tcprod(a),a %*% ht(a))
  testequal(cprod(a),cprod(a,a))

  testequal(cprod(a),ht(cprod(a)))
  testequal(tcprod(a),ht(tcprod(a)))

  
  testequal(a[1,],c(a[1,1],a[1,2],a[1,3],a[1,4]))
  testequal(a[,1],c(a[1,1],a[2,1],a[3,1]))
  testequal(a,a[])
  expect_error((a[1,,drop=TRUE])[1,1])

  I <- cbind(1:3,2:4)
  testequal(a[I],c(a[1,2],a[2,3],a[3,4]))

  expect_error(a[I,3])

  jj <- cprod(a)
  testzero(Im(jj+t(jj)))

} # checker1() closes

checkerv <- function(v){ # v an onion

  testequal(cprod(v)  , ht(v) %*% v)
  testequal(cprod(v,v), ht(v) %*% v)

  testequal(tcprod(v)  , ht(ht(v)) %*%   ht(v))
  testequal(tcprod(v,v), ht(ht(v)) %*%   ht(v))
}


checker1replace <- function(a){
  stopifnot(is.onionmat(a))
  stopifnot(all(dim(a) == c(3,4)))
  jj <- a
  jj[1,] <- jj[2,]
  testequal(jj[1,],jj[2,])

  jj <- a
  jj[,1] <- jj[,2]
  testequal(jj[,1],jj[,2])

  jj <- a
  jj[1,] <- Re(jj[2,])
  testequal(jj[1,],Re(jj[2,]))

  jj <- a
  jj[,1] <- Re(jj[,2])
  testequal(jj[,1],Re(jj[,2]))

  I <- cbind(1:3,2:4)
  jj <- a
  jj[I] <- jj[1,1]
  testequal(jj[I],jj[1,1])

  jj <- a
  jj[I] <- Re(jj[1,1])
  testequal(jj[I],Re(jj[1,1]))
}



checker1x <- function(a,x){
  stopifnot(is.onionmat(a))
  stopifnot(is.onion(x) | is.numeric(x))
  stopifnot(length(x) == 1)

  testequal(x+a, x-(-a))
  testequal(x*a, x/(1/a))

  testequal(a+x, a-(-x))
  testequal(a*x, a/(1/x))
}

checker1v <- function(a,v){  # v is a onion vector
  stopifnot(is.onionmat(a))
  stopifnot(is.onion(v)|is.numeric(v))
  stopifnot(ncol(a) == length(v))


  testequal( (a%*%v)[1,1], sum(a[1,]*v))
  testequal( (a%*%Re(v))[1,1], sum(a[1,]*Re(v)))
}

checker1va <- function(v,a){
  stopifnot(is.onionmat(a))
  stopifnot(is.onion(v)|is.numeric(v))
  stopifnot(nrow(a) == length(v))

  testequal( (v%*%a)[1,1], sum(v*a[,1]))
}


checker2 <- function(a,b){
  stopifnot(all(dim(a) == c(4,7)))
  stopifnot(all(dim(b) == c(7,5)))

  expect_true(all(dim(a %*% b) == c(4,5)))

  testequal((a %*% b)[1,1] , sum(a[1,]*b[,1]))
}

  f <- function(...){3}
  expect_error(as.onionmat(f))
  expect_error(cprod(f,f))
  expect_error(tcprod(f,f))
  expect_error(cprod(f))
  expect_error(tcprod(f))
  expect_error(cprod(rquat(),f))
  expect_error(tcprod(rquat(),f))
  expect_error(cprod(f,rquat()))
  expect_error(tcprod(f,rquat()))

  expect_error(cprod(romat(),f))
  expect_error(tcprod(romat(),f))
  expect_error(cprod(f,romat()))
  expect_error(tcprod(f,romat()))

  expect_error(tcprod(romat(),rquat(99)))
  expect_error(cprod(romat(),rquat(88)))

  expect_true(all(dim(cprod(rquat()))==1))
  expect_true(all(dim(tcprod(rquat(5)))==5))

  expect_true(all(dim(cprod(rquat(2),onionmat(rquat(10),2,5)))==c(1,5)))
  expect_true(diff(dim(tcprod(romat(),romat())))==0)

  expect_error(tcprod(rquat(),romat()))
  
  jj <- cprod(herm_onion_mat(1:3,roct(3)))
  testzero(Im(jj + t(jj)))
      

for(i in seq_len(n)){
  checker1(onionmat(rquat(12),3,4))
  checker1(onionmat(roct(12),3,4))

  checker1replace(onionmat(rquat(12),3,4))
  checker1replace(onionmat(roct(12),3,4))

  checkerv(rquat(5))
  checkerv(roct(5))

  checker1x(onionmat(rquat(28),4,7),rquat(1))
  checker1x(onionmat(roct(28),4,7),roct(1))

  checker1x(onionmat(rquat(28),4,7),runif(1))
  checker1x(onionmat(roct(28),4,7),runif(1))

  checker1v(onionmat(roct(28),4,7),roct(7))
  checker1v(onionmat(roct(28),4,7),runif(7))

  checker1va(roct(4),onionmat(roct(28),4,7))
  checker1va(runif(4),onionmat(roct(28),4,7))

  checker2(onionmat(rquat(28),4,7),onionmat(rquat(35),7,5))

}

})

