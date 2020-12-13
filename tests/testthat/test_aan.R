test_that("Test suite aan.R, functionality in matrix.R",{


testequal <- function(x,y){testzero(x-y)}
testzero <- function(x, TOL= 1e-8){expect_true(max(Mod(x))<TOL)}

n <- 4  # number of times to check


checker <- function(a){
  stopifnot(length(a) == 12)

  expect_true(is.onionmat(onionmat(a,3,4)))
  expect_true(is.onionmat(as.onionmat(a)))
  expect_true(is.onionmat(as.onionmat(as.onionmat(a))))
  
  expect_true(all(t(onionmat(a,3,4)) == onionmat(a,4,3,byrow=TRUE)))
  expect_error(as.onionmat("fish"))

  jj <- onionmat(a,3,4)
  testequal(sum(jj),sum(jj[1,]+jj[2,]+jj[3,]))
  testequal(sum(jj),sum(jj[,1]+jj[,2]+jj[,3]+jj[,4]))
  
}



for(i in seq_len(n)){
  checker(rquat(12))
  checker(roct(12))
}

})
