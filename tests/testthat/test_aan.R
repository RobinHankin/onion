test_that("Test suite aan.R, functionality in matrix.R",{


testequal <- function(x,y){testzero(x-y)}
testzero <- function(x, TOL= 1e-8){expect_true(max(Mod(x))<TOL)}

n <- 4  # number of times to check


checker <- function(a){
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

  expect_error(a %% a)
  expect_error(1 %% a)
  expect_error(a %% 1)

}

  expect_error(as.onionmat("fish"))



for(i in seq_len(n)){
  checker(onionmat(rquat(12),3,4))
  checker(onionmat(roct(12),3,4))

}

})
