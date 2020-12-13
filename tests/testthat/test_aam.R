test_that("Test suite aam.R, functionality in compare.R",{


testequal <- function(x,y){testzero(x-y)}
testzero <- function(x, TOL= 1e-8){expect_true(max(Mod(x))<TOL)}

n <- 4  # number of times to check


checker <- function(a){
  expect_error(a>a)
  expect_error(a<a)

  expect_error(a>=a)
  expect_error(a<=a)

  expect_error(a>1)
  expect_error(a<1)
  expect_error(1>a)
  expect_error(1<a)

  expect_false(any(a == a+1))
  expect_true (all(a != a+1))

  expect_false(any(a==1e55))
  expect_true (all(a!=1e55))
  expect_false(any(1e55 == a))
  expect_true (all(1e55 != a))

}


  expect_true (1==as.quaternion(1))
  expect_false(1!=as.quaternion(1))
  expect_true (1==as.octonion(1))
  expect_false(1!=as.octonion(1))
  expect_true (1==as.quaternion(1))
  expect_false(1!=as.quaternion(1))
  expect_true (1==as.octonion(1))
  expect_false(1!=as.octonion(1))


for(i in seq_len(n)){
  checker(rquat(3))
  checker(roct(3))
  checker(romat())
}
})
