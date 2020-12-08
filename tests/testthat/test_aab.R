test_that("Test suite aab.R",{


test <- function(x, TOL= 1e-10){
  stopifnot(Mod(x)<TOL)
  return(TRUE)
}

checker1 <- function(a){
  expect_true(test(a-a))
  expect_true(test(a + (-1)*a))
  expect_true(test((-1)*a + a))
  expect_true(test( (a+a  )-2*a))
  expect_true(test( (a+a  )-a*2))
  expect_true(test( (a+a+a)-3*a))
  expect_true(test( (a+a+a)-a*3))
  expect_true(test(a+1-a-1))
  expect_true(test(a+a[1]-a-a[1]))
  expect_true(test(a/a - 1))
  expect_true(test(a^2/a - a))
  expect_true(test(a^3/a^2 - a))
  expect_true(test(a^4/a^2 - a^2))
  expect_true(test( (a+a)/a - 2))
  expect_true(test( (a+a)/(a*a) - 2/a))
  expect_true(test(a*a       - a^2))
  expect_true(test(a*a*a     - a^3)) # octonions are *power* associative)
  expect_true(test(a*a*a*a   - a^4))
  expect_true(test(1/a - a^(-1)))
  expect_true(test(1/a^2 - a^(-2)))
  expect_true(test(1/(a^2) - (1/a)^2))
  expect_true(test(1/(a^3) - (1/a)^3))
  expect_true(test( (a/a[1])*a[1] - a))
}  

for(i in 1:10){
  checker1(rquat(1))
  checker1(rquat(9))
  checker1(roct(1))
  checker1(roct(9))

}

checker3_assoc <- function(x,y,z){expect_true(test(associator(x,y,z)))}
checker3_dist <- function(x,y,z){expect_true(test(x*(y+z) - (x*y+x*z)))}

for(i in 1:10){
  checker3_assoc(rquat(1),rquat(3),rquat(1))
  checker3_assoc(rquat(3),rquat(3),rquat(1))
  checker3_assoc(rquat(3),rquat(1),rquat(1))
  checker3_dist (rquat(1),rquat(3),rquat(1))
  checker3_dist (rquat(3),rquat(3),rquat(1))
  checker3_dist (rquat(3),rquat(1),rquat(1))

  checker3_dist (roct(1),roct(3),roct(1))
  checker3_dist (roct(3),roct(3),roct(1))
  checker3_dist (roct(3),roct(1),roct(1))
}


})
