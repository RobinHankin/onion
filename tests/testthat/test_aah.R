test_that("Test suite aah.R, functionality in onion.R and S4.R",{


testequal <- function(x,y){testzero(x-y)}
testzero <- function(x, TOL= 1e-8){expect_true(max(Mod(x))<TOL)}

n <- 4  # number of times to check
checker1 <- function(a){

    testequal(rep(a,4), c(a,a,a,a))

    testequal(a %>.<% a,0)   # outer product
    testequal(a %>*<% a,0)   # outer product
    testequal(a  %.%  a, Norm(a))

    testequal(Mod(Im(a %<.>% a)),0)
    testequal(Mod(a %<.>% a),Norm(a))
    testequal(Mod(a %<*>% a),Norm(a))

    
}


expect_silent(p3d(bunny[1:10,]))

for(i in 1:2){
  checker1(rquat(3))
  checker1(roct(3))

  expect_true(is.double(as.double(rquat(3))))
  expect_true(is.double(as.double(roct(3))))

  a <- roct(3)
  length(a) <- 2
  expect_true(length(a) == 2)

  a <- roct(3)
  length(a) <- 5
  expect_true(length(a) == 5)

  expect_true(biggest(runif(2),rquat(),roct())==class( roct()))
  expect_true(biggest(runif(2),rquat()       )==class(rquat()))
  expect_true(biggest(runif(2)               )=="scalar"      )

}  

} )
