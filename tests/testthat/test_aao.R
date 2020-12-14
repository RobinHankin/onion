test_that("Test suite aao.R, functionality in onionmat_components.R",{


testequal <- function(x,y){testzero(x-y)}
testzero <- function(x, TOL= 1e-8){expect_true(max(Mod(x))<TOL)}

n <- 4  # number of times to check


checker1 <- function(a){
  expect_error(Arg(a))
  expect_true(all(Conj(Conj(a))==a))
  expect_true(all(Mod(a)>=0))
  expect_true(all(Re(Im(a))==0))
}


for(i in seq_len(n)){
  checker1(onionmat(rquat(28),4,7))
  checker1(onionmat(roct(28),4,7))

}

} )

