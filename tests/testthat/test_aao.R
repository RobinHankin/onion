test_that("Test suite aao.R, functionality in onionmat_components.R",{


testequal <- function(x,y){testzero(x-y)}
testzero <- function(x, TOL= 1e-8){expect_true(max(Mod(x))<TOL)}

n <- 4  # number of times to check


checker1 <- function(a){

expect_true(TRUE)
  


}


for(i in seq_len(n)){
  checker1(onionmat(rquat(28),4,7))
  checker1(onionmat(roct(28),4,7))

  
}

} )

