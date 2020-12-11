test_that("Test suite aah.R, functionality in onion.R",{


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


for(i in 1:2){
  checker1(rquat(3))
  checker1(roct(3))
}  

} )
