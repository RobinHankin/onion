test_that("Test suite aau.R, ad",{


n <- 4  # number of times to check

checker2  <- function(x,y){ expect_true(all(ad(x)(y) == x*y-y*x)) }

for(i in seq_len(n)){
    checker2(rsquat(3),rsquat(3))
    checker2(rsoct(3),rsoct(3))
}

})
