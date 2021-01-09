test_that("Test suite aak.R, coercion between quat and oct",{


testequal <- function(x,y){testzero(x-y)}
testzero <- function(x, TOL= 1e-8){expect_true(max(Mod(x))<TOL)}

n <- 4  # number of times to check
checker_oct  <- function(a){testequal(octonion_to_quaternion(quaternion_to_octonion(octonion_to_quaternion(a))), octonion_to_quaternion(a))}

checker_quat  <- function(a){testequal(octonion_to_quaternion(quaternion_to_octonion(a)),a)}

for(i in seq_len(n)){
    checker_quat(rquat(3))
    checker_oct(roct(3))
}
})
