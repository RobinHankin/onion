test_that("Test suite aaj.R, functionality in summary.R",{

n <- 4  # number of times to check
checker <- function(a){
    expect_output(str(a))
    expect_output(str(a))
    expect_silent(condense(a))
    expect_silent(condense(a))
}

for(i in seq_len(n)){
    checker(rquat(3))
    checker(roct(3))
    checker(rquat(13))
    checker(roct(13))
    a <- rquat(3)
    names(a) <- letters[1:3]
    checker(a)
    a <- roct(3)
    names(a) <- letters[1:3]
    checker(a)
}

})
