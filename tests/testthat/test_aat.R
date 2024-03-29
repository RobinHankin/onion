test_that("Test suite aat.R",{
    x <- sample(1:10)
    expect_true(all(drop(as.quaternion(x)) == x))
    expect_true(all(drop(as.octonion  (x)) == x))

})
