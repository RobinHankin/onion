test_that("Test suite aas.R",{
  a <- matrix(as.quaternion(matrix(1:24,nrow=4)),2,3)
  expect_true(all(round(a*1.001) == a))
  expect_true(all(round(a*0.999) == a))

  a <- matrix(as.octonion(matrix(1:48,nrow=8)),2,3)
  expect_true(all(round(a*1.001) == a))
  expect_true(all(round(a*0.999) == a))

})
