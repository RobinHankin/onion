test_that("Test suite aad.R",{

  M <- matrix(1:4)
  rownames(M) <- letters[1:4]
  expect_silent(newonionmat(rquat(4),M))
})
