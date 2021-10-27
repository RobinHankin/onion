test_that("Test suite aas.R, BarryGoldman()", {
  keyRotors <- as.quaternion(
    cbind(
      c(1, 0, 0, 0),
      c(sqrt(2), 0, 0, sqrt(2)) / 2,
      c(0.5, 0.5, -0.5, 0.5)
    )
  )
  rotors <- BarryGoldman(keyRotors = keyRotors, n_intertimes = 2L)
  expect_equal(as.matrix(keyRotors), as.matrix(rotors[c(1L, 3L, 5L)]))
})
  