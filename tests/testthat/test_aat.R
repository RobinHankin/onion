test_that("Test suite aat.R, Kochanek-Bartels() with constant speed", {
  keyRotors <- as.quaternion(
    cbind(
      c(1, 0, 0, 0),
      c(sqrt(2), 0, 0, sqrt(2)) / 2,
      c(0.5, 0.5, -0.5, 0.5)
    )
  )
  spline <- KochanekBartels(
    keyRotors = keyRotors, constantSpeed = TRUE, endcondition = "closed"
  )
  times <- attr(spline, "times")#interpolateTimes(attr(spline, "times"), n = 3L)
  rotors <- spline(head(times, -1L))
  expect_equal(as.matrix(keyRotors), as.matrix(rotors))
})
  