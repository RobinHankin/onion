test_that("Test suite aad.R",{

  M <- matrix(1:4)
  rownames(M) <- letters[1:4]
  expect_silent(newonionmat(rquat(4),M))

  a <- onionmat(roct(24),3,8)
  dimnames(a) <- list(fish=letters[1:3],chips=LETTERS[1:8])
  expect_true(identical(dimnames(a), list(fish=letters[1:3],chips=LETTERS[1:8])))
  expect_output(print(dimnames(a)))

  rownames(a) <- LETTERS[1:3]
  colnames(a) <- letters[1:8]

  expect_true(all(rownames(a) == LETTERS[1:3]))
  expect_true(all(colnames(a) == letters[1:8]))

  expect_true(identical(names(a),NULL))
  expect_error(names(a) <- letters[1:3])

  jj <- rquat(6)
  names(jj) <- letters[1:6]
})
