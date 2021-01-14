test_that("Test suite aap.R",{
  expect_true(Hi*Hj ==  Hk)
  expect_true(Hj*Hi == -Hk)
  expect_true(Hj*Hk ==  Hi)
  expect_true(Hk*Hj == -Hi)
  expect_true(Hk*Hi ==  Hj)
  expect_true(Hi*Hk == -Hj)

  expect_true(Hi*Hi == -H1)
  expect_true(Hj*Hj == -H1)
  expect_true(Hk*Hk == -H1)

  expect_true(H1*H1 == H1)
  expect_true(H1*Hi == Hi)
  expect_true(H1*Hj == Hj)
  expect_true(H1*Hk == Hk)

  expect_true(H1*H1 == H1)
  expect_true(Hi*H1 == Hi)
  expect_true(Hj*H1 == Hj)
  expect_true(Hk*H1 == Hk)
  
  expect_true(Hi*Hj*Hk == -H1)

  ## Quaternion zero times table:
  expect_true(H0*H1 == H0)
  expect_true(H0*Hi == H0)
  expect_true(H0*Hj == H0)
  expect_true(H0*Hk == H0)

  expect_true(H1*H0 == H0)
  expect_true(Hi*H0 == H0)
  expect_true(Hj*H0 == H0)
  expect_true(Hk*H0 == H0)

  ## And some quaternion additions:
  expect_true(H1 + Him == Hall)
  expect_true(Hi + Hj + Hk == Him)
  expect_true(H1 + Hi + Hj + Hk == Hall)

  ## And some quaternion subtractions:
  expect_true(Hi - Hi == H0)
  expect_true(Hall - Hi - Hj - Hk == H1)
  expect_true(Hall - Him == H1)

  ## Now all 64 of the octonions:
  expect_true(O1*O1  == O1 )
  expect_true(O1*Oi  == Oi )
  expect_true(O1*Oj  == Oj )
  expect_true(O1*Ok  == Ok )
  expect_true(O1*Ol  == Ol )
  expect_true(O1*Oil == Oil)
  expect_true(O1*Ojl == Ojl)
  expect_true(O1*Okl == Okl)
  
  expect_true(Oi*O1  ==  Oi )
  expect_true(Oi*Oi  == -O1 )
  expect_true(Oi*Oj  ==  Ok )
  expect_true(Oi*Ok  == -Oj )
  expect_true(Oi*Ol  ==  Oil)
  expect_true(Oi*Oil == -Ol )
  expect_true(Oi*Ojl == -Okl)
  expect_true(Oi*Okl ==  Ojl)

  expect_true(Oj*O1  ==  Oj )
  expect_true(Oj*Oi  == -Ok )
  expect_true(Oj*Oj  == -O1 )
  expect_true(Oj*Ok  ==  Oi )
  expect_true(Oj*Ol  ==  Ojl)
  expect_true(Oj*Oil ==  Okl)
  expect_true(Oj*Ojl == -Ol )
  expect_true(Oj*Okl == -Oil)

  expect_true(Ok*O1  ==  Ok )
  expect_true(Ok*Oi  ==  Oj )
  expect_true(Ok*Oj  == -Oi )
  expect_true(Ok*Ok  == -O1 )
  expect_true(Ok*Ol  ==  Okl)
  expect_true(Ok*Oil == -Ojl)
  expect_true(Ok*Ojl ==  Oil)
  expect_true(Ok*Okl == -Ol )

  expect_true(Ol*O1  ==  Ol )
  expect_true(Ol*Oi  == -Oil)
  expect_true(Ol*Oj  == -Ojl)
  expect_true(Ol*Ok  == -Okl)
  expect_true(Ol*Ol  == -O1 )
  expect_true(Ol*Oil ==  Oi )
  expect_true(Ol*Ojl ==  Oj )
  expect_true(Ol*Okl ==  Ok )

  expect_true(Oil*O1  ==  Oil)
  expect_true(Oil*Oi  ==  Ol )
  expect_true(Oil*Oj  == -Okl)
  expect_true(Oil*Ok  ==  Ojl)
  expect_true(Oil*Ol  == -Oi )
  expect_true(Oil*Oil == -O1 )
  expect_true(Oil*Ojl == -Ok )
  expect_true(Oil*Okl ==  Oj )

  expect_true(Ojl*O1  ==  Ojl)
  expect_true(Ojl*Oi  ==  Okl)
  expect_true(Ojl*Oj  ==  Ol )
  expect_true(Ojl*Ok  == -Oil)
  expect_true(Ojl*Ol  == -Oj )
  expect_true(Ojl*Oil ==  Ok )
  expect_true(Ojl*Ojl == -O1 )
  expect_true(Ojl*Okl == -Oi )

  expect_true(Okl*O1  ==  Okl)
  expect_true(Okl*Oi  == -Ojl)
  expect_true(Okl*Oj  ==  Oil)
  expect_true(Okl*Ok  ==  Ol )
  expect_true(Okl*Ol  == -Ok )
  expect_true(Okl*Oil == -Oj )
  expect_true(Okl*Ojl ==  Oi )
  expect_true(Okl*Okl == -O1 )



  ## And the zero octonion times table:
  expect_true(O0*O0  == O0)

  expect_true(O0*O1  == O0)
  expect_true(O0*Oi  == O0)
  expect_true(O0*Oj  == O0)
  expect_true(O0*Ok  == O0)
  expect_true(O0*Ol  == O0)
  expect_true(O0*Oil == O0)
  expect_true(O0*Ojl == O0)
  expect_true(O0*Okl == O0)

  expect_true(O1*O0  == O0)
  expect_true(Oi*O0  == O0)
  expect_true(Oj*O0  == O0)
  expect_true(Ok*O0  == O0)
  expect_true(Ol*O0  == O0)
  expect_true(Oil*O0 == O0)
  expect_true(Ojl*O0 == O0)
  expect_true(Okl*O0 == O0)

  ## And some octonion additions:
  expect_true(O1 + Oim == Oall)
  expect_true(Oi + Oj + Ok + Ol + Oil + Ojl + Okl == Oim)
  expect_true(O1 + Oi + Oj + Ok + Ol + Oil + Ojl + Okl == Oall)

  ## And some subtractions:
  expect_true(Oil - Oil == O0)
  expect_true(Oall - Oim == O1)

  ## some tests of R/creation.R:

  expect_true(as.onion(1:4,single=TRUE)==1*H1 + 2*Hi + 3*Hj + 4*Hk)
  expect_true(as.onion(1:8,single=TRUE)==1*O1 + 2*Oi + 3*Oj + 4*Ok + 5*Ol + 6*Oil + 7*Ojl + 8*Okl)
  expect_error(as.onion(matrix(1:25,5,5)))

  expect_true(is.quaternion(as.quaternion(rquat(4))))
  expect_true(is.octonion(as.octonion(roct(4))))

  expect_false(is.octonion(as.quaternion(rquat(4))))
  expect_false(is.quaternion(as.octonion(roct(4))))

  expect_true(is.quaternion(as.onion(1:4,matrix(rquat(1),2,2))))
  expect_false(is.quaternion(as.onion(1:4,matrix(roct(1),2,2))))
  expect_false(is.octonion(as.onion(1:4,matrix(rquat(1),2,2))))
  expect_true(is.octonion(as.onion(1:4,matrix(roct(1),2,2))))

  expect_true(is.onion(drop(matrix(Hi,1,3))))
  expect_false(is.onionmat(drop(matrix(Hi,1,3))))

  o <- as.onion(c(1,1e-20,1e-20,1e-20),single=TRUE)
  expect_false(Im(o)==0)
  expect_true(Im(zapsmall(o))==0)

  o <- onionmat(o,2,2)
  expect_false(all(Im(o)==0))
  expect_true(all(Im(zapsmall(o))==0))

  
})
