test_that("Test suite aar.R, bind()",{

  expect_true(all(dim(rbind(roct(3)))==c(1,3)))
  expect_true(all(dim(cbind(roct(3)))==c(3,1)))

  expect_true(all(dim(rbind(onionmat(roct(6),2,3)))==2:3))
  expect_true(all(dim(cbind(onionmat(roct(6),2,3)))==2:3))

  expect_true(all(dim(rbind(roct(3),roct(3)))==2:3))
  expect_true(all(dim(cbind(roct(3),roct(3)))==3:2))

  expect_true(all(dim(rbind(roct(3),roct(1)))==2:3))
  expect_true(all(dim(cbind(roct(3),roct(1)))==3:2))

  expect_true(all(dim(rbind(roct(3),3))==2:3))
  expect_true(all(dim(cbind(roct(3),3))==3:2))

  expect_true(all(dim(rbind(onionmat(Oall,2,3),onionmat(Oall,2,3)))==c(4,3)))
  expect_true(all(dim(cbind(onionmat(Oall,2,3),onionmat(Oall,2,3)))==c(2,6)))

  expect_true(all(dim(rbind(onionmat(Oall,2,3),1))==c(3,3)))
  expect_true(all(dim(cbind(onionmat(Oall,2,3),1))==c(2,4)))

  expect_true(all(dim(rbind(onionmat(Oall,2,3),Oim))==c(3,3)))
  expect_true(all(dim(cbind(onionmat(Oall,2,3),Oim))==c(2,4)))

  expect_true(all(dim(rbind(1,onionmat(Oall,2,3)))==c(3,3)))
  expect_true(all(dim(cbind(1,onionmat(Oall,2,3)))==c(2,4)))

  expect_true(all(dim(rbind(1,roct(3)))==c(2,3)))
  expect_true(all(dim(cbind(1,roct(3)))==c(3,2)))

  expect_true(all(dim(rbind(diag(3),roct(3)))==c(4,3)))
  expect_true(all(dim(cbind(diag(3),roct(3)))==c(3,4)))

  expect_true(all(dim(rbind(roct(3),diag(3)))==c(4,3)))
  expect_true(all(dim(cbind(roct(3),diag(3)))==c(3,4)))

  expect_true(all(dim(rbind(onionmat(roct(9),3,3),roct(3)))==c(4,3)))
  expect_true(all(dim(cbind(onionmat(roct(9),3,3),roct(3)))==c(3,4)))

  expect_true(all(dim(rbind(roct(3),onionmat(roct(9),3,3)))==c(4,3)))
  expect_true(all(dim(cbind(roct(3),onionmat(roct(9),3,3)))==c(3,4)))


  expect_true(all(dim(rbind(Oim,onionmat(Oall,2,3)))==c(3,3)))
  expect_true(all(dim(cbind(Oim,onionmat(Oall,2,3)))==c(2,4)))

  expect_true(all(dim(rbind(Oim,matrix(1,2,3)))==c(3,3)))
  expect_true(all(dim(cbind(Oim,matrix(1,2,3)))==c(2,4)))


  expect_true(all(dim(rbind(onionmat(Oim,3,3),diag(3)))==c(6,3)))
  expect_true(all(dim(cbind(onionmat(Oim,3,3),diag(3)))==c(3,6)))

  expect_true(all(dim(rbind(diag(3),onionmat(Oim,3,3)))==c(6,3)))
  expect_true(all(dim(cbind(diag(3),onionmat(Oim,3,3)))==c(3,6)))

  expect_true(all(Re(rbind(diag(2),romat("q",2,2)))[1:2,1:2]==diag(2)))
  expect_true(all(Re(cbind(diag(2),romat("q",2,2)))[1:2,1:2]==diag(2)))

  expect_true(all(Re(rbind(romat("q",2,2),diag(2)))[3:4,1:2]==diag(2)))
  expect_true(all(Re(cbind(romat("q",2,2),diag(2)))[1:2,3:4]==diag(2)))


  expect_true(all(rbind(romat("q",2,3),Him)[3,]==Him))
  expect_true(all(cbind(romat("q",3,2),Him)[,3]==Him))

  


})





