test_that("Test suite aaq.R, numeric matrix <op> onion",{

    expect_true((Him + diag(5))[1,1] == Hall)
    expect_true((Oim + diag(5))[1,1] == Oall)
    expect_true((diag(4) + Him)[1,1] == Hall)
    expect_true((diag(4) + Oim)[1,1] == Oall)


    expect_true((Him * diag(5))[1,1] == Him)
    expect_true((Oim * diag(5))[1,1] == Oim)
    expect_true((diag(4) * Him)[1,1] == Him)
    expect_true((diag(4) * Oim)[1,1] == Oim)

    expect_true(valid_quaternion(rquat()))
    expect_true(valid_octonion(roct()))


    expect_true(all(onionmat(Hall,2,3)[1]==Hall))
    expect_true(all(onionmat(Hall,2,3)[1,drop=TRUE]==Hall))
    expect_true(all(onionmat(Hall,2,3)[1,drop=FALSE]==Hall))
    expect_error(all(onionmat(Hall,2,3)[1,,,drop=FALSE]==Hall))
    expect_true(all(onionmat(Hall,2,3)[1,]==Hall))
    expect_error(onionmat(Hall,2,3)[1,,]==Hall)


    expect_silent(A <- onionmat(Hall,2,3))
    expect_silent(A[1] <- Him)
    expect_true(A[1] == Him)
    
    expect_silent(A[1,] <- Him)
    expect_true(all(A[1,] == Him))
    expect_error(A[1,,] <- Him)

    expect_silent(A[1,] <- 5)
    expect_true(all(A[1,] ==5))
    expect_error(A[1,,] <- 5)

    expect_error(A[1,,] <- 3)
    
    expect_silent(A[1] <- 3)
    expect_true(all(A[1] == 3))


    
})
