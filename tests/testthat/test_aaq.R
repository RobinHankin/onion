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

    
})
