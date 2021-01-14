test_that("Test suite aaq.R, numeric matrix <op> onion",{

    expect_true((Him + diag(5))[1,1] == Hall)
    expect_true((Oim + diag(5))[1,1] == Oall)
    expect_true((diag(4) + Him)[1,1] == Hall)
    expect_true((diag(4) + Oim)[1,1] == Oall)


    expect_true((Him * diag(5))[1,1] == Him)
    expect_true((Oim * diag(5))[1,1] == Oim)
    expect_true((diag(4) * Him)[1,1] == Him)
    expect_true((diag(4) * Oim)[1,1] == Oim)

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


    expect_true(all(onionmat(Hall,3,3)==Hall))
    expect_true(all(onionmat(Oall,3,3)==Oall))

    

    jj <- c(Oall,O1,Oi,Oj,Ok)
    expect_true(all(diag(diag(jj)) == jj))

    D <- diag(jj)
    expect_true(all(D-14 == -(14-D)))
    expect_true(all(D+14 == -(-14-D)))
    expect_true(all(Oi-D == D*(-1)+Oi))
    expect_true(all(D/1==D))
    expect_true(all(14-D == D*(-1)+14))
    expect_true(all(D-14 == -(14-D)))
    expect_true(all(D/Oi== -D*Oi))

    D <- romat()
    expect_true(max(Mod(D^(-1)-1/D))<1e-10)
    expect_true(max(Mod(1/D - D^(-1)))<1e-10)

    expect_error(1i+diag(2) + Oall)
    expect_error(D^D)
    expect_error(diag(2)^D)
    expect_error(diag(2)^Him)

    D <- romat("q",2,2)
    expect_true(all(D+1 == 1+D))
    expect_true(max(Mod((D+Him) - (D-(-Him)))) < 1e-10)

    expect_true(all(diag(2) + Oim ==  Oim + diag(2)))
    expect_true(all(diag(2) - Oim == -Oim + diag(2)))
    expect_true(all(Oim +diag(2) ==   diag(2) + Oim))
    expect_true(all(Oim -diag(2) ==  -diag(2) + Oim))

    expect_error(D+1i*diag(2))
    expect_error(1i*diag(2)+D)
    expect_error(Oim + 1i*diag(2))
    expect_error(1i*diag(2) + Oim)
    expect_error(diag(2)^D[1,1])
    expect_error(diag(2)%%D[1,1])

    expect_true(Mod((diag(2)/D[1,1])[1,1] - 1/D[1,1]) < 1e-10)

    expect_true(all(Him/matrix(1,2,2) == Him))
    expect_true(all(Mod(Him^matrix(1,2,2) - Him) < 1e-10)) 
    expect_error(Him%%matrix(1,2,2))

    A <- romat()
    Im(A) <- Im(A)*10
    Im(A) <- 0
    expect_true(all(Mod(Im(A))==0))

    A <- matrix(roct(4),2,2)
    expect_true(is.onionmat(A+diag(2)))
    expect_true(is.onionmat(A-diag(2)))
    expect_true(is.onionmat(diag(2)-A))
    expect_true(is.onionmat(diag(2)+A))
                            
    expect_true(is.onionmat(A*diag(2)))
    expect_true(is.onionmat(A/diag(2)))
    expect_true(is.onionmat(diag(2)*A))
    expect_true(is.onionmat(diag(2)/A))

    expect_error(is.onionmat(diag(2)^A))
    expect_true(is.onionmat(A^diag(2)))
    expect_error(A %% diag(2))
    expect_error(diag(2) %% A )

    
})
