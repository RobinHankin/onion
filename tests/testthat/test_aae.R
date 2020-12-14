test_that("Test suite aae.R, onion and onionmat extract and replace methods, eg i() and i<-()",{
  
  checker_re <- function(a,x){Re(a) <- x ; expect_true(all(Re(a)==x))}
  checker_i  <- function(a,x){ i(a) <- x ; expect_true(all( i(a)==x))}
  checker_j  <- function(a,x){ j(a) <- x ; expect_true(all( j(a)==x))}
  checker_k  <- function(a,x){ k(a) <- x ; expect_true(all( k(a)==x))}
  checker_l  <- function(a,x){ l(a) <- x ; expect_true(all( l(a)==x))}
  checker_il <- function(a,x){il(a) <- x ; expect_true(all(il(a)==x))}
  checker_jl <- function(a,x){jl(a) <- x ; expect_true(all(jl(a)==x))}
  checker_kl <- function(a,x){kl(a) <- x ; expect_true(all(kl(a)==x))}

  checker_extract_onion <- function(a,x){a[1] <- x ; expect_true(a[1] == x)}
  checker_extract_onionmat <- function(a,x){a[1,1] <- x ; expect_true(a[1,1] == x)}

  checkerB <- function(a,x){
    stopifnot(is.onion(a))
    expect_true(a[1:2][2] == a[2])
    a[1] <- x
    expect_true(a[1] == x)
    expect_error(a[1] <- "fish")
    expect_error(a[1,2] <- x)

    jj <- a
    jj[] <- 3
    expect_true(all(jj==3))
    y <- runif(1)
    a[1] <- y
    expect_true(a[1] == y)
    expect_error(a[1,2] <- y)
  }
  
  for(i in 1:2){
    n <- 3 # length of onion
    o1 <- 3
    o2 <- 4 # o1,o2: size of onionmats

    H <- rquat(n)
    x <- rnorm(1)
    checker_re(H,x)
    checker_i (H,x)
    checker_j (H,x)
    checker_k (H,x)
    checker_extract_onion(H,x)

    H <- rquat(n)
    x <- rnorm(1)
    checker_re(H,x)
    checker_i (H,x)
    checker_j (H,x)
    checker_k (H,x)
    checker_extract_onion(H,x)

    om_q <- romat("quaternion",o1,o2)
    checker_re(om_q,x)
    checker_i (om_q,x)
    checker_j (om_q,x)
    checker_k (om_q,x)
    checker_extract_onionmat(om_q,x)

    x <- rnorm(n)
    checker_re(H,x)
    checker_i (H,x)
    checker_j (H,x)
    checker_k (H,x)

    O <- roct(n)
    x <- rnorm(1)
    checker_re(O,x)
    checker_i (O,x)
    checker_j (O,x)
    checker_l (O,x)
    checker_il(O,x)
    checker_jl(O,x)
    checker_kl(O,x)
    checker_extract_onion(O,x)

    om_o <- romat("octonion",o1,o2)
    checker_re(om_o,x)
    checker_i (om_o,x)
    checker_j (om_o,x)
    checker_k (om_o,x)

    checker_l (om_o,x)
    checker_il(om_o,x)
    checker_jl(om_o,x)
    checker_kl(om_o,x)

    checker_extract_onionmat(om_o,x)

    x <- rnorm(n)
    checker_re(O,x)
    checker_i (O,x)
    checker_j (O,x)
    checker_l (O,x)
    checker_il(O,x)
    checker_jl(O,x)
    checker_kl(O,x)



    checkerB(rquat(10),rquat(1))
    checkerB(roct(10),roct(1))

  }



  

})
