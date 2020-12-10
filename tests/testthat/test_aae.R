test_that("Test suite aae.R, onion extract and replace methods, eg i() and i<-()",{
  
  checker_re <- function(a,x){Re(a) <- x ; expect_true(all(Re(a)==x))}
  checker_i  <- function(a,x){ i(a) <- x ; expect_true(all( i(a)==x))}
  checker_j  <- function(a,x){ j(a) <- x ; expect_true(all( j(a)==x))}
  checker_k  <- function(a,x){ k(a) <- x ; expect_true(all( k(a)==x))}
  checker_l  <- function(a,x){ l(a) <- x ; expect_true(all( l(a)==x))}
  checker_il <- function(a,x){il(a) <- x ; expect_true(all(il(a)==x))}
  checker_jl <- function(a,x){jl(a) <- x ; expect_true(all(jl(a)==x))}
  checker_kl <- function(a,x){kl(a) <- x ; expect_true(all(kl(a)==x))}

  checker_extract <- function(a,x){a[1] <- x ; expect_true(a[1] == x)}

  for(i in 1:2){
    n <- 3 # length of onion

    H <- rquat(n)
    x <- rnorm(1)
    checker_re(H,x)
    checker_i (H,x)
    checker_j (H,x)
    checker_k (H,x)
    checker_extract(H,x)

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
    checker_extract(O,x)

    x <- rnorm(n)
    checker_re(O,x)
    checker_i (O,x)
    checker_j (O,x)
    checker_l (O,x)
    checker_il(O,x)
    checker_jl(O,x)
    checker_kl(O,x)




  }
  

})
