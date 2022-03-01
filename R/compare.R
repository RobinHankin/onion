
`onion_compare` <- function(e1,e2){
  stopifnot(is.onion(e1) | is.onion(e2))
  if(!is.onion(e1)){e1 <- as.onion(e1,e2)}
  if(!is.onion(e2)){e2 <- as.onion(e2,e1)}
    
  jj <- harmonize_oo(e1,e2)
  out <- apply(jj[[1]]==jj[[2]],2,all)

  switch(.Generic,
         "==" =  out,
         "!=" = !out,
         stop(gettextf("comparison operator %s not defined for onions", dQuote(.Generic)))
         )
}

setMethod("Compare",signature(e1 = "onion"  , e2="onion"  ), onion_compare)
setMethod("Compare",signature(e1 = "onion"  , e2="numeric"), onion_compare)
setMethod("Compare",signature(e1 = "numeric", e2="onion"  ), onion_compare)


`onionmat_equal_onionmat` <- function(e1,e2){
  jj <- getM(e1)==getM(e2) # traps nonconformable matrices
  jj[] <- getd(e1)==getd(e2)
  return(jj)
}

`onionmat_equal_single` <- function(e1,e2){
  jj <- getM(e1)
  storage.mode(jj) <- "logical"
  jj[] <- getd(e1)==e2
  return(jj)
}

`onionmat_compare_onionmat` <- function(e1,e2){
  switch(.Generic,
         "==" = return( onionmat_equal_onionmat(e1,e2)),
         "!=" = return(!onionmat_equal_onionmat(e1,e2)),
         stop(gettextf("comparison operator %s not defined for onionmats", dQuote(.Generic)))
         )
}

`onionmat_compare_single` <- function(e1,e2){
  switch(.Generic,
         "==" = return( onionmat_equal_single(e1,e2)),
         "!=" = return(!onionmat_equal_single(e1,e2)),
         stop(gettextf("comparison operator %s not defined for onionmats", dQuote(.Generic)))
         )
}

`single_compare_onionmat` <- function(e1,e2){
  switch(.Generic,
         "==" = return( onionmat_equal_single(e2,e1)),
         "!=" = return(!onionmat_equal_single(e2,e1)),
         stop(gettextf("comparison operator %s not defined for onionmats", dQuote(.Generic)))
         )
}

setMethod("Compare",signature(e1 = "onionmat"  , e2="onionmat"  ), onionmat_compare_onionmat)
setMethod("Compare",signature(e1 = "onionmat"  , e2="ANY"  ), onionmat_compare_single)
setMethod("Compare",signature(e1 = "ANY"  , e2="onionmat"  ), single_compare_onionmat)


