
`onion_compare` <- function(e1,e2){
  stopifnot(is.onion(e1) | is.onion(e2))
  if(!is.onion(e1)){e1 <- as.onion(e1,e2)}
  if(!is.onion(e2)){e2 <- as.onion(e2,e1)}
    
  jj <- harmonize_oo(e1,e2)
  switch(.Generic,
         "==" = return(apply(jj[[1]]==jj[[2]],2,all)),
         "!=" = return(apply(jj[[1]]!=jj[[2]],2,all)),
         stop(paste("comparision operator \"", .Generic, "\" not defined for onions"))
         )
}

setMethod("Compare",signature(e1 = "onion"  , e2="onion"  ), onion_compare)
setMethod("Compare",signature(e1 = "onion"  , e2="numeric"), onion_compare)
setMethod("Compare",signature(e1 = "numeric", e2="onion"  ), onion_compare)

