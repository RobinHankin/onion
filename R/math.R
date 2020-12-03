`process` <- function(A,B){
  ## takes two onions, returns a list of two same-sized matrices with appropriate (col)names

  sA <- seq_along(A)
  sB <- seq_along(B)
  names(sA) <- names(A)
  names(sB) <- names(B)
  ind <-  rbind(sA,sB)

  A <- as.matrix(A)[,ind[1,,drop=TRUE]]
  B <- as.matrix(B)[,ind[2,,drop=TRUE]]

  colnames(A) <- colnames(ind)
  colnames(B) <- colnames(ind)

  list(A,B)
}
  
`onion_cumprod` <- function(x){
  if(length(x)==1){return(x)}
  out <- x
  for(i in seq_along(x)[-1]){
    out[i] <- out[i-1]*x[i]
  }
  return(out)
}

`onion_cumsum` <- function(x){as.onion(t(apply(as.matrix(x),1,cumsum)))}

setMethod("Math","onion",
          function(x){
          browser()
            switch(.Generic,
                   abs     = onion_abs(x),
                   cumprod = onion_cumprod(x),
                   cumsum  = onion_cumsum(x),
                   stop("Not yet implemented for onions")
                   )
          } )

`onion_allsum` <- function(x){as.onion(cbind(rowSums(as.matrix(x))))}
`quaternion_allprod` <- function(x){
  out <- x[1]
  for(i in seq(from=2,len=length(x)-1)){
    out <- out*x[i]
  }
  return(out)
}
