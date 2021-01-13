setGeneric("seq")

`seq_onion` <-
  function (from = 1, to = 1, by = ((to - from)/(length.out - 1)), length.out = NULL, slerp = FALSE, ...) 
{
  if (identical(length.out,0)) {stop()}

  if (!missing(length.out)){ 
    length.out <- ceiling(length.out)
  }

  if(missing(to)){
    to <- from + by*(length.out-1)
  }
  if(missing(from)){
    from <- to - by*(length.out-1)
  }
  del <- to - from
  if(missing(by)){
    by <- del/length.out
  }

  h <- seq(from=0,to=1,len=length.out)
  if(slerp){
    return(from*(onion_inverse(from)*to)^h)
  } else {
    return(from*(1-h)+ to*h)
  }
}

setMethod("seq","onion",seq_onion)
