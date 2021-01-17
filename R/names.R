setGeneric("names")
setMethod("names","onion",function(x){colnames(as.matrix(x))})

setReplaceMethod("names",signature(x="onion"),
                 function(x,value){
                   out <- as.matrix(x)
                   colnames(out) <- value
                   return(as.onion(out))
                 } ) 





## now onionmats
setMethod("names","onionmat",function(x){NULL})
setMethod("dimnames","onionmat",function(x){dimnames(getM(x))})
setMethod("rownames","onionmat",function(x){rownames(getM(x))})
setMethod("colnames","onionmat",function(x){colnames(getM(x))})

setReplaceMethod("names",signature(x="onionmat"), function(x,value){stop("names attribute not defined for an oniomat")})
setReplaceMethod("dimnames",signature(x="onionmat"), function(x,value){
  M <- getM(x)
  dimnames(M) <- value
  newonionmat(getd(x),M)
} )

setReplaceMethod("rownames",signature(x="onionmat"), function(x,value){
  M <- getM(x)
  rownames(M) <- value
  newonionmat(getd(x),M)
} )

setReplaceMethod("colnames",signature(x="onionmat"), function(x,value){
  M <- getM(x)
  colnames(M) <- value
  newonionmat(getd(x),M)
} )

setGeneric("nrow")
setGeneric("ncol")
setGeneric("dim")
setMethod("nrow","onionmat", function(x){nrow(getM(x))})
setMethod("ncol","onionmat", function(x){ncol(getM(x))})
setMethod("dim" ,"onionmat",function(x){dim  (getM(x))})

setReplaceMethod("dim",signature(x="onionmat"), function(x,value){
  M <- getM(x)
  dim(M) <- value
  newonionmat(getd(x),M)
} )

