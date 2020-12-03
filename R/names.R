setGeneric("names")
setMethod("names","onion",function(x){colnames(as.matrix(x))})

setReplaceMethod("names",signature(x="onion"),
                 function(x,value){
                   out <- as.matrix(x)
                   colnames(out) <- value
                   return(as.onion(out))
                 } ) 

