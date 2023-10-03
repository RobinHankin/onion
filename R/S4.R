setClass("onion",
         representation = "VIRTUAL"
         )

setClass("onionmat",
         slots = c(d  = "onion", M = "matrix")
         )

setClass("quaternion",
         slots    = c(x = "matrix"),
         contains = "onion",
         validity = function(object){
           x <- object@x
           if(!is.numeric(x)){
             return("not numeric")
           } else if(nrow(x) != 4){
             return("must have 4 rows")
           } else {
             return(TRUE)
           }
         }
         )

setClass("octonion",
         slots    = c(x = "matrix"),
         contains = "onion",
         validity = function(object){
           x <- object@x
           if(!is.numeric(x)){
             return("not numeric")
           } else if(nrow(x) != 8){
             return("must have 8 rows")
           } else {
             return(TRUE)
           }
         }
         )

"is.quaternion" <- function(x){is(x,"quaternion")}
"is.octonion" <- function(x){is(x,"octonion")}
"is.onion" <- function(x){is(x,"onion")}
"is.onionmat" <- function(x){is(x,"onionmat")}

setAs("onion", "matrix", function(from){
  out <-   from@x
  rownames(out) <- comp_names(from)
  return(out)
} )

setMethod("as.matrix",signature(x="onion"),function(x){as(x,"matrix")})

setAs("onion", "double", function(from){ as.double(from@x)})  # there are no occurences of "@" below this line or elsewhere in this directory
setMethod("as.double",signature(x="onion"),function(x){as(x,"double")})


setGeneric("length")
setMethod("length","onion",function(x){ncol(as.matrix(x))})

setGeneric("length<-")
setReplaceMethod("length","onion",function(x,value){
  if(value <= length(x)){
    return(x[seq_len(value)])
  } else {
    out <- as.matrix(x)
    out <- cbind(out,matrix(NA,nrow(out),value-ncol(out)))
    return(as.onion(out))
  }
})


`biggest` <- function(...){
  a <-  unlist(lapply(list(...),class))
  if("octonion" %in% a){
    return("octonion")
  } else if("quaternion" %in% a)
    {return("quaternion")
   } else {
     return("scalar")
   }
}


setGeneric("drop")
setMethod("drop","onion",function(x){
  if(all(Im(x)==0)){x <- Re(x)}
  return(x)
} )
