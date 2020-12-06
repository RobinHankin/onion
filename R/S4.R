setClass("onion",
         representation = "VIRTUAL"
         )

setClass("quaternion",
         slots    = c(x = "matrix"),
         contains = "onion"
         )

setClass("octonion",
         slots    = c(x = "matrix"),
         contains = "onion"
         )

`valid_quaternion` <- function(object){
  x <- object@x
  if(!is.numeric(x)){
    return("not numeric")
  } else if(!is.matrix(x)){
    return("not a matrix")
  } else if(nrow(x) != 4){
    return("must have 4 rows")
  } else {
    return(TRUE)
  }
}

`valid_octonion` <- function(object){
  x <- object@x
  if(!is.numeric(x)){
    return("not numeric")
  } else if(!is.matrix(x)){
    return("not a matrix")
  } else if(nrow(x) != 8){
    return("must have 8 rows")
  } else {
    return(TRUE)
  }
}
    
setValidity("quaternion", valid_quaternion)
setValidity("octonion", valid_octonion)

"is.quaternion" <- function(x){is(x,"quaternion")}
"is.octonion" <- function(x){is(x,"octonion")}
"is.onion" <- function(x){is(x,"onion")}

setAs("onion", "matrix", function(from){ from@x} )
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

