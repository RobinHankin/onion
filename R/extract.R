setClassUnion("index", members =  c("numeric", "logical", "character")) # taken from the Matrix package.

setMethod("[", signature("onion",i="index",j="missing",drop="ANY"),function(x,i,j,drop){as.onion(as.matrix(x)[,i,drop=FALSE])})
setMethod("[", signature("onion",i="index",j="ANY"    ,drop="ANY"),function(x,i,j,drop){stop('signature("onion",i="index",j="ANY",drop="ANY") infelicitous')})

setReplaceMethod("[",signature(x="onion"),
                 function(x,i,j,value){
                   if(!missing(j)){
                     warning("second argument to extractor function ignored")
                   }
                   out <- as.matrix(x)
                   if(is.vector(value)){
                     value <- kronecker(t(value),c(1,rep(0,nrow(out)-1)))
                   }
                   out[,i] <- as.matrix(value)
                   return(as.onion(out))
                 } )

## following copies the excellent logic of the Matrix package, whose
## inspiration I gratefully acknowledge.

setMethod("[", signature(x="onionmat",i="index"  ,j="index"  ,drop="ANY"),function(x,i,j,drop){newonionmat(getd(x)[c(getM(x)[i,j,drop=drop])],getM(x)[i,j,drop=drop])})
setMethod("[", signature(x="onionmat",i="index"  ,j="missing",drop="ANY"),function(x,i,j,drop){newonionmat(getd(x)[c(getM(x)[i, ,drop=drop])],getM(x)[i, ,drop=drop])})
setMethod("[", signature(x="onionmat",i="missing",j="index"  ,drop="ANY"),function(x,i,j,drop){newonionmat(getd(x)[c(getM(x)[ ,j,drop=drop])],getM(x)[ ,j,drop=drop])})
setMethod("[", signature(x="onionmat",i="missing",j="missing",drop="ANY"),function(x,i,j,drop){newonionmat(getd(x)[c(getM(x)[ , ,drop=drop])],getM(x)[ , ,drop=drop])})
setMethod("[", signature(x="onionmat",i="matrix" ,j="missing",drop="ANY"),function(x,i,j,drop){newonionmat(getd(x)[c(getM(x)[i,  drop=drop])],getM(x)[i,  drop=drop])})
setMethod("[", signature(x="onionmat",i="ANY"    ,j="ANY"    ,drop="ANY"),function(x,i,j,drop){stop("signature not recognised")} )








setReplaceMethod("[",signature(x="onionmat"),
                 function(x,i,j,value){
                   d <- getd(x)
                   M <- getM(x)
                   if(is.vector(value)){
                     value <- kronecker(t(value),c(1,rep(0,nrow(as.matrix(d))-1)))
                   }
                   if(missing(j)){ d[c(M[i])] <- value
                   } else {
                     d[c(M[i,j])] <- value
                   }
                   newonionmat(d,M)
                 } )

setGeneric("i",function(z){standardGeneric("i")})
setGeneric("j",function(z){standardGeneric("j")})
setGeneric("k",function(z){standardGeneric("k")})
setGeneric("l",function(z){standardGeneric("l")})

setMethod("i","onion",function(z){as.matrix(z)[2,]})
setMethod("j","onion",function(z){as.matrix(z)[3,]})
setMethod("k","onion",function(z){as.matrix(z)[4,]})

setGeneric("l" ,function(z){standardGeneric("l" )})
setGeneric("il",function(z){standardGeneric("il")})
setGeneric("jl",function(z){standardGeneric("jl")})
setGeneric("kl",function(z){standardGeneric("kl")})

setMethod("l" ,"octonion",function(z){as.matrix(z)[5,]})
setMethod("il","octonion",function(z){as.matrix(z)[6,]})
setMethod("jl","octonion",function(z){as.matrix(z)[7,]})
setMethod("kl","octonion",function(z){as.matrix(z)[8,]})

setGeneric("i<-",function(x,value){standardGeneric("i<-")})
setReplaceMethod("i",signature(x="onion"), function(x,value){
  x <-  as.matrix(x)
  x[2,] <- value
  return(as.onion(x))
} )

setGeneric("j<-",function(x,value){standardGeneric("j<-")})
setReplaceMethod("j",signature(x="onion"), function(x,value){
  x <-  as.matrix(x)
  x[3,] <- value
  return(as.onion(x))
} )

setGeneric("k<-",function(x,value){standardGeneric("k<-")})
setReplaceMethod("k",signature(x="onion"), function(x,value){
  x <-  as.matrix(x)
  x[4,] <- value
  return(as.onion(x))
} )

setGeneric("l<-",function(x,value){standardGeneric("l<-")})
setReplaceMethod("l",signature(x="octonion"), function(x,value){
  x <-  as.matrix(x)
  x[5,] <- value
  return(as.onion(x))
} )

setGeneric("il<-",function(x,value){standardGeneric("il<-")})
setReplaceMethod("il",signature(x="octonion"), function(x,value){
  x <-  as.matrix(x)
  x[6,] <- value
  return(as.onion(x))
} )

setGeneric("jl<-",function(x,value){standardGeneric("jl<-")})
setReplaceMethod("jl",signature(x="octonion"), function(x,value){
  x <-  as.matrix(x)
  x[7,] <- value
  return(as.onion(x))
} )

setGeneric("kl<-",function(x,value){standardGeneric("kl<-")})
setReplaceMethod("kl",signature(x="octonion"), function(x,value){
  x <-  as.matrix(x)
  x[8,] <- value
  return(as.onion(x))
} )
