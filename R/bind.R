`bind_onion` <- function(x, bind, ...){
    jj <- seq_along(x)
    names(jj) <- names(x)
    newonionmat(d=x,M=bind(jj))
}

`bind_onion_onion` <- function(x,y, bind, ...){
    Mout <- bind(drop(t(getM(t(x)))),-drop(t(getM(t(y)))))
    dout <- rep(x[1],length(Mout))
    dout[which(Mout>0)] <- x
    dout[which(Mout<0)] <- y
    newonionmat(d=dout,M=Mout)
}

`bind_onionmat_onionmat` <- function(x,y, bind, ...){
    Mout <- bind(getM(x),-getM(y))
    dout <- rep(x[1],length(Mout))
    dout[which(Mout>0)] <- getd(x)
    dout[which(Mout<0)] <- getd(y)
    newonionmat(d=dout,M=Mout)
}

`bind_onionmat_onion` <- function(x,y, bind, ...){
    Mout <- bind(getM(x),-drop(t(getM(t(y)))))
    dout <- rep(x[1],length(Mout))
    dout[which(Mout>0)] <- getd(x)
    dout[which(Mout<0)] <- y
    newonionmat(d=dout,M=Mout)
}

`bind_onion_onionmat` <- function(x,y, bind, ...){
    Mout <- bind(drop(t(getM(t(x)))),-getM(y))
    dout <- rep(x[1],length(Mout))
    dout[which(Mout>0)] <- x
    dout[which(Mout<0)] <- getd(y)
    newonionmat(d=dout,M=Mout)
}

setMethod("rbind2",signature(x="onion",y="missing"),function(x,y){bind_onion(x,rbind)})
setMethod("cbind2",signature(x="onion",y="missing"),function(x,y){bind_onion(x,cbind)})
          
setMethod("rbind2",signature(x="onionmat",y="missing"),function(x,y){x})
setMethod("cbind2",signature(x="onionmat",y="missing"),function(x,y){x})
          


setMethod("rbind2", signature(x="onion",y="onion"),function(x,y, ...){bind_onion_onion(x,y,rbind)})
setMethod("cbind2", signature(x="onion",y="onion"),function(x,y, ...){bind_onion_onion(x,y,cbind)})

setMethod("rbind2", signature(x="onionmat",y="onionmat"),function(x,y, ...){bind_onionmat_onionmat(x,y,rbind)})
setMethod("cbind2", signature(x="onionmat",y="onionmat"),function(x,y, ...){bind_onionmat_onionmat(x,y,cbind)})






setMethod("rbind2", signature(x="onion",y="onionmat"),function(x,y, ...){bind_onion_onionmat(x,y,rbind)})
setMethod("cbind2", signature(x="onion",y="onionmat"),function(x,y, ...){bind_onion_onionmat(x,y,cbind)})

setMethod("rbind2", signature(x="onionmat",y="onion"),function(x,y, ...){bind_onionmat_onion(x,y,rbind)})
setMethod("cbind2", signature(x="onionmat",y="onion"),function(x,y, ...){bind_onionmat_onion(x,y,cbind)})

setMethod("rbind2", signature(x="onion",y="numeric"),function(x,y, ...){bind_onion_onion(x,as.onion(y,x),rbind)})
setMethod("cbind2", signature(x="onion",y="numeric"),function(x,y, ...){bind_onion_onion(x,as.onion(y,x),cbind)})

setMethod("rbind2", signature(x="numeric" ,y="onion"),function(x,y, ...){bind_onion_onion(as.onion(x,y),y,rbind)})
setMethod("cbind2", signature(x="numeric" ,y="onion"),function(x,y, ...){bind_onion_onion(as.onion(x,y),y,cbind)})

setMethod("rbind2", signature(x="onionmat",y="numeric"),function(x,y, ...){bind_onionmat_onion(x,as.onion(y,x[1]),rbind)})
setMethod("cbind2", signature(x="onionmat",y="numeric"),function(x,y, ...){bind_onionmat_onion(x,as.onion(y,x[1]),cbind)})

setMethod("rbind2", signature(x="numeric",y="onionmat"),function(x,y, ...){bind_onion_onionmat(as.onion(x,y[1]),y,rbind)})
setMethod("cbind2", signature(x="numeric",y="onionmat"),function(x,y, ...){bind_onion_onionmat(as.onion(x,y[1]),y,cbind)})

setMethod("rbind2", signature(x="onion",y="matrix"),function(x,y, ...){bind_onion_onionmat(x,y+x[1]*0,rbind)})
setMethod("cbind2", signature(x="onion",y="matrix"),function(x,y, ...){bind_onion_onionmat(x,y+x[1]*0,cbind)})

setMethod("rbind2", signature(x="matrix",y="onion"),function(x,y, ...){bind_onionmat_onion(x+y[1]*0,y,rbind)})
setMethod("cbind2", signature(x="matrix",y="onion"),function(x,y, ...){bind_onionmat_onion(x+y[1]*0,y,cbind)})

setMethod("rbind2", signature(x="onionmat",y="matrix"),function(x,y, ...){bind_onionmat_onionmat(x,y+x[1]*0,rbind)})
setMethod("cbind2", signature(x="onionmat",y="matrix"),function(x,y, ...){bind_onionmat_onionmat(x,y+x[1]*0,cbind)})

setMethod("rbind2", signature(x="matrix",y="onionmat"),function(x,y, ...){bind_onionmat_onionmat(x+y[1]*0,y,rbind)})
setMethod("cbind2", signature(x="matrix",y="onionmat"),function(x,y, ...){bind_onionmat_onionmat(x+y[1]*0,y,cbind)})
