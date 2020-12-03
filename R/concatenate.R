`c_onionpair` <- function(x,y){as.onion(cbind(as.matrix(x),as.matrix(y)))}

setMethod("c",signature(x="onion"),
          function(x,...){
            if(nargs()==1){
              return(x)
            } else if(nargs() <= 2){
              return(c_onionpair(x,...))
            } else {
              return(c_onionpair(x,Recall(...)))
            }
          } )

