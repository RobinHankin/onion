onion_to_string_lowlevel <- function(x,type){
  if(type == "quaternion"){
    stopifnot(length(x) == 4)
    onames <- .quatnames
  } else if(type == "octonion"){
    stopifnot(length(x) == 8)
    onames <- .octnames
  } else {
    stop()
  }
  onames[1]  <- ""
  wanted <- x!=0
  x <- x[wanted]
  onames <- onames[wanted]
  sign <- ifelse(x>0,"+","")
  out <-  paste(sign,x,onames,sep="",collapse="")
  if(substr(out,1,1)=="+"){out <- substr(out,2,nchar(out))}
  return(out)
}

