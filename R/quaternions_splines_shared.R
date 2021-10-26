.check_keyRotors <- function(keyRotors, closed){
  if(length(keyRotors) < 2L){
    stop("At least two keyRotors are required.")
  }
  if(closed){
    keyRotors <- c(keyRotors, keyRotors[1L])
  }
  .canonicalized(keyRotors)
}

.check_keyTimes <- function(keyTimes, n_quaternions){
  if(is.null(keyTimes)){
    return(seq_len(n_quaternions))
  }
  if(any(diff(keyTimes) <= 0)){
    stop("`keyTimes` must be an increasing vector of numbers.")
  }
  keyTimes
}

.check_time <- function(t, keyTimes){
  n_keyTimes <- length(keyTimes)
  lastKeyTime <- keyTimes[n_keyTimes]
  if(t < keyTimes[1L] || t > lastKeyTime){
    stop("The interpolating times must be within the range of `keyTimes`.")
  }
  if(t < lastKeyTime){
    idx <- findInterval(trunc(t), keyTimes, left.open = TRUE)
  }else{ # t = lastKeyTime
    idx <- n_keyTimes - 2L
  }
  idx
}

.slerp <- function(q1, q2, t){
  (q2 * onion_inverse(q1))^t * q1
}

.isPositiveInteger <- function(x){
  is.numeric(x) && (length(x) == 1L) && (!is.na(x)) && (floor(x) == x)
}
