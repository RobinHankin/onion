.select_segment_and_normalize_t <- function(segments, keyTimes, t){
  idx <- .check_time(t, keyTimes) #+ 1L
  t0 <- keyTimes[idx]
  t1 <- keyTimes[idx+1L]
  delta_t <- t1 - t0
  t <- (t - t0) / delta_t
  list("segment" = segments[[idx]], "time" = t, "difftime" = delta_t)
}

.reduce_de_casteljau <- function(segment, t){
  if(length(segment) < 2L){
    stop("Segment must have at least two quaternions.")
  }
  while(length(segment) > 2L){
    newsegment <- quaternion(length.out = length(segment) - 1L)
    for(i in seq_len(length(segment)-1L)){
      one <- segment[i]
      two <- segment[i+1L]
      newsegment[i] <- .slerp(one, two, t)
    }
    segment <- newsegment
  }
  segment
}

#' @title Spline using the De Casteljau algorithm
#' @description Constructs a quaternions spline using the De Casteljau
#'   algorithm.
#'
#' @param segments a list of vectors of unit quaternions; each segment must
#'   contain at least two quaternions
#' @param keyTimes the times corresponding to the segment boundaries, an
#'   increasing vector of length \code{length(segments)+1}; if \code{NULL},
#'   it is set to \code{1, 2, ..., length(segments)+1}
#' @param times the interpolating times, they must lie within the range of
#'   \code{keyTimes}; ignored if \code{constantSpeed=TRUE}
#' @param constantSpeed Boolean, whether to re-parameterize the spline to
#'   have constant speed; in this case, \code{"times"} is ignored and a
#'   function is returned, with an attribute \code{"times"}, the vector of
#'   new times corresponding to the key rotors
#'
#' @return A vector of quaternions having the same length as \code{times},
#'   or a function if \code{constantSpeed=TRUE}.
#' @export
#'
#' @note This algorithm is rather for internal purpose. It serves for example
#'   as a base for the \link[=KochanekBartels]{Konachek-Bartels} algorithm.
DeCasteljau <- function(
  segments, keyTimes = NULL, times, constantSpeed = FALSE
){
  n_segments <- length(segments)
  if(is.null(keyTimes)){
    keyTimes <- seq_len(n_segments + 1L)
  }else if(length(keyTimes) != n_segments + 1L){
    stop("Number of key times must be one more than number of segments.")
  }
  evaluate <- function(t, value){
    if((n_times <- length(t)) > 1L){
      out <- quaternion(n_times)
      for(j in seq_len(n_times)){
        out[j] <- evaluate(t[j], value)
      }
      return(out)
    }
    x <- .select_segment_and_normalize_t(segments, keyTimes, t)
    segment <- x[["segment"]]
    s       <- x[["time"]]
    quats <- .reduce_de_casteljau(segment, s)
    if(!value){
      tangent <- log(quats[2L] * onion_inverse(quats[1L]))
      degree <- length(segment) - 1L
      tangent * 2 * degree / x[["difftime"]]
    }else{
      .slerp(quats[1L], quats[2L], s)
    }
  }
  if(constantSpeed){
    f <- function(t) evaluate(t, FALSE)
    t0 <- head(keyTimes, -1L)
    t1 <- keyTimes[-1L]
    intervals <- cbind(t0, t1)
    speed <- function(t){
      sqrt(apply(as.matrix(f(t))[-1L,], 2L, crossprod))
    }
    integrated_speed <- 0
    for(i in seq_len(nrow(intervals))){
      integral <- integrate(speed, intervals[i, 1L], intervals[i, 2L])
      integrated_speed <- c(integrated_speed, integral[["value"]])
    }
    newTimes <- cumsum(integrated_speed)
    s2t <- Vectorize(function(s){
      idx <- .check_time(s, newTimes) #+ 1L
      s <- s - newTimes[idx]
      t0 <- keyTimes[idx]
      t1 <- keyTimes[idx+1L]
      l <- function(t){
        integrate(speed, t0, t)[["value"]] - s
      }
      uniroot(l, c(t0, t1))[["root"]]
    })
    feval <- function(s){
      if((n_s <- length(s)) > 1L){
        out <- quaternion(n_s)
        for(j in seq_len(n_s)){
          out[j] <- feval(s[j])
        }
        return(out)
      }
      if(s < newTimes[1L] || s >= newTimes[length(newTimes)]){
        warning("Evaluation of function outside its range, returning `NA`.")
        return(NA_real_)
      }
      evaluate(s2t(s), value = TRUE)
    }
    attr(feval, "times") <- newTimes
    feval
  }else{
    evaluate(times, value = TRUE)
  }
}

.calculate_control_quaternions <- function(quaternions, times, tcb){
  q_1 <- quaternions[1L]
  q0  <- quaternions[2L]
  q1  <- quaternions[3L]
  t_1 <- times[1L]
  t0  <- times[2L]
  t1  <- times[3L]
  T   <- tcb[1L]
  C   <- tcb[2L]
  B   <- tcb[3L]
  a <- (1 - T) * (1 + C) * (1 + B)
  b <- (1 - T) * (1 - C) * (1 - B)
  c <- (1 - T) * (1 - C) * (1 + B)
  d <- (1 - T) * (1 + C) * (1 - B)
  q_in  <- q0 * onion_inverse(q_1)
  q_out <- q1 * onion_inverse(q0)
  v_in  <- log(q_in) / (t0 - t_1)
  v_out <- log(q_out) / (t1 - t0)
  v_in  <- as.numeric(v_in)[-1L]
  v_out <- as.numeric(v_out)[-1L]
  v0 <- function(weight_in, weight_out){
    (
      weight_in * (t1 - t0) * v_in + weight_out * (t0 - t_1) * v_out
    ) / (t1 - t_1)
  }
  out     <- quaternion(length.out = 2L)
  out[1L] <- exp(
    as.quaternion(c(0, -v0(c, d) * (t0 - t_1) / 3), single = TRUE)
  ) * q0
  out[2L] <- exp(
    as.quaternion(c(0, v0(a, b) * (t1 - t0) / 3), single = TRUE)
  ) * q0
  out
}

.check_endcondition <- function(endcondition, rotors, times){
  n_rotors <- length(rotors)
  n_times <- length(times)
  if(endcondition == "closed"){
    prefix <- rotors[n_rotors - 1L]
    if(dotprod(prefix, rotors[1L]) < 0){
      prefix <- -prefix
    }
    suffix <- rotors[2L]
    if(dotprod(rotors[n_rotors], suffix) < 0){
      suffix <- -suffix
    }
    rotors <- c(prefix, rotors, suffix)
    times <- c(
      times[1L] - (times[n_times] - times[n_times-1L]),
      times,
      times[n_times] + (times[2L] - times[1L])
    )
    triples_times <- t(vapply(seq_len(n_times+1L), function(i){
      times[c(i, i+1L, i+2L)]
    }, numeric(3L)))
    triples_rotors <- lapply(seq_len(n_rotors), function(i){
      rotors[c(i, i+1L, i+2L)]
    })
  }else{
    triples_times <- t(vapply(seq_len(n_times), function(i){
      times[c(i, i+1L, i+2L)]
    }, numeric(3L)))
    triples_rotors <- lapply(seq_len(n_rotors-2L), function(i){
      rotors[c(i, i+1L, i+2L)]
    })
  }
  list("times" = triples_times, "rotors" = triples_rotors)
}

.natural_control_quaternion <- function(outer, inner_control, inner){
  (
    (inner_control * onion_inverse(inner)) * (inner * onion_inverse(outer))
  )^(1 / 2) * outer
}

#' @title Kochanek-Bartels quaternions spline
#' @description Constructs a quaternions spline by the Kochanek-Bartels
#'   algorithm.
#'
#' @param keyRotors a vector of unit quaternions (rotors) to be interpolated
#' @param keyTimes the times corresponding to the key rotors; must be an
#'   increasing vector of the same length a \code{keyRotors} if
#'   \code{endcondition = "natural"} or of length one more than number of key
#'   rotors if \code{endcondition = "closed"}
#' @param tcb a vector of three numbers respectively corresponding to tension,
#'   continuity and bias
#' @param times the times of interpolation; each time must lie within the range
#'   of the key times; this parameter can be missing if \code{keyTimes} is
#'   \code{NULL} and \code{n_intertimes} is not missing, and it is ignored if
#'   \code{constantSpeed=TRUE}
#' @param n_intertimes should be missing if \code{times} is given; otherwise,
#'   \code{keyTimes} should be \code{NULL} and \code{times} is constructed by
#'   linearly interpolating the automatic key times such that there are
#'   \code{n_intertimes - 1} between two key times (so the times are the key
#'   times if \code{n_intertimes = 1})
#' @param endcondition start/end conditions, can be \code{"closed"} or
#'   \code{"natural"}
#' @param constantSpeed Boolean, whether to re-parameterize the spline to
#'   have constant speed; in this case, \code{"times"} is ignored and a
#'   function is returned, with an attribute \code{"times"}, the vector of
#'   new times corresponding to the key rotors
#'
#' @return A vector of quaternions having the same length as the \code{times}
#'   vector, or a (slow) function if \code{constantSpeed=TRUE}.
#' @export
#'
#' @examples library(onion)
#' # We will use a Kochanek-Bartels quaternions spline to construct a spherical
#' #   curve interpolating some key points on the sphere of radius 5
#'
#' # helper function: spherical to Cartesian coordinates
#' sph2cart <- function(rho, theta, phi){
#'   return(c(
#'     rho * cos(theta) * sin(phi),
#'     rho * sin(theta) * sin(phi),
#'     rho * cos(phi)
#'   ))
#' }
#'
#' # construction of the key points on the sphere
#' keyPoints <- matrix(nrow = 0L, ncol = 3L)
#' theta_ <- seq(0, 2*pi, length.out = 9L)[-1L]
#' phi <- 1.3
#' for(theta in theta_){
#'   keyPoints <- rbind(keyPoints, sph2cart(5, theta, phi))
#'   phi = pi - phi
#' }
#' n_keyPoints <- nrow(keyPoints)
#'
#' # construction of the key rotors; the first key rotor is the identity
#' #   quaternion and rotor i sends the key point i-1 to the key point i
#' keyRotors <- quaternion(length.out = n_keyPoints)
#' rotor <- keyRotors[1L] <- H1
#' for(i in seq_len(n_keyPoints - 1L)){
#'   keyRotors[i+1L] <- rotor <-
#'     quaternionFromTo(keyPoints[i, ]/5, keyPoints[i+1L, ]/5) * rotor
#' }
#'
#' # Kochanek-Bartels quaternions spline
#' rotors <- KochanekBartels(
#'   keyRotors, n_intertimes = 25L, endcondition = "closed", tcb = c(-1, 5, 0)
#' )
#'
#' # construction of the interpolating points on the sphere
#' points <- matrix(nrow = 0L, ncol = 3L)
#' keyPoint1 <- rbind(keyPoints[1L, ])
#' for(i in seq_along(rotors)){
#'   points <- rbind(points, rotate(keyPoint1, rotors[i]))
#' }
#'
#' # visualize the result with the 'rgl' package
#' library(rgl)
#' spheres3d(0, 0, 0, radius = 5, color = "lightgreen")
#' spheres3d(points, radius = 0.2, color = "midnightblue")
#' spheres3d(keyPoints, radius = 0.25, color = "red")
KochanekBartels <- function(
  keyRotors, keyTimes = NULL, tcb = c(0, 0, 0),
  times, n_intertimes, endcondition = "natural",
  constantSpeed = FALSE
){
  endcondition <- match.arg(endcondition, c("closed", "natural"))
  closed <- endcondition == "closed"
  keyRotors <- .check_keyRotors(keyRotors, closed)
  n_keyRotors <- length(keyRotors)
  if(is.null(keyTimes) && !missing(n_intertimes)){
    stopifnot(.isPositiveInteger(n_intertimes))
    times <- seq(
      1, n_keyRotors, length.out = n_intertimes * (n_keyRotors - 1L) + 1L
    )
  }
  keyTimes <- .check_keyTimes(keyTimes, n_keyRotors)
  triples <- .check_endcondition(endcondition, keyRotors, keyTimes)
  triples_rotors <- triples[["rotors"]]
  triples_times <- triples[["times"]]
  control_points <- quaternion(length.out = 0L)
  for(i in seq_along(triples_rotors)){
    qs <- triples_rotors[[i]]
    qb_qa <- .calculate_control_quaternions(
      qs,
      triples_times[i, ],
      tcb
    )
    q_before <- qb_qa[1L]
    q_after  <- qb_qa[2L]
    control_points <- c(
      control_points, q_before, qs[2L], qs[2L], q_after
    )
  }
  n_control_points <- length(control_points)
  if(closed){
    stopifnot(4*length(keyTimes) == n_control_points)
    control_points <- control_points[3L:(n_control_points-2L)]
  }else if(n_control_points == 0L){
    # two quaternions -> slerp
    stopifnot(n_keyRotors == 2L)
    stopifnot(length(keyTimes) == 2L)
    q0 <- keyRotors[1L]
    q1 <- keyRotors[2L]
    offset <- (q1 * onion_inverse(q0))^(1/3)
    control_points <- c(q0, offset*q0, onion_inverse(offset)*q1, q1)
  }else{ # natural
    control_points <- c(
      keyRotors[1L],
      .natural_control_quaternion(
        keyRotors[1L], control_points[1L], control_points[2L]
      ),
      control_points,
      .natural_control_quaternion(
        keyRotors[n_keyRotors],
        control_points[n_control_points],
        control_points[n_control_points-1L]
      ),
      keyRotors[n_keyRotors]
    )
  }
  segments <- lapply(4L*seq_len(length(control_points) %/% 4L)-3L, function(i){
    control_points[c(i, i+1L, i+2L, i+3L)]
  })
  DeCasteljau(segments, keyTimes, times = times, constantSpeed = constantSpeed)
}
