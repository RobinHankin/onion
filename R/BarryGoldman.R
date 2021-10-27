#' @title Barry-Goldman quaternions spline
#' @description Constructs a spline of unit quaternions by the Barry-Goldman
#'   method.
#'
#' @param keyRotors a vector of unit quaternions (rotors) to be interpolated;
#'   it is automatically appended with the first quaternion
#' @param keyTimes the times corresponding to the key rotors; must be an
#'   increasing vector of length \code{length(keyRotors)+1}; if \code{NULL},
#'   it is set to \code{c(1, 2, ..., length(keyRotors)+1)}
#' @param times the times of interpolation; each time must lie within the
#'   range of the key times; this parameter can be missing if \code{keyTimes}
#'   is \code{NULL} and \code{n_intertimes} is not missing
#' @param n_intertimes should be missing if \code{times} is given; otherwise,
#'   \code{keyTimes} should be \code{NULL} and \code{times} is constructed by
#'   linearly interpolating the automatic key times such that there are
#'   \code{n_intertimes - 1} between two key times (so the times are the key
#'   times if \code{n_intertimes = 1})
#'
#' @return A vector of unit quaternions with the same length as \code{times}.
#' @export
#' @note The function does not check whether the quaternions given in
#'   \code{keyRotors} are unit quaternions.
#'
#' @examples library(onion)
#' # We will use a Barry-Goldman quaternions spline to construct a spherical
#' #   curve interpolating some key points on the sphere of radius 5.
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
#' phi <- 1
#' for(theta in theta_){
#'   keyPoints <- rbind(keyPoints, sph2cart(5, theta, phi))
#'   phi = pi - phi
#' }
#' n_keyPoints <- nrow(keyPoints)
#'
#' # construction of the key rotors; the first key rotor is the identity
#' #   quaternion and rotor i sends the first key point to the key point i
#' keyRotors <- quaternion(length.out = n_keyPoints)
#' rotor <- keyRotors[1L] <- H1
#' for(i in seq_len(n_keyPoints - 1L)){
#'   keyRotors[i+1L] <- rotor <-
#'     quaternionFromTo(keyPoints[i, ]/5, keyPoints[i+1L, ]/5) * rotor
#' }
#'
#' # Barry-Goldman quaternions spline
#' rotors <- BarryGoldman(keyRotors, n_intertimes = 10L)
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
BarryGoldman <- function(keyRotors, keyTimes = NULL, times, n_intertimes){
  stopifnot(is.quaternion(keyRotors))
  keyRotors <- .check_keyRotors(keyRotors, closed = TRUE)
  n_keyRotors <- length(keyRotors)
  if(is.null(keyTimes) && !missing(n_intertimes)){
    stopifnot(.isPositiveInteger(n_intertimes))
    times <- seq(
      1, n_keyRotors, length.out = n_intertimes * (n_keyRotors - 1L) + 1L
    )
  }
  keyTimes <- .check_keyTimes(keyTimes, n_keyRotors)
  n_keyTimes <- length(keyTimes)
  evaluate <- function(t){
    if((n_times <- length(t)) > 1L){
      out <- quaternion(n_times)
      for(j in seq_len(n_times)){
        out[j] <- evaluate(t[j])
      }
      return(out)
    }
    idx <- .check_time(t, keyTimes, special = TRUE) #+ 1L
    q0 <- keyRotors[idx]
    q1 <- keyRotors[idx + 1L]
    t0 <- keyTimes[idx]
    t1 <- keyTimes[idx + 1L]
    if(idx == 1L){
      q_1 <- keyRotors[n_keyRotors - 1L]
      if(dotprod(q_1, q0) < 0){
        q_1 <- -q_1
      }
      t_1 <- t0 - (keyTimes[n_keyTimes] - keyTimes[n_keyTimes - 1L])
    }else{
      q_1 <- keyRotors[idx-1L]
      t_1 <- keyTimes[idx-1L]
    }
    if(idx + 1L == n_keyRotors){
      q2 <- keyRotors[2L]
      if(dotprod(q1, q2) < 0){
        q2 <- -q2
      }
      t2 <- t1 + (keyTimes[2L] - keyTimes[1L])
    }else{
      q2 <- keyRotors[idx+2L]
      t2 <- keyTimes[idx+2L]
    }
    slerp_0_1 <- .slerp(q0, q1, (t - t0) / (t1 - t0))
    .slerp(
      .slerp(
        .slerp(q_1, q0, (t - t_1) / (t0 - t_1)),
        slerp_0_1,
        (t - t_1) / (t1 - t_1)
      ),
      .slerp(
        slerp_0_1,
        .slerp(q1, q2, (t - t1) / (t2 - t1)),
        (t - t0) / (t2 - t0)
      ),
      (t - t0) / (t1 - t0)
    )
  }
  evaluate(times)
}
