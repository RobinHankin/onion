library(onion)

# spherical to Cartesian coordinates
sph2cart <- function(rho, theta, phi){
  return(c(
    rho * cos(theta) * sin(phi),
    rho * sin(theta) * sin(phi),
    rho * cos(phi)
  ))
}

# construction of the key points on the sphere
keyPoints <- matrix(nrow = 0L, ncol = 3L)
theta_ <- seq(0, 2*pi, length.out = 9L)[-1L]
phi <- 1.3
for(theta in theta_){
  keyPoints <- rbind(keyPoints, sph2cart(5, theta, phi))
  phi = pi - phi
}

n_keyPoints <- nrow(keyPoints)

# construction of the key rotors; the first key rotor is the identity
#   quaternion and rotor i sends the key point 1 to the key point i
keyRotors <- quaternion(length.out = n_keyPoints)
rotor <- keyRotors[1L] <- H1
for(i in seq_len(n_keyPoints - 1L)){
  keyRotors[i+1L] <- rotor <-
    quaternionFromTo(keyPoints[i, ]/5, keyPoints[i+1L, ]/5) * rotor
}

Spline <- function(tcb){
  # Kochanek-Bartels quaternions spline
  rotors <- KochanekBartels(
    keyRotors, n_intertimes = 15L, endcondition = "closed", tcb = tcb
  )
  # construction of the interpolating points on the sphere
  points <- matrix(nrow = 0L, ncol = 3L)
  keyPoint1 <- rbind(keyPoints[1L, ])
  for(i in seq_along(rotors)){
    points <- rbind(points, rotate(keyPoint1, rotors[i]))
  }
  points
}

shinyServer(
  function(input, output, session){

    observeEvent(input[["run"]], {
      spline <- 
        Spline(c(input[["numt"]], input[["numc"]], input[["numb"]]))
      session$sendCustomMessage("spline", spline)
    })   

  }
)
