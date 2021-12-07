library(onion)
library(rgl)

unitSphere <- subdivision3d(icosahedron3d(), depth = 4)
unitSphere$vb[4, ] <-
  apply(unitSphere$vb[1:3, ], 2, function(x) sqrt(sum(x * x)))
unitSphere$normals <- unitSphere$vb
drawSphere <- function(center=c(0, 0, 0), radius = 1, ...){
  sphere <- scale3d(unitSphere, radius, radius, radius)
  shade3d(translate3d(sphere, center[1], center[2], center[3]), ...)
}

# helper function: spherical to Cartesian coordinates
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
#   quaternion and rotor i sends the key point i-1 to the key point i
keyRotors <- quaternion(length.out = n_keyPoints)
rotor <- keyRotors[1L] <- H1
for(i in seq_len(n_keyPoints - 1L)){
  keyRotors[i+1L] <- rotor <-
    quaternionFromTo(keyPoints[i, ]/5, keyPoints[i+1L, ]/5) * rotor
}

Spline <- function(tcb){
  # Kochanek-Bartels quaternions spline
  rotors <- KochanekBartels(
    keyRotors, n_intertimes = 25L, endcondition = "closed", tcb=tcb
  )
  # construction of the interpolating points on the sphere
  points <- matrix(nrow = 0L, ncol = 3L)
  keyPoint1 <- rbind(keyPoints[1L, ])
  for(i in seq_along(rotors)){
    points <- rbind(points, rotate(keyPoint1, rotors[i]))
  }
  points
}

open3d(windowRect = c(50, 50, 562, 562), zoom = 0.9)
bg3d(rgb(54, 57, 64, maxColorValue = 255))
continuity <- seq(0, 5, by = 0.2)
for(i in seq_along(continuity)){
  drawSphere(radius = 5, color = "navy")
  points <- Spline(tcb = c(-1, continuity[i], 0))
  spheres3d(points, radius = 0.2, color = "yellow")
  spheres3d(keyPoints, radius = 0.25, color = "red")
  rgl.snapshot(sprintf("zpic_%03d.png", i))
  clear3d()
}


command <- 
  "magick convert -dispose previous -loop 0 -delay 8 -duplicate 1,-2-1 zpic_*.png KB.gif"
system(command)
