Quaternions and octonions in R
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Build
Status](https://travis-ci.com/RobinHankin/onion.svg?branch=master)](https://travis-ci.com/RobinHankin/onion)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/onion)](https://cran.r-project.org/package=onion)
[![Rdoc](https://www.rdocumentation.org/badges/version/onion)](https://www.rdocumentation.org/packages/onion)
[![Codecov test
coverage](https://codecov.io/gh/RobinHankin/onion/branch/master/graph/badge.svg)](https://codecov.io/gh/RobinHankin/onion/branch/master)
<!-- badges: end -->

# Overview

The `onion` package provides functionality for working with quaternions
and octonions in R. A detailed vignette is provided in the package.

Informally, the *quaternions*, usually denoted
![\\mathbb{H}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BH%7D "\mathbb{H}"),
are a generalization of the complex numbers represented as a
four-dimensional vector space over the reals. An arbitrary quaternion
![q](https://latex.codecogs.com/png.latex?q "q") represented as

![
q=a + b\\mathbf{i} + c\\mathbf{j}+ d\\mathbf{k}
](https://latex.codecogs.com/png.latex?%0Aq%3Da%20%2B%20b%5Cmathbf%7Bi%7D%20%2B%20c%5Cmathbf%7Bj%7D%2B%20d%5Cmathbf%7Bk%7D%0A "
q=a + b\mathbf{i} + c\mathbf{j}+ d\mathbf{k}
")

where
![a,b,c,d\\in\\mathbb{R}](https://latex.codecogs.com/png.latex?a%2Cb%2Cc%2Cd%5Cin%5Cmathbb%7BR%7D "a,b,c,d\in\mathbb{R}")
and
![\\mathbf{i},\\mathbf{j},\\mathbf{k}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bi%7D%2C%5Cmathbf%7Bj%7D%2C%5Cmathbf%7Bk%7D "\mathbf{i},\mathbf{j},\mathbf{k}")
are the quaternion units linked by the equations

![
\\mathbf{i}^2=
\\mathbf{j}^2=
\\mathbf{k}^2=
\\mathbf{i}\\mathbf{j}\\mathbf{k}=-1.](https://latex.codecogs.com/png.latex?%0A%5Cmathbf%7Bi%7D%5E2%3D%0A%5Cmathbf%7Bj%7D%5E2%3D%0A%5Cmathbf%7Bk%7D%5E2%3D%0A%5Cmathbf%7Bi%7D%5Cmathbf%7Bj%7D%5Cmathbf%7Bk%7D%3D-1. "
\mathbf{i}^2=
\mathbf{j}^2=
\mathbf{k}^2=
\mathbf{i}\mathbf{j}\mathbf{k}=-1.")

which, together with distributivity, define quaternion multiplication.
We can see that the quaternions are not commutative, for while
![\\mathbf{i}\\mathbf{j}=\\mathbf{k}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bi%7D%5Cmathbf%7Bj%7D%3D%5Cmathbf%7Bk%7D "\mathbf{i}\mathbf{j}=\mathbf{k}"),
it is easy to show that
![\\mathbf{j}\\mathbf{i}=-\\mathbf{k}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bj%7D%5Cmathbf%7Bi%7D%3D-%5Cmathbf%7Bk%7D "\mathbf{j}\mathbf{i}=-\mathbf{k}").
Quaternion multiplication is, however, associative (the proof is messy
and long).

Defining

![
\\left( a+b\\mathbf{i} + c\\mathbf{j}+ d\\mathbf{k}\\right)^{-1}=
\\frac{1}{a^2 + b^2 + c^2 + d^2}
\\left(a-b\\mathbf{i} - c\\mathbf{j}- d\\mathbf{k}\\right)
](https://latex.codecogs.com/png.latex?%0A%5Cleft%28%20a%2Bb%5Cmathbf%7Bi%7D%20%2B%20c%5Cmathbf%7Bj%7D%2B%20d%5Cmathbf%7Bk%7D%5Cright%29%5E%7B-1%7D%3D%0A%5Cfrac%7B1%7D%7Ba%5E2%20%2B%20b%5E2%20%2B%20c%5E2%20%2B%20d%5E2%7D%0A%5Cleft%28a-b%5Cmathbf%7Bi%7D%20-%20c%5Cmathbf%7Bj%7D-%20d%5Cmathbf%7Bk%7D%5Cright%29%0A "
\left( a+b\mathbf{i} + c\mathbf{j}+ d\mathbf{k}\right)^{-1}=
\frac{1}{a^2 + b^2 + c^2 + d^2}
\left(a-b\mathbf{i} - c\mathbf{j}- d\mathbf{k}\right)
")

shows that the quaternions are a division algebra: division works as
expected (although one has to be careful about ordering terms).

The *octonions*
![\\mathbb{O}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BO%7D "\mathbb{O}")
are essentially a pair of quaternions, with a general octonion written

![a+b\\mathbf{i}+c\\mathbf{j}+d\\mathbf{k}+e\\mathbf{l}+f\\mathbf{il}+g\\mathbf{jl}+h\\mathbf{kl}](https://latex.codecogs.com/png.latex?a%2Bb%5Cmathbf%7Bi%7D%2Bc%5Cmathbf%7Bj%7D%2Bd%5Cmathbf%7Bk%7D%2Be%5Cmathbf%7Bl%7D%2Bf%5Cmathbf%7Bil%7D%2Bg%5Cmathbf%7Bjl%7D%2Bh%5Cmathbf%7Bkl%7D "a+b\mathbf{i}+c\mathbf{j}+d\mathbf{k}+e\mathbf{l}+f\mathbf{il}+g\mathbf{jl}+h\mathbf{kl}")

(other notations are sometimes used); Baez gives a multiplication table
for the unit octonions and together with distributivity we have a
well-defined division algebra. However, octonion multiplication is not
associative and we have
![x(yz)\\neq (xy)z](https://latex.codecogs.com/png.latex?x%28yz%29%5Cneq%20%28xy%29z "x(yz)\neq (xy)z")
in general.

# Installation

You can install the released version of onion from
[CRAN](https://CRAN.R-project.org) with:

``` r
# install.packages("onion")  # uncomment this to install the package
library("onion")
```

# The `onion` package in use

The basic quaternions are denoted `H1`, `Hi`, `Hj` and `Hk` and these
should behave as expected in R idiom:

``` r
a <- 1:9 + Hi -2*Hj
a
#>    [1] [2] [3] [4] [5] [6] [7] [8] [9]
#> Re   1   2   3   4   5   6   7   8   9
#> i    1   1   1   1   1   1   1   1   1
#> j   -2  -2  -2  -2  -2  -2  -2  -2  -2
#> k    0   0   0   0   0   0   0   0   0
a*Hk
#>    [1] [2] [3] [4] [5] [6] [7] [8] [9]
#> Re   0   0   0   0   0   0   0   0   0
#> i   -2  -2  -2  -2  -2  -2  -2  -2  -2
#> j   -1  -1  -1  -1  -1  -1  -1  -1  -1
#> k    1   2   3   4   5   6   7   8   9
Hk*a
#>    [1] [2] [3] [4] [5] [6] [7] [8] [9]
#> Re   0   0   0   0   0   0   0   0   0
#> i    2   2   2   2   2   2   2   2   2
#> j    1   1   1   1   1   1   1   1   1
#> k    1   2   3   4   5   6   7   8   9
```

Function `rquat()` generates random quaternions:

``` r
a <- rquat(9)
names(a) <- letters[1:9]
a
#>             a          b            c          d          e          f
#> Re  1.2629543  0.4146414 -0.005767173 -1.1476570  0.2522234 -0.2242679
#> i  -0.3262334 -1.5399500  2.404653389 -0.2894616 -0.8919211  0.3773956
#> j   1.3297993 -0.9285670  0.763593461 -0.2992151  0.4356833  0.1333364
#> k   1.2724293 -0.2947204 -0.799009249 -0.4115108 -1.2375384  0.8041895
#>              g           h          i
#> Re -0.05710677 -1.28459935 -0.4333103
#> i   0.50360797  0.04672617 -0.6494716
#> j   1.08576936 -0.23570656  0.7267507
#> k  -0.69095384 -0.54288826  1.1519118
a[6] <- 33
a
#>             a          b            c          d          e  f           g
#> Re  1.2629543  0.4146414 -0.005767173 -1.1476570  0.2522234 33 -0.05710677
#> i  -0.3262334 -1.5399500  2.404653389 -0.2894616 -0.8919211  0  0.50360797
#> j   1.3297993 -0.9285670  0.763593461 -0.2992151  0.4356833  0  1.08576936
#> k   1.2724293 -0.2947204 -0.799009249 -0.4115108 -1.2375384  0 -0.69095384
#>              h          i
#> Re -1.28459935 -0.4333103
#> i   0.04672617 -0.6494716
#> j  -0.23570656  0.7267507
#> k  -0.54288826  1.1519118
cumsum(a)
#>             a          b         c          d          e          f          g
#> Re  1.2629543  1.6775957 1.6718285  0.5241715  0.7763950 33.7763950 33.7192882
#> i  -0.3262334 -1.8661834 0.5384700  0.2490084 -0.6429127 -0.6429127 -0.1393047
#> j   1.3297993  0.4012322 1.1648257  0.8656106  1.3012939  1.3012939  2.3870632
#> k   1.2724293  0.9777089 0.1786996 -0.2328112 -1.4703496 -1.4703496 -2.1613035
#>              h          i
#> Re 32.43468886 32.0013785
#> i  -0.09257857 -0.7420502
#> j   2.15135668  2.8781074
#> k  -2.70419172 -1.5522800
```

## Octonions

Octonions follow the same general pattern and we may show
nonassociativity numerically:

``` r
x <- roct(5)
y <- roct(5)
z <- roct(5)
x*(y*z) - (x*y)*z
#>          [1]           [2]           [3]           [4]           [5]
#> Re  0.000000 -5.329071e-15 -1.776357e-15 -8.881784e-16  8.881784e-16
#> i   7.201225  1.045435e+00 -3.015861e+00 -4.261327e+00  8.612680e+00
#> j   6.177845 -5.797569e+00 -5.642415e+00 -6.342342e+00  1.118819e+01
#> k  -4.917863 -4.484153e+00 -1.591524e+01 -1.119394e+00  1.571936e+01
#> l  -1.403122  1.827970e-01  7.268523e+00 -6.298392e-01 -3.564195e+00
#> il -4.950594  4.440918e+00  9.922722e+00 -7.116999e-01  7.448039e+00
#> jl  5.253879  9.239258e+00  7.195855e+00  4.224830e+00 -4.883673e+00
#> kl -2.031907  1.159402e+01 -1.147093e+01 -1.264476e+00 -2.728531e+00
```

# References

-   RKS Hankin (2006). “Normed division algebras with R: introducing the
    onion package”. *R News*, 6(2):49-52
-   JC Baez (2001). “The octonions”. *Bulletin of the American
    Mathematical Society*, 39(5), 145–205

# Further information

For more detail, see the package vignette

`vignette("onionpaper")`
