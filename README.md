Quaternions and octonions in R
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Build
Status](https://travis-ci.org/RobinHankin/onion.svg?branch=master)](https://travis-ci.org/RobinHankin/onion)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/onion)](https://cran.r-project.org/package=onion)
[![Rdoc](http://www.rdocumentation.org/badges/version/onion)](http://www.rdocumentation.org/packages/onion)
<!-- badges: end -->

# Overview

The `onion` package provides functionality for working with quaternions
and octonions in R. A detailed vignette is provided in the package.

Informally, the *quaternions*, usually denoted
![\\mathbb{H}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BH%7D
"\\mathbb{H}"), are a generalization of the complex numbers represented
as a four-dimensional vector space over the reals. An arbitrary
quaternion ![q](https://latex.codecogs.com/png.latex?q "q") represented
as

  
![
q=a + b\\mathbf{i} + c\\mathbf{j}+ d\\mathbf{k}
](https://latex.codecogs.com/png.latex?%0Aq%3Da%20%2B%20b%5Cmathbf%7Bi%7D%20%2B%20c%5Cmathbf%7Bj%7D%2B%20d%5Cmathbf%7Bk%7D%0A
"
q=a + b\\mathbf{i} + c\\mathbf{j}+ d\\mathbf{k}
")  

where
![a,b,c,d\\in\\mathbb{R}](https://latex.codecogs.com/png.latex?a%2Cb%2Cc%2Cd%5Cin%5Cmathbb%7BR%7D
"a,b,c,d\\in\\mathbb{R}") and
![\\mathbf{i},\\mathbf{j},\\mathbf{k}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bi%7D%2C%5Cmathbf%7Bj%7D%2C%5Cmathbf%7Bk%7D
"\\mathbf{i},\\mathbf{j},\\mathbf{k}") are the quaternion units linked
by the equations

  
![
\\mathbf{i}^2=
\\mathbf{j}^2=
\\mathbf{k}^2=
\\mathbf{i}\\mathbf{j}\\mathbf{k}=-1.](https://latex.codecogs.com/png.latex?%0A%5Cmathbf%7Bi%7D%5E2%3D%0A%5Cmathbf%7Bj%7D%5E2%3D%0A%5Cmathbf%7Bk%7D%5E2%3D%0A%5Cmathbf%7Bi%7D%5Cmathbf%7Bj%7D%5Cmathbf%7Bk%7D%3D-1.
"
\\mathbf{i}^2=
\\mathbf{j}^2=
\\mathbf{k}^2=
\\mathbf{i}\\mathbf{j}\\mathbf{k}=-1.")  

which, together with distributivity, define quaternion multiplication.
We can see that the quaternions are not commutative, for while
![\\mathbf{i}\\mathbf{j}=\\mathbf{k}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bi%7D%5Cmathbf%7Bj%7D%3D%5Cmathbf%7Bk%7D
"\\mathbf{i}\\mathbf{j}=\\mathbf{k}"), it is easy to show that
![\\mathbf{j}\\mathbf{i}=-\\mathbf{k}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bj%7D%5Cmathbf%7Bi%7D%3D-%5Cmathbf%7Bk%7D
"\\mathbf{j}\\mathbf{i}=-\\mathbf{k}"). Quaternion multiplication is,
however, associative (the proof is messy and long).

Defining

  
![
\\left( a+b\\mathbf{i} + c\\mathbf{j}+ d\\mathbf{k}\\right)^{-1}=
\\frac{1}{a^2 + b^2 + c^2 + d^2}
\\left(a-b\\mathbf{i} - c\\mathbf{j}- d\\mathbf{k}\\right)
](https://latex.codecogs.com/png.latex?%0A%5Cleft%28%20a%2Bb%5Cmathbf%7Bi%7D%20%2B%20c%5Cmathbf%7Bj%7D%2B%20d%5Cmathbf%7Bk%7D%5Cright%29%5E%7B-1%7D%3D%0A%5Cfrac%7B1%7D%7Ba%5E2%20%2B%20b%5E2%20%2B%20c%5E2%20%2B%20d%5E2%7D%0A%5Cleft%28a-b%5Cmathbf%7Bi%7D%20-%20c%5Cmathbf%7Bj%7D-%20d%5Cmathbf%7Bk%7D%5Cright%29%0A
"
\\left( a+b\\mathbf{i} + c\\mathbf{j}+ d\\mathbf{k}\\right)^{-1}=
\\frac{1}{a^2 + b^2 + c^2 + d^2}
\\left(a-b\\mathbf{i} - c\\mathbf{j}- d\\mathbf{k}\\right)
")  

shows that the quaternions are a division algebra: division works as
expected (although one has to be careful about ordering terms).

The *octonions*
![\\mathbb{O}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BO%7D
"\\mathbb{O}") are essentially a pair of quaternions, with a general
octonion written

  
![
a + b\\mathbf{i} + c\\mathbf{j}+ d\\mathbf{k} +
e\\mathbf{l} + f\\mathbf{il} + g\\mathbf{jl}+ h\\mathbf{kl}
](https://latex.codecogs.com/png.latex?%0Aa%20%20%20%20%20%20%20%20%20%20%20%2B%20b%5Cmathbf%7Bi%7D%20%2B%20c%5Cmathbf%7Bj%7D%2B%20d%5Cmathbf%7Bk%7D%20%2B%0Ae%5Cmathbf%7Bl%7D%20%2B%20f%5Cmathbf%7Bil%7D%20%2B%20g%5Cmathbf%7Bjl%7D%2B%20h%5Cmathbf%7Bkl%7D%0A
"
a           + b\\mathbf{i} + c\\mathbf{j}+ d\\mathbf{k} +
e\\mathbf{l} + f\\mathbf{il} + g\\mathbf{jl}+ h\\mathbf{kl}
")  

(other notations are sometimes used); Baez gives a multiplication table
for the unit octonions and together with distributivity we have a
well-defined division algebra. However, octonion multiplication is not
associative and we have ![x(yz)\\neq
(xy)z](https://latex.codecogs.com/png.latex?x%28yz%29%5Cneq%20%28xy%29z
"x(yz)\\neq (xy)z") in general.

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
#>    a b c d e f g h i
#> Re 2 1 3 3 2 3 1 1 2
#> i  1 2 2 1 2 1 1 1 1
#> j  4 1 2 1 2 3 1 2 4
#> k  3 3 3 1 2 1 2 2 3
a[6] <- 33
a
#>    a b c d e  f g h i
#> Re 2 1 3 3 2 33 1 1 2
#> i  1 2 2 1 2  0 1 1 1
#> j  4 1 2 1 2  0 1 2 4
#> k  3 3 3 1 2  0 2 2 3
cumsum(a)
#>    a b c  d  e  f  g  h  i
#> Re 2 3 6  9 11 44 45 46 48
#> i  1 3 5  6  8  8  9 10 11
#> j  4 5 7  8 10 10 11 13 17
#> k  3 6 9 10 12 12 14 16 19
```

## Octonions

Octonions follow the same general pattern and we may show
nonassociativity numerically:

``` r
x <- roct(5)
y <- roct(5)
z <- roct(5)
x*(y*z) - (x*y)*z
#>     [1]  [2]  [3]  [4]  [5]
#> Re    0    0    0    0    0
#> i   360   50  392  -80  328
#> j  -244  514  554  546 -248
#> k   -96 -472 -250  -50   82
#> l  -384  468 -986 -388  264
#> il  208  106 -216  936  -38
#> jl  144 -446  528 -720 -396
#> kl  -92  -28  180 -308   -6
```

# References

  - RKS Hankin (2006). “Normed division algebras with R: introducing the
    onion package”. *R News*, 6(2):49-52
  - JC Baez (2001). “The octonions”. *Bulletin of the American
    Mathematical Society*, 39(5), 145–205

# Further information

For more detail, see the package vignette

`vignette("onionpaper")`
