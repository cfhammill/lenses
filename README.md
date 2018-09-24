# fresnel

A small lens library for R. Aims to make working with nested data structures
a little bit easier.

## Installation

``` r
 install_github("cfhammill/fresnel")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
lget(1:10, index(4)) # returns 4
lset(1:10, index(1), 10) # returns c(10, 2:10)
```

Lenses can be composed together to access objects nested
within other objects

```r
# Inconveniently replace the first element of list
# element b with 100
lset(list(a = 1, b = 1:3, c = 3)
   , index("b") %.% index(1)
   , 100)
```
