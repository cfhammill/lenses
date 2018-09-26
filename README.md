# Focal

A small lens library for R. Aims to make working with nested data structures
a little bit easier.

## Installation

``` r
 install_github("cfhammill/focal")
```

## Example

Use `view` and `set` to access or modify data with an appropriate lens:

``` r
view(1:10, index(4))      # returns 4
set(1:10, index(1), 10)  # returns c(10, 2:10)
```

Lenses can be composed together to access objects nested
within other objects:

```r
# Inconveniently replace the first element of list
# element b with 100
set(list(a = 1, b = 1:3, c = 3)
   , index("b") %.% index(1)
   , 100)
```
