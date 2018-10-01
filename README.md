
<!-- README.md is generated from README.Rmd. Please edit that file -->
Lenses for R
============

Installation
------------

``` r
devtools::install_github("cfhammill/lenses")
```

Intro
-----

In typical R use we frequently perform two common operations on our data. We `view` some piece of the data, or we `set` some piece of the data. Base R comes with many pairs of `view` and `set` functions. For example let's take the iris data set

``` r
library(lenses)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

head(iris)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
#> 4          4.6         3.1          1.5         0.2  setosa
#> 5          5.0         3.6          1.4         0.2  setosa
#> 6          5.4         3.9          1.7         0.4  setosa
```

and we'll `view` the 3rd element of the `Sepal.Length` column.

``` r
iris$Sepal.Length[3]
#> [1] 4.7
```

we can equivalently set the same element to a new value

``` r
iris$Sepal.Length[3] <- 10
head(iris, 3)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3         10.0         3.2          1.3         0.2  setosa
```

There are some problems with having our separate `view` and `set` functions. First is that composing these isn't easy. They can't be composed via piping

``` r
iris %>%
  .$Sepal.Length %>%
  `[<-`(20)
#>   [1] 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20
#>  [24] 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20
#>  [47] 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20
#>  [70] 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20
#>  [93] 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20
#> [116] 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20
#> [139] 20 20 20 20 20 20 20 20 20 20 20 20
```

Additionally, if you want to transform the data, not just modify it, you need to specify your chain of accessors twice.

``` r
iris$Sepal.Length[3] <- iris$Sepal.Length[3] * 2
head(iris, 3)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3         20.0         3.2          1.3         0.2  setosa
```

But there is a better way. And that way is lenses.

Using lenses
------------

Lenses give you all the power of R's `view` and `set` functions plus:

-   bidirectionality (you can `set` anything you can `view`)
-   composability (lenses can be put together to `set`/`view` nested data)
-   reduced code duplication
-   non-destructive (lenses won't update your data without your permission)

Here's how we would do the above operations with lenses.

First we'll construct a lens into the third element of `Sepal.Length`:

``` r
sepal_length3 <- index("Sepal.Length") %.% index(3)
```

Here we combined two lenses using the lens composition operator `%.%`.

Now we can view that element with the `view` function:

``` r
iris %>% view(sepal_length3)
#> [1] 20
```

To set this element we can use the `set` function:

``` r
iris %>% set(sepal_length3, 50) %>% head(3)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3         50.0         3.2          1.3         0.2  setosa
```

If we want to apply a function to change the data, we can apply a function `over` the lens:

``` r
iris %>% over(sepal_length3, log) %>% head(3)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1     5.100000         3.5          1.4         0.2  setosa
#> 2     4.900000         3.0          1.4         0.2  setosa
#> 3     2.995732         3.2          1.3         0.2  setosa
```

More interesting lenses
-----------------------

Now you have seen the main lens verbs and operations

1.  `view`: see the subpart of an object a lens is focussed on.
2.  `set`: set the subpart to a particular value, then return the whole object with the subpart updated.
3.  `over`: apply a function to the subpart, then return the whole object with the subpart updated.
4.  `%.%`: compose two lenses to focus on a subpart of a subpart.

Now if all lenses had to offer was more composable indexing of vectors, you might not be interested in integrating them into your workflows. But lenses can do a lot more than just pick and set elements in vectors.

For example, `lenses` provides lens-ified version of `dplyr::select`. Since lenses are "bidirectional", this means you can `set` the results of your selection.

let's select columns between `Sepal.Width` and `Petal.Width` and increment them by 10:

``` r
iris %>%
  over(select_l(Sepal.Width:Petal.Width)
     , function(x){ x + 10 }) %>%
  head(3)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1        13.5         11.4        10.2  setosa
#> 2          4.9        13.0         11.4        10.2  setosa
#> 3         20.0        13.2         11.3        10.2  setosa
```

At this point I can imagine you saying, all this is very clear, but what good is it, I have `mutate`, well that is certainly true. But have you ever wanted to `set` or modify the results of a `filter`? This is not super easy to do in the `dplyr` universe. But our lens-ified `filter` does it with ease.

Let's set all "sepal" columns where the row number is less than three to zero. And for fun let's also change the column names to all upper case:

``` r
iris %>%
  mutate(row_num = seq_len(n())) %>%
  set(filter_l(row_num < 3) %.%
      select_l(matches("Sepal"))
    , 0) %>%
  over(names_l, toupper) %>%
  head(3)
#>   SEPAL.LENGTH SEPAL.WIDTH PETAL.LENGTH PETAL.WIDTH SPECIES ROW_NUM
#> 1            0         0.0          1.4         0.2  setosa       1
#> 2            0         0.0          1.4         0.2  setosa       2
#> 3           20         3.2          1.3         0.2  setosa       3
```

As you can see lenses can be smoothly integrated into your `tidyverse` workflows, as well as your base R workflows.

Polishing your own
------------------

You can make a lens from scratch (!) by passing `view` and `set` functions to the `lens` constructor:

``` r
first_l <- lens(view = function(d) d[[1]],
                set  = function(d, x) { d[[1]] <- x; d })
```

As you see, the `view` function must accept an element of data, while the `set` function must accepts such an element as well as the new value of the subpart, and return the new data in its entirety - thus achieving composability - without modifying the original.

In order to avoid unpleasant surprises or inconsistencies for users, an author of a `lens` (via `lens`) should ensure it obeys the following rules (the "Lenz laws", here paraphrased from [a Haskell lens tutorial](www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)):

1.  View-Set: If you `view` some data with a lens, and then `set` the data with that value, you get the input data back.
2.  Set-View: If you `set` a value with a lens, then `view` that value with the same lens, you get back what you put in.
3.  Set-Set: If you `set` a value into some data with a lens, and then `set` another value with the same lens, it's the same as only doing the second `set`.

"Lenses" which do not satisfy these properties should be documented accordingly. By convention, the few such specimens in this library are suffixed by "\_il" ("illegal lens").

How do they work?
-----------------

As you can see from the `lens` constructor, knowing how to implement `view` and `set` for a lens turns out to be sufficient to implement the other verbs such as `over` and - most importantly - lens composition (`%.%`).

In our implementation, `lens`es are trivial. They simply store the provided functions. A `lens` under the hood is a two element list with an element `set` and an element `view`.

History of lens making
----------------------

There is nothing particularly new about the lenses appearing here. For a fairly comprehensive (and highly technical) history of lenses, see [links here](https://github.com/ekmett/lens/wiki/History-of-Lenses) and [this blog post](https://julesh.com/2018/08/16/lenses-for-philosophers/) .
