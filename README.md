
<!-- README.md is generated from README.Rmd. Please edit that file -->
Lenses for R
============

Installation
------------

``` r
devtools::install_github("cfhammill/focal")
```

Intro
-----

In typical R use we frequently perform two common operations on our data. We `view` some piece of the data, or we `set` some piece of the data. Base R comes with many pairs of `view` and `set` functions. For example let's take the iris data set

``` r
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

But there is a better way. And that way is lenses. Lenses give you all the power of R's `view` and `set` functions and gives you composability, and a way to reduce code duplication. Here's how we would do this with lenses. First we'll construct a lens into the third element of `Sepal.Length`

``` r
sepal_length3 <- index("Sepal.Length") %.% index(3)
```

Here we combined two lenses using the lens composition operator `%.%`.

Now to `view` we use the view function

``` r
iris %>% view(sepal_length3)
#> [1] 20
```

To set we use the set function

``` r
iris %>% set(sepal_length3, 50) %>% head(3)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3         50.0         3.2          1.3         0.2  setosa
```

If we want to apply a function to the data `over` the lens

``` r
iris %>% over(sepal_length3, log) %>% head(3)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1     5.100000         3.5          1.4         0.2  setosa
#> 2     4.900000         3.0          1.4         0.2  setosa
#> 3     2.995732         3.2          1.3         0.2  setosa
```

An additional benefit to lenses is that they are non-destructive. The underlying data-set is never changed, so you get complete control of when, if ever, you mutate the raw data. Now if all lenses had to offer was more composable indexing, you might not be interested in integrating them into your workflows. But lenses can do a lot more than just pick and set elements in vectors.

More interesting lenses.
------------------------

We have a lensified `dplyr::select`, let's select columns between `Sepal.Width` and `Petal.Width` and increment them by 10.

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

I can imagine you saying, what good is that, I have `mutate`, well that is certainly true. But have you ever wanted to `set` or modify the results of a filter? Let's set all "sepal" columns where the row number is less than 3 to zero. Let's also change the column names to all upper case.

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

As you can see lenses can be smoothly integrated into your `tidyverse` workflows, or your base R workflows.

Polishing your own
------------------

You can make a lens from scratch (!) by passing `view` and `set` functions to the `lens` constructor:

``` r
first_l <- lens(view = function(d) d[[1]],
                set  = function(d, x) { d[[1]] <- x; d })
```

As you see, the `view` function must accept an element of data, while the `set` function must accepts such an element as well as the new value of the subpart, and return the new data in its entirety - thus achieving composability - without modifying the original.

In order to avoid unpleasant surprises or inconsistencies for users, an author of a `lens` (via `lens`) should ensure it obeys the following rules (the "Lenz laws", here paraphrased from [a Haskell lens tutorial](www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)):

1.  Get-Put: If you get (view) some data with a lens, and then modify (set) the data with that value, you get the input data back.
2.  Put-Get: If you put (set) a value into some data with a lens, then get that value with the lens, you get back what you put in.
3.  Put-Put: If you put a value into some data with a lens, and then put another value with the same lens, it's the same as only doing the second put.

"Lenses" which do not satisfy these properties should be documented accordingly. By convention, the few such specimens in this library are suffixed by "\_il" ("illegal lens").

How do they work?
-----------------

As you can see from the `lens` constructor, knowing how to implement `view` and `set` for a lens turns out to be sufficient to implement the other verbs such as `over` and - most importantly - lens composition (`%.%`). In fact, our implementation of `lens` is trivial: it simply stores the provided functions.

History of lens making
----------------------

There is nothing particularly new about the lenses appearing here. For a fairly comprehensive (and highly technical) history of lenses, see [links here](https://github.com/ekmett/lens/wiki/History-of-Lenses) and [this blog post](https://julesh.com/2018/08/16/lenses-for-philosophers/) .
