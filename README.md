
Elegant Data Manipulation with Lenses
=====================================

Installation
------------

``` r
devtools::install_github("cfhammill/lenses")
```

Intro
-----

When programming in R there are two fundamental operations we perform on our data. We `view` some piece of the data, or we `set` some piece of the data to a particular value. These two operations are so fundamental that R comes with many pairs of `view` and `set` functions. A classic example would be `names`. Names can be viewed `names(x)` and set `names(x) <- new_names`. Lenses are an extension of the idea of `view`/`set` pairs, offering the following advantages:

-   lenses are bidirectional (you can `set` anything you can `view`)
-   they are composable (they can be put together to `set`/`view` nested data)
-   they reduce code duplication
-   they play well with piping - [see R4DS to learn more about piping](http://r4ds.had.co.nz/pipes.html)
-   and they are non-destructive (lenses won't update your data without your permission)

In this document, we'll see a few common data manipulation operations and how they can be improved with lenses.

Simple manipulations
--------------------

Let's take the iris data set for example, we want to perform some manipulations on it.

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

We're curious about the value of the 3rd element of the `Sepal.Length` column. Using base R we can `view` it with:

``` r
iris$Sepal.Length[3]
#> [1] 4.7
```

we can update (set) the value by assigning into it:

``` r
iris$Sepal.Length[3] <- 100
head(iris, 3)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3        100.0         3.2          1.3         0.2  setosa
```

and we can perform some operation to update it:

``` r
iris$Sepal.Length[3] <- log(iris$Sepal.Length[3])
```

This works well, however, there are some problems.

The first problem comes with having our `view` and `set` functions separate. Composing our operations isn't easy, particularly when using pipes:

``` r
iris %>%
  .$Sepal.Length %>%
  `[<-`(3, 20)
#>   [1]  5.1  4.9 20.0  4.6  5.0  5.4  4.6  5.0  4.4  4.9  5.4  4.8  4.8  4.3
#>  [15]  5.8  5.7  5.4  5.1  5.7  5.1  5.4  5.1  4.6  5.1  4.8  5.0  5.0  5.2
#>  [29]  5.2  4.7  4.8  5.4  5.2  5.5  4.9  5.0  5.5  4.9  4.4  5.1  5.0  4.5
#>  [43]  4.4  5.0  5.1  4.8  5.1  4.6  5.3  5.0  7.0  6.4  6.9  5.5  6.5  5.7
#>  [57]  6.3  4.9  6.6  5.2  5.0  5.9  6.0  6.1  5.6  6.7  5.6  5.8  6.2  5.6
#>  [71]  5.9  6.1  6.3  6.1  6.4  6.6  6.8  6.7  6.0  5.7  5.5  5.5  5.8  6.0
#>  [85]  5.4  6.0  6.7  6.3  5.6  5.5  5.5  6.1  5.8  5.0  5.6  5.7  5.7  6.2
#>  [99]  5.1  5.7  6.3  5.8  7.1  6.3  6.5  7.6  4.9  7.3  6.7  7.2  6.5  6.4
#> [113]  6.8  5.7  5.8  6.4  6.5  7.7  7.7  6.0  6.9  5.6  7.7  6.3  6.7  7.2
#> [127]  6.2  6.1  6.4  7.2  7.4  7.9  6.4  6.3  6.1  7.7  6.3  6.4  6.0  6.9
#> [141]  6.7  6.9  5.8  6.8  6.7  6.7  6.3  6.5  6.2  5.9
```

Whoops, that's not what we wanted. Here we see `Sepal.Length` with the third element replaced, but where did the rest of `iris` go! So we lose information when we pipe from a `view` to a `set`.

R's `set`/`view` pairs also can't be composed with function compostion:

``` r
`[<-`(`$`(iris, `Sepal.Length`), 3, 20)
#>   [1]  5.1  4.9 20.0  4.6  5.0  5.4  4.6  5.0  4.4  4.9  5.4  4.8  4.8  4.3
#>  [15]  5.8  5.7  5.4  5.1  5.7  5.1  5.4  5.1  4.6  5.1  4.8  5.0  5.0  5.2
#>  [29]  5.2  4.7  4.8  5.4  5.2  5.5  4.9  5.0  5.5  4.9  4.4  5.1  5.0  4.5
#>  [43]  4.4  5.0  5.1  4.8  5.1  4.6  5.3  5.0  7.0  6.4  6.9  5.5  6.5  5.7
#>  [57]  6.3  4.9  6.6  5.2  5.0  5.9  6.0  6.1  5.6  6.7  5.6  5.8  6.2  5.6
#>  [71]  5.9  6.1  6.3  6.1  6.4  6.6  6.8  6.7  6.0  5.7  5.5  5.5  5.8  6.0
#>  [85]  5.4  6.0  6.7  6.3  5.6  5.5  5.5  6.1  5.8  5.0  5.6  5.7  5.7  6.2
#>  [99]  5.1  5.7  6.3  5.8  7.1  6.3  6.5  7.6  4.9  7.3  6.7  7.2  6.5  6.4
#> [113]  6.8  5.7  5.8  6.4  6.5  7.7  7.7  6.0  6.9  5.6  7.7  6.3  6.7  7.2
#> [127]  6.2  6.1  6.4  7.2  7.4  7.9  6.4  6.3  6.1  7.7  6.3  6.4  6.0  6.9
#> [141]  6.7  6.9  5.8  6.8  6.7  6.7  6.3  6.5  6.2  5.9
```

still not what we want. It has the same problem above.

This is a failure of "bidirectionality", once you've chosen to use a `view` function, or a `set` function, you are locked into that direction.

Lack of composability and bidirectionality means that you frequently have to duplicate your code. For example, if you want to apply an operation to the third element of "Sepal.Length", you need to specify the chain of accessors twice, once in `view` mode, and once in `set` mode, making your code messy and cumbersome:

``` r
iris$Sepal.Length[3] <- iris$Sepal.Length[3] * 2
head(iris, 3)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1      5.10000         3.5          1.4         0.2  setosa
#> 2      4.90000         3.0          1.4         0.2  setosa
#> 3      9.21034         3.2          1.3         0.2  setosa
```

We can fix both of these problems by using lenses.

Using lenses
------------

Lenses give you all the power of R's `view` and `set` functions plus the advantages noted above. Especially important are the composition and bidirectionality features. Each lens can be used with the [`view`](https://cfhammill.github.io/lenses/reference/view.html), and [`set`](https://cfhammill.github.io/lenses/reference/set.html) functions.

Let's revisit the operations we performed above using lenses.

The first thing we will do is construct a lens into the third element of the `Sepal.Length` component of a structure:

``` r
library(lenses)

sepal_length3 <- index("Sepal.Length") %.% index(3)
```

In the above code we're creating two lenses, one into `Sepal.Length` and another into element 3, using the [`index`](https://cfhammill.github.io/lenses/reference/set.html) function. We're then composing these two lenses with [`%.%`](https://cfhammill.github.io/lenses/reference/lens-compose.html) producing a new lens into our element of interest.

Note that this lens has no idea we're going to apply it to `iris`. Lenses are constructed without knowing what data they will be applied to.

Now that we have a lens into the third element of `Sepal.Length`, we can examine the appropriate element of the `iris` dataset with the [`view`](https://cfhammill.github.io/lenses/reference/view.html) function:

``` r
iris %>% view(sepal_length3)
#> [1] 9.21034
```

We can update this element with the [`set`](https://cfhammill.github.io/lenses/reference/set.html) function:

``` r
iris %>% set(sepal_length3, 50) %>% head(3)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3         50.0         3.2          1.3         0.2  setosa
```

And we can apply a function to change the data. To do this we can apply a function [`over`](https://cfhammill.github.io/lenses/reference/over.html) the lens:

``` r
iris %>% over(sepal_length3, log) %>% head(3)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1     5.100000         3.5          1.4         0.2  setosa
#> 2     4.900000         3.0          1.4         0.2  setosa
#> 3     2.220327         3.2          1.3         0.2  setosa
```

Note that we never had to respecify what subpart we wanted, the lens kept track for us. We saw that the same lens can be used to both [`view`](https://cfhammill.github.io/lenses/reference/view.html) and [`set`](https://cfhammill.github.io/lenses/reference/set.html), and that they can be composed easily with [`%.%`](https://cfhammill.github.io/lenses/reference/lens-compose.html).

More interesting lenses
-----------------------

Now you have seen the main lens verbs and operations

1.  [`view`](https://cfhammill.github.io/lenses/reference/view.html): see the subpart of an object a lens is focussed on.
2.  [`set`](https://cfhammill.github.io/lenses/reference/set.html): set the subpart to a particular value, then return the whole object with the subpart updated.
3.  [`over`](https://cfhammill.github.io/lenses/reference/over.html): apply a function to the subpart, then return the whole object with the subpart updated.
4.  [`%.%`](https://cfhammill.github.io/lenses/reference/lens-compose.html): compose two lenses to focus on a subpart of a subpart.

Now if all lenses had to offer was more composable indexing of vectors, you might not be interested in integrating them into your workflows. But lenses can do a lot more than just pick and set elements in vectors.

For example, this package provides lens-ified version of `dplyr::select`. Unlike `select`, [`select_l`](https://cfhammill.github.io/lenses/reference/select_l.html) is bidirectional. This means you can [`set`](https://cfhammill.github.io/lenses/reference/set.html) the results of your selection.

let's select columns between `Sepal.Width` and `Petal.Width` and increment them by 10:

``` r
iris %>%
  over(select_l(Sepal.Width:Petal.Width)
     , ~ . + 10
       ) %>%
  head(3)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1      5.10000        13.5         11.4        10.2  setosa
#> 2      4.90000        13.0         11.4        10.2  setosa
#> 3      9.21034        13.2         11.3        10.2  setosa
```

Not only does [`select_l`](https://cfhammill.github.io/lenses/reference/select_l.html) create the appropriate lens for you with `dplyr::select` style column references, but [`over`](https://cfhammill.github.io/lenses/reference/over.html) allows us to declare anonymous functions like in `purrr`.

At this point I can imagine you saying, all this is very clear, but what good is it, I have `mutate`. Well that is a good point. It is hard to beat the convenience of `mutate`. However, `select_l` has an advantage, it can be used on any named object:

``` r
iris %>%
  as.list %>%
  view(select_l(matches("Sepal")) %.%
       index(1) %.%
       index(1)
       ) 
#> [1] 5.1
```

You can use it with vectors, lists, data.frames, etc.

If `select_l` isn't enticing enough, have you ever wanted to `set` or modify the results of a `filter`? This is not super easy to do in the `dplyr` universe. But our `lens`ified `filter`, [`filter_l`](https://cfhammill.github.io/lenses/reference/filter_l.html) does this with ease.

Let's set all "Sepal" columns where the row number is less than three to zero. And for fun let's also change the column names to all upper case:

``` r
library(dplyr)

iris %>%
  mutate(row_num = seq_len(n())) %>%
  set(filter_l(row_num < 3) %.%
      select_l(matches("Sepal"))
    , 0) %>%
  over(names_l, toupper) %>%
  head(3)
#>   SEPAL.LENGTH SEPAL.WIDTH PETAL.LENGTH PETAL.WIDTH SPECIES ROW_NUM
#> 1      0.00000         0.0          1.4         0.2  setosa       1
#> 2      0.00000         0.0          1.4         0.2  setosa       2
#> 3      9.21034         3.2          1.3         0.2  setosa       3
```

You can even use mutate `over` your `filter_l`

``` r
iris %>%
  mutate(row_num = seq_len(n())) %>%
  over(filter_l(row_num < 3)
     , ~ mutate(., Sepal.Length = 0)) %>%
  head(3)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species row_num
#> 1      0.00000         3.5          1.4         0.2  setosa       1
#> 2      0.00000         3.0          1.4         0.2  setosa       2
#> 3      9.21034         3.2          1.3         0.2  setosa       3
```

As you can see, lenses can be smoothly integrated into your `tidyverse` workflows, as well as your base R workflows. Giving you the powers of compositionality and bidirectionality to improve your code.

Mapping lenses
--------------

Frequently we end up in situations where we want to modify each element of a nested object. This is especially cumbersome without lenses. Let's imagine our data lives inside a larger structure. And additionally that it isn't a nice data frame, but a list.

``` r
packed_iris <- list(as.list(iris))

packed_iris %>% str(2)
#> List of 1
#>  $ :List of 5
#>   ..$ Sepal.Length: num [1:150] 5.1 4.9 9.21 4.6 5 ...
#>   ..$ Sepal.Width : num [1:150] 3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
#>   ..$ Petal.Length: num [1:150] 1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
#>   ..$ Petal.Width : num [1:150] 0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
#>   ..$ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
```

say I want to add 10 to the first element of each column between `Sepal.Length` and `Petal.Width`. Base R I might do something like:

``` r
els_of_interest <-
  grep("Sepal|Petal", names(packed_iris[[1]]), value = TRUE)

packed_iris[[1]][1:4] <-
  lapply(packed_iris[[1]][1:4]
       , function(x){ x[1] <- x[1] + 10; x })

str(packed_iris, 2)
#> List of 1
#>  $ :List of 5
#>   ..$ Sepal.Length: num [1:150] 15.1 4.9 9.21 4.6 5 ...
#>   ..$ Sepal.Width : num [1:150] 13.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
#>   ..$ Petal.Length: num [1:150] 11.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
#>   ..$ Petal.Width : num [1:150] 10.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
#>   ..$ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
```

pretty ugly right?

To do this with lenses we can use the `map_l` function to promote a `lens` to apply to each element of a list.

``` r
els_l <-
  index(1) %.%
  select_l(Sepal.Length:Petal.Width) %.%
  map_l(index(1))

map_over(packed_iris, els_l, ~ . + 10) %>%
  str(2)
#> List of 1
#>  $ :List of 5
#>   ..$ Sepal.Length: num [1:150] 25.1 4.9 9.21 4.6 5 ...
#>   ..$ Sepal.Width : num [1:150] 23.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
#>   ..$ Petal.Length: num [1:150] 21.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
#>   ..$ Petal.Width : num [1:150] 20.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
#>   ..$ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
```

Here we use the `map_over` function to apply a function to each element, you could equivalently use `over` with `lapply` as well. As you can see setting and applying functions to multiple elements of nested data is dramatically improved by using lenses.

Polishing your own
------------------

You can make a lens from scratch (!) by passing `view` and `set` functions to the [`lens`](https://cfhammill.github.io/lenses/reference/lens.html) constructor:

``` r
first_l <- lens(view = function(d) d[[1]],
                set  = function(d, x) { d[[1]] <- x; d })
```

As you can see, the `view` function must accept an element of data, while the `set` function must accept such an element as well as the new value of the subpart, and return the new data in its entirety - thus achieving composability - without modifying the original.

In order to avoid unpleasant surprises or inconsistencies for users, an author of a `lens` (via [`lens`](https://cfhammill.github.io/lenses/reference/lens.html)) should ensure it obeys the following rules (the "Lenz laws", here paraphrased from [a Haskell lens tutorial](www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)):

1.  View-Set: If you `view` some data with a lens, and then `set` the data with that value, you get the input data back.
2.  Set-View: If you `set` a value with a lens, then `view` that value with the same lens, you get back what you put in.
3.  Set-Set: If you `set` a value into some data with a lens, and then `set` another value with the same lens, it's the same as only doing the second `set`.

"Lenses" which do not satisfy these properties should be documented accordingly. By convention, the few such specimens in this library are suffixed by "\_il" ("illegal lens"). See the package [reference](https://cfhammill.github.io/lenses/reference/) for more.

How do they work?
-----------------

As you can see from the `lens` constructor, knowing how to implement `view` and `set` for a lens turns out to be sufficient to implement the other verbs such as `over` and - most importantly - lens composition (`%.%`).

In our implementation, lenses are trivial. They simply store the provided functions. A `lens` under the hood is a two element list with an element `view` and an element `set`.

History of lens making
----------------------

There is nothing particularly new about the lenses appearing here. For a fairly comprehensive (and highly technical) history of lenses, see [links here](https://github.com/ekmett/lens/wiki/History-of-Lenses) and [this blog post](https://julesh.com/2018/08/16/lenses-for-philosophers/) .

------------------------------------------------------------------------

Thanks to Leigh Spencer Noakes, Zsu Lindenmaier, and Lily Qiu for reading drafts of this document and providing very helpful feedback.
