#' Construct a lens
#'
#' A `lens` represents the process of focussing on a specific part of a data structure.
#' We represent this via a `lget` function and
#' an `lset` function, roughly corresponding to object-oriented
#' "getters" and "setters" respectively.
#' Lenses can be composed to access or modify deeply nested
#' structures.
#'
#' @param lget A function that takes a data structure of a certain type
#' and returns a subpart of that structure
#' @param lset A function that takes a data structure of a certain type
#' and a value and returns a new data structure with the given subpart
#' replaced with the given value.  Note that `lset` should not modify
#' the original data.
#' @details Lenses are popular in functional programming because
#' they allow you to build pure, compositional, and re-usable "getters" and "setters".
#'
#' An author of a `lens` (via `lens`) should ensure it obeys the following
#' rules (the "Lens laws", here paraphrased from
#' www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial):
#'
#' 1. Get-Put: If you get (lget) some data with a lens, and then
#' modify (lset) the data with that value, you get the input data back.
#' 2. Put-Get: If you put (lset) a value into some data with a lens,
#' then get that value with the lens, you get back what you put in.
#' 3. Put-Put: If you put a value into some data with a lens, and
#' then put another value with the same lens, it's the same as only
#' doing the second put.
#'
#' There is nothing particularly new about the lenses appearing here.
#' For a fairly comprehensive (and rather technical) history of lenses, see
#' https://github.com/ekmett/lens/wiki/History-of-Lenses and
#' https://julesh.com/2018/08/16/lenses-for-philosophers/ .
#'
#' @examples
#'   lget(1:10, index_l(4)) # returns 4
#'   lset(1:10, index_l(1), 10) # returns c(10, 2:10)
#' @export
lens <- function(lget, lset){
  structure(
    list(lget = lget
       , lset = lset)
  , class = "lens")
}

#' Bind data to a lens
#'
#' To flatten lens composition, you can prespecify the data
#' the lens with be applied to by constructing an objectoscope.
#' These can be integrated easily with normal data pipelines.
#'
#' @param d The data for interest
#' @param l The lens to bind the data to. Defaults to the identity lens
#' @examples
#' list(a = 5, b = 1:3, c = 8) %>%
#'   oscope()   %.%
#'   index_l("b") %.%
#'   index_l(1)   %>%
#'   lset(10)
#' @export
oscope <- function(d, l = id_l){
  structure(
    list(data = d, lens = l)
  , class = "oscope")
}

#' Compose lenses
#'
#' Compose two lenses to produce a new lens which represents
#' focussing first with the first lens, then with the second.
#' A `lget` using the resulting composite lens will first `lget` using
#' the first, then the second, while an `lset` will `lget` via the first lens,
#' `lset` into the resulting piece with the second, and then replace the
#' updated structure in the first with `lset`.  Lens composition
#' is analogous to the `.` syntax of object-oriented programming or to
#' a flipped version of function composition.
#'
#' @rdname lens-compose
#' @param l the first [lens] (or an [oscope])
#' @param m the second lens
#' @examples
#'   lst <- list(b = c(3,4,5))
#'   lns <- index_l("b") %.% index_l(2)
#'   lst %>% lget(lns)                 # returns 4
#'   lst %>% lset(lns, 1)              # returns list(b = c(3,2,5))
#'   lst                               # returns list(b = c(3,4,5))
#' @export
`%.%` <- function(l, m) UseMethod("%.%")

#' @method "%.%" lens 
#' @export
`%.%.lens` <- function(l, m){
  if(!inherits(m, "lens"))
    stop("the second argument of lens composition must be a lens")
  
  lens(function(d) m$lget(l$lget(d))
     , lset = function(d, x){
       l$lset(d, m$lset(l$lget(d), x))
       })
}

#' @method "%.%" oscope
#' @export
`%.%.oscope` <- function(l, m){
  if(!inherits(m, "lens"))
    stop("the second argument of lens composition must be a lens")

  oscope(l$data, l$lens %.% m)
}

#' Modify data with a lens
#'
#' Set the subcomponent of the data referred to by a lens
#' with a new value. See [lens] for details.  Merely dispatches
#' to the `lset` component of the lens.
#'
#' @param d the data, or an [oscope]
#' @param l the lens, or in the case of an `oscope`, the replacement
#' @param x the replacement value, or nothing in the case of an `oscope`
#' @export
lset <- function(d,l,x) UseMethod("lset", d)

#' @method lset default
#' @export
lset.default <- function(d, l, x){
  if(!inherits(l,"lens"))
    stop("second argument of lset must be a lens")

  l$lset(d, x)
}

#' @method lset oscope
#' @export
lset.oscope <- function(d,l,x){
  if(!missing("x"))
    stop("Argument `x` cannot be used with `lset` and an `oscope`")

  lset(d$data, d$lens, l)
}

#' Lget data with a lens
#'
#' Lget the subcomponent of the data referred to by a lens.  This function
#' merely dispatches to the `lget` component of the lens. 
#'
#' @param d the data
#' @param l the lens
#' @export
lget <- function(d, l) UseMethod("lget")

#' @method lget default
#' @export
lget.default <- function(d, l){
  if(!inherits(l, "lens"))
    stop("lget is only defined for lenses")

  l$lget(d)
}

#' @method lget oscope
#' @export
lget.oscope <- function(d, l){
  if(!missing("l"))
    stop("Argument `l` cannot be used with `lget` and an `oscope`")

  lget(d$data, d$lens)
}

#' Map a function over a lens
#'
#' Get the data pointed to by a lens, apply a function
#' and replace it with the result.
#'
#' @param d the data (or an [oscope])
#' @param l the lens (or the function if `d` is an `oscope`)
#' @param f the function (or nothing if `d` is an `oscope`)
#' @examples
#' third_l <- index(3)
#' over(1:5, third_l, function(x) x + 2)
#' # returns c(1:2, 5, 4:5)  
#' @export
over <- function(d, l, f) UseMethod("over")

#' @method over default
#' @export
over.default <- function(d, l, f){
  lset(d, l, f(lget(d, l)))
}

#' @method over oscope
#' @export
over.oscope <- function(d, l, f){
  if(!missing("f"))
    stop("Argument `f` cannot be used with `over` and an `oscope`")

  over(d$data, d$lens, l)
}

#' The identity (trivial lens)
#'
#' This lens focuses on the whole object
#' @export
id_l <- lens(identity, function(., x) x)

#' Construct a lens into an index/name
#'
#' This is the lens version of `[[`
#'
#' @param el The element the lens should point to
#' can be an `integer` or name.
#' @export
index_l <- function(el){
  lens(lget = function(d) d[[el]]
       , lset = function(d, x){
         d[[el]] <- x
         d
       })
}

#' @describeIn index_l shorthand
#' @export
index <- index_l

#' Construct a lens into a subset of an object
#'
#' This is the lens version of `[`
#'
#' @param els a subset vector, can be `integer`, `character`
#' of `logical`, pointing to one or more elements of the object
#' @export
indexes_l <- function(els){
  lens(lget = function(d) d[els]
       , lset = function(d, x){
         d[els] <- x
         d
       })
}

#' @describeIn indexes_l shorthand
#' @export
indexes <- indexes_l

#' A lens into the names of an object
#'
#' The lens versions of `names` and `names<-`.
#' @export
names_l <- lens(lget = names
              , lset = `names<-`)

#' A lens into the column names of an object
#'
#' The lens version of `colnames` and `colnames<-`
#' @export
colnames_l <- lens(lget = colnames
                  , lset = `colnames<-`)

#' A lens into the row names of an object
#'
#' The lens version of `rownames` and `rownames<-`
#' @export
rownames_l <- lens(lget = rownames
                  , lset = `rownames<-`)

#' Construct a lens into an attribute
#'
#' The lens version of `attr` and `attr<-`
#' @param attrib A length one character vector indicating
#' the attribute to lens into.
#' @export
attr_l <- function(attrib){
  lens(lget = function(d) attr(d, attrib)
       , lset = function(d, x) `attr<-`(d, attrib, x))
}

#' Environment lens
#'
#' A lens into the environment of an object. This
#' is the lens version of [environment] and [environment<-]
#' @export
env_l <- lens(environment, `environment<-`)

#' Tidyselect elements by name
#'
#' Create a lens into a named collection to be lgeted or replaced.
#' Names of the input are not changed. This generalizes [dplyr::select]
#' to arbitrary named collections and allows updating.
#' @param ... An expression to be interpretted by [tidyselect::vars_select]
#' which is the same interpretter as [dplyr::select]
#' @examples
#' lets <- setNames(seq_along(LETTERS), LETTERS)
#' lset(lets, select_l(G:F, A, B), 1:4) # A and B are 3,4 for a quick check
#' @export
select_l <- function(...){
  dots <- rlang::quos(...)
  lens(
    lget = function(d){
      vars <- tidyselect::vars_select(names(d), !!!dots)
      d[vars]
    }
  , lset = function(d,x){
    vars <- tidyselect::vars_select(names(d), !!!dots)
    d[vars] <- x
    d
  })
}

#' Row lens
#'
#' Create a lens into a set of rows
#'
#' @param rows the rows to focus on
#' @param drop whether or not to drop dimensions with length 1 
#' @export
rows_l <- function(rows, drop = FALSE){
  lens(lget = function(d) d[rows, ,drop = drop]
     , lset = function(d, x){
       d[rows, ] <- x
       d
     })
}

#' Column lens
#'
#' Create a lens into a set of columns
#'
#' @param cols the columns to focus on
#' @param drop whether or not to drop dimensions with length 1
#' @export
cols_l <- function(cols, drop = FALSE){
  lens(lget = function(d) d[, cols,drop = FALSE]
     , lset = function(d, x){
       d[,cols] <- x
       d
     })
}

#' Slice lens
#'
#' Create a lens into a specific slice of a specific
#' dimension of a multidimensional object. Not to be
#' confused with dplyr slice.
#' 
#' @param dimension the dimension to slice
#' @param slice the slice index
#' @param drop whether or not to drop dimensions with length 1 
#' @export
slice_l <- function(dimension, slice, drop = FALSE){
  getter <-
    function(d){
      al <- Reduce(function(acc, x){
        c(acc[-1], alist(,))
      }, dim(d), init = list())[-1]
      
      al[dimension] <- slice
      args <- c(list(d), al, list(drop = drop))
      do.call(`[`, args)
    }

  setter <-
    function(d, x){
      al <- Reduce(function(acc, x){
        c(acc[-1], alist(,))
      }, dim(d), init = list())[-1]
      
      al[dimension] <- slice
      args <- c(list(d), al, list(x))
      do.call(`[<-`, args)
    }

  lens(lget = getter, lset = setter)
}

#' Filter lens
#'
#' Create a lens into the result of a filter. Arguments
#' are interpretted with non-standard evaluation as in
#' [dplyr::filter]
#'
#' @param ... unquoted NSE filter arguments
#' @export
filter_l <- function(...){
  dots <- rlang::quos(...)
  if (any(rlang::have_name(dots))) {
    stop("arguments to filter_l must not be named, do you need `==`?")
  }

  filt_quo <- Reduce(function(acc,q){ rlang::expr(`|`(!!acc, !!q)) }
                   , dots
                   , rlang::expr(FALSE))

  lens(lget = function(d){
         filt_vec <- rlang::eval_tidy(filt_quo, d)
         d[filt_vec,]
       }
     , lset = function(d,x){
         filt_vec <- rlang::eval_tidy(filt_quo, d)
         d[filt_vec,] <- x
         d
     })
}
