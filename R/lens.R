#' Construct a lens
#'
#' A `lens` represents the process of focussing on a specific part of a data structure.
#' We represent this via a `view` function and
#' an `set` function, roughly corresponding to object-oriented
#' "getters" and "setters" respectively.
#' Lenses can be composed to access or modify deeply nested
#' structures.
#'
#' @param view A function that takes a data structure of a certain type
#' and returns a subpart of that structure
#' @param set A function that takes a data structure of a certain type
#' and a value and returns a new data structure with the given subpart
#' replaced with the given value.  Note that `set` should not modify
#' the original data.
#' @details Lenses are popular in functional programming because
#' they allow you to build pure, compositional, and re-usable "getters" and "setters".
#'
#' An author of a `lens` (via `lens`) should ensure it obeys the following
#' rules (the "Lens laws", here paraphrased from
#' www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial):
#'
#' 1. Get-Put: If you get (view) some data with a lens, and then
#' modify (set) the data with that value, you get the input data back.
#' 2. Put-Get: If you put (set) a value into some data with a lens,
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
#'   view(1:10, index_l(4)) # returns 4
#'   set(1:10, index_l(1), 10) # returns c(10, 2:10)
#' @export
lens <- function(view, set){
  structure(
    list(view = view
       , set = set)
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
#'   set(10)
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
#' A `view` using the resulting composite lens will first `view` using
#' the first, then the second, while an `set` will `view` via the first lens,
#' `set` into the resulting piece with the second, and then replace the
#' updated structure in the first with `set`.  Lens composition
#' is analogous to the `.` syntax of object-oriented programming or to
#' a flipped version of function composition.
#'
#' @rdname lens-compose
#' @param l the first [lens] (or an [oscope])
#' @param m the second lens
#' @examples
#'   lst <- list(b = c(3,4,5))
#'   lns <- index_l("b") %.% index_l(2)
#'   lst %>% view(lns)                 # returns 4
#'   lst %>% set(lns, 1)              # returns list(b = c(3,2,5))
#'   lst                               # returns list(b = c(3,4,5))
#' @export
`%.%` <- function(l, m) UseMethod("%.%")

#' @method "%.%" lens 
#' @export
`%.%.lens` <- function(l, m){
  if(!inherits(m, "lens"))
    stop("the second argument of lens composition must be a lens")
  
  lens(function(d) m$view(l$view(d))
     , set = function(d, x){
       l$set(d, m$set(l$view(d), x))
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
#' to the `set` component of the lens.
#'
#' @param d the data, or an [oscope]
#' @param l the lens, or in the case of an `oscope`, the replacement
#' @param x the replacement value, or nothing in the case of an `oscope`
#' @export
set <- function(d,l,x) UseMethod("set", d)

#' @method set default
#' @export
set.default <- function(d, l, x){
  if(!inherits(l,"lens"))
    stop("second argument of set must be a lens")

  l$set(d, x)
}

#' @method set oscope
#' @export
set.oscope <- function(d,l,x){
  if(!missing("x"))
    stop("Argument `x` cannot be used with `set` and an `oscope`")

  set(d$data, d$lens, l)
}

#' View data with a lens
#'
#' Get the subcomponent of the data referred to by a lens.  This function
#' merely dispatches to the `view` component of the lens. 
#'
#' @param d the data
#' @param l the lens
#' @export
view <- function(d, l) UseMethod("view")

#' @method view default
#' @export
view.default <- function(d, l){
  if(!inherits(l, "lens"))
    stop("view is only defined for lenses")

  l$view(d)
}

#' @method view oscope
#' @export
view.oscope <- function(d, l){
  if(!missing("l"))
    stop("Argument `l` cannot be used with `view` and an `oscope`")

  view(d$data, d$lens)
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
  set(d, l, f(view(d, l)))
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
  lens(view = function(d) d[[el]]
       , set = function(d, x){
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
  lens(view = function(d) d[els]
       , set = function(d, x){
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
names_l <- lens(view = names
              , set = `names<-`)

#' A lens into the column names of an object
#'
#' The lens version of `colnames` and `colnames<-`
#' @export
colnames_l <- lens(view = colnames
                  , set = `colnames<-`)

#' A lens into the row names of an object
#'
#' The lens version of `rownames` and `rownames<-`
#' @export
rownames_l <- lens(view = rownames
                  , set = `rownames<-`)

#' Construct a lens into an attribute
#'
#' The lens version of `attr` and `attr<-`
#' @param attrib A length one character vector indicating
#' the attribute to lens into.
#' @export
attr_l <- function(attrib){
  lens(view = function(d) attr(d, attrib)
       , set = function(d, x) `attr<-`(d, attrib, x))
}

#' Environment lens
#'
#' A lens into the environment of an object. This
#' is the lens version of [environment] and [environment<-]
#' @export
env_l <- lens(environment, `environment<-`)

#' Tidyselect elements by name
#'
#' Create a lens into a named collection. On [set]
#' names of the input are not changed. This generalizes [dplyr::select]
#' to arbitrary named collections and allows updating.
#' @param ... An expression to be interpreted by [tidyselect::vars_select]
#' which is the same interpreter as [dplyr::select]
#' @examples
#' lets <- setNames(seq_along(LETTERS), LETTERS)
#' set(lets, select_l(G:F, A, B), 1:4) # A and B are 3,4 for a quick check
#' @export
select_l <- function(...){
  dots <- rlang::quos(...)
  lens(
    view = function(d){
      vars <- tidyselect::vars_select(names(d), !!!dots)
      d[vars]
    }
  , set = function(d,x){
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
  lens(view = function(d) d[rows, ,drop = drop]
     , set = function(d, x){
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
  lens(view = function(d) d[, cols,drop = FALSE]
     , set = function(d, x){
       d[,cols] <- x
       d
     })
}

#' Dims lens
#'
#' A lens into an objects dimensions
#' @export
dim_l <- lens(view = dim
            , set = `dim<-`)

#' Slice lens
#'
#' Create a lens into a specific slice of a specific
#' dimension of a multidimensional object. Not to be
#' confused with dplyr slice.
#' 
#' @param dimension the dimension to slice
#' @param slice the slice index
#' @param drop whether or not to drop dimensions with length 1.
#' Only applies to [view].
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

  lens(view = getter, set = setter)
}

#' Slab lens
#'
#' Create a lens into a chunk of an array (hyperslab). Uses
#' the same syntactic rules as `[`.
#'
#' @param ... arguments as they would be passed `[` e.g. `x[3,5,7]`
#' @param drop whether or not to drop dimensions with length 1. Only
#' applies to `view`
#' @export
slab_l <- function(..., drop = FALSE){
  lens(view = function(d) d[..., drop = drop]
     , set = function(d, x){ d[...] <- x; d }) 
}

#' Filter lens
#'
#' Create an illegal lens into the result of a filter. Arguments
#' are interpreted with non-standard evaluation as in
#' [dplyr::filter]
#'
#' @param ... unquoted NSE filter arguments
#' @export
filter_il <- function(...){
  dots <- rlang::quos(...)
  if (any(rlang::have_name(dots))) {
    stop("arguments to filter_l must not be named, do you need `==`?")
  }

  filt_quo <- Reduce(function(acc,q){ rlang::expr(`|`(!!acc, !!q)) }
                   , dots
                   , rlang::expr(FALSE))

  lens(view = function(d){
         filt_vec <- rlang::eval_tidy(filt_quo, d)
         d[filt_vec,]
       }
     , set = function(d,x){
         filt_vec <- rlang::eval_tidy(filt_quo, d)
         d[filt_vec,] <- x
         d
     })
}

#' Filter lens
#'
#' Create a lawful lens into the result of a filter. This
#' focuses only columns not involved in the filter condition.
#'
#' @param ... unquoted NSE filter arguments
#' @export
filter_l <- function(...){
  dots <- rlang::quos(...)
  if (any(rlang::have_name(dots))) {
    stop("arguments to filter_l must not be named, do you need `==`?")
  }

  filt_expr <- Reduce(function(acc,q){ rlang::expr(`|`(!!acc, !!q)) }
                   , dots
                   , rlang::expr(FALSE))

  symbol_gatherer <-
    function(expr){
      if(is.name(expr)) return(as.character(expr))
      if(!is.call(expr)) return(NULL)
      
      unlist(lapply(as.list(expr), symbol_gatherer))
    }

  expr_symbols <-
    symbol_gatherer(filt_expr) %>%
    as.character %>%
    gsub("`", "", .)

  lens(view = function(d){
         filt_vec <- rlang::eval_tidy(filt_expr, d)
         d[filt_vec, ! names(d) %in% expr_symbols ]
       }
     , set = function(d,x){
         filt_vec <- rlang::eval_tidy(filt_expr, d)
         d[filt_vec, ! names(d) %in% expr_symbols] <- x
         d
     })
}



#' Matrix transpose lens
#'
#' Lens into the transpose of a matrix
#'
#' @export
t_l <-
  lens(view = t
     , set = function(d, x){
       new_d <- t(x)
       if(any(dim(d) != dim(new_d)))
         stop("transposed matrix replacement in `t_l` does not have the right dimensions")
       
       new_d
     })

#' Lens into a list of rows
#'
#' A lens that creates a list-of-rows view of a `data.frame`
#' @export
transpose_l <-
  lens(view = function(d) lapply(seq_len(nrow(d)), function(i) d[i, , drop = FALSE])
     , set = function(d, x){
       new_d <- Reduce(rbind, x)
       if(any(names(new_d) != names(d)))
         stop("Names of replacement list components in `transpose_l` don't match the "
            , "source data")

       if(any(dim(new_d) != dim(d)))
         stop("Length of the frames in the replacement list in `transpose_l` don't match "
            , "the source data")

       new_d
     })

#' A lens into a substring
#'
#' Make a lens to focus on a character range in one or more
#' strings (character vector).
#'
#' @param first the starting character
#' @param last the last character
substr_l <- function(first, last){
  lens(
    view = function(d){
      if(first < 1 || any(nchar(d) < last))
        stop("first and last in `substr_l` must be within all target strings. ",
             "This means either first is less than 1 or at least one string has fewer ",
             "characters than last.")
      
      substr(d, first, last)
    }
  , set = function(d,x){
    if(first < 1 || any(nchar(d) < last))
      stop("first and last in `substr_l` must be within all target strings. ",
           "This means either first is less than 1 or at least one string has fewer ",
           "characters than last = ", last, ".")

    if(any(nchar(x) != 1 + last - first))
      stop("replacement strings in `substr_l` must have 1 + last - first characters ="
         , 1 + last - first)
    
    substr(d, first, last) <- x
    d
  })
}

#' Lens into the diagonal of a matrix
#'
#' A lens into a matrix's diagonal elements
#' @export
diag_l <-
  lens(view = diag
     , set = `diag<-`)

#' Lens into lower diagonal elements
#'
#' Create a lens into the lower diagonal elements
#' of a matrix
#' @param diag whether or not to include the diagonal
#' @export
lower_tri_l <-
  function(diag = FALSE){
    lens(
      view = function(d) d[lower.tri(d, diag = diag)]
    , set = function(d, x){
      d[lower.tri(d, diag = diag)] <- x
      d
    })
  }

#' Lens into upper diagonal elements
#'
#' Create a lens into the upper diagonal elements
#' of a matrix
#' @param diag whether or not to include the diagonal
#' @export
upper_tri_l <-
  function(diag = FALSE){
    lens(
      view = function(d) d[upper.tri(d, diag = diag)]
    , set = function(d, x){
      d[upper.tri(d, diag = diag)] <- x
      d
    })
  }

#' Set one lens to the view of another
#'
#' @param d the data
#' @param l the lens to view through
#' @param m the lens to set into
#' @export
send <- function(d, l, m){
  m$set(d, l$view(d))
}

#' Set one lens to the view of another (transformed)
#'
#' @param d the data
#' @param f the function to apply to the viewed data
#' @param l the lens to view through
#' @param m the lens to set into
#' @export
send_over <- function(d, f, l, m){
  m$set(d, f(l$view(d)))
}

