#' Construct a lens
#'
#' A `lens` represents the process of focussing on a specific part of a data structure.
#' We represent this via a `view` function and
#' an `over` function, roughly corresponding to object-oriented
#' "getters" and "setters" respectively.
#' Lenses can be composed to access or modify deeply nested
#' structures.
#'
#' @param view A function that takes a data structure of a certain type
#' and returns a subpart of that structure
#' @param over A function that takes a data structure of a certain type
#' and a value and returns a new data structure with the given subpart
#' replaced with the given value.  Note that `over` should not modify
#' the original data.
#' @details Lenses are popular in functional programming because
#' they allow you to build pure, compositional, and re-usable "getters" and "setters".
#'
#' An author of a `lens` (via `mkLens`) should ensure it obeys the following
#' rules (the "Lens laws", here paraphrased from
#' www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial):
#'
#' 1. Get-Put: If you get (view) some data with a lens, and then
#' modify (over) the data with that value, you get the input data back.
#' 2. Put-Get: If you put (over) a value into some data with a lens,
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
#'   view(1:10, index(4)) # returns 4
#'   over(1:10, index(1), 10) # returns c(10, 2:10)
#' @export
mkLens <- function(view, over){
  structure(
    list(view = view
       , over = over)
  , class = "lens")
}

#' Compose lenses
#'
#' Compose two lenses to produce a new lens which represents
#' focussing first with the first lens, then with the second.
#' A `view` using the resulting composite lens will first `view` using
#' the first, then the second, while an `over` will `view` via the first lens,
#' `over` into the resulting piece with the second, and then replace the
#' updated structure in the first with `over`.  Lens composition
#' is analogous to the `.` syntax of object-oriented programming or to
#' a flipped version of function composition.
#'
#' @rdname lens-compose
#' @param l the first lens
#' @param m the second lens
#' @examples
#'   lst <- list(b = c(3,4,5))
#'   lns <- index("b") %..% index(2)
#'   lst %>% view(lns)                 # returns 4
#'   lst %>% over(lns, 1)              # returns list(b = c(3,2,5))
#'   lst                               # returns list(b = c(3,4,5))
#' @export
`%..%` <- function(l, m){
  if(!inherits(l, "lens") || !inherits(m, "lens"))
    stop("lens composition is only defined for lenses")
  
  mkLens(function(d) m$view(l$view(d))
     , over = function(d, x){
       l$over(d, m$over(l$view(d), x))
       })
}


#' Modify data with a lens
#'
#' Set the subcomponent of the data referred to by a lens
#' with a new value. See [mkLens] for details.  Merely dispatches
#' to the `over` component of the lens.
#'
#' @param d the data
#' @param l the lens
#' @param x the replacement value
#' @export
over <- function(d, l, x){
  if(!inherits(l,"lens"))
    stop("over is only defined for lenses")

  l$over(d, x)
}

#' View data with a lens
#'
#' View the subcomponent of the data referred to by a lens.  This function
#' merely dispatches to the `view` component of the lens.
#'
#' @param d the data
#' @param l the lens
#' @export
view <- function(d, l){
  if(!inherits(l, "lens"))
    stop("view is only defined for lenses")

  l$view(d)
}

#' Construct a lens into an index/name
#'
#' This is the lens version of `[[`
#'
#' @param el The element the lens should point to
#' can be an `integer` or name.
#' @export
index <- function(el){
  mkLens(view = function(d) d[[el]]
       , over = function(d, x){
         d[[el]] <- x
         d
       })
}

#' Construct a lens into a subset of an object
#'
#' This is the lens version of `[`
#'
#' @param els a subset vector, can be `integer`, `character`
#' of `logical`, pointing to one or more elements of the object
#' @export
indexes <- function(els){
  mkLens(view = function(d) d[els]
       , over = function(d, x){
         d[els] <- x
         d
       })
}

#' A lens into the names of an object
#'
#' The lens versions of `names` and `names<-`.
#' @export
namel <- mkLens(view = names
              , over = `names<-`)

#' A lens into the column names of an object
#'
#' The lens version of `colnames` and `colnames<-`
#' @export
colnamesl <- mkLens(view = colnames
                  , over = `colnames<-`)

#' A lens into the row names of an object
#'
#' The lens version of `rownames` and `rownames<-`
#' @export
rownamesl <- mkLens(view = rownames
                  , over = `rownames<-`)

#' Construct a lens into an attribute
#'
#' The lens version of `attr` and `attr<-`
#' @param attrib A length one character vector indicating
#' the attribute to lens into.
#' @export
attrl <- function(attrib){
  mkLens(view = function(d) attr(d, attrib)
       , over = function(d, x) `attr<-`(d, attrib, x))
}
