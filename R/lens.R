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
#' An author of a `lens` (via `lens`) should ensure it obeys the following
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
lens <- function(view, over){
  structure(
    list(view = view
       , over = over)
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
#'   oscope()   %..%
#'   index("b") %..%
#'   index(1)   %>%
#'   over(10)
#' @export
oscope <- function(d, l = idl){
  structure(
    list(data = d, lens = l)
  , class = "oscope")
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
#' @param l the first [lens] (or an [oscope])
#' @param m the second lens
#' @examples
#'   lst <- list(b = c(3,4,5))
#'   lns <- index("b") %..% index(2)
#'   lst %>% view(lns)                 # returns 4
#'   lst %>% over(lns, 1)              # returns list(b = c(3,2,5))
#'   lst                               # returns list(b = c(3,4,5))
#' @export
`%..%` <- function(l, m) UseMethod("%..%")

#' @method "%..%" lens 
#' @export
`%..%.lens` <- function(l, m){
  if(!inherits(m, "lens"))
    stop("the second argument of lens composition must be a lens")
  
  lens(function(d) m$view(l$view(d))
     , over = function(d, x){
       l$over(d, m$over(l$view(d), x))
       })
}

#' @method "%..%" oscope
#' @export
`%..%.oscope` <- function(l, m){
  if(!inherits(m, "lens"))
    stop("the second argument of lens composition must be a lens")

  oscope(l$data, l$lens %..% m)
}

#' Modify data with a lens
#'
#' Set the subcomponent of the data referred to by a lens
#' with a new value. See [lens] for details.  Merely dispatches
#' to the `over` component of the lens.
#'
#' @param d the data, or an [oscope]
#' @param l the lens, or in the case of an `oscope`, the replacement
#' @param x the replacement value, or nothing in the case of an `oscope`
#' @export
over <- function(d,l,x) UseMethod("over", d)

#' @method over default
#' @export
over.default <- function(d, l, x){
  if(!inherits(l,"lens"))
    stop("second argument of over must be a lens")

  l$over(d, x)
}

#' @method over oscope
#' @export
over.oscope <- function(d,l,x){
  if(!missing("x"))
    stop("Argument `x` cannot be used with `over` and an `oscope`")

  over(d$data, d$lens, l)
}

#' View data with a lens
#'
#' View the subcomponent of the data referred to by a lens.  This function
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

#' The identity (trivial lens)
#'
#' This lens focuses on the whole object
#' @export
idl <- lens(identity, function(., x) x)

#' Construct a lens into an index/name
#'
#' This is the lens version of `[[`
#'
#' @param el The element the lens should point to
#' can be an `integer` or name.
#' @export
index <- function(el){
  lens(view = function(d) d[[el]]
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
  lens(view = function(d) d[els]
       , over = function(d, x){
         d[els] <- x
         d
       })
}

#' A lens into the names of an object
#'
#' The lens versions of `names` and `names<-`.
#' @export
namel <- lens(view = names
              , over = `names<-`)

#' A lens into the column names of an object
#'
#' The lens version of `colnames` and `colnames<-`
#' @export
colnamesl <- lens(view = colnames
                  , over = `colnames<-`)

#' A lens into the row names of an object
#'
#' The lens version of `rownames` and `rownames<-`
#' @export
rownamesl <- lens(view = rownames
                  , over = `rownames<-`)

#' Construct a lens into an attribute
#'
#' The lens version of `attr` and `attr<-`
#' @param attrib A length one character vector indicating
#' the attribute to lens into.
#' @export
attrl <- function(attrib){
  lens(view = function(d) attr(d, attrib)
       , over = function(d, x) `attr<-`(d, attrib, x))
}
