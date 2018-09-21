#' Construct a lens
#'
#' A lens consists of a `view` function and
#' an `over` function, a getter and setter respectively.
#' Lenses can be composed to access or modify deeply nested
#' structures.
#'
#' @param view A function that takes data and returns a subpart
#' of that structure
#' @param over A function that takes data and a value and replaces
#' the given subpart of the data with that value.
#' @details lenses are popular in functional programming because
#' they allow you to compositionally build functional getters and
#' setters. Lens need to follow the following rules (laws)
#'
#' 1. Get-Put: If you get (view) some data with a lens, and then
#' modify (over) the data with that value, you get the input data back.
#' 2. Put-Get: If you put (over) a value into some data with a lens,
#' then get that value with the lens, you get back what you put in.
#' 3. Put-Put: If you put a value into some data with a lens, and
#' then put another value with the same lens, it's the same as only
#' doing the second put.
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
#' Compose two lenses to view or modify a subcomponent of
#' a subcomponent. Lenses compose such that they are evaluated
#' left to right, get with the first then get with the second lens,
#' and set with the first lens the result of setting with the second
#' lens. See the code for a better explanation.
#'
#' @param l the first lens
#' @param m the second lens
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
#' with a new value. See [mkLens] for details.
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
#' View the subcomponent of the data referred to by a lens.
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
              , over = `names<-`
                )

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
       , over = function(d,x) `attr<-`(d, attrib, x))
}


  
