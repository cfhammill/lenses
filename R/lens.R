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
#' ```
#' get(1:10, index(4)) # returns 4
#' set(1:10, index(1), 10) # returns c(10, 2:10)
mkLens <- function(view, over){
  structure(
    list(view = view
       , over = over)
  , class = "lens")
}
  
`%..%` <- function(l, m){
  if(!inherits(l, "lens") || !inherits(m, "lens"))
    stop("lens composition is only defined for lenses")
  
  mkLens(function(d) m$view(l$view(d))
     , over = function(d, x){
       l$over(d, m$over(l$view(d), x))
       })
}


over <- function(d, l, x){
  if(!inherits(l,"lens"))
    stop("over is only defined for lenses")

  l$over(d, x)
}

view <- function(d, l){
  if(!inherits(x, "lens"))
    stop("view is only defined for lenses")

  l$view(d)
}


index <- function(el){
  mkLens(view = function(d) d[[el]]
       , over = function(d, x){
         d[[el]] <- x
         d
       })
}

indexes <- function(el){
  mkLens(view = function(d) d[el]
       , over = function(d, x){
         d[el] <- x
         d
       })
}

namel <- mkLens(view = names
              , over = `names<-`
                )

colnamesl <- mkLens(view = colnames
                  , over = `colnames<-`)

rownamesl <- mkLens(view = rownames
                  , over = `rownames<-`)

attrl <- function(attrib){
  mkLens(view = function(d) attr(d, attrib)
       , over = function(d,x) `attr<-`(d, attrib, x))
}


  
