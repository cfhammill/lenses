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
#' As noted in the README, using `lens` directly incurs the following obligations
#' (the "Lens laws"):
#'
#' 1. Get-Put: If you get (view) some data with a lens, and then
#' modify (set) the data with that value, you get the input data back.
#' 2. Put-Get: If you put (set) a value into some data with a lens,
#' then get that value with the lens, you get back what you put in.
#' 3. Put-Put: If you put a value into some data with a lens, and
#' then put another value with the same lens, it's the same as only
#' doing the second put.
#'
#' "Lenses" which do not satisfy these properties should be documented accordingly.
#' By convention, such objects present in this library are suffixed by "_il" ("illegal lens").
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



