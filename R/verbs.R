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

  if(attr(l, "getter"))
    stop("can't set into a `getter` lens")

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

  getter <- attr(l, "getter") || attr(m, "getter")
  
  lens(function(d) m$view(l$view(d))
     , set = function(d, x){
       l$set(d, m$set(l$view(d), x))
     }
     , getter = getter)
}

#' @method "%.%" oscope
#' @export
`%.%.oscope` <- function(l, m){
  if(!inherits(m, "lens"))
    stop("the second argument of lens composition must be a lens")

  oscope(l$data, l$lens %.% m)
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
  f <- as_closure(f)
  set(d, l, f(view(d, l)))
}

#' @method over oscope
#' @export
over.oscope <- function(d, l, f){
  if(!missing("f"))
    stop("Argument `f` cannot be used with `over` and an `oscope`")

  over(d$data, d$lens, l)
}

#' Map a function over a lens
#'
#' Apply the specified function to each element
#' of the subobject.
#'
#' @param d the data
#' @param l the lens
#' @param f the function to use, potentially a `~` specified anonymous function.
#' @export
map_over <- function(d, l, f){
  f <- as_closure(f)
  sd <- view(d, l)
  if(!is.list(sd))
    stop("Map over can only be used with lenses returning a list")
  
  set(d, l, lapply(sd, f))
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
  f <- as_closure(f)
  m$set(d, f(l$view(d)))
}
