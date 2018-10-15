#' @include lens.R verbs.R
NULL

#' Matrix transpose lens
#'
#' Lens into the transpose of a matrix
#' @examples
#' (x <- matrix(1:4, ncol = 2))
#' view(x, t_l)
#' set(x, t_l, matrix(11:14, ncol = 2))
#' @export
t_l <-
  lens(view = t
     , set = function(d, x){
       new_d <- t(x)
       if(any(dim(d) != dim(new_d)))
         stop("transposed matrix replacement in `t_l` does not have the right dimensions")
       
       new_d
     })

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
#' @examples
#' (x <- matrix(1:4, ncol = 2))
#' view(x, slice_l(1, 2)) # x[2,, drop = FALSE]
#' view(x, slice_l(2, 2)) # x[,2, drop = FALSE]
#' set(x, slice_l(1,1), c(10,20))
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

#' @title Slab lens
#' @description Create a lens into a chunk of an array (hyperslab). Uses
#' the same syntactic rules as `[`.
#' @param ... arguments as they would be passed to \code{[} for example `x[3,5,7]`.
#' @param drop whether or not to drop dimensions with length 1. Only
#' applies to `view`.
#' @examples
#' (x <- matrix(1:4, ncol = 2))
#' view(x, slab_l(2,)) # x[2,, drop = FALSE]
#' view(x, slab_l(2, 2)) # x[2,2, drop = FALSE]
#' set(x, slab_l(1,1:2), c(10,20))
#' @export
slab_l <- function(..., drop = FALSE){
  lens(view = function(d) d[..., drop = drop]
     , set = function(d, x){ d[...] <- x; d }) 
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
#' @examples
#' (x <- matrix(1:9, ncol = 3))
#' view(x, lower_tri_l())
#' view(x, lower_tri_l(diag = TRUE))
#' set(x, lower_tri_l(), c(100, 200, 300))
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
#' (x <- matrix(1:9, ncol = 3))
#' view(x, upper_tri_l())
#' view(x, upper_tri_l(diag = TRUE))
#' set(x, upper_tri_l(), c(100, 200, 300)) 
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

#' Lens into a new dimension(s)
#'
#' Construct a lens that is a view of the data with
#' a new set of dimensions. Both [view] and [set] check
#' that the new dimensions match the number of elements of
#' the data.
#' @param dims a vector with the new dimensions
#' @examples
#' x <- 1:9
#' view(x, reshape_l(c(3,3)))
#' set(x, reshape_l(c(3,3)) %.% diag_l, 100)
#' @export
reshape_l <- function(dims){
  lens(view = function(d) `dim<-`(d, dims)
     , set = function(d,x){
         rep_dims <- dim(x)
         if(!is.null(rep_dims) &&
            prod(rep_dims) != prod(dims) ||
            length(x) != prod(dims))
           stop("Replacement data in `reshape_l` does not have "
              , "the correct shape or number of elements")

         d[] <- x
         d
       })
}
