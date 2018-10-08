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

#' @title Slab lens
#' @description Create a lens into a chunk of an array (hyperslab). Uses
#' the same syntactic rules as `[`.
#' @param ... arguments as they would be passed to \code{[} for example `x[3,5,7]`.
#' @param drop whether or not to drop dimensions with length 1. Only
#' applies to `view`.
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
