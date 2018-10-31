#' @include lens.R verbs.R
NULL

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

#' Strsplit lens
#'
#' A lens into a split representation of a string. Here
#' [view] is almost equivalent to [strsplit] with `fixed = TRUE`,
#' the difference is that empty strings are included after a
#' trailing seperator. E.g. `view("/x/", strsplit_l("/"))` returns
#' `list(c("", "x", ""))` where `strsplit("/x/", "/")` returns
#' `list(c("", "x"))`.
#' [set] is equivalent to [paste0]`(x, collapse = split)` mapped
#' over each input with [vapply].
#'
#' Essentially the opposite of [collapse_l]
#' 
#' @param split The string to split at, this cannot be a regex (yet?).
#' @export
strsplit_l <- function(split){
  lens(view = function(d) strsplit(paste0(d,split), split, fixed = TRUE)
     , set = function(d, x){
         d[] <- vapply(x, function(strs) paste0(strs, collapse = split), character(1))
         d
       })
}

#' Collapse Lens
#'
#' [view] renders a list of character vectors as a vector of strings. Elements of
#' the input character vectors are pasted together with `paste0(v, collapse = collapse)`.
#' [set] replaces the input character vector with the replacement
#' data [view]ed with [strsplit_l]("collapse").
#'
#' Essentially the opposite of [strsplit_l]
#' 
#' @param collapse The collapse string for `view` and the split string for `set`.
#' @examples
#' view(list(c("a", "b", "c")), collapse_l("-"))
#' set(list(c("a", "b", "c")), collapse_l("-"), c("a-b"))
#' set(list(c("a", "b", "c")), collapse_l("-"), c("qr-pz"))
#' @export
collapse_l <- function(collapse){
  lens(view =
         function(d){
           vapply(d, function(strs) paste0(strs, collapse = collapse)
                , character(1))
         }
     , set =
         function(d,x){
           d[] <- view(x, strsplit_l(collapse))
           d
         })
}

#' Convert paths to vectors of path components
#'
#' This is strsplit_l(.Platform$file.sep)
#' @export
path2list_l <- strsplit_l(.Platform$file.sep)
  

           setNames(updated, NULL)
         }
       )
}

#' Lens into the name and extension of a path
#'
#' @export
fname_l <-
  lens(view = basename
       , set = function(d,x){
         sep <- .Platform$file.sep
         pattern <- paste0("(^.*", sep, ")([^", sep, "]*/?$)")
         sub(pattern, "\\2", x)
       })
  
#fpath_l
#fstem_l 
