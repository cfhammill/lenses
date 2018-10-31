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
  

which_last_non_empty <- function(x){
  lne <- which(x != "") %>% { .[length(.)] }
  if(length(lne) == 0) lne <- length(x)
  lne
}

#' Lens into the name and extension of a path
#'
#' [view] is `basename`, [set] replaces everything after
#' the last file separator. Trailing file separators
#' are ignored.
#'
#' @export
fname_l <-
  lens(view = basename
     , set =
         function(d,x){
           sep <- .Platform$file.sep
           if(any(grepl(sep, x)))
             stop("Replacements in `fname_l` cannot contain file separators")
           
           over(d, path2list_l,
                pl ~ Reduce(function(acc, i){
                  ith_l <- c_l(i)
                  over(acc, ith_l
                     , spl ~ set(spl, c_l(which_last_non_empty(spl)), view(x, ith_l)))
                }, seq_along(pl), init = pl))
         })

#' Lens into the path of the file
#'
#' This lens extracts the path from a string interpretted as a path.
#' [view] is roughly equivalent to [basename], it differs on how it
#' handles relative paths with no file separators. `view("x/", fpath_l)`
#' returns `NA`, `basename("x/")` returns `.`. This difference is to satisfy
#' the `view-set` law.
#' [set] also special cases replacing with `NA`, obliterating any path components.
#' `set("/a/file/path/", fpath_l, NA)` returns `"path/"` contrast this with
#' `set("/a/file/path/", fpath_l, "")` which returns `"/path/"`.
#'
#' @examples
#' set("/a/file/path/", fpath_l, NA)
#' set("/a/file/path/", fpath_l, "")
#' view("x", fpath_l)
#' view("/a/file/path/", fpath_l)
#' @export
fpath_l <-
  lens(view =
         function(d){
           sep <- .Platform$file.sep
           pl <- view(d, path2list_l)
           vapply(pl, function(l){
             pth <- view(l, c_l(take_l(which_last_non_empty(l) - 1)))
             if(length(pth) == 0) return(NA_character_)
             view(list(pth), collapse_l(sep))             
           }, character(1))             
         }
     , set =
         function(d, x){
           sep <- .Platform$file.sep
           over(d, path2list_l,
                pl ~ Reduce(function(acc, i){
                  ith_l <- c_l(i)
                  over(acc, ith_l
                     , spl ~ c(view(x, ith_l) %>% { `if`(is.na(.), NULL, .) }
                             , view(spl, c_l(which_last_non_empty(spl):length(spl)))))
                }, seq_along(pl), init = pl))
         }) 
  
#fpath_l
#fstem_l 
