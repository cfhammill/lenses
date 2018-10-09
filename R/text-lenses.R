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
#' [view] is equivalent to [strsplit] with `fixed = TRUE`
#' [set] is equivalent to [paste0] `collapse = split` mapped
#' over each input with [vapply].
#' 
#' @param split The string to split at, this cannot be a regex (yet?).
#' @export
strsplit_l <- function(split){
  lens(view = function(d) strsplit(d, split, fixed = TRUE)
     , set = function(d, x){
         d[] <- vapply(x, function(strs) paste0(strs, collapse = split), character(1))
         d
       })
}

#' Create lens into the file extension
#'
#' A lens that focuses on the extension of a filepath 
#' Inspiration/regex pieces from Jim Hester and r-lib's fs package.
#' The function creates one of two `last` which considers everything
#' after the last dot (`.`) an extension, and `first` which considers
#' everything after the first dot in the basename of the file to be
#' part of the file extension.
#' @param extension last or first indicating what type of extensions to
#' focus on.
#' @export
fext_l <- function(extension = c("last", "first")){
  sep <- .Platform$file.sep
  extension <- match.arg(extension)
  rexp <- 
    paste0("(?<!^|[.]|", sep, ")[.]"
         , `if`(extension == "last"
              , "([^.", "([^")
         , sep, "]*)$")
  
  lens(view =
         function(d){
           matches <- regexpr(rexp, d, perl = TRUE)
           ext_start <- attr(matches, "capture.start") %>% { .[,ncol(.)] }
           ext_length <- attr(matches, "capture.length") %>% { .[,ncol(.)] }

           vapply(seq_along(d), function(i){
             if(ext_start[i] == -1){
               NA_character_
             } else {
               substring(d[i], ext_start[i], ext_start[i] + ext_length[i])
             }
           }, character(1))
         }
     , set =
         function(d,x){
           if(any(grepl(sep, x)))
             stop("File extensions replacement can't contain path separators")
           
           matches <- regexpr(rexp, d, perl = TRUE)
           ext_start <- attr(matches, "capture.start") %>% { .[,ncol(.)] }
           ext_length <- attr(matches, "capture.length") %>% { .[,ncol(.)] }

           updated <-
             mapply(function(old, new, start, length){
               if(start == -1 && is.na(new)) return(old)
               
               if(extension == "last" && grepl("\\.", new))
                 stop("For `fext_l` with `extension = 'last'` the replacement extensions must "
                    , "not contain any `.` characters. "
                    , "For example 'tar' cannot be replaced with 'tar.gz'. ")

               if(start == -1)
                 start <- nchar(old) + 2

               basename <- substring(old, 1, start - 2)
               if(is.na(new)) return(basename)

               paste0(basename, ".", new)
             }, d, x, ext_start, ext_length)

           setNames(updated, NULL)
         }
       )
}

#' Lens into the name and extension of a path
#'
#' @export
fname_l <-
  strsplit_l(.Platform$file.sep) %.% map_l(rev_l %.% take_l(1))
  
#fpath_l
#fstem_l 
