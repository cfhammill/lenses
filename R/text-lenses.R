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

#' Lens into the name and extension of a path
#'
#' [view] is `basename`, [set] replaces everything after
#' the last file separator. Trailing file separators
#' are ignored.
#'
#' @export
fname_l <-
  path2list_l %.%
  map_l(
    rev_l %.%
    drop_while_il(seg ~ seg == "") %.%
    first_l
  ) %.%
  collapse_l(.Platform$file.sep) %>%
  ## Add an extra argument check to set to ensure no path seperators
  over(c_l("set", body_l), function(bdy){
    rlang::expr({
      if(any(grepl(.Platform$file.sep, x)))
        stop("Replacements in `fname_l` cannot have "
           , .Platform$file.sep
           , " in them")

      !!bdy
    })
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
  local({
    sep <- .Platform$file.sep
    pth_l <-
      path2list_l %.%
      map_l(rev_l %.%
            drop_while_il(seg ~ seg == "") %.%
            indexes_l(-1) %.%
            rev_l
            )
    
  lens(view =
         function(d){
           pth <- view(d, pth_l)
           empty_l <- c_l(vapply(pth, function(el) length(el) == 0, logical(1)))
           view(pth, collapse_l(sep)) %>%
             set(empty_l, NA)
         }
     , set =
         function(d, x){
           out <- d
           nas <- vapply(x, is.na, logical(1))
           nas_l <- c_l(nas)
           as_l <- c_l(!nas)
           
           out %>%
             send(nas_l %.% fname_l, nas_l) %>%
             over(as_l, el ~ paste0(x, sep, view(el, fname_l)))
         })
  })

#' Create a lens into the file extension
#'
#' A lens that focuses on the extension of a filepath 
#' The function creates one of two lenses, `last` extension which considers everything
#' after the last dot (`.`) an extension, and `first` which considers
#' everything after the first dot in the basename of the file to be
#' part of the file extension.
#' @param extension last or first indicating what type of extensions to
#' focus on.
#' @examples
#' view("hi.tar.gz", fext_l())
#' view("hi.tar.gz", fext_l("first"))
#' set("hi.tar.gz", fext_l(), "xz")
#' set("hi.tar.gz", fext_l("first"),  "tgz")
#' set(c("hi.tar.gz", "woah.txt.zip.gz"), fext_l("first"), c("tgz", "wut"))
#' set(c("hi.tar.gz", "woah.txt.zip.gz"), fext_l(), c("tgz", "wut"))
#' @export
fext_l <- function(extension = c("last", "first")){
  extension = match.arg(extension)
  if(extension == "first"){
    split_name <- fname_l %.% strsplit_l(".")
    dlast_and_collapse <- map_l(c_l(-1)) %.% collapse_l(".")
    all_first <- map_l(first_l)
    
    lens(view = function(d) view(d, split_name %.% dlast_and_collapse)
       , set =
           function(d,x){
             send_over(d
                     , split_name %.% all_first %.% unlist_l
                     , fname_l
                     , nm ~ paste0(nm, ".", x)
                     )
           }
         )
  
  } else {
    nml <- c_l(fname_l, strsplit_l("."), map_l(last_l), unlist_l)
    # this lens is exactly as above, but with an extra validation
    # that the replacement doesn't contain `.`
    over(nml, c_l("set", body_l)
       , function(cmd){
         rlang::expr({
           if(any(grepl("\\.", x)))
             stop("Replacement in `fext_l` with `extension = 'last'` can't"
                , " contain `.`")
           
           !!cmd
         })
       })
  }
}
