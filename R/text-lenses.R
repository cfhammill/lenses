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
