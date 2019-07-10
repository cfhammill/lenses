#' @include lens.R
NULL

#' Filter lens
#'
#' Create an illegal lens into the result of a filter. Arguments
#' are interpreted with non-standard evaluation as in
#' [dplyr::filter]
#'
#' @param ... unquoted NSE filter arguments
#' @examples
#' head(view(iris, filter_il(Species == "setosa"))) 
#' head(over(iris,
#'           filter_il(Species == "setosa") %.% select_l(-Species),
#'           function(x) x + 10))
#' @export
filter_il <- function(...){
  dots <- rlang::quos(...)
  if (any(rlang::have_name(dots))) {
    stop("arguments to filter_il must not be named, do you need `==`?")
  }

  filt_quo <- Reduce(function(acc,q){ rlang::expr(`|`(!!acc, !!q)) }
                   , dots
                   , rlang::expr(FALSE))

  lens(view = function(d){
         filt_vec <- rlang::eval_tidy(filt_quo, d)
         d[filt_vec,]
       }
     , set = function(d,x){
         filt_vec <- rlang::eval_tidy(filt_quo, d)
         if (any(filt_vec)){
           d[filt_vec,] <- x
         }
         d
     })
}

#' Filter lens
#'
#' Create a lawful lens into the result of a filter. This
#' focuses only columns not involved in the filter condition.
#'
#' @param ... unquoted NSE filter arguments
#' @include utils.R
#' @examples
#' head(view(iris, filter_l(Species == "setosa"))) # Note Species is not seen
#' head(over(iris, filter_l(Species == "setosa"), function(x) x + 10))
#' @export
filter_l <- function(...){
  dots <- rlang::quos(...)
  if (any(rlang::have_name(dots))) {
    stop("arguments to filter_l must not be named, do you need `==`?")
  }

  filt_expr <- Reduce(function(acc,q){ rlang::expr(`|`(!!acc, !!q)) }
                   , dots
                   , rlang::expr(FALSE))

  expr_symbols <-
    symbol_gatherer(filt_expr) %>%
    as.character %>%
    gsub("`", "", .)

  lens(view = function(d){
         filt_vec <- rlang::eval_tidy(filt_expr, d)
         d[filt_vec, ! names(d) %in% expr_symbols ]
       }
     , set = function(d,x){
         filt_vec <- rlang::eval_tidy(filt_expr, d)
         if (any(filt_vec)) {
           d[filt_vec, ! names(d) %in% expr_symbols] <- x
         }
         d
     })
}

#' Tidyselect elements by name
#'
#' Create a lens into a named collection. On [set]
#' names of the input are not changed. This generalizes [dplyr::select]
#' to arbitrary named collections and allows updating.
#' @param ... An expression to be interpreted by [tidyselect::vars_select]
#' which is the same interpreter as [dplyr::select]
#' @examples
#' lets <- setNames(seq_along(LETTERS), LETTERS)
#' set(lets, select_l(G:F, A, B), 1:4) # A and B are 3,4 for a quick check
#' @export
select_l <- function(...){
  dots <- rlang::quos(...)
  lens(
    view = function(d){
      vars <- tidyselect::vars_select(names(d), !!!dots)
      d[vars]
    }
  , set = function(d,x){
    vars <- tidyselect::vars_select(names(d), !!!dots)
    d[vars] <- x
    d
  })
}

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
