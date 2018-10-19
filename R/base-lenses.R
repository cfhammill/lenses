#' The identity (trivial lens)
#'
#' This lens focuses on the whole object
#' @examples
#' x <- 1:10
#' view(x, id_l)
#' head(set(x, id_l, iris))
#' @export
id_l <- lens(identity, function(., x) x)

#' Construct a lens into an index/name
#'
#' This is the lens version of `[[`
#'
#' @param el The element the lens should point to
#' can be an `integer` or name.
#' @examples
#' x <- 1:10
#' view(x, index_l(1))
#' set(x, index(5), 50)
#' head(view(iris, index(2)))
#' @export
index_l <- function(el){
  lens(view = function(d) d[[el]]
       , set = function(d, x){
         d[[el]] <- x
         d
       })
}

#' @describeIn index_l shorthand
#' @export
index <- index_l

#' A lens into the first element
#'
#' Lens version of `x[[1]]` and `x[[1]] <- val`
#' x <- 1:10
#' view(x, first_l)
#' set(x, first_l, 50)
#' @export
first_l <- lens(view = function(d) d[[1]]
              , set = function(d, x){ d[[1]] <- x; d})

#' A lens into the last element
#'
#' Lens version of `x[[length(x)]]` and `x[[length(x)]] <- val`
#' @examples
#' x <- 1:10
#' view(x, last_l)
#' set(x, last_l, 50)
#' @export
last_l <- lens(view = function(d) d[[length(d)]]
             , set = function(d, x){ d[[length(d)]] <- x; d})

#' Construct a lens into a subset of an object
#'
#' This is the lens version of `[`
#'
#' @param els a subset vector, can be `integer`, `character`
#' of `logical`, pointing to one or more elements of the object
#' @examples
#' x <- 1:10
#' view(x, indexes_l(3:5))
#' set(x, indexes_l(c(1,10)), NA)
#' head(view(iris, indexes_l(c("Sepal.Length", "Species"))))
#' @export
indexes_l <- function(els){
  lens(view = function(d) d[els]
       , set = function(d, x){
         d[els] <- x
         d
       })
}

#' @describeIn indexes_l shorthand
#' @export
indexes <- indexes_l

#' Construct a lens into a prefix of a vector
#'
#' This constructs a lens into the first `n` elements
#' of an object or the if negative indexing is used,
#' as many as `length(x) - n`.
#'
#' @param n number of elements to take, or if negative the
#' number of elements at the end to not take.
#' @examples
#' x <- 1:10
#' view(x, take_l(3))
#' view(x, take_l(-7))
#' set(x, take_l(2), c(100,200))
#' set(x, take_l(-8), c(100,200))
#' @export
take_l <- function(n){
  lens(view =
         function(d){
           if(n < 0) n <- length(d) + n
           if(n < 0) n <- length(d)
           if(n > length(d))
             stop("argument n in `take_l` must be less the the object length")
           d[seq_len(n)]
         }
     , set =
         function(d, x){
           if(n < 0) n <- length(d) + n
           if(n < 0) n <- length(d)
           if(n > length(d))
             stop("argument n in `take_l` must be less the the object length")

           old_opts <- options()
           on.exit(options(old_opts))
           options(warn = 2)
           d[seq_len(n)] <- x
           d
         })
}

#' Reverse lens
#'
#' Lens into the [rev]erse of an object.
#' @examples
#' x <- 1:10
#' view(x, rev_l)
#' set(x, rev_l, 11:20)
#' @export
rev_l <-
  lens(view = rev
     , set = function(d, x){
       old_opts <- options()
       on.exit(options(old_opts))
       options(warn = 2)
       d[] <- rev(x)
       d
     })

#' Slot lens
#'
#' The lens equivalent of `@` and `@<-`
#' for getting and setting S4 object slots.
#' @param slot the name of the slot
#' @examples
#' new_class <- setClass("new_class", slots = c(x = "numeric"))
#' (x <- new_class())
#'
#' view(x, slot_l("x"))
#' set(x, slot_l("x"), 1:10)
#' @export
slot_l <- function(slot){
  lens(view = function(d) eval(bquote(`@`(d, .(slot))))
     , set = function(d, x) eval(bquote(`@<-`(d, .(slot), x))))
}

#' A lens into the names of an object
#'
#' The lens versions of `names` and `names<-`.
#' @examples
#' view(iris, names_l)
#' head(set(iris, names_l, LETTERS[1:5]))
#' @export
names_l <- lens(view = names
              , set = `names<-`)

#' A lens into the column names of an object
#'
#' The lens version of `colnames` and `colnames<-`
#' @examples
#' x <- matrix(1:4, ncol = 2)
#' colnames(x) <- c("first", "second")
#' x
#' 
#' view(x, colnames_l)
#' set(x, colnames_l, c("premiere", "deuxieme"))
#' @export
colnames_l <- lens(view = colnames
                  , set = `colnames<-`)

#' A lens into the row names of an object
#'
#' The lens version of `rownames` and `rownames<-`
#' @examples
#' x <- matrix(1:4, ncol = 2)
#' rownames(x) <- c("first", "second")
#' x
#' 
#' view(x, rownames_l)
#' set(x, rownames_l, c("premiere", "deuxieme"))
#' @export
rownames_l <- lens(view = rownames
                 , set = `rownames<-`)

#' Dimnames lens
#'
#' A lens into the dimnames of an object. Lens
#' equivalent of [dimnames] and [dimnames<-].
#' @examples
#' x <- matrix(1:4, ncol = 2)
#' colnames(x) <- c("first", "second")
#' x
#' 
#' view(x, dimnames_l)
#' set(x, dimnames_l, list(NULL, c("premiere", "deuxieme")))
#' @export
dimnames_l <-
  lens(view = dimnames
     , set = `dimnames<-`)

#' Dims lens
#'
#' A lens into an objects dimensions
#' @examples
#' x <- 1:10
#'
#' (y <- set(x, dim_l, c(2,5)))
#' view(y, dim_l)
#' @export
dim_l <- lens(view = dim
            , set = `dim<-`)


#' Construct a lens into an attribute
#'
#' The lens version of `attr` and `attr<-`
#' @param attrib A length one character vector indicating
#' the attribute to lens into.
#' @examples
#' (x <- structure(1:10, important = "attribute"))
#' view(x, attr_l("important"))
#' set(x, attr_l("important"), "feature")
#' @export
attr_l <- function(attrib){
  lens(view = function(d) attr(d, attrib)
     , set = function(d, x) `attr<-`(d, attrib, x))
}

#' Attributes lens
#'
#' The lens equivalent of [attributes] and [attributes<-]
#' @examples
#' (x <- structure(1:10, important = "attribute"))
#' view(x, attributes_l)
#' set(x, attributes_l, list(important = "feature"))
#' @export
attributes_l <-
  lens(view = attributes
     , set = `attributes<-`)

#' Environment lens
#'
#' A lens into the environment of an object. This
#' is the lens version of [environment] and [environment<-]
#' @examples
#' x <- 10
#' f <- (function(){x <- 2; function() x + 1})()
#' f
#'
#' f()
#' view(f, env_l)$x
#' 
#' g <- over(f, env_l, parent.env)
#' g()
#' @export
env_l <- lens(environment, `environment<-`)

#' Row lens
#'
#' Create a lens into a set of rows
#'
#' @param rows the rows to focus on
#' @param drop whether or not to drop dimensions with length 1
#' @examples
#' x <- matrix(1:4, ncol = 2)
#' rownames(x) <- c("first", "second")
#' x
#'
#' view(x, rows_l(1))
#' view(x, rows_l("second"))
#' set(x, rows_l(1), c(20,40))
#' @export
rows_l <- function(rows, drop = FALSE){
  lens(view = function(d) d[rows, ,drop = drop]
     , set = function(d, x){
       d[rows, ] <- x
       d
     })
}

#' Column lens
#'
#' Create a lens into a set of columns
#'
#' @param cols the columns to focus on
#' @param drop whether or not to drop dimensions with length 1
#' @examples
#' x <- matrix(1:4, ncol = 2)
#' colnames(x) <- c("first", "second")
#' x
#'
#' view(x, cols_l(1))
#' view(x, cols_l("second"))
#' set(x, cols_l(1), c(20,40))
#' @export
cols_l <- function(cols, drop = FALSE){
  lens(view = function(d) d[, cols,drop = FALSE]
     , set = function(d, x){
       d[,cols] <- x
       d
     })
}

#' Promote a lens to apply to each element of a list
#'
#' Create a new lens that views and sets each element
#' of the list.
#'
#' @param l the lens to promote
#' @details Uses [lapply] under the hood for [view]
#' and [mapply] under the hood for [set]. This means
#' that [set] can be given a list of values to set,
#' one for each element. If the input or update are
#' lists this lens always returns a list. If the input
#' and update are vectors this lens will return a vector.
#' @examples
#' (ex <- replicate(10, sample(1:5), simplify = FALSE))
#' view(ex, map_l(index(1)))
#' set(ex, map_l(index(1)), 11:20)
#' @export
map_l <- function(l){
  is_length1_atomic <- function(x) is.atomic(x) && length(x) == 1
  lens(view =
         function(d){
           new_d <- lapply(d, view, l)
           if(!is.list(d) && all(vapply(new_d, is_length1_atomic, logical(1))))
             new_d <- unlist(new_d, recursive = FALSE)
           
           new_d
         }
     , set =
         function(d, x){
           new_d <- mapply(set, d, x, MoreArgs = list(l = l), SIMPLIFY = FALSE)
           if(!is.list(d) && !is.list(x) && all(vapply(new_d, is_length1_atomic, logical(1))))
             new_d <- unlist(new_d, recursive = FALSE)
           
           new_d
         })
}

#' Promote a function to a `getter` lens
#'
#' Create a `getter` lens from a function.
#'
#' @param f The function to promote.
#' @export
to_l <- function(f){
  lens(view = f
     , set = function(d,x) stop("`to_l` lenses cannot be set through")
     , getter = TRUE)
}

#' Body lens
#'
#' A lens into the body of a function. The
#' lens equivalent of [body] and [body<-].
#' You probably shouldn't use this.
#' @examples
#' inc2 <- function(x) x + 2
#' view(inc2, body_l)
#' inc4 <- set(inc2, body_l, quote(x + 4))
#' inc4(10)
#' @export
body_l <-
  lens(view = body
     , set = function(d, x){
       `body<-`(d, value = x)
     })

#' Formals lens
#'
#' A lens equivalent of [formals] and [formals<-],
#' allowing you to change the formal arguments of
#' a function. As with [body_l] you probably shouldn't
#' use this.
#' @examples
#' f <- function(x) x + y + 7
#' view(f, formals_l)
#' 
#' g <- set(f, formals_l, list(x = 1, y = 2))
#' g()
#' @export
formals_l <-
  lens(view = formals
     , set = function(d, x){
       `formals<-`(d, value = x)
     })

#' Class lens
#'
#' A lens into the class of an object. Lens
#' equivalent of [class] and [class<-].
#' @examples
#' x <- 1:10
#' view(x, class_l)
#' set(x, class_l, "super_integer")
#' @export
class_l <-
  lens(view = class
     , set = `class<-`)

#' Levels lens
#'
#' A lens into the levels of an object. Usually
#' this is factor levels. Lens
#' equivalent of [levels] and [levels<-].
#' @examples
#' x <- factor(c("a", "b"))
#' view(x, levels_l)
#' set(x, levels_l, c("A", "B"))
#' @export
levels_l <-
  lens(view = levels
     , set = `levels<-`)

#' Convenient lens composition
#'
#' A lens version of [purrr::pluck]. Takes
#' a series element indicators and creates a composite
#' lens.
#' 
#' - length one vectors are converted to [index_l],
#' - length one numeric vectors that are negative are converted to [indexes_l],
#' - larger vectors are converted to [indexes_l],
#' - lenses are composed as is.
#'
#' See examples for more
#' 
#' @param ... index vectors or lenses
#' @examples
#' view(iris, c_l("Petal.Length", 10:20, 3))
#' sepal_l <- index("Sepal.Length")
#' view(iris, c_l(sepal_l, id_l, 3))
#' @export
c_l <- function(...){
  dots <- list(...)
  Reduce(function(acc, x){
    if(inherits(x, "lens"))
      return(acc %.% x)

    if(!is.vector(x) || is.null(x))
      stop("`c_l` expects all arguments to be either a lens or a atomic vector")

    if(length(x) == 1 && ! (is.numeric(x) && x < 0))
      return(acc %.% index(x))
    
    return(acc %.% indexes(x))
  }, dots, init = id_l)
}

#' Unlist lens
#'
#' A lens between a list and an unrecursively [unlist]ed object.
#' @examples
#' (x <- list(x = list(y = 1:10)))
#' view(x, unlist_l)
#' set(x, unlist_l %.% unlist_l, rep("hello", 10))
#' @export
unlist_l <-
  lens(view =
         function(d){
           if(!is.list(d)) stop("`unlist_l` only works on lists") 
           Reduce(c, d)
         }
     , set =
         function(d, x){
           if(!is.list(d)) stop("`unlist_l` only works on lists")
           
           d[] <- Reduce(function(acc, v){
             max_ind <- acc$max_ind + length(v)
             inds <- seq(acc$max_ind + 1, max_ind, by = 1)
             
             list(max_ind = max_ind, new_vals = c(acc$new_vals, list(x[inds])))
           }, d, init = list(max_ind = 0, inds = NULL))$new_vals
           d
         })
  
