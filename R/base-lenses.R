#' The identity (trivial lens)
#'
#' This lens focuses on the whole object
#' @export
id_l <- lens(identity, function(., x) x)

#' Construct a lens into an index/name
#'
#' This is the lens version of `[[`
#'
#' @param el The element the lens should point to
#' can be an `integer` or name.
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

#' Construct a lens into a subset of an object
#'
#' This is the lens version of `[`
#'
#' @param els a subset vector, can be `integer`, `character`
#' of `logical`, pointing to one or more elements of the object
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

#' Slot lens
#'
#' The lens equivalent of `@` and `@<-`
#' for getting and setting S4 object slots.
#' @param slot the name of the slot
#' @export
slot_l <- function(slot){
  lens(view = function(d) eval(bquote(`@`(d, .(slot))))
     , set = function(d, x) eval(bquote(`@<-`(d, .(slot), x))))
}

#' A lens into the names of an object
#'
#' The lens versions of `names` and `names<-`.
#' @export
names_l <- lens(view = names
              , set = `names<-`)

#' A lens into the column names of an object
#'
#' The lens version of `colnames` and `colnames<-`
#' @export
colnames_l <- lens(view = colnames
                  , set = `colnames<-`)

#' A lens into the row names of an object
#'
#' The lens version of `rownames` and `rownames<-`
#' @export
rownames_l <- lens(view = rownames
                 , set = `rownames<-`)

#' Dims lens
#'
#' A lens into an objects dimensions
#' @export
dim_l <- lens(view = dim
            , set = `dim<-`)


#' Construct a lens into an attribute
#'
#' The lens version of `attr` and `attr<-`
#' @param attrib A length one character vector indicating
#' the attribute to lens into.
#' @export
attr_l <- function(attrib){
  lens(view = function(d) attr(d, attrib)
     , set = function(d, x) `attr<-`(d, attrib, x))
}

#' Environment lens
#'
#' A lens into the environment of an object. This
#' is the lens version of [environment] and [environment<-]
#' @export
env_l <- lens(environment, `environment<-`)

#' Row lens
#'
#' Create a lens into a set of rows
#'
#' @param rows the rows to focus on
#' @param drop whether or not to drop dimensions with length 1 
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
#' one for each element. 
#' @examples
#' (ex <- replicate(10, sample(1:5), simplify = FALSE))
#' view(ex, map_l(index(1)))
#' set(ex, map_l(index(1)), 11:20)
#' @export
map_l <- function(l){
  lens(view = function(d) lapply(d, view, l)
     , set = function(d, x){
       mapply(set, d, x, MoreArgs = list(l = l), SIMPLIFY = FALSE) 
     })
}

#' Attributes lens
#'
#' The lens equivalent of [attributes] and [attributes<-]
#' @export
attributes_l <-
  lens(view = attributes
     , set = `attributes<-`)

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
#' @export
class_l <-
  lens(view = class
     , set = `class<-`)

#' Dimnames lens
#'
#' A lens into the dimnames of an object. Lens
#' equivalent of [dimnames] and [dimnames<-].
#' @export
dimnames_l <-
  lens(view = dimnames
     , set = `dimnames<-`)

#' Levels lens
#'
#' A lens into the levels of an object. Usually
#' this is factor levels. Lens
#' equivalent of [levels] and [levels<-].
#' @export
levels_l <-
  lens(view = levels
     , set = `levels<-`)
