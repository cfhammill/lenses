utils::globalVariables(c(".", "!!", "!<-", "(<-")
                     , "lenses")


symbol_gatherer <-
  function(expr){
    if(is.name(expr)) return(as.character(expr))
    if(!is.call(expr)) return(NULL)
    
    unlist(lapply(as.list(expr), symbol_gatherer))
  }

as_lambda <- function(x, env = rlang::caller_env()){
  rlang::coerce_type(
           x
         , rlang::friendly_type("function")
         , primitive =
         , closure = {
           x
         }
       , formula = {
         if (length(x) == 2) {
           args <- list(... = rlang::missing_arg()
                      , .x = quote(..1)
                      , .y = quote(..2)
                      , . = quote(..1)
                        )
         } else {
           lhs <- rlang::f_lhs(x)
           if(length(lhs) > 1){
             if(any(vapply(lhs, length, numeric(1)) > 1))
               stop("LHS of a lambda must be a single symbol, or a single expression "
                  , "of symbols, it appears your expression is nested.")
             lhs <- lhs[-1]
          }

           args <- rep(list(rlang::missing_arg()), length(lhs))
           names(args) <- vapply(lhs, deparse, character(1))
         }

         rlang::new_function(args, rlang::f_rhs(x), rlang::f_env(x))
        }
      , string = {
        get(x, envir = env, mode = "function")
     })
}

as_closure <-
  function (x, env = rlang::caller_env()) {
    x <- as_lambda(x, env = env)
    rlang::coerce_type(x, "a closure", closure = x, primitive = {
      fn_name <- rlang::prim_name(x)
      `%||%` <- rlang::`%||%`
        fn <- op_as_closure(fn_name)
        if (!rlang::is_null(fn)) {
            return(fn)
        }
        if (fn_name == "eval") {
            fmls <- formals(base::eval)
        }
        else {
            fmls <- formals(.ArgsEnv[[fn_name]] %||% .GenericArgsEnv[[fn_name]])
        }
        args <- rlang::syms(names(fmls))
        args <- rlang::set_names(args)
        names(args)[(names(args) == "...")] <- ""
        prim_call <- rlang::call2(fn_name, rlang::splice(args))
        rlang::new_function(fmls, prim_call, rlang::base_env())
    })
  }

op_as_closure <- function(prim_nm) {
  switch(prim_nm,
    `<-` = ,
    `<<-` = ,
    `=` = function(.x, .y) {
      op <- rlang::sym(prim_nm)
      expr <- expr((!!op)(!!rlang::enexpr(.x), !!rlang::enexpr(.y)))
      rlang::eval_bare(expr, rlang::caller_env())
    },
    `@` = ,
    `$` = function(.x, .i) {
      op <- rlang::sym(prim_nm)
      expr <- expr((!!op)(.x, !!rlang::quo_squash(rlang::enexpr(.i), warn = TRUE)))
      rlang::eval_bare(expr)
    },
    `[[<-` = function(.x, .i, .value) {
      expr <- rlang::expr((!!rlang::enexpr(.x))[[!!rlang::enexpr(.i)]] <- !!rlang::enexpr(.value))
      rlang::eval_bare(expr, rlang::caller_env())
    },
    `[<-` = function(.x, ...) {
      args <- rlang::exprs(...)
      n <- length(args)
      if (n < 2L) {
        rlang::abort("Must supply operands to `[<-`")
      }
      expr <- rlang::expr((!!rlang::enexpr(.x))[!!!args[-n]] <- !!args[[n]])
      rlang::eval_bare(expr, rlang::caller_env())
    },
    `@<-` = function(.x, .i, .value) {
      expr <- rlang::expr(`@`(!!rlang::enexpr(.x), !!rlang::enexpr(.i)) <- !!rlang::enexpr(.value))
      rlang::eval_bare(expr, rlang::caller_env())
    },
    `$<-` = function(.x, .i, .value) {
      expr <- rlang::expr(`$`(!!rlang::enexpr(.x), !!rlang::enexpr(.i)) <- !!rlang::enexpr(.value))
      rlang::eval_bare(expr, rlang::caller_env())
    },
    `(` = function(.x) .x,
    `[` = function(.x, ...) .x[...],
    `[[` = function(.x, ...) .x[[...]],
    `{` = function(...) {
      values <- list(...)
      values[[length(values)]]
    },
    `&` = function(.x, .y) .x & .y,
    `|` = function(.x, .y) .x | .y,
    `&&` = function(.x, .y) .x && .y,
    `||` = function(.x, .y) .x || .y,
    `!` = function(.x) !.x,
    `+` = function(.x, .y) if (missing(.y)) .x else .x + .y,
    `-` = function(.x, .y) if (missing(.y)) -.x else .x - .y,
    `*` = function(.x, .y) .x * .y,
    `/` = function(.x, .y) .x / .y,
    `^` = function(.x, .y) .x ^ .y,
    `%%` = function(.x, .y) .x %% .y,
    `<` = function(.x, .y) .x < .y,
    `<=` = function(.x, .y) .x <= .y,
    `>` = function(.x, .y) .x > .y,
    `>=` = function(.x, .y) .x >= .y,
    `==` = function(.x, .y) .x == .y,
    `!=` = function(.x, .y) .x != .y,
    `:` = function(.x, .y) .x : .y,
    `~` = function(.x, .y) {
      if (rlang::is_missing(substitute(.y))) {
        rlang::new_formula(NULL, substitute(.x), rlang::caller_env())
      } else {
        rlang::new_formula(substitute(.x), substitute(.y), rlang::caller_env())
      }
    },

    # Unsupported primitives
    `break` = ,
    `for` = ,
    `function` = ,
    `if` = ,
    `next` = ,
    `repeat` = ,
    `return` = ,
    `while` = {
      nm <- paste0("`", prim_nm, "`")
      rlang::abort(paste0("Can't coerce the primitive function ", nm, " to a closure"))
    }
  )
}
