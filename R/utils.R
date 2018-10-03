symbol_gatherer <-
  function(expr){
    if(is.name(expr)) return(as.character(expr))
    if(!is.call(expr)) return(NULL)
    
    unlist(lapply(as.list(expr), symbol_gatherer))
  }

as_lambda <- function(x, env = rlang::caller_env()){
  rlang::coerce_type(x, friendly_type("function"), primitive = , closure = {
        x
    }, formula = {
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
    }, string = {
        get(x, envir = env, mode = "function")
    })
}

as_closure <-
  function (x, env = caller_env(), lambda = FALSE) {
    to_fun <- `if`(lambda, as_lambda, rlang::as_function)
    x <- to_fun(x, env = env)
    rlang::coerce_type(x, "a closure", closure = x, primitive = {
      fn_name <- rlang::prim_name(x)
      `%||%` <- rlang::`%||%`
        fn <- rlang:::op_as_closure(fn_name)
        if (!is_null(fn)) {
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
        prim_call <- call2(fn_name, rlang::splice(args))
        new_function(fmls, prim_call, base_env())
    })
}
