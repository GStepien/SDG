# SDG: A synthetic datastream generator. 
#      A generator of synthetic, multivariate & heterogeneous datasteams with 
#      probabilistically repeating patterns.
#
#    Copyright (c) 2018 Grzegorz Stepien
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.
# 
#    For more details, see './LICENSE.md'
#    (where '.' represents this program's root directory).

#' @author Grzegorz Stepien
#' @title  Collection of methods and/or fields for handling errors and warnings.
#' @description Contains a single [get_utils_lang_error_env()] method
#'   that returns an environment containing those methods.
#'   
#' @md

# TODO: Reconsider err$assert_msg vs err$stopifnot calls 

# Include guard
if (!exists("UTILS_LANG_ERROR_R", inherits = FALSE)) {
  UTILS_LANG_ERROR_R = TRUE
  
  #' @return An environment containing one or multiple methods and/or fields for handling errors and warnings.
  #' @export
  #'
  #' @examples
  #' @md
  get_utils_lang_error_env <- function() {
    local_env <- environment() # This function's local environment
    
    #' --------- Load required script dependencies -------
    #' __Optional__: Append paths to required R scripts to `other_scripts`-vector below. Each script should 
    #'                     contain some thematically related methods and/or fields required by the
    #'                     methods added to `result_env` below.
    #'                     __IMPORTANT__: Each of those scripts should be structured according to this template file, 
    #'                                    i.e., have a single method 
    #'                                    `get_<name>_env()` (e.g., `get_utils_lang_package_env()` 
    #'                                    from `utils_lang_package.R`) that returns an environment containing
    #'                                    all its methods and/or fields.
    other_scripts <- c(file.path(".","utils","utils_lang_typechecks.R"))   # e.g.: c(file.path(".", "utils", "utils_math_general.R"))
    if(length(other_scripts) > 0){
      invisible(lapply(other_scripts, source, local = local_env))
    }
    
    #' Create locked (and bindings-locked) environment holding the methods previously added to `other_scripts`.
    other_env <- new.env()
    #' Append `assign` calls for each of the scripts previously added to `other_scripts`.
    #'       Each call should be of the following pattern:
    #'         `assign("<name>", get_<name>_env(), envir = other_env)`
    #'       For examples, see below and the corresponding lines in `utils_lang_r6_baseclass.R`.
    #' __Example__: `assign("utils_math_general", get_utils_math_general_env(), envir = other_env)`, if
    #'              `file.path(".", "utils", "utils_math_general.R")` was added to `other_scripts` in the lines before.
    assign("utils_lang_typechecks", get_utils_lang_typechecks_env(), envir = other_env)
    lockEnvironment(other_env, bindings = TRUE)
    
    
    # --------- Load required package dependencies -------    
    #' Append other packages to `package_dependencies` which are to be installed (if necessary) and loaded
    package_dependencies <- c() # e.g., c("R6","smoother","rowr","randomcoloR")
    
    invisible(source(file.path(".", "utils", "utils_lang_package.R"), local = TRUE))
    lang_package <- get_utils_lang_package_env()
    if(!lang_package$install_and_load(package_dependencies, stop_on_failure = TRUE)){
      stop("At least one required package could not be installed and/or loaded.")
    }
    
    #' Cleanup all but `local_env`, `other_env`
    rm(list=setdiff(ls(envir = local_env), c("other_env", "local_env")), envir = local_env)
    
    # -------- Create private helper methods here: -------
    .assertions_active <- FALSE
    
    #' Adapted [base::stopifnot] code.
    #'@md
    .stopifnot_unsafe <- function(msg, mc, ...){
      n <- length(ll <- list(...))
      if (n == 0L) 
        return(invisible())
      Dparse <- function(call, cutoff = 60L) {
        ch <- deparse(call, width.cutoff = cutoff)
        if (length(ch) > 1L) 
          paste(ch[1L], "....")
        else ch
      }
      head <- function(x, n = 6L) x[seq_len(if (n < 0L) max(length(x) + 
                                                              n, 0L) else min(n, length(x)))]
      abbrev <- function(ae, n = 3L) paste(c(head(ae, n), if (length(ae) > 
                                                              n) "...."), collapse = "\n  ")
      
      for (i in 1L:n) if (!(is.logical(r <- ll[[i]]) && !anyNA(r) && 
                            all(r))) {
        cl.i <- mc[[i + 1L]]
        msg1 <- if (is.call(cl.i) && identical(cl.i[[1]], quote(all.equal)) && 
                   (is.null(ni <- names(cl.i)) || length(cl.i) == 3L || 
                    length(cl.i <- cl.i[!nzchar(ni)]) == 3L)) 
          sprintf(gettext("%s and %s are not equal:\n  %s"), 
                  Dparse(cl.i[[2]]), Dparse(cl.i[[3]]), abbrev(r))
        else sprintf(ngettext(length(r), "%s is not TRUE", "%s are not all TRUE"), 
                     Dparse(cl.i))
        
        if(is.null(msg)){
          msg <- msg1
        }
        else {
          msg <- paste(msg, paste0("Original error message: ", msg1), sep = '\n')
        }
        
        stop(msg, call. = FALSE, domain = NA)
      }
      invisible()
    }
    
    #' Adapted [base::stopifnot] code.
    #'@md
    .warnifnot_unsafe <- function(msg, mc, ...){
      n <- length(ll <- list(...))
      if (n == 0L) 
        return(invisible())
      Dparse <- function(call, cutoff = 60L) {
        ch <- deparse(call, width.cutoff = cutoff)
        if (length(ch) > 1L) 
          paste(ch[1L], "....")
        else ch
      }
      head <- function(x, n = 6L) x[seq_len(if (n < 0L) max(length(x) + 
                                                              n, 0L) else min(n, length(x)))]
      abbrev <- function(ae, n = 3L) paste(c(head(ae, n), if (length(ae) > 
                                                              n) "...."), collapse = "\n  ")
      
      for (i in 1L:n) if (!(is.logical(r <- ll[[i]]) && !anyNA(r) && 
                            all(r))) {
        cl.i <- mc[[i + 1L]]
        msg1 <- if (is.call(cl.i) && identical(cl.i[[1]], quote(all.equal)) && 
                    (is.null(ni <- names(cl.i)) || length(cl.i) == 3L || 
                     length(cl.i <- cl.i[!nzchar(ni)]) == 3L)) 
          sprintf(gettext("%s and %s are not equal:\n  %s"), 
                  Dparse(cl.i[[2]]), Dparse(cl.i[[3]]), abbrev(r))
        else sprintf(ngettext(length(r), "%s is not TRUE", "%s are not all TRUE"), 
                     Dparse(cl.i))
        
        if(is.null(msg)){
          msg <- msg1
        }
        else {
          msg <- paste(msg, "Original warning message:", msg1, sep = '\n')
        }
        
        warning(msg, call. = FALSE, domain = NA)
      }
      invisible()
    }
    
    # -------- Create environment to be returned containing 
    #          one or multiple topically related methods and/or fields -------
    #' Use `local_env` to access the enclosing environment and `other_env`
    #' (or `local_env$other_env`) to access subenvironments from
    #' the scripts from `other_scripts`.
    #'
    #' Add one or multiple methods and/or fields to the `evalq` expression below.
    result_env <- new.env()
    evalq(
      {
        #' Activate or deactivate assertions.
        #'
        #' @param active If `TRUE`, assertions are activated. If it is `FALSE`, they are deactivated.
        #'   If `active` is neither of those, an error is produced.
        #' @export
        #'
        #' @examples
        #' @md
        set_assertions_status <- function(active){
          base::stopifnot(other_env$utils_lang_typechecks$has_length_1(active, NA_on_fail = FALSE),
                          other_env$utils_lang_typechecks$is_logical(active,
                                                                     accept_NULL = FALSE,
                                                                     accept_NaN = FALSE,
                                                                     accept_NA = FALSE))
          
          unlockBinding(".assertions_active", env = local_env)
          local_env$.assertions_active <- active
          lockBinding(".assertions_active", env = local_env)
        }
        
        #' Check whether assertions are active. See [assert()] for more information.
        #'
        #' @return `TRUE` if and only if assertions are active.
        #' @export
        #'
        #' @examples
        #' @md
        get_assertions_status <- function(){
          return(local_env$.assertions_active)
        }
        
        #' Equivalent to a call to `assert_msg(msg = NULL, ...)`.
        #' @md
        assert <- function(...){
          if(local_env$.assertions_active){
            mc <- match.call()
            local_env$.stopifnot_unsafe("Assertion violated:", mc, ...)
          }
        }
        
        #' If assertions are deactivated (see [get_assertion_status()] and [set_assertion_status()]), this method
        #' does nothing. If assertions are activated, it behaves equivalently to a call to
        #' `stopifnot(paste0("Assertion violated: ", msg, '\n'), ...)`.
        #' @md
        assert_msg <- function(msg, ...){
          if(local_env$.assertions_active){
            base::stopifnot(is.null(msg) || other_env$utils_lang_typechecks$is_character(msg,
                                                                                         accept_NULL = FALSE,
                                                                                         accept_NaN = FALSE,
                                                                                         accept_NA = FALSE))
            mc <- match.call()
            mc[2] <- NULL
            local_env$.stopifnot_unsafe(paste0("Assertion violated: ", msg), mc, ...)
          }
        }
        
        #' Similar to [base::stopifnot()]. The only difference is that this method prepends the provided message `msg`
        #' to the error message output. Otherwise, this method has the exact same behavior.
        #'
        #' @param msg A message to be prepended to the lines containing the error message output. `NULL` indicates that
        #'   no custom message shall be prepended. If `msg` contains more than one strings, then they are concatenated
        #'   via linebreaks.
        #' @param ... Any number of logical R expressions. See [base::stopifnot()].
        #'
        #' @export
        #'
        #' @examples
        #' @md
        stopifnot <- function(msg, ...){
          # Do not use assertions here or this error might get overlooked if they are deactivated and no message was provided
          base::stopifnot(is.null(msg) || other_env$utils_lang_typechecks$is_character(msg,
                                                                                       accept_NULL = FALSE,
                                                                                       accept_NaN = FALSE,
                                                                                       accept_NA = FALSE))
          mc <- match.call()
          mc[2] <- NULL
          local_env$.stopifnot_unsafe(msg, mc, ...)
        }
        
        #' Similar to [result_env$stopifnot()], but instead of stopping when the first non-true expression occurs, this
        #' method simply prints a warning containing `msg` followed by the error message, that [base::stopifnot()]
        #' would have printed for that expression. The method then continues with evaluating the other logical expressions in `...`
        #' in the same manner..
        #'
        #' @param msg A message to be prepended to the lines containing the warning message output. `NULL` indicates that
        #'   no custom message shall be prepended. If `msg` contains more than one strings, then they are concatenated
        #'   via linebreaks.
        #' @param ... Any numbetr of logical R expressions. See [base::stopifnot()].
        #'
        #' @export
        #'
        #' @examples
        #' @md
        warnifnot <- function(msg, ...){
          # Do not use assertions here or this error might get overlooked if they are deactivated and no message was provided
          base::stopifnot(is.null(msg) || other_env$utils_lang_typechecks$is_character(msg,
                                                                                       accept_NULL = FALSE,
                                                                                       accept_NaN = FALSE,
                                                                                       accept_NA = FALSE))
          mc <- match.call()
          mc[2] <- NULL
          local_env$.warnifnot_unsafe(msg, mc, ...)
        }
      },
      envir = result_env)
    lockEnvironment(local_env, bindings = TRUE)
    invisible(result_env)
  }
} # End of include guard