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
#' @title An R6 baseclass that adds functionality equivalent to static methods and fields in Java.
#' @description File follows the `utils_TEMPLATE.R`-pattern:
#'              Contains single [get_r6_baseclass_env()] method that returns an environment containing a custom `R6` baseclass
#'              constructor object. The latter creates objects that provide public access to
#'              their static methods and fields via the [get_static_env()] method. The latter
#'              returns an environment containing the former.
#'
#' @seealso `utils_lang_r6_subclass_TEMPLATE.R` This is a template file 
#'          that should be used as the basis for R6 classes that are subclasses of this baseclass
#'          (i.e. that should also provide static functionality).
#' @md

#' TODO: Rework static system as real, "under the hood" R6 extension

# Include guard
if (!exists("UTILS_LANG_R6_BASECLASS_R", inherits = FALSE)) {
  UTILS_LANG_R6_BASECLASS_R = TRUE
  
  #' @return An environment containing a custom R6 baseclass constructor object.
  #' @export
  #'
  #' @examples
  #' @md
  get_utils_lang_r6_baseclass_env <- function() {
    local_env <- environment() # This function's local environment
    
    #' --------- Load required script dependencies -------
    other_scripts <- c(file.path(".","utils","utils_lang_general.R"), 
                       file.path(".","utils","utils_lang_package.R"), 
                       file.path(".","utils","utils_lang_error.R"), 
                       file.path(".","utils","utils_lang_typechecks.R"))
    if(length(other_scripts) > 0){
      invisible(lapply(other_scripts, source, local = local_env))
    }
    
    #' Create locked (and bindings-locked) environment holding the methods previously added to `other_scripts`.
    static_env <- new.env()
    assign("utils_lang_general", get_utils_lang_general_env(), envir = static_env)
    assign("utils_lang_package", get_utils_lang_package_env(), envir = static_env)
    assign("utils_lang_error", get_utils_lang_error_env(), envir = static_env)
    assign("utils_lang_typechecks", get_utils_lang_typechecks_env(), envir = static_env)
    
    #' Short-hand variants of the above:
    assign("gen", static_env$utils_lang_general, envir = static_env)
    assign("pkg", static_env$utils_lang_package, envir = static_env)
    assign("err", static_env$utils_lang_error, envir = static_env)
    assign("tcs", static_env$utils_lang_typechecks, envir = static_env)
    lockEnvironment(static_env, bindings = TRUE)
    
    
    # --------- Load required package dependencies -------    
    # Append other packages to be installed (if necessary) and loaded
    package_dependencies <- c("R6")
    
    if(!static_env$utils_lang_package$install_and_load(package_dependencies, stop_on_failure = TRUE)){
      stop("At least one required package could not be installed and/or loaded.")
    }
    
    #' Cleanup all but `local_env`, `static_env`
    rm(list=setdiff(ls(envir = local_env), c("static_env", "local_env")), envir = local_env)
    
    #' -------- Create private static helper methods and/or fields here: -------
    #' Shorthand access to local and static environment
    l <- local_env
    s <- static_env
    
    
    # -------- Create environment to be returned containing 
    #          one or multiple topically related methods and/or fields -------
    #' Use `local_env` (or `l`) to access the enclosing environment and `static_env`
    #' (or `s` or `local_env$static_env` or `l$s`, ...) to access static methods and/or fields
    #' previously added via `other_scripts`.
    #'
    result_env <- new.env()
    evalq(
      {
R6_Base <- R6Class("R6_Base",
             # ---- R6 inheritance -----
             #inherit = <R6 object constructor object>,
             
             # ---- R6 options --------
             portable = TRUE,
             cloneable = TRUE,
             lock_class = TRUE,
             lock_objects = TRUE,
             
             #----- Private Fields & methods ----
             private = list(
               
             ),
             #-----end of private-----
             
             #----- Public Fields & methods ----
             public = list(
               
               #' Static access to an environment containg a set of
               #' methods and/or fields. By default, it contains subenvironments
               #' from the `get_<name>_env()` methods from
               #' `utils_lang_general.R`, `utils_lang_package.R`, 
               #' `utils_lang_error.R` and `utils_lang_typecheck.R`. Subclasses may add additional environments.
               #' See also `other_scripts`.
               #'
               #' @return An environment containing set of methods and/or fields.
               #' @export
               #'
               #' @examples
               #' @md
               get_static_env = function(){
                 local_env$static_env
               },
               
               #' The `R6` `initialize` method. \cr
               #' __IMPORTANT__: Call `super$initialize(...)` at the beginning, 
               #' if you overwrite this method (with `...` being the parameters of the parent class'
               #' `initialize`` method).
               #'
               #' @return NULL
               #' @export
               #'
               #' @examples
               #' @md
               initialize = function(){},
               
               #' The `R6` `finalize` method. \cr
               #' __IMPORTANT__: Call `super$finalize()` at the beginning, 
               #' if you overwrite this method.
               #'
               #' @return NULL
               #' @export
               #'
               #' @examples
               #' @md
               finalize = function() {}
               
             ),
             #-----end of public-----
             
             
             #----- Active bindings ----
             active = list(
               
             )
             #-----end of active-----
          )
      },
      envir = result_env)
    lockEnvironment(local_env, bindings = TRUE)
    invisible(result_env)
  }
} # End of include guard