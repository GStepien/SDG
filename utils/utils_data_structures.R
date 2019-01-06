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
#' @title  Collection of methods and/or fields for the creation of different data structures.
#' @description Contains a single [get_utils_data_structures_env] method
#'   that returns an environment containing those methods.
#' @md

# Include guard
if (!exists("UTILS_DATA_STRUCTURES_R", inherits = FALSE)) {
  UTILS_DATA_STRUCTURES_R = TRUE
  
  #' @return An environment containing one or multiple methods and/or fields for 
  #'   the creation of different data structures.
  #' @export
  #'
  #' @examples
  #' @md
  get_utils_data_structures_env <- function() {
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
    other_scripts <- c()   # e.g.: c(file.path(".", "utils", "utils_math_general.R"))
    if(length(other_scripts) > 0){
      invisible(lapply(other_scripts, source, local = local_env))
    }
    
    #' Create locked (and bindings-locked) environment holding the methods previously added to `other_scripts`.
    other_env <- new.env()
    #' Append `assign` calls for each of the scripts previously added to `other_scripts`.
    #'       Each call should be of the following pattern:
    #'         `assign("<name>", get_<name>_env(), envir = other_env)`
    #'       For examples, see below and the corresponding lines in `utils_lang_r6_baseclass.R`.
    #' __Example__: `assign("math_general", get_utils_math_general_env(), envir = other_env)`, if
    #'              `file.path(".", "utils", "utils_math_general.R")` was added to `other_scripts` in the lines before.
    
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
        #' Return an enum like object `e` whose fields can be accessed via `e$<name>`.
        #' 
        #' Internally, `e` is a locked (and bind-locked) environment with the following set of variables:
        #' For the `i-th` entry `name` in `field_names` (i.e., `name = field_names[[i]]`), 
        #' we create a variabel in `e` of the name `as.character(name)` and
        #' set its value to `i`. The resulting environment `e` is then locked and bind-locked and returned.
        #' 
        #'
        #' @param field_names A data structure containing a set of character-coercible names to be
        #'   used as enum-fields.
        #'
        #' @return An enum like object `e` whose fields can be accessed via `e$<name>` and `e[["<name>"]]`.
        #' 
        #' @export
        #' @md
        make_enum <- function(field_names = c()) {
          e <- new.env(parent=emptyenv())
          n <- length(field_names)
          if(n > 0){
            sapply(1:n, FUN = function(i) {
              assign(x = as.character(field_names[[i]]), value = i, envir = e)
            })
          }
          lockEnvironment(e, bindings = TRUE)
          return(e)
        }
        
        #' A convenience method for creating functions which,
        #' independently of their argument, always
        #' return the same constant value `x`.
        #'
        #' @param x The object to be returned by the returned function.
        #'
        #' @return A function which always returns `x`.
        #' 
        #' @export
        #' @md
        const_fun <- function(x){
          force(x)
          result <- function(...) {
            x
          }
          invisible(result)
        }
        
        #' A simple identity method simply returning its arguments.
        #'
        #' @return If `...` contains only one element (i.e., `list(...)` has length one), this element is returned. If `...` contains
        #'    zero elements, `NULL` is returned. In every other case, the method returns `list(...)`.
        #' @export
        #'
        #' @examples
        #' @md
        identity <- function(...){
          l <- list(...)
          if(length(l) == 0){
            return(NULL)
          }
          else if(length(l) == 1){
            return(l[[1]])
          }
          else{
            return(l)
          }
        }
      },
      envir = result_env)
    lockEnvironment(local_env, bindings = TRUE)
    invisible(result_env)
  }
} # End of include guard