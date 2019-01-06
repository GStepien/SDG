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

#' @author  Grzegorz Stepien
#' @title  Collection of methods and/or fields for general mathematical operations.
#' @description Contains a single [get_<name>_env()] method
#'   that returns an environment containing those methods.
#' @md

# Include guard
if (!exists("UTILS_MATH_GENERAL_R", inherits = FALSE)){
  UTILS_MATH_GENERAL_R = TRUE
  
  #' @return An environment containing one or multiple methods and/or fields for general mathematical operations.
  #' @export
  #'
  #' @examples
  #' @md
  get_utils_math_general_env <- function() {
    local_env <- environment() # This function's local environment
    
    #' --------- Load required script dependencies -------
    #' TODO: __Optional__: Append paths to required R scripts to `other_scripts`-vector below. Each script should 
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
    
    #' __Optional__ -------- Create private helper methods and/or fields here: -------
    
    
    
    # -------- Create environment to be returned containing 
    #          one or multiple topically related methods and/or fields -------
    #' Use `local_env` to access the enclosing environment and `other_env`
    #' (or `local_env$other_env`) to access subenvironments from
    #' the scripts from `other_scripts`.
    #'
    result_env <- new.env()
    evalq(
      {
        #' Convenience method for unified matrix-scalar and matrix-matrix multiplication.
        #'
        #' @param x First factor.
        #' @param y Second factor
        #'
        #' @return If at least one of the provided arguments has length one, this method returns `x*y`.
        #'   Otherwise it returns `x %*% y`.
        #' @export
        #'
        #' @examples
        #' @md
        `%m%` <- function(x,y){
          if(length(x) == 1 || length(y) == 1){
            return(x * y)
          }
          else{ 
            return(x %*% y)
          }
        }
        
        #' Convenience method for unified matrix-scalar and matrix-matrix division
        #'
        #' @param x Dividend.
        #' @param y Divisor.
        #'
        #' @return Equivalent to `x %m% y^-1`.
        #' @export
        #'
        #' @examples
        #' @md
        `%d%` <- function(x,y){
          return(result_env$`%m%`(x, y^-1))
        }
        
        #' Infix variant of `xor(x,y)`.
        #'
        #' @param x A vector of logical expressions.
        #' @param y A vector of logical expressions of the same length as `x`.
        #'  
        #' @export
        #'
        #' @return `TRUE` if and only if exactly one of the two expressions are `TRUE`
        #' @examples
        #' @md
        `%xor%` <- function(x,y){
          return(xor(x, y))
        }
        
        #' Infix equivalence operator. Equivalent to `xor(!x,y)`.
        #'
        #' @param x A logical expression.
        #' @param y A logical expression of the same length as `x`.
        #'
        #' @return `TRUE` if and only if both arguments are `TRUE` or both are `FALSE`.
        #' @export
        #'
        #' @examples
        #' @md
        `%eq%` <- function(x,y){
          return(xor(!x, y))
        }
        
        #' Infix implication operator.
        #'
        #' @param x A vector of logical expressions of length.
        #' @param y A vector of logical expressions.
        #'
        #' @return If `x` contains only `FALSE` entries (i.e. `!any(as.logical(x))` is `TRUE`),
        #'   the method returns `!as.logical(x)` (note that in that case `y` is __NOT__ evaluated).
        #'   Otherwise it returns `!x | y`.
        #' @export
        #'
        #' @examples
        `%then%` <- function(x,y){
          x <- as.logical(x)
          if(all(z <- !x)){
            return(z)
          }
          else{
            return(!x | y)
          }
        }
      },
      envir = result_env)
    lockEnvironment(local_env, bindings = TRUE)
    invisible(result_env)
  }
} # End of include guard