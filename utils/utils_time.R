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
#' @title  Collection of methods and/or fields related to time and date.
#' @description Contains a single [get_utils_time_env()] method
#'   that returns an environment containing those methods.
#' @md

# Include guard
if (!exists("UTILS_TIME_R", inherits = FALSE)) {
  UTILS_TIME_R = TRUE
  
  #' @return An environment containing one or multiple methods and/or fields related to time and date.
  #' @export
  #'
  #' @examples
  #' @md
  get_utils_time_env <- function() {
    local_env = environment() # This function's local environment
    
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
        #' Returns the UNIX time in milliseconds.
        #'
        #' @return Equivalent to: `as.numeric(Sys.time()) * 1000`.
        #' @export
        #'
        #' @examples
        #' @md
        get_unix_time <- function(){
          return(as.numeric(Sys.time()) * 1000)
        }
      },
      envir = result_env)
    lockEnvironment(local_env, bindings = TRUE)
    invisible(result_env)
  }
} # End of include guard