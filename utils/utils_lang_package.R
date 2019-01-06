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

#' @author Grzegprz Stepien
#' @title  Collection of methods and/or fields for package installing and loading.
#' @description Contains a single [get_utils_lang_package_env()] method
#'   that returns an environment containing those methods.
#' @md

# Include guard
if (!exists("UTILS_PACKAGE_R", inherits = FALSE)) {
  UTILS_PACKAGE_R = TRUE
  
  #' @return An environment containing one or multiple methods and/or fields for package installing and loading.
  #' @export
  #'
  #' @examples
  #' @md
  get_utils_lang_package_env <- function() {
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
    
    #' Cleanup all but `local_env`, `other_env` and `package_dependencies` (latter removed at the end)
    rm(list=setdiff(ls(envir = local_env), c("other_env", "local_env", "package_dependencies")), envir = local_env)
    
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
        #' Install (if necessary) and load all packages in the provided data structure.
        #'
        #' For all package names `p` in `packages`:
        #' Method checks if `p` is already installed and, if not, downloads it from standard repositories and installs it
        #' along all its required dependencies. Method then loads package `p`. 
        #'
        #' @param packages A character vector-coercible data structure containing string names of packages to be installed.
        #' @param stop_on_failure If `TRUE`, the method calls `stop()` as soon as a package could not be
        #'                   installed and/or loaded correctly. `returnVector` is ignored if the latter case happens.
        #' @param returnVector See return value.
        #'
        #' @return If `returnVector` is `TRUE`, this method returns a logical vector of the same 
        #'         length as `packages` containing TRUE at its i-th position
        #'         if and only if the i-th element in `packages`
        #'         could be successfully installed (if necessary) and loaded.
        #'         If `returnVector` is false, the method returns `TRUE` if and only if 
        #'         all packages coulbe be successfully installed (if necessary) and loaded.
        #' @export
        #'
        #' @examples
        #' @md
        install_and_load <- function(packages, stop_on_failure=TRUE, returnVector=FALSE){
          packages <- as.vector(packages, mode="character")
          n <- length(packages)
          
          if(n == 0){
            if(returnVector){
              return(vector(mode = "logical", length = 0))
            }
            else{
              return(TRUE)
            }
          }
          
          result <- vector(mode = "logical", length=n)
          
          err_handler <- function(e) {
            warning(as.character(e), immediate.=TRUE)
          }
          
          for(i in 1:n){
            success <- FALSE
            tryCatch({success <- require(packages[i], character.only = TRUE)},
                     error = err_handler)
            
            if(success){
              result[i] <- TRUE
            }
            else {
              tryCatch({install.packages(packages[i], dependencies = TRUE, repos = "http://cran.us.r-project.org")},
                       error = err_handler)
              
              # At this point, 'success' is still false
              tryCatch({success <- require(packages[i], character.only = TRUE)},
                       error = err_handler)
              
              if(success){
                result[i] <- TRUE
              }
              else {
                if(stop_on_failure){
                  stop(paste("At least one required package could not be installed and/or loaded:",packages[i]))
                }
                else{
                  result[i] <- FALSE
                }
              }
            }
          }
          if(!returnVector){
            result <- all(result)
          }
          
          return(result)
        }
      },
      envir = result_env)
    
    if(!result_env$install_and_load(package_dependencies, stop_on_failure = TRUE)){
      stop("At least one required package could not be installed and/or loaded.")
    }
    
    rm(list=c("package_dependencies"), envir = local_env)
    
    lockEnvironment(local_env, bindings = TRUE)
    invisible(result_env)
  }
} # End of include guard