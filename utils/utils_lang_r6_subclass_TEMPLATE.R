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
#' @title A template file for R6 subclasses of `R6_Base` from `utils_lang_r6_baseclass.R`. 
#' @description File follows the `utils_TEMPLATE.R`-pattern:
#'              Should contain a single `get_<name>_env()` method (here: [get_utils_lang_r6_subclass_template_env()]) 
#'              that returns an environment which should contain one
#'              or multiple `R6` constructor objects that are subclasses of `R6_Base` from `utils_lang_r6_baseclass.R`. 
#'              The latter creates objects that provide public access to
#'              static methods and fields via the [get_static_env()] method. The latter
#'              returns an environment containing the former.\cr
#'              
#'              See `TODO`-comments (i.e., go over all matches of regular expression `\s*#'?\s*TODO`) for information on how and where to 
#'              adapt this template for your application. You should delete those comments afterwards in order to keep the documentation clean.\cr
#'              __IMPORTANT__: Only change this template according to those TODO comments in
#'                             order to ensure defined behavior. Consistency is NOT checked!
#'                             
#'              __NOTATION__: The user should use the following notation:
#'                            `R6_Base` subclassnames representing abstract
#'                            classes start with the "Abstract_" prefix. Abstract classes in our context
#'                            are classes that have at least one non-implemented (i.e. simply `stop()` calling)
#'                            method in the public environment.
#'                            Protected methods (i.e., methods that should only be called by child classes and not externally)
#'                            are denoted with a dot prefix.
#' @md              

#' TODO: Remove upper description, finish description below, remove its quotation marks and replace `<name>` with actual title
"#' @author  ...
#' @title  Collection of subclasses of `R6_Base` from `utils_lang_r6_baseclass.R` for ...
#' @description Contains a single [get_<name>_env()] method
#'   that returns an environment containing the `R6` constructor objects of these subclasses.
#' @md"

# Include guard
#' TODO: Put actual filename in uppercase here, format: `<FILE_NAME>_R`
if (!exists("UTILS_LANG_R6_SUBCLASS_TEMPLATE_R", inherits = FALSE)) {
  # TODO: Name variable as filename in uppercase in the same format as above
  UTILS_LANG_R6_SUBCLASS_TEMPLATE_R = TRUE
  
  #' TODO: Finish return description: Describe methods and/or fields returned in the `result_env` environment.
  #' TODO: Adapt method name to format `get_<name>_env`
  #' @return An environment containing one or multiple constructor objects for `R6` subclasses of 
  #'         `R6_Base` from `utils_lang_r6_baseclass.R`. These provide functionality for ...
  #' @export
  #'
  #' @examples
  #' @md
  get_utils_lang_r6_subclass_template_env <- function() {
    local_env <- environment() # This function's local environment
    print_warning_on_static_name_collision <- FALSE
    
    #' --------- Load required script dependencies -------
    #' TODO: __Optional__: Append paths to R scripts to `other_scripts`-vector below. Each script should 
    #'                     contain some thematically related methods and/or fields that shall be made
    #'                     statically available via the [get_static_env()] method of the classes added to
    #'                     `result_env` below.
    #'                     __IMPORTANT__: Each of those scripts should be structured according to the
    #'                                    `utils_TEMPLATE.R` file, 
    #'                                    i.e., have a single method 
    #'                                    `get_<name>_env()` (e.g., `get_utils_lang_package_env()` 
    #'                                    from `utils_lang_package.R`) that returns an environment containing
    #'                                    all its methods and/or fields.
    other_scripts <- c()   # e.g.: c(file.path(".", "utils", "utils_math_general.R"))
    if(length(other_scripts) > 0){
      invisible(lapply(other_scripts, source, local = local_env))
    }
    
    #' Create locked (and bindings-locked) environment holding the methods previously added to `other_scripts`.
    static_env <- new.env()
    #' TODO: Append `assign` calls for each of the scripts previously added to `other_scripts`.
    #'       Each call should be of the following pattern:
    #'         `assign("<name>", get_<name>_env(), envir = static_env)`
    #'       For examples, see below and the corresponding lines in `utils_lang_r6_baseclass.R`.
    #' __Example__: `assign("utils_math_general", get_utils_math_general_env(), envir = static_env)`, if
    #'              `file.path(".", "utils", "utils_math_general.R")` was added to `other_scripts` in the lines before.
    
    #' TODO: __Optional__: Add short-hand variants of the above:
    #' E.g.: assign("gen", static_env$utils_lang_general, envir = static_env)
    
    
    # --------- Load required package dependencies -------    
    #' TODO: Append other packages to `package_dependencies` which are to be installed (if necessary) and loaded
    package_dependencies <- c("R6") # e.g., c("R6","smoother","rowr","randomcoloR")
    
    invisible(source(file.path(".", "utils", "utils_lang_package.R"), local = TRUE))
    utils_lang_package <- get_utils_lang_package_env()
    if(!utils_lang_package$install_and_load(package_dependencies, stop_on_failure = TRUE)){
      stop("At least one required package could not be installed and/or loaded.")
    }
    
    # --------- Set fields required by all class instances -------
    
    #' The baseclass constructor object from `utils_lang_r6_baseclass.R`    
    invisible(source(file.path(".", "utils", "utils_lang_r6_baseclass.R"), local = TRUE))
    R6_Base <- get_utils_lang_r6_baseclass_env()$R6_Base
    
    #' Cleanup all but `local_env`, `static_env`, `print_warning_on_static_name_collision` and `R6_Base`
    rm(list=setdiff(ls(envir = local_env), 
                    c("static_env", "local_env", "print_warning_on_static_name_collision", "R6_Base")), envir = local_env)

    #' Stores whether `initialize()` has been called at least once for at least one of the R6_Base subclasses defined here
    .initialized_once <- FALSE
    
    #' Helper method to add elements from provided parent static environment to this static environment
    .add_to_static_env <- function(parent_static_env){
      if(!local_env$.initialized_once){ # We only need to add parent's static methods once
        parent_content <- ls(envir=parent_static_env)
        if(length(parent_content) > 0){
          for(elem in parent_content){
            not_exists <- !exists(elem, envir = local_env$static_env, inherits = FALSE)
            parent_static_env$err$warnifnot(paste("Name collision in static fields - keeping child value:", elem),
                                            !local_env$print_warning_on_static_name_collision || not_exists)
            if(not_exists) {
              assign(elem, get(elem, envir = parent_static_env, inherits = FALSE), envir = local_env$static_env)
            }
          }
        }
        lockEnvironment(local_env$static_env, bindings = TRUE)
        
        unlockBinding(".initialized_once", local_env)
        local_env$.initialized_once <- TRUE
        lockBinding(".initialized_once", local_env)
      }
    }
    
    #' Shorthand access to local and static environment
    l <- local_env
    s <- static_env
    
    #' TODO: __Optional__ -------- Create private static helper methods and/or fields here: -------

    
    # -------- Create environment to be returned containing 
    #          one or multiple topically related `R6_Base` subclasses-------
    #' Use `local_env` (or `l`) to access the enclosing environment and `static_env`
    #' (or `s` or `local_env$static_env` or `l$s`, ...) to access static methods and/or fields
    #' previously added via `other_scripts`. `R6_Base` (or `local_env$R6_Base`) contains 
    #' the baseclass constructor object from `utils_lang_r6_baseclass.R`.
    #' All `R6` classes here should be subclasses of this class.
    #'
    #' TODO: Add one or multiple `R6_Base` subclass constuctor objects to the `evalq` expression below.
    result_env <- new.env()
    evalq(
      {
        # TODO: Adapt both field and classname
        R6_Subclass <- R6Class("R6_Subclass",
                         # ---- R6 inheritance -----
                         #' TODO: __Optional__: Change to inherit from a different parent class.
                         #'                     __IMPORTANT__: The parent class MUST either directly or indirectly
                         #'                                    inherit from `R6_Base`!
                         inherit = R6_Base,
                         
                         #' TODO: ---- Adapt R6 options --------
                         portable = TRUE,
                         cloneable = TRUE,
                         lock_class = TRUE,
                         lock_objects = TRUE,
                         
                         #' TODO: ----- Add private Fields & methods ----
                         private = list(
                           
                         ),
                         #-----end of private-----
                         
                         #' TODO: ----- Add public fields & methods ----
                         #'       __Important__: Always make sure to copy the following methods
                         #'         into your subclass (and their subsequent subclasses) and adapt them only 
                         #'         according to the "TODO" comments.
                         public = list(
                           
                           #' See `R6_Base` in `utils_lang_r6_baseclass.R`
                           #' @export
                           #' @md
                           get_static_env = function(){
                             local_env$static_env
                           },

                           #' The `R6` `initialize` method.
                           #' 
                           #' Copies elements in parent's `static_env`  environment
                           #' (acquired via `super$get_static_env()`) to this
                           #' class' `static_env`. \cr
                           #' 
                           #' TODO: __IMPORTANT__: Add your own code below
                           #'   as indicated by the "TODO" comment.
                           #'   You also might need to adapt the `super$initialize()` 
                           #'   call to `super$initialize(...)`
                           #'   with `...` being the parameters expected by the parent's class' `initialize(...)` method.    
                           #'
                           #' @param assertions_status If `TRUE`, assertions are activated (see `set_assertion_status()`
                           #'   in `utils_lang_error.R`).
                           #'   TODO: Rename `assertions_status` to `.assertions_status`. 
                           #' 
                           #' @export
                           #'
                           #' @examples
                           #' @md
                           initialize = function(assertions_status = FALSE){
                             super$initialize()
                             local_env$.add_to_static_env(super$get_static_env())
                             local_env$static_env$err$set_assertions_status(assertions_status)
                             
                             #' TODO: Add your initialization code here here:
                             
                           },
                           
                            #' The `R6` `finalize` method. \cr
                           #' TODO: __IMPORTANT__: Call `super$finalize()` at the beginning, 
                           #' if you overwrite this method.
                           #'
                           #' @return NULL
                           #' @export
                           #'
                           #' @examples
                           #' @md
                           finalize = function() {
                             super$finalize()
                             
                             #' TODO: Add your finalization code here:
                           }
                           
                         ),
                         #-----end of public-----
                         
                         #' TODO: ----- Add active bindings ----
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