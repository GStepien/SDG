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
#' @title  Collection of subclasses of `Abstract_Iterator`s from `utils_iterators.R` for 
#'   iteration over __character__ values.
#' @description Contains a single [get_<name>_env()] method
#'   that returns an environment containing the `R6` constructor objects of these subclasses.
#' @md

# Include guard
if (!exists("CHARACTER_ITERATORS_R", inherits = FALSE)) {
  CHARACTER_ITERATORS_R = TRUE
  
  #' @return An environment containing one or multiple constructor objects for `R6` subclasses of 
  #'         `Abstract_Iterator` from `utils_iterators.R`. These provide functionality for iteration
  #'         over character values.
  #' @export
  #'
  #' @examples
  #' @md
  get_character_iterators_env <- function() {
    local_env <- environment() # This function's local environment
    print_warning_on_static_name_collision <- FALSE
    
    #' --------- Load required script dependencies -------
    #' __Optional__: Append paths to R scripts to `other_scripts`-vector below. Each script should 
    #'                     contain some thematically related methods and/or fields that shall be made
    #'                     statically available via the [get_static_env()] method of the classes added to
    #'                     `result_env` below.
    #'                     __IMPORTANT__: Each of those scripts should be structured according to the
    #'                                    `utils_TEMPLATE.R` file, 
    #'                                    i.e., have a single method 
    #'                                    `get_<name>_env()` (e.g., `get_utils_lang_package_env()` 
    #'                                    from `utils_lang_package.R`) that returns an environment containing
    #'                                    all its methods and/or fields.
    other_scripts <- c(file.path(".", "utils", "utils_iterators.R"), 
                       file.path(".","utils","utils_lang_error.R"), 
                       file.path(".","utils","utils_lang_typechecks.R"))   # e.g.: c(file.path(".", "utils", "utils_math_general.R"))
    if(length(other_scripts) > 0){
      invisible(lapply(other_scripts, source, local = local_env))
    }
    
    #' Create locked (and bindings-locked) environment holding the methods previously added to `other_scripts`.
    static_env <- new.env()
    #' Append `assign` calls for each of the scripts previously added to `other_scripts`.
    #'       Each call should be of the following pattern:
    #'         `assign("<name>", get_<name>_env(), envir = static_env)`
    #'       For examples, see below and the corresponding lines in `utils_lang_r6_baseclass.R`.
    #' __Example__: `assign("utils_math_general", get_utils_math_general_env(), envir = static_env)`, if
    #'              `file.path(".", "utils", "utils_math_general.R")` was added to `other_scripts` in the lines before.
    assign("utils_iterators", get_utils_iterators_env(), envir = static_env)
    assign("utils_lang_error", get_utils_lang_error_env(), envir = static_env)
    assign("utils_lang_typechecks", get_utils_lang_typechecks_env(), envir = static_env)
    
    #' __Optional__: Add short-hand variants of the above:
    #' E.g.: assign("gen", static_env$utils_lang_general, envir = static_env)
    assign("it", static_env$utils_iterators, envir = static_env)
    assign("err", static_env$utils_lang_error, envir = static_env)
    assign("tcs", static_env$utils_lang_typechecks, envir = static_env)
    
    # --------- Load required package dependencies -------    
    #' Append other packages to `package_dependencies` which are to be installed (if necessary) and loaded
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
    
    #' __Optional__ -------- Create private static helper methods and/or fields here: -------
    
    
    # -------- Create environment to be returned containing 
    #          one or multiple topically related `R6_Base` subclasses-------
    #' Use `local_env` (or `l`) to access the enclosing environment and `static_env`
    #' (or `s` or `local_env$static_env` or `l$s`, ...) to access static methods and/or fields
    #' previously added via `other_scripts`. `R6_Base` (or `local_env$R6_Base`) contains 
    #' the baseclass constructor object from `utils_lang_r6_baseclass.R`.
    #' All `R6` classes here should be subclasses of this class.
    #'
    result_env <- new.env()
    evalq(
      {
        #' A subclass of `Queue_Iterator` that only accepts character vectors of length one.
        Character_Queue_Iterator <- R6Class("Character_Queue_Iterator",
                                            # ---- R6 inheritance -----
                                            inherit = l$s$it$Queue_Iterator,
                                            
                                            #' ---- Adapt R6 options --------
                                            portable = TRUE,
                                            cloneable = TRUE,
                                            lock_class = TRUE,
                                            lock_objects = TRUE,
                                            
                                            #' ----- Add private Fields & methods ----
                                            private = list(
                                              
                                              check_if_single_atmoic_character = function(el){
                                                return(l$s$tcs$has_length_1(el, NA_on_fail = FALSE) &&
                                                         is.atomic(el) &&
                                                         l$s$tcs$is_character(el,
                                                                              accept_NULL = FALSE,
                                                                              accept_NaN = FALSE,
                                                                              accept_NA = FALSE))
                                              }
                                              
                                            ),
                                            #-----end of private-----
                                            
                                            #' ----- Add public fields & methods ----
                                            public = list(
                                              
                                              #' See `R6_Base` in `utils_lang_r6_baseclass.R`
                                              #' MUST always be copied over.
                                              #' @md
                                              get_static_env = function(){
                                                local_env$static_env
                                              },
                                              
                                              
                                              #' See documentation in `Abstract_Iterator`.
                                              #' @export
                                              #' @md
                                              .check_consistency = function(finished = self$.finished(), 
                                                                            next_count = self$.get_next_count()){
                                                super$.check_consistency(finished = finished,
                                                                         next_count = next_count)
                                                #' Add additional assertions for consistency checks, if required and call
                                                #'   this method at appropriate points.
                                              },
                                              
                                              #' A subclass of `Queue_Iterator` that only accepts character vectors of length one.
                                              #' I.e., each of the elements in the data structure provided to `append_elements`
                                              #' must be a character vector of length one.
                                              #' 
                                              #' __IMPORTANT__: See documentation of this method in `utils_lang_r6_subclass_TEMPLATE.R`.
                                              #'
                                              #' @param assertions_status See `Abstract_Iterator`.
                                              #' @param elements See `Queue_Iterator`.
                                              #' @param lock See `Queue_Iterator`.
                                              #'   
                                              #' @export
                                              #'
                                              #' @examples
                                              #' @md
                                              initialize = function(assertions_status = FALSE,
                                                                    elements = list(),
                                                                    lengths = 1,
                                                                    lock = FALSE){
                                                super$initialize(assertions_status = assertions_status,
                                                                 elements = elements,
                                                                 lengths = lengths,
                                                                 lock = lock)
                                                local_env$.add_to_static_env(super$get_static_env())
                                                local_env$static_env$err$set_assertions_status(assertions_status)
                                                
                                                #' Add your initialization code here here:
                                                
                                              },
                                              
                                              #' See documentation in `Abstract_Iterator`.
                                              #' @export
                                              #' @md
                                              finalize = function() {
                                                super$finalize()
                                              },
                                              
                                              .validate_next = function(element) {
                                                return(private$check_if_single_atmoic_character(element))
                                              },
                                              
                                              append_elements = function(elements, lengths = 1){
                                                if(l$s$err$get_assertions_status()){
                                                  elements <- as.list(elements)
                                                  for(el in elements){
                                                    l$s$err$assert_msg("'elements' must only contain atomic character vectors of length one.",
                                                                       private$check_if_single_atmoic_character(el))
                                                  }
                                                }
                                                super$append_elements(elements = elements, lengths = lengths)
                                              },
                                              
                                              #' ----------- THE FOLLOWING METHODS __MAY__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                              
                                              .get_next_post = function(){
                                                super$.get_next_post()
                                              }
                                              
                                              #' ----------- THE FOLLOWING METHODS __MUST__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                              
                                            ),
                                            #-----end of public-----
                                            
                                            #' ----- Add active bindings ----
                                            active = list(
                                              
                                            )
                                            #-----end of active-----
        )
        
        #' A `Concatenated_Iterator` subclass for internal iterators
        #' returning character values.
        Character_Concatenated_Iterator <- R6Class("Character_Concatenated_Iterator",
                                                   # ---- R6 inheritance -----
                                                   inherit = l$s$it$Concatenated_Iterator,
                                                   
                                                   #' ---- Adapt R6 options --------
                                                   portable = TRUE,
                                                   cloneable = TRUE,
                                                   lock_class = TRUE,
                                                   lock_objects = TRUE,
                                                   
                                                   #' ----- Add private Fields & methods ----
                                                   private = list(
                                                   ),
                                                   #-----end of private-----
                                                   
                                                   #' ----- Add public fields & methods ----
                                                   public = list(
                                                     
                                                     #' See `R6_Base` in `utils_lang_r6_baseclass.R`
                                                     #' MUST always be copied over.
                                                     #' @export
                                                     #' @md
                                                     get_static_env = function(){
                                                       local_env$static_env
                                                     },
                                                     
                                                     #' See documentation in `Abstract_Iterator`.
                                                     #' 
                                                     #' @export
                                                     #' @md
                                                     .check_consistency = function(finished = self$.finished(), 
                                                                                   next_count = self$.get_next_count()){
                                                       
                                                       super$.check_consistency(finished = finished,
                                                                                next_count = next_count)
                                                       #' Add additional assertions for consistency checks, if required and call
                                                       #'   this method at appropriate points.
                                                       
                                                     },
                                                     
                                                     #' A `Concatenated_Iterator` subclass for internal iterators
                                                     #' returning character values.
                                                     #' See documentation of `Concatenated_Iterator` from `utils_iterators.R` for more information.
                                                     #'
                                                     #' __IMPORTANT__: See documentation of this method in `utils_lang_r6_subclass_TEMPLATE.R`.
                                                     #'   
                                                     #' @export
                                                     #'
                                                     #' @examples
                                                     #' @md
                                                     initialize = function(assertions_status = FALSE, 
                                                                           iterators = list(), 
                                                                           lock = FALSE){
                                                       super$initialize(assertions_status = assertions_status,
                                                                        iterators = iterators,
                                                                        lock = lock)
                                                       
                                                       local_env$.add_to_static_env(super$get_static_env())
                                                       local_env$static_env$err$set_assertions_status(assertions_status)
                                                       
                                                       #' Add your initialization code here here:
                                                       
                                                     },
                                                     
                                                     finalize = function() {
                                                       super$finalize()
                                                     },
                                                     
                                                     .validate_next = function(element) {
                                                       #' Add additional validation, if required
                                                       #' Note that, at this point, each element is already validated 
                                                       #' by the respective iterator that returned it.
                                                       result <- super$.validate_next(element)
                                                       
                                                       return(result && 
                                                                l$s$tcs$has_length_1(element, NA_on_fail = FALSE) &&
                                                                l$s$tcs$is_character(element,
                                                                                     accept_NULL = FALSE,
                                                                                     accept_NaN = FALSE,
                                                                                     accept_NA = FALSE))
                                                     }
                                                     
                                                     #' ----------- THE FOLLOWING METHODS __MAY__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                                     
                                                     
                                                   ),
                                                   #-----end of public-----
                                                   
                                                   #' ----- Add active bindings ----
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