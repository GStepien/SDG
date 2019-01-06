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
#'   iteration over __numeric__ values.
#' @description Contains a single [get_<name>_env()] method
#'   that returns an environment containing the `R6` constructor objects of these subclasses.
#' @md

# Include guard
if (!exists("NUMERIC_ITERATORS_R", inherits = FALSE)) {
  NUMERIC_ITERATORS_R = TRUE
  
  #' @return An environment containing one or multiple constructor objects for `R6` subclasses of 
  #'         `Abstract_Iterator` from `utils_iterators.R`. These provide functionality for iteration
  #'         over numeric values.
  #' @export
  #'
  #' @examples
  #' @md
  get_numeric_iterators_env <- function() {
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
        #' An abstract `Abstract_Iterator` subclass for iteration over one dimensional,
        #' __numeric__ data elements.
        Abstract_Numeric_Iterator <- R6Class("Abstract_Numeric_Iterator",
                                             # ---- R6 inheritance -----
                                             inherit = l$s$it$Abstract_Iterator,
                                             
                                             #' ---- Adapt R6 options --------
                                             portable = TRUE,
                                             cloneable = TRUE,
                                             lock_class = TRUE,
                                             lock_objects = TRUE,
                                             
                                             #' ----- Add private Fields & methods ----
                                             private = list(
                                               accept_NA = NULL,
                                               accept_NaN = NULL,
                                               accept_neg_inf = NULL,
                                               accept_pos_inf = NULL
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
                                                 #' TODO: Add additional assertions for consistency checks, if required and call
                                                 #'   this method at appropriate points.
                                               },
                                               
                                               #' An abstract subclass of `Abstract_Iterator` for iteration over atomic numeric elements
                                               #' of length one. In essence, this class makes sure that `.validate_next()`
                                               #' returns only `TRUE` for numeric elements of length one that are also consistent
                                               #' with the other parameters provided here.
                                               #' 
                                               #' __IMPORTANT__: See documentation of this method in `utils_lang_r6_subclass_TEMPLATE.R`.
                                               #'
                                               #' @param assertions_status If `TRUE`, assertions are activated (see `set_assertion_status()`
                                               #'   in `utils_lang_error.R`).
                                               #'   
                                               #' @param fixed_length See documentation in `Abstract_Iterator`. 
                                               #' @param accept_NA `.validate_next()` shall only accept `NA` elements if and only if this
                                               #'   parameter is set to `TRUE`.
                                               #' @param accept_NaN `.validate_next()` shall only accept `NaN` elements if and only if this
                                               #'   parameter is set to `TRUE`.
                                               #' @param accept_neg_inf `.validate_next()` shall only accept `-Inf` elements if and only if this
                                               #'   parameter is set to `TRUE`.
                                               #' @param accept_pos_inf `.validate_next()` shall only accept `Inf` elements if and only if this
                                               #'   parameter is set to `TRUE`.
                                               #'
                                               #' @export
                                               #'
                                               #' @examples
                                               #' @md
                                               initialize = function(assertions_status = FALSE, 
                                                                     fixed_length,
                                                                     accept_NA = FALSE,
                                                                     accept_NaN = FALSE,
                                                                     accept_neg_inf = FALSE,
                                                                     accept_pos_inf = FALSE){
                                                 super$initialize(assertions_status = assertions_status, fixed_length = fixed_length)
                                                 local_env$.add_to_static_env(super$get_static_env())
                                                 local_env$static_env$err$set_assertions_status(assertions_status)
                                                 
                                                 #' Add your initialization code here here:
                                                 l$s$err$assert_msg(paste0("'accept_NA', 'accept_NaN', 'accept_neg_inf' and 'accept_pos_inf' ",
                                                                           "must be logicals of length one."),
                                                                    l$s$tcs$has_length_1(accept_NA, NA_on_fail = FALSE),
                                                                    l$s$tcs$has_length_1(accept_NaN, NA_on_fail = FALSE),
                                                                    l$s$tcs$has_length_1(accept_neg_inf, NA_on_fail = FALSE),
                                                                    l$s$tcs$has_length_1(accept_pos_inf, NA_on_fail = FALSE),
                                                                    l$s$tcs$is_logical(accept_NA,
                                                                                       accept_NULL = FALSE,
                                                                                       accept_NaN = FALSE,
                                                                                       accept_NA = FALSE),
                                                                    l$s$tcs$is_logical(accept_NaN,
                                                                                       accept_NULL = FALSE,
                                                                                       accept_NaN = FALSE,
                                                                                       accept_NA = FALSE),
                                                                    l$s$tcs$is_logical(accept_neg_inf,
                                                                                       accept_NULL = FALSE,
                                                                                       accept_NaN = FALSE,
                                                                                       accept_NA = FALSE),
                                                                    l$s$tcs$is_logical(accept_pos_inf,
                                                                                       accept_NULL = FALSE,
                                                                                       accept_NaN = FALSE,
                                                                                       accept_NA = FALSE))
                                                 
                                                 private$accept_NA <- accept_NA
                                                 private$accept_NaN <- accept_NaN
                                                 private$accept_neg_inf <- accept_neg_inf
                                                 private$accept_pos_inf <- accept_pos_inf
                                                 
                                               },
                                               
                                               #' See documentation in `Abstract_Iterator`.
                                               #' @export
                                               #' @md
                                               finalize = function() {
                                                 super$finalize()
                                               },
                                               
                                               #' Override this method in order to empose some restrictions on the elements 
                                               #' returned by [.get_next(num)] (length, class, dim, etc.). Note that validation
                                               #' is only performed if assertions are activated (see `l$s$err$get_ and set_assertions_status()`)
                                               #' 
                                               #' Do not forget to include `super$.validate_next(element)` in your result if that is applicable.
                                               #' 
                                               #' @param element The element to be tested.
                                               #'
                                               #' @return `TRUE` if and only if the provided element shall be considered valid in
                                               #'   [get_next(num)]. Note that a `FALSE` always produces an error in the latter method.
                                               #' @export
                                               #'
                                               #' @examples
                                               #' @md
                                               .validate_next = function(element) {
                                                 return(l$s$tcs$has_length_1(element, NA_on_fail = FALSE) &&
                                                          is.atomic(element) &&
                                                          l$s$tcs$is_numeric(element,
                                                                             accept_NULL = FALSE,
                                                                             accept_NaN = private$accept_NaN,
                                                                             accept_NA = private$accept_NA,
                                                                             lower_bound = -Inf,
                                                                             lower_bound_inclusive = private$accept_neg_inf,
                                                                             upper_bound = Inf,
                                                                             upper_bound_inclusive = private$accept_pos_inf,
                                                                             accept_non_integer = TRUE))
                                               },
                                               
                                               #' TODO: ----------- THE FOLLOWING METHODS __MAY__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                               
                                               #' Method is called right before [get_next()] returns.
                                               #' May be overridden by subclasses in order to perform additional
                                               #' clean-up operations at the end of a retrieval operation (i.e., call to [get_next()]).
                                               #' 
                                               #' Call `super$.get_next_post()` in the beginning, if you override this method.
                                               #' 
                                               #' This dummy-implementation is empty.
                                               #'
                                               #' @export
                                               #'
                                               #' @examples
                                               #' @md
                                               .get_next_post = function(){
                                                 super$.get_next_post()
                                               },
                                               
                                               
                                               #' TODO: ----------- THE FOLLOWING METHODS __MUST__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                               
                                               #' Called by [get_num(num)] in order to retrieve the next `num` elements.
                                               #' At that point, if assertions are activated (see `s$l$err$get_ and set_assertions_status()`),
                                               #' `num` is guaranteed not to be bigger than the value
                                               #' returned by [get_next_count()].
                                               #'
                                               #' @param num The number of elements to retrieve.
                                               #'
                                               #' @return A list of `num` elements.
                                               #' @export
                                               #'
                                               #' @examples
                                               #' @md
                                               .get_next = function(num) {
                                                 stop("Attempt to call an 'abstract' method. 
                                                      You need to extend this class and explicitly 
                                                      overwrite this method.")
                                               },
                                               
                                               #' Get the number of currently retrievable elements.
                                               #'
                                               #' @return The number of currently retrievalbe elements (the `num`
                                               #' argument in [get_next(num)] should never be bigger than this value).
                                               #' Note that a return value of zero does not necessarily mean that
                                               #' there won't be retrievable elements in the future - see [finished()].
                                               #' 
                                               #' For example, if all remaining elements are available, this 
                                               #' method might simply return `self$get_num_remaining()` 
                                               #' @export
                                               #'
                                               #' @examples
                                               #' @md
                                               .get_next_count = function() {
                                                 stop("Attempt to call an 'abstract' method. 
                                                      You need to extend this class and explicitly 
                                                      overwrite this method.")
                                               },
                                               
                                               #' @return `TRUE` if and only if this iterator has reached its end and there
                                               #' won't be any more retrievable elements (thus, [get_next_count()] will __always__ return zero
                                               #' from now on). Note that this should never return `TRUE` if [get_length()] is `+Inf`.
                                               #' 
                                               #' @export
                                               #'
                                               #' @examples
                                               #' @md
                                               .finished = function() {
                                                 stop("Attempt to call an 'abstract' method. 
                                                      You need to extend this class and explicitly 
                                                      overwrite this method.")
                                               }
                                               
                                                 ),
                                             #-----end of public-----
                                             
                                             #' ----- Add active bindings ----
                                             active = list(
                                               
                                             )
                                             #-----end of active-----
        )
        
        #' A `Abstract_Numeric_Iterator` subclass for generating numeric values representing a
        #' random walk.
        Random_Walk_Numeric_Iterator <- R6Class("Random_Walk_Numeric_Iterator",
                                     # ---- R6 inheritance -----
                                     inherit = result_env$Abstract_Numeric_Iterator,
                                     
                                     #' ---- Adapt R6 options --------
                                     portable = TRUE,
                                     cloneable = TRUE,
                                     lock_class = TRUE,
                                     lock_objects = TRUE,
                                     
                                     #' ----- Add private Fields & methods ----
                                     private = list(
                                       stddev_is_neg_step = NULL,
                                       mean_step = NULL,
                                       stddev_step = NULL,
                                       target_value = NULL,
                                       last_value = NULL,
                                       seed = NULL,
                                       last_rnd_state = NULL,
                                       initial_value = NULL
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
                                       
                                       #' An `Abstract_Numeric_Iterator` subclass for generating numeric values representing a
                                       #' random walk.
                                       #' Let `s(n)` be the `n`-th value generated by this iterator. Define `s(0) = initial_value`.
                                       #' Given `s(n-1)`, the value `s(n)` is generated as follows:
                                       #' 
                                       #' * Let `p_negative = pnorm(s(n-1), mean = target_value, sd = stddev_is_neg_step)` be the value of the 
                                       #'   cumulative normal distribution function with the denoted mean and standard deviation.
                                       #' * Let `delta(n) = rnorm(1, mean = mean_step, sd = stddev_step)` be the step size
                                       #'   drawn from a normal distribution with the denoted mean and standard deviation.
                                       #' * Draw `sign` from `{0,1}`, where `1` is drawn with probability `p_negative`
                                       #'   and `0` is drawn with probability `1 - p_negative`.
                                       #' * Then `s(n) = s(n - 1) + (-1)^sign times delta(n)`.
                                       #' 
                                       #' __Note__: You may set `stddev_is_neg_step` to infinity. In that case, `p_negative` 
                                       #'   will always be `0.5`. You may also set `stddev_step` to zero, resulting in a
                                       #'   constant stepsize of `mean_step`. Both these measures result in a "classical" random walk.
                                       #' 
                                       #' __IMPORTANT__: See documentation of this method in `utils_lang_r6_subclass_TEMPLATE.R`.
                                       #'
                                       #' @param assertions_status If `TRUE`, assertions are activated (see `set_assertion_status()`
                                       #'   in `utils_lang_error.R`).
                                       #'   
                                       #' @param fixed_length Here, must be a finite, non-negative, non-`NA` integer. 
                                       #'   Also see documentation in `Abstract_Iterator`. 
                                       #'   
                                       #' @param stddev_is_neg_step A non-negative, possibly infinite numeric of length one.
                                       #'   Characterized the "propensity" of the random walk to return to `offset`.
                                       #'   
                                       #'   Empirically, most elements will lie within a radius between `stddev_is_neg_step / 4` and `stddev_is_neg_step`
                                       #'   around `target_value`, given the stepsizes do not get too big (or too small) and `initial_value`
                                       #'   is close to `target_value`.
                                       #'   
                                       #'   See description above for more details.
                                       #'   
                                       #' @param mean_step A finite numeric of length one.
                                       #'   See description above for more details. Given that `stddev_step` is small and
                                       #'   `stddev_is_neg_step` is infinite, then the range of the random walk (i.e., the distance
                                       #'   between the maximal and the minimal value) is, empirically, roughly proportional to `sqrt(fixed_length)`.
                                       #' @param stddev_step A finite, non-negative numeric of length one.
                                       #'   See description above for more details.
                                       #' @param initial_value A finite numeric of length one.
                                       #'   See description above for more details.
                                       #' @param target_value A finite numeric of length one.
                                       #'   See description above for more details.
                                       #' @param seed `NULL` or an integer. If non-`NULL`,
                                       #'   the iterators values are generated with a 
                                       #'   random generator seed initially set to `seed` via `set.seed()`. May be used
                                       #'   to create reproducable random walks. Note that this instance keeps track of
                                       #'   its own random generator state and resets `.Random.seed` to:
                                       #'   * At the beginning of `get_next()` execution: Its value and the end of the last `get_next()` execution
                                       #'   or calls `set.seed(seed)` if this is the first one.
                                       #'   * At the end of each `get_next()` execution: Its original value before the upper procerure.
                                       #'   If `seed` is `NULL`, the argument is ignored.
                                       #'
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       initialize = function(assertions_status = FALSE, 
                                                             fixed_length,
                                                             stddev_is_neg_step = 10,
                                                             mean_step = 1,
                                                             stddev_step = 0.2,
                                                             initial_value = 0,
                                                             target_value = initial_value,
                                                             seed = NULL){
                                         
                                         super$initialize(assertions_status = assertions_status, 
                                                          fixed_length = fixed_length,
                                                          accept_NA = FALSE,
                                                          accept_NaN = FALSE,
                                                          accept_neg_inf = FALSE,
                                                          accept_pos_inf = FALSE)
                                         
                                         local_env$.add_to_static_env(super$get_static_env())
                                         local_env$static_env$err$set_assertions_status(assertions_status)
                                         
                                         #' Add your initialization code here here:
                                         l$s$err$assert_msg("'fixed_length' must be a finite, non-negative integer of length one.",
                                                            l$s$tcs$has_length_1(fixed_length, NA_on_fail = FALSE),
                                                            l$s$tcs$is_integer(fixed_length,
                                                                               accept_NULL = FALSE,
                                                                               accept_NaN = FALSE,
                                                                               accept_NA = FALSE,
                                                                               lower_bound = 0,
                                                                               lower_bound_inclusive = TRUE,
                                                                               upper_bound = Inf,
                                                                               upper_bound_inclusive = FALSE))
                                         
                                         l$s$err$assert_msg("'seed' must be NULL or a finite integer of length one.",
                                                            l$s$mg$`%then%`(!is.null(seed), 
                                                                            l$s$tcs$has_length_1(seed, NA_on_fail = FALSE)),
                                                            l$s$mg$`%then%`(!is.null(seed), 
                                                                            l$s$tcs$is_integer(seed,
                                                                                               accept_NULL = FALSE,
                                                                                               accept_NaN = FALSE,
                                                                                               accept_NA = FALSE,
                                                                                               lower_bound = -Inf,
                                                                                               lower_bound_inclusive = FALSE,
                                                                                               upper_bound = Inf,
                                                                                               upper_bound_inclusive = FALSE)))
                                         
                                         l$s$err$assert_msg("'stddev_is_neg_step' must be a non-negative numeric of length one.",
                                                            l$s$tcs$has_length_1(stddev_is_neg_step, NA_on_fail = FALSE),
                                                            l$s$tcs$is_numeric(stddev_is_neg_step,
                                                                               accept_NULL = FALSE,
                                                                               accept_NaN = FALSE,
                                                                               accept_NA = FALSE,
                                                                               lower_bound = 0,
                                                                               lower_bound_inclusive = TRUE,
                                                                               upper_bound = Inf,
                                                                               upper_bound_inclusive = TRUE,
                                                                               accept_non_integer = TRUE))
                                         
                                         l$s$err$assert_msg("'mean_step' must be a finite numeric of length one.",
                                                            l$s$tcs$has_length_1(mean_step, NA_on_fail = FALSE),
                                                            l$s$tcs$is_numeric(mean_step,
                                                                               accept_NULL = FALSE,
                                                                               accept_NaN = FALSE,
                                                                               accept_NA = FALSE,
                                                                               lower_bound = -Inf,
                                                                               lower_bound_inclusive = FALSE,
                                                                               upper_bound = Inf,
                                                                               upper_bound_inclusive = FALSE,
                                                                               accept_non_integer = TRUE))
                                         
                                         l$s$err$assert_msg("'stddev_step' must be a finite, non-negative numeric of length one.",
                                                            l$s$tcs$has_length_1(stddev_step, NA_on_fail = FALSE),
                                                            l$s$tcs$is_numeric(stddev_step,
                                                                               accept_NULL = FALSE,
                                                                               accept_NaN = FALSE,
                                                                               accept_NA = FALSE,
                                                                               lower_bound = 0,
                                                                               lower_bound_inclusive = TRUE,
                                                                               upper_bound = Inf,
                                                                               upper_bound_inclusive = FALSE,
                                                                               accept_non_integer = TRUE))
                                         
                                         l$s$err$assert_msg("'initial_value' must be a finite numeric of length one.",
                                                            l$s$tcs$has_length_1(initial_value, NA_on_fail = FALSE),
                                                            l$s$tcs$is_numeric(initial_value,
                                                                               accept_NULL = FALSE,
                                                                               accept_NaN = FALSE,
                                                                               accept_NA = FALSE,
                                                                               lower_bound = -Inf,
                                                                               lower_bound_inclusive = FALSE,
                                                                               upper_bound = Inf,
                                                                               upper_bound_inclusive = FALSE,
                                                                               accept_non_integer = TRUE))
                                         
                                         l$s$err$assert_msg("'target_value' must be a finite numeric of length one.",
                                                            l$s$tcs$has_length_1(target_value, NA_on_fail = FALSE),
                                                            l$s$tcs$is_numeric(target_value,
                                                                               accept_NULL = FALSE,
                                                                               accept_NaN = FALSE,
                                                                               accept_NA = FALSE,
                                                                               lower_bound = -Inf,
                                                                               lower_bound_inclusive = FALSE,
                                                                               upper_bound = Inf,
                                                                               upper_bound_inclusive = FALSE,
                                                                               accept_non_integer = TRUE))
                                         
                                         private$stddev_is_neg_step <- stddev_is_neg_step
                                         private$mean_step <- mean_step
                                         private$stddev_step <- stddev_step
                                         private$target_value <- target_value
                                         private$last_value <- initial_value
                                         private$initial_value <- initial_value
                                         private$seed <- seed
                                       },
                                       
                                       #' @return The `initial_value` provided to `initialize`.
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       get_initial_value = function(){
                                         return(private$initial_value)
                                       },
                                       
                                       #' @return The `target_value` provided to `initialize`.
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       get_target_value = function(){
                                         return(private$target_value)
                                       },
                                       
                                       #' See documentation in `Abstract_Iterator`.
                                       #' @export
                                       #' @md
                                       finalize = function() {
                                         super$finalize()
                                       },
                                       
                                       #' Override this method in order to empose some restrictions on the elements 
                                       #' returned by [.get_next(num)] (length, class, dim, etc.). Note that validation
                                       #' is only performed if assertions are activated (see `l$s$err$get_ and set_assertions_status()`)
                                       #' 
                                       #' Do not forget to include `super$.validate_next(element)` in your result if that is applicable.
                                       #' 
                                       #' @param element The element to be tested.
                                       #'
                                       #' @return `TRUE` if and only if the provided element shall be considered valid in
                                       #'   [get_next(num)]. Note that a `FALSE` always produces an error in the latter method.
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       .validate_next = function(element) {
                                         result <- super$.validate_next(element)
                                         
                                         return(result)
                                       },
                                       
                                       #' ----------- THE FOLLOWING METHODS __MAY__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                       
                                       #' Method is called right before [get_next()] returns.
                                       #' May be overridden by subclasses in order to perform additional
                                       #' clean-up operations at the end of a retrieval operation (i.e., call to [get_next()]).
                                       #' 
                                       #' Call `super$.get_next_post()` in the beginning, if you override this method.
                                       #' 
                                       #' This dummy-implementation is empty.
                                       #'
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       .get_next_post = function(){
                                         super$.get_next_post()
                                       },
                                       
                                       .get_next = function(num) {
                                         if(num == 0){
                                           return(list())
                                         }
                                         
                                         if(!is.null(private$seed)){
                                           glob_env <- globalenv()
                                           original_rnd_state <- get(x = ".Random.seed", envir = glob_env)
                                           
                                           if(is.null(private$last_rnd_state)){
                                             set.seed(private$seed)
                                           }
                                           else{
                                             assign(x = ".Random.seed", value = private$last_rnd_state, envir = glob_env)
                                           }
                                         }
                                        
                                         result <- vector(mode="list", length = num)
                                         for(i in 1:num){
                                           neg_prob_f <- pnorm(private$last_value, 
                                                               mean = private$target_value, 
                                                               sd = private$stddev_is_neg_step)
                                           
                                           direction <- (-1)^sample(c(0,1), 
                                                                    size=1, 
                                                                    prob=c(1-neg_prob_f, neg_prob_f))
                                           
                                           stepsize <- rnorm(1, mean = private$mean_step, sd = private$stddev_step)
                                           
                                           result[[i]] <- private$last_value + direction * stepsize
                                           private$last_value <- result[[i]]
                                         }
                                         
                                         if(!is.null(private$seed)){
                                           # Store current rnd generator state for next call to get_next
                                           private$last_rnd_state <- get(x = ".Random.seed", envir = glob_env)
                                           assign(x = ".Random.seed", value = original_rnd_state, envir = glob_env)
                                         }
                                         
                                         return(result)
                                       },
                                       
                                       .get_next_count = function() {
                                         return(self$get_num_remaining())
                                       },
                                       
                                       .finished = function() {
                                         return(identical(self$get_num_remaining(), 0))
                                       }
                                       
                                       #' ----------- THE FOLLOWING METHODS __MUST__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                       
                                       
                                     ),
                                     #-----end of public-----
                                     
                                     #' ----- Add active bindings ----
                                     active = list(
                                       
                                     )
                                     #-----end of active-----
        )
        
        #' A `Random_Walk_Numeric_Iterator` subclass for a random walk that roughly stays within a certain range
        #' around the offset.
        Range_Random_Walk_Numeric_Iterator <- R6Class("Range_Random_Walk_Numeric_Iterator",
                                                # ---- R6 inheritance -----
                                                inherit = result_env$Random_Walk_Numeric_Iterator,
                                                
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
                                                  #' @md
                                                  get_static_env = function(){
                                                    local_env$static_env
                                                  },
                                                  
                                                  
                                                  #' TODO: DEPRECATED: Shape not length invariant.
                                                  #' 
                                                  #' A `Random_Walk_Numeric_Iterator` subclass for a random walk that roughly stays within a certain range
                                                  #' around the target offset.
                                                  #' 
                                                  #' The returned random walk iterator behaves equivalently to a `Random_Walk_Numeric_Iterator`
                                                  #' where the `stddev_is_neg_step`, `mean_step` and `stddev_step` parameters have been initialized as:
                                                  #' * `stddev_is_neg_step = range`
                                                  #' * `mean_step = range / (fixed_length^0.5 times smoothness)`
                                                  #' * `stddev_step = stddev_step_factor times mean_step`
                                                  #' 
                                                  #' These initializations are besed on the following two empirical observations in `Random_Walk_Numeric_Iterator`s:
                                                  #' 1. Most elements will lie in a radius between `stddev_is_neg_step / 4` and `stddev_is_neg_step` 
                                                  #'    around `target_value`, given the stepsizes do not get too big (or too small) and `initial_value`
                                                  #'    is close to `target_value`.
                                                  #' 2. Given that `stddev_step` is small and
                                                  #'    `stddev_is_neg_step` is infinite, then the range of the random walk (i.e., the distance
                                                  #'    between the maximal and the minimal value) is, empirically, roughly proportional to `sqrt(fixed_length)`.
                                                  #' 3. In most cases, it makes more sense to let the standard deviation of the step size (i.e. `stddev_step`)
                                                  #'    be proportional to the actual mean stepsize.
                                                  #' 
                                                  #' The `smoothness` parameter represents the proportionality constant of the relation described in point 2.
                                                  #' A high value results in smaller step sizes and "smoother" shape of the resulting walk with a smaller range. 
                                                  #' A small `smoothness` value results in bigger step sizes and a more "noisy" shape of the resulting walk.
                                                  #' 
                                                  #' Empirically, `smoothness` values around `0.5` to `1.5` produce nice results, where the
                                                  #' random walk is not too noisy but also does not deviate too much from `offset` and
                                                  #' roughly fulfills the remarks about the radius in point 1. This is true if
                                                  #' `stddev_step_factor` lies between `0.2` and `1`.
                                                  #' 
                                                  #' __IMPORTANT__: See documentation of this method in `utils_lang_r6_subclass_TEMPLATE.R`.
                                                  #'
                                                  #' @param assertions_status If `TRUE`, assertions are activated (see `set_assertion_status()`
                                                  #'   in `utils_lang_error.R`).
                                                  #'   
                                                  #' @param fixed_length Here, must be a finite, non-negative, non-`NA` integer. 
                                                  #'   Also see documentation in `Abstract_Iterator`. 
                                                  #'
                                                  #' @param range A non-negative, possibly infinite numeric of length one.
                                                  #'   See description above for more details.
                                                  #' @param smoothness A positive, non-zero , possibly infinite numeric of length one.
                                                  #'   See description above for more details.
                                                  #' @param stddev_step_factor A positive, finite numeric of length one.
                                                  #' The step size standard deviation `stddev_step` 
                                                  #'   from `Random_Walk_Numeric_Iterator` is set to `stddev_step_factor * mean_step`.
                                                  #' @param initial_value See `Random_Walk_Numeric_Iterator`.
                                                  #' @param target_value See `Random_Walk_Numeric_Iterator`.
                                                  #' @param seed See `Random_Walk_Numeric_Iterator`.
                                                  #' 
                                                  #' @export
                                                  #'
                                                  #' @examples
                                                  #' @md
                                                  initialize = function(assertions_status = FALSE, 
                                                                        fixed_length,
                                                                        range,
                                                                        smoothness=1,
                                                                        stddev_step_factor = 0.6,
                                                                        initial_value = 0,
                                                                        target_value = initial_value,
                                                                        seed = NULL){
                                                    
                                                    warning(paste0("DEPRECATED class: Shape not length invariant. ",
                                                                   "Use Random_Walk_Numeric_Iterator instead."), immediate. = TRUE)
                                                    
                                                    super$initialize(assertions_status = assertions_status, 
                                                                     fixed_length,
                                                                     stddev_is_neg_step = (function(){
                                                                       l$s$err$assert_msg("'range' must be a non-negative numeric of length one.",
                                                                                          l$s$tcs$has_length_1(range, NA_on_fail = FALSE),
                                                                                          l$s$tcs$is_numeric(range,
                                                                                                             accept_NULL = FALSE,
                                                                                                             accept_NaN = FALSE,
                                                                                                             accept_NA = FALSE,
                                                                                                             lower_bound = 0,
                                                                                                             lower_bound_inclusive = TRUE,
                                                                                                             upper_bound = Inf,
                                                                                                             upper_bound_inclusive = TRUE,
                                                                                                             accept_non_integer = TRUE))
                                                                       l$s$err$assert_msg("'smoothness' must be a positive, non-zero numeric of length one.",
                                                                                          l$s$tcs$has_length_1(smoothness, NA_on_fail = FALSE),
                                                                                          l$s$tcs$is_numeric(smoothness,
                                                                                                             accept_NULL = FALSE,
                                                                                                             accept_NaN = FALSE,
                                                                                                             accept_NA = FALSE,
                                                                                                             lower_bound = 0,
                                                                                                             lower_bound_inclusive = FALSE,
                                                                                                             upper_bound = Inf,
                                                                                                             upper_bound_inclusive = TRUE,
                                                                                                             accept_non_integer = TRUE))
                                                                       l$s$err$assert_msg("'stddev_step_factor' must be a finite, non-negative numeric of length one.",
                                                                                          l$s$tcs$has_length_1(stddev_step_factor, NA_on_fail = FALSE),
                                                                                          l$s$tcs$is_numeric(stddev_step_factor,
                                                                                                             accept_NULL = FALSE,
                                                                                                             accept_NaN = FALSE,
                                                                                                             accept_NA = FALSE,
                                                                                                             lower_bound = 0,
                                                                                                             lower_bound_inclusive = TRUE,
                                                                                                             upper_bound = Inf,
                                                                                                             upper_bound_inclusive = FALSE,
                                                                                                             accept_non_integer = TRUE))
                                                                       
                                                                       return(range)
                                                                     })(),
                                                                     mean_step = (ms <- range / (fixed_length^0.5 * smoothness)),
                                                                     stddev_step = stddev_step_factor * ms,
                                                                     initial_value = initial_value,
                                                                     target_value = target_value,
                                                                     seed = seed)
                                                    
                                                    local_env$.add_to_static_env(super$get_static_env())
                                                    local_env$static_env$err$set_assertions_status(assertions_status)
                                                    
                                                    #' Add your initialization code here here:
                                                  }
                                                ),
                                                #-----end of public-----
                                                
                                                #' ----- Add active bindings ----
                                                active = list(
                                                  
                                                )
                                                #-----end of active-----
        )
        
        #' An `Abstract_Numeric_Iterator` subclass for iteration over values generated
        #' by a provided generator function.
        Generic_Periodic_Iterator <- R6Class("Generic_Periodic_Iterator",
                                             # ---- R6 inheritance -----
                                             inherit = result_env$Abstract_Numeric_Iterator,
                                             
                                             #' ---- Adapt R6 options --------
                                             portable = TRUE,
                                             cloneable = TRUE,
                                             lock_class = TRUE,
                                             lock_objects = TRUE,
                                             
                                             #' ----- Add private Fields & methods ----
                                             private = list(
                                               periodic_func = NULL,
                                               param_env = NULL,
                                               current_step_no = NULL,
                                               last_freq = NULL,
                                               current_freq = NULL,
                                               
                                               get_offset_arg_smooth_offset = function(){
                                                 old_offset <- 0
                                                 smooth_offset <- function(){
                                                   if(private$current_step_no == 0){
                                                     result <- private$param_env$offset_arg_original
                                                   }
                                                   else{
                                                     result <- 2*pi*(private$current_step_no - 1)*(private$last_freq - private$current_freq) + old_offset
                                                   }
                                                   old_offset <<- result
                                                   return(result)
                                                 }
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
                                                 l$s$err$assert_msg(paste0("'current_step_no' should be between 0 and 'get_length()' ",
                                                                           "and should only be equal to the latter if 'finished()' is TRUE."),
                                                                    l$s$tcs$is_bounded(private$current_step_no,
                                                                                       lower_bound = 0,
                                                                                       lower_bound_inclusive = TRUE,
                                                                                       upper_bound = (len <- self$get_length()),
                                                                                       upper_bound_inclusive = TRUE,
                                                                                       NA_on_fail = FALSE),
                                                                    l$s$mg$`%eq%`(finished, private$current_step_no == len))
                                               },
                                               
                                               #' An `Abstract_Numeric_Iterator` subclass for iteration over values generated by
                                               #' a provided function.
                                               #' It generates `fixed_length` values and the `i`-th value is generated as:
                                               #' `amplitude * periodic_func(2 * pi * frequency * i + offset_arg) + offset_value`
                                               #' 
                                               #' __IMPORTANT__: See documentation of this method in `utils_lang_r6_subclass_TEMPLATE.R`.
                                               #'
                                               #' @param assertions_status If `TRUE`, assertions are activated (see `set_assertion_status()`
                                               #'   in `utils_lang_error.R`).
                                               #'   
                                               #' @param fixed_length Here, must be a finite, non-negative, non-`NA` integer. 
                                               #'   Also see documentation in `Abstract_Iterator`. 
                                               #' @param periodic_func A function that takes a single numeric parameter and returns a numeric of length one.
                                               #'   The function should be periodic with a period length of `2 * pi` 
                                               #'   (i.e. `periodic_func(x + 2*pi) = periodic_func(x)` for all `x`).
                                               #' @param frequency If `frequency` is a function, 
                                               #'   the frequency is set to `frequency=frequency()` each time a new
                                               #'   element is retrieved. Otherwise the constant value `frequency` is used.
                                               #'   This enables the use of frequencies that change over time.                                               #'   
                                               #' @param amplitude If `amplitude` is a function, 
                                               #'   the amplitude is set to `amplitude=amplitude()` each time a new
                                               #'   element is retrieved. Otherwise the constant value `amplitude` is used.
                                               #'   This enables the use of amplitudes that change over time.
                                               #' @param offset_value If `offset_value` is a function, 
                                               #'   the offset value is set to `offset_value=offset_value()` each time a new
                                               #'   element is retrieved. Otherwise the constant value `offset_value` is used.
                                               #'   This enables the use of offset values that change over time.
                                               #' @param offset_arg If `offset_arg` is a function, 
                                               #'   the argument offset is set to `offset_arg=offset_arg()` each time a new
                                               #'   element is retrieved. Otherwise the constant value `offset_arg` is used.
                                               #'   This enables the use of argument offsets that change over time.
                                               #'   __IMPORTANT__: This argument cannot be a function, if `smooth_offset_arg` is `TRUE`.
                                               #' @param smooth_offset_arg If this is `TRUE`, `offset_arg` may not be a function.
                                               #'   Activate this parameter to preserve the general shape of the resulting periodic
                                               #'   function when using a non-constant frequency. More precisely, if this parameter is set
                                               #'   to `TRUE`, then the `i`-th argument offset `offset_arg_i` is set as follows:
                                               #'   * The first one (i.e., for `i=1`): `offset_arg_1 = offset_arg`
                                               #'   * The following ones (i.e., for `i > 1`): Let `frequency_i` be the i-th frequency, then:
                                               #'   `offset_arg_i = 2 * pi * (i - 1) * (frequency_(i-1) - frequency_i) + offset_arg_(i-1)`
                                               #'   
                                               #'   The upper equation ensures that `periodic_func(2 * pi * frequency_i * (i-1) + offset_arg_i)`
                                               #'   equals `periodic_func(2 * pi * frequency_(i-1) * (i-1) + offset_arg_(i-1))`
                                               #'   (of course given that `periodic_func` is actually periodic with period `2 * pi`).
                                               #' 
                                               #' @export
                                               #'
                                               #' @examples
                                               #' @md
                                               initialize = function(assertions_status = FALSE, 
                                                                     fixed_length,
                                                                     periodic_func,
                                                                     frequency,
                                                                     amplitude = 1,
                                                                     offset_value = 0,
                                                                     offset_arg = 0,
                                                                     smooth_offset_arg = TRUE){
                                                 
                                                 super$initialize(assertions_status = assertions_status, 
                                                                  fixed_length = fixed_length,
                                                                  accept_NA = FALSE,
                                                                  accept_NaN = FALSE,
                                                                  accept_neg_inf = FALSE,
                                                                  accept_pos_inf = FALSE)
                                                 
                                                 local_env$.add_to_static_env(super$get_static_env())
                                                 local_env$static_env$err$set_assertions_status(assertions_status)
                                                 
                                                 l$s$err$assert_msg("'fixed_length' must be a finite, non-negative integer of length one.",
                                                                    l$s$tcs$has_length_1(fixed_length, NA_on_fail = FALSE),
                                                                    l$s$tcs$is_integer(fixed_length,
                                                                                       accept_NULL = FALSE,
                                                                                       accept_NaN = FALSE,
                                                                                       accept_NA = FALSE,
                                                                                       lower_bound = 0,
                                                                                       lower_bound_inclusive = TRUE,
                                                                                       upper_bound = Inf,
                                                                                       upper_bound_inclusive = FALSE))
                                                 
                                                 #' Add your initialization code here here:
                                                 l$s$err$assert_msg("'periodic_func' must be a function.",
                                                                    is.function(periodic_func))
                                                 
                                                 l$s$err$assert_msg("'frequency' must be a function or a numeric of length one.",
                                                                    l$s$mg$`%then%`(!(is_func <- is.function(frequency)),
                                                                                    l$s$tcs$has_length_1(frequency, NA_on_fail = FALSE)),
                                                                    l$s$mg$`%then%`(!is_func, 
                                                                                    l$s$tcs$is_numeric(frequency,
                                                                                                       accept_NULL = FALSE,
                                                                                                       accept_NaN = FALSE,
                                                                                                       accept_NA = FALSE,
                                                                                                       lower_bound = -Inf,
                                                                                                       lower_bound_inclusive = FALSE,
                                                                                                       upper_bound = Inf,
                                                                                                       upper_bound_inclusive = FALSE,
                                                                                                       accept_non_integer = TRUE)))
                                                 
                                                 l$s$err$assert_msg("'amplitude' must be a function or a numeric of length one.",
                                                                    l$s$mg$`%then%`(!(is_func <- is.function(amplitude)),
                                                                                    l$s$tcs$has_length_1(amplitude, NA_on_fail = FALSE)),
                                                                    l$s$mg$`%then%`(!is_func, 
                                                                                    l$s$tcs$is_numeric(amplitude,
                                                                                                       accept_NULL = FALSE,
                                                                                                       accept_NaN = FALSE,
                                                                                                       accept_NA = FALSE,
                                                                                                       lower_bound = -Inf,
                                                                                                       lower_bound_inclusive = FALSE,
                                                                                                       upper_bound = Inf,
                                                                                                       upper_bound_inclusive = FALSE,
                                                                                                       accept_non_integer = TRUE)))
                                                 
                                                 l$s$err$assert_msg("'offset_value' must be a function or a numeric of length one.",
                                                                    l$s$mg$`%then%`(!(is_func <- is.function(offset_value)),
                                                                                    l$s$tcs$has_length_1(offset_value, NA_on_fail = FALSE)),
                                                                    l$s$mg$`%then%`(!is_func, 
                                                                                    l$s$tcs$is_numeric(offset_value,
                                                                                                       accept_NULL = FALSE,
                                                                                                       accept_NaN = FALSE,
                                                                                                       accept_NA = FALSE,
                                                                                                       lower_bound = -Inf,
                                                                                                       lower_bound_inclusive = FALSE,
                                                                                                       upper_bound = Inf,
                                                                                                       upper_bound_inclusive = FALSE,
                                                                                                       accept_non_integer = TRUE)))

                                                 l$s$err$assert_msg(paste0("'offset_arg' must be a function or a numeric of length one. ",
                                                                           "It may not be a function if 'smooth_offset_arg' is TRUE."),
                                                                    l$s$mg$`%then%`(!(is_func <- is.function(offset_arg)),
                                                                                    l$s$tcs$has_length_1(offset_arg, NA_on_fail = FALSE)),
                                                                    l$s$mg$`%then%`(!is_func, 
                                                                                    l$s$tcs$is_numeric(offset_arg,
                                                                                                       accept_NULL = FALSE,
                                                                                                       accept_NaN = FALSE,
                                                                                                       accept_NA = FALSE,
                                                                                                       lower_bound = -Inf,
                                                                                                       lower_bound_inclusive = FALSE,
                                                                                                       upper_bound = Inf,
                                                                                                       upper_bound_inclusive = FALSE,
                                                                                                       accept_non_integer = TRUE)),
                                                                    l$s$mg$`%then%`(is_func, !smooth_offset_arg))
                                                 
                                                 l$s$err$assert_msg("'smooth_offset_arg' must be a logical of length one.",
                                                                    l$s$tcs$has_length_1(smooth_offset_arg, NA_on_fail = FALSE),
                                                                    l$s$tcs$is_logical(smooth_offset_arg,
                                                                                       accept_NULL = FALSE,
                                                                                       accept_NaN = FALSE,
                                                                                       accept_NA = FALSE))
                                                 
                                                 private$periodic_func <- periodic_func
                                                 private$param_env <- new.env()
                                                 
                                                 if(is.function(frequency)){
                                                   makeActiveBinding("frequency", fun = function(){
                                                     result <- frequency()
                                                     l$s$err$assert_msg("'frequency()' must return a numeric of length one.",
                                                                        l$s$tcs$has_length_1(result, NA_on_fail = FALSE),
                                                                        l$s$tcs$is_numeric(result,
                                                                                           accept_NULL = FALSE,
                                                                                           accept_NaN = FALSE,
                                                                                           accept_NA = FALSE,
                                                                                           lower_bound = -Inf,
                                                                                           lower_bound_inclusive = FALSE,
                                                                                           upper_bound = Inf,
                                                                                           upper_bound_inclusive = FALSE,
                                                                                           accept_non_integer = TRUE))
                                                     
                                                     return(result)
                                                   }, env=private$param_env)
                                                 }
                                                 else{
                                                   assign("frequency", frequency, envir = private$param_env)
                                                 }
                                                 
                                                 if(is.function(amplitude)){
                                                   makeActiveBinding("amplitude", fun = function(){
                                                     result <- amplitude()
                                                     l$s$err$assert_msg("'amplitude()' must return a numeric of length one.",
                                                                        l$s$tcs$has_length_1(result, NA_on_fail = FALSE),
                                                                        l$s$tcs$is_numeric(result,
                                                                                           accept_NULL = FALSE,
                                                                                           accept_NaN = FALSE,
                                                                                           accept_NA = FALSE,
                                                                                           lower_bound = -Inf,
                                                                                           lower_bound_inclusive = FALSE,
                                                                                           upper_bound = Inf,
                                                                                           upper_bound_inclusive = FALSE,
                                                                                           accept_non_integer = TRUE))
                                                     
                                                     return(result)
                                                   }, env=private$param_env)
                                                 }
                                                 else{
                                                   assign("amplitude", amplitude, envir = private$param_env)
                                                 }
                                                 
                                                 if(is.function(offset_value)){
                                                   makeActiveBinding("offset_value", fun = function(){
                                                     result <- offset_value()
                                                     l$s$err$assert_msg("'offset_value()' must return a numeric of length one.",
                                                                        l$s$tcs$has_length_1(result, NA_on_fail = FALSE),
                                                                        l$s$tcs$is_numeric(result,
                                                                                           accept_NULL = FALSE,
                                                                                           accept_NaN = FALSE,
                                                                                           accept_NA = FALSE,
                                                                                           lower_bound = -Inf,
                                                                                           lower_bound_inclusive = FALSE,
                                                                                           upper_bound = Inf,
                                                                                           upper_bound_inclusive = FALSE,
                                                                                           accept_non_integer = TRUE))
                                                     
                                                     return(result)
                                                   }, env=private$param_env)
                                                 }
                                                 else{
                                                   assign("offset_value", offset_value, envir = private$param_env)
                                                 }
                                                 
                                                 if(is.function(offset_arg)){
                                                   makeActiveBinding("offset_arg", fun = function(){
                                                     result <- offset_arg()
                                                     l$s$err$assert_msg("'offset_arg()' must return a numeric of length one.",
                                                                        l$s$tcs$has_length_1(result, NA_on_fail = FALSE),
                                                                        l$s$tcs$is_numeric(result,
                                                                                           accept_NULL = FALSE,
                                                                                           accept_NaN = FALSE,
                                                                                           accept_NA = FALSE,
                                                                                           lower_bound = -Inf,
                                                                                           lower_bound_inclusive = FALSE,
                                                                                           upper_bound = Inf,
                                                                                           upper_bound_inclusive = FALSE,
                                                                                           accept_non_integer = TRUE))
                                                     
                                                     return(result)
                                                   }, env=private$param_env)
                                                 }
                                                 else if(smooth_offset_arg){
                                                   assign("offset_arg_original", offset_arg, envir = private$param_env)
                                                   makeActiveBinding("offset_arg", fun = private$get_offset_arg_smooth_offset(),
                                                                     env = private$param_env)
                                                 }
                                                 else{
                                                   assign("offset_arg", offset_arg, envir = private$param_env)
                                                 }
                                                 
                                                 private$current_step_no <- 0
                                                 private$last_freq <- 0
                                                 private$current_freq <- private$param_env$frequency
                                               },
                                               
                                               #' See documentation in `Abstract_Iterator`.
                                               #' @export
                                               #' @md
                                               finalize = function() {
                                                 super$finalize()
                                               },
                                               
                                               .validate_next = function(element) {
                                                 return(super$.validate_next(element))
                                               },
                                               
                                               #' ----------- THE FOLLOWING METHODS __MAY__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                               
                                               #' Method is called right before [get_next()] returns.
                                               #' May be overridden by subclasses in order to perform additional
                                               #' clean-up operations at the end of a retrieval operation (i.e., call to [get_next()]).
                                               #' 
                                               #' Call `super$.get_next_post()` in the beginning, if you override this method.
                                               #' 
                                               #' This dummy-implementation is empty.
                                               #'
                                               #' @export
                                               #'
                                               #' @examples
                                               #' @md
                                               .get_next_post = function(){
                                                 super$.get_next_post()
                                               },
                                               
                                               
                                               #' ----------- THE FOLLOWING METHODS __MUST__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                               
                                               .get_next = function(num) {
                                                 result <- vector(mode="numeric", length = num)
                                                 if(num > 0){
                                                   for(i in 1:num){
                                                     result[[i]] <- private$param_env$amplitude * 
                                                       private$periodic_func(2 * pi * private$current_freq * private$current_step_no + 
                                                                               private$param_env$offset_arg) + 
                                                       private$param_env$offset_value
                                                     
                                                     private$current_step_no <- private$current_step_no + 1
                                                     private$last_freq <- private$current_freq
                                                     private$current_freq <- private$param_env$frequency
                                                   }
                                                 }
                                                 return(result)
                                               },
                                               
                                               .get_next_count = function() {
                                                 return(self$get_num_remaining())
                                               },
                                               
                                              .finished = function() {
                                                 return(identical(self$get_num_remaining(), 0))
                                               }
                                               
                                             ),
                                             #-----end of public-----
                                             
                                             #' ----- Add active bindings ----
                                             active = list(
                                               
                                             )
                                             #-----end of active-----
        )
        
        #' A `Generic_Periodic_Iterator` subclass where the periodic function is `sin`.
        Sine_Iterator <- R6Class("Sine_Iterator",
                                             # ---- R6 inheritance -----
                                             inherit = result_env$Generic_Periodic_Iterator,
                                             
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
                                                 
                                               },
                                               
                                               #' A `Generic_Periodic_Iterator` subclass where the periodic function is `sin`.
                                               #' 
                                               #' __IMPORTANT__: See documentation of this method in `utils_lang_r6_subclass_TEMPLATE.R`.
                                               #'
                                               #' @param assertions_status If `TRUE`, assertions are activated (see `set_assertion_status()`
                                               #'   in `utils_lang_error.R`).
                                               #'   
                                               #' @param fixed_length See `Generic_Periodic_Iterator`.
                                               #' @param frequency See `Generic_Periodic_Iterator`.
                                               #' @param amplitude See `Generic_Periodic_Iterator`.
                                               #' @param offset_value See `Generic_Periodic_Iterator`.
                                               #' @param offset_arg See `Generic_Periodic_Iterator`.
                                               #' @param smooth_offset_arg See `Generic_Periodic_Iterator`.
                                               #' 
                                               #' @export
                                               #'
                                               #' @examples
                                               #' @md
                                               initialize = function(assertions_status = FALSE, 
                                                                     fixed_length,
                                                                     frequency,
                                                                     amplitude = 1,
                                                                     offset_value = 0,
                                                                     offset_arg = 0,
                                                                     smooth_offset_arg = TRUE){
                                                 
                                                 super$initialize(assertions_status = assertions_status, 
                                                                  fixed_length = fixed_length,
                                                                  periodic_func = sin,
                                                                  frequency = frequency,
                                                                  amplitude = amplitude,
                                                                  offset_value = offset_value,
                                                                  offset_arg = offset_arg,
                                                                  smooth_offset_arg = smooth_offset_arg)
                                                 
                                                 local_env$.add_to_static_env(super$get_static_env())
                                                 local_env$static_env$err$set_assertions_status(assertions_status)
                                                 
                                               },
                                               
                                               #' See documentation in `Abstract_Iterator`.
                                               #' @export
                                               #' @md
                                               finalize = function() {
                                                 super$finalize()
                                               },
                                               
                                               .validate_next = function(element) {
                                                 return(super$.validate_next(element))
                                               },
                                               
                                               #' ----------- THE FOLLOWING METHODS __MAY__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                               
                                               #' Method is called right before [get_next()] returns.
                                               #' May be overridden by subclasses in order to perform additional
                                               #' clean-up operations at the end of a retrieval operation (i.e., call to [get_next()]).
                                               #' 
                                               #' Call `super$.get_next_post()` in the beginning, if you override this method.
                                               #' 
                                               #' This dummy-implementation is empty.
                                               #'
                                               #' @export
                                               #'
                                               #' @examples
                                               #' @md
                                               .get_next_post = function(){
                                                 super$.get_next_post()
                                               },
                                               
                                               
                                               #' ----------- THE FOLLOWING METHODS __MUST__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                               
                                               .get_next = function(num) {
                                                 return(super$.get_next(num))
                                               },
                                               
                                               .get_next_count = function() {
                                                 return(super$.get_next_count())
                                               },
                                               
                                               .finished = function() {
                                                 return(super$.finished())
                                               }
                                               
                                             ),
                                             #-----end of public-----
                                             
                                             #' ----- Add active bindings ----
                                             active = list(
                                               
                                             )
                                             #-----end of active-----
        )
        
        #' A `Generic_Periodic_Iterator` subclass where the periodic function is a sawtooth wave.
        Sawtooth_Iterator <- R6Class("Sawtooth_Iterator",
                                 # ---- R6 inheritance -----
                                 inherit = result_env$Generic_Periodic_Iterator,
                                 
                                 #' ---- Adapt R6 options --------
                                 portable = TRUE,
                                 cloneable = TRUE,
                                 lock_class = TRUE,
                                 lock_objects = TRUE,
                                 
                                 #' ----- Add private Fields & methods ----
                                 private = list(
                                   sawtooth = function(x){
                                     return(x/pi - 2*floor(0.5*(1 + x/pi)))
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
                                     
                                   },
                                   
                                   #' A `Generic_Periodic_Iterator` subclass where the periodic function is a sawtooth wave
                                   #' that is centered around `offset_value`.
                                   #' 
                                   #' __IMPORTANT__: See documentation of this method in `utils_lang_r6_subclass_TEMPLATE.R`.
                                   #'
                                   #' @param assertions_status If `TRUE`, assertions are activated (see `set_assertion_status()`
                                   #'   in `utils_lang_error.R`).
                                   #'   
                                   #' @param fixed_length See `Generic_Periodic_Iterator`.
                                   #' @param frequency See `Generic_Periodic_Iterator`.
                                   #' @param amplitude See `Generic_Periodic_Iterator`.
                                   #' @param offset_value See `Generic_Periodic_Iterator`.
                                   #' @param offset_arg See `Generic_Periodic_Iterator`.
                                   #' @param smooth_offset_arg See `Generic_Periodic_Iterator`.
                                   #' 
                                   #' @export
                                   #'
                                   #' @examples
                                   #' @md
                                   initialize = function(assertions_status = FALSE, 
                                                         fixed_length,
                                                         frequency,
                                                         amplitude = 1,
                                                         offset_value = 0,
                                                         offset_arg = 0,
                                                         smooth_offset_arg = TRUE){
                                     
                                     super$initialize(assertions_status = assertions_status, 
                                                      fixed_length = fixed_length,
                                                      periodic_func = private$sawtooth,
                                                      frequency = frequency,
                                                      amplitude = amplitude,
                                                      offset_value = offset_value,
                                                      offset_arg = offset_arg,
                                                      smooth_offset_arg = smooth_offset_arg)
                                     
                                     local_env$.add_to_static_env(super$get_static_env())
                                     local_env$static_env$err$set_assertions_status(assertions_status)
                                     
                                   },
                                   
                                   #' See documentation in `Abstract_Iterator`.
                                   #' @export
                                   #' @md
                                   finalize = function() {
                                     super$finalize()
                                   },
                                   
                                   .validate_next = function(element) {
                                     return(super$.validate_next(element))
                                   },
                                   
                                   #' ----------- THE FOLLOWING METHODS __MAY__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                   
                                   #' Method is called right before [get_next()] returns.
                                   #' May be overridden by subclasses in order to perform additional
                                   #' clean-up operations at the end of a retrieval operation (i.e., call to [get_next()]).
                                   #' 
                                   #' Call `super$.get_next_post()` in the beginning, if you override this method.
                                   #' 
                                   #' This dummy-implementation is empty.
                                   #'
                                   #' @export
                                   #'
                                   #' @examples
                                   #' @md
                                   .get_next_post = function(){
                                     super$.get_next_post()
                                   },
                                   
                                   
                                   #' ----------- THE FOLLOWING METHODS __MUST__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                   
                                   .get_next = function(num) {
                                     return(super$.get_next(num))
                                   },
                                   
                                   .get_next_count = function() {
                                     return(super$.get_next_count())
                                   },
                                   
                                   .finished = function() {
                                     return(super$.finished())
                                   }
                                   
                                 ),
                                 #-----end of public-----
                                 
                                 #' ----- Add active bindings ----
                                 active = list(
                                   
                                 )
                                 #-----end of active-----
        )
        
        #' A `Generic_Periodic_Iterator` subclass where the periodic function is a triangle wave
        #' that is centered around `offset_value`.
        Triangle_Iterator <- R6Class("Triangle_Iterator",
                                     # ---- R6 inheritance -----
                                     inherit = result_env$Generic_Periodic_Iterator,
                                     
                                     #' ---- Adapt R6 options --------
                                     portable = TRUE,
                                     cloneable = TRUE,
                                     lock_class = TRUE,
                                     lock_objects = TRUE,
                                     
                                     #' ----- Add private Fields & methods ----
                                     private = list(
                                       triangle = function(x){
                                         x <- x + pi/2
                                         return(2*abs(x/pi - 2*floor(0.5*(1 + x/pi))) - 1)
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
                                         
                                       },
                                       
                                       #' A `Generic_Periodic_Iterator` subclass where the periodic function is a triangle wave.
                                       #' 
                                       #' __IMPORTANT__: See documentation of this method in `utils_lang_r6_subclass_TEMPLATE.R`.
                                       #'
                                       #' @param assertions_status If `TRUE`, assertions are activated (see `set_assertion_status()`
                                       #'   in `utils_lang_error.R`).
                                       #'   
                                       #' @param fixed_length See `Generic_Periodic_Iterator`.
                                       #' @param frequency See `Generic_Periodic_Iterator`.
                                       #' @param amplitude See `Generic_Periodic_Iterator`.
                                       #' @param offset_value See `Generic_Periodic_Iterator`.
                                       #' @param offset_arg See `Generic_Periodic_Iterator`.
                                       #' @param smooth_offset_arg See `Generic_Periodic_Iterator`.
                                       #' 
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       initialize = function(assertions_status = FALSE, 
                                                             fixed_length,
                                                             frequency,
                                                             amplitude = 1,
                                                             offset_value = 0,
                                                             offset_arg = 0,
                                                             smooth_offset_arg = TRUE){
                                         
                                         super$initialize(assertions_status = assertions_status, 
                                                          fixed_length = fixed_length,
                                                          periodic_func = private$triangle,
                                                          frequency = frequency,
                                                          amplitude = amplitude,
                                                          offset_value = offset_value,
                                                          offset_arg = offset_arg,
                                                          smooth_offset_arg = smooth_offset_arg)
                                         
                                         local_env$.add_to_static_env(super$get_static_env())
                                         local_env$static_env$err$set_assertions_status(assertions_status)
                                         
                                       },
                                       
                                       #' See documentation in `Abstract_Iterator`.
                                       #' @export
                                       #' @md
                                       finalize = function() {
                                         super$finalize()
                                       },
                                       
                                       .validate_next = function(element) {
                                         return(super$.validate_next(element))
                                       },
                                       
                                       #' ----------- THE FOLLOWING METHODS __MAY__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                       
                                       #' Method is called right before [get_next()] returns.
                                       #' May be overridden by subclasses in order to perform additional
                                       #' clean-up operations at the end of a retrieval operation (i.e., call to [get_next()]).
                                       #' 
                                       #' Call `super$.get_next_post()` in the beginning, if you override this method.
                                       #' 
                                       #' This dummy-implementation is empty.
                                       #'
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       .get_next_post = function(){
                                         super$.get_next_post()
                                       },
                                       
                                       
                                       #' ----------- THE FOLLOWING METHODS __MUST__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                       
                                       .get_next = function(num) {
                                         return(super$.get_next(num))
                                       },
                                       
                                       .get_next_count = function() {
                                         return(super$.get_next_count())
                                       },
                                       
                                       .finished = function() {
                                         return(super$.finished())
                                       }
                                       
                                     ),
                                     #-----end of public-----
                                     
                                     #' ----- Add active bindings ----
                                     active = list(
                                       
                                     )
                                     #-----end of active-----
        )
        
        #' An `Offset_Normalized_Concatenated_Iterator` subclass for internal iterators
        #' returning numeric values. Adds option for adding normal noise to the offset.
        Numeric_Offset_Normalized_Concatenated_Iterator <- R6Class("Numeric_Offset_Normalized_Concatenated_Iterator",
                                                           # ---- R6 inheritance -----
                                                           inherit = l$s$it$Offset_Normalized_Concatenated_Iterator,
                                                           
                                                           #' ---- Adapt R6 options --------
                                                           portable = TRUE,
                                                           cloneable = TRUE,
                                                           lock_class = TRUE,
                                                           lock_objects = TRUE,
                                                           
                                                           #' ----- Add private Fields & methods ----
                                                           private = list(
                                                             offset_mean_max_num_elems = NULL,
                                                             offset_sd_max_num_elems = NULL
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
                                                             
                                                             #' An `Offset_Normalized_Concatenated_Iterator` subclass for internal iterators
                                                             #' returning numeric values. Adds option for adding normal noise to the offset.
                                                             #' 
                                                             #' The offset `o` is computed as follows:
                                                             #' 1. Compute `o` exactly like in `Offset_Normalized_Concatenated_Iterator` 
                                                             #'    with `offset_max_num_elems` set to `offset_mean_max_num_elems + 1`
                                                             #'    (i.e., mean is computed over `offset_mean_max_num_elems` 
                                                             #'    differences `x_(i-1) - x_i` - see documentation of superclass).
                                                             #' 2. If the new iterator's corresponding `normalize_offsets` entry is `TRUE`, 
                                                             #'    add a random number to `o` which is drawn from a 
                                                             #'    normal distribution with zero mean and 
                                                             #'    standard deviation `sd` which is computed as follows:
                                                             #'    * Let `x_1, ..., x_(offset_sd_max_num_elems + 1)` be the
                                                             #'      last `offset_sd_max_num_elems + 1` elements produced by the finished iterator
                                                             #'      (or fewer, if it produced less elements) where `x_1` is the newest and
                                                             #'      `x_(offset_sd_max_num_elems + 1)` is the oldest one. Let 
                                                             #'      `sd = standard_deviation(x_i - x_(i-1))` be the standard deviation of the 
                                                             #'      `offset_sd_max_num_elems`
                                                             #'      differences of subsequent elements 
                                                             #'      (define `sd = 0` if `offset_sd_max_num_elems <= 1`).
                                                             #' 
                                                             #' 
                                                             #' __IMPORTANT__: See documentation of this method in `utils_lang_r6_subclass_TEMPLATE.R`.
                                                             #'
                                                             #'   
                                                             #' @param assertions_status See `Abstract_Iterator`.
                                                             #' @param iterators  See `Concatenated_Iterator`.
                                                             #' @param lock See `Concatenated_Iterator`.
                                                             #' @param initial_value See `Offset_Normalized_Concatenated_Iterator`.
                                                             #' @param offset_mean_max_num_elems A positive integer of length one.
                                                             #'   See description above for more details.
                                                             #' @param offset_sd_max_num_elems A positive integer of length one.
                                                             #' @param normalize_offsets See `Offset_Normalized_Concatenated_Iterator`.
                                                             #' @param additional_offsets See `Offset_Normalized_Concatenated_Iterator`.
                                                             #' @export
                                                             #'
                                                             #' @examples
                                                             #' @md
                                                             initialize = function(assertions_status = FALSE, 
                                                                                   iterators, 
                                                                                   lock = FALSE,
                                                                                   initial_offset = NULL,
                                                                                   normalize_offsets = TRUE,
                                                                                   additional_offsets = NULL,
                                                                                   offset_mean_max_num_elems = 2,
                                                                                   offset_sd_max_num_elems = 10){
                                                               super$initialize(
                                                                 assertions_status = assertions_status, 
                                                                 iterators = iterators,
                                                                 lock = lock,
                                                                 offset_max_num_elems = max(offset_mean_max_num_elems, offset_sd_max_num_elems) + 1,
                                                                 initial_offset = initial_offset,
                                                                 division_operator = `/`,
                                                                 summation_operator = `+`,
                                                                 minus_operator = `-`,
                                                                 normalize_offsets = normalize_offsets,
                                                                 additional_offsets = additional_offsets
                                                                 )
                                                               
                                                               local_env$.add_to_static_env(super$get_static_env())
                                                               local_env$static_env$err$set_assertions_status(assertions_status)
                                                               
                                                               #' Add your initialization code here here:
                                                               
                                                               l$s$err$assert_msg(paste0("'offset_mean_max_num_elems' be a positive integer ",
                                                                                         "of length one."),
                                                                                  l$s$tcs$has_length_1(offset_mean_max_num_elems, NA_on_fail = FALSE),
                                                                                  l$s$tcs$is_integer(offset_mean_max_num_elems,
                                                                                                     accept_NULL = FALSE,
                                                                                                     accept_NaN = FALSE,
                                                                                                     accept_NA = FALSE,
                                                                                                     lower_bound = 0,
                                                                                                     lower_bound_inclusive = TRUE,
                                                                                                     upper_bound = Inf,
                                                                                                     upper_bound_inclusive = FALSE))
                                                               
                                                               l$s$err$assert_msg(paste0("'offset_sd_max_num_elems' be a positive integer ",
                                                                                         "of length one."),
                                                                                  l$s$tcs$has_length_1(offset_sd_max_num_elems, NA_on_fail = FALSE),
                                                                                  l$s$tcs$is_integer(offset_sd_max_num_elems,
                                                                                                     accept_NULL = FALSE,
                                                                                                     accept_NaN = FALSE,
                                                                                                     accept_NA = FALSE,
                                                                                                     lower_bound = 0,
                                                                                                     lower_bound_inclusive = TRUE,
                                                                                                     upper_bound = Inf,
                                                                                                     upper_bound_inclusive = FALSE))
                                                               
                                                               private$offset_mean_max_num_elems <- offset_mean_max_num_elems
                                                               private$offset_sd_max_num_elems <- offset_sd_max_num_elems
                                                             },
                                                             
                                                             finalize = function() {
                                                               super$finalize()
                                                             },
                                                             
                                                             .validate_next = function(element) {
                                                               #' Add additional validation, if required
                                                               #' Note that, at this point, each element is already validated 
                                                               #' by the respective iterator that returned it.
                                                               #' 
                                                               result <- super$.validate_next(element)
                                                               
                                                               return(result && 
                                                                        l$s$tcs$has_length_1(element, NA_on_fail = FALSE) &&
                                                                        l$s$tcs$is_numeric(element,
                                                                                           accept_NULL = FALSE,
                                                                                           accept_NaN = FALSE,
                                                                                           accept_NA = FALSE,
                                                                                           lower_bound = -Inf,
                                                                                           lower_bound_inclusive = FALSE,
                                                                                           upper_bound = Inf,
                                                                                           upper_bound_inclusive = FALSE,
                                                                                           accept_non_integer = TRUE))
                                                             },
                                                             
                                                             .new_iterator = function(new_iterator){
                                                               new_iterator <- super$.new_iterator(new_iterator)
                                                               
                                                               return(new_iterator)
                                                             },
                                                             
                                                             .new_elements = function(new_elements){
                                                               new_elements <- super$.new_elements(new_elements)
                                                               
                                                               return(new_elements)
                                                             },
                                                             
                                                             #' Method is called right before [get_next()] returns.
                                                             #' May be overridden by subclasses in order to perform additional
                                                             #' clean-up operations at the end of a retrieval operation (i.e., call to [get_next()]).
                                                             #' 
                                                             #' Call `super$.get_next_post()` in the beginning, if you override this method.
                                                             #' 
                                                             #'
                                                             #' @export
                                                             #'
                                                             #' @examples
                                                             #' @md
                                                             .get_next_post = function(){
                                                               super$.get_next_post()
                                                             },
                                                             
                                                             .compute_offset = function(element_buffer, new_elements){
                                                               element_buffer <- as.vector(element_buffer, mode="numeric")
                                                               buf_len <- length(element_buffer)
                                                               l$s$err$assert_msg("'element_buffer' should contain at least one element at this point.",
                                                                                  buf_len > 0)
                                                               l$s$err$assert_msg("'new_elements' should contain at least one element at this point.",
                                                                                  length(new_elements) > 0)
                                                               
                                                               if(buf_len > 1){
                                                                 diffs <- mapply(FUN=`-`, element_buffer[1:(buf_len-1)], element_buffer[2:buf_len])
                                                                 l$s$err$assert_msg("'diffs' has invalid length.",
                                                                                    length(diffs) == buf_len - 1)
                                                               }
                                                               else{
                                                                 diffs <- c()
                                                               }
                                                               
                                                               num_diffs <- length(diffs)
                                                               
                                                               offset <- 0
                                                               num_mean <- min(num_diffs, private$offset_mean_max_num_elems)
                                                               if(num_mean > 0){
                                                                 offset <- offset + mean(diffs[1:num_mean])
                                                               }
                                                               
                                                               num_sd <- min(num_diffs, private$offset_sd_max_num_elems)
                                                               if(num_sd > 1){
                                                                 offset <- offset + rnorm(1, mean = 0, sd = sd(diffs[1:num_sd]))
                                                               }
                                                               
                                                               
                                                               offset <- offset + element_buffer[1] - new_elements[[1]]
                                                               
                                                               return(offset)
                                                             }
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