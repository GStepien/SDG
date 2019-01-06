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
#' @title  Collection of subclasses of `R6_Base` from `utils_lang_r6_baseclass.R` representing
#'   various iterators.
#' @description Contains a single [get_<name>_env()] method
#'   that returns an environment containing the `R6` constructor objects of these subclasses.
#' @md

# Include guard
if (!exists("UTILS_ITERATORS_R", inherits = FALSE)) {
  # TODO: Name variable as filename in uppercase in the same format as above
  UTILS_ITERATORS_R = TRUE
  
  #' @return An environment containing one or multiple constructor objects for `R6` subclasses of 
  #'         `R6_Base` from `utils_lang_r6_baseclass.R`. These provide functionality for iteration over
  #'         data from some arbitrary source.
  #' @export
  #'
  #' @examples
  #' @md
  get_utils_iterators_env <- function() {
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
    other_scripts <- c(file.path(".", "utils", "utils_time.R"),
                       file.path(".", "utils", "utils_math_general.R"),
                       file.path(".", "utils", "utils_data_tools.R"),
                       file.path(".", "utils", "utils_factory.R"), 
                       file.path(".", "utils", "utils_lang_error.R"), 
                       file.path(".", "utils", "utils_lang_typechecks.R"))
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
    assign("utils_time", get_utils_time_env(), envir = static_env)
    assign("utils_math_general", get_utils_math_general_env(), envir = static_env)
    assign("utils_data_tools", get_utils_data_tools_env(), envir = static_env)
    assign("utils_factory", get_utils_factory_env(), envir = static_env)
    assign("utils_lang_error", get_utils_lang_error_env(), envir = static_env)
    assign("utils_lang_typechecks", get_utils_lang_typechecks_env(), envir = static_env)
    
    #' __Optional__: Add short-hand variants of the above:
    #' E.g.: assign("gen", static_env$utils_lang_general, envir = static_env)
    assign("t", static_env$utils_time, envir = static_env)
    assign("mg", static_env$utils_math_general, envir = static_env)
    assign("dts", static_env$utils_data_tools, envir = static_env)
    assign("f", static_env$utils_factory, envir = static_env)
    assign("err", static_env$utils_lang_error, envir = static_env)
    assign("tcs", static_env$utils_lang_typechecks, envir = static_env)
    # --------- Load required package dependencies -------    
    #' Append other packages to `package_dependencies` which are to be installed (if necessary) and loaded
    package_dependencies <- c("R6", "dequer") # e.g., c("R6","smoother","rowr","randomcoloR")
    
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
    .initialized_once = FALSE
    
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
    json_dec_name <- "decorators"
    json_dec_with_name <- "decorate_with"
    
    l$json_dec_name <- trimws(l$json_dec_name)
    l$json_dec_with_name <- trimws(l$json_dec_with_name)
    
    # -------- Create environment to be returned containing 
    #          one or multiple topically related `R6_Base` subclasses-------
    #' Use `local_env` to access the enclosing environment and `static_env`
    #' (or `local_env$static_env`) to access static methods and/or fields
    #' previously added via `other_scripts`. `R6_Base` (or `local_env$R6_Base`) contains 
    #' the baseclass constructor object from `utils_lang_r6_baseclass.R`.
    #' All `R6` classes here should be subclasses of this class.
    #'
    #' Add one or multiple `R6_Base` subclass constuctor objects to the `evalq` expression below.
    result_env <- new.env()
    evalq(
      {
        
        #' An abstract class for iteration over abstract data elements.
        Abstract_Iterator <- R6Class("Abstract_Iterator",
                                     # ---- R6 inheritance -----
                                     inherit = R6_Base,
                                     
                                     #' ---- Adapt R6 options --------
                                     portable = TRUE,
                                     cloneable = TRUE,
                                     lock_class = TRUE,
                                     lock_objects = TRUE,
                                     
                                     #' ----- Add private Fields & methods ----
                                     private = list(
                                       
                                       next_count_upper_bound = 10000,
                                       
                                       #' Wrapper for
                                       #' `.validate_next(element)` that adds assertions that returned 
                                       #' value is a logical of length 1.
                                       #' Is called in `get_next(num)` for each element in the list returned by
                                       #' `.get_next(num)`.
                                       #' 
                                       #' @param element The element to be tested.
                                       #'
                                       #' @return `TRUE` if and only if the provided element shall be considered valid in
                                       #'   `get_next(num)`.
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       validate_next = function(element) {
                                         result <- self$.validate_next(element)
                                         l$s$err$assert_msg(".validate_next(element) must return a logical of length one.",
                                                            l$s$tcs$has_length_1(result, NA_on_fail = FALSE),      
                                                            l$s$tcs$is_logical(result, 
                                                                               accept_NULL = FALSE,
                                                                               accept_NaN = FALSE,
                                                                               accept_NA = FALSE))
                                         return(result)
                                       },
                                       
                                       fixed_length = NULL,
                                       num_remaining = NULL,
                                       old_fixed_length = NULL,
                                       
                                       #' Additional checks for consistency of `fixed_length` and `num_remaining`.
                                       #' Called in `get_length()` and `get_num_remaining()`.
                                       #'
                                       #' @return
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       check_lengths = function(){
                                         if(l$s$err$get_assertions_status()){
                                           l$s$err$assert_msg(paste0("If 'fixed_length' is 'NA', then and only then 'num_remaining' also must be 'NA'. ",
                                                                     "Otherwise, 'num_remaining' must be a non-negative, possibly infinite ",
                                                                     "integer of length one and not bigger than 'get_length()'."),
                                                              l$s$tcs$has_length_1(private$num_remaining, NA_on_fail = FALSE),
                                                              l$s$tcs$has_length_1(private$fixed_length, NA_on_fail = FALSE),
                                                              l$s$mg$`%eq%`(is.na(private$fixed_length), is.na(private$num_remaining)),
                                                              l$s$mg$`%then%`(!is.na(private$fixed_length), l$s$tcs$is_integer(private$fixed_length,
                                                                                                                               accept_NULL = FALSE,
                                                                                                                               accept_NaN = FALSE,
                                                                                                                               accept_NA = FALSE,
                                                                                                                               lower_bound = 0,
                                                                                                                               lower_bound_inclusive = TRUE,
                                                                                                                               upper_bound = Inf,
                                                                                                                               upper_bound_inclusive = TRUE)),
                                                              l$s$mg$`%then%`(!is.na(private$fixed_length), l$s$tcs$is_integer(private$num_remaining,
                                                                                                                               accept_NULL = FALSE,
                                                                                                                               accept_NaN = FALSE,
                                                                                                                               accept_NA = FALSE,
                                                                                                                               lower_bound = 0,
                                                                                                                               lower_bound_inclusive = TRUE,
                                                                                                                               upper_bound = private$fixed_length,
                                                                                                                               upper_bound_inclusive = TRUE)))
                                           is_na_length <- is.na(private$fixed_length)
                                           if(is.na(private$old_fixed_length) && !is_na_length){
                                             private$old_fixed_length <- private$fixed_length
                                           }
                                           else if(!is_na_length) {
                                             l$s$err$assert_msg("'fixed_length' must be constant once it is non-NA.",
                                                                l$s$dts$equals(private$old_fixed_length, private$fixed_length))
                                           }
                                         }
                                       }
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
                                       
                                       #' __IMPORTANT__: Use only if you know what you are doing! Provided value will be the one
                                       #'   returned by `get_length()` (and therefore indirectly used in a number of other methods) 
                                       #'   and might lead to inconsistencies if set to an inappropriate number. 
                                       #'  
                                       #'  Should only be called by extending subclasses.
                                       #'  See also [.set_num_remaining()].
                                       #'
                                       #' @param fixed_length Either `NA` if the total numer of elements to be iterated over is unknown,
                                       #' `+Inf` if this iterator never finishes (thus [finished()] will always return `FALSE`)
                                       #' or a non-negative integer indicating the __total__ numer of elements that this
                                       #' iterator iterates over.
                                       #' 
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       .set_fixed_length = function(fixed_length){
                                         l$s$err$assert_msg("'fixed_length' must be a non-negative integer of length 1, +Inf or 'NA'.", 
                                                            l$s$tcs$has_length_1(fixed_length, NA_on_fail = FALSE),
                                                            l$s$tcs$is_integer(fixed_length,
                                                                               accept_NULL = FALSE,
                                                                               accept_NaN = FALSE,
                                                                               accept_NA = TRUE,
                                                                               lower_bound = 0,
                                                                               lower_bound_inclusive = TRUE,
                                                                               upper_bound = Inf,
                                                                               upper_bound_inclusive = TRUE))
                                         private$fixed_length <- fixed_length
                                       },
                                       
                                       #' __IMPORTANT__: Use only if you know what you are doing! Provided value will be the one
                                       #'   returned by `get_num_remaining()` (and therefore indirectly used in a number of other methods) 
                                       #'   and might lead to inconsistencies if set to an inappropriate number.
                                       #'   Only call __after__ setting current length in [.set_fixed_length()].
                                       #'  
                                       #'  Should only be called by extending subclasses.
                                       #'  See also [.set_fixed_length()].
                                       #'
                                       #' @param num_remaining If `get_length()` is `NA` or `+Inf`, `num_remaining` should be the same.
                                       #'   Otherweise it must be a non-negative number not bigger than `get_length()` indicating
                                       #'   the __exact__ number of remaining elements that can be retrieved.
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       .set_num_remaining = function(num_remaining){
                                         if(l$s$err$get_assertions_status()){
                                           len <- private$fixed_length
                                           len_is_regular_int <- l$s$tcs$is_integer(len,
                                                                            accept_NULL = FALSE,
                                                                            accept_NaN = FALSE,
                                                                            accept_NA = FALSE,
                                                                            lower_bound = 0,
                                                                            lower_bound_inclusive = TRUE,
                                                                            upper_bound = Inf,
                                                                            upper_bound_inclusive = FALSE)
                                           
                                           l$s$err$assert_msg(paste0("'num_remaining' must be a non-negative integer of ",
                                                                     "length 1, +Inf or 'NA'."),
                                                              l$s$tcs$has_length_1(num_remaining, NA_on_fail = FALSE),
                                                              l$s$tcs$is_integer(num_remaining,
                                                                                 accept_NULL = FALSE,
                                                                                 accept_NaN = FALSE,
                                                                                 accept_NA = TRUE,
                                                                                 lower_bound = 0,
                                                                                 lower_bound_inclusive = TRUE,
                                                                                 upper_bound = Inf,
                                                                                 upper_bound_inclusive = TRUE),
                                                              !len_is_regular_int && l$s$dts$equals(num_remaining, len) ||
                                                                len_is_regular_int && num_remaining <= len)
                                         }
                                         private$num_remaining <- num_remaining
                                       },
                                       
                                       
                                       #' Assertions (see `s$l$err$get_ and set_assertions_status()`) performing
                                       #' domain and inter variable consistency checks for the provided values.
                                       #' 
                                       #' Note that here, this method is called at the end of `finished()`
                                       #' and `get_next_count()`. The latter is called in `get_next()`.
                                       #' 
                                       #' __IMPORTANT__: Extending classes might add additional checks and parameters and call this
                                       #'   method at appropriate points but do not forget
                                       #'   to call `super$.check_consistency()`!
                                       #' 
                                       #' Should only be called by extending subclasses.
                                       #'  
                                       #' @param finished The return value of `.finished()`.
                                       #' @param next_count The return value of `.get_next_count()`.
                                       #'
                                       #' @return
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       .check_consistency = function(finished = self$.finished(), 
                                                                      next_count = self$.get_next_count()){
                                         
                                         if(l$s$err$get_assertions_status()){
                                           length <- self$get_length()
                                           num_remaining <- self$get_num_remaining()
                                           
                                           #---- domain checks ----
                                           l$s$err$assert_msg(paste0("'.finished()' must return a logical of length one."),
                                                              l$s$tcs$has_length_1(finished, NA_on_fail=FALSE),
                                                              l$s$tcs$is_logical(finished,
                                                                                 accept_NULL = FALSE,
                                                                                 accept_NaN = FALSE,
                                                                                 accept_NA = FALSE))
                                           
                                           l$s$err$assert_msg(paste0("'.get_next_count()' must return +Inf or a ",
                                                                     "non-negative integer of length one."),
                                                              l$s$tcs$has_length_1(next_count, NA_on_fail=FALSE),
                                                              l$s$tcs$is_integer(next_count,
                                                                                 accept_NULL = FALSE,
                                                                                 accept_NaN = FALSE,
                                                                                 accept_NA = FALSE,
                                                                                 lower_bound = 0,
                                                                                 lower_bound_inclusive = TRUE,
                                                                                 upper_bound = Inf,
                                                                                 upper_bound_inclusive = TRUE))
                                           
                                           #---- inter-variables consistency ----
                                           
                                           l$s$err$assert_msg(paste0("'.finished()' cannot be 'TRUE' if 'get_length()' is '+Inf'."),
                                                              !finished || !is.infinite(length))
                                           
                                           l$s$err$assert_msg(paste0("If 'get_num_remaining()' is non-NA, '.get_next_count()' ",
                                                                     "cannot be bigger than the latter."),
                                                              is.na(num_remaining) || l$s$tcs$is_bounded(next_count,
                                                                                                         lower_bound = 0,
                                                                                                         lower_bound_inclusive = TRUE,
                                                                                                         upper_bound = num_remaining,
                                                                                                         upper_bound_inclusive = TRUE,
                                                                                                         NA_on_fail = FALSE))
                                           
                                           
                                           l$s$err$assert_msg(paste0("If '.finished()' returns 'TRUE', ", 
                                                                     "'get_next_count()' must return zero."),
                                                              !finished || next_count == 0)
                                           
                                           l$s$err$assert_msg(paste0("If 'get_num_remaining()' does not return 'NA', it ", 
                                                                     "must return zero if and only if '.finished()' ",
                                                                     "returns 'TRUE'."),
                                                              is.na(num_remaining) || (xor(!finished, num_remaining == 0)))
                                           
                                         }
                                       },
                                       
                                       #' __IMPORTANT__: See documentation of this method in `utils_lang_r6_subclass_TEMPLATE.R`.
                                       #'
                                       #' TODO: Test with `fixed_length = NA`.
                                       #'
                                       #' @param assertions_status If `TRUE`, assertions are activated (see `set_assertion_status()`
                                       #'   in `utils_lang_error.R`).
                                       #'   
                                       #' @param fixed_length A function without parameters or a fixed value. In case of the function,
                                       #'   the length is set as `fixed_length <- fixed_length()`. 
                                       #'   The resulting value as well as the fixed value, if provided, must be one of the following:
                                       #'   Either `NA` if the total numer of elements to be iterated over is unknown,
                                       #'   `+Inf` if this iterator never finished (thus [finished()] will always return `FALSE`)
                                       #'   or a non-negative integer indicating the total numer of elements that this iterator iterates over. 
                                       #'
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       initialize = function(assertions_status = FALSE, fixed_length){
                                         super$initialize()
                                         local_env$.add_to_static_env(super$get_static_env())
                                         local_env$static_env$err$set_assertions_status(assertions_status)
                                         
                                         #' Add your initialization code here here:
                                         if(is.function(fixed_length)){
                                           fixed_length <- fixed_length()
                                         }
                                         
                                         self$.set_fixed_length(fixed_length)
                                         self$.set_num_remaining(fixed_length)
                                         private$old_fixed_length  <- fixed_length
                                       },
                                      
                                       
                                       #' @param tab A character string of length containing zero or more whitespace characters to be
                                       #'   prependend to each line of the resulting character string.
                                       #'
                                       #' @return A human formatted, human readable character string containing
                                       #'   information about this iterator.
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       toString = function(tab = ""){
                                         l$s$err$assert_msg("'tab' must be a character of length one consisting of whitespace characters.",
                                                            l$s$tcs$has_length_1(tab, NA_on_fail = FALSE),
                                                            l$s$tcs$is_character(tab, accept_NULL = FALSE,
                                                                                 accept_NaN = FALSE,
                                                                                 accept_NA = FALSE),
                                                            grepl(pattern = "^[[:space:]]*$", x = tab))
                                         
                                         return(paste0(tab, "Iterator class: ", class(self)[[1]], "\n",
                                                       tab, "  Length: ", self$get_length(), "\n",
                                                       tab, "  Num remaining: ", self$get_num_remaining(), "\n",
                                                       tab, "  Next count: ", self$get_next_count(), "\n",
                                                       tab, "  Finished: ", self$finished()))
                                       },
                                       
                                       #' The `R6` `finalize` method. \cr
                                       #' __IMPORTANT__: Call `super$finalize()` at the beginning, 
                                       #' if you overwrite this method.
                                       #'
                                       #' @return NULL
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       finalize = function() {
                                         super$finalize()
                                       },
                                       
                                       #'
                                       #' @return The total number of objects this iterator iterates over. Might return
                                       #' `NA` if this number is unknown (e.g., if the iterator represents a data stream of unknown length)
                                       #' and `Inf` if there is an infinite number of objects (e.g., dynamically created as the iterator moves on).
                                       #' If non-`NA`, this method must always return the same value. It is allowed to switch from `NA` to a numeric
                                       #' return value that does not change afterwards though.
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       get_length = function(){
                                         private$check_lengths()
                                         return(private$fixed_length)
                                       },
                                       
                                       #'
                                       #' @return The number of objects this iterator has left to iterate over. Might return
                                       #' `NA` if this number is unknown (e.g., if the iterator represents a data stream of unknown length)
                                       #' and `Inf` if there is an infinite number of objects (e.g., dynamically created as the iterator moves on).
                                       #' Should return a non-`NA` numeric if `get_length()` returns a non-`NA` numeric.
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       get_num_remaining = function(){
                                         private$check_lengths()
                                         return(private$num_remaining)
                                       },
                                       
                                       #' Get a list of `num` elements. `num` cannot be bigger than the number of
                                       #' currently retrievable elements (as returned by [get_next_count()]).
                                       #' [validate_next(element)] must return `TRUE` for each one of the elements. 
                                       #' Otherwise, this method also produces an error.
                                       #'
                                       #' @param num The number of elements to retrieve (a non-negative, non-infinite integer
                                       #'   of length one).
                                       #'
                                       #' @return A list of `num` elements.
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       get_next = function(num = self$get_next_count()) {
                                         l$s$err$assert_msg("'num' must be non-negative, non-infinite integer of length one.",
                                                            l$s$tcs$has_length_1(num, NA_on_fail = FALSE),
                                                            l$s$tcs$is_integer(num,
                                                                               accept_NULL = FALSE,
                                                                               accept_NaN = FALSE,
                                                                               accept_NA = FALSE,
                                                                               lower_bound = 0,
                                                                               lower_bound_inclusive = TRUE,
                                                                               upper_bound = Inf,
                                                                               upper_bound_inclusive = FALSE))
                                         force(num)
                                         #' Implies call to `.check_consistency()`
                                         next_num <- self$get_next_count()
                                         
                                         l$s$err$stopifnot(paste0("Provided 'num' argument is bigger than the actual ",
                                                            "available number of elements returned by 'get_next_count()'."), 
                                                           next_num >= num)
                                         
                                         if(num == 0){
                                           return(list())
                                         }
                                         
                                         next_elems <- as.list(self$.get_next(num))
                                         l$s$err$assert_msg("'.get_next(num)' returned an incorrect number of objects.", 
                                                           length(next_elems) == num)
                                         
                                         for(next_elem in next_elems){
                                           l$s$err$assert_msg(paste0("At least one object returned by ",
                                                              "'.get_next(num)' did not pass validation."), 
                                                             private$validate_next(next_elem))
                                         }
                                         
                                         num_remaining <- self$get_num_remaining()
                                         
                                         if(!is.na(num_remaining) && !is.infinite(num_remaining)){
                                           self$.set_num_remaining(num_remaining - num)
                                         }
                                         
                                         self$.get_next_post()
                                         
                                         return(next_elems)
                                       },
                                       
                                       #' See [get_next_count()].
                                       #' 
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       get_next_count_upper_bound = function(){
                                         return(private$next_count_upper_bound)
                                       },
                                       
                                       
                                       #' Sets the value to be returned by [get_next_count_upper_bound()].
                                       #'
                                       #' @param next_count_upper_bound An integer > 0 of length 1.
                                       #'
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       set_next_count_upper_bound = function(next_count_upper_bound){
                                         l$s$err$assert_msg("'next_count_upper_bound' must be an integer > 0 of length 1.",
                                                            l$s$tcs$has_length_1(next_count_upper_bound),
                                                            l$s$tcs$is_integer(next_count_upper_bound,
                                                                               accept_NULL = FALSE,
                                                                               accept_NaN = FALSE,
                                                                               accept_NA = FALSE,
                                                                               lower_bound = 0,
                                                                               lower_bound_inclusive = FALSE,
                                                                               upper_bound = Inf,
                                                                               upper_bound_inclusive = FALSE))
                                         
                                         private$next_count_upper_bound <- next_count_upper_bound
                                       },
                                       
                                       #' This method is a wrapper for
                                       #' [.get_next_count()] that adds assertions like that its returned 
                                       #' value is a non-negative integer of length 1 and that
                                       #' either [finished()] is `FALSE` or the return value
                                       #' is zero and [get_length()] is not `+Inf`. Also it also bounds the
                                       #' return value by the value returned by [get_next_count_upper_bound()].
                                       #'
                                       #' @return The number of currently retrievalbe elements. The `num`
                                       #' argument in [get_next(num)] should never be bigger than this value.
                                       #' Note that a return value of zero does not necessarily mean that
                                       #' there won't be retrievable elements in the future - see [finished()]. 
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       get_next_count = function() {
                                         next_count <- self$.get_next_count()
                                         self$.check_consistency(next_count = next_count)
                                         return(min(next_count, private$next_count_upper_bound))
                                       },
                                       
                                       #' This method is a wrapper for
                                       #' [.finished()] that add assertions like that its returned 
                                       #' value is a logical of length 1 that is not `TRUE`
                                       #' if [get_length()] is `+Inf`.
                                       #'
                                       #' @return `TRUE` if and only if this iterator has reached its end and there
                                       #' are no more retrievable elements (thus, [get_next_count()] should always return zero
                                       #' from now on). If [get_num_remaining()] is not `NA`, it should return zero if
                                       #' and only if [.finished()] is `TRUE`.
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       finished = function() {
                                         fin <- self$.finished()
                                         self$.check_consistency(finished = fin)
                                         
                                         return(fin)
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
                                         #super$.get_next_post()
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
        
        #' An abstract decorator for [Abstract_Iterator]s. May be used to decorate the data a provided iterator
        #' iterates over.
        Abstract_Iterator_Decorator <- R6Class("Abstract_Iterator_Decorator",
                               # ---- R6 inheritance -----
                               inherit = result_env$Abstract_Iterator,
                               
                               #' TODO: ---- Adapt R6 options --------
                               portable = TRUE,
                               cloneable = TRUE,
                               lock_class = TRUE,
                               lock_objects = TRUE,
                               
                               #' ----- Add private Fields & methods ----
                               private = list(
                                 decorated_iterator = NULL,
                                 old_length = NULL,
                                 
                                 #' Called in `refresh()`.
                                 #' Checks whether the internal iterator's `get_length()`
                                 #' method switched from returning `NA` to returning
                                 #' a numerical. If so, the method calls its `.update_length`
                                 #' and `.update_num_remaining` methods in order to update
                                 #' its own length fields.
                                 #'
                                 #' @return
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 update_lengths = function(){
                                   len <- private$decorated_iterator$get_length()
                                   if(is.na(private$old_length) && !is.na(len)){
                                     self$.update_length(internal_length = len)
                                     self$.update_num_remaining(internal_num_remaining = private$decorated_iterator$get_num_remaining())
                                     private$old_length <- len
                                   }
                                 }
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

                                 #' __IMPORTANT__: See documentation of this method in `utils_lang_r6_subclass_TEMPLATE.R`.
                                 #'
                                 #' Inherits from [Abstract_Iterator] and decorates the provided 
                                 #' [Abstract_Iterator] instance `iterator` (see "decorator design pattern").
                                 #' 
                                 #' Override the `.*_decorated` methods. You may use the `.*_internal` methods 
                                 #' access the internally access the internal iterators
                                 #' corresponding methods.  Aee also 'TODO' comments.
                                 #'
                                 #' __IMPORTANT__: See documentation of this method in `utils_lang_r6_subclass_TEMPLATE.R`.
                                 #'
                                 #' Also see `.set_fixed_length()` and `.set_num_remaining()` in `Abstract_Iterator`.
                                 #'
                                 #' @param assertions_status If `TRUE`, assertions are activated (see `set_assertion_status()`
                                 #'   in `utils_lang_error.R`).
                                 #' @param iterator An `Abstract_Iterator` instance to be decorated. Note that
                                 #'   in order to avoid inconsistencies, the decorated instance should only
                                 #'   be accessed indirectly via this decorator.
                                 #'
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 initialize = function(assertions_status = FALSE, 
                                                       iterator){
                                   super$initialize(assertions_status=assertions_status,
                                                    fixed_length = (function(){
                                                      private$old_length <- iterator$get_length()
                                                      private$decorated_iterator <- iterator
                                                      return(private$old_length)
                                                    })())
                                   local_env$.add_to_static_env(super$get_static_env())
                                   local_env$static_env$err$set_assertions_status(assertions_status)
                                   
                                   #' Add your initialization code here here:
                                   l$s$err$assert_msg("'iterator' must be or inherit from 'Abstract_Iterator'.",
                                                      is.R6(iterator), inherits(iterator, "Abstract_Iterator"))
                                   self$.set_num_remaining(num_remaining = iterator$get_num_remaining())
                                 },

                                 #' The `R6` `finalize` method. \cr
                                 #' __IMPORTANT__: Call `super$finalize()` at the beginning, 
                                 #' if you overwrite this method.
                                 #'
                                 #' @return NULL
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 finalize = function() {
                                   super$finalize()
                                   
                                   #' Add your finalization code here:
                                 },
                                 
                                 toString = function(tab = ""){
                                   result <- super$toString(tab = tab)
                                   
                                   return(paste0(result, "\n",
                                                 tab, "  Decoration hierarchy: ", toString(self$get_decoration_hierarchy())))
                                 },
                                 
                                 #' @return A list of character vectors. Its first entry contains the `class(...)`
                                 #' vector of this decorator object. If this decorator's internal iterator is NOT
                                 #' itself a decorator (i.e., does not inherit from `Abstract_Iterator_Decorator`), 
                                 #' then this method simply appends this internal iterators
                                 #' `class(...)` vector to the list and returns the latter.
                                 #' If the internal iterator is a decorator itself, the method recursively calls
                                 #' the latter's [get_decoration_hierarchy()] method and appends the result.
                                 #' 
                                 #' This results in a list reflecting the classes of each decorator in a nested hierarchy
                                 #' of decorators. The last entry always corresponds to the non-decorator iterator on
                                 #' the lowest level of this herarchy.
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 get_decoration_hierarchy = function() {
                                   if(inherits(private$decorated_iterator, "Abstract_Iterator_Decorator")){
                                     return(c(list(class(self)), private$decorated_iterator$get_decoration_hierarchy()))
                                   }
                                   else{
                                     return(c(list(class(self)),list(class(private$decorated_iterator))))
                                   }
                                 },
                                 
                                 #' Provides access to the internal, non-decorated iterator.
                                 #' __IMPORTANT__: Use with caution! Externally changing the state of the internal iterator
                                 #'   might, depending on the decorator implementation, lead the latter in an
                                 #'   inconsistent state.
                                 #'
                                 #' @return The internal, non-decorated iterator (i.e., the one provided to `initialize()`).
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 .get_decorated_iterator = function(){
                                   return(private$decorated_iterator)
                                 },
                                 
                                 #' @return Internal accessor for decorated iterator's `get_next(num)` method.
                                 #' @export
                                 #' @md
                                 .get_next_internal = function(num){
                                   return(private$decorated_iterator$get_next(num))
                                 },
                                 
                                 #' @return Internal accessor for decorated iterator's `get_next_count()` method.
                                 #' @export
                                 #' @md
                                 .get_next_count_internal = function(){
                                   return(private$decorated_iterator$get_next_count())
                                 },
                                 
                                 #' @return Internal accessor for decorated iterator's `finished()` method.
                                 #' @export
                                 #' @md
                                 .finished_internal = function(){
                                   return(private$decorated_iterator$finished())
                                 },
                                 
                                 #' First calls the internal iterator's `refresh()` method
                                 #' if the latter inhertis from `Abstract_Iterator_Decorator`.
                                 #' 
                                 #' Then checks whether the internal iterator's `get_length()`
                                 #' method switched from returning `NA` to returning
                                 #' a numerical. If so, the method calls its `.update_length`
                                 #' and `.update_num_remaining` methods in order to update
                                 #' its own length fields.
                                 #'
                                 #' @return
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 refresh = function(){
                                   if(inherits(private$decorated_iterator, "Abstract_Iterator_Decorator")){
                                     private$decorated_iterator$refresh()
                                   }
                                   private$update_lengths()
                                 },
                                 
                                 #' TODO: ----------- THE FOLLOWING METHODS __MAY__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                 
                                 #' Called in `refresh()` once the internal iterator's `get_length()`
                                 #' method switched from returning `NA` to returning
                                 #' a numerical.
                                 #' Use `.set_fixed_length()` to update this iterator's corresponding field accordingly.
                                 #'
                                 #' @param internal_length The current internal iterator's
                                 #'   `get_length()` result (aquired via `.get_length_internal()`). 
                                 #'
                                 #' @return
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 .update_length = function(internal_length){
                                   self$.set_fixed_length(internal_length)
                                 },
                                 
                                 #' Called in `refresh()` once the internal iterator's `get_length()`
                                 #' method switched from returning `NA` to returning
                                 #' a numerical.
                                 #' Use `.set_num_remaining()` to update this iterator's corresponding field accordingly.
                                 #'
                                 #' @param internal_num_remaining The current internal iterator's
                                 #'   `get_num_remaining()` result (aquired via `.get_num_remaining_internal()`). 
                                 #'
                                 #' @return
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 .update_num_remaining = function(internal_num_remaining){
                                   self$.set_num_remaining(internal_num_remaining)
                                 },
                                 
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
                                 
                                 #' Use the [.get_next_internal(num)] method to access
                                 #' internal iterators `get_next(num)` method and decorate the
                                 #' returned `num` objects.
                                 #' 
                                 #' A dummy implementation not adding any functionality might, for example,
                                 #' simply consist of a `return(self$.get_next_internal(num))` statement.
                                 #' 
                                 #' @param num See original documentation of `.get_next(num)`.
                                 #' @return See original documentation of `.get_next(num)`.
                                 #' @md
                                 .get_next = function(num) {
                                   next_elems <- self$.get_next_internal(num)
                                   n <- length(next_elems)
                                   if(n > 0){
                                     for(i in 1:n){
                                       #' TODO: Decorate next_elems[[i]]
                                     }
                                   }
                                   
                                   stop("Attempt to call an 'abstract' method. 
                                        You need to extend this class and explicitly 
                                        overwrite this method.")
                                   
                                   return(next_elems)
                                 },
                                 
                                 #' At this point, it is guaranteed that the internal iterator
                                 #' has validated the provided `element`. You may therefore
                                 #' override this method in order to provide ADDITIONAL constraints. 
                                 #' 
                                 #' A dummy implementation not adding any functionality might, for example,
                                 #' simply consist of a `return(TRUE)` statement.
                                 #' 
                                 #' @param element See original documentation of `.validate_next(element)`.
                                 #' 
                                 #' @return See original documentation of `.validate_next(element)`. 
                                 #' 
                                 #' @export
                                 #' @md
                                 .validate_next = function(element){
                                   #' TODO Add constraingts
                                   
                                   stop("Attempt to call an 'abstract' method. 
                                              You need to extend this class and explicitly 
                                              overwrite this method.")
                                 },
                                 
                                 
                                 #' Use the [.get_next_count_internal()] method to access
                                 #' internal iterators `get_next_count()` method and decorate the
                                 #' returned number (e.g., if this decorator adds additional
                                 #' elements).
                                 #' 
                                 #' A dummy implementation not adding any functionality might, for example,
                                 #' simply consist of a `return(self$.get_next_count_internal())` statement.
                                 #' 
                                 #' Also see `.set_fixed_length()` and `.set_num_remaining()` in `Abstract_Iterator`.
                                 #' 
                                 #' @return See original documentation of `.get_next_count()`. 
                                 #' @export
                                 #' @md
                                 .get_next_count = function(){
                                   next_count <- self$.get_next_count_internal()
                                   
                                   #' TODO: Decorate `next_count`
                                   
                                   stop("Attempt to call an 'abstract' method. 
                                              You need to extend this class and explicitly 
                                              overwrite this method.")
                                 },
                                 
                                 #' Use the [.finished_internal()] method to access
                                 #' internal iterators `finished()` method and decorate the
                                 #' returned logical (e.g., if this decorator appends additional
                                 #' elements while the internal iterator is alread done).
                                 #' 
                                 #' A dummy implementation not adding any functionality might, for example,
                                 #' simply consist of a `return(self$.finished_internal())` statement.
                                 #' 
                                 #' Also see `.set_fixed_length()` and `.set_num_remaining()` in `Abstract_Iterator`.
                                 #' 
                                 #' @return See documentation of `.finished()`. 
                                 #' @export
                                 #' @md
                                 .finished = function(){
                                   finished <- self$.finished_internal()
                                   
                                   #' TODO: Decorate `finished`
                                   
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
        
        #' An `Abstract_Iterator_Decorator` subclass for retrieving and buffering elements in advance in order
        #' to accelerate access via `get_next()`.
        Buffer_Iterator_Decorator <- R6Class("Buffer_Iterator_Decorator",
                                               # ---- R6 inheritance -----
                                               inherit = l$result_env$Abstract_Iterator_Decorator,
                                               
                                               #' TODO: ---- Adapt R6 options --------
                                               portable = TRUE,
                                               cloneable = TRUE,
                                               lock_class = TRUE,
                                               lock_objects = TRUE,
                                               
                                               #' ----- Add private Fields & methods ----
                                               private = list(
                                                 buffer_size = NULL,
                                                 buffer_min_fill_factor = NULL,
                                                 buffer = NULL
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
                                                 
                                                 #' A buffer for pre-loading and buffering elements to be iterated over.
                                                 #' 
                                                 #' @param assertions_status  See `Abstract_Iterator_Decorator`.
                                                 #' @param iterator See `Abstract_Iterator_Decorator`.
                                                 #' @param buffer_size A non-negative integer indicating tha maximal
                                                 #'   numer of buffered elements (zero means no buffering).
                                                 #' @param buffer_min_fill_factor A numerical from the interval [0,1].
                                                 #'   If the number of buffered elements relative to the `buffer_size` is smaller 
                                                 #'   than or equal this fill factor, the buffered iterator attempts to refill it as much as possible. This
                                                 #'   check is performed at the end of each `get_next(num)` call.
                                                 #' @export
                                                 #'
                                                 #' @examples
                                                 #' @md
                                                 initialize = function(assertions_status = FALSE, 
                                                                       iterator,
                                                                       buffer_size = 50,
                                                                       buffer_min_fill_factor = 0.2){
                                                   
                                                   super$initialize(assertions_status=assertions_status,
                                                                    iterator = iterator)
                                                   local_env$.add_to_static_env(super$get_static_env())
                                                   local_env$static_env$err$set_assertions_status(assertions_status)
                                                   
                                                   #' Add your initialization code here here:
                                                   l$s$err$assert_msg("'buffer_size' must be a non-negative integer of length one.",
                                                                      l$s$tcs$has_length_1(buffer_size, NA_on_fail = FALSE),
                                                                      l$s$tcs$is_integer(buffer_size,
                                                                                         accept_NULL = FALSE,
                                                                                         accept_NaN = FALSE,
                                                                                         accept_NA = FALSE,
                                                                                         lower_bound = 0,
                                                                                         lower_bound_inclusive = TRUE,
                                                                                         upper_bound = Inf,
                                                                                         upper_bound_inclusive = FALSE))
                                                   private$buffer_size <- buffer_size
                                                   
                                                   l$s$err$assert_msg(paste0("'buffer_min_fill_factor' must be a numeric of length one in ",
                                                                             "the interval [0,1]."),
                                                                      l$s$tcs$has_length_1(buffer_min_fill_factor, NA_on_fail = FALSE),
                                                                      l$s$tcs$is_numeric(buffer_min_fill_factor,
                                                                                         accept_NULL = FALSE,
                                                                                         accept_NaN = FALSE,
                                                                                         accept_NA = FALSE,
                                                                                         lower_bound = 0,
                                                                                         lower_bound_inclusive = TRUE,
                                                                                         upper_bound = 1,
                                                                                         upper_bound_inclusive = TRUE,
                                                                                         accept_non_integer = TRUE))
                                                   private$buffer_min_fill_factor <- buffer_min_fill_factor
                                                   
                                                   private$buffer <- queue()
                                                 },
                                                 
                                                 finalize = function() {
                                                   super$finalize()
                                                   
                                                   #' Add your finalization code here:
                                                 },
                                                 
                                                 #' Accessor to the buffer size.
                                                 #'
                                                 #' @return The maximal number of elements the internal buffer can store.
                                                 #' @export
                                                 #'
                                                 #' @examples
                                                 #' @md
                                                 get_buffer_size = function(){
                                                   return(private$buffer_size)
                                                 },
                                                 
                                                 #' Accessor to the current number of buffered elements.
                                                 #'
                                                 #' @return The current number of buffered elements.
                                                 #' @export
                                                 #'
                                                 #' @examples
                                                 #' @md
                                                 get_buffer_length = function(){
                                                   return(length(private$buffer))
                                                 },
                                                 
                                                 #' Acessor to the minimal buffer fill level.
                                                 #'
                                                 #' @return The minimal buffer fill level.
                                                 #' If the number of buffered elements relative to the `buffer_size` is smaller than
                                                 #' this fill factor, the buffered iterator attempts to refill it as much as possible. This
                                                 #' check is performed at the end of each `get_next(num)` call.
                                                 #' @export
                                                 #'
                                                 #' @examples
                                                 get_buffer_min_fill_factor = function(){
                                                   return(private$buffer_min_fill_factor)
                                                 },
                                                 
                                                 #' ----------- THE FOLLOWING METHODS __MAY__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                                 
                                                 .update_num_remaining = function(internal_num_remaining){
                                                   self$.set_num_remaining(internal_num_remaining + self$get_buffer_length())
                                                 },
                                                 
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
                                                 
                                                 #'  ----------- THE FOLLOWING METHODS __MUST__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                                 
                                                 .check_consistency = function(finished = self$.finished(), 
                                                                               next_count = self$.get_next_count()){
                                                   super$.check_consistency(finished = finished,
                                                                            next_count = next_count)
                                                   
                                                   if(l$s$err$get_assertions_status()){
                                                     #' Add additional assertions for consistency checks, if required and call
                                                     #'   this method at appropriate points.  this method at appropriate points.
                                                     
                                                     l$s$err$assert_msg("'buffer' contains more entries than it should.",
                                                                        length(private$buffer) <= self$get_buffer_size())
                                                     
                                                     l$s$err$assert_msg(paste0("'get_next_count()' should equal 'get_buffer_length() ",
                                                                               "+ .get_next_count_internal()'."),
                                                                        self$.get_next_count() == 
                                                                          self$get_buffer_length() + self$.get_next_count_internal())
                                                   }
                                                 },
                                                 
                                                 .get_next = function(num) {
                                                   if(num == 0 || self$get_buffer_size() == 0){
                                                     return(self$.get_next_internal(num))
                                                   }
                                                   else {
                                                     result <- vector(length=num, mode="list")
                                                     
                                                     buf_len <- self$get_buffer_length()
                                                     missing_num <- num
                                                     retrievable <- min(buf_len,num)
                                                     
                                                     if(retrievable > 0){
                                                       for(i in 1:retrievable){
                                                         result[[i]] <- pop(private$buffer)
                                                       }
                                                     }
                                                     
                                                     missing_num <- missing_num - retrievable
                                                     
                                                     if(missing_num > 0){ # implies that buffer is empty
                                                       next_count_internal <- self$.get_next_count_internal()
                                                       l$s$err$assert_msg(paste0("Inconsistency between 'get_next_count()' ",
                                                                               "and '.get_next_count_internal()'."),
                                                                         next_count_internal == self$get_next_count())
                                                       
                                                       retrievable <- min(missing_num + self$get_buffer_size(), next_count_internal)
                                                       
                                                       l$s$err$assert_msg(paste0("Inconsistency between 'get_next_count()' ",
                                                                                 "and provided 'num' argument."),
                                                                          retrievable >= missing_num)
                                                       
                                                       next_elems <- self$.get_next_internal(retrievable)
                                                       
                                                       l$s$err$assert_msg(paste0("'.get_next_internal()' did return wrong number ",
                                                                                 "of elements."),
                                                                          length(next_elems) == retrievable)
                                                       
                                                       result[(num - missing_num + 1):num] <- next_elems[1:missing_num]
                                                       
                                                       if(missing_num < retrievable){
                                                         for(i in (missing_num+1):retrievable){
                                                           pushback(private$buffer, next_elems[[i]])
                                                         }
                                                       }
                                                     }
                                                     else{ # result is already full - see if buffer needs refilling
                                                       buf_len <- self$get_buffer_length()
                                                       buf_size <- self$get_buffer_size()
                                                       fill_factor <- buf_len / buf_size # buffer_size > 0 at this point!
                                                       if(fill_factor <= self$get_buffer_min_fill_factor() && fill_factor < 1){
                                                         
                                                         next_count_internal <- self$.get_next_count_internal()
                                                         retrievable <- min(buf_size - buf_len, next_count_internal)
                                                         
                                                         if(retrievable > 0){
                                                           next_elems <- self$.get_next_internal(retrievable)
                                                           l$s$err$assert_msg(paste0("'.get_next_internal()' did return wrong number ",
                                                                                     "of elements."),
                                                                              length(next_elems) == retrievable)
                                                           
                                                           for(i in 1:retrievable){
                                                             pushback(private$buffer, next_elems[[i]])
                                                           }
                                                         }
                                                       }
                                                     }
                                                   }
                                                   
                                                   return(result)
                                                 },
                                                 
                                                 .validate_next = function(element){
                                                   return(TRUE)
                                                 },
                                                 
                                                 .get_next_count = function(){
                                                   next_count_internal <- self$.get_next_count_internal()
                                                   
                                                   return(next_count_internal + self$get_buffer_length())
                                                 },
                                                 
                                                 .finished = function(){
                                                   finished <- self$.finished_internal()
                                                   
                                                   return(finished && self$get_buffer_length() == 0)
                                                 }
                                               ),
                                               #-----end of public-----
                                               
                                               #' ----- Add active bindings ----
                                               active = list(
                                               )
        )
        
        #' An `Abstract_Iterator_Decorator` subclass applying a `T*x + b` type of 
        #' linear transformation for each retrieved element `x`.
        Linear_Transform_Iterator_Decorator <- R6Class("Linear_Transform_Iterator_Decorator",
                                            # ---- R6 inheritance -----
                                            inherit = l$result_env$Abstract_Iterator_Decorator,
                                            
                                            #' TODO: ---- Adapt R6 options --------
                                            portable = TRUE,
                                            cloneable = TRUE,
                                            lock_class = TRUE,
                                            lock_objects = TRUE,
                                            
                                            #' ----- Add private Fields & methods ----
                                            private = list(
                                              param_env = NULL,
                                              mult_op = NULL,
                                              sum_op = NULL
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
                                                                                                                                                                                       #' 
                                              #' The linear transformation decorator performs
                                              #' a `z = T*x + b` type of linear transformation for each retrieved element `x`.
                                              #' before returning it.
                                              #'
                                              #' @param assertions_status  See `Abstract_Iterator_Decorator`.
                                              #' @param iterator See `Abstract_Iterator_Decorator`.
                                              #' @param transformation_matrix If `transformation_matrix` is a function, 
                                              #'   the matrix `T` is reset to `T=transformation_matrix()` each time a new
                                              #'   element `x` is retrieved. Otherwise it is set to `T=transformation_matrix`.
                                              #'   This enables the use of a transformation matrix that changes over time.
                                              #' @param offset If `offset` is a function, the vector `b` is reset
                                              #'   to `b=offset()` each time a new element `x` is retrieved. 
                                              #'   Otherwise it is set to `T=b`.
                                              #'   This enables the use of an offset vector that changes over time.
                                              #' @param multiplication_operator `y = T*x` will be computed as `y = multiplication_operator(T,x)`.
                                              #'   Default is `%m%` from `utils_math_general.R`. 
                                              #' @param summation_operator `y + b` will be computed as `z = summation_operator(y,b)`.
                                              #'   Default is the standard summation operator `+`.
                                              #' 
                                              #' @export
                                              #'
                                              #' @examples
                                              #' @md
                                              initialize = function(assertions_status = FALSE, 
                                                                    iterator,
                                                                    transformation_matrix,
                                                                    offset,
                                                                    multiplication_operator = l$s$mg$`%m%`,
                                                                    summation_operator = `+`){
                                                super$initialize(assertions_status=assertions_status,
                                                                 iterator = iterator)
                                                local_env$.add_to_static_env(super$get_static_env())
                                                local_env$static_env$err$set_assertions_status(assertions_status)
                                                
                                                #' Add your initialization code here here:
                                                l$s$err$assert_msg(paste0("'multiplication_operator' and 'summation_operator' ",
                                                                          "must be functions.",
                                                                          is.function(multiplication_operator),
                                                                          is.function(summation_operator)))
                                                private$param_env <- new.env()
                                                
                                                if(is.function(transformation_matrix)){
                                                  makeActiveBinding("trans_mat", fun = transformation_matrix, env=private$param_env)
                                                }
                                                else{
                                                  assign("trans_mat", transformation_matrix, envir = private$param_env)
                                                }
                                                
                                                if(is.function(offset)){
                                                  makeActiveBinding("offset", fun = offset, env=private$param_env)
                                                }
                                                else{
                                                  assign("offset", offset, env=private$param_env)
                                                }
                                                
                                                private$mult_op <- multiplication_operator
                                                private$sum_op <- summation_operator
                                              },
                                              
                                              finalize = function() {
                                                super$finalize()
                                                
                                                #' Add your finalization code here:
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
                                              
                                              .get_next = function(num) {
                                                next_elems <- self$.get_next_internal(num)
                                                n_elems <- length(next_elems)
                                                l$s$err$assert_msg(paste0("'.get_next_internal()' did return wrong number ",
                                                                          "of elements."),
                                                                   n_elems == num)
                                                if(n_elems > 0){
                                                  for(i in 1:n_elems){
                                                    next_elems[[i]] <- private$sum_op(private$mult_op(private$param_env$trans_mat, 
                                                                                                      next_elems[[i]]),
                                                                                      private$param_env$offset)
                                                  }
                                                }
                                                return(next_elems)
                                              },
                                              
                                              .validate_next = function(element){
                                                return(TRUE)
                                              },
                                              
                                              .get_next_count = function(){
                                                return(self$.get_next_count_internal())
                                              },
                                              
                                              .finished = function(){
                                                finished <- self$.finished_internal()
                                                return(finished)
                                              }
                                            ),
                                            #-----end of public-----
                                            
                                            #' ----- Add active bindings ----
                                            active = list(
                                              
                                            )
                                            #-----end of active-----
        )
        
        #' A `Linear_Transform_Iterator_Decorator` subclass applying an `x + b * eps` 
        #' linear transformation for each retrieved element `x` where `eps` is drawn 
        #' from a normal distribution and `b` is randomly drawn from `{-1,1}`.
        Noise_Iterator_Decorator <- R6Class("Noise_Iterator_Decorator",
                                            # ---- R6 inheritance -----
                                            inherit = l$result_env$Linear_Transform_Iterator_Decorator,
                                            
                                            #' TODO: ---- Adapt R6 options --------
                                            portable = TRUE,
                                            cloneable = TRUE,
                                            lock_class = TRUE,
                                            lock_objects = TRUE,
                                            
                                            
                                            
                                            #' ----- Add private Fields & methods ----
                                            private = list(
                                              rnd_seed = NULL,
                                              last_rnd_seed = NULL,
                                              
                                              create_noise_function = function(noise_mean, noise_stddev, positive_prob, rnd_seed){
                                                l$s$err$assert_msg("'noise_mean' must be a numeric of length one.",
                                                                   l$s$tcs$has_length_1(noise_mean, NA_on_fail = FALSE),
                                                                   l$s$tcs$is_numeric(noise_mean,
                                                                                      accept_NULL = FALSE,
                                                                                      accept_NaN = FALSE,
                                                                                      accept_NA = FALSE,
                                                                                      lower_bound = -Inf,
                                                                                      lower_bound_inclusive = FALSE,
                                                                                      upper_bound = Inf,
                                                                                      upper_bound_inclusive = FALSE,
                                                                                      accept_non_integer = TRUE))
                                                
                                                l$s$err$assert_msg("'noise_stddev' must be a non-negative numeric of length one.",
                                                                   l$s$tcs$has_length_1(noise_stddev, NA_on_fail = FALSE),
                                                                   l$s$tcs$is_numeric(noise_stddev,
                                                                                      accept_NULL = FALSE,
                                                                                      accept_NaN = FALSE,
                                                                                      accept_NA = FALSE,
                                                                                      lower_bound = 0,
                                                                                      lower_bound_inclusive = TRUE,
                                                                                      upper_bound = Inf,
                                                                                      upper_bound_inclusive = FALSE,
                                                                                      accept_non_integer = TRUE))
                                                
                                                l$s$err$assert_msg("'positive_prob' must be a numeric in [0,1].",
                                                                   l$s$tcs$has_length_1(positive_prob, NA_on_fail = FALSE),
                                                                   l$s$tcs$is_numeric(positive_prob,
                                                                                      accept_NULL = FALSE,
                                                                                      accept_NaN = FALSE,
                                                                                      accept_NA = FALSE,
                                                                                      lower_bound = 0,
                                                                                      lower_bound_inclusive = TRUE,
                                                                                      upper_bound = 1,
                                                                                      upper_bound_inclusive = TRUE,
                                                                                      accept_non_integer = TRUE))
                                                l$s$err$assert_msg("'rnd_seed' must be NULL or a finite integer of length one.",
                                                                   l$s$mg$`%then%`(!is.null(rnd_seed), 
                                                                                   l$s$tcs$has_length_1(rnd_seed, NA_on_fail = FALSE)),
                                                                   l$s$mg$`%then%`(!is.null(rnd_seed), 
                                                                                   l$s$tcs$is_integer(rnd_seed,
                                                                                                      accept_NULL = FALSE,
                                                                                                      accept_NaN = FALSE,
                                                                                                      accept_NA = FALSE,
                                                                                                      lower_bound = -Inf,
                                                                                                      lower_bound_inclusive = FALSE,
                                                                                                      upper_bound = Inf,
                                                                                                      upper_bound_inclusive = FALSE)))
                                                private$rnd_seed <- rnd_seed
                                                
                                                result <- function(){
                                                  if(!is.null(private$rnd_seed)){
                                                    glob_env <- globalenv()
                                                    old_rnd_state <- get(".Random.seed", envir = glob_env)
                                                    if(is.null(private$last_rnd_seed)){
                                                      set.seed(private$rnd_seed)
                                                    }
                                                    else{
                                                      assign(".Random.seed", value = private$last_rnd_seed, envir = glob_env)
                                                    }
                                                  }
                                                  
                                                  eps <- sample(c(-1,1), 1, prob= c(1-positive_prob, positive_prob)) * 
                                                    rnorm(1,noise_mean, noise_stddev)
                                                  
                                                  if(!is.null(private$rnd_seed)){
                                                    private$last_rnd_seed <<- get(".Random.seed", envir = glob_env)
                                                    assign(".Random.seed", value = old_rnd_state, envir = glob_env)
                                                  }
                                                  return(eps)
                                                }
                                                
                                                return(result)
                                              }
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
                                                 #' TODO Remove positive_prob
                                                 #' The noise decorator that performs an `x + b * eps` 
                                                 #' linear transformation for each retrieved element `x` where `eps` is 
                                                 #' drawn from a normal distribution with mean `noise_mean`
                                                 #' and standard deviation `std_dev` and `b` is
                                                 #' set to `1` with probability `positive_prob` and to `-1`
                                                 #' with probability `(1 - positive_prob)`.
                                                 #'
                                                 #' @param assertions_status  See `Abstract_Iterator_Decorator`.
                                                 #' @param iterator See `Abstract_Iterator_Decorator`.
                                                 #' @param noise_mean `eps` will be drawn from a normal distribution with `noise_mean`
                                                 #'   as mean. Must be a numeric of length one. 
                                                 #'   Default is `0`.
                                                 #' @param noise_stddev `eps` will be drawn from a normal distribution with `noise_stddev`
                                                 #'   as standard deviation. Must be a non-negative numeric of length one.
                                                 #' @param positive_prob `b` is set to `1` with probability `positive_prob` and to `-1`
                                                 #'    with probability `(1 - positive_prob)`. Must be a numeric in `[0,1]`.
                                                 #'    Default is `1`.
                                                 #' @param summation_operator `x + b*eps` will be computed as `z = summation_operator(x,b*eps)`.
                                                 #'   Default is the standard summation operator `+`.
                                                 #' 
                                                 #' @export
                                                 #'
                                                 #' @examples
                                                 #' @md
                                                 initialize = function(assertions_status = FALSE, 
                                                                       iterator,
                                                                       noise_mean = 0,
                                                                       noise_stddev,
                                                                       positive_prob = 1,
                                                                       summation_operator = `+`,
                                                                       rnd_seed = NULL){
                                                   
                                                   super$initialize(assertions_status=assertions_status,
                                                                    iterator = iterator,
                                                                    transformation_matrix=1,
                                                                    offset = private$create_noise_function(noise_mean, noise_stddev, positive_prob, rnd_seed),
                                                                    multiplication_operator = function(x,y){
                                                                      l$s$err$assert_msg("'x' should be one at this point.",
                                                                                         l$s$dts$equals(x,1))
                                                                      return(y)
                                                                    },
                                                                    summation_operator = summation_operator)
                                                   
                                                   local_env$.add_to_static_env(super$get_static_env())
                                                   local_env$static_env$err$set_assertions_status(assertions_status)
                                                   
                                                   
                                                 },
                                                 
                                                 finalize = function() {
                                                   super$finalize()
                                                   
                                                   #' Add your finalization code here:
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
                                                   
                                                 }
                                            ),
                                            #-----end of public-----
                                            
                                            #' ----- Add active bindings ----
                                            active = list(
                                              
                                            )
                                            #-----end of active-----
        )
        
       #' An `Abstract_Iterator_Decorator` subclass for appending an annotation to each element `x`
        #' retrieved by the internal iterator.
        Annotation_Iterator_Decorator <- R6Class("Annotation_Iterator_Decorator",
                                                # ---- R6 inheritance -----
                                                inherit = l$result_env$Abstract_Iterator_Decorator,
                                                
                                                #' TODO: ---- Adapt R6 options --------
                                                portable = TRUE,
                                                cloneable = TRUE,
                                                lock_class = TRUE,
                                                lock_objects = TRUE,
                                                
                                                #' ----- Add private Fields & methods ----
                                                private = list(
                                                  annotator_env = NULL,
                                                  annotation_name = NULL,
                                                  annotate = NULL
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
                                                  
                                                  
                                                  #' The annotation iterator adds an annotation `a` to each
                                                  #' element `x` retrieved from the internal iterator. 
                                                  #' 
                                                  #' Let `list(x_1, ..., x_num)` be `num` retrieved elements from the internal iterator's
                                                  #' `get_next()` method
                                                  #' and `a_1, ..., a_num` be the respective annotations. Then this decorator returns
                                                  #' `list(list(<annotation_name>=a_1, "data"=x_1), ..., list(<annotation_name>=a_num, "data"=x_num))`. 
                                                  #'
                                                  #' @param assertions_status  See `Abstract_Iterator_Decorator`.
                                                  #' @param iterator See `Abstract_Iterator_Decorator`.
                                                  #' @param annotate A logical of length one. If `TRUE` (by default),
                                                  #'   the annotation procedure is performed as described above. If `FALSE`,
                                                  #'   `annotation_name` and `annotation` are ignored and no annotation is performed
                                                  #'   (i.e., the `get_next()` method returns `list(x_1, ..., x_num)`).
                                                  #'   This might be used by subclasses that annotate only on certain, constructor provided
                                                  #'   conditions (see, for example, `Delay_Iterator_Decorator`).
                                                  #' @param annotation_name The name to be used for the list entry of 
                                                  #'   the annotation `a`. Must be a character of length 1 or `NULL`.
                                                  #' @param annotation If `annotation` is a function, 
                                                  #'   the annotation `a` is set to `a=annotation()` each time a new
                                                  #'   element `x` is retrieved. Otherwise it is set to `a=annotation`.
                                                  #'   This enables the use of annoatations that changes over time
                                                  #'   (e.g., an incremented ID).
                                                  #' @export
                                                  #'
                                                  #' @examples
                                                  #' @md
                                                  initialize = function(assertions_status = FALSE, 
                                                                        iterator,
                                                                        annotate = TRUE,
                                                                        annotation_name="annotation",
                                                                        annotation){
                                                    super$initialize(assertions_status=assertions_status,
                                                                     iterator = iterator)
                                                    local_env$.add_to_static_env(super$get_static_env())
                                                    local_env$static_env$err$set_assertions_status(assertions_status)
                                                    
                                                    #' Add your initialization code here here:
                                                    l$s$err$assert_msg("'annotate' must be a logical of lenth one.",
                                                                       l$s$tcs$has_length_1(annotate, NA_on_fail = FALSE),
                                                                       l$s$tcs$is_logical(annotate,
                                                                                          accept_NULL = FALSE,
                                                                                          accept_NaN = FALSE,
                                                                                          accept_NA = FALSE))
                                                    private$annotate <- annotate
                                                    if(annotate){
                                                      private$annotator_env <- new.env()
                                                      
                                                      l$s$err$assert_msg("'annotation_name' must be a character of length 1 or 'NULL'.",
                                                                         is.null(annotation_name) ||
                                                                           l$s$tcs$has_length_1(annotation_name, NA_on_fail = FALSE) &&
                                                                           l$s$tcs$is_character(annotation_name,
                                                                                                accept_NULL = FALSE,
                                                                                                accept_NaN = FALSE,
                                                                                                accept_NA = FALSE))
                                                      private$annotation_name <- annotation_name
                                                      
                                                      if(is.function(annotation)){
                                                        makeActiveBinding("annotation", fun = annotation, env=private$annotator_env)
                                                      }
                                                      else{
                                                        assign("annotation", annotation, envir = private$annotator_env)
                                                      }
                                                    }
                                                  },
                                                  
                                                  finalize = function() {
                                                    super$finalize()
                                                    
                                                    #' Add your finalization code here:
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
                                                  
                                                  .get_next = function(num) {
                                                    next_elems <- self$.get_next_internal(num)
                                                    l$s$err$assert_msg(paste0("'.get_next_internal()' did return wrong number ",
                                                                              "of elements."),
                                                                       length(next_elems) == num)
                                                    
                                                    if(private$annotate){
                                                      next_elems <- lapply(next_elems,
                                                                           FUN = function(elem){
                                                                             result <- list(private$annotator_env$annotation, elem)
                                                                             names(result) <- list(private$annotation_name, "data")
                                                                             return(result)
                                                                           })
                                                    }
                                                    return(next_elems)
                                                  },
                                                  
                                                  .validate_next = function(element){
                                                    return(TRUE)
                                                  },
                                                  
                                                  .get_next_count = function(){
                                                    return(self$.get_next_count_internal())
                                                  },
                                                  
                                                  .finished = function(){
                                                    finished <- self$.finished_internal()
                                                    return(finished)
                                                  }
                                                ),
                                                #-----end of public-----
                                                
                                                #' ----- Add active bindings ----
                                                active = list(
                                                  
                                                )
                                                #-----end of active-----
        )
        
        #' An `Abstract_Iterator_Decorator` subclass adding a timestamp `t` to each element `x`
        #' retrieved by the internal iterator.
        Timestamp_Iterator_Decorator <- R6Class("Timestamp_Iterator_Decorator",
                                                 # ---- R6 inheritance -----
                                                 inherit = l$result_env$Annotation_Iterator_Decorator,
                                                 
                                                 #' TODO: ---- Adapt R6 options --------
                                                 portable = TRUE,
                                                 cloneable = TRUE,
                                                 lock_class = TRUE,
                                                 lock_objects = TRUE,
                                                 
                                                 #' ----- Add private Fields & methods ----
                                                 private = list(
                                                   timestamp_env = NULL,
                                                   current_timestamp = NULL,
                                                   
                                                   get_timestamp = function(){
                                                     return(private$current_timestamp)
                                                   },
                                                   
                                                   update_timestamp = function(){
                                                     private$current_timestamp <- l$s$t$get_unix_time()
                                                   }  
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
                                                   
                                                   
                                                   #' The timestamp iterator simply adds the retrieval time
                                                   #' (obtained by `get_unix_time` from `utils_time.R`) to the
                                                   #' retrieved element. 
                                                   #' 
                                                   #' Let `list(x_1, ..., x_num)` be the retrieved elements from the internal iterator's
                                                   #' `get_next(num)` method
                                                   #' and `t` be the time of retrieval (i.e., the call to `get_next(num)`. Then this decorator returns
                                                   #' `list(list(<timestamp_name>=t, "data"=x_1), ..., list(<timestamp_name>=t, "data"=x_num))`.
                                                   #' 
                                                   #' Note that this implies that if you retrieve `num` objects at once in `get_next()`,
                                                   #' they all have the same timestamp.
                                                   #'
                                                   #' @param assertions_status  See `Abstract_Iterator_Decorator`.
                                                   #' @param timestamp_name The name to be used for the list entry of 
                                                   #'   the timestamp `t`. Must be a character of length one or `NULL`.
                                                   #' @param iterator See `Abstract_Iterator_Decorator`.
                                                   #' 
                                                   #' @export
                                                   #'
                                                   #' @examples
                                                   #' @md
                                                   initialize = function(assertions_status = FALSE, 
                                                                         iterator,
                                                                         timestamp_name = "time"){
                                                     super$initialize(assertions_status=assertions_status,
                                                                      iterator = iterator,
                                                                      annotate = TRUE,
                                                                      annotation_name = timestamp_name,
                                                                      annotation = private$get_timestamp)
                                                     local_env$.add_to_static_env(super$get_static_env())
                                                     local_env$static_env$err$set_assertions_status(assertions_status)
                                                     
                                                   },
                                                   
                                                   finalize = function() {
                                                     super$finalize()
                                                     
                                                     #' Add your finalization code here:
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
                                                   
                                                   .get_next = function(num) {
                                                     private$update_timestamp()
                                                     
                                                     return(super$.get_next(num))
                                                   },
                                                   
                                                   .validate_next = function(element){
                                                     return(TRUE)
                                                   },
                                                   
                                                   .get_next_count = function(){
                                                     return(self$.get_next_count_internal())
                                                   },
                                                   
                                                   .finished = function(){
                                                     finished <- self$.finished_internal()
                                                     return(finished)
                                                   }
                                                 ),
                                                 #-----end of public-----
                                                 
                                                 #' ----- Add active bindings ----
                                                 active = list(
                                                   
                                                 )
                                                 #-----end of active-----
        )
        
        #' An `Abstract_Iterator_Decorator` subclass enforcing a temporal delay between element retrieval
        #' and, optionally, annotating elements with the time of becoming available.
        Delay_Iterator_Decorator <- R6Class("Delay_Iterator_Decorator",
                                            # ---- R6 inheritance -----
                                            inherit = l$result_env$Annotation_Iterator_Decorator,
                                            
                                            #' TODO: ---- Adapt R6 options --------
                                            portable = TRUE,
                                            cloneable = TRUE,
                                            lock_class = TRUE,
                                            lock_objects = TRUE,
                                            
                                            #' ----- Add private Fields & methods ----
                                            private = list(
                                              delay_mean = NULL,
                                              delay_stddev = NULL,
                                              delay_min = NULL,
                                              delay_max = NULL,
                                              
                                              #' Time last element became available (i.e. `t`)
                                              time_last_next = NULL,
                                              #' Number of available elements
                                              next_count = NULL,
                                              #' First element retrieved?
                                              timer_started = NULL,
                                              #' Next delay time
                                              next_delta_t = NULL,
                                              
                                              annotate_with_time = NULL,
                                              annotation_time_start = NULL,
                                              first_timestamp = NULL,
                                              timestamps = NULL,
                                              
                                              max_num_timestamps = NULL,
                                              
                                              rnd_seed = NULL,
                                              last_rnd_seed = NULL,
                                              
                                              get_timestamp = function(){
                                                l$s$err$assert_msg("'timestamps' should not be empty at this point.",
                                                                   length(private$timestamps) > 0)
                                                result <- private$timestamps[[1]]
                                                
                                                private$timestamps[[1]] <- NULL
                                                return(result)
                                              },
                                              
                                              #' Get next delay duration `d_t_i`
                                              .get_next_delta_t = function(){
                                                if(!is.null(private$rnd_seed)){
                                                  glob_env <- globalenv()
                                                  old_rnd_state <- get(".Random.seed", envir = glob_env)
                                                  if(is.null(private$last_rnd_seed)){
                                                    set.seed(private$rnd_seed)
                                                  }
                                                  else{
                                                    assign(".Random.seed", value = private$last_rnd_seed, envir = glob_env)
                                                  }
                                                }
                                                
                                                result <- max(0, rnorm(1, private$delay_mean, private$delay_stddev))
                                                if(!is.null(private$delay_min)){
                                                  result <- max(result, private$delay_min)
                                                }
                                                if(!is.null(private$delay_max)){
                                                  result <- min(result, private$delay_max)
                                                }
                                                
                                                if(!is.null(private$rnd_seed)){
                                                  private$last_rnd_seed <- get(".Random.seed", envir = glob_env)
                                                  assign(".Random.seed", value = old_rnd_state, envir = glob_env)
                                                }
                                                
                                                return(result)
                                              },
                                              
                                              #' Update `time_last_next = max_l {t + d_t_1 + ... + d_t_l <= t_c`}
                                              #' and `next_count = min(n,l)`.
                                              .update_fields = function(){
                                                if(!private$timer_started){
                                                  private$time_last_next <- l$s$t$get_unix_time()
                                                  private$timer_started <- TRUE
                                                }
                                                
                                                n <- self$.get_next_count_internal()
                                                t_c <- l$s$t$get_unix_time()
                                                
                                                while(private$next_count < n &&
                                                      t_c >= private$time_last_next + private$next_delta_t &&
                                                      private$next_count < private$max_num_timestamps){
                                                  private$next_count <- private$next_count + 1
                                                  private$time_last_next <-  private$time_last_next + private$next_delta_t
                                                  private$next_delta_t <- private$.get_next_delta_t()
                                                  
                                                  if(is.null(private$annotation_time_start)){
                                                    timestamp <-  private$time_last_next
                                                  }
                                                  else{
                                                    if(is.null(private$first_timestamp)){
                                                      private$first_timestamp <- private$time_last_next
                                                    }
                                                    
                                                    timestamp <- private$time_last_next - private$first_timestamp + private$annotation_time_start
                                                  }
                                                  private$timestamps <- append(private$timestamps, timestamp)
                                                }
                                              }
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
                                              
                                              #' The delayed iterator adds a temporal delay for each new element to become
                                              #' available via `get_next()`.
                                              #'
                                              #' Let `t` be `max({time last element was retrieved, time of first call to get_next, time of first call to get_next_count})`,
                                              #' let the current time be `t_c`
                                              #' and `d_t_1, ..., d_t_i, ...` be a sequence of values `max(0,n_i)` 
                                              #' where the `n_i` are independently
                                              #' drawn from a normal distribution with the `delay_mean` as mean 
                                              #' and `delay_stddev` as standard deviation. If `delay_min` is provided,
                                              #' `n_i` is then set to `n_i = max(n_i, delay_min)`. If `delay_max` is provided,
                                              #' `n_i` is then set to `n_i = min(n_i, delay_max)`.
                                              #' 
                                              #' Let `n` be the current number of internally available elements (as returned by the
                                              #' internal iterator's `get_next_count()` method).
                                              #' Then this delayed iterator's `get_next_count()` returns `min(n, l)` with `l` being
                                              #' the biggest integer such that `t + d_t_1 + ... + d_t_l <= t_c`.
                                              #' 
                                              #' If `annotate_with_time` is `TRUE`, each new element is annotated with the time it became
                                              #' available. I.e., let `list(x_1, ..., x_num)` be the `num` values retrieved via 
                                              #' the internal iterator's `get_next(num)` method and
                                              #' `t_1, ..., t_num` be the times (linux time in millisecond - see `get_unix_time()`
                                              #' in `utils_time.R`) each of those values became available (i.e., `t_j = t+ d_t_1 + ... d_t_j`).
                                              #'  Then this iterator returns 
                                              #'  `list(list(<timestamp_name> = t'_1, "data" = x_1), ..., list(<timestamp_name> = t'_num, "data" = x_num))`.
                                              #'  `t'_j` is set as follows:
                                              #'    * If `annotation_time_start` is `NULL`: `t'_j = t_j`
                                              #'    * Othwerwise let `t(1)` be the unix time the __first__ element ever retrieved by 
                                              #'      this iterator became available. Then: `t'_j = t_j - t(1) + annotation_time_start`.
                                              #'      This means that the times are shifted by `- t(1) + annotation_time_start`
                                              #'      so that the time annotation `t'(1)` of the fist element ever retrieved is
                                              #'      `t'(1) = t(1) - t(1) + annotation_time_start = annotation_time_start`.
                                              #'      For example, you may set `annotation_time_start = 0` in order to start the
                                              #'      time annotations at zero.
                                              #' 
                                              #' @param assertions_status  See `Abstract_Iterator_Decorator`.
                                              #' @param iterator See `Abstract_Iterator_Decorator`.
                                              #' @param delay_mean The delay mean in milliseconds.
                                              #' @param delay_stddev The delay standard deviation in milliseconds.
                                              #' @param annotate_with_time A logical of length one.
                                              #' @param annotation_time_start Either `NULL` or a finite numeric of length one.
                                              #' @param timestamp_name The name to be used for the list entry of 
                                              #'   the timestamp `t`. Must be a character of length one or `NULL`.
                                              #' @param delay_min Either `NULL` or a non-negative numeric of length one. Represents
                                              #'   a lower bound for the delays.
                                              #' @param delay_max Either `NULL` or a non-negative numeric of length one that is
                                              #'   `>= delay_min` if the latter is provided. Represents an upper bound for the delays.
                                              #' @param max_num_timestamps Either `Inf` or a positive (i.e. `>= 1`) integer of length one.
                                              #'   This iterator precomputes and stores the timestamps
                                              #'   of elements, that are currently retrievable according to the documentation above.
                                              #'   For small `delay_means` and a low retrieval rate (i.e., the rate at which
                                              #'   new elements are retrieved via `get_next()`), this number of timestamps
                                              #'   might become very large and thus occupy an unnecessarily large amount of RAM.
                                              #'   `max_num_timestamps` might be used to restrict the number of stored
                                              #'   timestamps (note that `get_next_count()` will always return a number `<= max_num_timestamps`).
                                              #' 
                                              #' @export
                                              #'
                                              #' @examples
                                              #' @md
                                              initialize = function(assertions_status = FALSE, 
                                                                    iterator,
                                                                    delay_mean = 100,
                                                                    delay_stddev = 10,
                                                                    delay_min = 1,
                                                                    delay_max = NULL,
                                                                    annotate_with_time = FALSE,
                                                                    annotation_time_start = NULL,
                                                                    timestamp_name = "time",
                                                                    max_num_timestamps = 1000,
                                                                    rnd_seed = NULL){
                                                
                                                super$initialize(assertions_status=assertions_status,
                                                                 iterator = iterator,
                                                                 annotate = annotate_with_time,
                                                                 annotation_name = if(isTRUE(annotate_with_time)) timestamp_name else NULL,
                                                                 annotation = if(isTRUE(annotate_with_time)) private$get_timestamp else NULL)
                                                
                                                local_env$.add_to_static_env(super$get_static_env())
                                                local_env$static_env$err$set_assertions_status(assertions_status)
                                                
                                                #' Add your initialization code here here:
                                                l$s$err$assert_msg("'rnd_seed' must be NULL or a finite integer of length one.",
                                                                   l$s$mg$`%then%`(!is.null(rnd_seed), 
                                                                                   l$s$tcs$has_length_1(rnd_seed, NA_on_fail = FALSE)),
                                                                   l$s$mg$`%then%`(!is.null(rnd_seed), 
                                                                                   l$s$tcs$is_integer(rnd_seed,
                                                                                                      accept_NULL = FALSE,
                                                                                                      accept_NaN = FALSE,
                                                                                                      accept_NA = FALSE,
                                                                                                      lower_bound = -Inf,
                                                                                                      lower_bound_inclusive = FALSE,
                                                                                                      upper_bound = Inf,
                                                                                                      upper_bound_inclusive = FALSE)))
                                                
                                                l$s$err$assert_msg("'delay_mean' must be a non-negative numeric of length one.",
                                                                   l$s$tcs$has_length_1(delay_mean, NA_on_fail = FALSE),
                                                                   l$s$tcs$is_numeric(delay_mean,
                                                                                      accept_NULL = FALSE,
                                                                                      accept_NaN = FALSE,
                                                                                      accept_NA = FALSE,
                                                                                      lower_bound = 0,
                                                                                      lower_bound_inclusive = TRUE,
                                                                                      upper_bound = Inf,
                                                                                      upper_bound_inclusive = FALSE,
                                                                                      accept_non_integer = TRUE))
                                                
                                                l$s$err$assert_msg("'delay_stddev' must be a non-negative numeric of length one.",
                                                                   l$s$tcs$has_length_1(delay_stddev, NA_on_fail = FALSE),
                                                                   l$s$tcs$is_numeric(delay_stddev,
                                                                                      accept_NULL = FALSE,
                                                                                      accept_NaN = FALSE,
                                                                                      accept_NA = FALSE,
                                                                                      lower_bound = 0,
                                                                                      lower_bound_inclusive = TRUE,
                                                                                      upper_bound = Inf,
                                                                                      upper_bound_inclusive = FALSE,
                                                                                      accept_non_integer = TRUE))
                                                
                                                l$s$err$assert_msg("'max_num_timestamps' must be Inf or an integer >= 1 of length one.",
                                                                   l$s$tcs$has_length_1(max_num_timestamps, NA_on_fail = FALSE),
                                                                   l$s$tcs$is_integer(max_num_timestamps,
                                                                                      accept_NULL = FALSE,
                                                                                      accept_NaN = FALSE,
                                                                                      accept_NA = FALSE,
                                                                                      lower_bound = 1,
                                                                                      lower_bound_inclusive = TRUE,
                                                                                      upper_bound = Inf,
                                                                                      upper_bound_inclusive = TRUE))
                                                
                                                l$s$err$assert_msg(paste0("'delay_min' must either be NULL or a non-negative numeric of length one."),
                                                                   l$s$mg$`%then%`(!is.null(delay_min), l$s$tcs$has_length_1(delay_min, NA_on_fail = FALSE)),
                                                                   l$s$mg$`%then%`(!is.null(delay_min), l$s$tcs$is_numeric(delay_min,
                                                                                                                           accept_NULL = FALSE,
                                                                                                                           accept_NaN = FALSE,
                                                                                                                           accept_NA = FALSE,
                                                                                                                           lower_bound = 0,
                                                                                                                           lower_bound_inclusive = TRUE,
                                                                                                                           upper_bound = Inf,
                                                                                                                           upper_bound_inclusive = FALSE,
                                                                                                                           accept_non_integer = TRUE)))
                                                l$s$err$warnifnot("'delay_min' is non-NULL but bigger than 'delay_mean'.",
                                                                  l$s$mg$`%then%`(!is.null(delay_min), delay_min <= delay_mean))
                                                
                                                l$s$err$assert_msg(paste0("'delay_max' must either be NULL or a non-negative numeric of length one ",
                                                                          "that is also >= 'delay_min' if the latter has been provided."),
                                                                   l$s$mg$`%then%`(!is.null(delay_max), l$s$tcs$has_length_1(delay_max, NA_on_fail = FALSE)),
                                                                   l$s$mg$`%then%`(!is.null(delay_max), l$s$tcs$is_numeric(delay_max,
                                                                                                                           accept_NULL = FALSE,
                                                                                                                           accept_NaN = FALSE,
                                                                                                                           accept_NA = FALSE,
                                                                                                                           lower_bound = if(is.null(delay_min)) 0 else delay_min,
                                                                                                                           lower_bound_inclusive = TRUE,
                                                                                                                           upper_bound = Inf,
                                                                                                                           upper_bound_inclusive = FALSE,
                                                                                                                           accept_non_integer = TRUE)))
                                                l$s$err$warnifnot("'delay_max' is non-NULL but smaller than 'delay_mean'.",
                                                                  l$s$mg$`%then%`(!is.null(delay_max), delay_max >= delay_mean))
                                                
                                                l$s$err$assert_msg("'annotation_time_start' must be either NULL or a finite numeric of length one.",
                                                                   xor(is.null(annotation_time_start), 
                                                                       l$s$tcs$has_length_1(annotation_time_start,
                                                                                            NA_on_fail = FALSE) &&
                                                                         l$s$tcs$is_numeric(annotation_time_start,
                                                                                            accept_NULL = FALSE,
                                                                                            accept_NaN = FALSE,
                                                                                            accept_NA = FALSE,
                                                                                            lower_bound = -Inf,
                                                                                            lower_bound_inclusive = FALSE,
                                                                                            upper_bound = Inf,
                                                                                            upper_bound_inclusive = FALSE,
                                                                                            accept_non_integer = TRUE)))
                                                
                                                private$annotate_with_time <- annotate_with_time
                                                private$annotation_time_start <- annotation_time_start
                                                
                                                private$delay_mean <- delay_mean
                                                private$delay_stddev <- delay_stddev
                                                private$delay_min <- delay_min
                                                private$delay_max <- delay_max
                                                private$time_last_next <- 0
                                                private$next_count <- 0
                                                private$timer_started <- FALSE
                                                private$next_delta_t <- private$.get_next_delta_t()
                                                private$timestamps <- list()
                                                private$max_num_timestamps <- max_num_timestamps
                                                private$rnd_seed <- rnd_seed
                                              },
                                              
                                              finalize = function() {
                                                super$finalize()
                                                
                                                #' Add your finalization code here:
                                              },
                                              
                                              #' @return Independent of whether `annotate_with_time` was actually set to `TRUE` or `FALSE`,
                                              #'   this method returns the timestamp of the __next__ element retrieved via `get_next()`.
                                              #'   I.e., this is the timestamp that this next element would be annotated with, if
                                              #'   `annotation_with_time` was set to `TRUE` in `initialize()`, regardless of whether it
                                              #'   was actually set to `TRUE` or not.
                                              #' @export
                                              #'
                                              #' @examples
                                              #' @md
                                              get_next_time_available = function(){
                                                l$s$err$stopifnot(paste0("Timer has not started yet. It starts with the first call to ",
                                                                         "either 'get_next()' or 'get_next_count()'."),
                                                                  private$timer_started)
                                                if(length(private$timestamps) == 0){
                                                  result <-  private$time_last_next + private$next_delta_t
                                                  if(is.null(private$annotation_time_start)){
                                                    first_timestamp <-  0
                                                    annotation_time_start <- 0
                                                  }
                                                  else{
                                                    annotation_time_start <- private$annotation_time_start
                                                    if(is.null(private$first_timestamp)){
                                                      first_timestamp <- result
                                                    }
                                                    else{
                                                      first_timestamp <- private$first_timestamp
                                                    }
                                                    
                                                    result <- result - first_timestamp + annotation_time_start
                                                  }
                                                }
                                                else{
                                                  result <- private$timestamps[[1]]
                                                }
                                                
                                                return(result)
                                              },
                                              
                                              #' @return The `max_num_timestamps` parameter provided to `initialize()`.
                                              #' @export
                                              #'
                                              #' @examples
                                              #' @md
                                              get_max_num_timestamps = function(){
                                                return(private$max_num_timestamps)
                                              },
                                              
                                              #' @return The `delay_min` parameter provided to `initialize()`.
                                              #' @export
                                              #'
                                              #' @examples
                                              #' @md
                                              get_delay_min = function(){
                                                return(private$delay_min)
                                              },
                                              
                                              #' @return The `delay_max` parameter provided to `initialize()`.
                                              #' @export
                                              #'
                                              #' @examples
                                              #' @md
                                              get_delay_max = function(){
                                                return(private$delay_max)
                                              },
                                              
                                              #' @return The mean delay in milliseconds.
                                              #' @export
                                              #'
                                              #' @examples
                                              #' @md
                                              get_delay_mean = function(){
                                                return(private$delay_mean)
                                              },
                                              
                                              #'
                                              #' @return The delay standard deviation in milliseconds.
                                              #' @export
                                              #'
                                              #' @examples
                                              #' @md
                                              get_delay_stddev = function(){
                                                return(private$delay_stddev)
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
                                                
                                                if(l$s$err$get_assertions_status()){
                                                  l$s$err$assert_msg(paste0("'next_count' must be an integer of length one between 0 ",
                                                                            "and '.get_next_count_internal()'."),
                                                                     l$s$tcs$has_length_1(private$next_count, NA_on_fail = FALSE),
                                                                     l$s$tcs$is_integer(private$next_count,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE,
                                                                                        lower_bound = 0,
                                                                                        lower_bound_inclusive = TRUE,
                                                                                        upper_bound = self$.get_next_count_internal(),
                                                                                        upper_bound_inclusive = TRUE))
                                                  
                                                  l$s$err$assert_msg(paste0("'time_last_next' must be a numeric of length one between 0 ",
                                                                            "and 'get_unix_time()'."),
                                                                     l$s$tcs$has_length_1(private$time_last_next),
                                                                     l$s$tcs$is_numeric(private$time_last_next,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE,
                                                                                        lower_bound = 0,
                                                                                        lower_bound_inclusive = TRUE,
                                                                                        upper_bound = l$s$t$get_unix_time(),
                                                                                        upper_bound_inclusive = TRUE,
                                                                                        accept_non_integer = TRUE))
                                                  
                                                  l$s$err$assert_msg("'next_count' must be zero if '.finished_internal()' returns TRUE.",
                                                                     !self$.finished_internal() || private$next_count == 0)
                                                  l$s$err$assert_msg("'next_count' must equal 'private$next_count' and the length of 'timestamps'.",
                                                                     next_count == private$next_count,
                                                                     next_count == length(private$timestamps))
                                                }
                                              },
                                              
                                              .get_next = function(num) {
                                                
                                                private$.update_fields()
                                                
                                                next_elems <- super$.get_next(num)
                                                l$s$err$assert_msg(paste0("'super$.get_next(num)' did return wrong number ",
                                                                          "of elements."),
                                                                   length(next_elems) == num)
                                                private$next_count <- private$next_count - num
                                                if(!private$annotate_with_time && num > 0){
                                                  l$s$err$assert_msg("Length of 'timestamps' should be >= 'num' at this point.",
                                                                     length(private$timestamps) >= num)
                                                  for(i in 1:num){
                                                    private$timestamps[[1]] <- NULL
                                                  }
                                                }
                                                return(next_elems)
                                              },
                                              
                                              .validate_next = function(element){
                                                return(TRUE)
                                              },
                                              
                                              .get_next_count = function(){
                                                private$.update_fields()
                                                return(private$next_count)
                                              },
                                              
                                              .finished = function(){
                                                finished <- self$.finished_internal()
                                                return(finished)
                                              }
                                            ),
                                            #-----end of public-----
                                            
                                            #' ----- Add active bindings ----
                                            active = list(
                                              
                                            )
                                            #-----end of active-----
        )
        
        #' An `Abstract_Iterator` subclass for concatenating iterators for iteration over each iterator
        #' in the order they are added to this decorator.
        Concatenated_Iterator <- R6Class("Concatenated_Iterator",
                                         # ---- R6 inheritance -----
                                         inherit = l$result_env$Abstract_Iterator,
                                         
                                         #' ---- Adapt R6 options --------
                                         portable = TRUE,
                                         cloneable = TRUE,
                                         lock_class = TRUE,
                                         lock_objects = TRUE,
                                         
                                         #' ----- Add private Fields & methods ----
                                         private = list(
                                           iterators = NULL,
                                           locked = NULL,
                                           new_iterator = NULL,
                                           
                                           current_fixed_length = NULL,
                                           current_num_remaining = NULL
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
                                           
                                           #' @return The current number of internal iterators.
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           get_iterators_num = function(){
                                             return(length(private$iterators))
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
                                             
                                             l$s$err$assert_msg(paste0("'get_length()' and 'get_num_remaining()' are 'NA' ",
                                                                       "if and only if 'is_locked()' is 'FALSE'. ",
                                                                       "Otherwise, 'get_length()' and 'current_fixed_length' ",
                                                                       "as well as 'current_num_remaining' and ",
                                                                       "'get_num_remaining()' should be equal."),
                                                                l$s$mg$`%eq%`(len_na <- is.na((len <- self$get_length())), 
                                                                              is.na((num_rem <- self$get_num_remaining()))),
                                                                l$s$mg$`%eq%`(len_na, !self$is_locked()),
                                                                l$s$mg$`%then%`(!len_na, private$current_fixed_length <= len),
                                                                l$s$mg$`%then%`(!len_na, l$s$dts$equals(private$current_num_remaining, num_rem)))
                                             
                                           },
                                           
                                           #' Iterator for iteration over multiple `Abstract_Iterator` instances.
                                           #' First iterates over the first iterator in `iterators`, then over the second one
                                           #' and so on. Iterators are immediatelly removed when they are finished.
                                           #' 
                                           #' You may use the `append_iterator()` and `append_iterators()` methods
                                           #' in order to append additional iterators to be iterated over at the end.
                                           #' Those iterators must have a non-`NA` length and must be locked, if
                                           #' they inherit from this class.
                                           #' 
                                           #' Appending new iterators is only possible as long as this instance it not locked
                                           #' (see see `is_locked()` and `lock()`).
                                           #' 
                                           #' Since the total length is unknown until this instance is locked (until then, new
                                           #' iterators might be added, thus increasing the total length), `get_length()` and
                                           #' `get_num_remaining()` return `NA` until `lock()` has been called.
                                           #' After this instance has been locked, `get_length()` will always return the sum
                                           #' of lengths of all iterators that were in the internal queue __at the moment of locking__.
                                           #' This value does not change after locking. Similarly,
                                           #' `get_num_remaining()` returns the actual number of remaining elements, once this instance
                                           #' is locked. 
                                           #'  
                                           #' You may use the `get_current_length()` and 
                                           #' `get_current_num_remaining()` methods to access the total length and the total number 
                                           #' of remaining elements of the __current__ internal iterators, independently of the lock status. 
                                           #' These numbers might change as new iterators are appended or iterators with no remaining elements
                                           #' are removed from the internal queue. Note that since those methods always reflect the
                                           #' __current__ status of the internal queue, `get_current_length()` will decrease by the length
                                           #' of each finished iterator, even if this instance is locked.
                                           #' 
                                           #' For subclasses of this class, overriding the `.new_iterator()` 
                                           #' and `.new_elements()` methods might be of particular interest.
                                           #'
                                           #' __IMPORTANT__: See documentation of this method in `utils_lang_r6_subclass_TEMPLATE.R`.
                                           #'
                                           #' @param assertions_status See `Abstract_Iterator`.
                                           #' @param iterators Forwarded to `append_iterators()`. See latter for documentation.
                                           #' @param lock If `TRUE`, this instance's `lock()` method is called at the end 
                                           #'   of the initialization process. 
                                           #'   
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           initialize = function(assertions_status = FALSE, 
                                                                 iterators = list(), 
                                                                 lock = FALSE){
                                             super$initialize(assertions_status = assertions_status, fixed_length = NA)
                                             
                                             local_env$.add_to_static_env(super$get_static_env())
                                             local_env$static_env$err$set_assertions_status(assertions_status)
                                             
                                             #' Add your initialization code here here:
                                             private$iterators <- list()
                                             private$locked <- FALSE
                                             
                                             private$current_fixed_length <- 0
                                             private$current_num_remaining <- 0
                                             
                                             self$append_iterators(iterators)
                                             if(lock){
                                               self$lock()
                                             }
                                             private$new_iterator <- TRUE
                                           },
                                           
                                           finalize = function() {
                                             super$finalize()
                                           },
                                          
                                           #' Check whether this instance is locked.
                                           #' `append_iterator()` and `append_iterators()`
                                           #' produce an error if and only if this instance has been locked
                                           #' via `lock()`.
                                           #'
                                           #' @return `TRUE` if and only if this instance has been locked
                                           #'   (i.e., `lock()` has been called at least once).
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           is_locked = function() {
                                             return(private$locked)
                                           },
                                         
                                           #' Lock this instance.
                                           #' `append_iterator()` and `append_iterators()`
                                           #' produce an error if and only if this instance has been locked.
                                           #' 
                                           #' The method is without effect if this instance is already locked.
                                           #' 
                                           #' Note that once this instance has been locked, it is impossible
                                           #' to unlock it. `get_length()` will now return the
                                           #' sum of lengths of all internal iterators at the time
                                           #' this instance has been locked.
                                           #' 
                                           #' Current lock status can be checked via `is_locked()`.
                                           #'
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           lock = function() {
                                             if(!private$locked){
                                               private$locked <- TRUE
                                               self$.set_fixed_length(private$current_fixed_length)
                                               self$.set_num_remaining(private$current_num_remaining)
                                             }
                                           },

                                           #'
                                           #' @return The current total length of this iterator. I.e., the sum of lengths of all concatenated iterators.
                                           #'   Once the current iterator is finished, it is removed from the internal queue and
                                           #'   the total length is reduced by its length.
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           get_current_length = function(){
                                             return(private$current_fixed_length)
                                           },
                                           
                                           #'
                                           #' @return The current total number of remaining elements. I.e., the sum
                                           #'   of the number of remaining elements of all concatenated iterators.
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           get_current_num_remaining = function(){
                                             return(private$current_num_remaining)
                                           },
                                          
                                           toString = function(tab = ""){
                                             result <- super$toString(tab = tab)
                                             
                                             return(paste0(result, "\n",
                                                           tab, "  Current length: ", self$get_current_length(),"\n",
                                                           tab, "  Current num remaining: ", self$get_current_num_remaining()))
                                           },
                                                                                       
                                           #' Equivalent to a call to `append_iterators(list(iterator))`.
                                           #'
                                           #' @examples
                                           #' @md
                                           append_iterator = function(iterator){
                                             self$append_iterators(iterators = list(iterator))
                                           },

                                           #' Append provided iterators to the internal list of iterators to iterate over
                                           #' (that sentence contains way too many "iterators" ;)).
                                           #' 
                                           #' This method produces an error if this instance has been locked
                                           #' (see `lock()` and `is_locked()`).
                                           #' 
                                           #' @param iterators A list coercible data structure containing zero or more `Abstract_Iterator`
                                           #'   instances to iterate over.
                                           #'   
                                           #'   All of these iterators __must__ have a __fixed__, __non-`NA`__ and 
                                           #'   __non-`INF`__ length (see `get_length()`) that does not change over time. 
                                           #'   Otherwise this iterator might produce an error or simply behave in an undefined manner.
                                           #'   
                                           #'   Each iterator in `iterators` that is also a `Concatenated_Iterator`
                                           #'   instance must be locked (see `is_locked()` and `lock()`) so that its
                                           #'   `get_length()` method always returns the same, well defined value.
                                           #'   
                                           #'   Note that iterators in `iterators` whose `get_num_remaining()` method returns zero
                                           #'   are ignored.
                                           #'
                                           #' @return
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           append_iterators = function(iterators){
                                             l$s$err$stopifnot("Attempt to append iterators while 'is_locked()' is TRUE.",
                                                               !self$is_locked())
                                             
                                             iterators <- as.list(iterators)
                                             
                                             if(length(iterators) == 0){
                                               return()
                                             }
                                             
                                             total_length <- 0
                                             total_num_remaining <- 0
                                             i <- 1
                                             while(i <= length(iterators)){
                                               l$s$err$assert_msg("Each provided iterator must be or inherit from 'Abstract_Iterator'.",
                                                                  is.R6(iterators[[i]]), inherits(iterators[[i]], "Abstract_Iterator"))
                                               l$s$err$assert_msg("Provided 'Concatenated_Iterator' instances' 'is_locked()' must return TRUE.",
                                                                  !inherits(iterators[[i]], "Concatenated_Iterator") || iterators[[i]]$is_locked())
                                               
                                               len <- iterators[[i]]$get_length()
                                               
                                               l$s$err$assert_msg(paste0("'Concatenated_Iterator' only accepts iterators ", 
                                                                         "where 'get_length()' returns a non-infinite, non-negative, ",
                                                                         "non-NA and non-NaN integer of length 1."),
                                                                  l$s$tcs$has_length_1(len, NA_on_fail = FALSE),
                                                                  l$s$tcs$is_integer(len,
                                                                                     accept_NULL = FALSE,
                                                                                     accept_NaN = FALSE,
                                                                                     accept_NA = FALSE,
                                                                                     lower_bound = 0,
                                                                                     lower_bound_inclusive = TRUE,
                                                                                     upper_bound = Inf,
                                                                                     upper_bound_inclusive = FALSE))
                                               
                                               num_rem <- iterators[[i]]$get_num_remaining()
                                               l$s$err$assert_msg(paste0("Inconsistent 'get_num_remaining()'."),
                                                                  l$s$tcs$has_length_1(num_rem, NA_on_fail = FALSE),
                                                                  l$s$tcs$is_integer(num_rem,
                                                                                     accept_NULL = FALSE,
                                                                                     accept_NaN = FALSE,
                                                                                     accept_NA = FALSE,
                                                                                     lower_bound = 0,
                                                                                     lower_bound_inclusive = TRUE,
                                                                                     upper_bound = len,
                                                                                     upper_bound_inclusive = TRUE))
                                               
                                               if(num_rem == 0){
                                                 iterators[i] <- NULL
                                               }
                                               else{
                                                 l$s$err$stopifnot(paste0("Total length cannot be bigger than '.Machine$integer.max'."),
                                                                   .Machine$integer.max - len >= total_length)
                                                 
                                                 total_length <- total_length + len
                                                 total_num_remaining <- total_num_remaining + num_rem
                                                 
                                                 i <- i + 1
                                               }
                                             }
                                             
                                             if(total_num_remaining > 0){
                                               l$s$err$stopifnot(paste0("Total length cannot be bigger than '.Machine$integer.max'."),
                                                                 .Machine$integer.max - total_length >= private$current_fixed_length)
                                               
                                               private$iterators <- c(private$iterators, iterators)
                                               
                                               private$current_fixed_length <- private$current_fixed_length + total_length
                                               private$current_num_remaining <- private$current_num_remaining + total_num_remaining
                                             }
                                           },
                                           
                                           .get_next = function(num) {
                                             if(num == 0){
                                               return(list())
                                             }
                                             else{
                                               result <- vector(length=num, mode="list")
                                               
                                               num_retrieved <- 0
                                               num_it <- length(private$iterators)
                                               while(num_retrieved < num){
                                                 l$s$err$assert_msg("'num_retrieved' is smaller than 'num' but no iterators left.",
                                                                  num_it > 0)
                                                 
                                                 current_num <- min(num - num_retrieved, private$iterators[[1]]$get_next_count())
                                                 l$s$err$assert_msg("'current_num' should be at least one at this point.",
                                                                    current_num > 0)
                                                 
                                                 if(private$new_iterator){
                                                   new_it <- self$.new_iterator(private$iterators[[1]])
                                                   
                                                   l$s$err$assert_msg("'.new_iterator()' must return an iterator that inherits from 'Abstract_Iterator'.",
                                                                      is.R6(new_it), inherits(new_it, "Abstract_Iterator"))
                                                   
                                                   l$s$err$assert_msg(paste0("If '.new_iterator()' returns a 'Concatenated_Iterator' instance, the latter's ",
                                                                             "'is_locked()' method must return TRUE."),
                                                                      !inherits(new_it, "Concatenated_Iterator") || new_it$is_locked())
                                                   
                                                   l$s$err$assert_msg(paste0("'The iterator returned by '.new_iterator(iterators[[1]])'' must ",
                                                                            "have the same length, numer of remaining elements and ",
                                                                            "number of next elements as 'iterators[[1]]'."),
                                                                     new_it$get_length() == private$iterators[[1]]$get_length(),
                                                                     new_it$get_num_remaining() == private$iterators[[1]]$get_num_remaining(),
                                                                     new_it$get_next_count() == private$iterators[[1]]$get_next_count())
                                                   
                                                   private$iterators[[1]] <- new_it
                                                                      
                                                   private$new_iterator <- FALSE
                                                 }
                                                 
                                                 next_elems <- private$iterators[[1]]$get_next(current_num)
                                                 l$s$err$assert_msg(paste0("'get_next()' did return wrong number ",
                                                                           "of elements."),
                                                                    length(next_elems) == current_num)

                                                 next_elems <- self$.new_elements(next_elems)
                                                 l$s$err$assert_msg("'.next_elements()' returned wrong number of elements.",
                                                                    length(next_elems) == current_num)
                                                 
                                                 result[(num_retrieved+1):(num_retrieved + current_num)] <- next_elems
                                                 
                                                 num_retrieved <- num_retrieved + current_num
                                                 
                                                 #' NO NEED TO ADAPT NUM_REMAINING: 
                                                 #' this is done in the superclass' `get_next()` method
                                                 #self$.set_num_remaining(self$get_num_remaining() - current_num)
                                                 
                                                 l$s$err$assert_msg(paste0("Inconsistent numer of returned elements."),
                                                                    l$s$mg$`%then%`(num_retrieved < num, 
                                                                                    private$iterators[[1]]$get_num_remaining() == 0))
                                                 
                                                 if(private$iterators[[1]]$get_num_remaining() == 0){
                                                   l$s$err$assert_msg("Iterator should be finished at this point.",
                                                                      private$iterators[[1]]$finished())
                                                   private$current_fixed_length <- private$current_fixed_length - private$iterators[[1]]$get_length()
                                                   
                                                   l$s$err$assert_msg("'current_fixed_length' should not be negative.",
                                                                      private$current_fixed_length >= 0)
                                                   
                                                   private$iterators[[1]] <- NULL
                                                   private$new_iterator <- TRUE
                                                 }
                                                 
                                                 num_it <- num_it - 1
                                               }
                                               
                                               l$s$err$assert_msg("Inconsistent numer of retrieved elements.",
                                                                  length(result) == num,
                                                                  num_retrieved == num)
                                               
                                               private$current_num_remaining <- private$current_num_remaining - num_retrieved
                                               l$s$err$assert_msg(paste0("'current_num_remaining' should not be negative and ",
                                                                         "not bigger than 'current_fixed_length'."),
                                                                  private$current_num_remaining >= 0,
                                                                  private$current_num_remaining <= private$current_fixed_length)
                                               
                                               return(result)
                                             }
                                           },
                                           
                                           .validate_next = function(element) {
                                             #' TODO: Add additional validation, if required
                                             #' Note that, at this point, each element is already validated 
                                             #' by the respective iterator that returned it.
                                             return(TRUE)
                                           },
                                           
                                           .get_next_count = function() {
                                             num_it <- length(private$iterators)
                                             total_next_count <- 0
                                             if(num_it > 0){
                                               i <- 1
                                               while(i < num_it && 
                                                     (next_count <- private$iterators[[i]]$get_next_count()) == private$iterators[[i]]$get_num_remaining()){
                                                 total_next_count <- total_next_count + next_count
                                                 i <- i + 1
                                               }
                                               total_next_count <- total_next_count + private$iterators[[i]]$get_next_count()
                                             }
                                             
                                             return(total_next_count)
                                           },
                                           
                                           .finished = function() {
                                             return(self$is_locked() && l$s$dts$equals(self$get_num_remaining(), 0))
                                           },
                                           
                                           #' ----------- THE FOLLOWING METHODS __MAY__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                           
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
                                           
                                           
                                           #' Called each time right before at least one element
                                           #' from a __new__ iterator is retrieved in `get_next()`.
                                           #' I.e., it indicates the switch from one of the provided iterators
                                           #' to the next one.
                                           #' 
                                           #' Receives said next iterator as argument and returns
                                           #' an iterator to be actually used as the next one. 
                                           #' 
                                           #' Here, the method simply returns the same iterator that was provided.
                                           #' Overriding subclasses may use this method to decorate the next iterator
                                           #' (e.g., based on the values previously provided to `.new_values()`).
                                           #'
                                           #' @param new_iterator Next iterator from which elements will
                                           #'   be retrieved in `get_next()`. 
                                           #'
                                           #' @return Some, possibly decorated version of `new_iterator`.
                                           #'   Must inherit from `Abstract_Iterator`.
                                           #'  
                                           #'   If the returned iterator does extend `Concatenated_Iterator`,
                                           #'   its `is_locked()` method must return `TRUE`.
                                           #'   
                                           #'    __IMPORTANT__: The returned iterator must have the same length
                                           #'   (w.r.t. `get_length()`), number of remaining elements (w.r.t. `get_num_remaining()`)
                                           #'   and number of next elemements (w.r.t. `get_next_count()`) as `new_iterator`. 
                                           #'      
                                           #'   This implementation simply returns `new_iterator`.
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           .new_iterator = function(new_iterator){
                                             return(new_iterator)
                                           },
                                           
                                           #' Called each time elements were retrieved from a single
                                           #' internal iterator in `get_next()`. Those elements are provided
                                           #' as arguments.
                                           #' 
                                           #' `new_elements` are guaranteed to have been retrieved from the __last__
                                           #' iterator returned by `.new_iterator()`.
                                           #' 
                                           #' The idea is to enable subclasses a unique assignment between elements
                                           #' and the iterator they were created with. This, in turn, may be used
                                           #' in `.new_iterator` to decorate the next iterator based on the values
                                           #' retrieved by the preceeding iterators or to manipulate the actual elements
                                           #' directly in this method.
                                           #'
                                           #' @param new_elements Elements retrieved from the __last__
                                           #'   iterator returned by `.new_iterator()`.
                                           #' @return Some, possibly decorated version of `new_elements`.
                                           #'   Must inherit from `Abstract_Iterator`.
                                           #'  
                                           #'    __IMPORTANT__: The element list must have the same length
                                           #'    as `new_elements`. 
                                           #'      
                                           #'   This implementation simply returns `new_elements`.
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           .new_elements = function(new_elements){
                                             return(new_elements)
                                           }
                                         ),
                                         #-----end of public-----
                                         
                                         #' ----- Add active bindings ----
                                         active = list(
                                           
                                         )
                                         #-----end of active-----
        )
        
        
        #' A `Concatenated_Iterator` subclass for seamless transition ("step"-less)
        #' from one iterator to another.
        Offset_Normalized_Concatenated_Iterator <- R6Class("Offset_Normalized_Concatenated_Iterator",
                                         # ---- R6 inheritance -----
                                         inherit = l$result_env$Concatenated_Iterator,
                                         
                                         #' ---- Adapt R6 options --------
                                         portable = TRUE,
                                         cloneable = TRUE,
                                         lock_class = TRUE,
                                         lock_objects = TRUE,
                                         
                                         #' ----- Add private Fields & methods ----
                                         private = list(
                                           div_op = NULL,
                                           sum_op = NULL,
                                           minus_op = NULL,
                                           
                                           offset_max_num_elems = NULL,
                                           element_buffer = NULL,
                                           
                                           is_first_iterator = NULL,
                                           is_new_iterator = NULL,
                                           offset = NULL,
                                           
                                           initial_offset = NULL,
                                           normalize_offsets = NULL,
                                           additional_offsets = NULL,
                                           
                                           first_it_appended = NULL
                                         ),
                                         #-----end of private-----
                                         
                                         #' ----- Add public fields & methods ----
                                         public = list(
                                           #' @return The division operator previously provided to `initialize()`.
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           get_division_operator = function(){
                                             return(private$div_op)
                                           },
                                           
                                           #' @return The summation operator previously provided to `initialize()`.
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           get_summation_operator = function(){
                                             return(private$sum_op)
                                           },
                                           
                                           #' @return The minus operator previously provided to `initialize()`.
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           get_minus_operator = function(){
                                             return(private$minus_op)
                                           },
                                           
                                           #' @return The offset used for the __last__ element that was returned via `get_next()`.
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           get_current_offset = function(){
                                             return(private$offset)
                                           },
                                           
                                           #' @param i An integer between 1 and `get_buffer_length()`.
                                           #' 
                                           #' @return The `i`-th last element produced by the __current__ internal iterator.
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           get_buffer_element = function(i){
                                             l$s$err$assert_msg("Index out of bounds.",
                                                                i >= 1,
                                                                i <= length(private$element_buffer))
                                             return(private$element_buffer[[i]])
                                           },
                                           
                                           #' @return Current number of elements buffered from the __current__ internal iterator.
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           get_buffer_length = function(){
                                             return(length(private$element_buffer))
                                           },
                                           
                                           #' See `R6_Base` in `utils_lang_r6_baseclass.R`
                                           #' MUST always be copied over.
                                           #' @export
                                           #' @md
                                           get_static_env = function(){
                                             local_env$static_env
                                           },
                                                                                       
                                           #' @return The `initial_offset` provided to `initialize()`.
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           get_initial_offset = function(){
                                             return(private$initial_offset)
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
                                           
                                           #' A `Concatenated_Iterator` subclass for seamless transition ("step"-less)
                                           #' from one iterator to another.
                                           #'  
                                           #' When an internal iterator finishes, this subclass computes an offset `o` to be added
                                           #' to each value of the following iterator. This is performed as follows:
                                           #' * If the `normalize_offsets` entry associated with the following iterator
                                           #'   is set to `FALSE`, then the offset is set to zero.   
                                           #' * Otherwise: Let `x_1, ..., x_offset_max_num_elems` be the
                                           #'   last (potentially offset normalized) `offset_max_num_elems` elements produced 
                                           #'   by the finished iterator
                                           #'   (or fewer, if it produced less elements) where `x_1` is the newest and
                                           #'   `x_offset_max_num_elems` is the oldest one. Let `y_1` be the first value produced
                                           #'   by the new iterator and let 
                                           #'   `m = mean(x_(i-1) - x_i)` be the mean of the `(offset_max_num_elems-1)`
                                           #'   differences of subsequent elements (define `m=0` if `offset_max_num_elems = 1`).
                                           #'   Then the offset `o` is set to `o= x_1 + m - y_1`
                                           #' * Afterwards, if the `additional_offsets` entry associated with the following iterator
                                           #'   is non-`NULL`, that `additional_offsets` entry is additionally added to the
                                           #'   offset.
                                           #' 
                                           #' The first iterator this class iterates over is handled differently:
                                           #' The `normalize_offsets` entry associated with the first iterator
                                           #' must be `FALSE` or otherwise a warning is produced and that entry is
                                           #' overridden with `FALSE`. The offset for the first iterator is computed as follows:
                                           #' * If `initial_offset` is `NULL`, then this subclass simply forwards the
                                           #'   unnormalized values produced by the first internal iterator (i.e., `o` is set
                                           #'   to zero).
                                           #' * Otherwise the offset is set to `initial_offset`.
                                           #' 
                                           #' In summary: The `Concatenated_Iterator` may be used to avoid large value "jumps"
                                           #' when transitioning from one iterator to the next one.
                                           #' 
                                           #' Note that you may access the current offset and buffered elements `x_i`
                                           #' via `get_current_offset()`, `get_buffer_element()` and `get_buffer_length()`.
                                           #'
                                           #' __IMPORTANT__: See documentation of this method in `utils_lang_r6_subclass_TEMPLATE.R`.
                                           #'
                                           #' @param assertions_status See `Abstract_Iterator`.
                                           #' @param iterators  See `Concatenated_Iterator`.
                                           #' @param lock See `Concatenated_Iterator`.
                                           #' 
                                           #' @param offset_max_num_elems An integer of length one and a value >= 1. See
                                           #'   description above for more details.
                                           #' @param initial_offset `NULL` or a finite numeric of length one. See
                                           #'   description above for more details.
                                           #' @param division_operator The division operator to be used for the computation of
                                           #'   the element mean `m` (division of element sum with number of elements).
                                           #' @param summation_operator The summation operator to be used for the
                                           #'   computation of the element mean `m` (computing the element sum). Should be defined
                                           #'   for adding a scalar zero (i.e., `summation_operator(x,0)` should return `x`).
                                           #' @param minus_operator The minus operator to use for the computation of the offsets
                                           #'   (`m-x` and, if applicable, `initial_offset - x`).
                                           #' @param normalize_offsets A logical of length one or the same length as `iterators`.
                                           #'   If it has length one, it is treated as a list of the same length as `iterators`
                                           #'   containing only that value.
                                           #'   The i-th entry in `normalize_offsets` then refers to the i-th iterator in `iterators`. See
                                           #'   description above for more details.
                                           #' @param additional_offsets Either `NULL` or an object of length one or of the same length
                                           #'   as `iterators`. The first two cases are treated as a list containing onl< `NULL`
                                           #'   entries or, in the second case, only that object.
                                           #'   The i-th entry in `additional_offsets` then refers to the i-th iterator in `iterators`. See
                                           #'   description above for more details.
                                           #'   
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           initialize = function(assertions_status = FALSE, 
                                                                 iterators = list(), 
                                                                 lock = FALSE,
                                                                 offset_max_num_elems = 2,
                                                                 division_operator = `/`,
                                                                 summation_operator = `+`,
                                                                 minus_operator = `-`,
                                                                 initial_offset = NULL,
                                                                 normalize_offsets = TRUE,
                                                                 additional_offsets = NULL){
                                             super$initialize(assertions_status = assertions_status, 
                                                              iterators = list(),
                                                              lock = lock)
                                             
                                             local_env$.add_to_static_env(super$get_static_env())
                                             local_env$static_env$err$set_assertions_status(assertions_status)
                                             
                                             #' Add your initialization code here here:
                                             l$s$err$assert_msg(paste0("'offset_max_num_elems' must be an integer ",
                                                                       "of length one and a value >= 1."),
                                                                l$s$tcs$has_length_1(offset_max_num_elems, NA_on_fail = FALSE),
                                                                l$s$tcs$is_integer(offset_max_num_elems,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE,
                                                                                   lower_bound = 1,
                                                                                   lower_bound_inclusive = TRUE,
                                                                                   upper_bound = Inf,
                                                                                   upper_bound_inclusive = FALSE))
                                             
                                             l$s$err$assert_msg(paste0("'division_operator', 'summation_operator' ",
                                                                       "and 'minus_operator' must be functions.",
                                                                       is.function(division_operator),
                                                                       is.function(summation_operator),
                                                                       is.function(minus_operator)))
                                             
                                             private$offset_max_num_elems <- offset_max_num_elems
                                             private$initial_offset <- initial_offset
                                             private$div_op <- division_operator
                                             private$sum_op <- summation_operator
                                             private$minus_op <- minus_operator
                                             private$normalize_offsets <- queue()
                                             private$additional_offsets <- queue()
                                             private$first_it_appended <- FALSE
                                             
                                             self$append_iterators(iterators = iterators, 
                                                                   normalize_offsets = normalize_offsets,
                                                                   additional_offsets = additional_offsets)
                                           },
                                           
                                           
                                           #' Equivalent to a call to `append_iterators(list(iterator), normalize_offset, additional_offset)`.
                                           #'
                                           #' @examples
                                           #' @md
                                           append_iterator = function(iterator, 
                                                                      normalize_offset = TRUE,
                                                                      additional_offset = NULL){
                                             self$append_iterators(iterators = list(iterator), 
                                                                   normalize_offsets = normalize_offset,
                                                                   additional_offsets = additional_offset)
                                           },
                                           
                                           
                                           #' Similar to superclass, but may optionally take
                                           #' additional parameters specifying whether to normalize
                                           #' these iterator's offsets and whether to add additional offsets.
                                           #' 
                                           #' The more precise meaning of these parameters is the same as
                                           #' in the description of the parameters of the same name in `initialize()`.
                                           #'
                                           #' @param iterators See documentation of superclass and of `initialize()`.
                                           #' @param normalize_offsets See documentation of `initialize()`.
                                           #' @param additional_offsets See documentation of `initialize()`.
                                           #'
                                           #' @return
                                           #' @export
                                           #'
                                           #' @examples
                                           append_iterators = function(iterators, 
                                                                       normalize_offsets = TRUE,
                                                                       additional_offsets = NULL){
                                             
                                             super$append_iterators(iterators = iterators)
                                             
                                             iterators <- as.list(iterators)
                                             if(length(iterators) == 0){
                                               return()
                                             }
                                             
                                             normalize_offsets <- as.list(normalize_offsets)
                                             additional_offsets <- if(is.null(additional_offsets)) NULL else as.list(additional_offsets)
                                             
                                             l$s$err$assert_msg(paste0("'normalize_offsets' must be a logical of either length one or ",
                                                                       "the same length as 'iterators'."),
                                                                l$s$tcs$has_length_1(normalize_offsets, NA_on_fail = FALSE) || 
                                                                  l$s$tcs$has_length(normalize_offsets, len = length(iterators), NA_on_fail = FALSE),
                                                                all(l$s$tcs$is_logical(normalize_offsets,
                                                                                       accept_NULL = FALSE,
                                                                                       accept_NaN = FALSE,
                                                                                       accept_NA = FALSE)))
                                             
                                             l$s$err$assert_msg(paste0("'additional_offsets' must be NULL or either of length one or ",
                                                                       "of the same length as 'iterators'."),
                                                                is.null(additional_offsets) ||
                                                                  l$s$tcs$has_length_1(additional_offsets, NA_on_fail = FALSE) || 
                                                                  l$s$tcs$has_length(additional_offsets, len = length(iterators), NA_on_fail = FALSE))
                                             
                                             if(length(normalize_offsets) == 1){
                                               el <- normalize_offsets[[1]]
                                               normalize_offsets <- lapply(1:length(iterators), FUN = function(i) {
                                                 return(el)
                                               })
                                             }
                                             
                                             if(length(additional_offsets) <= 1){
                                               el <- if(is.null(additional_offsets)) NULL else additional_offsets[[1]]
                                               additional_offsets <- lapply(1:length(iterators), FUN = function(i) {
                                                 return(el)
                                               })
                                             }
                                             
                                             l$s$err$assert_msg(paste0("'normalize_offsets' and 'additional_offsets' should both be ", 
                                                                       "of same length as 'iterators' at this point."),
                                                                length(normalize_offsets) == length(additional_offsets),
                                                                length(normalize_offsets) == length(iterators))
                                             
                                             i <- 1
                                             while(i <= length(iterators)){
                                               if(iterators[[i]]$get_num_remaining() == 0){
                                                 iterators[[i]] <- NULL
                                                 normalize_offsets[[i]] <- NULL
                                                 additional_offsets[[i]] <- NULL
                                               }
                                               else{
                                                 if(!private$first_it_appended){
                                                   private$first_it_appended <- TRUE
                                                   l$s$err$warnifnot("The first iterator cannot be offset normalized.",
                                                                     !normalize_offsets[[i]])
                                                   normalize_offsets[[i]] <- FALSE
                                                 }
                                                 i <- i+1
                                               }
                                             }
                                             
                                             l$s$err$assert_msg(paste0("'normalize_offsets' and 'additional_offsets' should both be ", 
                                                                       "of same length as 'iterators' at this point."),
                                                                length(normalize_offsets) == length(additional_offsets),
                                                                length(normalize_offsets) == length(iterators))
                                             
                                             for(i in 1:length(iterators)){
                                               pushback(private$normalize_offsets, normalize_offsets[[i]])
                                               pushback(private$additional_offsets, additional_offsets[[i]])
                                             }
                                           },
                                           
                                           finalize = function() {
                                             super$finalize()
                                           },
                                           
                                           .validate_next = function(element) {
                                             #' Add additional validation, if required
                                             #' Note that, at this point, each element is already validated 
                                             #' by the respective iterator that returned it.
                                             result <- super$.validate_next(element)
                                              
                                             return(result)
                                           },
                                           
                                           .new_iterator = function(new_iterator){
                                             new_iterator <- super$.new_iterator(new_iterator)
                                             
                                             private$is_new_iterator <- TRUE
                                             
                                             if(is.null(private$is_first_iterator)){
                                               private$is_first_iterator <- TRUE
                                             }
                                             else{
                                               private$is_first_iterator <- FALSE
                                             }
                                             
                                             return(new_iterator)
                                           },
                                           
                                           .new_elements = function(new_elements){
                                             new_elements <- super$.new_elements(new_elements)
                                             
                                             buf_len <- length(private$element_buffer)
                                             len <- length(new_elements)
                                             
                                             l$s$err$assert_msg("'new_elements' should contain at least one element at this point.",
                                                                len > 0)
                                             l$s$err$assert_msg("'element_buffer' should not be bigger than 'offset_max_num_elems'.",
                                                                buf_len <= private$offset_max_num_elems)
                                             
                                             if(private$is_new_iterator){
                                               private$is_new_iterator <- FALSE
                                               l$s$err$assert_msg("'normalize_offsets' and 'additional_offsets' have illegal length.",
                                                                  length(private$normalize_offsets) == length(private$additional_offsets),
                                                                  length(private$normalize_offsets) == self$get_iterators_num())
                                               norm <- pop(private$normalize_offsets)
                                               a_off <- pop(private$additional_offsets)
                                               
                                               if(private$is_first_iterator){
                                                 if(!is.null(private$initial_offset)){
                                                   private$offset <- private$initial_offset #private$sum_op(private$initial_offset, new_elements[[1]])
                                                 }
                                                 else{
                                                   private$offset <- 0
                                                 }
                                               }
                                               else if(norm) {
                                                 private$offset <- self$.compute_offset(element_buffer = private$element_buffer,
                                                                                        new_elements = new_elements)
                                               }
                                               else{
                                                 private$offset <- 0
                                               }
                                               
                                               if(!is.null(a_off)){
                                                 private$offset <- private$sum_op(private$offset, a_off)
                                               }
                                               private$element_buffer <- list()
                                               buf_len <- 0
                                             }
                                             
                                             result <- vector(mode="list", length=len)
                                             for(i in 1:len){
                                               result[[i]] <- private$sum_op(new_elements[[i]], private$offset)
                                               
                                               if(buf_len == private$offset_max_num_elems){
                                                 private$element_buffer[[buf_len]] <- NULL
                                                 buf_len <- buf_len - 1
                                               }
                                               private$element_buffer <- c(list(result[[i]]), private$element_buffer)
                                               buf_len <- buf_len + 1
                                             }
                                             
                                             l$s$err$assert_msg("'buf_len' should equal 'length(element_buffer)' at this point.",
                                                                buf_len == length(private$element_buffer))
                                             
                                             return(result)
                                           },
                                           
                                           #' ----------- THE FOLLOWING METHODS __MAY__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                           
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
                                           
                                           #' Called in `.new_elements()` each time an internal iterator has finished
                                           #' and the __first__ elements from the next internal iterator
                                           #' are in the process of being retrieved.
                                           #'
                                           #' May be overwritten by a subclass in order to compute a
                                           #' different offset.
                                           #'
                                           #' @param element_buffer A buffer containing the last 
                                           #'   `min(offset_max_num_elems, last_iterator$length())` elements
                                           #'   produced by the now finished last internal iterator.
                                           #'   The buffer contains the newest element at the beginning.
                                           #'   
                                           #' @param new_elements The __first__ unnormalized elements retrieved by the
                                           #'   next internal iterator.
                                           #'
                                           #' @return The offset `o`. See description of `initialize()` for more details.
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           .compute_offset = function(element_buffer, new_elements){
                                             buf_len <- length(element_buffer)
                                             l$s$err$assert_msg("'element_buffer' should contain at least one element at this point.",
                                                                buf_len > 0)
                                             l$s$err$assert_msg("'new_elements' should contain at least one element at this point.",
                                                                length(new_elements) > 0)
                                             
                                             offset <- 0
                                             if(buf_len > 1){
                                               for(i in 2:buf_len){
                                                 offset <- private$sum_op(
                                                   private$minus_op(element_buffer[[i-1]], element_buffer[[i]]),
                                                   offset)
                                               }
                                               offset <- private$div_op(offset, (buf_len - 1))
                                             }
                                             offset <- private$sum_op(offset, element_buffer[[1]])
                                             offset <- private$minus_op(offset, new_elements[[1]])
                                             return(offset)
                                           }
                                         ),
                                         #-----end of public-----
                                         
                                         #' ----- Add active bindings ----
                                         active = list(
                                           
                                         )
                                         #-----end of active-----
        )
        
        #' An `Abstract_Iterator` subclass based on an internal `queue`.
        Queue_Iterator <- R6Class("Queue_Iterator",
                                     # ---- R6 inheritance -----
                                     inherit = l$result_env$Abstract_Iterator,
                                     
                                     #' ---- Adapt R6 options --------
                                     portable = TRUE,
                                     cloneable = TRUE,
                                     lock_class = TRUE,
                                     lock_objects = TRUE,
                                     
                                     #' ----- Add private Fields & methods ----
                                     private = list(
                                       locked = NULL,
                                       queue = NULL,
                                       current_element = NULL,
                                       nums_remaining = NULL,
                                       total_num_remaining = NULL
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
                                         if(l$s$err$get_assertions_status()){
                                           l$s$err$assert_msg("'total_num_remaining' must equal 'get_next_count()'.",
                                                              private$total_num_remaining == next_count)
                                           l$s$err$assert_msg(paste0("'total_num_remaining' must equal the sum of all ",
                                                                     "entries in 'nums_remaining'."),
                                                              private$total_num_remaining == sum(as.integer(private$nums_remaining)))
                                           
                                           l$s$err$assert_msg("Length of 'queue' should equal length of 'nums_remaining' minus one.",
                                                              length(private$queue) == length(private$nums_remaining) - 1)
                                           
                                           is_locked <- self$is_locked()
                                           l$s$err$assert_msg(paste0("'is_locked' being TRUE and 'next_count' being zero must be ",
                                                                     "equivalent to both 'finished' being TRUE and 'get_num_remaining()' ",
                                                                     "being zero."),
                                                              l$s$mg$`%eq%`(is_locked && next_count == 0,
                                                                              finished),
                                                              l$s$mg$`%eq%`(finished,
                                                                            l$s$dts$equals(self$get_num_remaining(), 0)))
                                         }
                                       },
                                       
                                       
                                       #' An `Abstract_Iterator` subclass based on an internal `queue`.
                                       #' Provides the means to add additional elements to the internal queue
                                       #' via `appen_element` and `append_elements`. The `get_next()` method
                                       #' returns those appended elements in the order they were appended (FIFO).
                                       #' For each inserted element, the `length` parameter may be used to specify
                                       #' how often each element shall be returned.
                                       #' 
                                       #' May be `locked` either via the `lock` parameter of the `initialize` method or,
                                       #' at a later time, via the `lock()` method. After this iterator has been locked,
                                       #' further attempts to append elements result in an error.
                                       #' 
                                       #' The current `queue` length may be accessed via 
                                       #' the `get_next_count()` method.
                                       #' Until this iterator has been locked, the `get_length()` and `get_num_remaining()`
                                       #' methods return `NA` as these numbers are unknown as long as new elements may be added.
                                       #' After locking, `get_length()` returns the length the internal queue had at the moment
                                       #' of locking and `get_num_remaining()` returns the same value as `get_next_count()`. 
                                       #' 
                                       #' __IMPORTANT__: See documentation of this method in `utils_lang_r6_subclass_TEMPLATE.R`.
                                       #'
                                       #' @param assertions_status See `Abstract_Iterator`.
                                       #' @param elements Elements to add via `append_elements()`.
                                       #' @param lock Whether to lock this instance at the end 
                                       #'   of the initialization process. 
                                       #' @param lengths A non-negative integer of either length one or the same length as `elements`.
                                       #'   The first case is treated as equivalent to providing a vector of the same length as
                                       #'   `elements` where each entry equals the single entry in `lengths`.
                                       #'   The i-th entry in `lengths` holds the number of times the i-th element in `elements`
                                       #'   shall be returned in `get_next()`.
                                       #'   
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       initialize = function(assertions_status = FALSE,
                                                             elements = list(),
                                                             lengths = 1,
                                                             lock = FALSE){
                                         super$initialize(assertions_status = assertions_status, fixed_length = NA)
                                         
                                         local_env$.add_to_static_env(super$get_static_env())
                                         local_env$static_env$err$set_assertions_status(assertions_status)
                                         
                                         l$s$err$assert_msg("'lock' must be a logical of length one.",
                                                            l$s$tcs$has_length_1(lock, NA_on_fail = FALSE),
                                                            l$s$tcs$is_logical(lock,
                                                                               accept_NULL = FALSE,
                                                                               accept_NaN = FALSE,
                                                                               accept_NA = FALSE))
                                         private$queue <- queue()
                                         private$locked <- FALSE
                                         private$nums_remaining <- list(0)
                                         private$total_num_remaining <- 0
                                         
                                         self$append_elements(elements, lengths)
                                         if(lock){
                                           self$lock()
                                         }
                                       },
                                      
                                       finalize = function() {
                                         super$finalize()
                                       },
                                       
                                       #' Equivalent to calling `append_elements(list(element), list(length))`.
                                       #'
                                       #' @return
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       append_element = function(element,
                                                                 length = 1) {
                                         self$append_elements(list(element), list(length))
                                       },
                                       
                                       #' Adds the provided elements to the internal queue in the order
                                       #' they appear in `elements`.
                                       #' 
                                       #' This method produces an error if this instance has been locked
                                       #' (see `lock()` and `is_locked()`).
                                       #'
                                       #' @param elements A list coercible data structure containing zero or more elements
                                       #'   to add to the internal queue.
                                       #' @param lengths A non-negative integer of either length one or the same length as `elements`.
                                       #'   The first case is treated as equivalent to providing a vector of the same length as
                                       #'   `elements` where each entry equals the single entry in `lengths`.
                                       #'   The i-th entry in `lengths` holds the number of times the i-th element in `elements`
                                       #'   shall be returned in `get_next()`.
                                       #' @return
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       append_elements = function(elements,
                                                                  lengths = 1){
                                         l$s$err$stopifnot("Attempt to append iterators while 'is_locked()' is TRUE.",
                                                           !self$is_locked())
                                         
                                         elements <- as.list(elements)
                                         lengths <- as.list(lengths)
                                         l$s$err$assert_msg(paste0("'lengths' must either be of length one or of the same length ",
                                                                   "as 'elements'."),
                                                            l$s$tcs$has_length_1(lengths, 
                                                                                 NA_on_fail = FALSE) ||
                                                              l$s$tcs$has_length(lengths, 
                                                                                 len = length(elements), 
                                                                                 NA_on_fail = FALSE))
                                         len <- length(lengths)
                                         len_elems <- length(elements)
                                         if(len == 1){
                                           el <- lengths[[1]]
                                           lengths <- lapply(1:len_elems, FUN = function(i) {
                                             return(el)
                                           })
                                         }
                                         
                                         l$s$err$assert_msg(paste0("Length of 'elements' should equal the length of ",
                                                                   "'lengths' at this point."),
                                                            length(lengths) == len_elems)
                                         
                                         if(len_elems > 0){
                                           i <- 1
                                           while(i <= len_elems){
                                             if(lengths[[i]] == 0){
                                               lengths[[i]] <- NULL
                                               elements[[i]] <- NULL
                                               len_elems <- len_elems - 1
                                             }
                                             else {
                                               i <- i + 1
                                             }
                                           }
                                         }
                                         
                                         l$s$err$assert_msg(paste0("'lengths' must only contain integers bigger than zero ",
                                                                   "at this point."),
                                                            all(l$s$tcs$is_integer(lengths,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE,
                                                                                   lower_bound = 1,
                                                                                   lower_bound_inclusive = TRUE,
                                                                                   upper_bound = Inf,
                                                                                   upper_bound_inclusive = FALSE)))
                                         
                                         for(el in elements){
                                           pushback(private$queue, el)
                                         }
                                         
                                         private$nums_remaining <- c(private$nums_remaining, lengths)
                                         private$total_num_remaining <- private$total_num_remaining +
                                           sum(as.integer(lengths))
                                         
                                         self$.check_consistency()
                                       },
                                       
                                       #' Check whether this instance is locked.
                                       #' `append_element()` and `append_elements()`
                                       #' produce an error if and only if this instance has been locked
                                       #' via `lock()`.
                                       #'
                                       #' @return `TRUE` if and only if this instance has been locked
                                       #'   (i.e., `lock()` has been called at least once).
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       is_locked = function() {
                                         return(private$locked)
                                       },
                                       
                                       #' Lock this instance.
                                       #' `append_element()` and `append_elements()`
                                       #' produce an error if and only if this instance has been locked.
                                       #' 
                                       #' A call to this method has no effect if this iterator has 
                                       #' already been locked.
                                       #' 
                                       #' Note that once this instance has been locked, it is impossible
                                       #' to unlock it. `get_length()` will now return the
                                       #' length of the internal queue at the moment of locking.
                                       #' 
                                       #' Current lock status can be checked via `is_locked()`.
                                       #'
                                       #' @export
                                       #'
                                       #' @examples
                                       #' @md
                                       lock = function() {
                                         if(!private$locked){
                                           private$locked <- TRUE
                                           self$.set_fixed_length(private$total_num_remaining)
                                           self$.set_num_remaining(private$total_num_remaining)
                                         }
                                       },
                                       
                                       
                                       #' ----------- THE FOLLOWING METHODS __MAY__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                       
                                       .get_next_post = function(){
                                         super$.get_next_post()
                                       },
                                       
                                       #' ----------- THE FOLLOWING METHODS __MUST__ BE OVERRIDDEN BY INHERITING CLASSES --------
                                       
                                       .get_next = function(num) {
                                         if(num == 0){
                                           return(list())
                                         }
                                         else{
                                           l$s$err$assert_msg("'total_num_remaining' must be >= 'num'.",
                                                              private$total_num_remaining >= num)
                                           
                                           result <- vector(mode="list", length = num)
                                           
                                           num_retrieved <- 0
                                           while(num_retrieved < num){
                                             l$s$err$assert_msg(paste0("'nums_remaining' should contain at least one element."),
                                                                length(private$nums_remaining) > 0)
                                             if(private$nums_remaining[[1]] == 0){
                                               l$s$err$assert_msg(paste0("'num_remaining' should contain at least two ",
                                                                         "and the internal ",
                                                                         "queue at least one element at this point ."),
                                                                  length(private$nums_remaining) > 1,
                                                                  length(queue) > 0)
                                               private$current_element <- pop(private$queue)
                                               private$nums_remaining[[1]] <- NULL
                                             }
                                             
                                             retrievable <- min(private$nums_remaining[[1]], num - num_retrieved)
                                             l$s$err$assert_msg("'retrievable' must be >= 1 at this point.",
                                                                retrievable >= 1)
                                             
                                             result[(num_retrieved + 1):(num_retrieved + retrievable)] <- 
                                               private$current_element
                                             num_retrieved <- num_retrieved + retrievable
                                             private$nums_remaining[[1]] <- private$nums_remaining[[1]] - retrievable
                                             
                                             l$s$err$assert_msg("'nums_remaining[[1]]' must be >= 0 at this point.",
                                                                private$nums_remaining[[1]] >= 0)
                                           }
                                           private$total_num_remaining <- private$total_num_remaining - num_retrieved
                                           l$s$err$assert_msg("'total_num_remaining' must be >= 0 at this point.",
                                                              private$total_num_remaining >= 0)
                                           return(result)
                                         }
                                       },
                                       
                                       .validate_next = function(element) {
                                         return(TRUE)
                                       },
                                       
                                       .get_next_count = function() {
                                         return(private$total_num_remaining)
                                       },
                                       
                                       .finished = function() {
                                         return(self$is_locked() && l$s$dts$equals(self$get_num_remaining(), 0))
                                       }
                                       
                                     ),
                                     #-----end of public-----
                                     
                                     #' ----- Add active bindings ----
                                     active = list(
                                       
                                     )
                                     #-----end of active-----
        )
        
        #' A container class for a unified access to multiple
        #' `Abstract_Iterator` instances.
        Iterators <- R6Class("Iterators",
                               # ---- R6 inheritance -----
                               inherit = R6_Base,
                               
                               #' ---- Adapt R6 options --------
                               portable = TRUE,
                               cloneable = TRUE,
                               lock_class = TRUE,
                               lock_objects = TRUE,
                               
                               #' ----- Add private Fields & methods ----
                               private = list(
                                 iterators = NULL
                               ),
                               #-----end of private-----
                               
                               public = list(
                                 
                                 #' See `R6_Base` in `utils_lang_r6_baseclass.R`
                                 #' @export
                                 #' @md
                                 get_static_env = function(){
                                   local_env$static_env
                                 },
                                 
                                 #' A container class for a unified access to multiple
                                 #' `Abstract_Iterator` instances.
                                 #'
                                 #' @param assertions_status If `TRUE`, assertions are activated (see `set_assertion_status()`
                                 #'   in `utils_lang_error.R`).
                                 #' @param iterators A data structure of `length > 0` containing 
                                 #'   instances of `Abstract_Iterator`s. Iterators are stored in the order they appear in
                                 #'   this data structure.
                                 #' 
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 initialize = function(assertions_status = FALSE,
                                                       iterators){
                                   super$initialize()
                                   local_env$.add_to_static_env(super$get_static_env())
                                   local_env$static_env$err$set_assertions_status(assertions_status)
                                   
                                   #' Add your initialization code here here:
                                   
                                   for(it in iterators){
                                     l$s$err$assert_msg("Iterators must only contain elements that inherit from 'Abstract_Iterator'.",
                                                        is.R6(it), inherits(it, "Abstract_Iterator"))
                                   }
                                   private$iterators <- array(iterators, dim = length(iterators))
                                 },
                                 
                                 #' Assertions that the provided number is a valid iterator ID.
                                 #'
                                 #' @param i An iterator ID.
                                 #'
                                 #' @return
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 .check_index = function(i){
                                   l$s$err$assert_msg("Index out of bounds.",
                                                      l$s$tcs$has_length_1(i, NA_on_fail = FALSE),
                                                      l$s$tcs$is_integer(i,
                                                                         accept_NULL = FALSE,
                                                                         accept_NaN = FALSE,
                                                                         accept_NA = FALSE,
                                                                         lower_bound = 1,
                                                                         lower_bound_inclusive = TRUE,
                                                                         upper_bound = length(private$iterators),
                                                                         upper_bound_inclusive = TRUE))
                                 },
                                 
                                 finalize = function() {
                                   super$finalize()
                                   
                                   #' Add your finalization code here:
                                 },
                                 
                                 #'
                                 #' @return The number of iterators this container holds. Number indicates the maximal
                                 #'   index to be used when accessing internal iterators' methods.
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 get_num_iterators = function(){
                                   return(length(private$iterators))
                                 }, 
                                 
                                 #' Method to directly access the internal iterators. Use with caution,
                                 #' as subclasses extending `Iterators` might depend on
                                 #' exclusive access to their iterators in order to keep some form of
                                 #' consistent inner state.
                                 #'
                                 #' @param i An integer between `1` and `get_num_iterators()`.
                                 #'
                                 #' @return The `i`-th internal iterator.
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 .get_iterator = function(i){
                                   self$.check_index(i)
                                   return(private$iterators[[i]])
                                 },
                                 
                                 #' @param i An integer between `1` and `get_num_iterators()`. 
                                 #'
                                 #' @return The result of a call to the `i`-th iterator's `get_length()` method.
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 get_length = function(i){
                                   self$.check_index(i)
                                   return(private$iterators[[i]]$get_length())
                                 },
                                 
                                 #' @param i An integer between `1` and `get_num_iterators()`. 
                                 #'
                                 #' @return The result of a call to the `i`-th iterator's `get_num_remaining()` method.
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 get_num_remaining = function(i){
                                   self$.check_index(i)
                                   return(private$iterators[[i]]$get_num_remaining())
                                 },
                                 
                                 #' @param i An integer between `1` and `get_num_iterators()`. 
                                 #' @param num The `num` argument to be provided to the `i`-th iterator's
                                 #'   `get_length()` method.
                                 #' 
                                 #' @return The result of a call to the `i`-th iterator's `get_length(num)` method.
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 get_next = function(i, num = self$get_next_count(i)) {
                                   self$.check_index(i)
                                   return(private$iterators[[i]]$get_next(num))
                                 },
                                 
                                 #'
                                 #' @return A list of length `get_iterators_num()` containing
                                 #'   `get_next(i)` at its i-th entry.
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 get_next_all = function(){
                                   num_its <- self$get_num_iterators()
                                   if(num_its == 0){
                                     result <- list()
                                   }
                                   else{
                                     result <- lapply(1:num_its,
                                                      FUN = function(i){
                                                        return(self$get_next(i))
                                                      })
                                   }
                                   return(result)
                                 },
                                 
                                 #' @param i An integer between `1` and `get_num_iterators()`. 
                                 #'
                                 #' @return The result of a call to the `i`-th iterator's `get_next_count()` method.
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 get_next_count = function(i) {
                                   self$.check_index(i)
                                   return(private$iterators[[i]]$get_next_count())
                                 },
                                 
                                 #' @param i An integer between `1` and `get_num_iterators()`. 
                                 #'
                                 #' @return The result of a call to the `i`-th iterator's `finished()` method.
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 finished = function(i){
                                   self$.check_index(i)
                                   return(private$iterators[[i]]$finished())
                                 },
                                 
                                 #'
                                 #' @return `TRUE` if and only if __all__ internal interators' `finished()`
                                 #'   method returns `TRUE`.
                                 #' @export
                                 #'
                                 #' @examples
                                 #' @md
                                 all_finished = function(){
                                   return(all(sapply(private$iterators, FUN = function(it){
                                     return(it$finished())
                                   })))
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