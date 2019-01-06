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
#' @title  Collection of subclasses of `R6_Base` from `utils_lang_r6_baseclass.R` for random color generation.
#' @description Contains a single [get_<name>_env()] method
#'   that returns an environment containing the `R6` constructor objects of these subclasses.
#' @md

# Include guard
if (!exists("UTILS_COLOR_R", inherits = FALSE)) {
  UTILS_COLOR_R = TRUE
  
  #' @return An environment containing one or multiple constructor objects for `R6` subclasses of 
  #'         `R6_Base` from `utils_lang_r6_baseclass.R`. These provide functionality for random color generation.
  #' @export
  #'
  #' @examples
  #' @md
  get_utils_color_env <- function() {
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
                       file.path(".", "utils", "utils_lang_error.R"), 
                       file.path(".", "utils", "utils_lang_typechecks.R"))   # e.g.: c(file.path(".", "utils", "utils_math_general.R"))
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
    #' Add one or multiple `R6_Base` subclass constuctor objects to the `evalq` expression below.
    result_env <- new.env()
    evalq(
      {
        Random_Color_Iterator <- R6Class("Random_Color_Iterator",
                               # ---- R6 inheritance -----
                               #' __Optional__: Change to inherit from a different parent class.
                               #'                     __IMPORTANT__: The parent class MUST either directly or indirectly
                               #'                                    inherit from `R6_Base`!
                               inherit = l$s$it$Abstract_Iterator,
                               
                               #' ---- Adapt R6 options --------
                               portable = TRUE,
                               cloneable = TRUE,
                               lock_class = TRUE,
                               lock_objects = TRUE,
                               
                               #' ----- Add private Fields & methods ----
                               private = list(
                                 rnd_seed = NULL,
                                 last_rnd_state = NULL,
                                 
                                 hue_intervals = NULL,
                                 
                                 saturation = NULL,
                                 value = NULL,
                                 
                                 first_call = NULL,
                                 
                                 transformToColor = function(hues){
                                   h <- as.vector(hues, mode = "numeric")
                                   s <- rep(private$saturation, times = length(h))
                                   v <- rep(private$value, times = length(h))
                                   
                                   return(hsv(h = h, s = s, v = v))
                                 }
                               ),
                               #-----end of private-----
                               
                               #' ----- Add public fields & methods ----
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
                                    
                                    initialize = function(assertions_status = FALSE,
                                                          rnd_seed = NULL,
                                                          saturation = 1.0,
                                                          value = 1.0){
                                      super$initialize(assertions_status = assertions_status, fixed_length = Inf)
                                      local_env$.add_to_static_env(super$get_static_env())
                                      local_env$static_env$err$set_assertions_status(assertions_status)
                                      
                                      if(l$s$err$get_assertions_status()){
                                        l$s$err$assert_msg("'rnd_seed' must be NULL or a finite integer of length 1.",
                                                           is.null(rnd_seed) ||
                                                             l$s$tcs$has_length_1(rnd_seed, NA_on_fail = FALSE) &&
                                                             l$s$tcs$is_integer(rnd_seed,
                                                                                accept_NULL = FALSE,
                                                                                accept_NaN = FALSE,
                                                                                accept_NA = FALSE,
                                                                                lower_bound = -Inf,
                                                                                lower_bound_inclusive = FALSE,
                                                                                upper_bound = Inf,
                                                                                upper_bound_inclusive = FALSE))
                                        
                                        l$s$err$assert_msg("'saturation' must be a number of length 1 from the interval [0,1].",
                                                           l$s$tcs$has_length_1(saturation, NA_on_fail = FALSE),
                                                           l$s$tcs$is_numeric(saturation,
                                                                              accept_NULL = FALSE,
                                                                              accept_NaN = FALSE,
                                                                              accept_NA = FALSE,
                                                                              lower_bound = 0,
                                                                              lower_bound_inclusive = TRUE,
                                                                              upper_bound = 1,
                                                                              upper_bound_inclusive = TRUE,
                                                                              accept_non_integer = TRUE))
                                        
                                        l$s$err$assert_msg("'value' must be a number of length 1 from the interval [0,1].",
                                                           l$s$tcs$has_length_1(value, NA_on_fail = FALSE),
                                                           l$s$tcs$is_numeric(value,
                                                                              accept_NULL = FALSE,
                                                                              accept_NaN = FALSE,
                                                                              accept_NA = FALSE,
                                                                              lower_bound = 0,
                                                                              lower_bound_inclusive = TRUE,
                                                                              upper_bound = 1,
                                                                              upper_bound_inclusive = TRUE,
                                                                              accept_non_integer = TRUE))
                                      }
                                      
                                      private$rnd_seed <- rnd_seed
                                      private$saturation <- saturation
                                      private$value <- value
                                      private$first_call <- TRUE
                                      
                                      if(!is.null(private$rnd_seed)){
                                        glob_env <- globalenv()
                                        old_rnd_state <- get(x = ".Random.seed", envir = glob_env)
                                        set.seed(private$rnd_seed)
                                      }
                                      
                                      hue_start <- runif(n = 1, min = 0, max = 1)
                                      #' start & length
                                      private$hue_intervals <- list(c(hue_start, 1))
                                      
                                      if(!is.null(private$rnd_seed)){
                                        private$last_rnd_state <- get(x = ".Random.seed", envir = glob_env)
                                        assign(x = ".Random.seed", value= old_rnd_state, envir = glob_env)
                                      }
                                    },
                                    
                                    
                                    
                                    #' See documentation in `Abstract_Iterator`.
                                    #' @export
                                    #' @md
                                    finalize = function() {
                                      super$finalize()
                                      
                                    },
                                    
                                    
                                    .get_next_post = function(){
                                      super$.get_next_post()
                                    },
                                    
                                    
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
                                      
                                      if(num == 0){
                                        return(list())
                                      }
                                      
                                      # This is actually not necessary at the moment -> no random operations
                                      # but it might change in the future
                                      if(!is.null(private$rnd_seed)){
                                        l$s$err$assert_msg("Inconsistent state.",
                                                           !is.null(private$last_rnd_state))
                                        glob_env <- globalenv()
                                        old_rnd_state <- get(x = ".Random.seed", envir = glob_env)
                                        assign(x = ".Random.seed", value= private$last_rnd_state, envir = glob_env)
                                      }
                                      
                                      l$s$err$assert_msg("Inconsistent state.",
                                                         length(private$hue_intervals) > 0)
                                      
                                      result <- list()
                                      original_num <- num
                                      if(private$first_call){
                                        l$s$err$assert_msg("Inconsistent state.",
                                                           length(private$hue_intervals) == 1)
                                        
                                        result <- append(result, private$hue_intervals[[1]][1])
                                        num <- num - 1
                                      }
                                      
                                      if(num > 0){
                                        num_remaining <- num
                                        
                                        num_splits_per_interval <- rep(x = 0, times = length(private$hue_intervals))
                                        
                                        new_hue_intervals <- list()
                                        
                                        # Choose next interval at random
                                        if(num <= length(private$hue_intervals)){
                                          num_splits_per_interval[sample(1:length(private$hue_intervals), size = num)] <- 1
                                          num_remaining <- num_remaining - num
                                        }
                                        else{
                                          idx <- sample(1:length(private$hue_intervals))
                                          for(i in idx){
                                            #' Num splits proportional to interval lengths, sum of lengths = 1
                                            num_splits_per_interval[i] <- min(num_remaining, ceiling(num * private$hue_intervals[[i]][2]))
                                            num_remaining <- num_remaining - num_splits_per_interval[i]
                                            
                                            if(num_remaining == 0){
                                              break
                                            }
                                            
                                            l$s$err$assert_msg("Inconsistent state.",
                                                               num_remaining >= 0)
                                          }
                                        }
                                        
                                        l$s$err$assert_msg("Inconsistent state.",
                                                           num_remaining == 0,
                                                           sum(num_splits_per_interval) == num)
                                        
                                        for(i in 1:length(private$hue_intervals)){
                                          old_start <- private$hue_intervals[[i]][1]
                                          old_len <- private$hue_intervals[[i]][2]
                                          
                                          
                                          new_len <- old_len / (num_splits_per_interval[i] + 1)
                                          
                                          new_hue_intervals <- append(new_hue_intervals, list(c(old_start, new_len)))
                                          
                                          if(num_splits_per_interval[i] > 0){
                                            for(j in 1:num_splits_per_interval[i]){
                                              new_start <- (old_start + j * new_len) %% 1
                                              new_hue_intervals <- append(new_hue_intervals, list(c(new_start, new_len)))
                                              result <- append(result, new_start)
                                            }
                                          }
                                        }
                                        l$s$err$assert_msg("Inconsistent state.",
                                                           length(new_hue_intervals) == length(private$hue_intervals) + num)
                                        
                                        private$hue_intervals <- new_hue_intervals
                                      }
                                      
                                      l$s$err$assert_msg("Inconsistent state.",
                                                         length(result) == original_num)
                                    
                                      result <- private$transformToColor(result)
                                      private$first_call <- FALSE
                                      result <- sample(result)
                                      
                                      if(!is.null(private$rnd_seed)){
                                        private$last_rnd_state <- get(x = ".Random.seed", envir = glob_env)
                                        assign(x = ".Random.seed", value= old_rnd_state, envir = glob_env)
                                      }
                                    
                                      return(result)
                                    },
                                    
                                    .validate_next = function(element) {
                                      return(TRUE)
                                    },
                                    
                                    #' Get the number of currently retrievable elements.
                                    #'
                                    #' @return `.Machine$integer.max`
                                    #' @export
                                    #'
                                    #' @examples
                                    #' @md
                                    .get_next_count = function() {
                                      return(.Machine$integer.max)
                                    },
                                    
                                    .finished = function() {
                                      return(FALSE)
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