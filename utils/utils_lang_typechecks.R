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
#' @title  Collection of methods and/or fields for type and boundary checking.
#' @description Contains a single [get_utils_typechecks_env()] method
#'   that returns an environment containing those methods.
#' @md

# Include guard
if (!exists("UTILS_TYPECHECKS_R", inherits = FALSE)) {
  UTILS_TYPECHECKS_R = TRUE
  
  #' Note: None of the methods here perform type and/or boundary checking for their own parameters! Therefore you
  #' have to make sure that they conform to the documentation.
  #' @return An environment containing one or multiple methods and/or fields for type and boundary checking.
  #' @export
  #'
  #' @examples
  #' @md
  get_utils_lang_typechecks_env <- function() {
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
    
    #' @param elem Element to be checked.
    #' @param type Character with expected type of element (e.g., "numeric" or "logical").
    #' @param accept_NULL If NULL element is accepted.
    #' @param accept_NaN If NaN element is accepted.
    #' @param accept_NA  If NA element is accepted.
    #' @param additional_conditions A boolean expression.
    #' 
    #' @return `TRUE` if and only if __ALL__ of the following holds:
    #'   * If `elem` is __NOT__ `NULL`, then `length(elem) == 1`
    #'   * If `elem` is `NULL`, then `accept_NULL == TRUE`
    #'   * If `elem` is `NaN`, then `accept_NaN == TRUE`
    #'   * If `elem` is `NA`, then `accept_NA == TRUE`
    #'   * If `elem` is __NEITHER__ `NULL`, `NaN` __NOR__ `NA`, then it
    #'     is of type `<type>` (i.e., a call to `is.<type>(elem)` returns `TRUE`)
    #'   * `additional_conditions` evaluates to `TRUE` 
    #'   
    #'   Returns `FALSE` in every other case.
    #' @export
    #'
    #' @examples
    #' @md
    .accept_elem <- function(elem, type,
                 accept_NULL = FALSE,
                 accept_NaN = FALSE,
                 accept_NA = FALSE,
                 additional_conditions = TRUE){
      if(is.null(elem)){
        return(accept_NULL)
      }
      else if(!result_env$has_length_1(elem, NA_on_fail=FALSE)){
        return(FALSE) 
      }
      else if(result_env$is_special(elem, 
                               accept_NULL = TRUE,
                               accept_NaN = TRUE,
                               accept_NA = TRUE,
                               NA_on_NULL = TRUE)){ # At this point, elem cannot be NULL
        # If elem is a special value, is it one of the accepted ones?
        return(result_env$is_special(elem, 
                                     accept_NULL = accept_NULL, # If elem is a list of length 1 holding a NULL, this becomes relevant
                                     accept_NaN = accept_NaN,
                                     accept_NA = accept_NA))
      }
      else{
        return(do.call(paste0("is.", type), list(elem)) && additional_conditions)
      }
    }
    
    #' Unsafe check if `elem` lies inside provided bounds. Unsafe means that method does not check
    #' whether provided `elem` fulfills constraints described in its parameter documentation.
    #'
    #' @param elem A `numeric` length 1 that is neither `NA` nor `NaN`.
    #' @param lower_bound See `lower_bound_inclusive`.
    #' @param lower_bound_inclusive `elem` should be `>`, if `lower_bound_inclusive == FALSE`, or
    #'    `>=`, if `lower_bound_inclusive == TRUE`, than `lower_bound`.
    #' @param upper_bound See `upper_bound_inclusive`.
    #' @param upper_bound_inclusive `elem` should be `<`, if `upper_bound_inclusive == FALSE`, or
    #'    `<=`, if `upper_bound_inclusive == TRUE`, than `upper_bound`.
    #'
    #' @return `TRUE` if and only if `elem` lies within the provided bounds. Undefined if `elem` is not
    #'   a `numeric` of length 1 or is `NA` or `NaN`.
    #' @export
    #'
    #' @examples
    #' @md
    .is_bounded_elem_unsafe <- function(elem,
                                 lower_bound = -Inf,
                                 lower_bound_inclusive = FALSE,
                                 upper_bound = Inf,
                                 upper_bound_inclusive = FALSE) {
      
      return((lower_bound_inclusive && elem >= lower_bound ||
                !lower_bound_inclusive && elem > lower_bound) &&
               (upper_bound_inclusive && elem <= upper_bound ||
                  !upper_bound_inclusive && elem < upper_bound))
      
    }
    
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
        #' Checks which element in `x` is bounded by the provided bounds.
        #'
        #' @param x A datastructure.
        #' @param lower_bound See `lower_bound_inclusive`.
        #' @param lower_bound_inclusive Each element in `x` should be `>`, if `lower_bound_inclusive == FALSE`, or
        #'    `>=`, if `lower_bound_inclusive == TRUE`, than `lower_bound`.
        #' @param upper_bound See `upper_bound_inclusive`.
        #' @param upper_bound_inclusive Each element in `x` should be `<`, if `upper_bound_inclusive == FALSE`, or
        #'    `<=`, if `upper_bound_inclusive == TRUE`, than `upper_bound`.
        #' @param NA_on_fail See return description.
        #'
        #' @return A logical vector `result` of the same length as `x`. Its i-th entry corresponds to the i-th entry in `x`.
        #'   If `x[[i]]` is neither a `numeric` vector of length 1 or is either `NA` or `NaN`, then `result[i]` is
        #'   set to `NA`, if `NA_on_fail == TRUE`, and to `FALSE`, if `NA_on_fail == FALSE`.
        #'   Otherwise, `result[i]` is set to `TRUE` if and only if `x[[i]]` lies within the provided bounds.
        #'   In every other case `result[i]` is set to `FALSE`.
        #' @export
        #'
        #' @examples
        #' @md
        is_bounded <- function(x,
                   lower_bound = -Inf,
                   lower_bound_inclusive = FALSE,
                   upper_bound = Inf,
                   upper_bound_inclusive = FALSE,
                   NA_on_fail = FALSE) {
          
          result <- as.logical(lapply(x, FUN = function(elem){
            if(!local_env$.accept_elem(elem, "numeric",
                                       accept_NULL = FALSE,
                                       accept_NaN = FALSE,
                                       accept_NA = FALSE,
                                       additional_conditions = TRUE)){
              if(NA_on_fail){
                return(NA)
              }
              else{
                return(FALSE)
              }
            }
            else{
              return(local_env$.is_bounded_elem_unsafe(elem,
                                                       lower_bound = lower_bound,
                                                       lower_bound_inclusive = lower_bound_inclusive,
                                                       upper_bound = upper_bound,
                                                       upper_bound_inclusive = upper_bound_inclusive))
            }
          }))
          
          return(result)
        }
        
        #' Checks the length of the provided object.
        #'
        #' @param x A datastructure.
        #' @param len An integer.
        #' @param NA_on_fail See return derscription.
        #'
        #' @return If no `length()` method is defined for `x`, then the method returns
        #'   `NA`, if `NA_on_fail == TRUE`, and `FALSE`, if `NA_on_fail == FALSE`.
        #'   Otherwise it returns the result of `length(x) == len`.
        #' @export
        #'
        #' @examples
        #' @md
        has_length <- function(x, len, NA_on_fail = FALSE){
          err_handler <- function(e){ # If length not defined for x
            if(NA_on_fail){
              return(NA)
            }
            else{
              return(FALSE)
            }
          }
          
          tryCatch({
            return(length(x) == len)
          }, warning = err_handler, error = err_handler)
        }
        
        #' Wrapper for [has_length()] equivalent to a call to `has_length(x, len = 0, NA_on_fail = NA_on_fail)`.
        #' @md
        has_length_0 <- function(x, NA_on_fail = FALSE){
          return(result_env$has_length(x, len = 0, NA_on_fail = NA_on_fail))
        }
        
        #' Wrapper for [has_length()] equivalent to a call to `has_length(x, len = 1, NA_on_fail = NA_on_fail)`.
        #' @md
        has_length_1 <- function(x, NA_on_fail = FALSE){
          return(result_env$has_length(x, len = 1, NA_on_fail = NA_on_fail))
        }
        
        #' Checks which elements in `x` are `NULL`, `NaN` or `NA`.
        #'
        #' @param x A datastructure.
        #' @param accept_NULL Check for `NULL` elements.
        #' @param accept_NaN Check for `NaN` elements.
        #' @param accept_NA Check for `NA` elements that are __NOT__ `NaN`.
        #' @param NA_on_NULL See return description.
        #'
        #' @return If `x` itself is `NULL` and `NA_on_NULL == TRUE`, the method returns `NA`.
        #'   If `NA_on_NULL == FALSE`, the method returns a logical vector `result` of the same length as
        #'   `x` (implying that it returns `NULL` if `x` itself is `NULL` as `x` then has length zero)
        #'   with `result[i]` set to `TRUE` if __at least one__ of the following holds:
        #'     * `accept_NULL == TRUE` __AND__ `x[[i]]` is `NULL`
        #'     * `accept_NaN == TRUE` __AND__ `x[[i]]` is `NaN`
        #'     * `accept_NA == TRUE` __AND__ `x[[i]]` is `NA` but not `NaN` (note that, in R, `is.nan` implies `is.na`)
        #'   In every other case, `result[i]` is set to `FALSE`.
        #' @export
        #'
        #' @examples
        #' @md
        is_special <- function(x,
                   accept_NULL = TRUE,
                   accept_NaN = TRUE,
                   accept_NA = TRUE,
                   NA_on_NULL = FALSE){
          
          if(is.null(x) && NA_on_NULL){
            return(NA)
          }
          
          result <- accept_NULL & as.logical(lapply(x, FUN = is.null))
          
          result <- result | accept_NaN & 
            as.logical(lapply(x, FUN = function(elem) {
                if(!is.null(elem) && is.atomic(elem)){
                  return(is.nan(elem))
                }
                else{
                  return(FALSE)
                }
              }))
          
          result <- result | accept_NA & 
            as.logical(lapply(x, FUN = function(elem) {
                if(!is.null(elem) && is.atomic(elem)){
                  return(!is.nan(elem) && is.na(elem))
                }
                else{
                  return(FALSE)
                }
              }))
          
          return(result)
        }
        
        #' Checks which entries in `x` are negatively infinite.
        #'
        #' @param x A datastructure.
        #' @param NA_on_fail See return description.
        #'
        #' @return A logical vector `result` of the same length as `x`. Its i-th entry corresponds to the i-th entry in `x`.
        #'   If `x[[i]]` is neither a `numeric` vector of length 1 or is either `NA` or `NaN`, then `result[i]` is
        #'   set to `NA`, if `NA_on_fail == TRUE`, and to `FALSE`, if `NA_on_fail == FALSE`.
        #'   Otherwise, `result[i]` is set to true if and only if `x[[i]]` is negatively infinite (i.e. `x[[i]] < 0`
        #'   and `is.infinite(x[[i]]) == TRUE`).
        #' @export
        #'
        #' @examples
        #' @md
        is_neg_infinite <- function(x, NA_on_fail = FALSE) {
          result <- as.logical(lapply(x, FUN = function(elem){
            if(!local_env$.accept_elem(elem, "numeric",
                                        accept_NULL = FALSE,
                                        accept_NaN = FALSE,
                                        accept_NA = FALSE,
                                        additional_conditions = TRUE)){
              if(NA_on_fail){
                return(NA)
              }
              else{
                return(FALSE)
              }
            }
            else{
              return(is.infinite(elem) && elem < 0)
            }
          }))
          
          return(result)
        }
        
        #' Checks which entries in `x` are positively infinite.
        #'
        #' @param x A datastructure.
        #' @param NA_on_fail See return description.
        #'
        #' @return A logical vector `result` of the same length as `x`. Its i-th entry corresponds to the i-th entry in `x`.
        #'   If `x[[i]]` is neither a `numeric` vector of length 1 or is either `NA` or `NaN`, then `result[i]` is
        #'   set to `NA`, if `NA_on_fail == TRUE`, and to `FALSE`, if `NA_on_fail == FALSE`.
        #'   Otherwise, `result[i]` is set to true if and only if `x[[i]]` is positively infinite (i.e. `x[[i]] > 0`
        #'   and `is.infinite(x[[i]]) == TRUE`).
        #' @export
        #'
        #' @examples
        #' @md
        is_pos_infinite <- function(x, NA_on_fail = FALSE) {
          result <- as.logical(lapply(x, FUN = function(elem){
            if(!local_env$.accept_elem(elem, "numeric",
                                        accept_NULL = FALSE,
                                        accept_NaN = FALSE,
                                        accept_NA = FALSE,
                                        additional_conditions = TRUE)){
              if(NA_on_fail){
                return(NA)
              }
              else{
                return(FALSE)
              }
            }
            else{
              return(is.infinite(elem) && elem > 0)
            }
          }))
          
          return(result)
        }
                
        #' A wrapper for [is_numeric()] with `accept_non_integer` set to `FALSE`.
        is_integer <- function(x,
                               accept_NULL = FALSE,
                               accept_NaN = FALSE,
                               accept_NA = FALSE,
                               lower_bound = -Inf,
                               lower_bound_inclusive = FALSE,
                               upper_bound = Inf,
                               upper_bound_inclusive = FALSE){
          return(result_env$is_numeric(x,
                                       accept_NULL = accept_NULL,
                                       accept_NaN = accept_NaN,
                                       accept_NA = accept_NA,
                                       lower_bound = lower_bound,
                                       lower_bound_inclusive = lower_bound_inclusive,
                                       upper_bound = upper_bound,
                                       upper_bound_inclusive = upper_bound_inclusive,
                                       accept_non_integer = FALSE))
        }
        
        #' Checks which entries of `x` are of type `numeric`, `NULL`, `NaN` or `NA` and
        #' bounded within certain bounds.
        #'
        #' @param x A datastructure.
        #' @param accept_NULL Check for `NULL` elements.
        #' @param accept_NaN Check for `NaN` elements.
        #' @param accept_NA Check for `NA` elements.
        #' @param lower_bound See `lower_bound_inclusive`.
        #' @param lower_bound_inclusive Each element in `x` should be `>`, if `lower_bound_inclusive == FALSE`, or
        #'    `>=`, if `lower_bound_inclusive == TRUE`, than `lower_bound`.
        #' @param upper_bound See `upper_bound_inclusive`.
        #' @param upper_bound_inclusive Each element in `x` should be `<`, if `upper_bound_inclusive == FALSE`, or
        #'    `<=`, if `upper_bound_inclusive == TRUE`, than `upper_bound`. 
        #' @param accept_non_integer Elements in `x` should be integers.
        #'
        #' @return A logical vector `result` of the same length as `x`. Its i-th entry corresponds to the i-th entry in `x`.
        #'   `result[i]` is set to `TRUE` if and only if __ALL__ of the following holds:
        #'   * If `x[[i]]` is __NOT__ `NULL`, then `length(x[[i]]) == 1`
        #'   * If `x[[i]]` is `NULL`, then `accept_NULL == TRUE`
        #'   * If `x[[i]]` is `NaN`, then `accept_NaN == TRUE`
        #'   * If `x[[i]]` is `NA`, then `accept_NA == TRUE`
        #'   * If `x[[i]]` is __NEITHER__ `NULL`, `NaN` __NOR__ `NA`, then it
        #'     is of type `numeric` (i.e., a call to `is.numeric(x[[i]])` returns `TRUE`) __AND__
        #'     `x[[i]]` lies within the provided bounds.
        #'     
        #'   Returns `FALSE` in every other case.
        #' @export
        #'
        #' @examples
        #' @md
        is_numeric <- function(x,
                               accept_NULL = FALSE,
                               accept_NaN = FALSE,
                               accept_NA = FALSE,
                               lower_bound = -Inf,
                               lower_bound_inclusive = FALSE,
                               upper_bound = Inf,
                               upper_bound_inclusive = FALSE,
                               accept_non_integer = TRUE) {

          return(result_env$is_generic(x,"numeric",
                                       accept_NULL = accept_NULL,
                                       accept_NaN = accept_NaN,
                                       accept_NA = accept_NA,
                                       additional_filter = function(elem){
                                         return(local_env$.is_bounded_elem_unsafe(elem,
                                                                      lower_bound = lower_bound,
                                                                      lower_bound_inclusive = lower_bound_inclusive,
                                                                      upper_bound = upper_bound,
                                                                      upper_bound_inclusive = upper_bound_inclusive) &&
                                                  (accept_non_integer || is.infinite(elem) || (elem %% 1 == 0)))
                                       }))
        }
        
        #' Checks which entries of `x` are of type `logical`, `NULL`, `NaN` or `NA`.
        #'
        #' @param x A datastructure.
        #' @param accept_NULL Check for `NULL` elements.
        #' @param accept_NaN Check for `NaN` elements.
        #' @param accept_NA Check for `NA` elements.
        #'
        #' @return A logical vector `result` of the same length as `x`. Its i-th entry corresponds to the i-th entry in `x`.
        #'   `result[i]` is set to `TRUE` if and only if __ALL__ of the following holds:
        #'   * If `x[[i]]` is __NOT__ `NULL`, then `length(x[[i]]) == 1`
        #'   * If `x[[i]]` is `NULL`, then `accept_NULL == TRUE`
        #'   * If `x[[i]]` is `NaN`, then `accept_NaN == TRUE`
        #'   * If `x[[i]]` is `NA`, then `accept_NA == TRUE`
        #'   * If `x[[i]]` is __NEITHER__ `NULL`, `NaN` __NOR__ `NA`, then it
        #'     is of type `logical` (i.e., a call to `is.logical(x[[i]])` returns `TRUE`)
        #'     
        #'   Returns `FALSE` in every other case.
        #' @export
        #'
        #' @examples
        #' @md
        is_logical <- function(x,
                               accept_NULL = FALSE,
                               accept_NaN = FALSE,
                               accept_NA = FALSE) {
          
          return(result_env$is_generic(x,"logical",
                                       accept_NULL = accept_NULL,
                                       accept_NaN = accept_NaN,
                                       accept_NA = accept_NA))
        }
        
        #' Checks which entries of `x` are of type `character`, `NULL`, `NaN` or `NA`.
        #'
        #' @param x A datastructure.
        #' @param accept_NULL Check for `NULL` elements.
        #' @param accept_NaN Check for `NaN` elements.
        #' @param accept_NA Check for `NA` elements.
        #'
        #' @return A logical vector `result` of the same length as `x`. Its i-th entry corresponds to the i-th entry in `x`.
        #'   `result[i]` is set to `TRUE` if and only if __ALL__ of the following holds:
        #'   * If `x[[i]]` is __NOT__ `NULL`, then `length(x[[i]]) == 1`
        #'   * If `x[[i]]` is `NULL`, then `accept_NULL == TRUE`
        #'   * If `x[[i]]` is `NaN`, then `accept_NaN == TRUE`
        #'   * If `x[[i]]` is `NA`, then `accept_NA == TRUE`
        #'   * If `x[[i]]` is __NEITHER__ `NULL`, `NaN` __NOR__ `NA`, then it
        #'     is of type `character` (i.e., a call to `is.character(x[[i]])` returns `TRUE`)
        #'     
        #'   Returns `FALSE` in every other case.
        #' @export
        #'
        #' @examples
        #' @md
        is_character <- function(x,
                                 accept_NULL = FALSE,
                                 accept_NaN = FALSE,
                                 accept_NA = FALSE) {
          return(result_env$is_generic(x,"character",
                                       accept_NULL = accept_NULL,
                                       accept_NaN = accept_NaN,
                                       accept_NA = accept_NA))
        }
        
        #' Checks which entries of `x` are of type `<type>`, `NULL`, `NaN` or `NA`.
        #'
        #' @param x A datastructure.
        #' @param type Character with expected type of elements in `x` (e.g., "numeric" or "logical").
        #' @param accept_NULL Check for `NULL` elements.
        #' @param accept_NaN Check for `NaN` elements.
        #' @param accept_NA Check for `NA` elements.
        #' @param additional_filter A method that is called on each element of `x` 
        #'   and that must return a `logical` of length one. May be used to empose additional
        #'   constraints. See return description for more details.
        #'
        #' @return A logical vector `result` of the same length as `x`. Its i-th entry corresponds to the i-th entry in `x`.
        #'   `result[i]` is set to `TRUE` if and only if __ALL__ of the following holds:
        #'   * If `x[[i]]` is __NOT__ `NULL`, then `length(x[[i]]) == 1`
        #'   * If `x[[i]]` is `NULL`, then `accept_NULL == TRUE`
        #'   * If `x[[i]]` is `NaN`, then `accept_NaN == TRUE`
        #'   * If `x[[i]]` is `NA`, then `accept_NA == TRUE`
        #'   * If `x[[i]]` is __NEITHER__ `NULL`, `NaN` __NOR__ `NA`, then it
        #'     is of type `<type>` (i.e., a call to `is.<type>(x[[i]])` returns `TRUE`)
        #'   __AND__ `additional_filter(x[[i]]`) returns `TRUE`
        #'     
        #'   Returns `FALSE` in every other case.
        #' @export
        #'
        #' @examples
        #' @md
        is_generic <- function(x,
                               type,
                               accept_NULL = FALSE,
                               accept_NaN = FALSE,
                               accept_NA = FALSE,
                               additional_filter = function(elem){return(TRUE)}){
          result <- as.logical(lapply(x, FUN = function(elem){
            return(local_env$.accept_elem(elem, type,
                                          accept_NULL = accept_NULL,
                                          accept_NaN = accept_NaN,
                                          accept_NA = accept_NA,
                                          additional_conditions = additional_filter(elem)))
          }))
          
          return(result)
        }
      },
      envir = result_env)
    lockEnvironment(local_env, bindings = TRUE)
    invisible(result_env)
  }
} # End of include guard