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
#' @title  Collection of methods and/or fields for inspection and manipulation of data structures (e.g. pattern detection, etc.).
#' @description Contains a single [get_utils_data_tools_env] method
#'   that returns an environment containing those methods.
#' @md

# Include guard
if (!exists("UTILS_DATA_TOOLS_R", inherits = FALSE)) {
  UTILS_DATA_TOOLS_R = TRUE
  
  #' @return An environment containing one or multiple methods and/or fields for inspection 
  #'   and manipulation of data structures (e.g. pattern detection, etc.).
  #' @export
  #'
  #' @examples
  #' @md
  get_utils_data_tools_env <- function() {
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
    other_scripts <- c(file.path(".","utils","utils_lang_error.R"),
                       file.path(".","utils","utils_lang_typechecks.R"))   # e.g.: c(file.path(".", "utils", "utils_math_general.R"))
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
    assign("utils_lang_error", get_utils_lang_error_env(), envir = other_env)
    assign("utils_lang_typechecks", get_utils_lang_typechecks_env(), envir = other_env)
    
    #' Short-hand variants of the above:
    assign("err", other_env$utils_lang_error, envir = other_env)
    assign("tcs", other_env$utils_lang_typechecks, envir = other_env)
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

    l <- local_env
    o <- other_env
    
    # -------- Create private helper methods here: -------
    
    #' @return If `y` is a function, simply returns `y`. Otherwise creates a function that takes `y`
    #'   and another element as arguments and returns true if and only if both are identical w.r.t. the
    #'   `equals()` function from this package.
    #' @export
    #'
    #' @examples
    #' @md
    .fulfills <- function(y, ...) {
      if(is.function(y)){
        FUN <- y
      }
      else{
        FUN <- function(elem){
          return(l$result_env$equals(elem, y, ...))
        }
      }
      return(FUN)
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
        #' Check elements in dataset for pattern `y`.
        #' 
        #' If `y` is not a function, the method checks if at least one element `e` in `x` is identical to
        #' `y` w.r.t. the `equals()` method from this package.\cr
        #' 
        #' If `y` is a function, the method checks if for at least one element `e` in `x`,
        #' `y(e)` returns `TRUE`\cr
        #' 
        #' In both cases we refer to `e` "fulfilling" `y` if and only if they return `TRUE`.\cr
        #' 
        #' By default, both cases are performed in a
        #' recursive manner. This means that if no element in `x` "fulfills" `y` and `x` is
        #' a recursive object (w.r.t. [base::is.recursive()]), the same procedure is recursively applied
        #' to each element in `x` resulting in a recursive search.
        #'
        #' @param x The dataset on which to apply this method.
        #' @param y The pattern for which to search in `x`.
        #' @param recursive Whether to search recursively for `y` or not.
        #' @param ... If `y` is not a function, `...` is used in the call to `equals()`.
        #'
        #' @return `contains` returns `TRUE` if and only if ONE of the following conditions is fulfilled:
        #'   * There is at least one element `e` in `x` that "fulfills" `y`.
        #'   * `recursive` is `TRUE` __AND__ `x` [is.recursive()] __AND__ for at 
        #'     least one  element `e` in `x`, `contains(e, FUN, recursive = TRUE)` returns `TRUE`
        #' @export
        #' @md
        contains <- function(x, y, recursive = FALSE, ...) {
          if(length(x) == 0){
            return(FALSE)
          }
          
          FUN <- local_env$.fulfills(y, ...)
          
          for(e in x){
            if(FUN(e)){
              return(TRUE)
            }
          }
          
          # No element fulfilled FUN
          if(!recursive || !is.recursive(x)){
            return(FALSE)
          }
          else {
            for(e in x){
              if(contains(e,FUN,recursive=TRUE)){
                return(TRUE)
              }
            }
            
            return(FALSE)
          }
        }
        

        #' Checks whether `x` itself "fulfills" `y` and (see [contains()] for
        #' definition of "fulfills"). If not, behaves equivalently to
        #' [contains()].
        #' 
        #' @return Returns true if and only if __at least ONE__ of the following is `TRUE`:
        #'  * `contains(list(x), y, recursive = FALSE)` returns `TRUE` (the "x itself 'fulfills' y" case)
        #'  * `contains(x, y, recursive = recursive)` returns `TRUE`
        #' @export
        #'
        #' @examples
        #' @md
        contains_or_is <- function(x, y, recursive = TRUE, ...) {
          if(result_env$contains(list(x), y, recursive = FALSE, ...)){
            return(TRUE)
          }
          else{
            return(result_env$contains(x, y, recursive = recursive, ...))
          }
        }
        
        #' A simple wrapper returning `isTRUE(all.equal(target, current, ...)`.
        #'
        #' @param target See `all.equal()`.
        #' @param current See `all.equal()`.
        #' @param ... See `all.equal()`.
        #'
        #' @return `isTRUE(all.equal(target, current, ...)`
        #' @export
        #'
        #' @examples
        #' @md
        equals <- function(target, current, ...){
          return(isTRUE(all.equal(target, current, ...)))
        }
        
        #' Converts `x` to a data frame `df`. 
        #' 
        #' 1. First recursively transforms `x` into 
        #' a "flat" list `x_flat` via `as_flat_list(x)`. 
        #' 2. The method then iterates over the entries of `x_flat` and fills a list `l` of lists (inner lists correspond
        #' to the columns of the resulting data frame) with
        #' the corresponding entries according the the parameters below.
        #' 3. Each inner list is then coerced according to the provided `modes` parameter and inserted as a new column
        #' in the resulting data frame.
        #' 
        #' Let `n = length(x_flat)` be the number of elements the matrix is filled with.
        #'
        #' @param x Transformed to `x_flat` via `as_flat_list(x)`. The latter's entries are used to fill the resulting data frame
        #' @param nrows A non-negative integer indicating the number of rows the resulting data frame shall have. 
        #'   * If `NULL` __and__ `ncols` is also `NULL`, then `nrows` is internally set to `1` if `byrow` is `TRUE` and to `n` otherwise.
        #'   * If `NULL` __and__ `ncols` is __NOT__ `NULL`, then `nrows` is internally set to `n / ncols`. Note that the resulting
        #'     numer __MUST__ be an integer.
        #'   * If __NOT__ `NULL` __and__ `ncols` is __NOT__ `NULL`, then `nrows x ncols` __MUST__ equal `n`.
        #' @param ncols A non-negative integer indicating the number of columns the resulting data frame shall have. 
        #'   * If `NULL` __and__ `nrows` is also `NULL`, then `ncols` is internally set to `1` if `byrow` is `FALSE` and to `n` otherwise.
        #'   * If `NULL` __and__ `nrows` is __NOT__ `NULL`, then `ncols` is internally set to `n / nrows`. Note that the resulting
        #'     numer __MUST__ be an integer.
        #'   * If __NOT__ `NULL` __and__ `nrows` is __NOT__ `NULL`, then `nrows x ncols` __MUST__ equal `n`.
        #' @param rownames The row names of the resulting data frame. Either `NULL` (no row names) or a data structure whose i-th entry 
        #'   contains the character name to be used for the i-th row.
        #' @param colnames The column names of the resulting data frame. Either `NULL` (no column names) or a data structure whose i-th entry 
        #'   contains the character name to be used for the i-th column.
        #' @param modes If `NULL`, then the i-th data frame column is set to `unlist(l[[i]])`.
        #'   Otherwise `mode` must either be a character of length one (use the same mode for all columns) 
        #'   or of length `ncols` where the i-th entry corresponds to the i-th data frame column. 
        #'   The i-th data frame column is then coerced to it corresponding mode `modes[[i]]` by setting `base::mode(l[[i]]) <- modes[[i]]`.
        #'   Note that coercion might introduce additional `NA`s. A warning is printed in that case.
        #' @param byrow If `TRUE`, the resuling data frame is filled by row, otherwise by column.
        #' @param check_args If `TRUE`, additonal consistency and type checks are performed on the parameters provided to
        #'   this method. Only deactivate if you are sure that your parameters are correct. 
        #' @param stringsAsFactors Provided to the corresponding parameter upon creation of the resulting `data.frame`.
        #'   See `data.frame` documentation for more details.
        #' 
        #' @return A data frame. See above for more details.
        #' @export
        #'
        #' @examples
        #' @md
        as_data_frame <- function(x, 
                                  nrows=NULL, 
                                  ncols=NULL, 
                                  rownames=NULL, 
                                  colnames=NULL, 
                                  modes = NULL, 
                                  byrow = FALSE,
                                  check_args = TRUE,
                                  stringsAsFactors = default.stringsAsFactors()){
          old_assert_status <- l$o$err$get_assertions_status()
          l$o$err$set_assertions_status(check_args)
          
          l$o$err$assert_msg("'nrows' must be 'NULL' or a non-negative integer of length one.",
                             is.null(nrows) ||
                               l$o$tcs$has_length_1(nrows, NA_on_fail = FALSE) &&
                               l$o$tcs$is_integer(nrows,
                                                  accept_NULL = FALSE,
                                                  accept_NaN = FALSE,
                                                  accept_NA = FALSE,
                                                  lower_bound = 0,
                                                  lower_bound_inclusive = TRUE,
                                                  upper_bound = Inf,
                                                  upper_bound_inclusive = FALSE))
          
          l$o$err$assert_msg("'ncols' must be 'NULL' or a non-negative integer of length one.",
                             is.null(ncols) ||
                               l$o$tcs$has_length_1(ncols, NA_on_fail = FALSE) &&
                               l$o$tcs$is_integer(ncols,
                                                  accept_NULL = FALSE,
                                                  accept_NaN = FALSE,
                                                  accept_NA = FALSE,
                                                  lower_bound = 0,
                                                  lower_bound_inclusive = TRUE,
                                                  upper_bound = Inf,
                                                  upper_bound_inclusive = FALSE))
                             
          l$o$err$assert_msg("'byrow' must be a logical of length one.",
                             l$o$tcs$has_length_1(byrow, NA_on_fail = FALSE),
                             l$o$tcs$is_logical(byrow,
                                                accept_NULL = FALSE,
                                                accept_NaN = FALSE,
                                                accept_NA = FALSE))
          
          x_flat <- result_env$as_flat_list(x)
          len <- length(x_flat)
          
          l$o$err$assert_msg("The \"flat\" version of 'x' may not contain 'NULL' entries.",
                             all(sapply(x_flat, FUN = function(e){
                               return(!is.null(e))
                             })))
          
          if(len == 0){
            if(!is.null(nrows)){
              if(!is.null(ncols)){
                l$o$err$assert_msg(paste0("Total (recursive) length of 'x' must equal ",
                                          "'nrows * ncols' if the latter two are provided."),
                                   ncols * nrows == 0)
              }
              else {
                ncols <- 0
              }
            }
            else { # nrows is NULL
              nrows <- 0
              
              if(is.null(ncols)){
                ncols <- 0
              }
            }
          }
          else{ # len > 0
            if(!is.null(nrows)){
              if(!is.null(ncols)){
                l$o$err$assert_msg(paste0("Total (recursive) length of 'x' must equal ",
                                          "'nrows * ncols' if the latter two are provided."),
                                   len == ncols * nrows)
              }
              else{
                l$o$err$assert_msg(paste0("Total (recursive) length of 'x' must be a multiple of ",
                                          "'nrows' if the latter is provided."),
                                   nrows > 0,
                                   len %% nrows == 0)
                ncols <- len / nrows
              }
            }
            else{ # nrows is NULL
              if(!is.null(ncols)){
                l$o$err$assert_msg(paste0("Total (recursive) length of 'x' must be a multiple of ",
                                          "'ncols' if the latter is provided."),
                                   ncols > 0,
                                   len %% ncols == 0)
                nrows <- len / ncols
              }
              else if(byrow){
                nrows <- 1
                ncols <- len
              }
              else{
                nrows <- len
                ncols <- 1
              }
            }
          }
          
          l$o$err$assert_msg(paste0("'modes' must be 'NULL' or a character of length one or ",
                                    "a character of length 'ncols'."),
                             is.null(modes) ||
                               (l$o$tcs$has_length_1(modes, NA_on_fail = FALSE) ||
                                  l$o$tcs$has_length(modes, len = ncols, NA_on_fail = FALSE)) &&
                               all(l$o$tcs$is_character(modes,
                                                        accept_NULL = FALSE,
                                                        accept_NaN = FALSE,
                                                        accept_NA = FALSE)))
          
          l$o$err$assert_msg("'rownames' must be 'NULL' or a character vector of length 'nrows'.",
                             is.null(rownames) ||
                               l$o$tcs$has_length(rownames, len = nrows, NA_on_fail = FALSE) &&
                               all(l$o$tcs$is_character(rownames,
                                                        accept_NULL = FALSE,
                                                        accept_NaN = FALSE,
                                                        accept_NA = FALSE)))
          l$o$err$assert_msg("'colnames' must be 'NULL' or a character vector of length 'ncols'.",
                             is.null(colnames) ||
                               l$o$tcs$has_length(colnames, len = ncols, NA_on_fail = FALSE) &&
                               all(l$o$tcs$is_character(colnames,
                                                        accept_NULL = FALSE,
                                                        accept_NaN = FALSE,
                                                        accept_NA = FALSE)))
          if(nrows == 0 || ncols == 0){
            result <- data.frame(matrix(nrow = nrows, ncol = ncols), stringsAsFactors = stringsAsFactors)
          }
          else{
            result <- lapply(1:ncols, FUN = function(e){
              return(vector(mode = "list", length = nrows))
            })
            
            
            if(byrow){
              for(r in 1:nrows){
                for(c in 1:ncols){
                  result[[c]][[r]] <- x_flat[[(r-1)*ncols + c]]
                }
              }
            }
            else {
              for(c in 1:ncols){
                for(r in 1:nrows){
                  result[[c]][[r]] <- x_flat[[(c-1)*nrows + r]]
                }
              }
            }
            
            if(!is.null(modes)){
              if(length(modes) == 1){
                modes <- rep(modes, times = ncols)
              }
              
              for(c in 1:ncols){
                base::mode(result[[c]]) <- modes[[c]]
              }
            }
            else{
              for(c in 1:ncols){
                result[[c]] <- unlist(result[[c]])
              }
            }
            
            result <- data.frame(result, stringsAsFactors = stringsAsFactors)
          }
          
          base::rownames(result) <- rownames
          base::colnames(result) <- colnames
          
          
          l$o$err$set_assertions_status(old_assert_status)
          
          return(result)
        }
        
        #' Converts `x` to a matrix `m`. First recursively transforms `x` into 
        #' a "flat" list `x_flat` via `as_flat_list(x)`. The method then
        #' iterates over the entries of `x_flat` and fills
        #' the corresponding entries in the result matrix according the the parameters below.
        #' Let `n = length(x_flat)` be the number of elements the matrix is filled with.
        #'
        #' @param x Transformed to `x_flat` via `as_flat_list(x)`. The latter's entries are used to fill the resulting matrix.
        #' @param nrows A non-negative integer indicating the number of rows the resulting matrix shall have. 
        #'   * If `NULL` __and__ `ncols` is also `NULL`, then `nrows` is internally set to `1` if `byrow` is `TRUE` and to `n` otherwise.
        #'   * If `NULL` __and__ `ncols` is __NOT__ `NULL`, then `nrows` is internally set to `n / ncols`. Note that the resulting
        #'     numer __MUST__ be an integer.
        #'   * If __NOT__ `NULL` __and__ `ncols` is __NOT__ `NULL`, then `nrows x ncols` __MUST__ equal `n`.
        #' @param ncols A non-negative integer indicating the number of columns the resulting matrix shall have. 
        #'   * If `NULL` __and__ `nrows` is also `NULL`, then `ncols` is internally set to `1` if `byrow` is `FALSE` and to `n` otherwise.
        #'   * If `NULL` __and__ `nrows` is __NOT__ `NULL`, then `ncols` is internally set to `n / nrows`. Note that the resulting
        #'     numer __MUST__ be an integer.
        #'   * If __NOT__ `NULL` __and__ `nrows` is __NOT__ `NULL`, then `nrows x ncols` __MUST__ equal `n`.
        #' @param rownames The row names of the resulting matrix. Either `NULL` (no row names) or a data structure whose i-th entry 
        #'   contains the character name to be used for the i-th row.
        #' @param colnames The column names of the resulting matrix. Either `NULL` (no column names) or a data structure whose i-th entry 
        #'   contains the character name to be used for the i-th column.
        #' @param mode If `NULL`, then no coercion is performed on the resulting matrix `m`.
        #'   Otherwise `mode` must be a character of length one and the following operations are performed after the matrix 
        #'   has been filled with the entries from `x_flat`:
        #'   * All `NULL` entries are replaced with `NA`
        #'   * `base::mode(m)` is set to `mode`, thus coercing all entries in `m` to `mode` (e.g. "character", "numeric", "list", ...).
        #'     Note that coercion might introduce additional `NA`s. A warning is printed in that case.
        #' @param byrow If `TRUE`, the resuling matrix is filled by row, otherwise by column.
        #' @param check_args If `TRUE`, additonal consistency and type checks are performed on the parameters provided to
        #'   this method. Only deactivate if you are sure that your parameters are correct. 
        #'
        #' @return A matrix. See above for more details.
        #' @export
        #'
        #' @examples
        #' @md
        as_matrix <- function(x, 
                              nrows=NULL, 
                              ncols=NULL, 
                              rownames=NULL, 
                              colnames=NULL, 
                              mode = NULL, 
                              byrow = FALSE, 
                              check_args = TRUE){
          old_assert_status <- l$o$err$get_assertions_status()
          l$o$err$set_assertions_status(check_args)
          
          l$o$err$assert_msg("'nrows' must be 'NULL' or a non-negative integer of length one.",
                             is.null(nrows) ||
                               l$o$tcs$has_length_1(nrows, NA_on_fail = FALSE) &&
                               l$o$tcs$is_integer(nrows,
                                                  accept_NULL = FALSE,
                                                  accept_NaN = FALSE,
                                                  accept_NA = FALSE,
                                                  lower_bound = 0,
                                                  lower_bound_inclusive = TRUE,
                                                  upper_bound = Inf,
                                                  upper_bound_inclusive = FALSE))
          l$o$err$assert_msg("'ncols' must be 'NULL' or a non-negative integer of length one.",
                             is.null(ncols) ||
                               l$o$tcs$has_length_1(ncols, NA_on_fail = FALSE) &&
                               l$o$tcs$is_integer(ncols,
                                                  accept_NULL = FALSE,
                                                  accept_NaN = FALSE,
                                                  accept_NA = FALSE,
                                                  lower_bound = 0,
                                                  lower_bound_inclusive = TRUE,
                                                  upper_bound = Inf,
                                                  upper_bound_inclusive = FALSE))
        
          l$o$err$assert_msg("'mode' must be 'NULL' or a character of length one.",
                             is.null(mode) ||
                               l$o$tcs$has_length_1(mode, NA_on_fail = FALSE) &&
                               l$o$tcs$is_character(mode,
                                                    accept_NULL = FALSE,
                                                    accept_NaN = FALSE,
                                                    accept_NA = FALSE))
          l$o$err$assert_msg("'byrow' must be a logical of length one.",
                             l$o$tcs$has_length_1(byrow, NA_on_fail = FALSE),
                             l$o$tcs$is_logical(byrow,
                                                accept_NULL = FALSE,
                                                accept_NaN = FALSE,
                                                accept_NA = FALSE))
          
          x_flat <- result_env$as_flat_list(x)
          len <- length(x_flat)
          if(len == 0){
            if(!is.null(nrows)){
              if(!is.null(ncols)){
                l$o$err$assert_msg(paste0("Total (recursive) length of 'x' must equal ",
                                          "'nrows * ncols' if the latter two are provided."),
                                   ncols * nrows == 0)
              }
              else {
                ncols <- 0
              }
            }
            else { # nrows is NULL
              nrows <- 0
              
              if(is.null(ncols)){
                ncols <- 0
              }
            }
          }
          else{ # len > 0
            if(!is.null(nrows)){
              if(!is.null(ncols)){
                l$o$err$assert_msg(paste0("Total (recursive) length of 'x' must equal ",
                                          "'nrows * ncols' if the latter two are provided."),
                                   len == ncols * nrows)
              }
              else{
                l$o$err$assert_msg(paste0("Total (recursive) length of 'x' must be a multiple of ",
                                          "'nrows' if the latter is provided."),
                                   nrows > 0,
                                   len %% nrows == 0)
                ncols <- len / nrows
              }
            }
            else{ # nrows is NULL
              if(!is.null(ncols)){
                l$o$err$assert_msg(paste0("Total (recursive) length of 'x' must be a multiple of ",
                                          "'ncols' if the latter is provided."),
                                   ncols > 0,
                                   len %% ncols == 0)
                nrows <- len / ncols
              }
              else if(byrow){
                nrows <- 1
                ncols <- len
              }
              else{
                nrows <- len
                ncols <- 1
              }
            }
          }
          l$o$err$assert_msg("'rownames' must be 'NULL' or a character vector of length 'nrows'.",
                             is.null(rownames) ||
                               l$o$tcs$has_length(rownames, len = nrows, NA_on_fail = FALSE) &&
                               all(l$o$tcs$is_character(rownames,
                                                        accept_NULL = FALSE,
                                                        accept_NaN = FALSE,
                                                        accept_NA = FALSE)))
          l$o$err$assert_msg("'colnames' must be 'NULL' or a character vector of length 'ncols'.",
                             is.null(colnames) ||
                               l$o$tcs$has_length(colnames, len = ncols, NA_on_fail = FALSE) &&
                               all(l$o$tcs$is_character(colnames,
                                                        accept_NULL = FALSE,
                                                        accept_NaN = FALSE,
                                                        accept_NA = FALSE)))
          
          result <- matrix(x_flat, nrow = nrows, ncol = ncols, byrow = byrow)
          
          if(!is.null(mode)){
            # Replace NULL entries with NA
            result[sapply(result, FUN = is.null)] <- NA
            
            base::mode(result) <- mode
          }
          
          base::rownames(result) <- rownames
          base::colnames(result) <- colnames
          
          
          l$o$err$set_assertions_status(old_assert_status)
          
          return(result)
        }
        
        #' Turns `x` into a "flat" list. See `return` description for more details.
        #'
        #' @param x A data structure.
        #' @param rm_null If `TRUE`, `NULL` entries are not included in the result list. 
        #' @param rm_nan If `TRUE`, `NaN` entries are not included in the result list. 
        #' @param rm_na If `TRUE`, `NA` entries that are __NOT__ `NaN` are not included in the result list. 
        #'
        #' @return A list containing the elements in `x` of length <= 1, aquired via a depth first search.
        #'   More precisely, the resulting list is constructed as follows:
        #'     * Iterate over all elements in `x`.
        #'     * If the current element has a length <= 1, insert it at the end of the result list
        #'       unless one of the `rm_*` parameters applies.
        #'     * If the current element has a length of > 1, recursively apply this method
        #'       to this element and append the resultung list to the result list.
        #' @export
        #'
        #' @examples
        #' @md
        as_flat_list <- function(x, rm_null = FALSE, rm_nan = FALSE, rm_na = FALSE){  
          result <- list()
          for(elem in x){
            if(length(elem) > 1){
              result <- c(result, result_env$as_flat_list(elem,
                                                          rm_null = rm_null,
                                                          rm_nan = rm_nan,
                                                          rm_na = rm_na))
            }
            else if(!l$o$tcs$is_special((le <- list(elem)),
                                        accept_NULL = TRUE,
                                        accept_NaN = TRUE,
                                        accept_NA = TRUE,
                                        NA_on_NULL = TRUE) ||
                    l$o$tcs$is_special(le,
                                       accept_NULL = !rm_null,
                                       accept_NaN = !rm_nan,
                                       accept_NA = !rm_na,
                                       NA_on_NULL = TRUE)){
              result <- c(result, le)
            }
          }
          return(result)
        }
        
        
        #' @param x Some iterable data structure.
        #' @param recursive A logical of length one.
        #' @param check_args If `TRUE`, additonal consistency and type checks are performed on the parameters provided to
        #'   this method. Only deactivate if you are sure that your parameters are correct. 
        
        #' @return If `names(x)` is not `NULL`, this method sets it to `trimws(names(x))` 
        #'   (thus removing trailing and leading whitespaces). If `recursive` is `TRUE`
        #'   and `x` is recursive (w.r.t. `is.recursive()`), this procedure is
        #'   recursively applied to each element in `x`.
        #'   
        #'   The resulting `x` with trimmed names is then returned.
        #' @export
        #'
        #' @examples
        #' @md
        trim_names <- function(x, recursive = FALSE, check_args = TRUE) {
          l$o$err$assert_msg("'recursive' must be a logical of length one.",
                             l$o$tcs$has_length_1(recursive, NA_on_fail = FALSE),
                             l$o$tcs$is_logical(recursive,
                                                accept_NULL = FALSE,
                                                accept_NaN = FALSE,
                                                accept_NA = FALSE))
          
          old_assert_status <- l$o$err$get_assertions_status()
          l$o$err$set_assertions_status(check_args)
          
          if(!is.null(names(x))){
            names(x) <- trimws(names(x))
          }
          
          if(recursive && is.recursive(x) && (len <- length(x)) > 0){
            
            for(i in 1:len){
              x[[i]] <- trim_names(x[[i]], recursive = TRUE) 
            }
          }
          
          l$o$err$set_assertions_status(old_assert_status)
          return(x)
        }
        
        
        #' @param data Data structure to be split.
        #' @param partition_weights A vector of length `k > 0` containing finite, 
        #'   non-negative entries that must sum up to a value `> 0`. If `length(data)` is zero, `partition_weights` might also have zero length.
        #' @param min_elems_per_partition A numeric of either the same length as `partition_weights` or of length one.
        #'   The latter case is treated as equivalent to providing a numeric vector of the same length as `partition_weights`
        #'   where all entries are set to `min_elems_per_partition`.
        #'   The `i`-th entry indicased the minimal number of elements to be put in the `i`-th partition.
        #'   Note that `sum(min_elems_per_partition)` must be `<= length(data)`.
        #' @param max_elems_per_partition Similar to `min_elems_per_partition` either a numeric vector of length one or the sam
        #'   length as `partition_weights`. Indicates the maximal number of elements in each partition.
        #'   Note that either `allow_subset_result` is `TRUE` or `sum(max_elems_per_partition)` must be `>= length(data)`.
        #'   If both are `FALSE`, then the result is a partition of a true subset of `data` as not all elements "fit"
        #'   into the provided `max_elems_per_partition` bound.
        #' @param check_args If `TRUE`, additonal consistency and type checks are performed on the parameters provided to
        #'   this method. Only deactivate if you are sure that your parameters are correct. 
        #' @param allow_subset_result See `max_elems_per_partition`.
        #' @return A list of length `k`. Its `i`-th entry contains the `i`-th data partition (a list of elements).
        #'   The list is created as follows:
        #'   1. For each partition `k` with `min_elems_per_partition[[k]] > 0`, randomly sample `min_elems_per_partition[[k]]`
        #'      elements from `data` and add them to the `k`-th partition. Remove the sampled elements from `data` so no
        #'      element gets sampled twice.
        #'   2. Let `non_max_i` be the set of partition indices whose length has not reached `max_elems_per_partition[[c]]` yet.
        #'      For each of those partition indices, sample partition `c` with a probability proportional
        #'      to `partition_weights[[c]]`. Then sample a random element from `data` and add it to this partition. Remove the element
        #'      from `data`.
        #'   3. Repeat 2. until either no element is left in `data` or all partitions have reached their maximal length
        #'      according to `max_elems_per_partition`.
        #'   4. Return the result.
        #' @export
        #'
        #' @examples
        #' @md
        partition_data <- function(data, 
                                   partition_weights,
                                   min_elems_per_partition = 1,
                                   max_elems_per_partition = Inf,
                                   check_args = TRUE,
                                   allow_subset_result = FALSE){
          data <- as.list(data)
          
          old_assert_status <- l$o$err$get_assertions_status()
          l$o$err$set_assertions_status(check_args)
          
          num_parts <- length(partition_weights)
          num_elems <- length(data)
          
          if(l$o$tcs$has_length_1(min_elems_per_partition, NA_on_fail = FALSE) && num_parts > 0){
            min_elems_per_partition <- sapply(1:num_parts,
                                              FUN = function(i) return(unlist(min_elems_per_partition)))
          }
          if(l$o$tcs$has_length_1(max_elems_per_partition, NA_on_fail = FALSE) && num_parts > 0){
            max_elems_per_partition <- sapply(1:num_parts,
                                              FUN = function(i) return(unlist(max_elems_per_partition)))
          }
          
          l$o$err$assert_msg("Illegal arguments.",
                             l$o$tcs$has_length_1(allow_subset_result, NA_on_fail = FALSE),
                             l$o$tcs$is_logical(allow_subset_result,
                                                accept_NULL = FALSE,
                                                accept_NaN = FALSE,
                                                accept_NA = FALSE),
                             num_elems == 0 && num_parts <= 1 || num_elems > 0 && num_parts > 0,
                             length(min_elems_per_partition) == num_parts,
                             length(max_elems_per_partition) == num_parts,
                             num_parts == 0 || all(sapply(1:num_parts,
                                        FUN = function(i){
                                          min_element <- min_elems_per_partition[[i]]
                                          max_element <- max_elems_per_partition[[i]]
                                          return(l$o$tcs$has_length_1(min_element, NA_on_fail = FALSE) &&
                                                   l$o$tcs$has_length_1(max_element, NA_on_fail = FALSE) &&
                                                   l$o$tcs$is_numeric(min_element,
                                                                      accept_NULL = FALSE,
                                                                      accept_NaN = FALSE,
                                                                      accept_NA = FALSE,
                                                                      lower_bound = 0,
                                                                      lower_bound_inclusive = TRUE,
                                                                      upper_bound = max_element,
                                                                      upper_bound_inclusive = TRUE,
                                                                      accept_non_integer = TRUE) &&
                                                   l$o$tcs$is_numeric(max_element,
                                                                      accept_NULL = FALSE,
                                                                      accept_NaN = FALSE,
                                                                      accept_NA = FALSE,
                                                                      lower_bound = min_element,
                                                                      lower_bound_inclusive = TRUE,
                                                                      upper_bound = Inf,
                                                                      upper_bound_inclusive = TRUE,
                                                                      accept_non_integer = TRUE))
                                        })),
                             num_elems >= sum(min_elems_per_partition),
                             allow_subset_result || sum(max_elems_per_partition) >= num_elems,
                             all(sapply(partition_weights,
                                        FUN = function(el){
                                          return(l$o$tcs$has_length_1(el, NA_on_fail = FALSE) &&
                                                   l$o$tcs$is_numeric(el,
                                                                      accept_NULL = FALSE,
                                                                      accept_NaN = FALSE,
                                                                      accept_NA = FALSE,
                                                                      lower_bound = 0,
                                                                      lower_bound_inclusive = TRUE,
                                                                      upper_bound = Inf,
                                                                      upper_bound_inclusive = FALSE,
                                                                      accept_non_integer = TRUE))
                                        })),
                             num_parts == 0 || sum(partition_weights) > 0)
          
          result <- list()
          if(num_elems > 0){
            for(i in 1:num_parts){
              min_num <- min_elems_per_partition[[i]]
              result[[i]] <- list()
              if(min_num > 0){
                num_elems <- length(data)
                l$o$err$assert_msg("Inconsistent state.",
                                   num_elems >= min_num)
                indices <- sample(1:num_elems, size = min_num, replace = FALSE)
                result[[i]] <- append(result[[i]], data[indices])
                data[indices] <- NULL
              }
            }
           
            unmaxed_ind <- sapply(1:num_parts,
                                  FUN = function(i){
                                    l$o$err$assert_msg("Inconsistent state.",
                                                       length(result[[i]]) <= max_elems_per_partition[[i]],
                                                       length(result[[i]]) >= min_elems_per_partition[[i]])
                                    return(length(result[[i]]) < max_elems_per_partition[[i]])
                                  })
            
            while((num_elems <- length(data)) > 0 &&
                  any(unmaxed_ind)){
              candidates <- which(unmaxed_ind)
              if(length(candidates) > 1){
                c <- sample(candidates, size = 1, prob = partition_weights[candidates])
              }
              else{
                c <- candidates[[1]]
              }
              el_id <- sample(1:num_elems, size = 1)
              result[[c]] <- append(result[[c]], data[[el_id]])
              data[[el_id]] <- NULL
              
              l$o$err$assert_msg("Inconsistent state.",
                                 length(result[[c]]) <= max_elems_per_partition[[c]],
                                 length(result[[c]]) >= min_elems_per_partition[[c]])
              
              unmaxed_ind[[c]] <- length(result[[c]]) < max_elems_per_partition[[c]]
            }
          }
          l$o$err$set_assertions_status(old_assert_status)
          return(result)
        }
      },
      envir = result_env)
    lockEnvironment(local_env, bindings = TRUE)
    invisible(result_env)
  }
} # End of include guard