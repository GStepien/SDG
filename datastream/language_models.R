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
#' @title  Collection of subclasses of `R6_Base` from `utils_lang_r6_baseclass.R` for creating, representing and 
#'   storing language models with multivariate histories.
#' @description Contains a single [get_<name>_env()] method
#'   that returns an environment containing the `R6` constructor objects of these subclasses.
#' @md

# Include guard
if (!exists("LANGUAGE_MODELS_R", inherits = FALSE)) {
  LANGUAGE_MODELS_R = TRUE
  
  #' @return An environment containing one or multiple constructor objects for `R6` subclasses of 
  #'         `R6_Base` from `utils_lang_r6_baseclass.R`. These provide functionality for creating, representing and 
  #'   storing language models with multivariate histories.
  #' @export
  #'
  #' @examples
  #' @md
  get_language_models_env <- function() {
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
    other_scripts <- c(file.path(".", "utils", "utils_math_general.R"))   # e.g.: c(file.path(".", "utils", "utils_math_general.R"))
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
    assign("utils_math_general", get_utils_math_general_env(), envir = static_env)
    
    #' __Optional__: Add short-hand variants of the above:
    #' E.g.: assign("gen", static_env$utils_lang_general, envir = static_env)
    assign("mg", static_env$utils_math_general, envir = static_env)
    
    # --------- Load required package dependencies -------    
    #' Append other packages to `package_dependencies` which are to be installed (if necessary) and loaded
    package_dependencies <- c("R6", "stringr", "rjson", "igraph", "Matrix", "RSpectra") # e.g., c("R6","smoother","rowr","randomcoloR")
    
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
    get_unique_character_from_integer_listlist <- function(integer_listlist){
      num_sublists <- length(integer_listlist)
      l$s$err$assert_msg("'integer_listlist' must be a list containing lists of integers.",
                         all(sapply(unlist(integer_listlist, recursive = FALSE),
                                    FUN = function(sublist){
                                      return(all(sapply(unlist(sublist),
                                                        FUN = function(element){
                                                          return(l$s$tcs$has_length_1(element, NA_on_fail = FALSE) &&
                                                                   l$s$tcs$is_integer(element,
                                                                                      accept_NULL = FALSE,
                                                                                      accept_NaN = FALSE,
                                                                                      accept_NA = FALSE,
                                                                                      lower_bound = -Inf,
                                                                                      lower_bound_inclusive = FALSE,
                                                                                      upper_bound = Inf,
                                                                                      upper_bound_inclusive = FALSE))
                                                        })))
                                    })))
      result <- list("STATE")
      #result <- list("|") #old: a
      result <- append(result, unlist(lapply(integer_listlist,
                                      FUN = function(sublist){
                                        result <- lapply(sublist,
                                                         FUN = function(element){
                                                           return(paste0("id", element)) # old c
                                                         })
                                        
                                        return(append("|", result)) # old append("b", result)
                                      })))
      return(paste0(result, collapse=''))
    }
    
    character_pattern <- "^STATE(\\|(id\\d+)*)*$"
    
    get_unique_integer_listlist_from_character <- function(character){
      l$s$err$assert_msg(paste0("'character' must be a character vector of length one."),
                                l$s$tcs$has_length_1(character, NA_on_fail = FALSE),
                                l$s$tcs$is_character(character,
                                                     accept_NULL = FALSE,
                                                     accept_NaN = FALSE,
                                                     accept_NA = FALSE))
      
      l$s$err$assert_msg(paste0("Character does not match pattern: \"", l$character_pattern, "\""),
                         grepl(l$character_pattern, character))
      
      substrings <- str_extract_all(string = character,
                                  pattern = "\\|(id\\d+)*")
      result <- lapply(unlist(substrings), 
                       FUN = function(substring){
                         result <- lapply(unlist(str_extract_all(string = substring, pattern = "\\d+")),
                                          FUN = function(integer_char){
                                            result <- strtoi(integer_char)
                                            l$s$err$assert_msg(paste0("'strtoi()' should have returned a finite integer for: ", integer_char),
                                                               l$s$tcs$has_length_1(result, NA_on_fail = FALSE),
                                                               l$s$tcs$is_integer(result,
                                                                                  accept_NULL = FALSE,
                                                                                  accept_NaN = FALSE,
                                                                                  accept_NA = FALSE,
                                                                                  lower_bound = -Inf,
                                                                                  lower_bound_inclusive = FALSE,
                                                                                  upper_bound = Inf,
                                                                                  upper_bound_inclusive = FALSE))
                                            return(result)
                                          })
                       })
      return(result)
    }
    
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
        
        #' TODO: Add option to add/remove words from voc
        Multivariate_Character_Vocabulary <- R6Class("Multivariate_Character_Vocabulary",
                         # ---- R6 inheritance -----
                         #' __Optional__: Change to inherit from a different parent class.
                         #'                     __IMPORTANT__: The parent class MUST either directly or indirectly
                         #'                                    inherit from `R6_Base`!
                         inherit = R6_Base,
                         
                         #' ---- Adapt R6 options --------
                         portable = TRUE,
                         cloneable = TRUE,
                         lock_class = TRUE,
                         lock_objects = TRUE,
                         
                         #' ----- Add private Fields & methods ----
                         private = list(
                           word_to_ids_listlist = NULL,
                           id_to_word_list = NULL,
                           id_to_dims_listlist = NULL,
                           dim_to_ids_listlist = NULL,
                           
                           idle_word = NULL,
                           idle_id = NULL,
                           
                           num_dims = NULL,
                           
                           dim_to_named_id_listlist = NULL,
                           
                           check_dim = function(dim){
                             l$s$err$assert_msg("Index out of bounds.",
                                                l$s$tcs$has_length_1(dim, NA_on_fail = FALSE),
                                                l$s$tcs$is_integer(dim,
                                                                   accept_NULL = FALSE,
                                                                   accept_NaN = FALSE,
                                                                   accept_NA = FALSE,
                                                                   lower_bound = 1,
                                                                   lower_bound_inclusive = TRUE,
                                                                   upper_bound = private$num_dims,
                                                                   upper_bound_inclusive = TRUE))
                           },
                           
                           
                           check_word_id = function(word_id){
                             l$s$err$assert_msg("Index out of bounds.",
                                                l$s$tcs$has_length_1(word_id, NA_on_fail = FALSE),
                                                l$s$tcs$is_integer(word_id,
                                                                   accept_NULL = FALSE,
                                                                   accept_NaN = FALSE,
                                                                   accept_NA = FALSE,
                                                                   lower_bound = 1,
                                                                   lower_bound_inclusive = TRUE,
                                                                   upper_bound = length(private$id_to_word_list),
                                                                   upper_bound_inclusive = TRUE))
                           },
                           
                           check_word = function(word){
                             l$s$err$assert_msg("Unknown word.",
                                                word %in% private$id_to_word_list)
                           },
                           #' TODO: Sort out inconsistencies of list vs non list and identical, integer vs non-integer, etc.
                           assert_consistency = function(){
                             # At this point guaranteed that num_dims > 0 and each dimension has at least one word
                             if(l$s$err$get_assertions_status()){
                               l$s$err$assert_msg("Inconsistent lengths.",
                                                  length(unlist(private$word_to_ids_listlist)) == length(private$id_to_word_list),
                                                  identical(sort(unlist(names(private$word_to_ids_listlist))), sort(unique(unlist(private$id_to_word_list)))),
                                                  
                                                  # Each word has unique id
                                                  length(unique(unlist(private$word_to_ids_listlist))) == length(private$id_to_word_list),
                                                  # Consistent assignments:
                                                  all(sapply(1:length(private$id_to_word_list),
                                                             FUN = function(word_id){
                                                               word <- private$id_to_word_list[[word_id]]
                                                               private$check_word_id(word_id)
                                                               return(word_id %in% private$word_to_ids_listlist[[word]])
                                                             })),
                                                  all(sapply(names(private$word_to_ids_listlist),
                                                             FUN = function(word){
                                                               private$check_word(word)
                                                               return(all(sapply(private$word_to_ids_listlist[[word]],
                                                                                   FUN = function(word_id){
                                                                                     return(identical(private$id_to_word_list[[word_id]], word))
                                                                                   })))
                                                             })),
                                                  
                                                  length(private$id_to_dims_listlist) == length(private$id_to_word_list),
                                                  length(private$dim_to_ids_listlist) == private$num_dims,
                                                  private$num_dims > 0,
                                                  all(sapply(1:private$num_dims,
                                                             FUN = function(dim){
                                                               return(length(private$dim_to_ids_listlist[[dim]]) >= 2) # idle word plus at least one word from word_listlist
                                                             })),
                                                  all(sapply(1:private$num_dims,
                                                             FUN = function(dim){
                                                               return(all(sapply(private$dim_to_ids_listlist[[dim]],
                                                                                 FUN = function(word_id){
                                                                                   return(dim %in% private$id_to_dims_listlist[[word_id]])
                                                                                 })))
                                                             })),
                                                  all(sapply(1:length(private$id_to_dims_listlist),
                                                             FUN = function(word_id){
                                                               return(all(sapply(private$id_to_dims_listlist[[word_id]],
                                                                                 FUN = function(dim){
                                                                                   private$check_dim(dim)
                                                                                   return(word_id %in% private$dim_to_ids_listlist[[dim]])
                                                                                 })))
                                                             })),
                                                  identical(private$id_to_dims_listlist[[1]], 1:private$num_dims))
                             }
                           },
                           
                           read_vocabulary = function(json_file, json_string){
                             if(!is.null(json_file)){
                             json_obj <- fromJSON(file = json_file)
                             }
                             else{
                               json_obj <- fromJSON(json_str = json_string)
                             }
                             
                             result <- list()
                             result[[1]] <- json_obj[["idle_word"]]
                             result[[2]] <- list()
                             next_id <- 2
                             
                             l$s$err$assert_msg("Inconsistent input file format.",
                                               length(names(json_obj[["words"]])) == length(json_obj[["words"]]),
                                               length(json_obj[["words"]]) > 0,
                                               identical(as.numeric(names(json_obj[["words"]])), as.numeric(1:length(json_obj[["words"]]))))
                             
                             for(dim in 1:length(json_obj[["words"]])){
                               l$s$err$assert_msg("Inconsistent input file format.",
                                                  length(json_obj[["words"]][[dim]]) > 0,
                                                  identical(as.numeric(names(json_obj[["words"]][[dim]])), as.numeric(next_id:(next_id + length(json_obj[["words"]][[dim]]) - 1))))
                               
                               result[[2]][[dim]] <- list()
                               for(i in 1:length(json_obj[["words"]][[dim]])){
                                 result[[2]][[dim]][[i]] <- json_obj[["words"]][[dim]][[i]]
                                 next_id <- next_id + 1
                               }
                             }
                             
                             return(result)
                           }
                         ),
                         #-----end of private-----
                         
                         public = list(
                           
                           #' See `R6_Base` in `utils_lang_r6_baseclass.R`
                           #' @export
                           #' @md
                           get_static_env = function(){
                             local_env$static_env
                           },

                           #' @export
                           #'
                           #' @examples
                           #' @md
                           initialize = function(assertions_status = FALSE,
                                                 json_file = NULL,
                                                 json_string = NULL,
                                                 word_listlist,
                                                 idle_word,
                                                 # If FALSE, an error will be produced if the word lists are not pairwise disjoint
                                                 allow_word_overlaps = FALSE
                                                 ){
                             super$initialize()
                             local_env$.add_to_static_env(super$get_static_env())
                             local_env$static_env$err$set_assertions_status(assertions_status)
                             
                             #' Add your initialization code here here:
                             l$s$err$assert_msg("Illegal arguments.",
                                                xor(!is.null(word_listlist) && !is.null(idle_word),  is.null(word_listlist) && is.null(idle_word)),
                                                length(which(c(!is.null(json_file), !is.null(json_string), !is.null(word_listlist) && !is.null(idle_word)))) == 1)
                             
                             if(!is.null(json_file) || !is.null(json_string)){
                               result <- private$read_vocabulary(json_file = json_file, json_string = json_string)
                               idle_word <- result[[1]]
                               word_listlist <- result[[2]]
                             }
                             
                             l$s$err$assert_msg("'allow_word_overlaps' must be a logical of length one.",
                                                l$s$tcs$has_length_1(allow_word_overlaps, NA_on_fail = FALSE),
                                                l$s$tcs$is_logical(allow_word_overlaps,
                                                                   accept_NULL = FALSE,
                                                                   accept_NaN = FALSE,
                                                                   accept_NA = FALSE))
                             
                             l$s$err$assert_msg("'word_listlist' must contain at least one sublist. All sublists must contain at least one character vector of length one.",
                                                length(word_listlist) > 0,
                                                all(sapply(word_listlist,
                                                           FUN = function(sublist){
                                                             result <- length(sublist) > 0 &&
                                                               all(unlist(lapply(sublist,
                                                                                 FUN = function(element){
                                                                                   return(l$s$tcs$has_length_1(element, NA_on_fail = FALSE) &&
                                                                                            l$s$tcs$is_character(element,
                                                                                                                 accept_NULL = FALSE,
                                                                                                                 accept_NaN = FALSE,
                                                                                                                 accept_NA = FALSE))
                                                                                 }))) &&
                                                               length(unique(sublist)) == length(sublist)
                                                           })))
                             l$s$err$assert_msg("'idle_word' must be a character vector of length one not present in 'word_listlist'.",
                                                l$s$tcs$has_length_1(idle_word, NA_on_fail = FALSE),
                                                l$s$tcs$is_character(idle_word,
                                                                     accept_NULL = FALSE,
                                                                     accept_NaN = FALSE,
                                                                     accept_NA = FALSE),
                                                !(idle_word %in% unlist(word_listlist, recursive = TRUE)))
                             
                             unique_words <- unique(unlist(word_listlist))
                             total_words <- unlist(word_listlist)
                             
                             stopifnot(allow_word_overlaps || length(unique_words) == length(total_words))
                             
                             private$num_dims <- length(word_listlist)
                             
                             private$word_to_ids_listlist <- lapply(1:(length(unique_words) + 1), FUN = function(i) {return(list())})
                             names(private$word_to_ids_listlist) <- c(idle_word, unique_words)
                             private$id_to_word_list <- vector(mode = "list", length = length(total_words)+1)
                             private$id_to_dims_listlist <- lapply(1:(length(total_words) + 1), FUN = function(i) {return(list())})
                             
                             private$idle_word <- idle_word
                             private$idle_id <- 1
                             
                             private$dim_to_named_id_listlist <- lapply(1:private$num_dims,
                                                                        FUN = function(i){
                                                                          result <- list()
                                                                          result[[private$idle_word]] <- private$idle_id
                                                                          return(result)
                                                                        })
                             
                             private$dim_to_ids_listlist <- lapply(1:private$num_dims,
                                                                   FUN = function(i){
                                                                     return(list(private$idle_id))
                                                                   })
                             private$word_to_ids_listlist[[private$idle_word]] <- list(private$idle_id)
                             private$id_to_word_list[[private$idle_id]] <- idle_word
                             private$id_to_dims_listlist[[private$idle_id]] <- as.list(1:private$num_dims)
                             
                             next_id <- private$idle_id + 1
                             for(dim in 1:private$num_dims){
                               for(j in 1:length(word_listlist[[dim]])){
                                 word <- word_listlist[[dim]][[j]]
                                 private$word_to_ids_listlist[[word]] <- append(private$word_to_ids_listlist[[word]], list(next_id))
                                 private$id_to_word_list[[next_id]] <- word
                                 private$id_to_dims_listlist[[next_id]] <- append(private$id_to_dims_listlist[[next_id]], list(dim))
                                 private$dim_to_ids_listlist[[dim]] <- append(private$dim_to_ids_listlist[[dim]], list(next_id))
                                 
                                 stopifnot(!(word %in% names(private$dim_to_named_id_listlist[[dim]])))
                                 private$dim_to_named_id_listlist[[dim]][[word]] <- next_id 
                                 
                                 next_id <- next_id + 1
                               }
                             }
                             
                             l$s$err$assert_msg("Unexpected zero length.",
                                                private$num_dims > 0,
                                                private$num_dims == length(private$dim_to_ids_listlist),
                                                length(private$id_to_dims_listlist) > 0,
                                                length(private$id_to_word_list) == length(private$id_to_dims_listlist))
                             for(dim in 1:private$num_dims){
                               private$dim_to_ids_listlist[[dim]] <- as.integer(private$dim_to_ids_listlist[[dim]])
                             }
                             for(i in 1:length(private$id_to_dims_listlist)){
                                 private$id_to_dims_listlist[[i]] <- as.integer(private$id_to_dims_listlist[[i]])
                             }
                             private$assert_consistency()
                           },
                           
                           get_word_id = function(dim , word){
                             l$s$err$assert_msg("'dim' must be an integer between 1 and 'get_num_dims'.",
                                                l$s$tcs$is_integer(dim,
                                                                   accept_NULL = FALSE,
                                                                   accept_NaN = FALSE,
                                                                   accept_NA = FALSE,
                                                                   lower_bound = 1,
                                                                   lower_bound_inclusive = TRUE,
                                                                   upper_bound = self$get_num_dims(),
                                                                   upper_bound_inclusive = TRUE))
                             l$s$err$assert_msg("'word' must be a character vector of length one from dimension 'dim'.",
                                                l$s$tcs$is_character(word,
                                                                     accept_NULL = FALSE,
                                                                     accept_NaN = FALSE,
                                                                     accept_NA = FALSE),
                                                word %in% names(private$dim_to_named_id_listlist[[dim]]))
                             return(private$dim_to_named_id_listlist[[dim]][[word]])
                           },
                           
                           as_json_string = function(){
                             json_obj <- list()
                             json_obj[["idle_word"]] <- private$idle_word
                             json_obj[["words"]] <- list()
                             for(dim in 1:private$num_dims){
                               dim_str <- as.character(dim)
                               json_obj[["words"]][[dim_str]] <- list()
                               for(word_id in private$dim_to_ids_listlist[[dim]]){
                                 if(word_id == private$idle_id){
                                   next
                                 }
                                 word_id_str <- as.character(word_id)
                                 json_obj[["words"]][[dim_str]][[word_id_str]] <- private$id_to_word_list[[word_id]]
                               }
                             }
                             return(toJSON(x = json_obj, indent = 2))
                           },
                           
                           write_vocabulary = function(json_file, overwrite = FALSE){
                             l$s$err$assert_msg("Illegal argument.",
                                                l$s$tcs$has_length_1(overwrite, NA_on_fail = FALSE),
                                                l$s$tcs$is_logical(overwrite,
                                                                   accept_NULL = FALSE,
                                                                   accept_NaN = FALSE,
                                                                   accept_NA = FALSE))
                             
                             stopifnot(overwrite || !file.exists(json_file))
                             
                             dir <- dirname(json_file)
                             if(!exists(dir)){
                               dir.create(path = dir, showWarnings = FALSE, recursive = TRUE)
                             }
                             cat(self$as_json_string(), file = json_file)
                             
                             l$s$err$assert_msg("Error during writing/reading of vocabulary.",
                                                self$identical(l$result_env$Multivariate_Character_Vocabulary$new(
                                                  assertions_status = TRUE,
                                                  json_file = json_file,
                                                  json_string = NULL,
                                                  word_listlist = NULL,
                                                  idle_word = NULL
                                                )))
                           },
                           
                           identical = function(other_voc){
                             result <- setequal(class(self), class(other_voc))
                             if(result){
                               word_ids <- as.numeric(self$get_all_ids())
                               other_word_ids <- as.numeric(other_voc$get_all_ids())
                               
                               result <- result &&
                                 self$get_num_dims() == other_voc$get_num_dims() &&
                                 identical(word_ids, other_word_ids) &&
                                 self$get_idle_word() == other_voc$get_idle_word() &&
                                 self$get_idle_id() == other_voc$get_idle_id() &&
                                 all(sapply(word_ids,
                                            FUN = function(word_id){
                                              return(self$get_word(word_id) == other_voc$get_word(word_id) &&
                                                       identical(unlist(self$get_id_dims(word_id)), 
                                                                 unlist(other_voc$get_id_dims(word_id))))
                                            }))
                             }
                             
                             return(isTRUE(result))
                           },
                           
                           get_num_word_ids = function(){
                             return(length(private$id_to_word_list))
                           },
                           
                           get_num_dims = function(){
                             return(private$num_dims)
                           },
                           
                           get_num_ids_in_dim = function(dim){
                             private$check_dim(dim)
                             return(length(private$dim_to_ids_listlist[[dim]]))
                           },
                           
                           get_all_ids = function(word){
                             return(1:length(private$id_to_word_list))
                           },
                           
                           get_word_ids = function(word){
                             private$check_word(word)
                             return(private$word_to_ids_listlist[[word]])
                           },
                           
                           get_word = function(word_id){
                             private$check_word_id(word_id)
                             return(private$id_to_word_list[[word_id]])
                           },
                           
                           get_id_dims = function(word_id){
                             private$check_word_id(word_id)
                             return(private$id_to_dims_listlist[[word_id]])
                           },
                           
                           get_dim_ids = function(dim){
                             private$check_dim(dim)
                             return(private$dim_to_ids_listlist[[dim]])
                           },
                           
                           get_idle_word = function(){
                             return(private$idle_word)
                           },
                           
                           get_idle_id = function(){
                             return(private$idle_id)
                           },
                           
                           finalize = function() {
                             super$finalize()
                             
                             #' Add your finalization code here:
                           }
                           
                         ),
                         #-----end of public-----
                         
                         #' ----- Add active bindings ----
                         active = list(
                           
                         )
                         #-----end of active-----
        )
        
        .Multivariate_Count_LM_State <- R6Class(".Multivariate_Count_LM_State",
                                                     # ---- R6 inheritance -----
                                                     #' __Optional__: Change to inherit from a different parent class.
                                                     #'                     __IMPORTANT__: The parent class MUST either directly or indirectly
                                                     #'                                    inherit from `R6_Base`!
                                                     inherit = R6_Base,
                                                     
                                                     #' ---- Adapt R6 options --------
                                                     portable = TRUE,
                                                     cloneable = TRUE,
                                                     lock_class = TRUE,
                                                     lock_objects = TRUE,
                                                     
                                                     #' ----- Add private Fields & methods ----
                                                     private = list(
                                                       child_states_list = NULL,
                                                       child_transition_word_ids = NULL,
                                                       child_transition_nums_list = NULL,
                                                       total_transition_num = NULL,
                                                       
                                                       state_word_id_listlist = NULL,
                                                       state_character_id = NULL,
                                                       
                                                       idle_id = NULL,
                                                       idle_state_character = NULL,
                                                       
                                                       argmax_word_ids_list = NULL,
                                                       
                                                       num_dims = NULL,
                                                       max_history_length = NULL,
                                                       
                                                       vocabulary = NULL,
                                                       simple_smooth_factor = NULL,
                                                       
                                                       assertions_status = NULL,
                                                       
                                                       assert_consistency = function(){
                                                         # At this point guaranteed that num_dims > 0 and each dimension has at least one word
                                                         if(l$s$err$get_assertions_status()){
                                                           l$s$err$assert_msg("Inconsistent state.",
                                                                              length(private$child_states_list) <= private$num_dims * (private$vocabulary$get_num_word_ids() - 1) + 1, # plus one for transition to idle
                                                                              length(private$child_transition_nums_list) == length(private$child_states_list),
                                                                              length(names(private$child_states_list)) == length(private$child_states_list),
                                                                              identical(names(private$child_transition_nums_list), names(private$child_states_list)),
                                                                              sum(unlist(private$child_transition_nums_list)) == private$total_transition_num,
                                                                              l$s$mg$`%eq%`(length(private$child_states_list) == 0, private$total_transition_num == 0),
                                                                              length(names(private$child_transition_word_ids)) == length(private$child_transition_word_ids),
                                                                              setequal(names(private$child_transition_word_ids), names(private$child_states_list)),
                                                                              all(sapply(names(private$child_transition_word_ids),
                                                                                         FUN = function(child_name){
                                                                                           return(child_name == self$get_transition_target_character(
                                                                                             private$child_transition_word_ids[[child_name]]))
                                                                                         })))
                                                         }
                                                       }
                                                     ),
                                                     #-----end of private-----
                                                     
                                                     public = list(
                                                       
                                                       #' See `R6_Base` in `utils_lang_r6_baseclass.R`
                                                       #' @export
                                                       #' @md
                                                       get_static_env = function(){
                                                         local_env$static_env
                                                       },
                                                       
                                                       #' @export
                                                       #'
                                                       #' @examples
                                                       #' @md
                                                       initialize = function(assertions_status = FALSE,
                                                                             state_character_id,
                                                                             vocabulary,
                                                                             max_history_length,
                                                                             simple_smooth_factor = 0.1){
                                                         super$initialize()
                                                         local_env$.add_to_static_env(super$get_static_env())
                                                         local_env$static_env$err$set_assertions_status(assertions_status)
                                                         
                                                         #' Add your initialization code here here:
                                                         l$s$err$assert_msg("Illegal arguments.",
                                                                            l$s$tcs$has_length_1(state_character_id, NA_on_fail = FALSE),
                                                                            l$s$tcs$is_character(state_character_id,
                                                                                                 accept_NULL = FALSE,
                                                                                                 accept_NaN = FALSE,
                                                                                                 accept_NA = FALSE),
                                                                            l$s$tcs$has_length_1(max_history_length, NA_on_fail = FALSE),
                                                                            l$s$tcs$is_integer(max_history_length,
                                                                                               accept_NULL = FALSE,
                                                                                               accept_NaN = FALSE,
                                                                                               accept_NA = FALSE,
                                                                                               lower_bound = 0,
                                                                                               lower_bound_inclusive = TRUE,
                                                                                               upper_bound = Inf,
                                                                                               upper_bound_inclusive = FALSE),
                                                                            l$s$tcs$has_length_1(simple_smooth_factor, NA_on_fail = FALSE),
                                                                            l$s$tcs$is_numeric(simple_smooth_factor,
                                                                                               accept_NULL = FALSE,
                                                                                               accept_NaN = FALSE,
                                                                                               accept_NA = FALSE,
                                                                                               lower_bound = 0,
                                                                                               lower_bound_inclusive = TRUE,
                                                                                               upper_bound = 1,
                                                                                               upper_bound_inclusive = TRUE,
                                                                                               accept_non_integer = TRUE),
                                                                            is.R6(vocabulary),
                                                                            inherits(vocabulary, "Multivariate_Character_Vocabulary"))
                                                         
                                                         private$child_states_list <- list()
                                                         private$child_transition_nums_list <- list()
                                                         private$child_transition_word_ids <- list()
                                                         
                                                         private$state_word_id_listlist <- l$get_unique_integer_listlist_from_character(state_character_id)
                                                         private$state_character_id <- state_character_id
                                                         private$idle_id <- vocabulary$get_idle_id()
                                                         private$num_dims <- vocabulary$get_num_dims()
                                                         private$max_history_length <- max_history_length
                                                         private$vocabulary <- vocabulary
                                                         
                                                         private$assertions_status <- assertions_status
                                                         
                                                         # Without transitions, each word is equally probable.
                                                         private$argmax_word_ids_list <- vocabulary$get_all_ids()
                                                         
                                                         private$total_transition_num = 0
                                                         private$simple_smooth_factor <- simple_smooth_factor
                                                         
                                                         private$idle_state_character <- lapply(1:private$num_dims, FUN = function(i) return(list()))
                                                         private$idle_state_character <- l$get_unique_character_from_integer_listlist(private$idle_state_character)
                                                         
                                                         l$s$err$assert_msg("Inconsistent state.",
                                                                            identical(private$state_character_id, l$get_unique_character_from_integer_listlist(private$state_word_id_listlist)),
                                                                            identical(private$state_word_id_listlist, l$get_unique_integer_listlist_from_character(private$state_character_id)),
                                                                            length(private$state_word_id_listlist) == private$num_dims,
                                                                            max(sapply(private$state_word_id_listlist, 
                                                                                       FUN = function(sublist){
                                                                                         return(length(sublist))
                                                                                       })) <= private$max_history_length,
                                                                            length(private$state_word_id_listlist) == private$num_dims,
                                                                            all(sapply(1:private$num_dims,
                                                                                       FUN = function(dim){
                                                                                         return(all(sapply(private$state_word_id_listlist[[dim]],
                                                                                                    FUN = function(word_id){
                                                                                                      return(word_id != private$idle_id &&
                                                                                                               word_id %in% vocabulary$get_dim_ids(dim))
                                                                                                    })))
                                                                                       })))
                                                         private$assert_consistency()
                                                       },
                                                       
                                                       #' Does NOT recursively check if childen are identical -> use lm for this
                                                       #' And does not check for vocabulary identitiy -> lm
                                                       identical = function(other_state){
                                                         result <- setequal(class(self), class(other_state))
                                                         
                                                         if(result){
                                                           result <- result &&
                                                             self$get_state_character_id() == other_state$get_state_character_id() &&
                                                             self$get_max_history_length() == other_state$get_max_history_length() &&
                                                             self$get_simple_smooth_factor() == other_state$get_simple_smooth_factor() &&
                                                             self$get_total_transition_num() == other_state$get_total_transition_num() &&
                                                             setequal(names(self$get_child_state_list()), names(other_state$get_child_state_list()))
                                                         }
                                                         
                                                         if(result){
                                                           child_names <- names(self$get_child_state_list())
                                                           
                                                           transition_num <- self$get_child_transition_nums_list()
                                                           other_transition_num <- other_state$get_child_transition_nums_list()
                                                           
                                                           result <- result &&
                                                             all(sapply(child_names,
                                                                        FUN = function(state_name){
                                                                          return(transition_num[[state_name]] ==
                                                                                   other_transition_num[[state_name]])
                                                                        }))
                                                         }
                                                         
                                                         return(isTRUE(result))
                                                       },
                                                       
                                                       get_max_history_length = function(){
                                                         return(private$max_history_length)
                                                       },
                                                       
                                                       get_vocabulary = function(){
                                                         return(private$vocabulary)
                                                       },
                                                       
                                                       get_simple_smooth_factor = function(){
                                                         return(private$simple_smooth_factor)
                                                       },
                                                       
                                                       get_child_state_list = function(){
                                                         return(private$child_states_list)
                                                       },
                                                       
                                                       get_child_transition_nums_list = function(){
                                                         return(private$child_transition_nums_list)
                                                       },
                                                       
                                                       get_total_transition_num = function(){
                                                         return(private$total_transition_num)
                                                       },
                                                       
                                                       get_transition_target_character = function(word_id){
                                                         dim <- private$vocabulary$get_id_dims(word_id)
                                                         is_idle <- word_id == private$idle_id
                                                         l$s$err$assert_msg("Inconsistent 'dim' length.",
                                                                            l$s$mg$`%then%`(is_idle, length(dim) == private$num_dims),
                                                                            l$s$mg$`%then%`(!is_idle, length(dim) == 1))
                                                         if(is_idle){
                                                           next_state_char_id <- private$idle_state_character
                                                         }
                                                         else{
                                                           dim <- unlist(dim)
                                                           next_state_char_id <- private$state_word_id_listlist
                                                           if(length(next_state_char_id[[dim]]) < private$max_history_length){
                                                             next_state_char_id[[dim]] <- append(word_id, next_state_char_id[[dim]])
                                                           }
                                                           else{
                                                             next_state_char_id[[dim]] <- head(append(word_id, next_state_char_id[[dim]]), n = -1)
                                                           }
                                                           next_state_char_id <- l$get_unique_character_from_integer_listlist(next_state_char_id)
                                                         }
                                                         return(next_state_char_id)
                                                       },
                                                       
                                                       get_state_word_id_listlist = function(){
                                                         return(private$state_word_id_listlist)
                                                       },
                                                       
                                                       get_state_character_id = function(){
                                                         return(private$state_character_id)
                                                       },
                                                       
                                                       add_transition = function(word_id, states_list, num = 1){
                                                         private$assert_consistency()
                                                         l$s$err$assert_msg("Illegal argument.",
                                                                            word_id %in% private$vocabulary$get_all_ids(),
                                                                            all(sapply(states_list,
                                                                                       FUN = function(state){
                                                                                         return(is.R6(state) &&
                                                                                                  inherits(state, ".Multivariate_Count_LM_State"))
                                                                                       })),
                                                                            l$s$tcs$has_length_1(num, NA_on_fail = FALSE),
                                                                            l$s$tcs$is_integer(num,
                                                                                               accept_NULL = FALSE,
                                                                                               accept_NaN = FALSE,
                                                                                               accept_NA = FALSE,
                                                                                               lower_bound = 0,
                                                                                               lower_bound_inclusive = TRUE,
                                                                                               upper_bound = Inf,
                                                                                               upper_bound_inclusive = FALSE))
                                                         
                                                         next_state_char_id <- self$get_transition_target_character(word_id)
                                                         if(next_state_char_id %in% names(private$child_transition_nums_list)){
                                                           l$s$err$assert_msg("Inconsistent state.",
                                                                              next_state_char_id %in% names(states_list))
                                                           
                                                           private$child_transition_nums_list[[next_state_char_id]] <-
                                                             private$child_transition_nums_list[[next_state_char_id]] + num
                                                         }
                                                         else {
                                                           if(next_state_char_id %in% names(states_list)){
                                                             state <- states_list[[next_state_char_id]]
                                                           }
                                                           else{
                                                             state <- l$result_env$.Multivariate_Count_LM_State$new(
                                                               assertions_status = private$assertions_status,
                                                               state_character_id = next_state_char_id,
                                                               vocabulary = private$vocabulary,
                                                               max_history_length = private$max_history_length,
                                                               simple_smooth_factor = private$simple_smooth_factor)
                                                             
                                                             states_list[[next_state_char_id]] <- state
                                                           }
                                                           
                                                           private$child_transition_nums_list[[next_state_char_id]] <- num
                                                           private$child_states_list[[next_state_char_id]] <- state
                                                           private$child_transition_word_ids[[next_state_char_id]] <- word_id
                                                         }
                                                         
                                                         max_num <- max(as.numeric(private$child_transition_nums_list))
                                                         max_child_names <- list()
                                                         for(child_name in names(private$child_transition_nums_list)){
                                                           if(private$child_transition_nums_list[[child_name]] == max_num){
                                                             max_child_names <- append(max_child_names, child_name)
                                                           }
                                                         }
                                                         
                                                         private$argmax_word_ids_list <- private$child_transition_word_ids[as.character(max_child_names)]
                                                         
                                                         l$s$err$assert_msg("Inconsistent state.",
                                                                            length(private$argmax_word_ids_list) > 0,
                                                                            length(private$argmax_word_ids_list) <= length(private$child_transition_nums_list))
                                                         
                                                         private$total_transition_num <- private$total_transition_num + num
                                                         
                                                         private$assert_consistency()
                                                         return(states_list)
                                                       },
                                                       
                                                       get_child_transition_word_ids = function(){
                                                         return(private$child_transition_word_ids)
                                                       },
                                                       
                                                       get_argmax_word_ids_list = function(){
                                                         return(private$argmax_word_ids_list)
                                                       },
                                                       
                                                       get_probability = function(word_id, simple_smooth_factor = private$simple_smooth_factor){
                                                         next_state_char_id <- self$get_transition_target_character(word_id)
                                                         num_words <- private$vocabulary$get_num_word_ids()
                                                         
                                                         l$s$err$assert_msg("'private$vocabulary$get_num_word_ids()' should be > 0 at this point.",
                                                                            l$s$tcs$has_length_1(num_words),
                                                                            l$s$tcs$is_integer(num_words,
                                                                                               accept_NULL = FALSE,
                                                                                               accept_NaN = FALSE,
                                                                                               accept_NA = FALSE,
                                                                                               lower_bound = 1,
                                                                                               lower_bound_inclusive = TRUE,
                                                                                               upper_bound = Inf,
                                                                                               upper_bound_inclusive = FALSE),
                                                                            l$s$tcs$has_length_1(simple_smooth_factor),
                                                                            l$s$tcs$is_numeric(simple_smooth_factor,
                                                                                               accept_NULL = FALSE,
                                                                                               accept_NaN = FALSE,
                                                                                               accept_NA = FALSE,
                                                                                               lower_bound = 0,
                                                                                               lower_bound_inclusive = TRUE,
                                                                                               upper_bound = 1,
                                                                                               upper_bound_inclusive = TRUE,
                                                                                               accept_non_integer = TRUE))
                                                         
                                                         l$s$err$assert_msg("Inconsistent state.",
                                                                            l$s$mg$`%eq%`(next_state_char_id %in% names(private$child_transition_nums_list),
                                                                                          word_id %in% private$child_transition_word_ids))
                                                         
                                                         if(next_state_char_id %in% names(private$child_transition_nums_list)){
                                                           l$s$err$assert_msg("'total_transition_num' should be > 0 at this point.",
                                                                              l$s$tcs$has_length_1(private$total_transition_num),
                                                                              l$s$tcs$is_integer(private$total_transition_num,
                                                                                                 accept_NULL = FALSE,
                                                                                                 accept_NaN = FALSE,
                                                                                                 accept_NA = FALSE,
                                                                                                 lower_bound = 1,
                                                                                                 lower_bound_inclusive = TRUE,
                                                                                                 upper_bound = Inf,
                                                                                                 upper_bound_inclusive = FALSE))
                                                           
                                                           result <- (1 - simple_smooth_factor) * (private$child_transition_nums_list[[next_state_char_id]] / private$total_transition_num) +
                                                             simple_smooth_factor * (1 / num_words)
                                                         }
                                                         else if(length(private$child_transition_nums_list) > 0){
                                                           result <- simple_smooth_factor * (1 / num_words)
                                                         }
                                                         else{
                                                           result <- (1 / num_words)
                                                         }
                                                         
                                                         return(result)
                                                       },
                                                       
                                                       get_raw_probability = function(word_id){
                                                         return(self$get_probability(word_id = word_id, simple_smooth_factor = 0))
                                                       },
                                                       
                                                       finalize = function() {
                                                         super$finalize()
                                                         
                                                         #' Add your finalization code here:
                                                       }
                                                       
                                                     ),
                                                     #-----end of public-----
                                                     
                                                     #' ----- Add active bindings ----
                                                     active = list(
                                                       
                                                     )
                                                     #-----end of active-----
        )
        
        #' TODO: Add method to add new words
        #' TODO: Add number "decay" over time (i.e., LM should adapt to changing patterns)
        Multivariate_Count_LM <- R6Class("Multivariate_Count_LM",
                                         # ---- R6 inheritance -----
                                         #' __Optional__: Change to inherit from a different parent class.
                                         #'                     __IMPORTANT__: The parent class MUST either directly or indirectly
                                         #'                                    inherit from `R6_Base`!
                                         inherit = R6_Base,
                                         
                                         #' ---- Adapt R6 options --------
                                         portable = TRUE,
                                         cloneable = TRUE,
                                         lock_class = TRUE,
                                         lock_objects = TRUE,
                                         
                                         #' ----- Add private Fields & methods ----
                                         private = list(
                                           assertions_status = NULL,
                                           state_names_to_states_list = NULL,
                                           idle_state = NULL,
                                           current_state = NULL,
                                           locked = NULL,
                                           
                                           first_plot_out_path = NULL,
                                           
                                           raw_adjacency_matrix = NULL,
                                           raw_adjacency_matrix_igraph = NULL,
                                           raw_state_names_to_error_prob_list = NULL,
                                           locked_adj_and_err_fields = NULL,
                                           
                                           vocabulary = NULL,
                                           
                                           max_history_length = NULL,
                                           simple_smooth_factor = NULL,
                                           
                                           last_device = NULL,
                                           
                                           num_transitions = NULL,
                                           
                                           make_raw_adj_and_err_fields = function(){
                                             if(!private$locked_adj_and_err_fields || 
                                                is.null(private$raw_adjacency_matrix)){
                                               
                                               state_names <- sort(names(private$state_names_to_states_list))
                                               num_states <- length(state_names)
                                               
                                               adj_matrix <- matrix(data = 0, 
                                                                    nrow = num_states,
                                                                    ncol = num_states,
                                                                    dimnames = list(as.character(state_names), as.character(state_names)))
                                               raw_state_names_to_error_prob_list <- list()
                                               
                                               for(state_name in state_names){
                                                 raw_state_names_to_error_prob_list[[state_name]] <- 0
                                                 state <- private$state_names_to_states_list[[state_name]]
                                                 
                                                 argmax_word_id <- state$get_argmax_word_ids_list()[[1]]
                                                 child_transition_word_ids <- state$get_child_transition_word_ids()
                                                 child_names <- names(child_transition_word_ids)
                                                 for(child_name in child_names){
                                                   word_id <- child_transition_word_ids[[child_name]]
                                                   adj_matrix[state_name, child_name] <- state$get_raw_probability(word_id = word_id)
                                                   
                                                   if(word_id != argmax_word_id){
                                                     raw_state_names_to_error_prob_list[[state_name]] <-
                                                       raw_state_names_to_error_prob_list[[state_name]] + adj_matrix[state_name, child_name]
                                                   }
                                                 }
                                               }
                                               
                                               private$raw_adjacency_matrix <- adj_matrix
                                               private$raw_state_names_to_error_prob_list <- raw_state_names_to_error_prob_list
                                               private$raw_adjacency_matrix_igraph <- 
                                                 igraph::graph_from_adjacency_matrix(adjmatrix = private$raw_adjacency_matrix,
                                                                                     mode = "directed",
                                                                                     weighted = TRUE
                                                                                     )
                                               
                                               if(self$is_locked()){
                                                 private$locked_adj_and_err_fields <- TRUE
                                               }
                                             }
                                           },
                                           
                                           assert_consistency = function(){
                                             # At this point guaranteed that num_dims > 0 and each dimension has at least one word
                                             if(l$s$err$get_assertions_status()){
                                               idle_name <- private$idle_state$get_state_character_id()
                                               curr_name <- private$current_state$get_state_character_id()
                                               l$s$err$assert_msg("Inconsistent state.",
                                                                  idle_name %in% names(private$state_names_to_states_list),
                                                                  curr_name %in% names(private$state_names_to_states_list),
                                                                  identical(private$idle_state, private$state_names_to_states_list[[idle_name]]),
                                                                  identical(private$current_state, private$state_names_to_states_list[[curr_name]]),
                                                                  length(unique(unlist(names(private$state_names_to_states_list)))) == length(private$state_names_to_states_list))
                                             
                                               for(state_name in names(private$state_names_to_states_list)){
                                                 state <- private$state_names_to_states_list[[state_name]]
                                                 l$s$err$assert_msg("Inconsistent state.",
                                                                    identical(private$idle_state$get_vocabulary(), state$get_vocabulary()),
                                                                    private$idle_state$get_max_history_length() == state$get_max_history_length(),
                                                                    private$idle_state$get_simple_smooth_factor() == state$get_simple_smooth_factor())
                                                 
                                                 state_child_list <- state$get_child_state_list()
                                                 for(child_state_name in names(state_child_list)){
                                                   
                                                   l$s$err$assert_msg("Inconsistent states.",
                                                                      identical(state_child_list[[child_state_name]], 
                                                                                private$state_names_to_states_list[[child_state_name]]))
                                                 }
                                               }
                                               
                                               remove_visited <- function(current_state = private$idle_state, non_visited_states){
                                                 state_name <- current_state$get_state_character_id()
                                                 l$s$err$assert_msg("Inconsistent state.",
                                                                    state_name %in% names(private$state_names_to_states_list),
                                                                    identical(current_state, private$state_names_to_states_list[[state_name]]))
                                                 if(state_name %in% names(non_visited_states)){
                                                   non_visited_states[[state_name]] <- NULL
                                                   
                                                   for(child_state in current_state$get_child_state_list()){
                                                     non_visited_states <- remove_visited(current_state = child_state, non_visited_states = non_visited_states)
                                                   }
                                                 }
                                                 
                                                 return()
                                               }
                                               copystate_names_to_states_list <- private$state_names_to_states_list
                                               copystate_names_to_states_list <- remove_visited(non_visited_states = copystate_names_to_states_list)
                                               
                                               l$s$err$assert_msg(paste0("'state_names_to_states_list should only contain states ",
                                                                         "that are reachable from the idle state."),
                                                                  length(copystate_names_to_states_list) == 0)
                                               
                                             }
                                           },
                                           
                                           check_not_locked = function(){
                                             l$s$err$assert_msg("This instance is locked.",
                                                                !private$locked)
                                           },
                                           
                                           check_locked = function(){
                                             l$s$err$assert_msg("This instance is not locked.",
                                                                private$locked)
                                           },
                                           
                                           add_transition_unsafe = function(word_id, state_character_id = NULL, proceed = TRUE, num = 1){
                                             private$check_not_locked()
                                             l$s$err$assert_msg("Illegal arguments.",
                                                                l$s$tcs$has_length_1(proceed, NA_on_fail = FALSE),
                                                                l$s$tcs$is_logical(proceed,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE),
                                                                l$s$tcs$has_length_1(num, NA_on_fail = FALSE),
                                                                l$s$tcs$is_integer(num,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE,
                                                                                   lower_bound = 0,
                                                                                   lower_bound_inclusive = TRUE,
                                                                                   upper_bound = Inf,
                                                                                   upper_bound_inclusive = FALSE))
                                             
                                             if(is.null(state_character_id)){
                                               start_state <- private$current_state
                                             }
                                             else{
                                               l$s$err$assert_msg("Unknown 'state_character_id'.",
                                                                  state_character_id %in% names(private$state_names_to_states_list))
                                               start_state <- private$state_names_to_states_list[[state_character_id]]
                                             }
                                             
                                             next_state_char_id <- start_state$get_transition_target_character(word_id)
                                             if(!(next_state_char_id %in% names(start_state$get_child_transition_word_ids()))){
                                               private$num_transitions <- private$num_transitions + 1
                                             }
                                             
                                             private$state_names_to_states_list <- start_state$add_transition(
                                               word_id = word_id,
                                               states_list = private$state_names_to_states_list,
                                               num = num
                                             )
                                             
                                             l$s$err$assert_msg("Inconsistent state.",
                                                                next_state_char_id %in%
                                                                  names(private$state_names_to_states_list),
                                                                start_state$get_child_transition_word_ids()[[next_state_char_id]] == word_id)
                                             if(proceed){
                                               private$current_state <- private$state_names_to_states_list[[next_state_char_id]]
                                             }
                                             return(next_state_char_id)
                                           }
                                           
                                         ),
                                         #-----end of private-----
                                         
                                         public = list(
                                           
                                           #' See `R6_Base` in `utils_lang_r6_baseclass.R`
                                           #' @export
                                           #' @md
                                           get_static_env = function(){
                                             local_env$static_env
                                           },
                                           
                                           #' TODO: Transition order awareness by similar means as in JAVA DataStreamLanguageModelReceiver
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           initialize = function(assertions_status = FALSE,
                                                                 vocabulary,
                                                                 max_history_length,
                                                                 simple_smooth_factor = 0.1,
                                                                 first_plot_out_path = NULL,
                                                                 json_file = NULL,
                                                                 json_string = NULL){
                                             super$initialize()
                                             local_env$.add_to_static_env(super$get_static_env())
                                             local_env$static_env$err$set_assertions_status(assertions_status)
                                             
                                             l$s$err$assert_msg("'first_plot_out_path' must be either `NULL` or a character vector of length one.",
                                                                l$s$mg$`%then%`(!is.null(first_plot_out_path),
                                                                                l$s$tcs$has_length_1(first_plot_out_path, NA_on_fail = FALSE) &&
                                                                                l$s$tcs$is_character(first_plot_out_path,
                                                                                                     accept_NULL = FALSE,
                                                                                                     accept_NaN = FALSE,
                                                                                                     accept_NA = FALSE)))
                                             
                                             l$s$err$assert_msg("Illegal arguments.",
                                                                is.null(vocabulary) && is.null(max_history_length) && 
                                                                  is.null(simple_smooth_factor) && is.null(first_plot_out_path) ||
                                                                  !is.null(vocabulary) && !is.null(max_history_length) && 
                                                                  !is.null(simple_smooth_factor),
                                                                length(which(c(!is.null(json_file), !is.null(json_string), 
                                                                               !is.null(vocabulary) && !is.null(max_history_length) && 
                                                                                 !is.null(simple_smooth_factor)))) == 1)
                                            
                                             read_mode <- !is.null(json_file) || !is.null(json_string)
                                             json_obj <- NULL
                                             if(read_mode){
                                               if(!is.null(json_file)){
                                                 json_obj <- fromJSON(file = json_file)
                                               }
                                               else{
                                                 json_obj <- fromJSON(json_str = json_string)
                                               }
                                               vocabulary <- l$result_env$Multivariate_Character_Vocabulary$new(
                                                 assertions_status = assertions_status,
                                                 json_file = NULL,
                                                 json_string = json_obj[["vocabulary"]],
                                                 word_listlist = NULL,
                                                 idle_word = NULL
                                               )
                                               max_history_length <- json_obj[["max_history_length"]]
                                               simple_smooth_factor <- json_obj[["simple_smooth_factor"]]
                                               first_plot_out_path <- json_obj[["first_plot_out_path"]]
                                             }
                                             
                                             #' Add your initialization code here here:
                                             private$max_history_length <- max_history_length
                                             private$simple_smooth_factor <- simple_smooth_factor
                                             private$first_plot_out_path <- first_plot_out_path
                                             
                                             l$s$err$assert_msg("Illegal argument.",
                                                                is.R6(vocabulary),
                                                                inherits(vocabulary, "Multivariate_Character_Vocabulary"),
                                                                vocabulary$get_num_dims() > 0,
                                                                all(sapply(1:vocabulary$get_num_dims(),
                                                                           FUN = function(dim){
                                                                             return(vocabulary$get_num_ids_in_dim(dim = dim) > 0)
                                                                           })))
                                             
                                             private$state_names_to_states_list <- list()
                                             idle_char_id <- l$get_unique_character_from_integer_listlist(
                                               lapply(1:vocabulary$get_num_dims(),
                                                      FUN = function(i) return(list()))
                                             )
                                             
                                             private$idle_state <- l$result_env$.Multivariate_Count_LM_State$new(
                                               assertions_status = assertions_status,
                                               state_character_id = idle_char_id,
                                               vocabulary = vocabulary,
                                               max_history_length = max_history_length,
                                               simple_smooth_factor = simple_smooth_factor)
                                             
                                             private$state_names_to_states_list[[idle_char_id]] <- private$idle_state
                                             private$current_state <- private$idle_state
                                             private$locked <- FALSE
                                             private$assertions_status <- assertions_status
                                             
                                             private$num_transitions <- 0
                                             
                                             private$locked_adj_and_err_fields <- FALSE
                                             
                                             private$vocabulary <- vocabulary
                                             
                                             if(read_mode){
                                               l$s$err$assert_msg("Inconsistent state.")
                                               
                                               l$s$err$assert_msg("Inconsistent input file format.",
                                                                  length(names(json_obj[["states"]])) == length(json_obj[["states"]]))
                                               
                                               visited <- list()
                                               traverse_and_process <- function(state_name = private$idle_state$get_state_character_id()){
                                                 if(state_name %in% visited){
                                                   return()
                                                 }
                                                 else{
                                                   l$s$err$assert_msg("Inconsistent input file format.",
                                                                      state_name %in% names(json_obj[["states"]]),
                                                                      length(names(json_obj[["states"]][[state_name]])) ==
                                                                        length(json_obj[["states"]][[state_name]]))
                                                   
                                                   for(child_name in names(json_obj[["states"]][[state_name]])){
                                                     num <- json_obj[["states"]][[state_name]][[child_name]][["num"]]
                                                     word_id <- json_obj[["states"]][[state_name]][[child_name]][["reached_via_word_id"]]
                                                     
                                                     private$add_transition_unsafe(word_id = word_id, 
                                                                                   state_character_id = state_name, 
                                                                                   proceed = FALSE, 
                                                                                   num = num)
                                                     
                                                     l$s$err$assert_msg("Inconsistent 'reached_via_word_id' entry.",
                                                                        self$.get_state_names_to_states_list()[[state_name]]$get_transition_target_character(word_id) ==
                                                                          child_name)
                                                   }
                                                   visited <<- append(visited, state_name)
                                                   
                                                   for(child_name in names(json_obj[["states"]][[state_name]])){
                                                     traverse_and_process(state_name = child_name)
                                                   }
                                                 }
                                               }
                                               traverse_and_process()
                                               
                                               l$s$err$assert_msg("Inconsistent input file format - contains unreachable states.",
                                                                  length(visited) == length(json_obj[["states"]]))
                                               
                                               private$current_state <- private$state_names_to_states_list[[json_obj[["current_state_char_id"]]]]
                                             }
                                             
                                             private$assert_consistency()
                                           },
                                           
                                           get_max_history_length = function(){
                                             return(private$max_history_length)
                                           },
                                           
                                           get_simple_smooth_factor = function(){
                                             return(private$simple_smooth_factor)
                                           },
                                           
                                           get_vocabulary = function(){
                                             return(private$vocabulary)
                                           },
                                           
                                           as_json_string = function(){
                                             json_obj <- list()
                                             json_obj[["vocabulary"]] <- private$idle_state$get_vocabulary()$as_json_string()
                                             json_obj[["current_state_char_id"]] <- private$current_state$get_state_character_id()
                                             json_obj[["max_history_length"]] <- private$idle_state$get_max_history_length()
                                             json_obj[["simple_smooth_factor"]] <- private$idle_state$get_simple_smooth_factor()
                                             json_obj[["states"]] <- list()
                                             json_obj[["first_plot_out_path"]] <- private$first_plot_out_path
                                             
                                             if(self$get_num_states() >= 3){
                                               stats <- self$get_raw_stats()
                                               json_obj[names(stats)] <- stats
                                             }
                                             
                                             for(state_name in names(private$state_names_to_states_list)){
                                               json_obj[["states"]][[state_name]] <- list()
                                               
                                               state <- private$state_names_to_states_list[[state_name]]
                                               child_transition_nums_list <- state$get_child_transition_nums_list()
                                               child_transition_word_ids <- state$get_child_transition_word_ids()
                                               for(child_name in names(child_transition_nums_list)){
                                                 json_obj[["states"]][[state_name]][[child_name]] <- list()
                                                 json_obj[["states"]][[state_name]][[child_name]][["num"]] <- child_transition_nums_list[[child_name]]
                                                 json_obj[["states"]][[state_name]][[child_name]][["reached_via_word_id"]] <- child_transition_word_ids[[child_name]]
                                               }
                                             }
                                             
                                             return(toJSON(x = json_obj, indent = 2))
                                           },
                                           
                                           write_lm = function(json_file, overwrite = FALSE){
                                             l$s$err$assert_msg("Illegal argument.",
                                                                l$s$tcs$has_length_1(overwrite, NA_on_fail = FALSE),
                                                                l$s$tcs$is_logical(overwrite,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE))
                                             
                                             stopifnot(overwrite || !file.exists(json_file))
                                             
                                             dir <- dirname(json_file)
                                             if(!exists(dir)){
                                               dir.create(path = dir, showWarnings = FALSE, recursive = TRUE)
                                             }
                                             cat(self$as_json_string(), file = json_file)
                                             
                                             l$s$err$assert_msg("Error during storing and/or loading.",
                                                                self$identical(other_lm =
                                                                                 l$result_env$Multivariate_Count_LM$new(
                                                                                   assertions_status = private$assertions_status,
                                                                                   vocabulary = NULL,
                                                                                   max_history_length = NULL,
                                                                                   simple_smooth_factor = NULL,
                                                                                   json_file = json_file,
                                                                                   json_string = NULL
                                                                                 )))
                                           },
                                           
                                           #' NEITHER checks whether both lms are in currently in the same state
                                           #' NOR whether both lms have the same locked status
                                           #' NOR their assertions status
                                           identical = function(other_lm){
                                             result <- setequal(class(self), class(other_lm))
                                             
                                             if(result){
                                               #' Implies recursion over all child states.
                                               #' All states of an lm are guaranteed to be reachable from the
                                               #' idle state. The latter is guaranteed in check_consistency()
                                               result <- result &&
                                                 setequal(self$get_state_names(), other_lm$get_state_names()) &&
                                                 self$.get_idle_state()$get_vocabulary()$identical(
                                                   other_lm$.get_idle_state()$get_vocabulary()
                                                 )
                                               
                                               if(result){
                                                 states <- self$.get_state_names_to_states_list()
                                                 other_states <- other_lm$.get_state_names_to_states_list()
                                                 
                                                 result <- result && 
                                                   all(sapply(self$get_state_names(),
                                                              FUN = function(state_name){
                                                                states[[state_name]]$identical(other_states[[state_name]])
                                                              }))
                                               }
                                             }
                                             
                                             return(isTRUE(result))
                                           },
                                           
                                           add_transition = function(word_id, state_character_id = NULL, proceed = TRUE){
                                             private$assert_consistency()
                                             private$check_not_locked()
                                             return(private$add_transition_unsafe(word_id = word_id, 
                                                                                  state_character_id = state_character_id, 
                                                                                  proceed = proceed, 
                                                                                  num = 1))
                                             private$assert_consistency()
                                           },
                                           
                                           reset_current_state = function(state_character_id = NULL){
                                             private$assert_consistency()
                                             
                                             if(is.null(state_character_id)){
                                               reset_state <- private$idle_state
                                             }
                                             else{
                                               l$s$err$assert_msg("Unknown 'state_character_id'.",
                                                                  state_character_id %in% names(private$state_names_to_states_list))
                                               reset_state <- private$state_names_to_states_list[[state_character_id]]
                                             }
                                             private$current_state <- reset_state
                                             
                                             private$assert_consistency()
                                           },
                                           
                                           get_argmax_word_ids_list = function(state_character_id = NULL) {
                                             if(is.null(state_character_id)){
                                               state_character_id <- self$get_current_state_character_id()
                                             }
                                             l$s$err$assert_msg("Unknown 'state_character_id'.",
                                                                state_character_id %in% names(private$state_names_to_states_list))
                                             state <- private$state_names_to_states_list[[state_character_id]]
                                             return(state$get_argmax_word_ids_list())
                                           },
                                           
                                           #' Note: Only words to existing states are emitted
                                           emit_word_id = function(proceed = TRUE){
                                             l$s$err$assert_msg("Illegal arguments.",
                                                                l$s$tcs$has_length_1(proceed, NA_on_fail = FALSE),
                                                                l$s$tcs$is_logical(proceed,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE))
                                             word_ids <- self$get_word_ids_to_existing_state()
                                             probs <- sapply(word_ids,
                                                             FUN = function(word_id){
                                                               return(self$get_probability(
                                                                 word_id = word_id,
                                                                 state_character_id = NULL, 
                                                                 proceed = FALSE))
                                                             })
                                             if(length(probs) > 1){
                                               next_word <- unlist(sample(word_ids, size = 1, prob = probs))
                                             }
                                             else{
                                               next_word <- word_ids[[1]]
                                             }
                                             
                                             if(proceed){
                                               self$proceed(word_id = next_word)
                                             }
                                             
                                             return(next_word)
                                           },
                                           
                                           proceed = function(word_id){
                                             next_state_char_id <- private$current_state$get_transition_target_character(word_id)
                                             l$s$err$assert_msg(paste0("Cannot proceed to unknown next state with ID: \"", next_state_char_id, "\""),
                                                                next_state_char_id %in% names(private$state_names_to_states_list))
                                             
                                             self$reset_current_state(state_character_id = next_state_char_id)
                                           },
                                           
                                           get_probability = function(word_id, 
                                                                      simple_smooth_factor = private$idle_state$get_simple_smooth_factor(), 
                                                                      state_character_id = NULL, 
                                                                      proceed = TRUE){
                                             l$s$err$assert_msg("Illegal arguments.",
                                                                l$s$tcs$has_length_1(proceed, NA_on_fail = FALSE),
                                                                l$s$tcs$is_logical(proceed,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE))
                                             
                                             if(is.null(state_character_id)){
                                               start_state <- private$current_state
                                             }
                                             else{
                                               l$s$err$assert_msg("Unknown 'state_character_id'.",
                                                                  state_character_id %in% names(private$state_names_to_states_list))
                                               start_state <- private$state_names_to_states_list[[state_character_id]]
                                             }
                                             result <- start_state$get_probability(word_id = word_id, simple_smooth_factor = simple_smooth_factor)
                                             
                                             if(proceed){
                                               next_state_char_id <- start_state$get_transition_target_character(word_id)
                                               l$s$err$assert_msg(paste0("Cannot proceed to unknown next state with ID: \"", next_state_char_id, "\""),
                                                                  next_state_char_id %in% names(private$state_names_to_states_list))
                                               
                                               self$reset_current_state(state_character_id = next_state_char_id)
                                             }
                                             
                                             return(result)
                                           },
                                           
                                           get_raw_probability = function(word_id, 
                                                                          state_character_id = NULL, 
                                                                          proceed = TRUE){
                                             return(self$get_probability(word_id, 
                                                                         simple_smooth_factor = 0, 
                                                                         state_character_id = NULL, 
                                                                         proceed = TRUE))
                                           },
                                           
                                           #' "Raw" -> based on raw probabilities (i.e., probs with smooth factor = 0)
                                           get_raw_adjacency_matrix = function(){
                                             private$make_raw_adj_and_err_fields()
                                             
                                             return(private$raw_adjacency_matrix)
                                           },
                                           
                                           get_word_ids_to_non_existing_state = function(state_character_id = NULL){
                                             existing_state_names <- self$get_state_names()
                                             
                                             if(!is.null(state_character_id)){
                                               l$s$err$assert_msg("Illegal argument.",
                                                                  state_character_id %in% existing_state_names
                                                                  )
                                               state <- private$state_names_to_states_list[[state_character_id]]
                                             }
                                             else{
                                               state <- self$.get_current_state()
                                             }
                                             
                                             word_ids <- private$vocabulary$get_all_ids()
                                             result <- list()
                                             for(word_id in word_ids){
                                               new_state_name <- state$get_transition_target_character(word_id)
                                               if(!(new_state_name %in% existing_state_names)){
                                                 result <- append(result, word_id)
                                               }
                                             }
                                             l$s$err$assert_msg("Inconsistent state.",
                                                                !(private$vocabulary$get_idle_id() %in% result))
                                             return(result)
                                           },
                                           
                                           get_word_ids_to_existing_state = function(state_character_id = NULL, 
                                                                                     .word_ids_to_non_existing_state = self$get_word_ids_to_non_existing_state()){
                                             if(is.null(.word_ids_to_non_existing_state)){
                                               .word_ids_to_non_existing_state <- self$get_word_ids_to_non_existing_state()
                                             }
                                             
                                             result <- setdiff(private$vocabulary$get_all_ids(), .word_ids_to_non_existing_state)
                                             l$s$err$assert_msg("Inconsistent state.",
                                                                private$vocabulary$get_idle_id() %in% result,
                                                                length(result) + length(.word_ids_to_non_existing_state) == private$vocabulary$get_num_word_ids())
                                             
                                             return(as.list(result))
                                           },
                                           
                                           get_word_ids_to_existing_state_with_no_existing_edge = function(state_character_id = NULL, 
                                                                                     .word_ids_to_existing_state = self$get_word_ids_to_existing_state()){
                                             if(is.null(.word_ids_to_existing_state)){
                                               .word_ids_to_existing_state <- self$get_word_ids_to_existing_state()
                                             }
                                             
                                             existing_state_names <- self$get_state_names()
                                             if(!is.null(state_character_id)){
                                               l$s$err$assert_msg("Illegal argument.",
                                                                  state_character_id %in% existing_state_names
                                               )
                                               state <- private$state_names_to_states_list[[state_character_id]]
                                             }
                                             else{
                                               state <- self$.get_current_state()
                                             }
                                             
                                             result <- list()
                                             for(word_id in .word_ids_to_existing_state){
                                               if(!(word_id %in% state$get_child_transition_word_ids())){
                                                 result <- append(result, word_id)
                                               }
                                             }
                                             return(result)
                                           },
                                           
                                           #' "Raw" -> based on raw probabilities (i.e., probs with smooth factor = 0)
                                           get_raw_adjacency_matrix_igraph = function(){
                                             private$make_raw_adj_and_err_fields()
                                             
                                             return(private$raw_adjacency_matrix_igraph)
                                           },
                                           
                                           plot_raw_adjacency_matrix_igraph = function(rnd_seed = NULL,
                                                                                       plot_width = 1080 / 2,
                                                                                       plot_height = 1080 / 2,
                                                                                       plot_state_names = TRUE){
                                             if(!is.null(rnd_seed)){
                                               glob_env <- globalenv()
                                               old_rnd <- get(x = ".Random.seed", envir = glob_env)
                                               set.seed(seed = rnd_seed)
                                             }
                                             
                                             private$make_raw_adj_and_err_fields()
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
                                             l$s$err$assert_msg("'plot_state_names' must be a logical of length one.",
                                                                l$s$tcs$has_length_1(plot_state_names, NA_on_fail = FALSE),
                                                                l$s$tcs$is_logical(plot_state_names,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE))
                         
                                             if(!is.null(private$last_device) && !identical(dev.cur(), private$last_device)){
                                               found <- FALSE
                                               for(dev in dev.list()){
                                                 if(identical(private$last_device, dev)){
                                                   dev.set(dev)
                                                   found <- TRUE
                                                   break
                                                 }
                                               }
                                               if(!found){
                                                 private$last_device <- NULL
                                               }
                                             }
                                             
                                             if(is.null(private$last_device)){
                                               # Old dev not found -> create new one
                                               X11(width = plot_width / 72, 
                                                   height = plot_height / 72,
                                                   title = "Language model graph")
                                               private$last_device <- dev.cur()
                                             }
                                             
                                             
                                             layout <- layout_with_graphopt(private$raw_adjacency_matrix_igraph)
                                             layout <- layout - c(mean(layout[,1]), mean(layout[,2]))
                                             layout <- layout * 
                                               length(V(private$raw_adjacency_matrix_igraph)) * 3
                                             x_lims <- c(min(layout[,1]), max(layout[,1]))
                                             y_lims <- c(min(layout[,2]), max(layout[,2]))
                                             
                                             params <- list(x =private$raw_adjacency_matrix_igraph,
                                                            edge.curved = FALSE,
                                                            edge.label = round(E(private$raw_adjacency_matrix_igraph)$weight, digits = 2),
                                                            edge.width = E(private$raw_adjacency_matrix_igraph)$weight * 3,
                                                            vertex.label.color = "black",
                                                            edge.color = "grey",
                                                            edge.label.color = "black",
                                                            vertex.frame.color = "grey",
                                                            vertex.color = "grey",
                                                            layout = layout,
                                                            rescale = FALSE,
                                                            xlim = x_lims,
                                                            ylim = y_lims)
                                             
                                             if(!plot_state_names){
                                               params[["vertex.label"]] <- NA
                                             }
                                             do.call("plot", args = params)
                                             
                                             if(!is.null(private$first_plot_out_path)){
                                               dir <- dirname(private$first_plot_out_path)
                                               if(!exists(dir)){
                                                 dir.create(path = dir, showWarnings = FALSE, recursive = TRUE)
                                               }
                                               
                                               dev.print(svg, private$first_plot_out_path,
                                                         width = plot_width / 72, 
                                                         height = plot_height / 72,
                                                         pointsize = 12,
                                                         antialias = "subpixel")
                                               private$first_plot_out_path <- NULL
                                             }
                                             
                                             if(!is.null(rnd_seed)){
                                               assign(x = ".Random.seed", value = old_rnd, envir = glob_env)
                                             }
                                           },
                                           
                                           #' Returns list with entry names, but requires LM to consist of at least 3 states (including idle state):
                                           #' * min_error
                                           #' * entropy_rate
                                           #' * perplexity
                                           get_raw_stats = function(){
                                             l$s$err$stopifnot("get_raw_stats() only applicable for language models with at least 3 states (including idle state).",
                                               self$get_num_states() >= 3)
                                             
                                             private$make_raw_adj_and_err_fields()
                                             
                                             min_error <- 0
                                             l$s$err$stopifnot("Underlying graph must be strongly connected in order to compute meaningful statistics.",
                                                               self$is_strongly_connected())
                                             #' Compute stationary distribution
                                             state_probs <- Re(eigs(A = Matrix(t(private$raw_adjacency_matrix), sparse = TRUE), 1, which = "LR")$vectors[,1])
                                             state_probs <- sign(state_probs[abs(state_probs) == max(abs(state_probs))][1]) * state_probs
                                             state_probs <- state_probs / sum(state_probs)
                                             names(state_probs) <- rownames(private$raw_adjacency_matrix)
                                             negative_entries <- state_probs < 0
                                             if(any(negative_entries)){
                                               warning(paste0("Encountered negative state probability entries due to numeric erros - setting them to zero: ", 
                                                              toString(state_probs[negative_entries])) , immediate. = TRUE)
                                               state_probs[negative_entries] <- 0
                                             }
                                             
                                             for(state_name in names(private$state_names_to_states_list)){
                                               min_error <- min_error + state_probs[[state_name]] * private$raw_state_names_to_error_prob_list[[state_name]]
                                             }

                                             l$s$err$assert_msg("Inconsistent state.",
                                                                min_error <= 1,
                                                                min_error >= 0)
                                             
                                             entropy_rate <- 0
                                             for(state_name in names(private$state_names_to_states_list)){
                                               state_entropy <- 0
                                               state <- self$.get_state_names_to_states_list()[[state_name]]
                                               child_word_ids <- state$get_child_transition_word_ids()
                                               for(word_id in child_word_ids){
                                                 transition_prob <- state$get_raw_probability(word_id = word_id)
                                                 
                                                 l$s$err$assert_msg("Inconsistent state.",
                                                                    l$s$tcs$has_length_1(transition_prob, NA_on_fail = FALSE),
                                                                    l$s$tcs$is_numeric(transition_prob,
                                                                                       accept_NULL = FALSE,
                                                                                       accept_NaN = FALSE,
                                                                                       accept_NA = FALSE,
                                                                                       lower_bound = 0,
                                                                                       lower_bound_inclusive = TRUE,
                                                                                       upper_bound = 1,
                                                                                       upper_bound_inclusive = TRUE,
                                                                                       accept_non_integer = TRUE))
                                                 # More transitions -> more tolerance (total num transitions should be >= 1 at this point)
                                                 if(transition_prob >= 1e-10 / self$get_total_num_transitions()){
                                                   state_entropy <- state_entropy - transition_prob * log2(transition_prob) 
                                                 }
                                               }
                                               entropy_rate <- entropy_rate + state_probs[[state_name]] * state_entropy
                                             }
                                             
                                             perplexity_rate <- 2 ^ entropy_rate
                                             
                                             return(c(min_error = min_error, entropy_rate = entropy_rate, perplexity_rate = perplexity_rate))
                                           },
                                           
                                           is_strongly_connected = function(){
                                             igraph <- self$get_raw_adjacency_matrix_igraph()
                                             return(is_connected(igraph, mode = "strong"))
                                           },
                                           
                                           is_weakly_connected = function(){
                                             igraph <- self$get_raw_adjacency_matrix_igraph()
                                             return(is_connected(igraph, mode = "weak"))
                                           },
                                           
                                           lock = function(){
                                             private$locked <- TRUE
                                           },
                                           
                                           is_locked = function(){
                                             return(private$locked)
                                           },
                                           
                                           get_unique_character_from_integer_listlist = function(integer_listlist){
                                             return(l$get_unique_character_from_integer_listlist(integer_listlist))
                                           },
                                           
                                           get_unique_integer_listlist_from_character = function(character){
                                             return(l$get_unique_integer_listlist_from_character(character)) 
                                           },
                                           
                                           get_idle_state_character_id = function(){
                                             return(private$idle_state$get_state_character_id())
                                           },
                                           
                                           .get_idle_state = function(){
                                             return(private$idle_state)
                                           },
                                           
                                           get_current_state_character_id = function(){
                                             return(private$current_state$get_state_character_id())
                                           },
                                           
                                           is_in_idle_state = function(){
                                             return(identical(private$current_state, private$idle_state))
                                           },
                                           
                                           .get_current_state = function(){
                                             return(private$current_state)
                                           },
                                           
                                           .get_state_names_to_states_list = function(){
                                             return(private$state_names_to_states_list)
                                           },
                                           
                                           get_state_names = function(){
                                             return(names(private$state_names_to_states_list))
                                           },
                                           
                                           get_num_states = function(){
                                             return(length(private$state_names_to_states_list))
                                           },
                                           
                                           get_total_num_transitions = function(){
                                             l$s$err$assert_msg("Inconsistent state.",
                                                                sum(sapply(private$state_names_to_states_list,
                                                                           FUN = function(state){
                                                                             return(length(state$get_child_state_list()))
                                                                           })) == private$num_transitions)
                                             
                                             return(private$num_transitions)
                                           },
                                           
                                           finalize = function() {
                                             super$finalize()
                                             
                                             #' Add your finalization code here:
                                           }
                                           
                                         ),
                                         #-----end of public-----
                                         
                                         #' ----- Add active bindings ----
                                         active = list(
                                           
                                         )
                                         #-----end of active-----
        )
        
        Trainer_Multivariate_Count_LM <- R6Class("Trainer_Multivariate_Count_LM",
                                         # ---- R6 inheritance -----
                                         #' __Optional__: Change to inherit from a different parent class.
                                         #'                     __IMPORTANT__: The parent class MUST either directly or indirectly
                                         #'                                    inherit from `R6_Base`!
                                         inherit = R6_Base,
                                         
                                         #' ---- Adapt R6 options --------
                                         portable = TRUE,
                                         cloneable = TRUE,
                                         lock_class = TRUE,
                                         lock_objects = TRUE,
                                         
                                         #' ----- Add private Fields & methods ----
                                         private = list(
                                           assertions_status = NULL,
                                           
                                           vocabulary = NULL,
                                           max_history_length = NULL,
                                           simple_smooth_factor = NULL,
                                           first_plot_out_path = NULL,
                                           
                                           max_num_edge_insert_iterations = NULL,
                                           max_num_edge_weight_transitions = NULL,
                                           transition_to_new_state_weight = NULL,
                                           transition_to_existing_state_weight = NULL,
                                           min_transition_weight = NULL,
                                           max_transition_weight = NULL,
                                           
                                           assert_consistency = function(){
                                             # At this point guaranteed that num_dims > 0 and each dimension has at least one word
                                             if(l$s$err$get_assertions_status()){
                                               
                                             }
                                           },
                                           
                                           # List of length 3: 
                                           #   First entry: list of words to non existing states
                                           #   Second entry: list of words to existing states excluding idle word
                                           #   Third entry: Like second but additionally excluding words to existing states where there exists an edge to that state 
                                           get_w = function(lm, state){
                                             state_id <- state$get_state_character_id()
                                             w_new <- lm$get_word_ids_to_non_existing_state(state_character_id = state_id)
                                             w_existing <- lm$get_word_ids_to_existing_state(state_character_id = state_id,
                                                                                             .word_ids_to_non_existing_state = w_new)
                                             w_existing_no_edges <- lm$get_word_ids_to_existing_state_with_no_existing_edge(state_character_id = state_id,
                                                                                                                            .word_ids_to_existing_state = w_existing)
                                             idle_id <- private$vocabulary$get_idle_id()
                                             l$s$err$assert_msg("Inconsistent state.",
                                                                length(w_existing) == private$vocabulary$get_num_word_ids() - length(w_new))
                                             w_existing <- setdiff(w_existing, idle_id)
                                             l$s$err$assert_msg("Inconsistent state.",
                                                                length(w_existing) == private$vocabulary$get_num_word_ids() - length(w_new) - 1)
                                             w_existing_no_edges <- setdiff(w_existing_no_edges, idle_id)
                                             l$s$err$assert_msg("Inconsistent state.",
                                                                length(w_existing_no_edges) <= length(w_existing))
                                             
                                             return(list(w_new, w_existing, w_existing_no_edges))
                                           },
                                           
                                           edge_insertion_phase = function(lm){
                                             idle_char_id <- lm$get_idle_state_character_id()
                                             
                                             l$s$err$assert_msg("Illegal argument.",
                                                                is.R6(lm),
                                                                inherits(lm, "Multivariate_Count_LM"),
                                                                lm$.get_current_state()$get_state_character_id() == idle_char_id,
                                                                length(lm$get_state_names) == 1)
                                             
                                             current_iteration_count <- 0
                                             idle_word_id <- private$vocabulary$get_idle_id()
                                             
                                             new_state_weight <- private$transition_to_new_state_weight
                                             existing_state_weight <- private$transition_to_existing_state_weight
                                             idle_state_weight <- 1
                                             while( current_iteration_count < private$max_num_edge_insert_iterations){
                                               current_state <- lm$.get_current_state()
                                               current_state_char_id <- current_state$get_state_character_id()
                                               if(current_state_char_id == idle_char_id && sample(c(TRUE,FALSE), size = 1, prob = c(private$transition_to_existing_state_weight, 1))){
                                                current_state <- lm$.get_state_names_to_states_list()[[sample(lm$get_state_names(), size = 1)]]
                                                current_state_char_id <- current_state$get_state_character_id()
                                                lm$reset_current_state(state_character_id = current_state_char_id)
                                                
                                                graph <- lm$get_raw_adjacency_matrix_igraph()
                                                distance_from_idle <- shortest_paths(graph = graph,
                                                                              from = V(graph)[[idle_char_id]],
                                                                              to = V(graph)[[current_state_char_id]],
                                                                              mode = "out",
                                                                              output = "epath")
                                                l$s$err$assert_msg("Inconsistent state.",
                                                                   length(distance_from_idle$epath) == 1)
                                                distance_from_idle <- length(distance_from_idle$epath[[1]])
                                                new_state_weight <- new_state_weight - min(distance_from_idle, new_state_weight)
                                               }
                                               w <- private$get_w(lm, current_state)
                                               w_new <- w[[1]]
                                               w_existing <- w[[2]]
                                               # Currently not required
                                               # TODO: Jumping to existing node might break strong connectivity -> Future versions: Annotate nodes which are part of at least
                                               # one directed path starting and ending at idle state -> only jumps to these states allowed
                                               # BETTER: At the end of edge insertion phase: Add fixed number of shortcut edges -> remove transition_to_existing_weight parameter!
                                               #w_existing_no_edge <- w[[3]]
                                               
                                               l$s$err$assert_msg("Inconsistent state.",
                                                                  setequal(w_existing, 
                                                                           setdiff(lm$get_word_ids_to_existing_state(state_character_id = 
                                                                                                                       current_state$get_state_character_id()), 
                                                                                   private$vocabulary$get_idle_id())))
                                               # Vocabulary class should guarantee that there is always at least one
                                               # word dimension and no dimension is empty
                                               l$s$err$assert_msg("Inconsistent state.",
                                                                  length(w_new) + length(w_existing) > 0)
                                               
                                               #' No transitions to new states possible?
                                               if(length(w_new) == 0){
                                                 new_state_weight <- 0
                                               }
                                               
                                               # No idle-idle transitions:
                                               # action == 1: jump to new state
                                               # action == 2: jump back to idle
                                               if(current_state$get_state_character_id() == lm$get_idle_state_character_id()){
                                                 if(length(w_new) == 0){ # No edge insertion possible
                                                   break
                                                 }
                                                 else{
                                                  action <- 1
                                                 }
                                               }
                                               else{
                                                 action <- sample(x = c(1,2), size = 1, prob = c(new_state_weight, idle_state_weight))
                                               }
                                               
                                               if(action == 1){
                                                 # New node
                                                 l$s$err$assert_msg("Inconsistent state.",
                                                                    length(w_new) > 0)
                                                 if(length(w_new) > 1){
                                                   next_word_id <- sample(w_new, size = 1)[[1]]
                                                 }
                                                 else{
                                                   next_word_id <- w_new[[1]]
                                                 }
                                               }
                                               else{
                                                 l$s$err$assert_msg("Inconsistent state.",
                                                                    action == 2)
                                                 # Idle node
                                                 next_word_id <- idle_word_id
                                                 #' Reset weights
                                                 new_state_weight <- private$transition_to_new_state_weight
                                                 existing_state_weight <- private$transition_to_existing_state_weight
                                                 idle_state_weight <- 1
                                               }
                                               
                                               next_state_name <- current_state$get_transition_target_character(word_id = next_word_id)
                                               if(l$s$err$get_assertions_status()){
                                                 if(action == 1){
                                                   l$s$err$assert_msg("Inconsistent state.",
                                                                      !(next_state_name %in% lm$get_state_names()),
                                                                      next_word_id %in% w_new,
                                                                      !(next_word_id %in% w_existing))
                                                 }
                                                 else{
                                                   l$s$err$assert_msg("Inconsistent state.",
                                                                      action == 2,
                                                                      !(next_word_id %in% w_existing),
                                                                      !(next_word_id %in% w_new),
                                                                      next_word_id == idle_word_id)
                                                 }
                                               }
                                               
                                               hasEdge <- next_state_name %in% names(current_state$get_child_transition_nums_list())
                                               l$s$err$assert_msg("Inconsistent state.",
                                                                  l$s$mg$`%then%`(hasEdge, action == 2))
                                               if(!hasEdge){
                                                 lm$add_transition(word_id = next_word_id, 
                                                                   state_character_id = NULL, 
                                                                   proceed = TRUE)
                                               }
                                               else{
                                                 lm$reset_current_state(state_character_id = next_state_name)
                                               }
                                               current_iteration_count <- current_iteration_count + 1
                                               
                                               if(l$s$err$get_assertions_status()){
                                                 last_state <- current_state
                                                 current_state <- lm$.get_current_state()
                                                 current_state_name <- current_state$get_state_character_id()
                                                 current_expected_name <- last_state$get_transition_target_character(next_word_id)
                                                 l$s$err$assert_msg("Inconsistent state.",
                                                                    current_state_name == current_expected_name,
                                                                    current_state$get_child_transition_nums_list()[[next_state_name]] == 1)
                                                 
                                                 l$s$err$assert_msg("No idle-idle transition should be possible.",
                                                                    l$s$mg$`%then%`(next_word_id == idle_word_id, 
                                                                                    last_state$get_state_character_id() != lm$get_idle_state_character_id()))
                                               }
                                             }
                                               
                                             #' Add edge to idle if required
                                             current_state <- lm$.get_current_state()
                                             current_state_name <- current_state$get_state_character_id()
                                             #' Last state: Add transition to idle, if this is not the idle state
                                             if(current_state_name != idle_char_id){
                                               # If node has children, there should be a path from its children to idle state
                                               # since those must have been created during a previous idle-to-idle path creation
                                               hasChildren <- length(current_state$get_child_state_list()) > 0
                                               
                                               if(!hasChildren){
                                                 lm$add_transition(word_id = idle_word_id, 
                                                                   state_character_id = NULL, 
                                                                   proceed = TRUE)
                                                 l$s$err$assert_msg("Inconsistent state.",
                                                                    current_state$get_child_transition_nums_list()[[idle_char_id]] == 1)
                                                 
                                               }
                                               else{
                                                 lm$reset_current_state(state_character_id = idle_char_id)
                                               }
                                             }
                                             l$s$err$assert_msg("Inconsistent state.",
                                                                lm$.get_current_state()$get_state_character_id() == idle_char_id)
                                             #' TODO: Make this an assertion after sufficient tests
                                             l$s$err$stopifnot("Inconsistent state.",
                                                               lm$is_strongly_connected())
                                             
                                           },
                                           
                                           edge_weight_phase = function(lm) {
                                             idle_char_id <- lm$get_idle_state_character_id()
                                             idle_word_id <- private$vocabulary$get_idle_id()
                                             state_names_to_states_list <- lm$.get_state_names_to_states_list()
                                             state_names <- names(state_names_to_states_list)
                                             l$s$err$assert_msg("Illegal argument.",
                                                                is.R6(lm),
                                                                inherits(lm, "Multivariate_Count_LM"),
                                                                lm$.get_current_state()$get_state_character_id() == idle_char_id,
                                                                all(sapply(state_names_to_states_list,
                                                                           FUN = function(state){
                                                                             return(all(sapply(state$get_child_transition_nums_list(),
                                                                                               FUN = function(num){
                                                                                                 return(num == 1)
                                                                                               })))
                                                                           })))
                                             
                                             current_iteration_count <- 0
                                             weight_list <- list()
                                             for(state_name in state_names){
                                               state <- state_names_to_states_list[[state_name]]
                                               child_names <- names(state$get_child_state_list())
                                               num_children <- length(child_names)
                                               l$s$err$assert_msg("Inconsistent state.",
                                                                  num_children > 0)
                                               weight_list[[state_name]] <- lapply(1:num_children,
                                                                 FUN = function(i){
                                                                   result <- runif(n = 1, 
                                                                                   min = private$min_transition_weight,
                                                                                   max = private$max_transition_weight)
                                                                   })
                                               names(weight_list[[state_name]]) <- child_names
                                               
                                               l$s$err$assert_msg("Inconsistent state.",
                                                                  length(weight_list[[state_name]]) == num_children,
                                                                  length(weight_list[[state_name]]) == length(state$get_child_transition_word_ids()))
                                             }
                                             
                                             current_state <- lm$.get_current_state()
                                             while(current_iteration_count < private$max_num_edge_weight_transitions
                                                   || lm$.get_current_state()$get_state_character_id() != idle_char_id){
                                               current_state <- lm$.get_current_state()
                                               state_name <- current_state$get_state_character_id()
                                               
                                               child_transition_word_ids <- current_state$get_child_transition_word_ids()
                                               transition_weights <- weight_list[[state_name]]
                                               
                                               l$s$err$assert_msg("Inconsistent state.",
                                                                  length(child_transition_word_ids) == length(transition_weights))
                                               
                                               if(length(child_transition_word_ids) > 1){
                                                next_word_id <- sample(x = child_transition_word_ids, size = 1, prob = transition_weights)[[1]]
                                               }
                                               else{
                                                 next_word_id <- child_transition_word_ids[[1]]
                                               }
                                               
                                               if(l$s$err$get_assertions_status()){
                                                 next_state_name <- current_state$get_transition_target_character(word_id = next_word_id)
                                                 l$s$err$assert_msg("Inconsistent state.",
                                                                    next_state_name %in% lm$get_state_names(),
                                                                    current_state$get_child_transition_nums_list()[[next_state_name]] > 0)
                                               }
                                               
                                               lm$add_transition(word_id = next_word_id, 
                                                                 state_character_id = NULL, 
                                                                 proceed = TRUE)
                                               current_iteration_count <- current_iteration_count + 1
                                               
                                               if(l$s$err$get_assertions_status()){
                                                 last_state <- current_state
                                                 current_state <- lm$.get_current_state()
                                                 current_state_name <- current_state$get_state_character_id()
                                                 current_expected_name <- last_state$get_transition_target_character(next_word_id)
                                                 l$s$err$assert_msg("Should never happen.",
                                                                    current_state_name == current_expected_name)
                                                 l$s$err$assert_msg("Should never happen.",
                                                                    l$s$mg$`%then%`(next_word_id == idle_word_id, 
                                                                                    last_state$get_state_character_id() != lm$get_idle_state_character_id()))
                                               }
                                             }
                                           }
                                           
                                         ),
                                         #-----end of private-----
                                         
                                         public = list(
                                           
                                           #' See `R6_Base` in `utils_lang_r6_baseclass.R`
                                           #' @export
                                           #' @md
                                           get_static_env = function(){
                                             local_env$static_env
                                           },
                                           
                                          
                                           #' Two training phases:
                                           #' 
                                           #' 1. Edge insertions: Insertion of non-existing edges into the underlying
                                           #'                     lm graph. All transition nums are 1.
                                           #'                     Starting at state s=idle, do the following:
                                           #'                     
                                           #'                     Given: Current state s, let W_new(s) be the set of word IDs
                                           #'                     resulting in a new state if emitted at s and
                                           #'                     W_existing(s) = W SETMINUS (W_new(s) UNION idle_id)
                                           #'                     Note that idle_id can never be in W_new(s).
                                           #'                     
                                           #'                     1. If s = idle: With probability 
                                           #'                        `transition_to_existing_state_weight / (transition_to_existing_state_weight + 1)`,
                                           #'                        set s to a randomly chosen existing state. 
                                           #'                        Note: Probability of staying at s is `1 / (transition_to_existing_state_weight + 1)`.
                                           #'                        (Idea: Control lm ambiguity with transition_to_existing_state_weight).
                                           #'                     2.1 If s = idle, choose arbitrary word ID from W_new(s) and add correspodning transition
                                           #'                        while proceeding to the new state. If this is not possible (i.e., W_new(s) is empty),
                                           #'                        finish edge insertion phase.
                                           #'                     2.2 If s != idle:   
                                           #'                        With probability p_new = 
                                           #'                        (|W_new(s)| > 0) * transition_to_new_state_weight / NORM 
                                           #'                        (with (|W_new(s)| > 0 ) being 1, if condition is true, and 0 otherwise),
                                           #'                        choose arbitrary word ID from W_new(s) and with probability 1 / NORM, choose
                                           #'                        the idle word ID, add transition to corresponding target state (in case of idle state,
                                           #'                        only if such a transition does not already exist) and proceed to that state.
                                           #'                        (Idea: Control average sequence length via transition_to_new_state_weight).
                                           #'                     3. Repeat above steps for 'max_num_edge_insert_iterations' steps. 
                                           #'                     
                                           #'                     4. If current state s is not ide, add transition to idle, if not present, and proceed to idle.
                                           #'                        
                                           #'                     Note: NORM = (|W_new(s)| > 0) * transition_to_new_state_weight + 1.
                                           #'                     
                                           #' 2. Edge weight: Assign each edge with arbitrarily chosen weights chosen from
                                           #'                 Uniform(min_transition_weight, max_transition_weight)
                                           #'                 and traverse the graph while increasing transition count.
                                           #'                 At each edge, chose branch according to assigned weights.
                                           #'                 If 'max_num_edge_weight_transitions' have been performed,
                                           #'                 use first encountered idle-branch to return to idle and finish.
                                           #' 
                                           #' @export
                                           #'
                                           #' @examples
                                           #' @md
                                           initialize = function(assertions_status = FALSE,
                                                                 vocabulary,
                                                                 
                                                                 max_history_length,
                                                                 simple_smooth_factor = 0.1,
                                                                 first_plot_out_path = NULL,
                                                                 
                                                                 max_num_edge_insert_iterations,
                                                                 max_num_edge_weight_transitions,
                                                                 # Higher value -> longer sequences
                                                                 transition_to_new_state_weight,
                                                                 # Higher value -> more "shortcuts" in lm graph -> more ambiguous lm
                                                                 transition_to_existing_state_weight,
                                                                 # Higher range -> larger probability variance of outgoing edges of each vertex
                                                                 min_transition_weight = 1,
                                                                 max_transition_weight = 9
                                                                 ){
                                             super$initialize()
                                             local_env$.add_to_static_env(super$get_static_env())
                                             local_env$static_env$err$set_assertions_status(assertions_status)
                                             
                                             l$s$err$assert_msg("Illegal arguments.",
                                                                is.R6(vocabulary) && inherits(vocabulary, "Multivariate_Character_Vocabulary"),
                                                                l$s$tcs$has_length_1(max_num_edge_insert_iterations, NA_on_fail = FALSE),
                                                                l$s$tcs$has_length_1(max_num_edge_weight_transitions, NA_on_fail = FALSE),
                                                                l$s$tcs$has_length_1(transition_to_new_state_weight, NA_on_fail = FALSE),
                                                                l$s$tcs$has_length_1(transition_to_existing_state_weight, NA_on_fail = FALSE),
                                                                l$s$tcs$has_length_1(min_transition_weight, NA_on_fail = FALSE),
                                                                l$s$tcs$has_length_1(max_transition_weight, NA_on_fail = FALSE),
                                                                l$s$tcs$is_integer(max_num_edge_insert_iterations,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE,
                                                                                   lower_bound = 0,
                                                                                   lower_bound_inclusive = TRUE,
                                                                                   upper_bound = Inf,
                                                                                   upper_bound_inclusive = FALSE),
                                                                l$s$tcs$is_integer(max_num_edge_weight_transitions,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE,
                                                                                   lower_bound = 0,
                                                                                   lower_bound_inclusive = TRUE,
                                                                                   upper_bound = Inf,
                                                                                   upper_bound_inclusive = FALSE),
                                                                l$s$tcs$is_numeric(transition_to_new_state_weight,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE,
                                                                                   lower_bound = 0,
                                                                                   lower_bound_inclusive = FALSE,
                                                                                   upper_bound = Inf,
                                                                                   upper_bound_inclusive = FALSE,
                                                                                   accept_non_integer = TRUE),
                                                                l$s$tcs$is_numeric(transition_to_existing_state_weight,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE,
                                                                                   lower_bound = 0,
                                                                                   lower_bound_inclusive = TRUE,
                                                                                   upper_bound = Inf,
                                                                                   upper_bound_inclusive = FALSE,
                                                                                   accept_non_integer = TRUE),
                                                                l$s$tcs$is_numeric(min_transition_weight,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE,
                                                                                   lower_bound = 0,
                                                                                   lower_bound_inclusive = TRUE,
                                                                                   upper_bound = Inf,
                                                                                   upper_bound_inclusive = FALSE,
                                                                                   accept_non_integer = TRUE),
                                                                l$s$tcs$is_numeric(max_transition_weight,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE,
                                                                                   lower_bound = min_transition_weight,
                                                                                   lower_bound_inclusive = TRUE,
                                                                                   upper_bound = Inf,
                                                                                   upper_bound_inclusive = FALSE,
                                                                                   accept_non_integer = TRUE),
                                                                l$s$tcs$has_length_1(max_history_length, NA_on_fail = FALSE),
                                                                l$s$tcs$is_integer(max_history_length,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE,
                                                                                   lower_bound = 1,
                                                                                   lower_bound_inclusive = TRUE,
                                                                                   upper_bound = Inf,
                                                                                   upper_bound_inclusive = FALSE),
                                                                l$s$tcs$has_length_1(simple_smooth_factor, NA_on_fail = FALSE),
                                                                l$s$tcs$is_numeric(simple_smooth_factor,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE,
                                                                                   lower_bound = 0,
                                                                                   lower_bound_inclusive = TRUE,
                                                                                   upper_bound = 1,
                                                                                   upper_bound_inclusive = TRUE,
                                                                                   accept_non_integer = TRUE)
                                                                )
                                             
                                             private$assertions_status <- assertions_status
                                             
                                             private$vocabulary <- vocabulary
                                             private$max_history_length <- max_history_length
                                             private$simple_smooth_factor <- simple_smooth_factor
                                             private$first_plot_out_path <- first_plot_out_path
                                             
                                             private$max_num_edge_insert_iterations <- max_num_edge_insert_iterations
                                             private$max_num_edge_weight_transitions <- max_num_edge_weight_transitions
                                             private$transition_to_new_state_weight <- transition_to_new_state_weight
                                             private$transition_to_existing_state_weight <- transition_to_existing_state_weight
                                             private$min_transition_weight <- min_transition_weight
                                             private$max_transition_weight <- max_transition_weight
                                           },
                                           
                                           train_new_lm = function(rnd_seed = NULL, 
                                                                   lock = TRUE){
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
                                                                                                   upper_bound_inclusive = FALSE)),
                                                                l$s$tcs$has_length_1(lock, NA_on_fail),
                                                                l$s$tcs$is_logical(lock,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE))
                                             
                                             if(!is.null(rnd_seed)){
                                               glob_env <- globalenv()
                                               original_seed <- get(x = ".Random.seed", envir = glob_env)
                                               set.seed(rnd_seed)
                                             }
                                             
                                             lm <- l$result_env$Multivariate_Count_LM$new(
                                               assertions_status = private$assertions_status,
                                               vocabulary = private$vocabulary,
                                               max_history_length = private$max_history_length,
                                               simple_smooth_factor = private$simple_smooth_factor,
                                               first_plot_out_path = private$first_plot_out_path,
                                               json_file = NULL,
                                               json_string = NULL
                                             )
                                             
                                             private$edge_insertion_phase(lm)
                                             private$edge_weight_phase(lm)
                                           
                                             l$s$err$assert_msg("Inconsistent state.",
                                                                lm$.get_current_state()$get_state_character_id() ==
                                                                  lm$get_idle_state_character_id())
                                             
                                             if(!is.null(rnd_seed)){
                                               assign(x = ".Random.seed", value = original_seed, envir = glob_env)
                                             }
                                             
                                             if(lock){
                                               lm$lock()
                                             }
                                             return(lm)
                                           },
                                           
                                           finalize = function() {
                                             super$finalize()
                                             
                                             #' Add your finalization code here:
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