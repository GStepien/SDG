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
#' @title Collection of subclasses of `Abstract_Iterator`s from `utils_iterators.R` for 
#'   iteration over __alphanumeric__ values.
#' @description Contains a single [get_<name>_env()] method
#'   that returns an environment containing the `R6` constructor objects of these subclasses.
#' @md

# Include guard
if (!exists("ALPHANUM_ITERATORS_R", inherits = FALSE)) {
  ALPHANUM_ITERATORS_R = TRUE
  
  #' @return An environment containing one or multiple constructor objects for `R6` subclasses of 
  #'         `Abstract_Iterator` from `utils_iterators.R`. These provide functionality for iteration
  #'         over alphanumeric values.
  #' @export
  #'
  #' @examples
  #' @md
  get_alphanum_iterators_env <- function() {
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
                       file.path(".", "datastream", "character_iterators.R"),
                       file.path(".", "datastream", "numeric_iterators.R"),
                       file.path(".", "utils", "utils_math_general.R"),
                       file.path(".", "utils", "utils_data_tools.R"),
                       file.path(".", "utils", "utils_data_structures.R"),
                       file.path(".", "utils", "utils_lang_typechecks.R"),
                       file.path(".", "utils", "utils_time.R"),
                       file.path(".", "utils", "utils_factory.R"),
                       file.path(".", "utils", "utils_color.R"),
                       file.path(".", "datastream", "language_models.R"))   # e.g.: c(file.path(".", "utils", "utils_math_general.R"))
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
    assign("character_iterators", get_character_iterators_env(), envir = static_env)
    assign("numeric_iterators", get_numeric_iterators_env(), envir = static_env)
    assign("utils_math_general", get_utils_math_general_env(), envir = static_env)
    assign("utils_data_tools", get_utils_data_tools_env(), envir = static_env)
    assign("utils_data_structures", get_utils_data_structures_env(), envir = static_env)
    assign("utils_lang_typechecks", get_utils_lang_typechecks_env(), envir = static_env)
    assign("utils_time", get_utils_time_env(), envir = static_env)
    assign("utils_factory", get_utils_factory_env(), envir = static_env)
    assign("utils_color", get_utils_color_env(), envir = static_env)
    assign("language_models", get_language_models_env(), envir = static_env)
    
    #' __Optional__: Add short-hand variants of the above:
    #' E.g.: assign("gen", static_env$utils_lang_general, envir = static_env)
    assign("it", static_env$utils_iterators, envir = static_env)
    assign("err", static_env$utils_lang_error, envir = static_env)
    assign("cit", static_env$character_iterators, envir = static_env)
    assign("nit", static_env$numeric_iterators, envir = static_env)
    assign("mg", static_env$utils_math_general, envir = static_env)
    assign("dts", static_env$utils_data_tools, envir = static_env)
    assign("ds", static_env$utils_data_structures, envir = static_env)
    assign("tcs", static_env$utils_lang_typechecks, envir = static_env)
    assign("t", static_env$utils_time, envir = static_env)
    assign("fac", static_env$utils_factory, envir = static_env)
    assign("col", static_env$utils_color, envir = static_env)
    assign("lm", static_env$language_models, envir = static_env)
    
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
    json_is_NA_rnd_walk_name <- "is_NA_rnd_walk"
    json_is_padding_NA_rnd_walk_name <- "is_padding_NA_rnd_walk"
    json_is_intra_NA_rnd_walk_name <- "is_intra_NA_rnd_walk"
    json_is_inter_NA_rnd_walk_name <- "is_inter_NA_rnd_walk"
    
    json_is_character_sequence_name <- "is_character_sequence"
    json_is_NA_character_sequence_name <- "is_NA_character_sequence"
    json_is_padding_NA_character_sequence_name <- "is_padding_NA_character_sequence"
    json_is_intra_NA_character_sequence_name <- "is_intra_NA_character_sequence"
    json_is_inter_NA_character_sequence_name <- "is_inter_NA_character_sequence"
    
    required_param_assertions_status <- "assertions_status"
    required_param_fixed_length <- "fixed_length"
    required_param_initial_value <- "initial_value"
    required_param_element <- "element"
    required_param_seed <- "seed"
    
    json_is_NA_rnd_walk_name <- trimws(json_is_NA_rnd_walk_name)
    json_is_padding_NA_rnd_walk_name <- trimws(json_is_padding_NA_rnd_walk_name)
    json_is_intra_NA_rnd_walk_name <- trimws(json_is_intra_NA_rnd_walk_name)
    json_is_inter_NA_rnd_walk_name <- trimws(json_is_inter_NA_rnd_walk_name)
    
    json_is_character_sequence_name <- trimws(json_is_character_sequence_name)
    json_is_NA_character_sequence_name <- trimws(json_is_NA_character_sequence_name)
    json_is_padding_NA_character_sequence_name <- trimws(json_is_padding_NA_character_sequence_name)
    json_is_intra_NA_character_sequence_name <- trimws(json_is_intra_NA_character_sequence_name)
    json_is_inter_NA_character_sequence_name <- trimws(json_is_inter_NA_character_sequence_name)
    
    .unique_event_env <- new.env()
    assign(x = ".unique_event_id", value = 0, envir = .unique_event_env)
    lockEnvironment(env = .unique_event_env, bindings = FALSE)
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
        
        #' A subclass of `Iterator_Factory` from `utils_iterators.R`
        #' adding assertions that objects created in `create_object(...)` are
        #' (possibly decorated) instances of `Abstract_Numeric_Iterator`, `Numeric_Offset_Normalized_Concatenated_Iterator` 
        #' or `Character_Queue_Iterator`.
        Alphanumeric_Iterator_Factory <- R6Class("Alphanumeric_Iterator_Factory",
                                            # ---- R6 inheritance -----
                                            #'  __Optional__: Change to inherit from a different parent class.
                                            #'                     __IMPORTANT__: The parent class MUST either directly or indirectly
                                            #'                                    inherit from `R6_Base`!
                                            inherit = l$s$fac$Factory,
                                            
                                            #' ---- Adapt R6 options --------
                                            portable = TRUE,
                                            cloneable = TRUE,
                                            lock_class = TRUE,
                                            lock_objects = TRUE,
                                            
                                            #' ----- Add private Fields & methods ----
                                            private = list(
                                              check_if_alnum_iterator = function(iterator){
                                                if(l$s$err$get_assertions_status()){
                                                  inherited <- if(inherits(iterator, "Abstract_Iterator_Decorator")) iterator$get_decoration_hierarchy() else class(iterator)
                                                  is_num <- l$s$dts$contains(inherited, "Abstract_Numeric_Iterator", recursive = TRUE) ||
                                                    l$s$dts$contains(inherited, "Numeric_Offset_Normalized_Concatenated_Iterator", recursive = TRUE)
                                                  is_char <- l$s$dts$contains(inherited, "Character_Queue_Iterator", recursive = TRUE)
                                                  l$s$err$assert_msg(paste0("'iterator' must be a (possibly decorateded) iterator that inherits ",
                                                                            "from 'Abstract_Numeric_Iterator', 'Numeric_Offset_Normalized_Concatenated_Iterator' 
                                                                            or 'Character_Queue_Iterator'."),
                                                                     is.R6(iterator),
                                                                     xor(is_num, is_char))
                                                }
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
                                              
                                              #' A subclass of `Iterator_Factory` from `utils_iterators.R`
                                              #' adding assertions that objects created in `create_object(...)` are
                                              #' (possibly decorated) instances of `Abstract_Numeric_Iterator`, 
                                              #' `Numeric_Offset_Normalized_Concatenated_Iterator` 
                                              #' or `Character_Queue_Iterator`.
                                              #' 
                                              #' See documentation of `Iterator_Factory` for more information.
                                              #'   
                                              #' @export
                                              #'
                                              #' @examples
                                              #' @md
                                              initialize = function(assertions_status = FALSE,
                                                                    json_str = NULL, 
                                                                    json_file = NULL,
                                                                    json_obj = NULL){
                                                super$initialize(assertions_status = assertions_status,
                                                                 json_str = json_str,
                                                                 json_file = json_file,
                                                                 json_obj = json_obj)
                                                local_env$.add_to_static_env(super$get_static_env())
                                                local_env$static_env$err$set_assertions_status(assertions_status)
                                              },
                                              
                                              
                                              #' See documentation of `super$create_object()`.
                                              #'
                                              #' @examples
                                              #' @md
                                              create_object = function(object_type, ...){
                                                iterator <- super$create_object(object_type = object_type, ...)
                                                private$check_if_alnum_iterator(iterator)
                                                return(iterator)
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
                                                
                                                #'Add your finalization code here:
                                              }
                                              
                                            ),
                                            #-----end of public-----
                                            
                                            #' ----- Add active bindings ----
                                            active = list(
                                              
                                            )
                                            #-----end of active-----
        )
        
        #' TODO: Rename 'non_special_event_names' to 'numeric_sequence_names' and make sure the respective iterators are actually numeric
        #' TODO: 'character_sequence_names' instead of 'character_sequence_name'
        #' A subclass of `Alphanumeric_Iterator_Factory` from `utils_iterators.R`
        #' checking and asserting presence of certain `Synthetic_Datastream_Iterators` related fields
        #' and providing get methods for the values of said fields.
        Synthetic_Datastream_Iterator_Factory <- R6Class("Synthetic_Datastream_Iterator_Factory",
                                                 # ---- R6 inheritance -----
                                                 #'  __Optional__: Change to inherit from a different parent class.
                                                 #'                     __IMPORTANT__: The parent class MUST either directly or indirectly
                                                 #'                                    inherit from `R6_Base`!
                                                 inherit = l$result_env$Alphanumeric_Iterator_Factory,
                                                 
                                                 #' ---- Adapt R6 options --------
                                                 portable = TRUE,
                                                 cloneable = TRUE,
                                                 lock_class = TRUE,
                                                 lock_objects = TRUE,
                                                 
                                                 #' ----- Add private Fields & methods ----
                                                 private = list(
                                                   non_special_event_names = NULL,
                                                   req_params_non_special_event_names = NULL,
                                                   
                                                   padding_NA_rnd_walk_name = NULL,
                                                   NA_rnd_walk_name = NULL,
                                                   intra_NA_rnd_walk_name = NULL,
                                                   inter_NA_rnd_walk_name = NULL,
                                                   NA_rnd_walk_names = NULL,
                                                   
                                                   character_sequence_name = NULL,
                                                   padding_NA_character_sequence_name = NULL,
                                                   NA_character_sequence_name = NULL,
                                                   intra_NA_character_sequence_name = NULL,
                                                   inter_NA_character_sequence_name = NULL,
                                                   NA_character_sequence_names = NULL,
                                                   req_params_character_sequence = NULL
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
                                                   
                                                   #' A subclass of `Alphanumeric_Iterator_Factory` from `utils_iterators.R`
                                                   #' checking and asserting presence of certain `Synthetic_Datastream_Iterators` related fields
                                                   #' and providing get methods for the values of said fields.
                                                   #' 
                                                   #' TODO: Document which fields these are.
                                                   #'   
                                                   #' @export
                                                   #'
                                                   #' @examples
                                                   #' @md
                                                   initialize = function(assertions_status = FALSE,
                                                                         json_str = NULL, 
                                                                         json_file = NULL,
                                                                         json_obj = NULL){
                                                     super$initialize(assertions_status = assertions_status,
                                                                      json_str = json_str,
                                                                      json_file = json_file,
                                                                      json_obj = json_obj)
                                                     local_env$.add_to_static_env(super$get_static_env())
                                                     local_env$static_env$err$set_assertions_status(assertions_status)
                                                     
                                                     seq_names <- self$get_object_names()
                                                     json_special_names <- c(l$json_is_NA_rnd_walk_name,
                                                                             l$json_is_padding_NA_rnd_walk_name,
                                                                             l$json_is_intra_NA_rnd_walk_name,
                                                                             l$json_is_inter_NA_rnd_walk_name,
                                                                             l$json_is_character_sequence_name,
                                                                             l$json_is_NA_character_sequence_name,
                                                                             l$json_is_padding_NA_character_sequence_name,
                                                                             l$json_is_intra_NA_character_sequence_name,
                                                                             l$json_is_inter_NA_character_sequence_name)
                                                     
                                                     private$non_special_event_names <- as.list(seq_names)
                                                     for(json_name in json_special_names){
                                                       var_name <- paste0(sub("is_", "", json_name), "_name")
                                                       l$s$err$assert_msg(paste0("Field of name '", var_name, "' does not exist."),
                                                                          exists(var_name, envir = private, inherits = FALSE))
                                                       
                                                       if(l$s$err$get_assertions_status()){
                                                         other_special_names <-  setdiff(json_special_names, json_name)
                                                       }
                                                       
                                                       for(seq_name in seq_names){
                                                         if(json_name %in% names(self$get_attributes(seq_name))
                                                            && isTRUE(self$get_attributes(seq_name)[[json_name]])){
                                                           
                                                           var_value <- get(var_name, envir = private)
                                                           l$s$err$assert_msg(paste0("Multiple sequences with '", json_name, "' attribute being 'TRUE'."),
                                                                              is.null(var_value))
                                                           
                                                           assign(var_name, seq_name, envir = private)
                                                           private$non_special_event_names[private$non_special_event_names == seq_name] <- NULL
                                                           
                                                           if(l$s$err$get_assertions_status()){
                                                             for(other_json_name in other_special_names){
                                                               l$s$err$assert_msg(paste0("Sequences with '", json_name, "' attribute being 'TRUE' should not have attributte '",
                                                                                         other_json_name, "' set to 'TRUE'"),
                                                                                  l$s$mg$`%then%`(other_json_name %in% names(self$get_attributes(seq_name)),
                                                                                                  !isTRUE(self$get_attributes(seq_name)[[other_json_name]])))
                                                             }
                                                           }
                                                           else{
                                                             break
                                                           }
                                                         }
                                                       }
                                                       l$s$err$assert_msg(paste0("Attribute '", json_name, "' not found among sequences."),
                                                                          !is.null(get(var_name, envir = private)))
                                                     }
                                                     
                                                     if(l$s$err$get_assertions_status()){
                                                       forbidden_non_NA_sequence_labels <-
                                                         c(private$NA_label,
                                                           private$inter_NA_char_label,
                                                           private$intra_NA_char_label,
                                                           private$inter_NA_rnd_walk_label,
                                                           private$intra_NA_rnd_walk_label)
                                                       l$s$err$assert_msg(paste0("No non-NA sequence should have a ground truth label that equals one of the NA ",
                                                                                 "labels (as their ground truth labels are set to their sequence names, ",
                                                                                 "this would create ambiguities to \"actual\" NA labels."),
                                                                          length(setdiff(private$non_special_event_names, forbidden_non_NA_sequence_labels)) ==
                                                                            length(private$non_special_event_names))
                                                     }
                                                     
                                                     req_params_non_special_event_names <- c(l$required_param_assertions_status)
                                                     
                                                     req_params_NA_rnd_walk <- c(req_params_non_special_event_names, 
                                                                                 l$required_param_fixed_length, 
                                                                                 l$required_param_initial_value)
                                                     req_params_intra_NA_rnd_walk <- setdiff(req_params_NA_rnd_walk, 
                                                                                             l$required_param_fixed_length)
                                                     req_params_inter_NA_rnd_walk <- req_params_intra_NA_rnd_walk
                                                     req_params_padding_NA_rnd_walk <- req_params_intra_NA_rnd_walk
                                                     
                                                     req_params_character_sequence <- c(req_params_non_special_event_names, 
                                                                                        l$required_param_element)
                                                     req_params_NA_character_sequence <- c(req_params_non_special_event_names, 
                                                                                           l$required_param_fixed_length)
                                                     req_params_intra_NA_character_sequence <- setdiff(req_params_NA_character_sequence, 
                                                                                                       l$required_param_fixed_length)
                                                     req_params_inter_NA_character_sequence <- req_params_intra_NA_character_sequence
                                                     req_params_padding_NA_character_sequence <- req_params_intra_NA_character_sequence
                                                     
                                                     if(l$s$err$get_assertions_status()){
                                                       
                                                       # Check for consistency between required parameters from this implementation and
                                                       # the ones stated in the json config
                                                       check_req_params <- function(object_name, params){
                                                         req_params <- self$get_required_params(object_name)
                                                         illegal_params <- setdiff(params, req_params)
                                                         missing_params <- setdiff(req_params, params)
                                                         l$s$err$assert_msg(paste0("Missing required parameters for object type '", object_name, "':",
                                                                                   toString(missing_params)),
                                                                            length(missing_params) == 0)
                                                         l$s$err$assert_msg(paste0("Unknown required parameters for object type '", object_name, "':",
                                                                                   toString(illegal_params)),
                                                                            length(illegal_params) == 0)
                                                       }
                                                       
                                                       for(non_na_name in private$non_special_event_names){
                                                         check_req_params(non_na_name, req_params_non_special_event_names)
                                                       }
                                                       check_req_params(private$NA_rnd_walk_name, req_params_NA_rnd_walk)
                                                       check_req_params(private$intra_NA_rnd_walk_name, req_params_intra_NA_rnd_walk)
                                                       check_req_params(private$inter_NA_rnd_walk_name, req_params_inter_NA_rnd_walk)
                                                       check_req_params(private$padding_NA_rnd_walk_name, req_params_padding_NA_rnd_walk)
                                                       
                                                       
                                                       check_req_params(private$character_sequence_name, req_params_character_sequence)
                                                       check_req_params(private$NA_character_sequence_name, req_params_NA_character_sequence)
                                                       check_req_params(private$intra_NA_character_sequence_name, req_params_intra_NA_character_sequence)
                                                       check_req_params(private$inter_NA_character_sequence_name, req_params_inter_NA_character_sequence)
                                                       check_req_params(private$padding_NA_character_sequence_name, req_params_padding_NA_character_sequence)
                                                     }
                                                     
                                                     private$req_params_non_special_event_names <- req_params_non_special_event_names
                                                     
                                                     private$req_params_character_sequence <- req_params_character_sequence
                                                     
                                                     private$NA_rnd_walk_names <- c(private$NA_rnd_walk_name,
                                                                                    private$intra_NA_rnd_walk_name,
                                                                                    private$inter_NA_rnd_walk_name,
                                                                                    private$padding_NA_rnd_walk_name)
                                                     private$NA_character_sequence_names <- c(private$NA_character_sequence_name,
                                                                                              private$intra_NA_character_sequence_name,
                                                                                              private$inter_NA_character_sequence_name,
                                                                                              private$padding_NA_character_sequence_name)
                                                   },
                                                   
                                                   #' TODO: Document getters
                                                   
                                                   get_non_special_event_names = function(){
                                                     l$s$err$assert_msg("Unexpected NULL field.",
                                                                        !is.null(private$non_special_event_names))
                                                     return(private$non_special_event_names)
                                                   },
                                                   
                                                   get_req_params_non_special_event_names = function(){
                                                     l$s$err$assert_msg("Unexpected NULL field.",
                                                                        !is.null(private$req_params_non_special_event_names))
                                                     return(private$req_params_non_special_event_names)
                                                   },
                                                   
                                                   get_padding_NA_rnd_walk_name = function() {
                                                     l$s$err$assert_msg("Unexpected NULL field.",
                                                                        !is.null(private$padding_NA_rnd_walk_name))
                                                     return(private$padding_NA_rnd_walk_name)
                                                     
                                                   },
                                                   
                                                   get_NA_rnd_walk_name = function(){
                                                     l$s$err$assert_msg("Unexpected NULL field.",
                                                                        !is.null(private$NA_rnd_walk_name))
                                                     return(private$NA_rnd_walk_name)
                                                   },
                                                   
                                                   get_intra_NA_rnd_walk_name = function(){
                                                     l$s$err$assert_msg("Unexpected NULL field.",
                                                                        !is.null(private$intra_NA_rnd_walk_name))
                                                     return(private$intra_NA_rnd_walk_name)
                                                   },
                                                   
                                                   get_inter_NA_rnd_walk_name = function(){
                                                     l$s$err$assert_msg("Unexpected NULL field.",
                                                                        !is.null(private$inter_NA_rnd_walk_name))
                                                     return(private$inter_NA_rnd_walk_name)
                                                   },
                                                   
                                                   get_NA_rnd_walk_names = function(){
                                                     l$s$err$assert_msg("Unexpected NULL field.",
                                                                        !is.null(private$NA_rnd_walk_names))
                                                     return(private$NA_rnd_walk_names)
                                                   },
                                                   
                                                   get_character_sequence_name = function(){
                                                     l$s$err$assert_msg("Unexpected NULL field.",
                                                                        !is.null(private$character_sequence_name))
                                                     return(private$character_sequence_name)
                                                   },
                                                   
                                                   get_padding_NA_character_sequence_name = function() {
                                                     l$s$err$assert_msg("Unexpected NULL field.",
                                                                        !is.null(private$padding_NA_character_sequence_name))
                                                     return(private$padding_NA_character_sequence_name)
                                                     
                                                   },
                                                   
                                                   get_NA_character_sequence_name = function(){
                                                     l$s$err$assert_msg("Unexpected NULL field.",
                                                                        !is.null(private$NA_character_sequence_name))
                                                     return(private$NA_character_sequence_name)
                                                   },
                                                   
                                                   get_intra_NA_character_sequence_name = function(){
                                                     l$s$err$assert_msg("Unexpected NULL field.",
                                                                        !is.null(private$intra_NA_character_sequence_name))
                                                     return(private$intra_NA_character_sequence_name)
                                                   },
                                                   
                                                   get_inter_NA_character_sequence_name = function(){
                                                     l$s$err$assert_msg("Unexpected NULL field.",
                                                                        !is.null(private$inter_NA_character_sequence_name))
                                                     return(private$inter_NA_character_sequence_name)
                                                   },
                                                   
                                                   get_NA_character_sequence_names = function(){
                                                     l$s$err$assert_msg("Unexpected NULL field.",
                                                                        !is.null(private$NA_character_sequence_names))
                                                     return(private$NA_character_sequence_names)
                                                   },
                                                   
                                                   get_req_params_character_sequence = function(){
                                                     l$s$err$assert_msg("Unexpected NULL field.",
                                                                        !is.null(private$req_params_character_sequence))
                                                     return(private$req_params_character_sequence)
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
                                                     
                                                     #'Add your finalization code here:
                                                   }
                                                   
                                                 ),
                                                 #-----end of public-----
                                                 
                                                 #' ----- Add active bindings ----
                                                 active = list(
                                                   
                                                 )
                                                 #-----end of active-----
        )
        
        #' A container class for a unified access to multiple alphanumeric
        #' `Abstract_Iterator` instances.
        Alphanumeric_Iterators <- R6Class("Alphanumeric_Iterators",
                             # ---- R6 inheritance -----
                             inherit = l$s$it$Iterators,
                             
                             #' ---- Adapt R6 options --------
                             portable = TRUE,
                             cloneable = TRUE,
                             lock_class = TRUE,
                             lock_objects = TRUE,
                             
                             #' ----- Add private Fields & methods ----
                             private = list(
                               numeric_it_indices = NULL,
                               character_it_indices = NULL
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
                               #' @param iterators A data structure of `length > 0` containing:
                               #' 
                               #'   * __Numeric iterators__: Must either directly inherit from `Abstract_Numeric_Iterator` or
                               #'     `Numeric_Offset_Normalized_Concatenated_Iterator` or, in case they are decorated
                               #'     (i.e., inherit from `Abstract_Iterator_Decorator`), their `get_decoration_hierarchy()`
                               #'     must contain either `Abstract_Numeric_Iterator` or
                               #'     `Numeric_Offset_Normalized_Concatenated_Iterator`.
                               #'   * __Character iterators__: Must either directly inherit from `Character_Queue_Iterator` or, 
                               #'     in case they are decorated
                               #'     (i.e., inherit from `Abstract_Iterator_Decorator`), their `get_decoration_hierarchy()`
                               #'     must contain either `Character_Queue_Iterator`.
                               #'   
                               #'   Iterators are stored in the order they appear in
                               #'   this data structure. And the indices of the numeric ones may be accessed via 
                               #'   `get_numeric_indices()` and the indices of the character iterators via
                               #'   `get_character_indices()`.
                               #' 
                               #' @export
                               #'
                               #' @examples
                               #' @md
                               initialize = function(assertions_status = FALSE,
                                                     iterators){
                                 super$initialize(assertions_status = assertions_status,
                                                  iterators = iterators)
                                 local_env$.add_to_static_env(super$get_static_env())
                                 local_env$static_env$err$set_assertions_status(assertions_status)
                                 
                                 #' Add your initialization code here here:
                                
                                 private$numeric_it_indices <- list()
                                 private$character_it_indices <- list()
                                 
                                 for(i in 1:self$get_num_iterators()){
                                   it <- self$.get_iterator(i)
                                   inherited <- if(inherits(it, "Abstract_Iterator_Decorator")) it$get_decoration_hierarchy() else class(it)
                                   is_num <- l$s$dts$contains(inherited, "Abstract_Numeric_Iterator", recursive = TRUE) ||
                                     l$s$dts$contains(inherited, "Numeric_Offset_Normalized_Concatenated_Iterator", recursive = TRUE)
                                   is_char <- l$s$dts$contains(inherited, "Character_Queue_Iterator", recursive = TRUE) ||
                                     l$s$dts$contains(inherited, "Character_Concatenated_Iterator", recursive = TRUE)
                                   
                                   l$s$err$assert_msg(paste0("'iterators' must only contain (possibly decorated) elements that inherit ",
                                                             "from 'Abstract_Numeric_Iterator', 'Numeric_Offset_Normalized_Concatenated_Iterator', ",
                                                             "'Character_Queue_Iterator' or 'Character_Concatenated_Iterator'."),
                                                      is.R6(it),
                                                      xor(is_num, is_char))
                                   
                                   if(is_num){
                                     private$numeric_it_indices <- c(private$numeric_it_indices, list(i))
                                   }
                                   else{
                                     private$character_it_indices <- c(private$character_it_indices, list(i))
                                   }
                                 }
                                 
                                 private$numeric_it_indices <- unlist(private$numeric_it_indices)
                                 private$character_it_indices <- unlist(private$character_it_indices)
                                 
                                 l$s$err$assert_msg("Indices should not overlap.",
                                                    length(intersect(private$numeric_it_indices,
                                                                     private$character_it_indices)) == 0)
                               },
                               
                               finalize = function() {
                                 super$finalize()
                                 
                                 #' Add your finalization code here:
                               },
                              
                               #' @return A vector holding the indices assigned to the numeric iterators this 
                               #'   object contains. See documentation of `initialize()` for more
                               #'   information on what is considered to be a numeric iterator.
                               #' @export
                               #'
                               #' @examples
                               #' @md
                               get_numeric_indices = function(){
                                 return(private$numeric_it_indices)
                               },
                               
                               #' @return A vector holding the indices assigned to the character iterators this 
                               #'   object contains. See documentation of `initialize()` for more
                               #'   information on what is considered to be a character iterator.
                               #' @export
                               #'
                               #' @examples
                               #' @md
                               get_character_indices = function() {
                                 return(private$character_it_indices)
                               },
                               
                               #' @return The number of internal numerical iterators.
                               #' @export
                               #'
                               #' @examples
                               #' @md
                               get_num_numeric_iterators = function() {
                                 return(length(private$numeric_it_indices))
                               },
                               
                               #' @return The number of internal character iterators.
                               #' @export
                               #'
                               #' @examples
                               #' @md
                               get_num_character_iterators = function() {
                                 return(length(private$character_it_indices))
                               }
                             ),
                             #-----end of public-----
                             
                             #' ----- Add active bindings ----
                             active = list(
                               
                             )
                             #-----end of active-----
        )
        
        #' Helper class representing metadata of a pending subsequence insertion
        #' into an iterator from `Synthetic_Datastream_Iterators`.
        .Insertion_Event <- R6Class(".Insertion_Event",
                                    # ---- R6 inheritance -----
                                    inherit = R6_Base,
                                    
                                    #'  ---- Adapt R6 options --------
                                    portable = TRUE,
                                    cloneable = TRUE,
                                    lock_class = TRUE,
                                    lock_objects = TRUE,
                                    
                                    #' ----- Add private Fields & methods ----
                                    private = list(
                                      iterator_id = NULL,
                                      sequence_name = NULL,
                                      sequence_params = NULL,
                                      append_at = NULL,
                                      start_timestamp = NULL,
                                      last_emitted_timestamp = NULL,
                                      end_timestamp = NULL,
                                      event_dependencies = NULL,
                                      iterator_created = NULL,
                                      earliest_start_timestamp = NULL,
                                      earliest_start_timestamp_temp = NULL,
                                      is_invisible = NULL,
                                      inserted_unfinished_events_sublist_index = NULL,
                                      unique_id = NULL
                                    ),
                                    #-----end of private-----
                                    
                                    public = list(
                                      
                                      get_static_env = function(){
                                        local_env$static_env
                                      },
                                      
                                      
                                      #' @param assertions_status If `TRUE`, assertions are activated (see `set_assertion_status()`
                                      #'   in `utils_lang_error.R`).
                                      #'
                                      #' @param iterator_id An integer >= 1 representing the index of a 
                                      #'   `Synthetic_Datastream_Iterators`
                                      #'   instance's internal iterator into which
                                      #'   the sequence is to be inserted.
                                      #' @param sequence_name A character vector of length one representing
                                      #'   the subsequence's name (i.e., the name to be used for the `object_type` parameter
                                      #'   to be used in the associated`Synthetic_Datastream_Iterators` instance's factory's
                                      #'   `create_object()` method.
                                      #' @param sequence_params A list containing the required parameters to be provided to the
                                      #'   ellipsis of the associated`Synthetic_Datastream_Iterators` instance's factory's
                                      #'   `create_object()` method when creating an iterator of type `sequence_name`.
                                      #' @param append_at Either a character vector of length one
                                      #'     or of the same length as `event_dependencies`.
                                      #'     The former case is treated equivalent to the latter case
                                      #'     having identical entries of value `append_at`. Each entry corresponds to a temporal insertion
                                      #'     condition and all of them must be fulfilled when this insertion event's
                                      #'     iterator is created and inserted.
                                      #' @param .unique_id Either `NULL` or a non-negative integer of length 1. May be used for
                                      #'   debug purposes by assigning each insertion event a unique id.
                                      #'     
                                      #'     The `i`-th entry corresponds to the `i`-th entry in `event_dependencies`and
                                      #'     must have one of the following values:
                                      #'   * `"end"`: Append this subsequence so that its first timestamp `t` is `t >=` the
                                      #'     end timestamp of the `i`-th `.Insertion_Event` provided via `event_dependencies`
                                      #'     (accessible via its `get_end_timestamp()` method).
                                      #'     Note that a `NULL` ending timstamp means the event's corresponding iterator
                                      #'     has not finished yet and thus `NULL` is treated as being `NULL > t`.
                                      #'     
                                      #'   * `"begin"`: Append this subsequence so that its first timestamp `t` is `t >=` the
                                      #'     start timestamp of the `i`-th `.Insertion_Event` provided via `event_dependencies`
                                      #'     (accessible via its `get_start_timestamp()` method).
                                      #'     Note that a `NULL` starting timestamp means the event's corresponding iterator
                                      #'     has not started yet and thus `NULL` is treated as being `NULL > t`.
                                      #'     
                                      #'     
                                      #' @param event_dependencies A set of `.Insertion_Event`s representing events that have to be
                                      #'    inserted before this one. See `append_at` and `immediate_insertion_possible()` for more
                                      #'    details.
                                      #' @param is_invisible If `TRUE`, this insertion event is not allowed to be present in
                                      #'   other insertion event's `event_dependenies`.
                                      #' 
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      initialize = function(assertions_status = FALSE,
                                                            iterator_id,
                                                            sequence_name,
                                                            sequence_params,
                                                            append_at,
                                                            event_dependencies,
                                                            is_invisible,
                                                            .unique_id = (function(){
                                                              if(assertions_status){
                                                                result <- get(".unique_event_id", envir = l$.unique_event_env)
                                                                assign(".unique_event_id", value = result + 1, envir = l$.unique_event_env)
                                                              }
                                                              else{
                                                                result <- NULL
                                                              }
                                                              return(result)
                                                            })()){
                                        super$initialize()
                                        local_env$.add_to_static_env(super$get_static_env())
                                        local_env$static_env$err$set_assertions_status(assertions_status)
                                       
                                        append_at <- trimws(append_at)
                                        sequence_name <- trimws(sequence_name)
                                        
                                        sequence_params <- as.list(sequence_params)
                                        
                                        event_dependencies <- if(is.R6(event_dependencies)) list(event_dependencies) else as.list(event_dependencies)
                                        
                                        len_ed <- length(event_dependencies)
                                        hl1 <- l$s$tcs$has_length_1(append_at, NA_on_fail = FALSE)
                                        if(hl1){
                                          if(len_ed == 0){
                                            append_at <- list()
                                          }
                                          else{
                                            append_at <- lapply(1:len_ed, FUN = function(i){
                                              return(append_at)
                                            })
                                          }
                                        }
                                        else{
                                          append_at <- as.list(append_at)
                                        }
                                        
                                        if(l$s$err$get_assertions_status()){
                                          append_at_values <- c("end", "begin")
                                          
                                          l$s$err$assert_msg("'sequence_name' may not be empty after trimming.",
                                                             sequence_name != "")
                                          
                                          l$s$err$assert_msg(paste0("'iterator_id', 'sequence_name' and 'is_invisible'",
                                                                    "must have a length of one."),
                                                             l$s$tcs$has_length_1(iterator_id, NA_on_fail = FALSE),
                                                             l$s$tcs$has_length_1(sequence_name, NA_on_fail = FALSE),
                                                             l$s$tcs$has_length_1(is_invisible, NA_on_fail = FALSE))
                                          
                                          l$s$err$assert_msg(paste0("'iterator_id' must be an integer >= 1."),
                                                             l$s$tcs$is_integer(iterator_id,
                                                                                accept_NULL = FALSE,
                                                                                accept_NaN = FALSE,
                                                                                accept_NA = FALSE,
                                                                                lower_bound = 1,
                                                                                lower_bound_inclusive = TRUE,
                                                                                upper_bound = Inf,
                                                                                upper_bound_inclusive = FALSE))
                                          
                                          l$s$err$assert_msg(paste0("'.unique_id' must be NULL or an integer >= 0."),
                                                             l$s$mg$`%then%`(!is.null(.unique_id), 
                                                                             l$s$tcs$has_length_1(.unique_id, NA_on_fail = FALSE) &&
                                                                               l$s$tcs$is_integer(.unique_id,
                                                                                                  accept_NULL = FALSE,
                                                                                                  accept_NaN = FALSE,
                                                                                                  accept_NA = FALSE,
                                                                                                  lower_bound = 0,
                                                                                                  lower_bound_inclusive = TRUE,
                                                                                                  upper_bound = Inf,
                                                                                                  upper_bound_inclusive = FALSE)))
                                          
                                          l$s$err$assert_msg(paste0("'sequence_name' must be a character vector."),
                                                             l$s$tcs$is_character(sequence_name,
                                                                                  accept_NULL = FALSE,
                                                                                  accept_NaN = FALSE,
                                                                                  accept_NA = FALSE))
                                          
                                          l$s$err$assert_msg(paste0("At this point, 'append_at' must be a character vector ",
                                                                    "of the same length as 'event_dependencies'."),
                                                             l$s$tcs$has_length(append_at, len = len_ed, NA_on_fail = FALSE),
                                                             all(l$s$tcs$is_character(append_at,
                                                                                      accept_NULL = FALSE,
                                                                                      accept_NaN = FALSE,
                                                                                  accept_NA = FALSE)))
                                          
                                          l$s$err$assert_msg(paste0("'append_at' must only contain values from 'append_at_values'."),
                                                             l$s$tcs$has_length_0(setdiff(append_at, append_at_values), NA_on_fail = FALSE))
                                          
                                          l$s$err$assert_msg("'is_invisible' must be a logical.",
                                                             l$s$tcs$is_logical(is_invisible,
                                                                                accept_NULL = FALSE,
                                                                                accept_NaN = FALSE,
                                                                                accept_NA = FALSE))
                                          
                                          for(event in event_dependencies){
                                            l$s$err$assert_msg(paste0("'event_dependencies' may only contain instances of '.Insertion_Event'."),
                                                               is.R6(event),
                                                               inherits(event, ".Insertion_Event"))
                                            l$s$err$assert_msg(paste0("Invisible insertion event provided in 'event_dependencies'."),
                                                               !event$get_is_invisible())
                                          }
                                          
                                        }
                                        
                                        private$append_at <- append_at
                                        private$iterator_id <- as.integer(iterator_id)
                                        private$sequence_name <- as.character(sequence_name)
                                        private$sequence_params <- sequence_params
                                        private$event_dependencies <- event_dependencies
                                        private$iterator_created <- FALSE
                                        private$earliest_start_timestamp_temp <- 0
                                        private$is_invisible <- is_invisible
                                        private$unique_id <- .unique_id
                                      },
                                      
                                      #'
                                      #' @return The `.unique_id` parameter provided to the `initialize()` method.
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      .get_unique_id = function(){
                                        return(private$unique_id)
                                      },
                                      
                                      #' @return The `is_invisible` parameter previously provided to `initialize()`.
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      get_is_invisible = function(){
                                        return(private$is_invisible)
                                      },
                                     
                                      #' Should be used to hold the indiex to access this event inside the 
                                      #' associated `Synthetic_Datastream_Iterator`'s `inserted_unfinished_events[[get_iterator_id()]]`
                                      #' sublist. 
                                      #' A zero indicates that the event's associated subsequence has already been iterated over and
                                      #' is already removed from `inserted_unfinished_events[[get_iterator_id()]]`.
                                      #'
                                      #' @param sublist_index A finite integer >= 0 of length one.
                                      #'
                                      #' @return
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      set_inserted_unfinished_events_index = function(sublist_index){
                                        l$s$err$assert_msg("Illegal argument.",
                                                           l$s$tcs$has_length_1(sublist_index, NA_on_fail = FALSE),
                                                           l$s$tcs$is_integer(sublist_index,
                                                                              accept_NULL = FALSE,
                                                                              accept_NaN = FALSE,
                                                                              accept_NA = FALSE,
                                                                              lower_bound = 0,
                                                                              lower_bound_inclusive = TRUE,
                                                                              upper_bound = Inf,
                                                                              upper_bound_inclusive = FALSE))
                                        private$inserted_unfinished_events_sublist_index <- sublist_index
                                      },
                                      
                                      #'
                                      #' @return `NULL`, if `set_inserted_unfinished_events_index()` has not been called yet
                                      #'   otherwise an integer holding the
                                      #'  `sublist_index` parameter previously provided to the aforementioned method.
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      get_inserted_unfinished_events_index = function(){
                                        is_null_index <- is.null(private$inserted_unfinished_events_sublist_index)
                                        
                                        if(is_null_index){
                                          return(NULL)
                                        }
                                        else{
                                          return(private$inserted_unfinished_events_sublist_index)
                                        }
                                      },
                                      
                                      #' Note that this method removes those entries from `event_dependencies`
                                      #' and `append_at`, for which the conditions described below are fulfilled
                                      #' (the implementation guarantees that once such a condiditon is fulfilled, it
                                      #' will always be fulfilled).
                                      #' 
                                      #' @param timestamp A non-negative numeric of length one.
                                      #' 
                                      #' @return `TRUE` if and only if for each event `event_dependencies[[i]]` and the associated
                                      #'   `append_at[[i]]` value, the following holds:
                                      #'   * `append_at[[i]] == "begin"` and we have `event_dependencies[[i]]$get_start_timestamp()`
                                      #'     is not `NULL` and `<= timestamp` (i.e., the respective iterator's first element has been retrieved).
                                      #'   * __OR__: `append_at[[i]] == "end"` and we have `event_dependencies[[i]]$get_end_timestamp()`
                                      #'     is not `NULL` and `<= timestamp` (i.e., the respective iterator's last element has been retrieved).
                                      #'     
                                      #'  In other words: This method returns `TRUE` if and only if:
                                      #'   * `t := get_earliest_start_timestamp()` is not `NULL`
                                      #'   * __AND__ `t <= timestamp`.
                                      #'     
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      is_immediate_insertion_possible = function(timestamp){
                                        earliest_t <- self$get_earliest_start_timestamp()
                                        
                                        result <- !is.null(earliest_t) && !is.null(timestamp) && earliest_t <= timestamp
                                        
                                        return(result)
                                      },
                                      
                                     
                                      #' @return For `t_1, ..., t_n`, let `t_i` be set to 
                                      #'   `event_dependencies[[i]]$get_start_timestamp()` if `append_at[[i]]` equals `"begin"`
                                      #'   and to`event_dependencies[[i]]$get_end_timestamp()` if `append_at[[i]]` equals `"end"`.
                                      #'   This method returns `NULL` if at least one `t_i` is `NULL`.
                                      #'   Otherwise it returns the maximum over `t_1, ..., t_n`.
                                      #'   
                                      #'   This means that this method returns the earliest time at which this insertion event's iterator
                                      #'   is allowed to emit values (i.e., the timestamps of these values may not be smaller that this time).
                                      #'   
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      get_earliest_start_timestamp = function(){
                                        if(is.null(private$earliest_start_timestamp)){
                                          dependencies_empty <- TRUE
                                          earliest_start_timestamp <- private$earliest_start_timestamp_temp
                                          while(dependencies_empty && length(private$event_dependencies) > 0){
                                            event <- private$event_dependencies[[1]]
                                            
                                            if(private$append_at[[1]] == "begin"){
                                              t <- event$get_start_timestamp()
                                            }
                                            else if(private$append_at[[1]] == "end"){
                                              t <- event$get_end_timestamp()
                                            }
                                            else {
                                              l$s$err$assert_msg(paste0("'append_at[[1]]' has an unknown value of: \"", private$append_at[[1]], "\""),
                                                                FALSE)
                                            }
                                            
                                            dependencies_empty <- !is.null(t)
                                            
                                            if(dependencies_empty){
                                              earliest_start_timestamp <- max(earliest_start_timestamp, t)
                                              private$earliest_start_timestamp_temp <- earliest_start_timestamp
                                              
                                              private$event_dependencies[[1]] <- NULL
                                              private$append_at[[1]] <- NULL
                                            }
                                          }
                                          
                                          if(dependencies_empty){
                                            private$earliest_start_timestamp <- private$earliest_start_timestamp_temp
                                          }
                                        }
                                        
                                        return(private$earliest_start_timestamp)
                                      },
                                      
                                      
                                      #' @return The last value provided to `set_last_emitted_timestamp()` or `NULL` if the former
                                      #'   has not been called yet.
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      get_last_emitted_timestamp = function(){
                                        return(private$last_emitted_timestamp)
                                      },
                                      
                                      
                                      #' Sets the value to be returned by `get_last_emitted_timestamp()`. Should be called
                                      #' with the latest timestamp of this insertion event's iterator's `get_next()` result.
                                      #' 
                                      #' The value of the first call to this method is also the one that will be returned by `get_end_timestamp()`.
                                      #'
                                      #' May not be called after `set_end_timestamp()` has been called and not before `is_immediate_insertion_possible(timestamp)`
                                      #' returns `TRUE`.
                                      #'
                                      #' @param timestamp A finite numeric of length one, that is `>=` the value returned by `get_earlierst_start_time()`
                                      #'   and `get_last_emitted_timestamp()` if the latter is non-`NULL`.
                                      #'
                                      #' @return
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      set_last_emitted_timestamp = function(timestamp){
                                        last_emitted_timestamp <- self$get_last_emitted_timestamp()
                                        is_null_last <- is.null(last_emitted_timestamp)
                                        
                                        if(l$s$err$get_assertions_status()){
                                          earliest_start_time <- self$get_earliest_start_timestamp()
                                          insertion_possible <- self$is_immediate_insertion_possible(timestamp)
                                          l$s$err$assert_msg(paste0("The last emitted timestamp should only be set once ",
                                                                    "'is_immediate_insertion_possible(timestamp)' returns 'TRUE'."),
                                                             insertion_possible)
                                          
                                          l$s$err$assert_msg(paste0("'get_earliest_start_timestamp()' should return a non-NULL value at this point."),
                                                             !is.null(earliest_start_time))
                                         
                                          l$s$err$assert_msg(paste0("No new last emitted timestamp may be set once the end timestamp has."),
                                                                    is.null(self$get_end_timestamp()))
                                          
                                          l$s$err$assert_msg(paste0("'timestamp' must be a non-negative, finite numeric of length ",
                                                                    "one that is >=  'get_earliest_start_timestamp()' and >= the last emitted timestamp ",
                                                                    "if the latter is not NULL."),
                                                             l$s$tcs$has_length_1(timestamp, NA_on_fail = FALSE),
                                                             l$s$tcs$is_numeric(timestamp,
                                                                                accept_NULL = FALSE,
                                                                                accept_NaN = FALSE,
                                                                                accept_NA = FALSE,
                                                                                lower_bound = if(is_null_last) earliest_start_time else last_emitted_timestamp,
                                                                                lower_bound_inclusive = TRUE,
                                                                                upper_bound = Inf,
                                                                                upper_bound_inclusive = FALSE,
                                                                                accept_non_integer = TRUE))
                                          start_t <- self$get_start_timestamp()
                                          l$s$err$assert_msg(paste0("If last emitted timestamp is non-NULL, the start timestamp ", 
                                                                    "must be non-NULL and <= timestamp."),
                                                             l$s$mg$`%then%`(!is_null_last, !is.null(start_t)),
                                                             l$s$mg$`%then%`(!is_null_last, start_t <= timestamp))
                                        }
                                        
                                        if(is_null_last){
                                          private$start_timestamp <- timestamp
                                        }
                                        
                                        private$last_emitted_timestamp <- timestamp
                                        
                                      },
                                      
                                      
                                      #'
                                      #' @return The value provided during the first call to `set_last_emitted_timestamp()`
                                      #'   or `NULL` if the former has not been called yet.
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      get_start_timestamp = function(){
                                        return(private$start_timestamp)
                                      },
                                      
                                      #'
                                      #' @return If `set_end_timestamp()` has been called, the method returns the last timestamp 
                                      #'   provided to `set_last_emitted_timestamp()`. Otherwise, the method returns `NULL`.
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      get_end_timestamp = function(){
                                        return(private$end_timestamp)
                                      },
                                      
                                      #' May only be called once and only after at least one value has been provided to `set_last_emitted_timestamp()`.
                                      #' Afterwards, `get_end_timestamp()` will always return the last value provided to `set_last_emitted_timestamp()`
                                      #' and no further calls to the latter can be made.
                                      #' 
                                      #' @return
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      set_end_timestamp = function(){
                                        l$s$err$assert_msg(paste0("The ending time may be set only once ", 
                                                                  "and only after at least one last emitted timestamp has been set."),
                                                           is.null(self$get_end_timestamp()),
                                                           !is.null(self$get_last_emitted_timestamp()))
                                        
                                        private$end_timestamp <- self$get_last_emitted_timestamp()
                                      },
                                      
                                      #'
                                      #' Note that this method produces an error if assertions are active and `is_iterator_created()`
                                      #' is `TRUE`.
                                      #'
                                      #' @param factory An `Alphanumeric_Iterator_Factory` instance.
                                      #' @param ... Additional parameters.
                                      #'
                                      #' @return The iterator represented by this
                                      #'   `.Insertions_Event` instance. It is created via the factory's `create_object`
                                      #'   method with creation parameters aquired via `c(get_sequence_params(), list(...))`.
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      create_iterator = function(factory, ...){
                                        l$s$err$assert_msg("This instertion event's iterator may only be created once.",
                                                           !self$is_iterator_created())
                                        
                                        l$s$err$assert_msg("'factory' must inherit from 'Synthetic_Datastream_Iterator_Factory'.",
                                                           is.R6(factory),
                                                           inherits(factory, "Synthetic_Datastream_Iterator_Factory"))
                                        params <- c(list(object_type = self$get_sequence_name()),
                                                    self$get_sequence_params(),
                                                    list(...))
                                        
                                        iterator <- do.call(what = factory$create_object,
                                                      args = params)
                                        
                                        inherited <- if(inherits(iterator, "Abstract_Iterator_Decorator")) iterator$get_decoration_hierarchy() else class(iterator)
                                        is_num_concat <- l$s$dts$contains(inherited, "Numeric_Offset_Normalized_Concatenated_Iterator", recursive = TRUE)
                                        is_char_concat <- l$s$dts$contains(inherited, "Character_Concatenated_Iterator", recursive = TRUE)
                                        
                                        if(is_num_concat || is_char_concat){
                                          iterator$lock()
                                        }
                                        
                                        if(l$s$err$get_assertions_status()){
                                          is_num <- l$s$dts$contains(inherited, "Abstract_Numeric_Iterator", recursive = TRUE) || is_num_concat
                                          
                                          is_char <- l$s$dts$contains(inherited, "Character_Queue_Iterator", recursive = TRUE)
                                          
                                          l$s$err$assert_msg(paste0("'iterator' must be a (possibly decorateded) iterator that inherits ",
                                                                    "from 'Abstract_Numeric_Iterator', 'Numeric_Offset_Normalized_Concatenated_Iterator', ", 
                                                                    "'Character_Queue_Iterator' or 'Character_Concatenated_Iterator'."),
                                                             is.R6(iterator),
                                                             xor(is_num, is_char))
                                          
                                          len <- iterator$get_length()
                                          l$s$err$assert_msg(paste0("'iterator' must have a non-NA, finite length that equals its 'get_num_remaining()' value."),
                                                             l$s$tcs$has_length_1(len, NA_on_fail = FALSE),
                                                             l$s$tcs$is_integer(len,
                                                                                accept_NULL = FALSE,
                                                                                accept_NaN = FALSE,
                                                                                accept_NA = FALSE,
                                                                                lower_bound = 0,
                                                                                lower_bound_inclusive = TRUE,
                                                                                upper_bound = Inf,
                                                                                upper_bound_inclusive = FALSE),
                                                             l$s$dts$equals(len, iterator$get_num_remaining()))
                                        }
                                        
                                        private$iterator_created <- TRUE
                                        
                                        return(iterator)
                                      },
                                      
                                      
                                      #'
                                      #' @return `TRUE` if and only if `create_iterator()` has been successfully called once.
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      is_iterator_created = function(){
                                        return(private$iterator_created)
                                      },
                                      
                                      #' @return The list-coerced `sequence_params` parameter previously provided to `initialize()`.
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      get_sequence_params = function(){
                                        return(private$sequence_params)
                                      },
                                      
                                      #' @return The trimmed, list-coerced `append_at` parameter previously provided to `initialize()`.
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      get_append_at = function(){
                                        return(private$append_at)
                                      },
                                      
                                      #' @return The `event_dependencies` parameter previously provided to `initialize()`.
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      get_event_dependencies = function(){
                                        return(private$event_dependencies)
                                      },
                                      
                                      #' @return The iterator id previously provided to `initialize()`.
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      get_iterator_id = function(){
                                        return(private$iterator_id)
                                      },
                                      
                                      #' @return The subsequence name previously provided to `initialize()`.
                                      #' @export
                                      #'
                                      #' @examples
                                      #' @md
                                      get_sequence_name = function(){
                                        return(private$sequence_name)
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
                                        return(paste0(tab,".Insertion_Event:\n",
                                                      tab,"  Event ID: ", self$.get_unique_id(),"\n",
                                                      tab,"  Sequence name: ", self$get_sequence_name(),"\n",
                                                      tab,"  Iterator id: ", self$get_iterator_id(), "\n",
                                                      tab,"  Invisible: ", self$get_is_invisible(),"\n",
                                                      tab,"  Inserted unfinished insertions index: ", self$get_inserted_unfinished_events_index(),"\n",
                                                      tab,"  Earliest start time: ", self$get_earliest_start_timestamp(),"\n",
                                                      tab,"  Start timestamp: ", self$get_start_timestamp(),"\n",
                                                      tab,"  Last emitted timestamp: ", self$get_last_emitted_timestamp(),"\n",
                                                      tab,"  End timestamp: ", self$get_end_timestamp(),"\n",
                                                      tab,"  Iterator created: ", self$is_iterator_created(),"\n",
                                                      tab,"  Append at: ", toString(self$get_append_at()),"\n",
                                                      tab,"  Dependency unique IDs: ", toString(
                                                        lapply(self$get_event_dependencies(),
                                                               FUN = function(event){
                                                                 return(event$.get_unique_id())
                                                               }))
                                                      ))
                                        
                                      },
                                      
                                      finalize = function() {
                                        super$finalize()
                                      }
                                      
                                    )
                                    #-----end of public-----
        )
        
        #' Helper class representing a queue returning the subsequence name of each element 
        #' returned by an internal iterator of a `Synthetic_Datastream_Iterators` instance.
        #' Used to annotate each element returned by such an internal iterator 
        #' with the name of the subsequence that element belongs to.
        .Annotation_Information <- R6Class(".Annotation_Information",
                                           # ---- R6 inheritance -----
                                           inherit = R6_Base,
                                           
                                           #'  ---- Adapt R6 options --------
                                           portable = TRUE,
                                           cloneable = TRUE,
                                           lock_class = TRUE,
                                           lock_objects = TRUE,
                                           
                                           #' ----- Add private Fields & methods ----
                                           private = list(
                                             sequence_names = NULL,
                                             nums_remaining = NULL,
                                             lengths = NULL
                                           ),
                                           #-----end of private-----
                                           
                                           public = list(
                                             
                                             get_static_env = function(){
                                               local_env$static_env
                                             },
                                             
                                             
                                             #' @param assertions_status If `TRUE`, assertions are activated (see `set_assertion_status()`
                                             #'   in `utils_lang_error.R`).
                                             #' 
                                             #' @export
                                             #'
                                             #' @examples
                                             #' @md
                                             initialize = function(assertions_status = FALSE){
                                               super$initialize()
                                               local_env$.add_to_static_env(super$get_static_env())
                                               local_env$static_env$err$set_assertions_status(assertions_status)
                                              
                                               private$sequence_names <- list()
                                               private$nums_remaining <- list()
                                               private$lengths <- list()
                                             },
                                             
                                             #' Appends `sequence_name` to the internal queue.
                                             #' `sequence_name` will be returned by `length` subsequent
                                             #' calls to `pop()`.
                                             #'
                                             #' @param sequence_name A character vector of length one representing
                                             #'   the subsequence's name. A trimmed version of this character is
                                             #'   stored internally. The latter may not be an empty string 
                                             #'   (i.e., "").
                                             #' @param length An integer of length one that is >= 1.
                                             #'
                                             #' @return
                                             #' @export
                                             #'
                                             #' @examples
                                             #' @md
                                             pushback = function(sequence_name, length){
                                               l$s$err$assert_msg(paste0("'sequence_name' must be a character vector of length one."),
                                                                  l$s$tcs$has_length_1(sequence_name, NA_on_fail = FALSE),
                                                                  l$s$tcs$is_character(sequence_name,
                                                                                       accept_NULL = FALSE,
                                                                                       accept_NaN = FALSE,
                                                                                       accept_NA = FALSE))
                                               sequence_name <- trimws(sequence_name)
                                               l$s$err$assert_msg("'sequence_name' may not be empty after trimming",
                                                                  sequence_name != "")
                                               
                                               l$s$err$assert_msg("'length' must be an integer >= 1 of length one.",
                                                                  l$s$tcs$has_length_1(length, NA_on_fail = FALSE),
                                                                  l$s$tcs$is_integer(length,
                                                                                     accept_NULL = FALSE,
                                                                                     accept_NaN = FALSE,
                                                                                     accept_NA = FALSE,
                                                                                     lower_bound = 1,
                                                                                     lower_bound_inclusive = TRUE,
                                                                                     upper_bound = Inf,
                                                                                     upper_bound_inclusive = FALSE))
                                               private$sequence_names <- append(private$sequence_names, sequence_name)
                                               private$nums_remaining <- append(private$nums_remaining, length)
                                               private$lengths <- append(private$lengths, length)
                                               l$s$err$assert_msg("'sequence_names', 'nums_remaining' and 'lengths' should have the same length.",
                                                                  length(private$sequence_names) == length(private$nums_remaining),
                                                                  length(private$sequence_names) == length(private$lengths))
                                             },
                                             
                                             #'
                                             #' @return A character vector of length one representing the 
                                             #'   next sequence name previously added via `pushback()`.
                                             #' @export
                                             #'
                                             #' @examples
                                             #' @md
                                             pop = function(){
                                               l$s$err$assert_msg(paste0("'sequence_names', 'nums_remaining' and 'lengths' must have ",
                                                                     "the same length >= 1."),
                                                              length(private$sequence_names) == length(private$nums_remaining),
                                                              length(private$sequence_names) == length(private$lengths),
                                                              length(private$sequence_names) >= 1)
                                               
                                               l$s$err$assert_msg("'nums_remaining[[1]]' should be >= 1.",
                                                                  private$nums_remaining[[1]] >= 1)
                                               
                                               result <- private$sequence_names[[1]]
                                               if(private$nums_remaining[[1]] == 1){
                                                 private$nums_remaining[[1]] <- NULL
                                                 private$sequence_names[[1]] <- NULL
                                                 private$lengths[[1]] <- NULL
                                               }
                                               else{
                                                 private$nums_remaining[[1]] <- private$nums_remaining[[1]] - 1
                                               }
                                               
                                               l$s$err$assert_msg("'sequence_names' and 'nums_remaining' should have the same length.",
                                                                  length(private$sequence_names) == length(private$nums_remaining))
                                               return(result)
                                             },
                                             
                                             pop_silent = function(n = 1){
                                               l$s$err$assert_msg("'n' must a non-negative, finite integer of length one and <= sum(nums_remaining).",
                                                                  l$s$tcs$has_length_1(n, NA_on_fail = FALSE),
                                                                  l$s$tcs$is_integer(n,
                                                                                     accept_NULL = FALSE,
                                                                                     accept_NaN = FALSE,
                                                                                     accept_NA = FALSE,
                                                                                     lower_bound = 0,
                                                                                     lower_bound_inclusive = TRUE,
                                                                                     upper_bound = sum(unlist(private$nums_remaining)),
                                                                                     upper_bound_inclusive = TRUE))
                                               
                                               if(n == 0){
                                                 return()
                                               }
                                               
                                               remaining <- n
                                               
                                               while(remaining > 0){
                                                 l$s$err$assert_msg(paste0("'sequence_names', 'nums_remaining' and 'lengths' must have ",
                                                                           "the same length >= 1."),
                                                                    length(private$sequence_names) == length(private$nums_remaining),
                                                                    length(private$sequence_names) == length(private$lengths),
                                                                    length(private$sequence_names) >= 1)
                                                 l$s$err$assert_msg("'nums_remaining[[1]]' should be >= 1.",
                                                                    private$nums_remaining[[1]] >= 1)
                                                 
                                                 next_num <- min(remaining, private$nums_remaining[[1]])
                                                 remaining <- remaining - next_num
                                                 private$nums_remaining[[1]] <- private$nums_remaining[[1]] - next_num
                                                 l$s$err$assert_msg("'nums_remaining[[1]]' should be >= 0.",
                                                                    private$nums_remaining[[1]] >= 0)
                                                 if(private$nums_remaining[[1]] == 0){
                                                   private$nums_remaining[[1]] <- NULL
                                                   private$sequence_names[[1]] <- NULL
                                                   private$lengths[[1]] <- NULL
                                                 }
                                               }
                                             },
                                             
                                             #'
                                             #' @return A character vector representing the
                                             #'   sequence names previously added via `puchback()` -
                                             #'   excluding those that have already been completely retrieved via `pop()`.
                                             #' @export
                                             #'
                                             #' @examples
                                             #' @md
                                             get_sequence_names = function(){
                                               return(private$sequence_names)
                                             },
                                             
                                             #'
                                             #' @return A numeric vector of the same length as the one
                                             #'   returned by `get_sequence_names()`. The former's `i`-th entry corresponds to the
                                             #'   `i`-th entry of the latter and holds the remaining number of `pop()`
                                             #'   calls returning each of those names.
                                             #' @export
                                             #'
                                             #' @examples
                                             #' @md
                                             get_nums_remaining = function(){
                                               return(private$nums_remaining)
                                             },
                                             
                                             #'
                                             #' @return A numeric vector of the same length as the one
                                             #'   returned by `get_sequence_names()`. The former's `i`-th entry corresponds to the
                                             #'   `i`-th entry of the latter and holds the original length provided
                                             #'   to `pushback()` for that name.
                                             #' @export
                                             #'
                                             #' @examples
                                             #' @md
                                             get_lengths = function(){
                                               return(private$lengths)
                                             },
                                             
                                             finalize = function() {
                                               super$finalize()
                                             }
                                             
                                           )
                                           #-----end of public-----
        )
        
        #'TODO:
        #' Currently produces non reproducable graphs in driver.R (visualize_synth_ds_its()) even if seeds and other initial conditions
        #' are the same. Suspicion: May have something to do with asynchronicity of Delay_Iterator_Decorator ? 
        #' Weirdly, some of the non-seeded rnd walks are have the same shape but are hozitontally mirrored...
        #' TODO: Sketch control flow and mark which portions are gouverned by which rnd seed -> maybe some inconsistencies/non-determinism
        #'       there???
        #' TODO: Also: Add seed for refresh() calls
        Synthetic_Datastream_Iterators <- R6Class("Synthetic_Datastream_Iterators",
                                          # ---- R6 inheritance -----
                                          inherit = l$result_env$Alphanumeric_Iterators,
                                          
                                          #' ---- Adapt R6 options --------
                                          portable = TRUE,
                                          cloneable = TRUE,
                                          lock_class = TRUE,
                                          lock_objects = TRUE,
                                          
                                          #' ----- Add private Fields & methods ----
                                          private = list(
                                            
                                            undec_num_its = NULL,
                                            undec_char_its = NULL,
                                            
                                            delay_dec_its = NULL,
                                            
                                            pending_insertions = NULL,
                                            pending_immediate_insertions = NULL,
                                            inserted_unfinished_events = NULL,
                                            
                                            new_iterators = NULL,
                                            
                                            synth_ds_it_factory = NULL,
                                            
                                            annotation_information_ground_truth = NULL,
                                            annotation_information_insertion_events = NULL,
                                            
                                            assertions_status = NULL,
                                            debug_mode = NULL,
                                            output_as_frame = NULL,
                                            
                                            NA_label = NULL,
                                            NA_character_sequence_label = NULL,
                                            padding_NA_character_sequence_label = NULL,
                                            inter_NA_character_sequence_label = NULL,
                                            intra_NA_character_sequence_label = NULL,
                                            NA_rnd_walk_label = NULL,
                                            padding_NA_rnd_walk_label = NULL,
                                            inter_NA_rnd_walk_label = NULL,
                                            intra_NA_rnd_walk_label = NULL,
                                            
                                            idle_event_name = NULL,
                                            
                                            non_special_event_names = NULL,
                                            getter_req_params_non_special_event_names = NULL,
                                            
                                            NA_rnd_walk_name = NULL,
                                            padding_NA_rnd_walk_name = NULL,
                                            intra_NA_rnd_walk_name = NULL,
                                            inter_NA_rnd_walk_name = NULL,
                                            NA_rnd_walk_names = NULL,
                                                                                        
                                            character_sequence_name = NULL,
                                            NA_character_sequence_name = NULL,
                                            padding_NA_character_sequence_name = NULL,
                                            intra_NA_character_sequence_name = NULL,
                                            inter_NA_character_sequence_name = NULL,
                                            NA_character_sequence_names = NULL,
                                            getter_req_params_character_sequence = NULL,
                                            
                                            last_immediate_insertion_was_inter = NULL,
                                            
                                            #min_num_NA_padding_elements = NULL,
                                            #max_num_NA_padding_elements = NULL,
                                            
                                            #append_ground_truth = NULL,
                                            verbose = NULL,
                                            
                                            get_next_event_func = NULL,
                                            
                                            #' Other seeds created based on this seed
                                            rnd_seed = NULL,
                                            last_rnd_state = NULL,
                                            
                                            get_next_rnd_seeds = NULL,
                                            last_get_next_rnd_states = NULL,
                                            
                                            get_next_count_rnd_seeds = NULL,
                                            last_get_next_count_rnd_states = NULL,
                                            
                                            refresh_rnd_seed = NULL,
                                            last_refresh_rnd_state = NULL,
                                            
                                            get_annotation_ground_truth_func = function(i){
                                              iterator_id <- i
                                              result <- function(){
                                                l$s$err$assert_msg("Index out of bounds.",
                                                                   iterator_id <= length(private$annotation_information_ground_truth),
                                                                   iterator_id >= 1)
                                                return(private$annotation_information_ground_truth[[iterator_id]]$pop())
                                              }
                                              
                                              return(result)
                                            },
                                            
                                            #' Create internal iterators, set global seed, generate and set noise and delay decorator seeds (if rnd_seed != NULL)
                                            pre_init = function(assertions_status,
                                                                        numeric_iterators_num,
                                                                        character_iterators_num,
                                                                        individual_subparams,
                                                                        noise_decorator_params,
                                                                        delay_decorator_params,
                                                                        num_concat_iterator_params,
                                                                        #append_ground_truth,
                                                                        ground_truth_name,
                                                                        #' Required for delay decorator seed:
                                                                        rnd_seed){
                                              l$s$err$assert_msg("'assertions_status' must be a logical of length one.",
                                                                l$s$tcs$has_length_1(assertions_status, NA_on_fail = FALSE),
                                                                l$s$tcs$is_logical(assertions_status,
                                                                                   accept_NULL = FALSE,
                                                                                   accept_NaN = FALSE,
                                                                                   accept_NA = FALSE))
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
                                              
                                              if(!is.null(rnd_seed)){
                                                private$rnd_seed <- rnd_seed
                                                
                                                glob_env <- globalenv()
                                                old_rnd_state <- get(x = ".Random.seed", envir = glob_env) 
                                                
                                                set.seed(rnd_seed)
                                              }
                                              
                                              total_it_num <- numeric_iterators_num + character_iterators_num
                                              
                                              if(assertions_status){
                                                l$s$err$assert_msg("'numeric_iterators_num' must be a finite, non-negative integer of length one.",
                                                                  l$s$tcs$has_length_1(numeric_iterators_num, NA_on_fail = FALSE),
                                                                  l$s$tcs$is_integer(numeric_iterators_num,
                                                                                     accept_NULL = FALSE,
                                                                                     accept_NaN = FALSE,
                                                                                     accept_NA = FALSE,
                                                                                     lower_bound = 0,
                                                                                     lower_bound_inclusive = TRUE,
                                                                                     upper_bound = Inf,
                                                                                     upper_bound_inclusive = FALSE))
                                                l$s$err$assert_msg("'character_iterators_num' must be a finite, non-negative integer of length one.",
                                                                  l$s$tcs$has_length_1(character_iterators_num, NA_on_fail = FALSE),
                                                                  l$s$tcs$is_integer(character_iterators_num,
                                                                                     accept_NULL = FALSE,
                                                                                     accept_NaN = FALSE,
                                                                                     accept_NA = FALSE,
                                                                                     lower_bound = 0,
                                                                                     lower_bound_inclusive = TRUE,
                                                                                     upper_bound = Inf,
                                                                                     upper_bound_inclusive = FALSE))
                                                
                                                l$s$err$assert_msg("'individual_subparams' must be a logical of length one.",
                                                                  l$s$tcs$has_length_1(individual_subparams, NA_on_fail = FALSE),
                                                                  l$s$tcs$is_logical(individual_subparams,
                                                                                     accept_NULL = FALSE,
                                                                                     accept_NaN = FALSE,
                                                                                     accept_NA = FALSE))
                                                
                                                # l$s$err$assert_msg(paste0("'append_ground_truth' must be a logical of length one if ",
                                                #                          "'individual_subparams' is TRUE and ",
                                                #                          "of length 'numeric_iterators_num + character_iterators_num' otherwise."),
                                                #                   l$s$mg$`%then%`(!individual_subparams, 
                                                #                                   l$s$tcs$has_length_1(append_ground_truth, NA_on_fail = FALSE)),
                                                #                   l$s$mg$`%then%`(individual_subparams,
                                                #                                   l$s$tcs$has_length(append_ground_truth, len = total_it_num, NA_on_fail = FALSE)),
                                                #                   all(l$s$tcs$is_logical(append_ground_truth,
                                                #                                          accept_NULL = FALSE,
                                                #                                          accept_NaN = FALSE,
                                                #                                          accept_NA = FALSE)))
                                                
                                                l$s$err$assert_msg("'ground_truth_name' must be a character vector of length one.",
                                                                  l$s$tcs$has_length_1(ground_truth_name, NA_on_fail = FALSE),
                                                                  l$s$tcs$is_character(ground_truth_name,
                                                                                       accept_NULL = FALSE,
                                                                                       accept_NaN = FALSE,
                                                                                       accept_NA = FALSE))
                                              }
                                              
                                              if(!individual_subparams && total_it_num > 0){
                                                if(numeric_iterators_num > 0){
                                                  noise_decorator_params <- lapply(1:numeric_iterators_num, FUN = function(i){
                                                    return(noise_decorator_params)
                                                  })
                                                  
                                                  num_concat_iterator_params <- lapply(1:numeric_iterators_num, FUN = function(i){
                                                    return(num_concat_iterator_params)
                                                  })
                                                }
                                               
                                                delay_decorator_params <- lapply(1:total_it_num, FUN = function(i){
                                                  return(delay_decorator_params)
                                                })
                                                
                                                # append_ground_truth <- lapply(1:total_it_num, FUN = function(i){
                                                #   return(append_ground_truth)
                                                # })
                                              }
                                              else {
                                                noise_decorator_params <- as.list(noise_decorator_params)
                                                num_concat_iterator_params <- as.list(num_concat_iterator_params)
                                                delay_decorator_params <- as.list(delay_decorator_params)
                                                
                                              }
                                              if(assertions_status){
                                                l$s$err$assert_msg("Parameter list has wrong number of elements.",
                                                                  l$s$tcs$has_length(noise_decorator_params, 
                                                                                     len = numeric_iterators_num, 
                                                                                     NA_on_fail = FALSE),
                                                                  l$s$tcs$has_length(num_concat_iterator_params, 
                                                                                     len = numeric_iterators_num, 
                                                                                     NA_on_fail = FALSE),
                                                                  # l$s$tcs$has_length(append_ground_truth, 
                                                                  #                    len = total_it_num, 
                                                                  #                    NA_on_fail = FALSE),
                                                                  l$s$tcs$has_length(delay_decorator_params, 
                                                                                     len = total_it_num, 
                                                                                     NA_on_fail = FALSE))
                                              }
                                              
                                              undec_num_its <- list()
                                              undec_char_its <- list()
                                              delay_dec_its  <- list()
                                              
                                              num_its <- list()
                                              char_its <- list()
                                              
                                              if(numeric_iterators_num > 0){
                                                for(i in 1:numeric_iterators_num){
                                                  l$s$err$assert_msg(paste0("'assertions_status', 'iterators' and 'lock' should not ",
                                                                            "be present in the names of 'num_concat_iterator_params[[i]]'."),
                                                                     !("assertions_status" %in% names( num_concat_iterator_params[[i]])),
                                                                     !("iterators" %in% names( num_concat_iterator_params[[i]])),
                                                                     !("lock" %in% names( num_concat_iterator_params[[i]])))
                                                  
                                                  undec_num_its[[i]] <- do.call(what = l$s$nit$Numeric_Offset_Normalized_Concatenated_Iterator$new,
                                                                                args = c(list(assertions_status = assertions_status, 
                                                                                              iterators = list(), 
                                                                                              lock = FALSE),
                                                                                         num_concat_iterator_params[[i]]))
                                                  num_its[[i]] <- undec_num_its[[i]]
                                                  
                                                  if(!is.null(noise_decorator_params[[i]])){
                                                    l$s$err$assert_msg(paste0("'assertions_status', 'rnd_seed' and 'iterator' should not ",
                                                                              "be present in the names of 'noise_decorator_params[[i]]'."),
                                                                       !("assertions_status" %in% names( noise_decorator_params[[i]])),
                                                                       !("iterator" %in% names( noise_decorator_params[[i]])),
                                                                       !("rnd_seed" %in% names( noise_decorator_params[[i]])))
                                                    
                                                    if(!is.null(rnd_seed)){
                                                      noise_decorator_params[[i]][["rnd_seed"]] <- sample(.Machine$integer.max, 1)
                                                    }
                                                    
                                                    num_its[[i]] <- do.call(what = l$s$it$Noise_Iterator_Decorator$new,
                                                                            args = c(list(assertions_status = assertions_status,
                                                                                          iterator = num_its[[i]]),
                                                                                     noise_decorator_params[[i]]))
                                                  }
                                                }
                                              }
                                              
                                              if(character_iterators_num > 0){
                                                for(i in 1:character_iterators_num){
                                                  undec_char_its[[i]] <- do.call(what = l$s$cit$Character_Concatenated_Iterator$new,
                                                                                 args = c(list(assertions_status = assertions_status,
                                                                                               iterators = list(),
                                                                                               lock = FALSE)))
                                                  char_its[[i]] <- undec_char_its[[i]]
                                                }
                                              }
                                              all_undec_its <- c(undec_num_its, undec_char_its)
                                              all_its <- c(num_its, char_its)
                                              
                                              if(assertions_status){
                                                l$s$err$assert_msg("Inconsistent length encountered.",
                                                                  length(all_its) == total_it_num,
                                                                  length(all_undec_its) == total_it_num)
                                              }
                                              
                                              if(total_it_num > 0){
                                                time_start <- NULL
                                                time_name <- NULL
                                                
                                                for(i in 1:total_it_num){
                                                  l$s$err$assert_msg("Missing 'timestamp_name' parameter.",
                                                                     "timestamp_name" %in% names(delay_decorator_params[[i]]))
                                                  if(is.null(time_name)){
                                                    time_name <- delay_decorator_params[[i]][["timestamp_name"]]
                                                  }
                                                  else {
                                                    l$s$err$assert_msg("'timestamp_name' in 'delay_decorator_params[[i]]' must be equal across all iterators.",
                                                                       l$s$dts$equals(time_name, delay_decorator_params[[i]][["timestamp_name"]]))
                                                  }
                                                  l$s$err$assert_msg("Missing 'annotation_time_start' parameter.",
                                                                     "annotation_time_start" %in% names(delay_decorator_params[[i]]))
                                                  
                                                  if(is.null(time_start)){
                                                    time_start <- delay_decorator_params[[i]][["annotation_time_start"]]
                                                  }
                                                  else {
                                                    l$s$err$assert_msg("'annotation_time_start' in 'delay_decorator_params[[i]]' must be equal across all iterators.",
                                                                       l$s$dts$equals(time_start, delay_decorator_params[[i]][["annotation_time_start"]]))
                                                  }
                                                  
                                                  if("delay_min" %in% names(delay_decorator_params[[i]])){
                                                    l$s$err$assert_msg("'delay_min' must be set to a value >= 1.",
                                                                       l$s$tcs$has_length_1(delay_decorator_params[[i]][["delay_min"]], NA_on_fail = FALSE),
                                                                       l$s$tcs$is_numeric(delay_decorator_params[[i]][["delay_min"]],
                                                                                          accept_NULL = FALSE,
                                                                                          accept_NaN = FALSE,
                                                                                          accept_NA = FALSE,
                                                                                          lower_bound = 1,
                                                                                          lower_bound_inclusive = TRUE,
                                                                                          upper_bound = Inf,
                                                                                          upper_bound_inclusive = FALSE,
                                                                                          accept_non_integer = TRUE))
                                                  }
                                                  else{
                                                    delay_decorator_params[[i]][["delay_min"]] <- 1
                                                  }
                                                  
                                                  l$s$err$assert_msg(paste0("'assertions_status', 'iterator', 'rnd_seed' and 'annotate_with_time' should not ",
                                                                            "be present in the names of 'delay_decorator_params[[i]]'."),
                                                                     !("assertions_status" %in% names(delay_decorator_params[[i]])),
                                                                     !("iterator" %in% names(delay_decorator_params[[i]])),
                                                                     !("annotate_with_time" %in% names(delay_decorator_params[[i]])),
                                                                     !("rnd_seed" %in% names(delay_decorator_params[[i]])))
                                                  
                                                  if(!is.null(rnd_seed)){
                                                    delay_decorator_params[[i]][["rnd_seed"]] <- sample(.Machine$integer.max, 1)
                                                  }
                                                  
                                                  all_its[[i]] <- do.call(what = l$s$it$Delay_Iterator_Decorator$new,
                                                                          args = c(list(assertions_status = assertions_status,
                                                                                        iterator = all_its[[i]],
                                                                                        annotate_with_time = TRUE),
                                                                                   delay_decorator_params[[i]]))
                                                  delay_dec_its[[i]] <- all_its[[i]]
                                                  
                                                  #if(append_ground_truth[[i]]){
                                                    all_its[[i]] <- do.call(what = l$s$it$Annotation_Iterator_Decorator$new,
                                                                            args = c(list(assertions_status = assertions_status,
                                                                                          iterator = all_its[[i]],
                                                                                          annotate = TRUE,
                                                                                          annotation_name=ground_truth_name,
                                                                                          annotation <- private$get_annotation_ground_truth_func(i))))
                                                  #}
                                                }
                                              }
                                              
                                              private$undec_num_its <- undec_num_its
                                              private$undec_char_its <- undec_char_its
                                              private$delay_dec_its <- delay_dec_its
                                              #private$append_ground_truth <- append_ground_truth
                                              
                                              if(!is.null(private$rnd_seed)){
                                                l$s$err$assert_msg("Inconsistent state.",
                                                                   is.null(private$last_rnd_state))
                                                
                                                private$last_rnd_state <- get(x = ".Random.seed", envir = glob_env) 
                                                assign(".Random.seed", value = old_rnd_state, envir = glob_env)
                                              }
                                              
                                              return(all_its)
                                            },
                                            
                                            assert_init_consistency = function(){
                                              if(l$s$err$get_assertions_status()){
                                                num_its_count <- length(private$undec_num_its)
                                                char_its_count <- length(private$undec_char_its)
                                                total_its_count <- num_its_count + char_its_count
                                                
                                                l$s$err$assert_msg("Inconsistent lengths.",
                                                                   l$s$dts$equals(self$get_num_iterators(), total_its_count),
                                                                   length(private$pending_insertions) == total_its_count,
                                                                   length(private$pending_immediate_insertions) == total_its_count,
                                                                   length(private$inserted_unfinished_events) == total_its_count)
                                                
                                                num_ind <- self$get_numeric_indices()
                                                char_ind <- self$get_character_indices()
                                                l$s$err$assert_msg("Inconsistent indices.",
                                                                   l$s$dts$equals(length(num_ind), num_its_count),
                                                                   l$s$dts$equals(length(char_ind), char_its_count),
                                                                   l$s$mg$`%then%`(num_its_count > 0, setequal(1:num_its_count, num_ind)),
                                                                   l$s$mg$`%then%`(char_its_count > 0, 
                                                                                   setequal((num_its_count + 1):(num_its_count + char_its_count), char_ind)))
                                              }
                                            },
                                            
                                            find_last_non_invisible_event_index = function(event_list, front){
                                              l$s$err$assert_msg("Unknown 'event_list' parameter.",
                                                                 l$s$mg$`%then%`(!l$s$tcs$has_length_0(event_list, NA_on_fail = FALSE), 
                                                                                 length(which(as.logical(lapply(c(private$pending_insertions,
                                                                                                                  private$pending_immediate_insertions,
                                                                                                                  private$inserted_unfinished_events),
                                                                                                                FUN = function(other_event_list){
                                                                                                                  return(identical(event_list, other_event_list))
                                                                                                                })))) == 1))
                                              l$s$err$assert_msg("'front' must be a logical of length one.",
                                                                 l$s$tcs$has_length_1(front, NA_on_fail = FALSE),
                                                                 l$s$tcs$is_logical(front,
                                                                                    accept_NULL = FALSE,
                                                                                    accept_NaN = FALSE,
                                                                                    accept_NA = FALSE))
                                              len <- length(event_list)
                                              result <- NULL
                                              if(len > 0){
                                                if(front){
                                                  indices <- 1:len
                                                }
                                                else{
                                                  indices <- len:1
                                                }
                                                for(i in indices) {
                                                  if(!event_list[[i]]$get_is_invisible()){
                                                    result <- i
                                                    break
                                                  }
                                                }
                                              }
                                              
                                              l$s$err$assert_msg("Inconsistent state.",
                                                                 l$s$mg$`%then%`(!is.null(result),
                                                                                 l$s$tcs$has_length_1(result, NA_on_fail = FALSE)),
                                                                 l$s$mg$`%then%`(!is.null(result),
                                                                                 l$s$tcs$is_integer(result,
                                                                                                    accept_NULL = FALSE,
                                                                                                    accept_NaN = FALSE,
                                                                                                    accept_NA = FALSE,
                                                                                                    lower_bound = 1,
                                                                                                    lower_bound_inclusive = TRUE,
                                                                                                    upper_bound = len,
                                                                                                    upper_bound_inclusive = TRUE)))
                                              
                                              return(result)
                                            },
                                            
                                            assert_consistency = function(){
                                              if(l$s$err$get_assertions_status()){
                                                max_pend_in_len <- max(sapply(private$pending_insertions, FUN = function(ins_list){
                                                  return(length(ins_list))
                                                }))
                                                l$s$err$assert_msg("'max_pend_in_len' should be <= 2.",
                                                               max_pend_in_len <= 2)
                                              
                                                l$s$err$assert_msg("Iterator counts do not match.",
                                                                   self$get_num_numeric_iterators() == length(private$undec_num_its),
                                                                   self$get_num_character_iterators() == length(private$undec_char_its))
                                                
                                                num_its <- num_its <- self$get_num_iterators()
                                                if(num_its > 0){
                                                  for(it_id in 1:num_its){
                                                    dec_it <- self$.get_iterator(it_id)
                                                    undec_it <- self$.get_undecorated_iterator(it_id)
                                                    delay_it <- private$delay_dec_its[[it_id]]
                                                    
                                                    l$s$err$assert_msg("Inconsistency in internal iterators.",
                                                                       l$s$mg$`%then%`(TRUE, #private$append_ground_truth, 
                                                                         identical(delay_it, dec_it$.get_decorated_iterator())),
                                                                       l$s$mg$`%then%`(FALSE, #!private$append_ground_truth, 
                                                                                       identical(delay_it, self$.get_iterator(it_id))),
                                                                       delay_it$get_next_count() <= undec_it$get_next_count(),
                                                                       l$s$dts$equals(delay_it$get_length(), undec_it$get_length()),
                                                                       l$s$dts$equals(delay_it$get_num_remaining(), undec_it$get_num_remaining()),
                                                                       delay_it$get_next_count() <= undec_it$get_current_num_remaining(),
                                                                       l$s$dts$equals(dec_it$get_length(), delay_it$get_length()),
                                                                       l$s$dts$equals(dec_it$get_num_remaining(), delay_it$get_num_remaining()),
                                                                       dec_it$get_next_count() <= undec_it$get_current_num_remaining(),
                                                                       l$s$mg$`%eq%`(undec_it$get_current_num_remaining() == 0, length(private$inserted_unfinished_events[[it_id]]) == 0))
                                                    
                                                    curr_num_rem <- undec_it$get_current_num_remaining()
                                                    
                                                    curr_annotation <- private$annotation_information_insertion_events[[it_id]]
                                                    curr_ann_nums_rem <- as.numeric(curr_annotation$get_nums_remaining())
                                                    curr_ann_lengths <- as.numeric(curr_annotation$get_lengths())
                                                    num_anns <- length(curr_ann_nums_rem)
                                                    l$s$err$assert_msg(paste0("'curr_ann_nums_rem' and 'curr_ann_lengths' should only contain ",
                                                                              "entries > 0 and be of the same length."),
                                                                       all(curr_ann_nums_rem > 0),
                                                                       all(curr_ann_lengths > 0),
                                                                       length(curr_ann_lengths) == num_anns)
                                                    l$s$err$assert_msg(paste0("The length of 'curr_ann_nums_rem' should always match ",
                                                                              "the length of 'inserted_unfinished_events[[it_id]]' ",
                                                                              "and the sum of the formers entries should equal the current ",
                                                                              "number of remaining elements in the corresponding internal iterator."),
                                                                       length(curr_ann_nums_rem) == length(private$inserted_unfinished_events[[it_id]]),
                                                                       curr_num_rem == sum(curr_ann_nums_rem))
                                                    
                                                    if(num_anns > 0){
                                                      for(i in num_anns:1){
                                                        l$s$err$assert_msg("Inconsistent annotation information status.",
                                                                           curr_ann_nums_rem[[i]] <= curr_ann_lengths[[i]])
                                                        if(curr_ann_nums_rem[[i]] < curr_ann_lengths[[i]]){
                                                          l$s$err$assert_msg(paste0("Only fist element in 'curr_ann_nums_rem' may have ",
                                                                                    "its remaining number of elements smaller than its length."),
                                                                             i == 1)
                                                        }
                                                      }
                                                    }
                                                    
                                                    gt_annotation <- private$annotation_information_ground_truth[[it_id]]
                                                    gt_ann_nums_rem <- as.numeric(gt_annotation$get_nums_remaining())
                                                    gt_ann_lengths <- as.numeric(gt_annotation$get_lengths())
                                                    gt_num_anns <- length(gt_ann_nums_rem)
                                                    if(gt_num_anns > 0){
                                                      for(i in gt_num_anns:1){
                                                        l$s$err$assert_msg("Inconsistent annotation information status.",
                                                                           gt_ann_nums_rem[[i]] <= gt_ann_lengths[[i]])
                                                        if(gt_ann_nums_rem[[i]] < gt_ann_lengths[[i]]){
                                                          l$s$err$assert_msg(paste0("Only fist element in 'gt_ann_nums_rem' may have ",
                                                                                    "its remaining number of elements smaller than its length."),
                                                                             i == 1)
                                                        }
                                                      }
                                                    }
                                                   
                                                    l$s$err$assert_msg("No invisible events should be present in 'pending_insertions[[it_id]]'.",
                                                                       all(sapply(private$pending_insertions[[it_id]], FUN = function(event){
                                                                         return(!event$get_is_invisible())
                                                                       })))
                                                    
                                                    l$s$err$assert_msg("Inconsistent append at values 'pending_insertions[[it_id]]'.",
                                                                       all(sapply(private$pending_insertions[[it_id]], 
                                                                                  FUN = function(event){
                                                                                    return(length(event$get_event_dependencies()) == 
                                                                                             length(event$get_append_at()))
                                                                                  })),
                                                                       all(sapply(private$pending_insertions[[it_id]], 
                                                                                  FUN = function(event){
                                                                                    return(l$s$mg$`%then%`(length(event$get_append_at()) > 0, 
                                                                                                           l$s$mg$`%eq%`(event$get_sequence_name() == private$inter_NA_rnd_walk_name ||
                                                                                                                           event$get_sequence_name() == private$inter_NA_character_sequence_name,
                                                                                                                         all(event$get_append_at() == "end"))))
                                                                                  })),
                                                                       all(sapply(private$pending_insertions[[it_id]], 
                                                                                  FUN = function(event){
                                                                                    return(l$s$mg$`%then%`(length(event$get_append_at()) > 0, 
                                                                                                           l$s$mg$`%eq%`(event$get_sequence_name() != private$inter_NA_rnd_walk_name &&
                                                                                                                           event$get_sequence_name() != private$inter_NA_character_sequence_name,
                                                                                                                         all(event$get_append_at() == "begin"))))
                                                                                  })))
                                                   
                                                    all_events_at_concat_it <- unlist(c(private$pending_insertions[[it_id]],
                                                                                        private$pending_immediate_insertions[[it_id]],
                                                                                        private$inserted_unfinished_events[[it_id]]))
                                                    l$s$err$assert_msg("No invisible events should be presend in the event dependencies of an event.",
                                                                       all(sapply(all_events_at_concat_it,
                                                                           FUN = function(event){
                                                                             return(all(sapply(event$get_event_dependencies(),
                                                                                               FUN = function(event_to_which_dependent){
                                                                                                 return(!event_to_which_dependent$get_is_invisible())
                                                                                               })))
                                                                           })))
                                                    
                                                    next_t <- delay_it$get_next_time_available()
                                                    
                                                    l$s$err$assert_msg(paste0("For events in 'pending_immediate_insertions' and ",
                                                                              "'inserted_unfinished_events', 'is_immediate_insertion_possible(next_t)' ",
                                                                              "should return TRUE."),
                                                                              all(sapply(unlist(c(private$pending_immediate_insertions[[it_id]],
                                                                                                  private$inserted_unfinished_events[[it_id]])),
                                                                                         FUN = function(event){
                                                                                           return(event$is_immediate_insertion_possible(next_t))
                                                                                         })))
                                                    
                                                    l$s$err$assert_msg("Non-matching iterator ids.",
                                                                       all(sapply(all_events_at_concat_it, 
                                                                                  FUN = function(event){
                                                                                    return(event$get_iterator_id() == it_id)
                                                                                  })))
                                                    
                                                    l$s$err$assert_msg("'get_inserted_unfinished_events_index()' returned an inconsistent value.",
                                                                       all(sapply(unlist(c(private$pending_insertions[[it_id]],
                                                                                           private$pending_immediate_insertions[[it_id]])),
                                                                                  FUN = function(event){
                                                                                    return(is.null(event$get_inserted_unfinished_events_index()))
                                                                                  })),
                                                                       all(sapply(unlist(private$inserted_unfinished_events[[it_id]]),
                                                                                  FUN = function(event){
                                                                                    ins_id <- event$get_inserted_unfinished_events_index()
                                                                                    is_num <- l$s$tcs$has_length_1(ins_id, NA_on_fail = FALSE) &&
                                                                                      l$s$tcs$is_integer(ins_id,
                                                                                                         accept_NULL = FALSE,
                                                                                                         accept_NaN = FALSE,
                                                                                                         accept_NA = FALSE,
                                                                                                         lower_bound = 0,
                                                                                                         lower_bound_inclusive = TRUE,
                                                                                                         upper_bound = length(private$inserted_unfinished_events[[it_id]]),
                                                                                                         upper_bound_inclusive = TRUE)
                                                                                    
                                                                                    if(!is_num){
                                                                                      return(FALSE)
                                                                                    }
                                                                                    else{
                                                                                      return(identical(event, private$inserted_unfinished_events[[it_id]][[ins_id]]))
                                                                                    }
                                                                                  })))
                                                  }
                                                  
                                                  all_sublists <- c(private$pending_insertions,
                                                                    private$pending_immediate_insertions,
                                                                    private$inserted_unfinished_events)
                                                  l$s$err$assert_msg("Event lists should be disjoint.",
                                                                     all(as.logical(lapply(all_events_at_concat_it,
                                                                                           FUN = function(event){
                                                                                             count <- sum(as.numeric(lapply(all_sublists,
                                                                                                                            FUN = function(sublist){
                                                                                                                              return(sum(as.numeric(lapply(sublist,
                                                                                                                                                           FUN = function(list_event){
                                                                                                                                                             if(identical(event, list_event)){
                                                                                                                                                               return(1)
                                                                                                                                                             }
                                                                                                                                                             else{
                                                                                                                                                               return(0)
                                                                                                                                                             }
                                                                                                                                                           }))))
                                                                                                                            })))
                                                                                             return(count == 1)
                                                                                           }))))
                                                }
                                              }
                                            },
                                            
                                            get_dependencies_for_new_event = function(){
                                              event_dependencies <- list()
                                              num_its <- self$get_num_iterators()
                                              
                                              for(i in 1:num_its){
                                                if((len_pend <- length(private$pending_insertions[[i]])) > 0){
                                                  l$s$err$assert_msg("Elements in 'pending_insertions' should not be invisible.",
                                                                     !private$pending_insertions[[i]][[len_pend]]$get_is_invisible())
                                                  event_dependencies <- append(event_dependencies, 
                                                                               private$pending_insertions[[i]][[len_pend]])
                                                }
                                                else {
                                                  event_dependency_id <- private$find_last_non_invisible_event_index(
                                                    event_list = private$pending_immediate_insertions[[i]], front = FALSE)
                                                  
                                                  if(is.null(event_dependency_id)){
                                                    event_dependency_id <- private$find_last_non_invisible_event_index(
                                                      event_list = private$inserted_unfinished_insertins[[i]], front = FALSE)
                                                    
                                                    if(!is.null(event_dependency_id)){
                                                      event_dependencies <- append(event_dependencies,
                                                                                   private$inserted_unfinished_insertins[[i]][[event_dependency_id]])
                                                    }
                                                  }
                                                  else{
                                                    event_dependencies <- append(event_dependencies,
                                                                                 private$pending_immediate_insertions[[i]][[event_dependency_id]])
                                                  }
                                                }
                                              }
                                              return(event_dependencies)
                                            },
                                            
                                            refresh_pending_insertions = function(){
                                              private$print_info("  'refresh_pending_insertions()': START")
                                              #' Guaranteed to only be called if get_num_iterators() > 0
                                              private$assert_consistency()
                                              
                                              # Insert new events into pending insertions until at 
                                              # least one entry in 'pending_insertions' has length 2
                                              max_pend_in_len <- max(sapply(private$pending_insertions, FUN = function(ins_list){
                                                return(length(ins_list))
                                              }))
                                              
                                              num_its <- self$get_num_iterators()
                                              l$s$err$assert_msg("Inconsistent state.",
                                                                 max_pend_in_len <= 2)
                                              
                                              while(max_pend_in_len < 2){
                                                next_event <- private$get_next_event_func()
                                                next_event <- as.list(next_event)
                                                
                                                l$s$err$assert_msg(paste0("'get_next_event_func()' returned an invalid object."),
                                                                   length(next_event) == 3,
                                                                   l$s$tcs$has_length_1(next_event[[1]], NA_on_fail = FALSE),
                                                                   l$s$tcs$is_character(next_event[[1]],
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE))
                                                
                                                name <- trimws(next_event[[1]])
                                                next_insert_events <- list()
                                                is_idle <- l$s$dts$equals(name, private$idle_event_name)
                                                
                                                if(l$s$err$get_assertions_status()){
                                                  l$s$err$assert_msg(paste0("'get_next_event_func()' returned an invalid object."),
                                                                     l$s$tcs$has_length_1(next_event[[1]], NA_on_fail = FALSE),
                                                                     l$s$tcs$is_character(next_event[[1]],
                                                                                          accept_NULL = FALSE,
                                                                                          accept_NaN = FALSE,
                                                                                          accept_NA = FALSE))
                                                  
                                                  is_NA_rnd_walk <- l$s$dts$equals(name, private$NA_rnd_walk_name)
                                                  is_i_NA_rnd_walk <- l$s$dts$equals(name, private$intra_NA_rnd_walk_name) ||
                                                    l$s$dts$equals(name, private$inter_NA_rnd_walk_name)
                                                  
                                                  is_char_seq <- l$s$dts$equals(name, private$character_sequence_name)
                                                  is_NA_char_seq <- l$s$dts$equals(name, private$NA_character_sequence_name)
                                                  is_i_NA_char_seq <- l$s$dts$equals(name, private$intra_NA_character_sequence_name) ||
                                                    l$s$dts$equals(name, private$inter_NA_character_sequence_name)
                                                  
                                                  is_non_special <- !is_idle && !is_NA_rnd_walk && !is_i_NA_rnd_walk &&
                                                    !is_char_seq && !is_NA_char_seq && !is_i_NA_char_seq
                                                  
                                                  l$s$err$assert_msg(paste0("'get_next_event_func()' returned an invalid object."),
                                                                     !is_NA_rnd_walk,
                                                                     !is_i_NA_rnd_walk,
                                                                     !is_NA_char_seq,
                                                                     !is_i_NA_char_seq,
                                                                     length(which(c(is_idle, is_char_seq, is_non_special))) == 1,
                                                                     
                                                                     l$s$mg$`%eq%`(is_idle || is_non_special, 
                                                                                     l$s$tcs$has_length_0(next_event[[3]], NA_on_fail = FALSE)),
                                                                     l$s$mg$`%eq%`(is_char_seq,
                                                                                     setequal(names(next_event[[3]]), l$required_param_element)),
                                                                     l$s$mg$`%eq%`(is_non_special,
                                                                                     name %in% private$non_special_event_names),
                                                                     l$s$mg$`%eq%`(is_idle,
                                                                                     l$s$tcs$has_length_0(next_event[[2]], NA_on_fail = FALSE)),
                                                                     l$s$mg$`%eq%`(is_char_seq || is_non_special,
                                                                                     l$s$tcs$has_length_1(next_event[[2]], NA_on_fail = FALSE)),
                                                                     l$s$mg$`%then%`(is_char_seq || is_non_special,
                                                                                     l$s$tcs$is_integer(next_event[[2]],
                                                                                                        accept_NULL = FALSE,
                                                                                                        accept_NaN = FALSE,
                                                                                                        accept_NA = FALSE,
                                                                                                        lower_bound = 1,
                                                                                                        lower_bound_inclusive = TRUE,
                                                                                                        upper_bound = num_its,
                                                                                                        upper_bound_inclusive = TRUE)),
                                                                     l$s$mg$`%then%`(is_char_seq || is_non_special,
                                                                                     length(private$pending_insertions[[next_event[[2]]]]) <= 1))
                                                }
                                                
                                                event_dependencies <- private$get_dependencies_for_new_event()
                                                if(is_idle){
                                                  for(i in 1:num_its){
                                                    next_insert_events <- append(next_insert_events,
                                                                                 private$get_NA_event(i, 
                                                                                                   inter = TRUE, 
                                                                                                   invisible = FALSE, 
                                                                                                   event_dependencies = event_dependencies))
                                                  }
                                                  max_pend_in_len <- max_pend_in_len + 1
                                                }
                                                else { # Non-idle event
                                                  params <- list()
                                                  params[[l$required_param_assertions_status]] <- private$assertions_status
                                                  params <- append(params, as.list(next_event[[3]]))
                                                  
                                                  id <- next_event[[2]]
                                                  next_insert_events <- append(next_insert_events, 
                                                                               result_env$.Insertion_Event$new(
                                                                                 assertions_status = private$assertions_status,
                                                                                 iterator_id = id,
                                                                                 sequence_name = name,
                                                                                 sequence_params = params,
                                                                                 append_at = "begin",
                                                                                 event_dependencies = event_dependencies,
                                                                                 is_invisible = FALSE))
                                                  
                                                  max_pend_in_len <- max(max_pend_in_len, length(private$pending_insertions[[id]]) + 1) 
                                                }
                                                num_next_events <- length(next_insert_events)
                                                l$s$err$assert_msg("Inconsistent state.",
                                                                   l$s$mg$`%then%`(is_idle, num_next_events == num_its),
                                                                   l$s$mg$`%then%`(!is_idle, num_next_events == 1))
                                                
                                                for(i in 1:num_next_events){
                                                  event <- next_insert_events[[i]]
                                                  id <- event$get_iterator_id()
                                                  private$pending_insertions[[id]] <- append(private$pending_insertions[[id]], event)
                                                  private$print_info(paste0("    Appending event:\n",
                                                                            "      Position in pending_insertions[[", event$get_iterator_id(),"]]: ", 
                                                                            length(private$pending_insertions[[id]]),"\n",
                                                                            event$toString(tab = "      ")))
                                                  
                                                }
                                              }
                                             
                                              private$assert_consistency()
                                              private$print_info("  'refresh_pending_insertions()': FINISHED")
                                            },
                                            
                                            #' TODO: More exact, fail safe method that never returns NULL
                                            get_est_latest_global_non_inv_event_start = function() {
                                              # Called in pad_with_NA_rnd_walk
                                              #' Guaranteed to only be called if get_num_iterators() > 0
                                              private$print_info("      'get_est_latest_global_non_inv_event_start()': START")
                                              
                                              num_its <- self$get_num_iterators()
                                              est_latest_global_non_inv_event_start <- NULL
                                              
                                              for(it_id in 1:num_its){
                                                undec_it <- self$.get_undecorated_iterator(it_id)
                                                private$print_info(paste0("        Processing iterator with ID: ", it_id,"\n",
                                                                          "          Iterator information:\n",
                                                                          undec_it$toString("            ")))
                                                
                                                total_num_rem <- undec_it$get_current_num_remaining()
                                                
                                                if(total_num_rem == 0){
                                                  private$print_info(paste0("          Iterator has no elements left. Proceeding to next one."))
                                                  
                                                  next
                                                }
                                                last_non_inv_inserted_event_id <- 
                                                  private$find_last_non_invisible_event_index(
                                                    private$inserted_unfinished_events[[it_id]], front = FALSE)
                                                
                                                private$print_info(paste0("          find_last_non_invisible_event_index(inserted_unfinished_events[[", it_id, "]], front = FALSE) ",
                                                                          "returned ", if(is.null(last_non_inv_inserted_event_id)) "NULL" else last_non_inv_inserted_event_id))
                                                
                                                
                                                if(is.null(last_non_inv_inserted_event_id)){ # no non-invisible events present in inserted event list
                                                  private$print_info(paste0("          find_last_non_invisible_event_index returned NULL. Proceeding to next iterator."))
                                                  
                                                  next
                                                }
                                                else {
                                                  event <- private$inserted_unfinished_events[[it_id]][[last_non_inv_inserted_event_id]]
                                                  private$print_info(paste0("          Correspodning event:\n",
                                                                            event$toString("            ")))
                                                  
                                                  #' Refresh only called if iterators empty -> no element of new event should have been retrieved yet
                                                  l$s$err$assert_msg("Inconsistent state.",
                                                                     is.null(event$get_start_timestamp()))
                                                  nums_rem <- private$annotation_information_insertion_events[[it_id]]$get_nums_remaining()
                                                  l$s$err$assert_msg("Inconsistent state.",
                                                                     sum(as.numeric(nums_rem)) == total_num_rem,
                                                                     last_non_inv_inserted_event_id <= length(nums_rem),
                                                                     last_non_inv_inserted_event_id >= 1)
                                                  
                                                  num_elems_until_event <- 1 + if(last_non_inv_inserted_event_id == 1) 0 else sum(as.numeric(nums_rem[1:(last_non_inv_inserted_event_id - 1)]))
                                                  
                                                  delay_it <- self$.get_delay_decorated_iterator(it_id)
                                                  next_t <- delay_it$get_next_time_available()
                                                  mean_delta_t <- delay_it$get_delay_mean()
                                                  min_delta_t <- delay_it$get_delay_min()
                                                  l$s$err$assert_msg("'min_delta_t' should be >= 1 at this point.",
                                                                     min_delta_t >= 1)
                                                  
                                                  curr_latest_start <- next_t + max(mean_delta_t, min_delta_t) * (num_elems_until_event - 1)
                                                  private$print_info(paste0("        New latest start time estimation: ", curr_latest_start))
                                                  
                                                  l$s$err$assert_msg("'curr_latest_start' should be >= 0 at this point.",
                                                                     l$s$tcs$has_length_1(curr_latest_start, NA_on_fail = FALSE),
                                                                     l$s$tcs$is_numeric(curr_latest_start,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE,
                                                                                        lower_bound = 0,
                                                                                        lower_bound_inclusive = TRUE,
                                                                                        upper_bound = Inf,
                                                                                        upper_bound_inclusive = FALSE,
                                                                                        accept_non_integer = TRUE))
                                                  
                                                  if(is.null(est_latest_global_non_inv_event_start)){
                                                    est_latest_global_non_inv_event_start <- curr_latest_start
                                                  }
                                                  else{
                                                    est_latest_global_non_inv_event_start <- max(est_latest_global_non_inv_event_start, curr_latest_start)
                                                  }
                                                  private$print_info(paste0("          New result: ",est_latest_global_non_inv_event_start))
                                                  
                                                }
                                              }
                                              is_null_result <- is.null(est_latest_global_non_inv_event_start)
                                              private$print_info(paste0("        Final result: ", if(is_null_result) "NULL" else est_latest_global_non_inv_event_start))
                                              
                                              l$s$err$assert_msg("'est_latest_global_non_inv_event_start' should be >= 0 at this point.",
                                                                 l$s$mg$`%then%`(!is_null_result, l$l$s$tcs$has_length_1(est_latest_global_non_inv_event_start, NA_on_fail = FALSE)),
                                                                 l$s$mg$`%then%`(!is_null_result, l$s$tcs$is_numeric(est_latest_global_non_inv_event_start,
                                                                                                                     accept_NULL = FALSE,
                                                                                                                     accept_NaN = FALSE,
                                                                                                                     accept_NA = FALSE,
                                                                                                                     lower_bound = 0,
                                                                                                                     lower_bound_inclusive = TRUE,
                                                                                                                     upper_bound = Inf,
                                                                                                                     upper_bound_inclusive = FALSE,
                                                                                                                     accept_non_integer = TRUE)))
                                              
                                              private$print_info(paste0("      'get_est_latest_global_non_inv_event_start()': FINISHED"))
                                              return(est_latest_global_non_inv_event_start)
                                            },
                                            
                                            refresh_pending_immediate_insertions = function(){
                                              #' Guaranteed to only be called if get_num_iterators() > 0
                                              private$print_info("  'refresh_pending_immediate_insertions()': START")
                                              private$assert_consistency()
                                              
                                              # 0.1 Performed in get_next() and NOT here:
                                              # Setting set_last_emitted_timestamp() of current inserted_unfinished_events insertion event and
                                              # set_end_timestamp() if applicable. Remove insertion event in the latter case.
                                              # Use annotation_information_insertion_events for the above and pop_silent() the appropriate number of
                                              # values.
                                              
                                              # 1.1 Go over all pending insertions and check if immediate insertion possible ->
                                              # move to pending_immediate_insertions where possible (plus intra rnd walk) 
                                              
                                              params <- list()
                                              params[[l$required_param_assertions_status]] <- private$assertions_status
                                              num_its <- self$get_num_iterators()
                                              
                                              for(it_id in 1:num_its){
                                                insert_possible <- TRUE
                                                next_timestamp <- private$delay_dec_its[[it_id]]$get_next_time_available()
                                                len_pend_ins <- length(private$pending_insertions[[it_id]])
                                                private$print_info(paste0("    Processing iterator: ", it_id,"\n",
                                                                         "    Iterator's next element timestamp: ", next_timestamp, "\n",
                                                                         "    Length of pending_insertions[[", it_id,"]]: ", len_pend_ins))
                                                if(len_pend_ins > 0){
                                                  private$print_info(paste0("    pending_insertions[[", it_id,"]][[1]]:\n",
                                                                            private$pending_insertions[[it_id]][[1]]$toString(tab="      ")))
                                                                           
                                                }
                                                
                                                while(insert_possible){
                                                  
                                                  if(len_pend_ins > 0 && 
                                                     private$pending_insertions[[it_id]][[1]]$is_immediate_insertion_possible(next_timestamp)){
                                                    #' Append event itself prepended by an intra NA sequence 
                                                    #' (the latter only if neither previous nor next event is inter event)
                                                    append_list <- list(private$pending_insertions[[it_id]][[1]])
                                                    next_sequence_name <-  private$pending_insertions[[it_id]][[1]]$get_sequence_name()
                                                    next_is_inter <- (next_sequence_name %in%
                                                                        list(private$inter_NA_rnd_walk_name,
                                                                             private$inter_NA_character_sequence_name))
                                                    
                                                    l$s$err$assert_msg("Inconsistent state.",
                                                                       l$s$mg$`%then%`(!next_is_inter,
                                                                                       next_sequence_name %in% private$non_special_event_names ||
                                                                                         next_sequence_name %in% 
                                                                                         list(private$NA_rnd_walk_name,
                                                                                              private$NA_character_sequence_name,
                                                                                              private$character_sequence_name)))
                                                    
                                                    l$s$err$assert_msg("This should not happen.",
                                                                       l$s$mg$`%then%`(!next_is_inter,
                                                                                       next_sequence_name %in% private$non_special_event_names ||
                                                                                         next_sequence_name %in% 
                                                                                           list(private$NA_rnd_walk_name,
                                                                                                private$NA_character_sequence_name,
                                                                                                private$character_sequence_name)))
                                                    append_intra_NA <- !private$last_immediate_insertion_was_inter[[it_id]] && !next_is_inter
                                                    if(append_intra_NA){
                                                      intra_NA_event <- private$get_NA_event(i = it_id, 
                                                                                             intra = TRUE,
                                                                                             invisible = TRUE, 
                                                                                             event_dependencies = NULL)
                                                      
                                                      append_list <- append(list(intra_NA_event), append_list)
                                                    }
                                                    private$last_immediate_insertion_was_inter[[it_id]] <- next_is_inter
                                                    
                                                    private$pending_immediate_insertions[[it_id]] <-
                                                      append(private$pending_immediate_insertions[[it_id]], append_list)
                                                    
                                                    private$print_info(paste0("    Moved event with ID ", private$pending_insertions[[it_id]][[1]]$.get_unique_id(),
                                                                       " to pending_immediate_insertions[[", it_id, "]]"))
                                                    
                                                    if(append_intra_NA){
                                                      private$print_info(paste0("\n    Appending NA event to pending_immediate_insertions[[", it_id, "]]:\n",
                                                                       intra_NA_event$toString(tab = "      "),"\n",
                                                                       "    Positions: ", length(private$pending_immediate_insertions[[it_id]]),
                                                                       " and ", length(private$pending_immediate_insertions[[it_id]]) - 1))
                                                    }
                                                    
                                                    private$pending_insertions[[it_id]][[1]] <- NULL
                                                    len_pend_ins <- len_pend_ins - 1
                                                    l$s$err$assert_msg("Inconsistent state.",
                                                                       len_pend_ins == length(private$pending_insertions[[it_id]]))
                                                    
                                                  }
                                                  else{
                                                    insert_possible <- FALSE
                                                  }
                                                }
                                              }
                                              
                                              private$assert_consistency()
                                              private$print_info("  'refresh_pending_immediate_insertions()': FINISHED")
                                              
                                            },
                                            
                                            get_NA_event = function(i, 
                                                                    inter = FALSE, 
                                                                    intra = FALSE,
                                                                    padding = FALSE,
                                                                    fixed_length = NULL,
                                                                    invisible, 
                                                                    event_dependencies = NULL) {
                                              self$.check_index(i)
                                              
                                              fixed_length_provided <- !is.null(fixed_length)
                                              num_num_its <- self$get_num_numeric_iterators()
                                              
                                              if(l$s$err$get_assertions_status()){
                                                l$s$err$assert_msg("Illegal arguments.",
                                                                   l$s$tcs$has_length_1(inter, NA_on_fail = FALSE),
                                                                   l$s$tcs$has_length_1(intra, NA_on_fail = FALSE),
                                                                   l$s$tcs$has_length_1(padding, NA_on_fail = FALSE),
                                                                   l$s$tcs$has_length_1(invisible, NA_on_fail = FALSE),
                                                                   l$s$tcs$is_logical(inter,
                                                                                      accept_NULL = FALSE,
                                                                                      accept_NaN = FALSE,
                                                                                      accept_NA = FALSE),
                                                                   l$s$tcs$is_logical(intra,
                                                                                      accept_NULL = FALSE,
                                                                                      accept_NaN = FALSE,
                                                                                      accept_NA = FALSE),
                                                                   l$s$tcs$is_logical(invisible,
                                                                                      accept_NULL = FALSE,
                                                                                      accept_NaN = FALSE,
                                                                                      accept_NA = FALSE),
                                                                   l$s$tcs$is_logical(padding,
                                                                                      accept_NULL = FALSE,
                                                                                      accept_NaN = FALSE,
                                                                                      accept_NA = FALSE),
                                                                   l$s$mg$`%then%`(!inter, l$s$tcs$has_length_0(event_dependencies, NA_on_fail)),
                                                                   l$s$mg$`%then%`(fixed_length_provided, 
                                                                                   l$s$tcs$has_length_1(fixed_length, NA_on_fail = FALSE)),
                                                                   l$s$mg$`%then%`(fixed_length_provided, l$s$tcs$is_integer(fixed_length,
                                                                                                               accept_NULL = FALSE,
                                                                                                               accept_NaN = FALSE,
                                                                                                               accept_NA = FALSE,
                                                                                                               lower_bound = 1,
                                                                                                               lower_bound_inclusive = TRUE,
                                                                                                               upper_bound = Inf,
                                                                                                               upper_bound_inclusive = FALSE)),
                                                                   length(which(c(inter, intra, padding, fixed_length_provided))) == 1)
                                                if(i <= num_num_its){
                                                  l$s$err$assert_msg("Numeric iterator ID inconsistency.",
                                                                     i %in% self$get_numeric_indices())
                                                }
                                                else {
                                                  l$s$err$assert_msg("Character iterator ID inconsistency.",
                                                                     i %in% self$get_character_indices())
                                                }
                                              }
                                              
                                              params <- list()
                                              params[[l$required_param_assertions_status]] <- private$assertions_status
                                              if(fixed_length_provided){
                                                params[[l$required_param_fixed_length]] <- fixed_length
                                                if(i <= num_num_its){
                                                  NA_seq_name <-  private$NA_rnd_walk_name
                                                }
                                                else {
                                                  NA_seq_name <-  private$NA_character_sequence_name
                                                }
                                                append_at = "begin"
                                              }
                                              else if(inter){
                                                if(i <= num_num_its){
                                                  NA_seq_name <-  private$inter_NA_rnd_walk_name
                                                }
                                                else {
                                                  NA_seq_name <-  private$inter_NA_character_sequence_name
                                                }
                                                append_at = "end"
                                              }
                                              else if(intra){
                                                if(i <= num_num_its){
                                                  NA_seq_name <-  private$intra_NA_rnd_walk_name
                                                }
                                                else {
                                                  NA_seq_name <-  private$intra_NA_character_sequence_name
                                                }
                                                append_at = "begin"
                                              }
                                              else { # padding
                                                if(i <= num_num_its){
                                                  NA_seq_name <-  private$padding_NA_rnd_walk_name
                                                }
                                                else {
                                                  NA_seq_name <-  private$padding_NA_character_sequence_name
                                                }
                                                append_at = "begin"
                                              }
                                              
                                              NA_event <- result_env$.Insertion_Event$new(
                                                assertions_status = private$assertions_status,
                                                iterator_id = i,
                                                sequence_name = NA_seq_name,
                                                sequence_params = params,
                                                append_at = append_at,
                                                event_dependencies = event_dependencies,
                                                is_invisible = invisible)
                                              
                                              return(NA_event)
                                            },
                                            
                                            pad_with_NA_events = function() {
                                              #' Guaranteed to only be called if get_num_iterators() > 0
                                              
                                              private$print_info("  'pad_with_NA_events()': START")
                                              private$assert_consistency()
                                              
                                              result <- FALSE
                                              
                                              # 2 For iterators with smallest next_t: If iterator is empty:
                                              #   2.1 Estimate time t when insertion becomes possible
                                              #   2.2 Append rnd walk/NA characters to immediate insertion list 
                                              #       of a length s.th. time of next element after last NA element will be 
                                              #       approximatelly >= t
                                              num_its <- self$get_num_iterators()
                                              oldest_empty_it_ids <- list()
                                              oldest_it_ids <- list()
                                              min_next_t <- Inf
                                              for(it_id in 1:num_its){
                                                undec_it <- self$.get_undecorated_iterator(it_id)
                                                delay_it <- private$delay_dec_its[[it_id]]
                                                
                                                total_num_rem <- undec_it$get_current_num_remaining()
                                                # But not the other way around 
                                                l$s$err$assert_msg(paste0("At this point, 'private$pending_immediate_insertions[[it_id]]' ",
                                                                          "should be empty if 'total_num_rem' is zero."),
                                                                   l$s$mg$`%then%`(
                                                                     total_num_rem == 0, 
                                                                     length(private$pending_immediate_insertions[[it_id]]) == 0),
                                                                   l$s$mg$`%eq%`(total_num_rem == 0,
                                                                                 length(private$inserted_unfinished_events[[it_id]]) == 0))
                                                
                                                next_t <- delay_it$get_next_time_available()
                                                if(next_t < min_next_t){
                                                  min_next_t <- next_t
                                                  oldest_it_ids <- list(it_id)
                                                }
                                                else if(next_t == min_next_t){
                                                  oldest_it_ids <- append(oldest_it_ids, it_id)
                                                }
                                              }
                                              for(it_id in oldest_it_ids){
                                                undec_it <- self$.get_undecorated_iterator(it_id)
                                                if(undec_it$get_current_num_remaining() == 0){
                                                  oldest_empty_it_ids <- append(oldest_empty_it_ids, it_id)
                                                }
                                              }
                                              private$print_info(paste0("    Empty iterator IDs: ", toString(oldest_empty_it_ids)))
                                              
                                              #est_latest_global_non_inv_event_start <- NULL
                                              for(it_id in oldest_empty_it_ids){
                                                undec_it <- self$.get_undecorated_iterator(it_id)
                                                l$s$err$assert_msg("Iterator should be empty at this point.",
                                                                   undec_it$get_current_num_remaining() == 0,
                                                                   length(private$pending_immediate_insertions[[it_id]]) == 0,
                                                                   length(private$inserted_unfinished_events[[it_id]]) == 0)
                                                
                                                private$print_info(paste0("    Processing iterator with ID: ", it_id,"\n",
                                                                          "      Iterator information:\n",
                                                                          undec_it$toString("        ")))

                                                # Compute only once
                                                # if(is.null(est_latest_global_non_inv_event_start)){
                                                #   est_latest_global_non_inv_event_start <- private$get_est_latest_global_non_inv_event_start()
                                                #   
                                                #   if(is.null(est_latest_global_non_inv_event_start)){
                                                #     private$print_info(paste0("      Estimation of latest global non invisible event ",
                                                #                               "start time resulted in NULL, setting estimation to zero."))
                                                #     est_latest_global_non_inv_event_start <- 0
                                                #   }
                                                #   else{
                                                #     private$print_info(paste0("      Estimation of latest global non invisible event ",
                                                #                               "start time resulted in: ", est_latest_global_non_inv_event_start))
                                                #   }
                                                # }
                                                #' est_latest_global_non_inv_event_start should be non-null at this point
                                                #' because there should be at least one non-invisible event in 'inserted_unfinished_events' at this point
                                                #' (Reason: refresh() is only called if iterators empty -> at least one new non-invisibl 
                                                #' event should have been inserted at this point)
                                                # l$s$err$assert_msg("Estimation of 'est_latest_global_non_inv_event_start' should always succeed at this point.",
                                                #                    l$s$tcs$has_length_1(est_latest_global_non_inv_event_start, NA_on_fail),
                                                #                    l$s$tcs$is_numeric(est_latest_global_non_inv_event_start,
                                                #                                       accept_NULL = FALSE,
                                                #                                       accept_NaN = FALSE,
                                                #                                       accept_NA = FALSE,
                                                #                                       lower_bound = 0,
                                                #                                       lower_bound_inclusive = TRUE,
                                                #                                       upper_bound = Inf,
                                                #                                       upper_bound_inclusive = FALSE,
                                                #                                       accept_non_integer = TRUE))
                                                
                                                # est_earliest_start_time <- est_latest_global_non_inv_event_start
                                                # delay_it <- private$delay_dec_its[[it_id]]
                                                # next_t <- delay_it$get_next_time_available()
                                                # 
                                                # mean_delta_t <- delay_it$get_delay_mean()
                                                # min_delta_t <- delay_it$get_delay_min()
                                                # 
                                                # duration <- max(0, est_earliest_start_time - next_t)
                                                # 
                                                # private$print_info(paste0("      Employed earliest event time start: ", est_earliest_start_time,"\n",
                                                #                           "      Next timestamp of internal iterator: ", next_t, "\n",
                                                #                           "      NA event duration without zero cropping: ", est_earliest_start_time - next_t, "\n",
                                                #                           "      NA event duration with zero cropping: ", duration))
                                                # 
                                                # min_delta_t should be guaranteed to be >= 1 here - see create_iterators()
                                                # l$s$err$assert_msg("'min_delta_t' should be >= 1 at this point.",
                                                #                    min_delta_t >= 1)
                                                # num_elems <- 1 + (if(mean_delta_t > min_delta_t) ceiling(duration/mean_delta_t) 
                                                #                   else ceiling(duration/min_delta_t))
                                                # l$s$err$assert_msg("'num_elems' should be an integer >= 1 at this point.",
                                                #                    l$s$tcs$has_length_1(num_elems, NA_on_fail = FALSE),
                                                #                    l$s$tcs$is_integer(num_elems,
                                                #                                       accept_NULL = FALSE,
                                                #                                       accept_NaN = FALSE,
                                                #                                       accept_NA = FALSE,
                                                #                                       lower_bound = 1,
                                                #                                       lower_bound_inclusive = TRUE,
                                                #                                       upper_bound = Inf,
                                                #                                       upper_bound_inclusive = FALSE))
                                                # 
                                                #est_NA_elem_num <- min(max(private$min_num_NA_padding_elements, num_elems), private$max_num_NA_padding_elements)
                                                
                                                #' TODO: Better length estimation of padding event
                                                NA_padding_event <- private$get_NA_event(i = it_id, padding = TRUE, invisible = TRUE)
                                                
                                                #NA_padding_event <- private$get_NA_event(i = it_id, fixed_length = est_NA_elem_num, invisible = TRUE)
                                                # Also add short intra NA sequence to padding -> adds to random asynchronicity
                                                #intra_NA_event <- private$get_NA_event(i = it_id, intra = TRUE, invisible = TRUE)
                                                
                                                private$print_info(paste0(#"      Number of NA elements: ", num_elems, "\n",
                                                                          #"      Min-max bounded number of NA elements: ", est_NA_elem_num, "\n",
                                                                          "      Adding padding event to pending_immediate_insertions[[", it_id, "]]\n",
                                                                          NA_padding_event$toString("        "), "\n"))#,
                                                                          
                                                                          #"      Also adding intra NA event:\n",
                                                                          #intra_NA_event$toString("        ")))
                                                
                                                private$pending_immediate_insertions[[it_id]] <-
                                                  #append(private$pending_immediate_insertions[[it_id]], list(intra_NA_event, NA_padding_event))
                                                  append(private$pending_immediate_insertions[[it_id]], list(NA_padding_event))
                                                
                                                result <- TRUE
                                                
                                              }
                                              private$assert_consistency()
                                              
                                              private$print_info("  'pad_with_NA_events()': FINISHED")
                                              return(result)
                                            },
                                            
                                            refresh_inserted_unfinished_events = function(){
                                              #' Guaranteed to only be called if get_num_iterators() > 0
                                              private$print_info("  'refresh_inserted_unfinished_events()': START")
                                              
                                              private$assert_consistency()
                                              
                                              num_its <- self$get_num_iterators()
                                              num_num_its <- self$get_num_numeric_iterators()
                                              num_char_its <- self$get_num_character_iterators()
                                              # 1. Append all event's iterators in 'pending_immediate_insertions'
                                              #    to corresponding internal concatenated iterator, if latter is empty
                                              # 2. Move events from 'pending_immediate_insertions' to 'inserted_unfinished_events'
                                              # 3. Push information to both 'annotation_information_ground_truth' and 'annotation_information_insertion_events'
                                              # 4. Update event's set_inserted_unfinished_events_index().
                                              for(it_id in 1:num_its){
                                                undec_it <- self$.get_undecorated_iterator(it_id)
                                                total_num_rem <- undec_it$get_current_num_remaining()
                                                append_events <- list()
                                                
                                                num_immediate_insertions <- length(private$pending_immediate_insertions[[it_id]])
                                                private$print_info(paste0("    Processing iterator with ID: ", it_id,"\n",
                                                                          "      Iterator information:\n",
                                                                          undec_it$toString("        "),"\n",
                                                                          "      Length of pending_immediate_insertions[[", it_id,"]]: ", num_immediate_insertions))
                                                
                                                if(num_immediate_insertions == 0){ # Nothing to do here
                                                  private$print_info(paste0("      Zero length. Proceeding to next iterator id."))
                                                  next
                                                }
                                                
                                                first_na_rnd_walk <- private$pending_immediate_insertions[[it_id]][[1]]$get_sequence_name() %in% private$NA_rnd_walk_names
                                                if(num_immediate_insertions > 0 && (!first_na_rnd_walk || total_num_rem == 0)){
                                                  append_events <- append(append_events, private$pending_immediate_insertions[[it_id]][[1]])
                                                  if(num_immediate_insertions > 1){
                                                    for(event_index in 2:num_immediate_insertions){
                                                      event <- private$pending_immediate_insertions[[it_id]][[event_index]]
                                                      if(!(event$get_sequence_name() %in% private$NA_rnd_walk_names)){
                                                        append_events <- append(append_events, event)
                                                      }
                                                      else {
                                                        break
                                                      }
                                                    }
                                                  }
                                                }
                                                
                                                num_append_events <- length(append_events)
                                                private$print_info(paste0("      Appending ", num_append_events, " events."))
                                                
                                                if(num_append_events > 0){
                                                  l$s$err$assert_msg("Inconsistent state.",
                                                                     all(sapply(1:num_append_events,
                                                                                FUN = function(event_id){
                                                                                  return(identical(private$pending_immediate_insertions[[it_id]][[event_id]],
                                                                                                   append_events[[event_id]]))
                                                                                })))
                                                  for(append_event_index in 1:num_append_events){
                                                    private$print_info(paste0("      Processing and appending event pending_immediate_insertions[[", it_id, "]][[", append_event_index, "]]:\n",
                                                                       private$pending_immediate_insertions[[it_id]][[append_event_index]]$toString(tab = "        ")))
                                                    event <- private$pending_immediate_insertions[[it_id]][[append_event_index]]
                                                    
                                                    event_id <- event$get_iterator_id()
                                                    event_name <- event$get_sequence_name()
                                                    if(l$s$err$get_assertions_status()){
                                                      next_t <- private$delay_dec_its[[it_id]]$get_next_time_available()
                                                      l$s$err$assert_msg("Inconsistent_state.",
                                                                         event_id == it_id,
                                                                         event$is_immediate_insertion_possible(next_t))
                                                    }
                                                    
                                                    additional_params <- list()
                                                    is_NA_rnd_walk <- event_name %in% private$NA_rnd_walk_names
                                                    is_NA_char_seq <- event_name %in% private$NA_character_sequence_names
                                                    is_char_seq <- l$s$dts$equals(event_name, private$character_sequence_name)
                                                    is_regular_num <- event_name %in% private$non_special_event_names
                                                    
                                                    l$s$err$assert_msg("Inconsistent state.",
                                                                       length(which(c(is_NA_rnd_walk, 
                                                                                      is_NA_char_seq, 
                                                                                      is_char_seq, 
                                                                                      is_regular_num))) == 1,
                                                                       num_its == num_num_its + num_char_its,
                                                                       l$s$mg$`%then%`(is_char_seq || is_NA_char_seq, it_id > num_num_its),
                                                                       l$s$mg$`%then%`(is_char_seq || is_NA_char_seq, it_id <= num_its),
                                                                       l$s$mg$`%then%`(is_regular_num || is_NA_rnd_walk, it_id <= num_num_its))
                                                    
                                                    if(is_NA_rnd_walk || is_NA_char_seq){
                                                      
                                                      if(is_NA_rnd_walk){
                                                        #' If appended event is NA rnd walk, it must be the first one 
                                                        #' and the total number of remainig elements must be zero
                                                        l$s$err$assert_msg("Inconsistent state.",
                                                                           append_event_index == 1,
                                                                           total_num_rem == 0,
                                                                           it_id <= self$get_num_numeric_iterators())
                                                        
                                                        offset <- undec_it$get_initial_offset()
                                                        if(is.null(offset)){
                                                          offset <- 0
                                                        }
                                                        if(undec_it$get_buffer_length() > 0) {
                                                          initial_value <- undec_it$get_buffer_element(1) - offset# - undec_it$get_current_offset()
                                                        }
                                                        else {
                                                          initial_value <- 0#private$initial_value[[it_id]]
                                                        }
                                                        l$s$err$assert_msg("'initial_value' should be a finite numeric of length one.",
                                                                           l$s$tcs$has_length_1(initial_value, NA_on_fail),
                                                                           l$s$tcs$is_numeric(initial_value,
                                                                                              accept_NULL = FALSE,
                                                                                              accept_NaN = FALSE,
                                                                                              accept_NA = FALSE,
                                                                                              lower_bound = -Inf,
                                                                                              lower_bound_inclusive = FALSE,
                                                                                              upper_bound = Inf,
                                                                                              upper_bound_inclusive = FALSE,
                                                                                              accept_non_integer = TRUE))
                                                        
                                                        additional_params[[l$required_param_initial_value]] <- initial_value
                                                      }
                                                      var_label_name <- ls(envir = private)[
                                                        sapply(ls(envir = private), 
                                                                     FUN = function(var_name){
                                                                       return(l$s$dts$equals(get(x=var_name, envir = private), event_name) &&
                                                                                grepl("_name$", x = var_name))
                                                                     })]
                                                      
                                                      l$s$err$assert_msg("Inconsistent state.",
                                                                         l$s$tcs$has_length_1(var_label_name, NA_on_fail = FALSE),
                                                                         grepl("_name$", x = var_label_name))
                                                      
                                                      var_label_name <- gsub(pattern = "_name$", replacement = "_label", x = var_label_name)
                                                      label_name <- get(x = var_label_name, envir = private)
                                                    }
                                                    else if(is_char_seq){
                                                      label_name <- event$get_sequence_params()[[l$required_param_element]]
                                                      l$s$err$assert_msg("'label_name' should not be a character vector of length one at this point.",
                                                                         l$s$tcs$has_length_1(label_name, NA_on_fail = FALSE),
                                                                         l$s$tcs$is_character(label_name,
                                                                                              accept_NULL = FALSE,
                                                                                              accept_NaN = FALSE,
                                                                                              accept_NA = FALSE))
                                                    }
                                                    else if(is_regular_num){
                                                      label_name <- event_name 
                                                    }
                                                    else {
                                                      l$s$err$assert_msg("Should not be reached.",
                                                                         FALSE)
                                                    }
                                                    params <- list()
                                                    params[["factory"]] <- private$synth_ds_it_factory
                                                    params <- c(params, additional_params)
                                                    iterator <- do.call(what = event$create_iterator,
                                                                        args = params)
                                                    
                                                    len <- iterator$get_length()
                                                    l$s$err$assert_msg("Event produced a subsequence of length zero!",
                                                                       len > 0)
                                                    private$print_info(paste0("        Append annotation information:\n",
                                                                              "          Ground truth: sequence name = \"",label_name,"\", ",
                                                                              "length = ",len,"\n",
                                                                              "          Insertion events: sequence name = \"", event_name,"\", ",
                                                                              "length = ",len))
                                                    private$annotation_information_ground_truth[[it_id]]$pushback(sequence_name = label_name,
                                                                                            length = len)
                                                    private$annotation_information_insertion_events[[it_id]]$pushback(sequence_name = event_name,
                                                                                                    length = len)
                                                    
                                                    if(it_id <= num_num_its){
                                                      if(is_regular_num){
                                                        normalize_offset <- TRUE
                                                        additional_offset <- NULL
                                                      }
                                                      else if(is_NA_rnd_walk){
                                                        normalize_offset <- FALSE
                                                        additional_offset <- offset
                                                      }
                                                      else{
                                                        l$s$err$assert_msg("Inconsistent state.",
                                                                           FALSE)
                                                      }
                                                      
                                                      private$print_info(paste0("        Appending numeric iterator:\n",
                                                                                iterator$toString(tab = "          ")))
                                                      undec_it$append_iterator(iterator, 
                                                                               normalize_offset = normalize_offset,
                                                                               additional_offset = additional_offset)
                                                    }
                                                    else{
                                                      private$print_info(paste0("        Appending character iterator:\n",
                                                                                iterator$toString(tab = "          ")))
                                                      undec_it$append_iterator(iterator)
                                                    }
                                                   
                                                    private$inserted_unfinished_events[[it_id]] <- 
                                                      append(private$inserted_unfinished_events[[it_id]],
                                                             event)
                                                    event$set_inserted_unfinished_events_index(length(private$inserted_unfinished_events[[it_id]]))
                                                    private$print_info(paste0("        Moving corresponding event to inserted_unfinished_events[[", it_id, "]][[",
                                                                              event$get_inserted_unfinished_events_index(),"]]"))
                                                    l$s$err$assert_msg("Inconsistent state.",
                                                                       event$get_inserted_unfinished_events_index() == length(private$inserted_unfinished_events[[it_id]]),
                                                                       identical(private$inserted_unfinished_events[[it_id]][[event$get_inserted_unfinished_events_index()]],
                                                                                 event))
                                                  }
                                                  
                                                  private$print_info(paste0("      Removing first ",num_append_events, " entries from pending_immediate_insertions[[", it_id, "]]"))
                                                  
                                                  private$pending_immediate_insertions[[it_id]][1:num_append_events] <- NULL
                                                  l$s$err$assert_msg("Inconsistent state.",
                                                                     length(private$pending_immediate_insertions[[it_id]]) == 
                                                                       num_immediate_insertions - num_append_events,
                                                                     num_append_events > 0)
                                                }
                                                
                                              }
                                              
                                              private$assert_consistency()
                                              private$print_info("  'refresh_inserted_unfinished_events()': FINISHED")
                                              
                                            },
                                            
                                            refresh = function(){
                                              if(!is.null(private$refresh_rnd_seed)){
                                                glob_env <- globalenv()
                                                old_rnd_state <- get(x = ".Random.seed", envir = glob_env) 
                                                if(is.null(private$last_refresh_rnd_state)){
                                                  set.seed(private$refresh_rnd_seed)
                                                }
                                                else{
                                                  assign(".Random.seed", value = private$last_refresh_rnd_state, envir = glob_env)
                                                }
                                              }
                                              
                                              private$print_info("'refresh()': Start")
                                              
                                              num_its <- self$get_num_iterators()
                                              
                                              
                                              if(num_its > 0){
                                                private$refresh_pending_insertions()
                                                private$refresh_pending_immediate_insertions()
                                                private$refresh_inserted_unfinished_events()
                                                na_padding_happened <- private$pad_with_NA_events()
                                                if(na_padding_happened){
                                                  private$refresh_inserted_unfinished_events()
                                                }
                                              }
                                              private$print_info("'refresh()': Finished")
                                              
                                              if(!is.null(private$refresh_rnd_seed)){
                                                private$last_refresh_rnd_state <- get(x = ".Random.seed", envir = glob_env) 
                                                assign(".Random.seed", value = old_rnd_state, envir = glob_env)
                                              }
                                            },
                                            
                                            print_info = function(msg){
                                              if(private$verbose){
                                                cat(paste0(msg, "\n"))
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
                                            
                                            #' Note: Delay timers start immediatelly
                                            #' Note: Two instances of synthetic data stream iterators that were initialized
                                            #'       with the same arguments only produce the exact same results if:
                                            #'       * Initial .Random.seed is identical
                                            #'       * Order and arguments of calls to get_next and get_next_count is identical
                                            #' @export
                                            #'
                                            #' @examples
                                            #' @md
                                            initialize = function(assertions_status = FALSE,
                                                                  synthetic_datastream_iterator_factory,
                                                                  numeric_iterators_num,
                                                                  character_iterators_num,
                                                                  individual_subparams = FALSE,
                                                                  noise_decorator_params,
                                                                  delay_decorator_params,
                                                                  num_concat_iterator_params,
                                                                  # TODO: implement option later: append_ground_truth = TRUE,
                                                                  ground_truth_name = "label",
                                                                  idle_event_name = "idle",
                                                                  get_next_event_func,
                                                                  NA_label = "NA",
                                                                  #min_num_NA_padding_elements = 1,
                                                                  #max_num_NA_padding_elements = 1000,
                                                                  .debug_mode = assertions_status,
                                                                  .verbose = FALSE,
                                                                  output_as_frame = TRUE,
                                                                  .NA_character_sequence_label = "NA_char",
                                                                  .padding_NA_character_sequence_label = "padding_NA_char",
                                                                  .inter_NA_character_sequence_label = "inter_NA_char",
                                                                  .intra_NA_character_sequence_label = "intra_NA_char",
                                                                  .NA_rnd_walk_label = "NA_rnd_walk",
                                                                  .padding_NA_rnd_walk_label = "padding_NA_rnd_walk",
                                                                  .inter_NA_rnd_walk_label = "inter_NA_rnd_walk",
                                                                  .intra_NA_rnd_walk_label = "intra_NA_rnd_walk",
                                                                  rnd_seed = NULL){
                                              
                                              super$initialize(assertions_status = assertions_status,
                                                               iterators = private$pre_init(
                                                                 assertions_status = assertions_status,
                                                                 numeric_iterators_num = numeric_iterators_num,
                                                                 character_iterators_num = character_iterators_num,
                                                                 individual_subparams = individual_subparams,
                                                                 noise_decorator_params = noise_decorator_params,
                                                                 delay_decorator_params = delay_decorator_params,
                                                                 num_concat_iterator_params = num_concat_iterator_params,
                                                                 #append_ground_truth = append_ground_truth,
                                                                 ground_truth_name = ground_truth_name,
                                                                 rnd_seed = rnd_seed))
                                              
                                              local_env$.add_to_static_env(super$get_static_env())
                                              local_env$static_env$err$set_assertions_status(assertions_status)
                                              
                                              if(!is.null(private$rnd_seed)){
                                                glob_env <- globalenv()
                                                old_rnd_state <- get(x = ".Random.seed", envir = glob_env) 
                                                
                                                l$s$err$assert_msg("Inconsistent state.",
                                                                   !is.null(private$last_rnd_state))
                                                assign(".Random.seed", value = private$last_rnd_state, envir = glob_env)
                                              }
                                              
                                              #' Add your initialization code here here:
                                              if(l$s$err$get_assertions_status()){
                                                l$s$err$assert_msg("'synthetic_datastream_iterator_factory' must inherit from 'Synthetic_Datastream_Iterator_Factory'.",
                                                                   is.R6(synthetic_datastream_iterator_factory),
                                                                   inherits(synthetic_datastream_iterator_factory, "Synthetic_Datastream_Iterator_Factory"))
                                                
                                                
                                                l$s$err$assert_msg("'.debug_mode' must be a logical of length one.",
                                                                   l$s$tcs$has_length_1(.debug_mode, NA_on_fail = FALSE),
                                                                   l$s$tcs$is_logical(.debug_mode,
                                                                                      accept_NULL = FALSE,
                                                                                      accept_NaN = FALSE,
                                                                                      accept_NA = FALSE))
                                                
                                                l$s$err$assert_msg("'.verbose' must be a logical of length one.",
                                                                   l$s$tcs$has_length_1(.verbose, NA_on_fail = FALSE),
                                                                   l$s$tcs$is_logical(.verbose,
                                                                                      accept_NULL = FALSE,
                                                                                      accept_NaN = FALSE,
                                                                                      accept_NA = FALSE))
                                                
                                                l$s$err$assert_msg("'output_as_frame' must be a logical of length one.",
                                                                   l$s$tcs$has_length_1(output_as_frame, NA_on_fail = FALSE),
                                                                   l$s$tcs$is_logical(output_as_frame,
                                                                                      accept_NULL = FALSE,
                                                                                      accept_NaN = FALSE,
                                                                                      accept_NA = FALSE))
                                                
                                                l$s$err$assert_msg("'idle_event_name' must be a character vector of length one.",
                                                                   l$s$tcs$has_length_1(idle_event_name, NA_on_fail = FALSE),
                                                                   l$s$tcs$is_character(idle_event_name,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE))
                                                l$s$err$assert_msg("'get_next_event_func' must be a function.",
                                                                   is.function(get_next_event_func))
                                                
                                                l$s$err$assert_msg("'NA_label' must be a character vector of length one.",
                                                                   l$s$tcs$has_length_1(NA_label, NA_on_fail = FALSE),
                                                                   l$s$tcs$is_character(NA_label,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE))
                                                
                                                # l$s$err$assert_msg("'min_num_NA_padding_elements' must be an integer of length one and > 0.",
                                                #                    l$s$tcs$has_length_1(min_num_NA_padding_elements, NA_on_fail = FALSE),
                                                #                    l$s$tcs$is_integer(min_num_NA_padding_elements,
                                                #                                       accept_NULL = FALSE,
                                                #                                       accept_NaN = FALSE,
                                                #                                       accept_NA = FALSE,
                                                #                                       lower_bound = 1,
                                                #                                       lower_bound_inclusive = TRUE,
                                                #                                       upper_bound = Inf,
                                                #                                       upper_bound_inclusive = FALSE))
                                                # 
                                                # l$s$err$assert_msg("'max_num_NA_padding_elements' must be an integer of length one and >= min_num_NA_padding_elements.",
                                                #                    l$s$tcs$has_length_1(max_num_NA_padding_elements, NA_on_fail = FALSE),
                                                #                    l$s$tcs$is_integer(max_num_NA_padding_elements,
                                                #                                       accept_NULL = FALSE,
                                                #                                       accept_NaN = FALSE,
                                                #                                       accept_NA = FALSE,
                                                #                                       lower_bound = min_num_NA_padding_elements,
                                                #                                       lower_bound_inclusive = TRUE,
                                                #                                       upper_bound = Inf,
                                                #                                       upper_bound_inclusive = FALSE))
                                                
                                                l$s$err$assert_msg("'.NA_character_sequence_label' must be a character vector of length one.",
                                                                   l$s$tcs$has_length_1(.NA_character_sequence_label, NA_on_fail = FALSE),
                                                                   l$s$tcs$is_character(.NA_character_sequence_label,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE))
                                                l$s$err$assert_msg("'.padding_NA_character_sequence_label' must be a character vector of length one.",
                                                                   l$s$tcs$has_length_1(.padding_NA_character_sequence_label, NA_on_fail = FALSE),
                                                                   l$s$tcs$is_character(.padding_NA_character_sequence_label,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE))
                                                l$s$err$assert_msg("'.inter_NA_character_sequence_label' must be a character vector of length one.",
                                                                  l$s$tcs$has_length_1(.inter_NA_character_sequence_label, NA_on_fail = FALSE),
                                                                  l$s$tcs$is_character(.inter_NA_character_sequence_label,
                                                                                       accept_NULL = FALSE,
                                                                                       accept_NaN = FALSE,
                                                                                       accept_NA = FALSE))
                                                l$s$err$assert_msg("'.intra_NA_character_sequence_label' must be a character vector of length one.",
                                                                  l$s$tcs$has_length_1(.intra_NA_character_sequence_label, NA_on_fail = FALSE),
                                                                  l$s$tcs$is_character(.intra_NA_character_sequence_label,
                                                                                       accept_NULL = FALSE,
                                                                                       accept_NaN = FALSE,
                                                                                       accept_NA = FALSE))
                                                l$s$err$assert_msg("'.NA_rnd_walk_label' must be a character vector of length one.",
                                                                   l$s$tcs$has_length_1(.NA_rnd_walk_label, NA_on_fail = FALSE),
                                                                   l$s$tcs$is_character(.NA_rnd_walk_label,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE))
                                                l$s$err$assert_msg("'.padding_NA_rnd_walk_label' must be a character vector of length one.",
                                                                   l$s$tcs$has_length_1(.padding_NA_rnd_walk_label, NA_on_fail = FALSE),
                                                                   l$s$tcs$is_character(.padding_NA_rnd_walk_label,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE))
                                                l$s$err$assert_msg("'.inter_NA_rnd_walk_label' must be a character vector of length one.",
                                                                  l$s$tcs$has_length_1(.inter_NA_rnd_walk_label, NA_on_fail = FALSE),
                                                                  l$s$tcs$is_character(.inter_NA_rnd_walk_label,
                                                                                       accept_NULL = FALSE,
                                                                                       accept_NaN = FALSE,
                                                                                       accept_NA = FALSE))
                                                l$s$err$assert_msg("'.intra_NA_rnd_walk_label' must be a character vector of length one.",
                                                                  l$s$tcs$has_length_1(.intra_NA_rnd_walk_label, NA_on_fail = FALSE),
                                                                  l$s$tcs$is_character(.intra_NA_rnd_walk_label,
                                                                                       accept_NULL = FALSE,
                                                                                       accept_NaN = FALSE,
                                                                                       accept_NA = FALSE))
                                              }
                                              
                                              total_it_num <- numeric_iterators_num + character_iterators_num
                                              private$last_immediate_insertion_was_inter <- rep(x = TRUE, times = total_it_num)
                                              
                                              if(total_it_num == 0){
                                                private$get_next_rnd_seeds <- list()
                                                private$last_get_next_rnd_states <- list
                                                private$get_next_count_rnd_seeds <- list()
                                                private$last_get_next_count_rnd_states <- list()
                                              }
                                              else {
                                                null_list <- lapply(1:total_it_num,
                                                                    FUN = function(i){
                                                                      return(NULL)
                                                                    })
                                                private$last_get_next_rnd_states <- null_list
                                                private$last_get_next_count_rnd_states <- null_list
                                                if(!is.null(rnd_seed)){
                                                  private$refresh_rnd_seed <- sample(.Machine$integer.max, size = 1)
                                                  
                                                  private$get_next_rnd_seeds <- lapply(1:total_it_num,
                                                                                       FUN = function(i){
                                                                                         return(sample(.Machine$integer.max, size = 1))
                                                                                       })
                                                  private$get_next_count_rnd_seeds <- lapply(1:total_it_num,
                                                                                             FUN = function(i){
                                                                                               return(sample(.Machine$integer.max, size = 1))
                                                                                             })
                                                  }
                                                else{
                                                  private$get_next_rnd_seeds <- null_list
                                                  private$get_next_count_rnd_seeds <- null_list
                                                }
                                              }
                                              
                                              ll <-  if(total_it_num == 0) list() else lapply(1:total_it_num, FUN = function(i){return(list())})
                                              
                                              private$pending_insertions <- ll
                                              private$pending_immediate_insertions <- ll
                                              private$inserted_unfinished_events <- ll
                                              
                                              if(total_it_num > 0){
                                                private$annotation_information_ground_truth <- 
                                                  lapply(1:total_it_num,
                                                         FUN = function(i){
                                                           return(l$result_env$.Annotation_Information$new(assertions_status = assertions_status))
                                                         })
                                                private$annotation_information_insertion_events <- 
                                                  lapply(1:total_it_num,
                                                         FUN = function(i){
                                                           return(l$result_env$.Annotation_Information$new(assertions_status = assertions_status))
                                                         })
                                              }
                                              else{
                                                private$annotation_information_ground_truth <- list()
                                                private$annotation_information_insertion_events <- list()
                                              }
                                              private$assertions_status <- assertions_status
                                              
                                              private$NA_label <- NA_label
                                              if(.debug_mode){
                                                private$NA_character_sequence_label <- .NA_character_sequence_label
                                                private$padding_NA_character_sequence_label <- .padding_NA_character_sequence_label
                                                private$inter_NA_character_sequence_label <- .inter_NA_character_sequence_label
                                                private$intra_NA_character_sequence_label <- .intra_NA_character_sequence_label
                                                private$NA_rnd_walk_label <- .NA_rnd_walk_label
                                                private$padding_NA_rnd_walk_label <- .padding_NA_rnd_walk_label
                                                private$inter_NA_rnd_walk_label <- .inter_NA_rnd_walk_label
                                                private$intra_NA_rnd_walk_label <- .intra_NA_rnd_walk_label
                                              }
                                              else{
                                                private$NA_character_sequence_label <- NA_label
                                                private$padding_NA_character_sequence_label <- NA_label
                                                private$inter_NA_character_sequence_label <- NA_label
                                                private$intra_NA_character_sequence_label <- NA_label
                                                private$NA_rnd_walk_label <- NA_label
                                                private$padding_NA_rnd_walk_label <- NA_label
                                                private$inter_NA_rnd_walk_label <- NA_label
                                                private$intra_NA_rnd_walk_label <- NA_label
                                              }
                                              
                                              private$synth_ds_it_factory <- synthetic_datastream_iterator_factory
                                              private$idle_event_name <- idle_event_name
                                              private$get_next_event_func <- get_next_event_func
                                              
                                              #private$min_num_NA_padding_elements <- min_num_NA_padding_elements
                                              #private$max_num_NA_padding_elements <- max_num_NA_padding_elements
                                              
                                              private$non_special_event_names <- 
                                                synthetic_datastream_iterator_factory$get_non_special_event_names()
                                              # 'get_next_event_func' should not provide assertions_status parameter -
                                              # the latter is set by this synthetic datastream interators instance.
                                              private$getter_req_params_non_special_event_names <- 
                                                setdiff(synthetic_datastream_iterator_factory$get_req_params_non_special_event_names(),
                                                        l$required_param_assertions_status)
                                              
                                              private$NA_rnd_walk_name <- 
                                                synthetic_datastream_iterator_factory$get_NA_rnd_walk_name()
                                              private$padding_NA_rnd_walk_name <- 
                                                synthetic_datastream_iterator_factory$get_padding_NA_rnd_walk_name()
                                              private$intra_NA_rnd_walk_name <- 
                                                synthetic_datastream_iterator_factory$get_intra_NA_rnd_walk_name()
                                              private$inter_NA_rnd_walk_name <- 
                                                synthetic_datastream_iterator_factory$get_inter_NA_rnd_walk_name()
                                              private$NA_rnd_walk_names <- 
                                                synthetic_datastream_iterator_factory$get_NA_rnd_walk_names()
                                              
                                              private$character_sequence_name <- 
                                                synthetic_datastream_iterator_factory$get_character_sequence_name()
                                              private$NA_character_sequence_name <- 
                                                synthetic_datastream_iterator_factory$get_NA_character_sequence_name()
                                              private$padding_NA_character_sequence_name <- 
                                                synthetic_datastream_iterator_factory$get_padding_NA_character_sequence_name()
                                              private$intra_NA_character_sequence_name <- 
                                                synthetic_datastream_iterator_factory$get_intra_NA_character_sequence_name()
                                              private$inter_NA_character_sequence_name <- 
                                                synthetic_datastream_iterator_factory$get_inter_NA_character_sequence_name()
                                              private$NA_character_sequence_names <- 
                                                synthetic_datastream_iterator_factory$get_NA_character_sequence_names()
                                              
                                              #' Same as for getter_req_params_non_special_event_names
                                              private$getter_req_params_character_sequence <- 
                                                setdiff(synthetic_datastream_iterator_factory$get_req_params_character_sequence(),
                                                        l$required_param_assertions_status)
                                              
                                              private$debug_mode <- .debug_mode
                                              private$output_as_frame <- output_as_frame
                                              private$verbose <- .verbose
                                              
                                              num_its <- self$get_num_iterators()
                                              if(num_its > 0){
                                                #' Start timers
                                                for(i in 1:num_its){
                                                  self$.get_delay_decorated_iterator(i)$get_next_count()
                                                }
                                              }
                                              
                                              private$assert_init_consistency()
                                              private$assert_consistency()
                                              
                                              if(!is.null(private$rnd_seed)){
                                                private$last_rnd_state <- get(x = ".Random.seed", envir = glob_env) 
                                                assign(".Random.seed", value = old_rnd_state, envir = glob_env)
                                              }
                                            },
                                            
                                            .get_undecorated_iterator = function(i){
                                              self$.check_index(i)
                                              len_num <- length(private$undec_num_its)
                                              if(i > len_num){
                                                return(private$undec_char_its[[i - len_num]])
                                              }
                                              else{
                                                return(private$undec_num_its[[i]])
                                              }
                                            },
                                            
                                            .get_delay_decorated_iterator = function(i){
                                              self$.check_index(i)
                                              return(private$delay_dec_its[[i]])
                                            },
                                            
                                            .get_non_NA_label_object_types = function(){
                                              return(private$non_NA_label_event_names)
                                            },
                                            
                                            get_next_count = function(i){
                                              if(!is.null(private$get_next_count_rnd_seeds[[i]])){
                                                glob_env <- globalenv()
                                                old_rnd_state <- get(x = ".Random.seed", envir = glob_env) 
                                                if(is.null(private$last_get_next_count_rnd_states[[i]])){
                                                  set.seed(private$get_next_count_rnd_seeds[[i]])
                                                }
                                                else{
                                                  assign(".Random.seed", value = private$last_get_next_count_rnd_states[[i]], envir = glob_env)
                                                }
                                              }
                                              
                                              private$print_info(paste0("'get_next_count(", i, ")': START"))
                                              self$.check_index(i)
                                              
                                              private$assert_consistency()
                                              
                                              num_its <- self$get_num_iterators()
                                              result <- super$get_next_count(i)
                                              
                                              private$print_info(paste0("  Initial result: ", result))
                                              if(num_its == 0 || result > 0){
                                                l$s$err$assert_msg("Inconsistent state.",
                                                                   l$s$mg$`%then%`(num_its == 0, result == 0))
                                                
                                                private$print_info(paste0(
                                                  "  Iterator is non-empty. Returning.\n",
                                                  "'get_next_count(", i, ")': FINISHED"))
                                              }
                                              else{
                                                all_empty <- all(sapply(1:num_its,
                                                                        FUN = function(it_id){
                                                                          return(length(private$inserted_unfinished_events[[it_id]]) == 0)
                                                                        }))
                                                #' The condition of all iterators being empty ensures that the produces data steam is
                                                #' independent of the order and time at which get_next_count is called. 
                                                if(all_empty){
                                                  private$print_info("  All iterators are empty. Calling refresh().")
                                                  
                                                  l$s$err$assert_msg("All iterators should be empty at this point.",
                                                                     all(sapply(1:num_its,
                                                                                FUN = function(it_id){
                                                                                  return(self$.get_undecorated_iterator(it_id)$get_current_num_remaining() == 0)
                                                                                })))
                                                  
                                                  private$refresh()
                                                  result <- super$get_next_count(i)
                                                  
                                                  l$s$err$assert_msg("At least one iterator should be non-empty at this point.",
                                                                     any(sapply(1:num_its,
                                                                                FUN = function(it_id){
                                                                                  return(self$.get_undecorated_iterator(it_id)$get_current_num_remaining() > 0)
                                                                                })))
                                                  
                                                  #' Either: 
                                                  #' 1. This iterator has pending immediate insertion events: This should have been 
                                                  #'    inserted during 'refresh()'
                                                  #' 2. This iterator has pending insertion events: Then
                                                  #' 2.1 Dependencies are fulfilled: Then those insertions should have been performed 
                                                  #'     during 'refresh'
                                                  #' 2.2 Or there are unfulfilled dependencies: Then there is at least one other dependency,
                                                  #'     that has either not started yet (if corresponding append_at is "begin") or has not finished yet 
                                                  #'     (if append_at is "end").
                                                  #'     Then rnd walk is only inserted if next_t <= estimated starting time
                                                }else{
                                                  private$print_info("  At least one iterator is non-empty. Doing nothing.")
                                                  
                                                }
                                              }
                                              
                                              private$assert_consistency()
                                              
                                              private$print_info(paste0("  Final result: ", result))
                                              
                                              private$print_info(paste0("'get_next_count(", i, ")': FINISHED"))
                                              
                                              if(!is.null(private$get_next_count_rnd_seeds[[i]])){
                                                private$last_get_next_count_rnd_states[[i]] <- get(x = ".Random.seed", envir = glob_env) 
                                                assign(".Random.seed", value = old_rnd_state, envir = glob_env)
                                              }
                                              
                                              return(result)
                                            },
                                            
                                            get_next = function(i, num = self$get_next_count(i)){
                                              # Setting set_last_emitted_timestamp() of current inserted_unfinished_events insertion event and
                                              # set_end_timestamp() if applicable. Remove insertion event in the latter case.
                                              # Use annotation_information_insertion_events for the above and pop_silent() the appropriate number of
                                              # values.
                                              # Update event's set_inserted_unfinished_events_index()
                                              
                                              if(!is.null(private$get_next_rnd_seeds[[i]])){
                                                glob_env <- globalenv()
                                                old_rnd_state <- get(x = ".Random.seed", envir = glob_env) 
                                                if(is.null(private$last_get_next_rnd_states[[i]])){
                                                  set.seed(private$get_next_rnd_seeds[[i]])
                                                }
                                                else{
                                                  assign(".Random.seed", value = private$last_get_next_rnd_states[[i]], envir = glob_env)
                                                }
                                              }
                                              
                                              private$print_info(paste0("'get_next(i = ", i, ", num = ",num, ")': START"))
                                              self$.check_index(i)
                                              private$assert_consistency()
                                              
                                              l$s$err$assert_msg("Provided 'num' argument is larger than the result of 'get_next_count(i)'.",
                                                                 num <= self$get_next_count(i))
                                              
                                              result <- super$get_next(i = i, num = num)
                                              num_elems <- length(result)
                                              l$s$err$assert_msg("Inconsistent state.",
                                                                 num_elems == num)
                                              if(num_elems == 0){
                                                private$print_info(paste0("  'num' is zero, returning: ", toString(result)))
                                                private$print_info(paste0("'get_next(i = ", i, ", num = ",num, ")': FINISHED"))
                                                if(private$output_as_frame){
                                                  result <- as.data.frame(matrix(nrow = 0, ncol = 3))
                                                  names(result) <- NULL
                                                }
                                                else{
                                                  result <- list()
                                                }
                                              }
                                              else{
                                                private$print_info(paste0("  'num' > 0, updating internal lists."))
                                                annotation_ins <- private$annotation_information_insertion_events[[i]]
                                                
                                                nums_rem <- annotation_ins$get_nums_remaining()
                                                lengths <- annotation_ins$get_lengths()
                                                num_inserted_events <- length(nums_rem)
                                                l$s$err$assert_msg("Inconsistent state.",
                                                                   l$s$mg$`%then%`(num_elems > 0, num_inserted_events > 0),
                                                                   num_inserted_events == length(private$inserted_unfinished_events[[i]]),
                                                                   num_inserted_events == length(lengths))
                                                
                                                private$print_info(paste0("  Annotation information's nums remaining: ", toString(nums_rem), "\n",
                                                                          "  Annotation information's lengths: ", toString(lengths)))
                                                
                                                num_elems_per_event <- list()
                                                nums_unprocessed <- num_elems
                                                event_id <- 1
                                                while(nums_unprocessed > 0){
                                                  l$s$err$assert_msg("Inconsistent state.",
                                                                     event_id <= num_inserted_events)
                                                  curr_ev_retrieved_num <- min(nums_unprocessed, nums_rem[[event_id]])
                                                  l$s$err$assert_msg("Inconsistent state.",
                                                                     curr_ev_retrieved_num <= lengths[[event_id]])
                                                  
                                                  num_elems_per_event <- append(num_elems_per_event, curr_ev_retrieved_num)
                                                  nums_unprocessed <- nums_unprocessed - curr_ev_retrieved_num
                                                  event_id <- event_id + 1
                                                }
                                                relevant_event_num <- length(num_elems_per_event)
                                                l$s$err$assert_msg("Inconsistent state",
                                                                   nums_unprocessed == 0,
                                                                   event_id >= 2,
                                                                   relevant_event_num == event_id - 1,
                                                                   relevant_event_num > 0,
                                                                   all(sapply(1:relevant_event_num,
                                                                              FUN = function(i){
                                                                                if(i == relevant_event_num){
                                                                                  return(num_elems_per_event[[i]] <= nums_rem[[i]])
                                                                                }
                                                                                else{
                                                                                  return(num_elems_per_event[[i]] == nums_rem[[i]])
                                                                                }
                                                                              })),
                                                                   sum(as.numeric(num_elems_per_event)) == num_elems)
                                                private$print_info(paste0("  Number of elements per event: ", toString(num_elems_per_event)))
                                                curr_result_index <- 0
                                                num_removed_events <- 0
                                                nums_unprocessed <- num_elems
                                                for(event_id in 1:relevant_event_num){
                                                  event <- private$inserted_unfinished_events[[i]][[event_id]]
                                                  curr_ev_retrieved_num <- num_elems_per_event[[event_id]]
                                                  curr_result_index <- curr_result_index + 1
                                                  current_t <- result[[curr_result_index]][[2]][[1]] 
                                                  
                                                  private$print_info(paste0("  Processing event inserted_unfinished_events[[", i, "]][[", event_id, "]]:\n",
                                                                            "    Event Information:\n",
                                                                            event$toString("      ")))
                                                  
                                                  
                                                  l$s$err$assert_msg("'current_t' should be a numeric >= 0 of length one at this point.",
                                                                     l$s$tcs$has_length_1(current_t, NA_on_fail = FALSE),
                                                                     l$s$tcs$is_numeric(current_t,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE,
                                                                                        lower_bound = 0,
                                                                                        lower_bound_inclusive = TRUE,
                                                                                        upper_bound = Inf,
                                                                                        upper_bound_inclusive = FALSE,
                                                                                        accept_non_integer = TRUE))
                                                  # curr_result_index should point to first element of currently processed event
                                                  l$s$err$assert_msg("Inconsistent state.",
                                                                     curr_result_index == 1 + if(event_id == 1) 0 else sum(as.numeric(num_elems_per_event[1:(event_id - 1)])))
                                                  # First element of event retrieved during this call?
                                                  l$s$err$assert_msg("Inconsistent state",
                                                                     l$s$mg$`%eq%`(nums_rem[[event_id]] == lengths[[event_id]],
                                                                                   is.null(event$get_start_timestamp())))
                                                  event$set_last_emitted_timestamp(current_t)
                                                  l$s$err$assert_msg("Inconsistent state",
                                                                     !is.null(event$get_start_timestamp()))
                                                  # curr_result_index should point to last element of currently processed event
                                                  curr_result_index <- curr_result_index + (curr_ev_retrieved_num - 1)
                                                  current_t <- result[[curr_result_index]][[2]][[1]] 
                                                  l$s$err$assert_msg("'current_t' should be a numeric >= 0 of length one at this point.",
                                                                     l$s$tcs$has_length_1(current_t, NA_on_fail = FALSE),
                                                                     l$s$tcs$is_numeric(current_t,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE,
                                                                                        lower_bound = 0,
                                                                                        lower_bound_inclusive = TRUE,
                                                                                        upper_bound = Inf,
                                                                                        upper_bound_inclusive = FALSE,
                                                                                        accept_non_integer = TRUE))
                                                  
                                                  l$s$err$assert_msg("Inconsistent state.",
                                                                     curr_result_index == sum(as.numeric(num_elems_per_event[1:event_id])))
                                                  
                                                  
                                                  event$set_last_emitted_timestamp(current_t)
                                                  if(curr_ev_retrieved_num == nums_rem[[event_id]]){ # => All remaining event elements retrieved
                                                    event$set_end_timestamp()
                                                    event$set_inserted_unfinished_events_index(0)
                                                    num_removed_events <- num_removed_events + 1
                                                  }
                                                  nums_unprocessed <- nums_unprocessed - curr_ev_retrieved_num
                                                  annotation_ins$pop_silent(n = curr_ev_retrieved_num)
                                                  
                                                  private$print_info(paste0("    Post process event Information:\n",
                                                                            event$toString("      ")))
                                                }
                                                
                                                l$s$err$assert_msg("Inconsistent state.",
                                                                   num_removed_events == relevant_event_num ||
                                                                     num_removed_events == relevant_event_num - 1,
                                                                   nums_unprocessed == 0,
                                                                   sum(as.numeric(nums_rem)) - num_elems == sum(as.numeric(annotation_ins$get_nums_remaining())))
                                                
                                                if(num_removed_events > 0){
                                                  l$s$err$assert_msg("Inconsistent state.",
                                                                     num_removed_events <= length(private$inserted_unfinished_events[[i]]))
                                                  private$print_info(paste0("  Removing first ", num_removed_events, " events from private$inserted_unfinished_events[[", i, "]] ",
                                                                            "and updating remaining event's unfinished_insertions_index"))
                                                  
                                                  private$inserted_unfinished_events[[i]][1:num_removed_events] <- NULL
                                                  num_remaining_events <- length(private$inserted_unfinished_events[[i]])
                                                  if(num_remaining_events > 0){
                                                    for(event_id in 1:num_remaining_events){
                                                      l$s$err$assert_msg("Inconsistent state.",
                                                                         event_id + num_removed_events == 
                                                                           private$inserted_unfinished_events[[i]][[event_id]]$get_inserted_unfinished_events_index())
                                                      private$inserted_unfinished_events[[i]][[event_id]]$set_inserted_unfinished_events_index(event_id)
                                                    }
                                                  }
                                                }
                                                if(private$output_as_frame){
                                                  data_mode <- if(i <= self$get_num_numeric_iterators()) "numeric" else "character"
                                                  
                                                  result <- l$s$dts$as_data_frame(result,
                                                                                  nrows=length(result), 
                                                                                  ncols=3, 
                                                                                  rownames=NULL, 
                                                                                  colnames=c(names(result[[1]])[[1]], names(result[[1]][[2]])[[1]], names(result[[1]][[2]])[[2]]), 
                                                                                  modes = c("character", "numeric", data_mode), 
                                                                                  byrow = TRUE,
                                                                                  check_args = private$assertions_status,
                                                                                  stringsAsFactors = default.stringsAsFactors())
                                                }
                                              }
                                              private$assert_consistency()
                                              private$print_info(paste0("'get_next(i = ", i, ", num = ",num, ")': FINISHED"))
                                              
                                              if(!is.null(private$get_next_rnd_seeds[[i]])){
                                                private$last_get_next_rnd_states[[i]] <- get(x = ".Random.seed", envir = glob_env) 
                                                assign(".Random.seed", value = old_rnd_state, envir = glob_env)
                                              }
                                              
                                              return(result)
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
        
        Datastream_Visualization <- R6Class("Datastream_Visualization",
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
                                              numeric_iterators_num = NULL,
                                              character_iterators_num = NULL,
                                              iterators_num = NULL,
                                              
                                              first_plot_out_path = NULL,
                                              first_plot_start = NULL,
                                              
                                              NA_label = NULL,
                                              
                                              replot_min_interval = NULL, 
                                              x_axis_range = NULL,
                                              
                                              y_axis_labels = NULL,
                                              ground_truth_name = NULL,
                                              timestamp_name = NULL,
                                              
                                              frame_colnames = NULL,
                                              
                                              buffer_list = NULL,
                                              
                                              x_lim_min = NULL,
                                              x_lim_max = NULL,
                                              
                                              max_x_progress = NULL,
                                              
                                              max_ts = NULL,
                                              last_plot_t = NULL,
                                              
                                              all_dims_have_elems = NULL,
                                              graph_labels = NULL,
                                              
                                              plot_height = NULL,
                                              plot_width = NULL,
                                              plot_margins = c(3.5, 3.2, 2, 0.5),
                                              
                                              window_label = NULL,
                                              
                                              last_device = NULL,
                                              
                                              assertions_status = NULL,
                                              
                                              NA_color = "#AAAAAA",
                                              named_label_color_list = NULL,
                                              
                                              color_iterator = NULL,
                                              
                                              get_label_colors = function(unique_labels){
                                                unique_labels <- as.character(unique_labels)
                                                present_color_labels <- intersect(unique_labels, names(private$named_label_color_list))
                                                missing_color_labels <- setdiff(unique_labels, present_color_labels)
                                                
                                                result <- list()
                                                result[present_color_labels] <- private$named_label_color_list[present_color_labels]
                                                
                                                num_cols <- length(missing_color_labels)
                                                if(private$NA_label %in% missing_color_labels){
                                                  result[[private$NA_label]] <- private$NA_color
                                                  num_cols <- num_cols - 1
                                                  missing_color_labels <- missing_color_labels[missing_color_labels != private$NA_label]
                                                }
                                                
                                                l$s$err$assert_msg("Inconsistent state.",
                                                                   length(missing_color_labels) == num_cols)
                                                
                                                if(num_cols > 0){
                                                  #' For some reason, t-SNE variant only works for k >= 2!
                                                  colors <- unlist(private$color_iterator$get_next(num_cols))
                                                  names(colors) <- NULL
                                                  
                                                  result[missing_color_labels] <- colors
                                                  
                                                  # Increase saturation
                                                  result[missing_color_labels] <- sapply(result[missing_color_labels],
                                                                                         FUN = function(rgb_color){
                                                                                           rgb_color <- col2rgb(rgb_color)
                                                                                           hsv_color <- rgb2hsv(rgb_color)
                                                                                           l$s$err$assert_msg("Inconsistent state - no grey colors should appear here.",
                                                                                                              hsv_color[[2]] > 0)
                                                                                           hsv_color[[2]] <- hsv_color[[2]] + 0.5*(1-hsv_color[[2]])
                                                                                           
                                                                                           return(hsv(hsv_color[[1]],
                                                                                                      hsv_color[[2]],
                                                                                                      hsv_color[[3]]))
                                                                                         })
                                                }
                                                
                                                private$named_label_color_list[missing_color_labels] <- result[missing_color_labels]
                                                if(private$NA_label %in% names(result) && !(private$NA_label %in% names(private$named_label_color_list))){
                                                  private$named_label_color_list[[private$NA_label]] <- result[[private$NA_label]]
                                                }
                                                
                                                l$s$err$assert_msg("Inconsistent state.",
                                                                  all(unique_labels %in% names(private$named_label_color_list)),
                                                                  all(names(result) %in% names(private$named_label_color_list)),
                                                                  length(names(result)) == length(result),
                                                                  length(names(private$named_label_color_list)) == length(private$named_label_color_list))
                                                return(result)
                                              },
                                              
                                              plot = function(data){
                                                l$s$err$assert_msg("Inconsistent state.",
                                                                   private$iterators_num > 0,
                                                                   length(data) == private$iterators_num,
                                                                   all(sapply(1:private$iterators_num,
                                                                              FUN = function(dim){
                                                                                return(nrow(data[[dim]]) > 0 &&
                                                                                         all(data[[dim]][private$timestamp_name] >= private$x_lim_min &&
                                                                                               data[[dim]][private$timestamp_name] <= private$x_lim_max))
                                                                              })))
                                                
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
                                                  X11(width = private$plot_width / 72, 
                                                      height = private$plot_height / 72,
                                                      title = private$window_label)
                                                  private$last_device <- dev.cur()
                                                }
                                                
                                                par(mar=private$plot_margins)
                                                par(mfrow=c(private$iterators_num,1))
                                                
                                                l$s$err$assert_msg("Inconsistent state.",
                                                                   private$x_axis_range > 0)
                                                
                                                num_ticks <- 100
                                                x_labels_every_tick <- 10
                                                magnitude <- 10^ceiling(log10(x = private$x_axis_range))
                                                tick_width <- magnitude / num_ticks
                                                
                                                tick_seq_x <- seq(private$x_lim_min, private$x_lim_max, by = tick_width)
                                                if(tail(tick_seq_x, n = 1) < private$x_lim_max){
                                                  tick_seq_x <- c(tick_seq_x, (tail(tick_seq_x, n = 1) + tick_width))
                                                }
                                                label_seq_x <- formatC(seq(private$x_lim_min, private$x_lim_max, by = x_labels_every_tick * tick_width),
                                                                       format = 'd')
                                                
                                                l$s$err$assert_msg("Inconsistent state.",
                                                                   all(sapply(data,
                                                                              FUN = function(data_frame){
                                                                                return(head(data_frame, n = 1)[[2]] >= head(tick_seq_x, n = 1))
                                                                              })),
                                                                   all(sapply(data,
                                                                              FUN = function(data_frame){
                                                                                return(tail(data_frame, n = 1)[[2]] <= tail(tick_seq_x, n = 1))
                                                                              })),
                                                                   all(sapply(data,
                                                                              FUN = function(data_frame){
                                                                                return(head(data_frame, n = 1)[[2]] >= as.numeric(head(label_seq_x, n = 1)))
                                                                              })))
                                                
                                                legend_colors <- list()
                                                unique_labels <- list()
                                                #' Point type
                                                pch_num <- 21
                                                #' Point size
                                                cex_num <- 0.1
                                                #' Line width
                                                lwd_num <- 0.5
                                                
                                                x_label = paste0(private$timestamp_name)
                                                
                                                pch_char <- 23
                                                cex_char_NA <- 0.5
                                                cex_char <- 1.0
                                                
                                                for(dim in 1:private$iterators_num){
                                                  if(dim > private$numeric_iterators_num){
                                                    data[[dim]]["data"] <- 0
                                                    data[[dim]][data[[dim]][,1] != private$NA_label, "data"] <- 1
                                                  }
                                                  unique_labels <- unique(c(unique_labels, unique(as.character(data[[dim]][, 1]))))
                                                }
                                                
                                                legend_colors <- unlist(private$get_label_colors(unique_labels))
                                                
                                                for(dim in 1:private$iterators_num){
                                                  plot_pars <- list(x = data[[dim]][,private$timestamp_name],
                                                                    y = data[[dim]][,"data"], 
                                                                    type = "n", 
                                                                    xlab="", 
                                                                    ylab="", 
                                                                    xaxt="n",
                                                                    cex.lab=0.8)
                                                  
                                                  if(dim > private$numeric_iterators_num){
                                                    plot_pars[["yaxt"]] <- "n"
                                                    plot_pars[["ylim"]] <- c(-1, 2)
                                                  }
                                                  
                                                  do.call("plot", plot_pars)
                                                  par(new=TRUE)
                                                  
                                                  axis(1, at = setdiff(tick_seq_x,label_seq_x), labels = FALSE, tcl=-0.25)
                                                  axis(1, at = label_seq_x, labels = label_seq_x, las = 1, tcl=-0.5)
                                                  
                                                  if(dim <= private$numeric_iterators_num){
                                                    abline(h=axTicks(2),lty=3, col="#CCCCCC")
                                                  }
                                                  
                                                  abline(v=label_seq_x, lty=3, col="#CCCCCC")
                                                  
                                                  #' Draw lines for numeric data
                                                  num_elems <- nrow(data[[dim]])
                                                  
                                                  if(num_elems >= 1){
                                                    points_x <- data[[dim]][,2]
                                                    
                                                    points_y <- data[[dim]][,3]
                                                    labels <- as.character(data[[dim]][,1])
                                                    
                                                    l$s$err$assert_msg("Inconsistent state.",
                                                                       all(labels %in% names(legend_colors)))
                                                    colors <- legend_colors[labels]
                                                    
                                                    if(dim <= private$numeric_iterators_num){
                                                      pch <- pch_num
                                                      cex <- cex_num
                                                      
                                                      if(num_elems > 1){
                                                        segment_cols <- sapply(1:(num_elems - 1),
                                                                               FUN = function(i){
                                                                                 if(data[[dim]][i,][[1]] == data[[dim]][i + 1,][[1]]){
                                                                                   return(colors[[i]])
                                                                                 }
                                                                                 else{
                                                                                   return(private$NA_color)
                                                                                 }
                                                                               })
                                                        segments(points_x[1:(num_elems - 1)], points_y[1:(num_elems - 1)], 
                                                                 points_x[2:num_elems], points_y[2:num_elems], 
                                                                 col = segment_cols,
                                                                 lwd = lwd_num)
                                                      }
                                                    }
                                                    else{
                                                      pch <- pch_char
                                                      cex <- sapply(1:num_elems,
                                                                    FUN = function(i){
                                                                      if(data[[dim]][i,][[1]] == private$NA_label){
                                                                        return(cex_char_NA)
                                                                      }
                                                                      else{
                                                                        return(cex_char)
                                                                      }
                                                                    })
                                                    }
                                                    
                                                    points(x = points_x,
                                                           y = points_y,
                                                           col = colors,
                                                           bg = colors,
                                                           pch = pch,
                                                           cex = cex)
                                                  }
                                                  if(dim > private$numeric_iterators_num){
                                                    pch <- pch_char
                                                  }
                                                  else{
                                                    pch <- pch_num
                                                  }
                                                  legend_labels <- unique(labels)
                                                  this_legend_colors <- legend_colors[legend_labels]
                                                  legend(x="topleft",
                                                         legend = names(this_legend_colors),
                                                         col = this_legend_colors,
                                                         pt.bg = this_legend_colors,
                                                         pch = pch,
                                                         cex = 1,
                                                         bg = "#F2F2F2")
                                                  
                                                  title(main = paste0(private$graph_labels[[dim]], ": ",num_elems, " elements") , line=0.8)
                                                  title(xlab = x_label, line=2)
                                                  title(ylab = private$y_axis_labels[[dim]], line=2)
                                                }
                                                
                                                if(!is.null(private$first_plot_out_path) && head(tick_seq_x, n = 1) >= private$first_plot_start){
                                                  dir <- dirname(private$first_plot_out_path)
                                                  if(!exists(dir)){
                                                    dir.create(path = dir, showWarnings = FALSE, recursive = TRUE)
                                                  }
                                                  
                                                  dev.print(svg, private$first_plot_out_path,
                                                            width = private$plot_width / 72, 
                                                            height = private$plot_height / 72,
                                                            pointsize = 12,
                                                            antialias = "subpixel")
                                                  private$first_plot_out_path <- NULL
                                                }
                                              },
                                              
                                              check_elements = function(elements, dim){
                                                if(is.data.frame(elements)){
                                                  elems_num <- nrow(elements)
                                                  if(nrow(elements) == 0){
                                                    return(TRUE)
                                                  }
                                                  else{
                                                    return(all(sapply(1:elems_num,
                                                                      FUN = function(i){
                                                                        return(private$check_element(elements[i,], dim))
                                                                      })))
                                                  }
                                                }
                                                else{
                                                  return(all(sapply(elements,
                                                                    FUN = function(elem){
                                                                      return(private$check_element(elem, dim))
                                                                    })))
                                                }
                                              },
                                              
                                              check_element = function(element, dim){
                                                l$s$err$assert_msg("Illegal argument",
                                                                   l$s$tcs$has_length(element, len = 3, NA_on_fail = FALSE),
                                                                   l$s$tcs$has_length_1(element[[1]], NA_on_fail = FALSE),
                                                                   l$s$tcs$has_length_1(element[[2]], NA_on_fail = FALSE),
                                                                   l$s$tcs$has_length_1(element[[3]], NA_on_fail = FALSE),
                                                                   is.factor(element[[1]]) || l$s$tcs$is_character(element[[1]],
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE),
                                                                   l$s$tcs$is_numeric(element[[2]],
                                                                                      accept_NULL = FALSE,
                                                                                      accept_NaN = FALSE,
                                                                                      accept_NA = FALSE,
                                                                                      lower_bound = 0,
                                                                                      lower_bound_inclusive = TRUE,
                                                                                      upper_bound = Inf,
                                                                                      upper_bound_inclusive = FALSE,
                                                                                      accept_non_integer = TRUE),
                                                                   xor(dim >= 1 && dim <= private$numeric_iterators_num && 
                                                                         l$s$tcs$is_numeric(element[[3]],
                                                                                            accept_NULL = FALSE,
                                                                                            accept_NaN = FALSE,
                                                                                            accept_NA = FALSE,
                                                                                            lower_bound = -Inf,
                                                                                            lower_bound_inclusive = FALSE,
                                                                                            upper_bound = Inf,
                                                                                            upper_bound_inclusive = FALSE,
                                                                                            accept_non_integer = TRUE),
                                                                       dim > private$numeric_iterators_num && dim <= private$iterators_num &&
                                                                         (is.factor(element[[3]]) || l$s$tcs$is_character(element[[3]],
                                                                                                                          accept_NULL = FALSE,
                                                                                                                          accept_NaN = FALSE,
                                                                                                                          accept_NA = FALSE))))
                                                return(TRUE)
                                              },
                                              
                                              assert_consistency = function(){
                                                # At this point guaranteed that num_dims > 0 and each dimension has at least one word
                                                if(l$s$err$get_assertions_status()){
                                                  l$s$err$assert_msg("Inconsistent state.",
                                                                     private$numeric_iterators_num >= 0,
                                                                     private$character_iterators_num >= 0,
                                                                     private$iterators_num == private$numeric_iterators_num + 
                                                                       private$character_iterators_num,
                                                                     length(private$y_axis_labels) == private$iterators_num,
                                                                     length(private$max_ts) == private$iterators_num,
                                                                     
                                                                     length(private$buffer_list) == private$iterators_num,
                                                                     
                                                                     private$x_lim_min < private$x_lim_max,
                                                                     all.equal(private$x_lim_min + private$x_axis_range,
                                                                               private$x_lim_max),
                                                                     l$s$mg$`%eq%`(any(sapply(private$max_ts, FUN = is.na)),
                                                                                   !private$all_dims_have_elems))
                                                  
                                                  if(private$iterators_num > 0){
                                                    for(dim in 1:private$iterators_num){
                                                      l$s$err$assert_msg("Inconsistent state.",
                                                                         is.data.frame(private$buffer_list[[dim]]),
                                                                         identical(names(private$buffer_list[[dim]]), private$frame_colnames),
                                                                         #This is already checked for each newly added chunk in add_data()
                                                                         #private$check_elements(private$buffer_list[[dim]], dim),
                                                                         l$s$mg$`%eq%`(is.na(private$max_ts[[dim]]),
                                                                                       nrow(private$buffer_list[[dim]]) == 0),
                                                                         l$s$mg$`%eq%`(!is.na(private$max_ts[[dim]]),
                                                                                       nrow(private$buffer_list[[dim]]) > 0 &&
                                                                                         tail(private$buffer_list[[dim]], n = 1)[[2]] == private$max_ts[[dim]])
                                                      )
                                                      
                                                      
                                                      num_elems <- nrow(private$buffer_list[[dim]])
                                                      if(num_elems > 0){
                                                        l$s$err$assert_msg("Inconsistent state.",
                                                                           private$buffer_list[[dim]][[1,2]] >= private$x_lim_min)
                                                      }

                                                      # The check above, together with the note below, already implies this check
                                                      #' if(num_elems > 0){
                                                      #'   l$s$err$assert_msg("Inconsistent state.",
                                                      #'                      all(sapply(1:num_elems,
                                                      #'                                 FUN = function(i){
                                                      #'                                   return(private$buffer_list[[dim]][[i,2]] >= 
                                                      #'                                            private$x_lim_min)
                                                      #'                                 })))
                                                      #' }
                                                      #' 
                                                      # This is already checked for each newly added data chunk in add_data() - plus, the latter
                                                      # method also asserts that first chunk element has a timestamp >= the last buffer timestamp
                                                      # => the check here is redundant (and expensive!)
                                                      #' if(num_elems > 1){
                                                      #'   #' Increasing timestamps
                                                      #'   l$s$err$assert_msg("Inconsistent state.",
                                                      #'                      all(sapply(1:(num_elems - 1),
                                                      #'                                 FUN = function(i){
                                                      #'                                   return(private$buffer_list[[dim]][[i,2]] <= 
                                                      #'                                            private$buffer_list[[dim]][[i + 1, 2]])
                                                      #'                                 })))
                                                      #' }
                                                    }
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
                                              
                                              #' @export
                                              #'
                                              #' @examples
                                              #' @md
                                              initialize = function(assertions_status = FALSE,
                                                                    numeric_iterators_num,
                                                                    character_iterators_num,
                                                                    NA_label = "NA",
                                                                    #' Replot graph only after at least this many ms have passed
                                                                    #' since the last replot.
                                                                    replot_min_interval,
                                                                    #' Plot at most 'max_x_progress' into the future w.r.t. last plot
                                                                    max_x_progress,
                                                                    #' In ms
                                                                    x_axis_range,
                                                                    graph_labels = NULL,
                                                                    y_axis_labels = "",
                                                                    ground_truth_name = "label",
                                                                    timestamp_name = "time",
                                                                    plot_width = 1920 / 1.3,
                                                                    plot_height = 1080 / 1.1,
                                                                    plot_height_per_graph = NULL,
                                                                    window_label = "Graphs",
                                                                    color_rnd_seed = 303021,
                                                                    first_plot_out_path = NULL,
                                                                    first_plot_start = -Inf){
                                                
                                                super$initialize()
                                                local_env$.add_to_static_env(super$get_static_env())
                                                local_env$static_env$err$set_assertions_status(assertions_status)
                                                
                                                #' Add your initialization code here here:
                                                if(is.null(y_axis_labels)){
                                                  y_axis_labels <- ""
                                                }
                                                
                                                if(l$s$err$get_assertions_status()){
                                                  l$s$err$assert_msg("'color_rnd_seed' must be NULL or a finite integer of length one.",
                                                                     l$s$mg$`%then%`(!is.null(color_rnd_seed), 
                                                                                     l$s$tcs$has_length_1(color_rnd_seed, NA_on_fail = FALSE)),
                                                                     l$s$mg$`%then%`(!is.null(color_rnd_seed), 
                                                                                     l$s$tcs$is_integer(color_rnd_seed,
                                                                                                        accept_NULL = FALSE,
                                                                                                        accept_NaN = FALSE,
                                                                                                        accept_NA = FALSE,
                                                                                                        lower_bound = -Inf,
                                                                                                        lower_bound_inclusive = FALSE,
                                                                                                        upper_bound = Inf,
                                                                                                        upper_bound_inclusive = FALSE)))
                                                  
                                                  l$s$err$assert_msg("'first_plot_out_path' must be either `NULL` or a character vector of length one.",
                                                                     l$s$mg$`%then%`(!is.null(first_plot_out_path),
                                                                                     l$s$tcs$has_length_1(first_plot_out_path, NA_on_fail = FALSE) &&
                                                                                     l$s$tcs$is_character(first_plot_out_path,
                                                                                                          accept_NULL = FALSE,
                                                                                                          accept_NaN = FALSE,
                                                                                                          accept_NA = FALSE)))
                                                  
                                                  l$s$err$assert_msg("'timestamp_name' must be a character vector of length one.",
                                                                     l$s$tcs$has_length_1(timestamp_name, NA_on_fail = FALSE),
                                                                     l$s$tcs$is_character(timestamp_name,
                                                                                          accept_NULL = FALSE,
                                                                                          accept_NaN = FALSE,
                                                                                          accept_NA = FALSE))
                                                  
                                                  l$s$err$assert_msg("'ground_truth_name' must be a character vector of length one.",
                                                                     l$s$tcs$has_length_1(ground_truth_name, NA_on_fail = FALSE),
                                                                     l$s$tcs$is_character(ground_truth_name,
                                                                                          accept_NULL = FALSE,
                                                                                          accept_NaN = FALSE,
                                                                                          accept_NA = FALSE))
                                                  
                                                  l$s$err$assert_msg("'window_label' must be a character vector of length one.",
                                                                     l$s$tcs$has_length_1(window_label, NA_on_fail = FALSE),
                                                                     l$s$tcs$is_character(window_label,
                                                                                          accept_NULL = FALSE,
                                                                                          accept_NaN = FALSE,
                                                                                          accept_NA = FALSE))
                                                  
                                                  l$s$err$assert_msg("'NA_label' must be a character vector of length one.",
                                                                     l$s$tcs$has_length_1(NA_label, NA_on_fail = FALSE),
                                                                     l$s$tcs$is_character(NA_label,
                                                                                          accept_NULL = FALSE,
                                                                                          accept_NaN = FALSE,
                                                                                          accept_NA = FALSE))
                                                  
                                                  l$s$err$assert_msg("'numeric_iterators_num' must be a finite, non-negative integer of length one.",
                                                                     l$s$tcs$has_length_1(numeric_iterators_num, NA_on_fail = FALSE),
                                                                     l$s$tcs$is_numeric(numeric_iterators_num,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE,
                                                                                        lower_bound = 0,
                                                                                        lower_bound_inclusive = TRUE,
                                                                                        upper_bound = Inf,
                                                                                        upper_bound_inclusive = FALSE))
                                                  
                                                  l$s$err$assert_msg("'plot_width' must be a finite numeric > 0 of length one.",
                                                                     l$s$tcs$has_length_1(plot_width, NA_on_fail = FALSE),
                                                                     l$s$tcs$is_numeric(plot_width,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE,
                                                                                        lower_bound = 0,
                                                                                        lower_bound_inclusive = FALSE,
                                                                                        upper_bound = Inf,
                                                                                        upper_bound_inclusive = FALSE,
                                                                                        accept_non_integer = TRUE))
                                                  
                                                  l$s$err$assert_msg("Exactly one of 'plot_height' and 'plot_height_per_graph' must be non-NULL.",
                                                                     xor(is.null(plot_height), is.null(plot_height_per_graph)))
                                                  
                                                  if(!is.null(plot_height)){
                                                    l$s$err$assert_msg("If 'plot_height' is non-NULL, it must be a finite numeric > 0 of length one.",
                                                                       l$s$tcs$has_length_1(plot_height, NA_on_fail = FALSE),
                                                                       l$s$tcs$is_numeric(plot_height,
                                                                                          accept_NULL = FALSE,
                                                                                          accept_NaN = FALSE,
                                                                                          accept_NA = FALSE,
                                                                                          lower_bound = 0,
                                                                                          lower_bound_inclusive = FALSE,
                                                                                          upper_bound = Inf,
                                                                                          upper_bound_inclusive = FALSE,
                                                                                          accept_non_integer = TRUE))
                                                  }
                                                  else{
                                                    l$s$err$assert_msg("If 'plot_height_per_graph' is non-NULL, it must be a finite numeric > 0 of length one.",
                                                                       l$s$tcs$has_length_1(plot_height_per_graph, NA_on_fail = FALSE),
                                                                       l$s$tcs$is_numeric(plot_height_per_graph,
                                                                                          accept_NULL = FALSE,
                                                                                          accept_NaN = FALSE,
                                                                                          accept_NA = FALSE,
                                                                                          lower_bound = 0,
                                                                                          lower_bound_inclusive = FALSE,
                                                                                          upper_bound = Inf,
                                                                                          upper_bound_inclusive = FALSE,
                                                                                          accept_non_integer = TRUE))
                                                  }
                                                  l$s$err$assert_msg("'character_iterators_num' must be a finite, non-negative integer of length one.",
                                                                     l$s$tcs$has_length_1(character_iterators_num, NA_on_fail = FALSE),
                                                                     l$s$tcs$is_integer(character_iterators_num,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE,
                                                                                        lower_bound = 0,
                                                                                        lower_bound_inclusive = TRUE,
                                                                                        upper_bound = Inf,
                                                                                        upper_bound_inclusive = FALSE))
                                                  
                                                  l$s$err$assert_msg("'first_plot_start' must be -Inf or a finite numeric of length one.",
                                                                     l$s$tcs$has_length_1(first_plot_start, NA_on_fail = FALSE),
                                                                     l$s$tcs$is_numeric(first_plot_start,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE,
                                                                                        lower_bound = -Inf,
                                                                                        lower_bound_inclusive = TRUE,
                                                                                        upper_bound = Inf,
                                                                                        upper_bound_inclusive = FALSE,
                                                                                        accept_non_integer = TRUE))
                                                  
                                                  l$s$err$assert_msg("'x_axis_range' must be a finite numeric > 0 of length one.",
                                                                     l$s$tcs$has_length_1(x_axis_range, NA_on_fail = FALSE),
                                                                     l$s$tcs$is_numeric(x_axis_range,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE,
                                                                                        lower_bound = 0,
                                                                                        lower_bound_inclusive = FALSE,
                                                                                        upper_bound = Inf,
                                                                                        upper_bound_inclusive = FALSE,
                                                                                        accept_non_integer = TRUE))
                                                  
                                                  l$s$err$assert_msg(paste0("'replot_min_interval' must be a finite, non-negative numeric."),
                                                                     l$s$tcs$has_length_1(replot_min_interval, NA_on_fail = FALSE),
                                                                     l$s$tcs$is_numeric(replot_min_interval,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE,
                                                                                        lower_bound = 0,
                                                                                        lower_bound_inclusive = TRUE,
                                                                                        upper_bound = Inf,
                                                                                        upper_bound_inclusive = FALSE,
                                                                                        accept_non_integer = TRUE))
                                                  
                                                  l$s$err$assert_msg(paste0("'max_x_progress' must be a finite, non-negative numeric ",
                                                                            "of length one not bigger than 'x_axis_range'."),
                                                                     l$s$tcs$has_length_1(max_x_progress, NA_on_fail = FALSE),
                                                                     l$s$tcs$is_numeric(max_x_progress,
                                                                                        accept_NULL = FALSE,
                                                                                        accept_NaN = FALSE,
                                                                                        accept_NA = FALSE,
                                                                                        lower_bound = 0,
                                                                                        lower_bound_inclusive = FALSE,
                                                                                        upper_bound = x_axis_range,
                                                                                        upper_bound_inclusive = TRUE,
                                                                                        accept_non_integer = TRUE))
                                                  
                                                  l$s$err$assert_msg(paste0("'y_axis_labels' must either be a character vector of length one ",
                                                                            "or of the same length as 'numeric_iterators_num + character_iterators_num'."),
                                                                     l$s$tcs$has_length_1(y_axis_labels, NA_on_fail = FALSE) ||
                                                                       l$s$tcs$has_length(y_axis_labels, 
                                                                                          len = numeric_iterators_num + character_iterators_num, 
                                                                                          NA_on_fail = FALSE),
                                                                     all(l$s$tcs$is_character(y_axis_labels,
                                                                                              accept_NULL = FALSE,
                                                                                              accept_NaN = FALSE,
                                                                                              accept_NA = FALSE)))
                                                  
                                                  l$s$err$assert_msg(paste0("'graph_labels' must either NUll, or a character vector of length one ",
                                                                            "or of the same length as 'numeric_iterators_num + character_iterators_num'."),
                                                                     l$s$tcs$has_length_0(graph_labels, NA_on_fail = FALSE) ||
                                                                       ((l$s$tcs$has_length_1(graph_labels, NA_on_fail = FALSE) ||
                                                                           l$s$tcs$has_length(graph_labels, 
                                                                                              len = numeric_iterators_num + character_iterators_num, 
                                                                                              NA_on_fail = FALSE)) &&
                                                                          all(l$s$tcs$is_character(graph_labels,
                                                                                                   accept_NULL = FALSE,
                                                                                                   accept_NaN = FALSE,
                                                                                                   accept_NA = FALSE))))
                                                }
                                                
                                                dims_num <- numeric_iterators_num + character_iterators_num
                                                private$iterators_num <- dims_num
                                                if(length(y_axis_labels) == 1 && dims_num > 1){
                                                  y_axis_labels <- lapply(1:dims_num,
                                                                          FUN = function(i){
                                                                            list(unlist(y_axis_labels))
                                                                          })
                                                }
                                                if(l$s$tcs$has_length_1(graph_labels, NA_on_fail = FALSE) && dims_num > 1){
                                                  graph_labels <- lapply(1:dims_num,
                                                                          FUN = function(i){
                                                                            list(unlist(graph_labels))
                                                                          })
                                                }
                                                else if(l$s$tcs$has_length_0(graph_labels, NA_on_fail = FALSE) && dims_num > 1){
                                                  graph_labels <- list()
                                                  if(numeric_iterators_num > 0){
                                                    max_num_zeroes <- floor(log10(numeric_iterators_num))
                                                    
                                                    for(i in 1:numeric_iterators_num){
                                                      num_zeroes <- floor(log10(i))
                                                      if(max_num_zeroes - num_zeroes > 0){
                                                        prefix_str <- paste0(sapply(1:(max_num_zeroes - num_zeroes), FUN = function(i){return("0")}), collapse = "")
                                                      }
                                                      else{
                                                        prefix_str <- ""
                                                      }
                                                      graph_labels[[i]] <- paste0("Numeric graph ", prefix_str, i)
                                                    }
                                                  }
                                                  
                                                  if(character_iterators_num > 0){
                                                    max_num_zeroes <- floor(log10(character_iterators_num))
                                                    
                                                    for(i in 1:character_iterators_num){
                                                      num_zeroes <- floor(log10(i))
                                                      if(max_num_zeroes - num_zeroes > 0){
                                                        prefix_str <- paste0(sapply(1:(max_num_zeroes - num_zeroes), FUN = function(i){return("0")}), collapse = "")
                                                      }
                                                      else{
                                                        prefix_str <- ""
                                                      }
                                                      graph_labels[[numeric_iterators_num + i]] <- paste0("Character graph ", prefix_str, i)
                                                    }
                                                  }
                                                }
                                                
                                                private$first_plot_start <- first_plot_start
                                                private$first_plot_out_path <- first_plot_out_path
                                                private$graph_labels <- graph_labels
                                                private$numeric_iterators_num <- numeric_iterators_num
                                                private$character_iterators_num <- character_iterators_num
                                                private$NA_label <- NA_label
                                                private$replot_min_interval <- replot_min_interval
                                                private$x_axis_range <- x_axis_range
                                                private$y_axis_labels <- y_axis_labels
                                                private$ground_truth_name <- ground_truth_name
                                                private$timestamp_name <- timestamp_name
                                                private$max_x_progress <- max_x_progress
                                                private$all_dims_have_elems <- FALSE
                                                private$frame_colnames <- c(ground_truth_name,
                                                                            timestamp_name,
                                                                            "data")
                                                private$window_label <- window_label
                                                private$assertions_status <- assertions_status
                                                
                                                private$color_iterator <- l$s$col$Random_Color_Iterator$new(assertions_status = assertions_status,
                                                                                                            rnd_seed = color_rnd_seed,
                                                                                                            saturation = 1.0,
                                                                                                            value = 0.9)
                                                
                                                if(is.null(plot_height)){
                                                  plot_height <- 
                                                    private$plot_margins[[1]] * 72 +
                                                    private$plot_margins[[3]] * 72 + 
                                                    (numeric_iterators_num + character_iterators_num) * plot_height_per_graph
                                                }
                                                private$plot_height <- plot_height
                                                private$plot_width <- plot_width
                                                
                                                if(private$iterators_num > 0){
                                                  empty_data_frame <- data.frame(matrix(ncol = 3, 
                                                                                        nrow = 0),
                                                                                 stringsAsFactors = TRUE)
                                                  names(empty_data_frame) <- private$frame_colnames
                                                  
                                                  l <- lapply(y_axis_labels,
                                                              FUN = function(i){
                                                                return(empty_data_frame)
                                                              })
                                                }
                                                else{
                                                  l <- list()
                                                }
                                                named_label_color_list <- list()
                                                private$buffer_list <- l
                                                
                                                private$x_lim_min <- 0
                                                private$x_lim_max <- private$x_lim_min + private$x_axis_range
                                                
                                                private$max_ts <- sapply(y_axis_labels,
                                                                         FUN = function(i){
                                                                           return(NA)
                                                                         })
                                                
                                                private$last_plot_t <- -Inf
                                                
                                                private$assert_consistency()
                                              },
                                              
                                              replot_possible = function(){
                                                t <- l$s$t$get_unix_time()
                                                if(private$iterators_num == 0 || !private$all_dims_have_elems ||
                                                   t < private$last_plot_t + private$replot_min_interval || 
                                                   any(sapply(private$max_ts,
                                                              FUN = function(ts){
                                                                # Should not be reached if 'all_dims_have_elems' is false
                                                                l$s$err$assert_msg("Inconsistent state.",
                                                                                   !is.na(ts))
                                                                return(ts <= private$x_lim_max)
                                                              }))){
                                                  return(FALSE)
                                                }
                                                else{
                                                  return(TRUE)
                                                }
                                              },
                                              
                                              get_numeric_indices = function(){
                                                if(private$numeric_iterators_num > 0){
                                                  return(1:private$numeric_iterators_num)
                                                }
                                                else {
                                                  return(c())
                                                }
                                              },
                                              
                                              get_next_x_axis_max = function(){
                                                return(private$x_lim_max)
                                              },
                                              
                                              get_character_indices = function(){
                                                if(private$character_iterators_num > 0){
                                                  return((private$numeric_iterators_num+1):private$character_iterators_num)
                                                }
                                                else {
                                                  return(c())
                                                }
                                              },
                                              
                                              replot = function(){
                                                private$assert_consistency()
                                                
                                                t <- l$s$t$get_unix_time()
                                                if(!self$replot_possible()){
                                                  return()
                                                }
                                                
                                                rows <- vector(mode="list", length = private$iterators_num)
                                                data <- vector(mode="list", length = private$iterators_num)
                                                for(dim in 1:private$iterators_num){
                                                  rows[[dim]] <- private$buffer_list[[dim]][private$timestamp_name] >= private$x_lim_min &
                                                    private$buffer_list[[dim]][private$timestamp_name] <= private$x_lim_max
                                                  
                                                  data[[dim]] <- private$buffer_list[[dim]][rows[[dim]], ]
                                                }
                                                
                                                private$plot(data)
                                                
                                                private$x_lim_min <- private$x_lim_min + private$max_x_progress
                                                private$x_lim_max <- private$x_lim_min + private$x_axis_range
                                                
                                                for(dim in 1:private$iterators_num){
                                                  # Remove old entries
                                                  private$buffer_list[[dim]] <-
                                                    private$buffer_list[[dim]][private$buffer_list[[dim]][private$timestamp_name] >= private$x_lim_min, ]
                                                  
                                                  #' We only plot if all max ts are >= x_lim_max_old and max_x_progress is always <=
                                                  #' x_axis_range => max_ts are >= than new x_lim_min_new
                                                  l$s$err$assert_msg("Inconsistent state.",
                                                                     nrow(private$buffer_list[[dim]]) > 0)
                                                }
                                                
                                                private$last_plot_t <- t
                                                Sys.sleep(0) # Force flush
                                                
                                                private$assert_consistency()
                                              },
                                              
                                              # elements must be a list whose i-th entry contains either a dataframes or a list containing
                                              # the data elements for the dimension stored in dimensions[[i]]
                                              add_data = function(elements, dimensions, replot = TRUE){
                                                private$assert_consistency()
                                                
                                                if(is.null(dimensions) && private$iterators_num > 0){
                                                  dimensions <- 1:private$iterators_num
                                                }
                                                
                                                num_dims = length(dimensions)
                                                l$s$err$assert_msg("Illegal arguments.",
                                                                   l$s$tcs$has_length_1(replot, NA_on_fail),
                                                                   l$s$tcs$is_logical(replot,
                                                                                      accept_NULL = FALSE,
                                                                                      accept_NaN = FALSE,
                                                                                      accept_NA = FALSE),
                                                                   num_dims == length(elements),
                                                                   all(sapply(dimensions,
                                                                              FUN = function(dim){
                                                                                return(l$s$tcs$has_length_1(dim) &&
                                                                                         l$s$tcs$is_integer(dim,
                                                                                                            accept_NULL = FALSE,
                                                                                                            accept_NaN = FALSE,
                                                                                                            accept_NA = FALSE,
                                                                                                            lower_bound = 1,
                                                                                                            lower_bound_inclusive = TRUE,
                                                                                                            upper_bound = private$iterators_num,
                                                                                                            upper_bound_inclusive = TRUE))
                                                                              })),
                                                                   l$s$mg$`%then%`(num_dims > 0, 
                                                                                   all(sapply(1:num_dims,
                                                                                              FUN = function(i){
                                                                                                elements_sublist <- elements[[i]]
                                                                                                return(private$check_elements(elements_sublist,
                                                                                                                              dim = dimensions[[i]]))
                                                                                              }))))
                                                if(num_dims > 0){
                                                  for(i in 1:num_dims){
                                                    if(!is.data.frame(elements[[i]])){
                                                      dim <- dimensions[[i]]
                                                      data_mode <- if(dim <= private$numeric_iterators_num) "numeric" else "character"
                                                      elements[[i]] <- l$s$dts$as_data_frame(
                                                        x = elements[[i]], 
                                                        nrows=length(elements[[i]]), 
                                                        ncols=3, 
                                                        rownames=NULL, 
                                                        colnames=private$frame_colnames, 
                                                        modes = c("character", "numeric", data_mode), 
                                                        byrow = TRUE,
                                                        check_args = private$assertions_status,
                                                        stringsAsFactors = TRUE)
                                                    }
                                                    
                                                    if(is.null(colnames(elements[[i]]))){
                                                      colnames(elements[[i]]) <- private$frame_colnames
                                                    }
                                                  }
                                                }
                                                
                                                l$s$err$assert_msg("Illegal arguments.",
                                                                   l$s$mg$`%then%`(num_dims > 0, 
                                                                                   all(sapply(1:num_dims,
                                                                                              FUN = function(i){
                                                                                                return(is.data.frame(elements[[i]]))
                                                                                              }))),
                                                                   l$s$mg$`%then%`(num_dims > 0, 
                                                                                   all(sapply(1:num_dims,
                                                                                              FUN = function(i){
                                                                                                data_chunk <- elements[[i]]
                                                                                                chunk_size <- nrow(data_chunk)
                                                                                                if(chunk_size == 0){
                                                                                                  return(TRUE)
                                                                                                }
                                                                                                else{
                                                                                                  return(all(sapply(1:chunk_size,
                                                                                                                    FUN = function(j){
                                                                                                                      dim <- dimensions[[i]]
                                                                                                                      if(nrow(private$buffer_list[[dim]]) > 0){
                                                                                                                        result <- data_chunk[[j,2]] >= tail(private$buffer_list[[dim]], n = 1)[[2]]
                                                                                                                      }
                                                                                                                      else{
                                                                                                                        result <- TRUE
                                                                                                                      }
                                                                                                                      return(result)
                                                                                                                    })))
                                                                                                }
                                                                                              }))),
                                                                   l$s$mg$`%then%`(num_dims > 0, 
                                                                                   all(sapply(1:num_dims,
                                                                                              FUN = function(i){
                                                                                                return(identical(names(elements[[i]]), private$frame_colnames))
                                                                                              }))),
                                                                   l$s$mg$`%then%`(num_dims > 0, 
                                                                                   all(sapply(1:num_dims,
                                                                                              FUN = function(i){
                                                                                                data_chunk <- elements[[i]]
                                                                                                chunk_size <- nrow(data_chunk)
                                                                                                if(chunk_size > 1){
                                                                                                  return(all(sapply(1:(chunk_size - 1),
                                                                                                                    FUN = function(i){
                                                                                                                      return(data_chunk[[i,2]] <= data_chunk[[i + 1, 2]])
                                                                                                                    })))
                                                                                                }
                                                                                                else{
                                                                                                  return(TRUE)
                                                                                                }
                                                                                              }))))
                                                
                                                if(num_dims > 0){
                                                  for(i in 1:num_dims){
                                                    dim <- dimensions[[i]]
                                                    private$buffer_list[[dim]] <- rbind(private$buffer_list[[dim]], elements[[i]])
                                                    
                                                    if(nrow(private$buffer_list[[dim]]) > 0){
                                                      new_max_t <- tail(private$buffer_list[[dim]], n = 1)[[2]]
                                                      l$s$err$assert_msg("Inconsistent state.",
                                                                         l$s$mg$`%then%`(!is.na(private$max_ts[[dim]]), new_max_t >= private$max_ts[[dim]]))
                                                      private$max_ts[[dim]] <- new_max_t
                                                    }
                                                  }
                                                  
                                                  if(!private$all_dims_have_elems){
                                                    private$all_dims_have_elems <- !any(sapply(private$max_ts, FUN = is.na))
                                                    
                                                    if(private$all_dims_have_elems){
                                                      min_t <- min(sapply(private$buffer_list,
                                                                          FUN = function(data_frame){
                                                                            l$s$err$assert_msg("Inconsistent state.",
                                                                                               nrow(data_frame) > 0)
                                                                            
                                                                            return(head(data_frame, n = 1)[[2]])
                                                                          }))
                                                      
                                                      private$x_lim_min <- min_t
                                                      private$x_lim_max <- private$x_lim_min + private$x_axis_range
                                                    }
                                                  }
                                                }
                                                
                                                if(replot){
                                                  self$replot()
                                                }
                                                private$assert_consistency()
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