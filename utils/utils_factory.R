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
#' @title  Collection of subclasses of `R6_Base` from `utils_lang_r6_baseclass.R` for generic object creation
#'   based on strings and/or JSON files.
#' @description Contains a single [get_<name>_env()] method
#'   that returns an environment containing the `R6` constructor objects of these subclasses.
#' @md

# Include guard
if (!exists("UTILS_FACTORY_R", inherits = FALSE)) {
  UTILS_FACTORY_R = TRUE
  
  #' @return An environment containing one or multiple constructor objects for `R6` subclasses of 
  #'         `R6_Base` from `utils_lang_r6_baseclass.R`. These provide functionality for generic object creation
  #'         based on strings and/or JSON files.
  #' @export
  #'
  #' @examples
  #' @md
  get_utils_factory_env <- function() {
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
    package_dependencies <- c("R6", "rjson") # e.g., c("R6","smoother","rowr","randomcoloR")
    
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
    
    json_rnd_seed_name <- "rnd_seed"
    json_glob_init_name <- "global_init_code"
    json_objects_name <- "objects"
    json_loc_name <- "location"
    json_init_name <- "init_code"
    json_gen_name <- "gen_code"
    json_req_name <- "required_params"
    json_duplicate_name <- "duplicate"
    
    l$json_rnd_seed_name <- trimws(l$json_rnd_seed_name)
    l$json_glob_init_name <- trimws(l$json_glob_init_name)
    l$json_objects_name <- trimws(l$json_objects_name)
    l$json_loc_name <- trimws(l$json_loc_name)
    l$json_init_name <- trimws(l$json_init_name)
    l$json_gen_name <- trimws(l$json_gen_name)
    l$json_req_name <- trimws(l$json_req_name)
    l$json_duplicate_name <- trimws(l$json_duplicate_name)
    
    #' @return If `elements` is `NULL`, then this method returns `list()`.
    #'   Otherweise it removes all `NULL` entries from `elements`, then trims each element
    #'   and removes entries of zero chars (i.e., `""`).
    #'   If `collapse` is `TRUE`, then the resulting `elements` is collapsed
    #'   with a `'\n'` separator. The resulting `elements` is then returned.
    clean_character_container <- function(elements, collapse){
      if(is.null(elements)){
        elements <- list()
      }
      
      #' Remove NULL entries
      elements <- elements[!sapply(elements, FUN = is.null)]
      #' Trim remaining entries
      elements <- trimws(elements)
      #' Remove zero length entires
      elements <- elements[nchar(elements) > 0]
      
      if(collapse && length(elements) > 0) {
        #' Combine each line to single, line break separated string
        elements <-  paste0(elements, collapse='\n')
        
        l$s$err$assert_msg("Length should be one at this point.",
                           length(elements) == 1)
      }
      return(elements)
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
        #' A factory for generic object creation based on 
        #' specifications provided via a JSON object, file or string.
        Factory <- R6Class("Factory",
                           # ---- R6 inheritance -----
                           #'  __Optional__: Change to inherit from a different parent class.
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
                             #'List of character names of each object type this factory creates
                             names = NULL,
                             #' List of expressions, i-th expression generats object corresponding
                             #' to i-th name in `names` 
                             gen_expr = NULL,
                             #' List of (possibly empty) lists of character parameter names.
                             #' i-th inner list corresponds to parameter names that must
                             #' be provided to `create_object()` in order to create the object
                             #' correspoinding to the i-th name in `names`. 
                             req_params = NULL,
                             #' Used as parent environment for all `init_envs`.
                             #' The global initialization code is executed in this environment
                             shared_env = NULL,
                             #' List of environments. i-th environment used to execute initialization
                             #' code of i-th object type.
                             init_envs = NULL,
                             #' List of environments. i-th environment's parent environment is 
                             #' set to `init_envs[[i]]`. `generator_envs[[i]]` holds
                             #' the environment in which each of the objects of i-th type
                             #' are created.
                             generator_envs = NULL,
                             #' A list whose i-th entry is a list containing the json key value pairs of
                             #' the i-th object type that are not used by this factory
                             #' (i.e., that are not "location", "init_code", "gen_code" or "required_params").
                             attributes = NULL,
                             
                             object_rnd_seed = NULL,
                             last_object_rnd_state = NULL
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
                             
                             #' Helper method for computing a JSON object from the three parameters of the initialize method.
                             #' Note that exactly one of the arguments must be non-`NULL`.
                             #' 
                             #' @param json_str `NULL` or a character vector 
                             #'   for which `fromJSON(json_str = json_str)` returns a JSON object that conforms to the specification
                             #'   above (i.e., `json_str` must be a string that is parseable to a valid JSON object).
                             #'   Note that exactly one of `json_str`, `json_file` and `json_obj` may be non-`NULL`.
                             #' @param json_file `NULL` or a character vector
                             #'   for which `fromJSON(file = json_file)` returns a JSON object that conforms to the specification
                             #'   above (i.e., `json_file` must be a path to a valid JSON file).
                             #' @param json_obj `NULL` or a JSON object that conforms to the specification
                             #'   above (i.e., one where `fromJSON(json_str = toJSON(json_obj))` would return an equivalent JSON
                             #'   object).
                             #'   If non-`NULL`, `json_str` and `json_file` must be `NULL`.
                             #'
                             #' @return A json object that is either constructred from `json_str`, the file at `json_file` or
                             #'   that is equal to `json_obj` - depending on which parameters have been provided.
                             #' @export
                             #'
                             #' @examples
                             #' @md
                             .compute_json_obj = function(json_str = NULL,
                                               json_file = NULL,
                                               json_obj = NULL){
                               if(is.null(json_obj)){
                                 file_provided <- is.null(json_str)
                                 
                                 l$s$err$assert_msg("If 'json_obj' is NULL, 'json_str' xor 'json_file' must be NULL.",
                                                    xor(file_provided, is.null(json_file)) )
                                 l$s$err$assert_msg("'json_str' and 'json_file' must be NULL or a character of length one.",
                                                    l$s$mg$`%then%`(file_provided, l$s$tcs$is_character(json_file,
                                                                                                        accept_NULL = FALSE,
                                                                                                        accept_NaN = FALSE,
                                                                                                        accept_NA = FALSE)),
                                                    l$s$mg$`%then%`(!file_provided, l$s$tcs$is_character(json_str,
                                                                                                         accept_NULL = FALSE,
                                                                                                         accept_NaN = FALSE,
                                                                                                         accept_NA = FALSE)))
                                 if(file_provided){
                                   json_obj <- fromJSON(file = json_file)
                                 }
                                 else{
                                   json_obj <- fromJSON(json_str = json_str)
                                 }
                               }
                               else{
                                 l$s$err$assert_msg("If 'json_obj' is not NULL, 'json_str' and 'json_file' must be NULL.",
                                                    is.null(json_str),
                                                    is.null(json_file))
                               }
                               return(json_obj)
                             },
                             
                             #' A factory for generic object creation based on 
                             #' specifications provided via a JSON object, file or a string representing the 
                             #' content of a JSON file. Objects can be generated via the `create_object()` method.
                             #' 
                             #' The JSON object must have the following key-value pairs at its outermost layer
                             #' (see https://www.json.org/ for a specification of JSON terminology):
                             #' * Key: `"rnd_seed"`, Value: A finite integer or `NULL`. In the latter case, this argument is ignored.
                             #'                             If non-`NULL`: The R random seed is set to that value (via `set.seed()`)
                             #'                             at the beginning of `initialize()` while the old
                             #'                             `.Random.seed` is saved and restored at the end of that method. 
                             #'                             During initialization, for each object name 
                             #'                             (as returned by `get_object_names` sorted in 
                             #'                             lexicographical order), a new seed is randomly drawn via
                             #'                             `sample(.Machine$integer.max, size = 1)`.
                             #'                             Given an object name `x`, at the beginning of the first call
                             #'                             to `create_object(x, ...)`, the R random seed is set to the previously
                             #'                             generated random seed associated with `x` (via `set.seed()`) while
                             #'                             the old `.Random.seed` is stored. At the end of that method,
                             #'                             the current `.Random.seed` state is saved and the old one previously stored
                             #'                             at the beginning is restored.
                             #'                             At the beginning of subsequent calls to `create_object(x, ...)`,
                             #'                             `.Random.seed` is set to the one stored at the end of the last call
                             #'                             to `create_object(x, ...)`. Like before, the old `.Random.seed` is saved at the
                             #'                             beginning and at the end, the current `.Random.seed` state is saved and the 
                             #'                             old one is restored.
                             #'                             
                             #'                             This mechanism is intended to enable reproducibility without affecting
                             #'                             the external state of the R random number generator 
                             #'                             (as represented by `.Random.seed`).                             
                             #'                             
                             #' * Key: `"global_init_code"`, Value: A character array with strings as values. `NULL` entries
                             #'                                     are removed and each string is trimmed (leading and trailing
                             #'                                     whitespaces are removed). Then each `""` entry is removed.
                             #'                                     The remaining strings in the resulting JSON array 
                             #'                                     are treated as subsequent lines of initialization code to be
                             #'                                     executed in a new environment `global = new.env()`
                             #'                                     (of course, this code might be empty, if no code is to be
                             #'                                     executed).
                             #' * Key: `"objects"`, Value: A JSON object where each key equals the name of an object type
                             #'                            (let the current object type be `<object_type>`)
                             #'                            that this factory will create. Its associated value holds
                             #'                            information on how to create the object of said type.
                             #' * Key: `<other>`: Is ignored. 
                             #'                                                      
                             #' The keys in `"objects"` refer to values that re JSON objects with the following key-value pairs:
                             #' * Key: `"init_code"`, Value: A character array that is first preprocessed the same way as
                             #'                              the one from `global_init_code` resulting in strings
                             #'                              holding object type specific initialization code lines.
                             #'                              For the current `<object_type>`, a new environment 
                             #'                              `init[[<object_type>]] = new.env(parent=global)` is created and the
                             #'                              the object specific initialization code is executed
                             #'                              in that environment.
                             #'                              Note that `global` is the parent of that environment, meaning
                             #'                              that fields created in the context of the execution of `global_init_code`
                             #'                              may be accessed here.
                             #' * Key: `"gen_code"`, Value: A character array that is first preprocessed the same way as
                             #'                             the one from `global_init_code` resulting in strings
                             #'                             holding object type specific generation code lines.
                             #'                             For the current `<object_type>`, a new environment 
                             #'                             `gen[[<object_type>]] = new.env(parent=init[[<object_type>]])` 
                             #'                             is created and the
                             #'                             the generation code code is later executed in this environment
                             #'                             for each call of `create_object`.
                             #'                             Execution of this code should result in an assignable object.
                             #'                             Note that `global` as well as init[[<object_type>]] are 
                             #'                             (grand-)parents of that environment, meaning
                             #'                             that fields created in the context of the execution of `global_init_code`
                             #'                             and `init_code` may be accessed here.
                             #' * Key: `"required_params"`, Value: A character array that is first preprocessed the same way as
                             #'                                    the one from `global_init_code`
                             #'                                    If the resulting array is non-empty, 
                             #'                                    `gen_code` is assumed to represent a function call of the form
                             #'                                    `[optional: <environment>$]<function_name>(<params>)`
                             #'                                    and each string `<param>` in `required_params` represents the name
                             #'                                    of a parameter that has to be additionally provided to the 
                             #'                                    `create_object()` method in the form of `<param> = <value>`
                             #'                                    for that object type. A `<param> = <value>` expression is then added to the
                             #'                                    parameter list function call in `gen_code` 
                             #'                                    and it is made sure that `<value>` is accessible in the environment
                             #'                                    `gen[[<object_type>>]]`.
                             #' * Key: `"location"`, Value: A character array that is first preprocessed the same way as
                             #'                             the one from `global_init_code`. The strings are
                             #'                             then concatenated with linebreak separators.
                             #'                             If the resulting array is non-empty, the code in `gen_code`
                             #'                             is executed as `<location_value>$<gen_code_value>`.
                             #'                             For example, if `<gen_code_value>` is a call to a constructor
                             #'                             of an R6 constructor object, that object might be stored
                             #'                             in an environment `<location_value>` created in
                             #'                             `global_init_code`.
                             #' * (OPTIONAL) Key: `"duplicate"`, Value: If `NULL`, this key is ignored. Otherwise it must be a non-negative integer.
                             #'                                         The whole value assigned to the object type key (i.e., the entry under the current key in `"objects"`)
                             #'                                         is copied the provided number of times with all its fields except for the `"duplicate"` one.
                             #'                                         The `n`-th copy is assigned
                             #'                                         to the key `"<object_type>_<0min>n"` where `"<object_type>"` is the current key 
                             #'                                         w.r.t. `"objects"` and `"<0min>"` denotes the minimal length zero padding so that each number `n` has the same
                             #'                                         number of digits.
                             #'                                         The original key `"<object_type>"` is then deleted from `"objects"`.
                             #'                                         Each of the `"<object_type>_<0min>n"` keys may now be used as object type names in
                             #'                                         `create_object(...)`.
                             #'                                         
                             #' * Key: `<other>`: All other key-value pairs of that object type are stored in this factory
                             #'                   and my be later accessed via `get_attributes()` (with trimmed keys). 
                             #'                             
                             #' See `./config/test.json` for an example file.
                             #' 
                             #' __IMPORTANT__: The `global` environment as well as each `init[[<object_type>]]` environment is
                             #'   bindings locked after their respective code has been executed.
                             #' 
                             #' __NOTE__: This class assumes that the JSON object is defined according the description above.
                             #'           It will produce an error in most cases if it is not - but this is not guaranteed!
                             #' 
                             #' __IMPORTANT__: See documentation of this method in `utils_lang_r6_subclass_TEMPLATE.R`.
                             #'
                             #' @param assertions_status If `TRUE`, assertions are activated (see `set_assertion_status()`
                             #'   in `utils_lang_error.R`).
                             #'   
                             #' @param json_str `NULL` or a character vector 
                             #'   for which `fromJSON(json_str = json_str)` returns a JSON object that conforms to the specification
                             #'   above (i.e., `json_str` must be a string that is parseable to a valid JSON object).
                             #'   Note that exactly one of `json_str`, `json_file` and `json_obj` may be non-`NULL`.
                             #' @param json_file `NULL` or a character vector
                             #'   for which `fromJSON(file = json_file)` returns a JSON object that conforms to the specification
                             #'   above (i.e., `json_file` must be a path to a valid JSON file).
                             #' @param json_obj `NULL` or a JSON object that conforms to the specification
                             #'   above (i.e., one where `fromJSON(json_str = toJSON(json_obj))` would return an equivalent JSON
                             #'   object).
                             #'   If non-`NULL`, `json_str` and `json_file` must be `NULL`.
                             #'   
                             #' @export
                             #'
                             #' @examples
                             #' @md
                             initialize = function(assertions_status = FALSE,
                                                   json_str = NULL, 
                                                   json_file = NULL,
                                                   json_obj = NULL){
                               
                               super$initialize()
                               local_env$.add_to_static_env(super$get_static_env())
                               local_env$static_env$err$set_assertions_status(assertions_status)
                               
                               #' Add your initialization code here here:
                               json_obj <- self$.compute_json_obj(json_str = json_str,
                                                             json_file = json_file,
                                                             json_obj = json_obj)
                               
                               private$gen_expr <- list()
                               private$req_params <- list()
                               private$shared_env <- new.env()
                               private$init_envs <- list()
                               private$generator_envs <- list()
                               private$attributes <- list()
                               
                               names(json_obj) <- trimws(names(json_obj))
                               l$s$err$assert_msg("Empty JSON fields encountered.",
                                                  !("" %in% names(json_obj)))
                               l$s$err$assert_msg("JSON fields non unique.",
                                                  length(unique(names(json_obj))) == length(names(json_obj)))
                               
                               json_names <- c(l$json_glob_init_name,
                                               l$json_objects_name,
                                               l$json_rnd_seed_name)
                               if(l$s$err$get_assertions_status()){
                                 missing <- setdiff(json_names, names(json_obj))
                                 l$s$err$assert_msg(paste0("Missing JSON fields in JSON object: ", 
                                                           paste0(missing, collapse=" ")),
                                                    length(missing) == 0)
                               }
                               
                               rnd_seed <- json_obj[[l$json_rnd_seed_name]]
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
                                 glob_env <- globalenv()
                                 old_rnd_state <- get(x = ".Random.seed", envir = glob_env) 
                                 set.seed(rnd_seed)
                               }
                               
                               
                               json_obj[[l$json_glob_init_name]] <-
                                 l$clean_character_container(json_obj[[l$json_glob_init_name]], collapse = TRUE)
                               
                               if(length(json_obj[[l$json_glob_init_name]]) == 1){
                                 eval(
                                   expr = parse(text = json_obj[[l$json_glob_init_name]]),
                                   envir = private$shared_env
                                 )
                               }
                               
                               #------
                               
                               json_obj <- json_obj[[l$json_objects_name]]
                               names(json_obj) <- trimws(names(json_obj))
                               l$s$err$assert_msg("Empty object type name encountered.",
                                                  !("" %in% names(json_obj)))
                               l$s$err$assert_msg("JSON fields non unique.",
                                                  length(unique(names(json_obj))) == length(names(json_obj)))
                               
                               current_names <- names(json_obj)
                               for(obj_type in current_names){
                                 names(json_obj[[obj_type]]) <- trimws(names(json_obj[[obj_type]]))
                                 
                                 l$s$err$assert_msg("JSON fields non unique.",
                                                    length(unique(names(json_obj[[obj_type]]))) == length(names(json_obj[[obj_type]])))
                                 
                                 if(l$json_duplicate_name %in% names(json_obj[[obj_type]])){
                                   num <- json_obj[[obj_type]][[l$json_duplicate_name]]
                                   l$s$err$assert_msg("Num must be NULL or a non negative integer of length 1.",
                                                      l$s$mg$`%then%`(!is.null(num), 
                                                                      l$s$tcs$has_length_1(num, NA_on_fail = FALSE) &&
                                                                        l$s$tcs$is_integer(num,
                                                                                           accept_NULL = FALSE,
                                                                                           accept_NaN = FALSE,
                                                                                           accept_NA = FALSE,
                                                                                           lower_bound = 0,
                                                                                           lower_bound_inclusive = TRUE,
                                                                                           upper_bound = Inf,
                                                                                           upper_bound_inclusive = FALSE)))
                                   if(!is.null(num)){
                                     json_obj[[obj_type]] <- json_obj[[obj_type]][names(json_obj[[obj_type]])[which(names(json_obj[[obj_type]]) != l$json_duplicate_name)]]
                                     if(num > 0){
                                       for(i in 1:num){
                                         name <- paste(obj_type, formatC(i, width = floor(log10(num))+1, flag = "0"), sep = "_")
                                         l$s$err$assert_msg(paste0("Duplicating results in object name that is already present: ", name),
                                                            !(name %in% names(json_obj)))
                                         json_obj[[name]] <- json_obj[[obj_type]]
                                       }
                                     }
                                     json_obj <- json_obj[names(json_obj)[which(names(json_obj) != obj_type)]]
                                   }
                                 }
                               }
                               private$names <- sort(as.character(names(json_obj)))
                               json_names <- c(l$json_loc_name,
                                               l$json_init_name,
                                               l$json_gen_name,
                                               l$json_req_name)
                               
                               for(obj_type in private$names){
                                 names(json_obj[[obj_type]]) <- trimws(names(json_obj[[obj_type]]))
                                 if(l$s$err$get_assertions_status()){
                                   l$s$err$assert_msg(paste0("Inconsistent state.",
                                                             !(l$json_duplicate_name %in% names(json_obj[[obj_type]]))))
                                   missing <- setdiff(json_names, names(json_obj[[obj_type]]))
                                   l$s$err$assert_msg(paste0("Missing JSON fields in JSON object \"", 
                                                             obj_type,"\": ", paste0(missing, collapse=" ")),
                                                      length(missing) == 0)
                                 }
                                 
                                 private$attributes[[obj_type]] <- list()
                                 attrib_names <- setdiff(names(json_obj[[obj_type]]), json_names)
                                 for(attrib_name in attrib_names){
                                   private$attributes[[obj_type]][[attrib_name]] <- json_obj[[obj_type]][[attrib_name]]
                                 }
                                 
                                 for(json_name in json_names){
                                   json_obj[[obj_type]][[json_name]] <-
                                     l$clean_character_container(json_obj[[obj_type]][[json_name]], collapse = json_name != l$json_req_name)
                                 }
                                 
                                 l$s$err$assert_msg("Missing generator code.",
                                                    length(json_obj[[obj_type]][[l$json_gen_name]]) == 1)
                                 
                                 private$init_envs[[obj_type]] <- new.env(parent = private$shared_env)
                                 private$generator_envs[[obj_type]] <- new.env(parent = private$init_envs[[obj_type]])
                                 
                                 private$req_params[[obj_type]] <- json_obj[[obj_type]][[l$json_req_name]]
                                 
                                 if(length(json_obj[[obj_type]][[l$json_init_name]]) == 1){
                                   eval(
                                     expr = parse(text = json_obj[[obj_type]][[l$json_init_name]]),
                                     envir = private$init_envs[[obj_type]]
                                   )
                                 }
                                 lockEnvironment(private$init_envs[[obj_type]], bindings = TRUE)
                                 
                                 gen_expr <- json_obj[[obj_type]][[l$json_gen_name]]
                                 
                                 if(length(json_obj[[obj_type]][[l$json_req_name]]) > 0){
                                   req_param_expr <- paste0(json_obj[[obj_type]][[l$json_req_name]], "=", json_obj[[obj_type]][[l$json_req_name]],
                                                            collapse = ", ")
                                   
                                   gen_expr_split <- strsplit(gen_expr, split="\\s*\\(\\s*", fixed = FALSE)
                                   l$s$err$assert_msg(paste0("There should be at least one occurrence of the character \"(\"",
                                                             " in the concatenated string value of the key \"", json_gen_name, "\"",
                                                             " of the JSON object \"", obj_type, "\"."),
                                                      length(gen_expr_split) == 1,
                                                      length(gen_expr_split[[1]]) > 1)
                                   gen_expr_split <- gen_expr_split[[1]] #' strsplit returns list of vectors
                                   
                                   
                                   if(!grepl(pattern = "^\\s*\\)", x=gen_expr_split[[2]])){
                                     # There are arguments present and we need to append a comma to req_param_expr
                                     req_param_expr <- paste0(req_param_expr,", ")
                                   }
                                   
                                   gen_expr <- paste0(gen_expr_split[[1]], 
                                                      "(", 
                                                      req_param_expr, 
                                                      paste0(gen_expr_split[2:length(gen_expr_split)], collapse="("))
                                   
                                 }
                                 if(length(json_obj[[obj_type]][[l$json_loc_name]]) == 1){
                                   gen_expr <- paste0(json_obj[[obj_type]][[l$json_loc_name]],"$",gen_expr)
                                 }
                                 
                                 gen_expr <- paste0(obj_type,"=",gen_expr)
                                 
                                 private$gen_expr[[obj_type]] <- parse(text = gen_expr)
                                 lockEnvironment(private$shared_env, bindings = TRUE)
                               }
                               
                               if(!is.null(rnd_seed)){
                                 private$object_rnd_seed <- sapply(private$names, FUN = function(i) {
                                   return(sample(.Machine$integer.max, size = 1))
                                 })
                                 names(private$object_rnd_seed) <- private$names
                                 
                                 private$last_object_rnd_state = lapply(private$names,
                                                                       FUN = function(i){
                                                                         return(NULL)
                                                                       })
                                 names(private$last_object_rnd_state) <- private$names
                                 
                                 assign(".Random.seed", value = old_rnd_state, envir = glob_env)
                               }
                             },
                             
                             #' @return The bindings locked `global` environment described in the documentation of
                             #'  `initialize()`.
                             #' @export
                             #'
                             #' @examples
                             #' @md
                             get_global_env = function(){
                               return(private$shared_env)
                             },
                             
                             #' @return The bindings locked `init[[<object_type>]]` environment described in the documentation of
                             #'  `initialize()`.
                             #' @export
                             #'
                             #' @examples
                             #' @md
                             get_init_env = function(object_type){
                               l$s$err$assert_msg(paste0("'object_type' \"", object_type, "\" must be a character of length one."),
                                                  l$s$tcs$has_length_1(object_type, NA_on_fail = FALSE),
                                                  l$s$tcs$is_character(object_type,
                                                                       accept_NULL = FALSE,
                                                                       accept_NaN = FALSE,
                                                                       accept_NA = FALSE))
                               
                               l$s$err$assert_msg(paste0("Unknown 'object_type': ", object_type),
                                                  object_type %in% private$names)
                               
                               result <- private$init_envs[[object_type]]
                               l$s$err$assert_msg("'result' should be an environment at this point.",
                                                  !is.null(result),
                                                  is.environment(result))
                               return(result)
                             },
                             
                             #' @param object_type A character vector of length one. Must be one of the
                             #'   characters returned by `get_object_names()`.
                             #' @param ... Must contain the required parameters for the provided object type.
                             #'   I.e., `list(...)` must be a subset of the list returned by 
                             #'   `req_par = get_required_params(object_type)`.
                             #'   Note that elements in `list(...)` that are not in `req_par` are ignored.
                             #'
                             #' @return An object created by the code associated with
                             #'   the type name `object_type`. See documentation of `initialize()` for more details.
                             #' @export
                             #'
                             #' @examples
                             #' @md
                             create_object = function(object_type, ...){
                               object_type <- trimws(object_type)
                               l$s$err$assert_msg(paste0("'object_type' \"", object_type, "\" must be a character of length one."),
                                                  l$s$tcs$has_length_1(object_type, NA_on_fail = FALSE),
                                                  l$s$tcs$is_character(object_type,
                                                                       accept_NULL = FALSE,
                                                                       accept_NaN = FALSE,
                                                                       accept_NA = FALSE))
                               
                               l$s$err$assert_msg(paste0("Unknown 'object_type': ", object_type),
                                                  object_type %in% private$names)
                               
                               if(!is.null(private$object_rnd_seed)){
                                 glob_env <- globalenv()
                                 old_rnd_state <- get(x = ".Random.seed", envir = glob_env)
                                 l$s$err$assert_msg("Inconsistent state.",
                                                    object_type %in% names(private$object_rnd_seed),
                                                    object_type %in% names(private$last_object_rnd_state))
                                 if(is.null(private$last_object_rnd_state[[object_type]])){
                                   set.seed(private$object_rnd_seed[[object_type]])
                                 }
                                 else{
                                   assign(".Random.seed", value = private$last_object_rnd_state[[object_type]], envir = glob_env)
                                 }
                               }
                               
                               params <- list(...)
                               
                               if(l$s$err$get_assertions_status()){
                                 missing <- setdiff(private$req_params[[object_type]], names(params))
                                 
                                 l$s$err$assert_msg(paste0("Missing parameters for object type \"", 
                                                           object_type,"\": ", paste0(missing, collapse=" ")),
                                                    length(missing) == 0)
                                 dup_ind <- duplicated(names(params))
                                 l$s$err$assert_msg(paste0("The parameter list contains the following duplicate parameter names: ",
                                                           toString(names(params)[dup_ind])),
                                                    !any(dup_ind))
                               }
                               
                               for(req_param_name in private$req_params[[object_type]]){
                                 assign(req_param_name, value=params[[req_param_name]], envir = private$generator_envs[[object_type]])
                               }
                               
                               eval(
                                 expr = private$gen_expr[[object_type]],
                                 envir = private$generator_envs[[object_type]]
                               )
                               
                               result <- get(object_type, envir=private$generator_envs[[object_type]], inherits = FALSE)
                               
                               if(!is.null(private$object_rnd_seed)){
                                 private$last_object_rnd_state[[object_type]] <- get(x = ".Random.seed", envir = glob_env) 
                                 assign(".Random.seed", value = old_rnd_state, envir = glob_env)
                               }
                               
                               return(result)
                             },
                             
                             #' @return A vector of character names of each object type this factory creates.
                             #'   The names are sorted in ascending lexicographical order.
                             #'   See description of `<object_type>` in `initialize()` for more details.
                             #' @export
                             #'
                             #' @examples
                             #' @md
                             get_object_names = function(){
                               l$s$err$assert_msg("Names should be sorted at this point.",
                                                  identical(private$names, sort(as.character(private$names))))
                               return(private$names)
                             },
                             
                             #' Allows to retrieve object names filtered by certain conditions on their required parameters and attributes.
                             #' 
                             #' @param required_params `NULL` or a character list. 
                             #' @param req_params_relation `NULL` or a character list. 
                             #' @param attribute_names A binary function returning a logical vector (i.e. `is.element`, `setequal` etc.).
                             #' @param attribute_names_relation A binary function returning a logical vector.
                             #'
                             #' @return All object names `name` from the set returned by `get_object_names()` where the following holds:
                             #'   * Let `req = get_required_params(name)`, then `all(req_params_relation(required_params, req))`
                             #'     must be `TRUE`.
                             #'   * __AND__: Let `attr = get_attributes(name)`, then `all(attributes_names_relation(attribute_names, attr))`
                             #'     must be `TRUE`.
                             #' @export
                             #'
                             #' @examples
                             get_object_names_where = function(required_params = NULL, req_params_relation = is.element,
                                                               attribute_names = NULL, attribute_names_relation = is.element){
                               if(l$s$err$get_assertions_status()){
                                 l$s$err$assert_msg("'required_params' and 'attribute_names' must be either NULL or character vectors.",
                                                    l$s$mg$`%then%`(!is.null(required_params),
                                                                    all(l$s$tcs$is_character(required_params,
                                                                                             accept_NULL = FALSE,
                                                                                             accept_NaN = FALSE,
                                                                                             accept_NA = FALSE))),
                                                    l$s$mg$`%then%`(!is.null(attribute_names),
                                                                    all(l$s$tcs$is_character(attribute_names,
                                                                                             accept_NULL = FALSE,
                                                                                             accept_NaN = FALSE,
                                                                                             accept_NA = FALSE))))
                                 l$s$err$assert_msg("'req_params_relation' and 'attribute_names_relation' must be functions.",
                                                    is.function(req_params_relation),
                                                    is.function(attribute_names_relation))
                               }
                               
                               obj_names <- as.list(self$get_object_names())
                               i <- 1
                               while(i <= length(obj_names)){
                                 name <- obj_names[[i]]
                                 req_params <- self$get_required_params(name)
                                 attribs <- self$get_attributes(name)
                                 
                                 if(all(req_params_relation(required_params, req_params))
                                    && all(attribute_names_relation(attribute_names, names(attribs)))){
                                   i <- i + 1
                                 }
                                 else{
                                   obj_names[[i]] <- NULL
                                 }
                               }
                               return(as.character(obj_names))
                             },
                             
                             #'
                             #' @param object_type A character vector of length one. Must be one of the
                             #'   characters returned by `get_object_names()`.
                             #'
                             #' @return Possibly empty list of character parameter names that must
                             #' be provided to `create_object()` in order to create the object
                             #' correspoinding to `object_type`. See description of
                             #' `required_params` in the documentaion of `initialize()` for more details.
                             #' @export
                             #'
                             #' @examples
                             #' @md
                             get_required_params = function(object_type){
                               object_type <- trimws(object_type)
                               l$s$err$assert_msg(paste0("Unknown object type: ", object_type),
                                                  object_type %in% private$names)
                               
                               return(private$req_params[[object_type]])
                             },
                      
                             #'       
                             #' @param object_type A character vector of length one. Must be one of the
                             #'   characters returned by `get_object_names()`.
                             #'
                             #' @return A list containing the JSON key value pairs associated with
                             #'  `object_type` that are not used by this iterator
                             #'   (i.e., that are not `location`, `init_code`, `gen_code` or `required_params`).
                             #'   See documentation of `initialize()` for more details.
                             #' 
                             #' @export
                             #'
                             #' @examples
                             #' @md
                             get_attributes = function(object_type) {
                               object_type <- trimws(object_type)
                               l$s$err$assert_msg(paste0("Unknown object type: ", object_type),
                                                  object_type %in% private$names)
                               
                               return(private$attributes[[object_type]])
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
      },
      envir = result_env)
    lockEnvironment(local_env, bindings = TRUE)
    invisible(result_env)
  }
} # End of include guard