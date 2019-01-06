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
#' @title  Collection of methods and/or fields for creating instances of `Synthetic_Datastream_Iterators`
#'         `Datastream_Visualization`, `Multivariate_Character_Vocabulary` and `Multivariate_Count_LM` instances from
#'         JSON configuration files.
#'         
#' @description Contains a single [get_<name>_env()] method
#'   that returns an environment containing those methods.
#' @md

#' Clear the enclosing environment. Only this driver should be executed in this environment.
rm(list=ls())

invisible({
  packages <- c("rprojroot")
  for(pkg in packages){
    if(!require(pkg, character.only = TRUE)){
      install.packages(pkg, dependencies = TRUE, repos = "http://cran.us.r-project.org")
      library(pkg, character.only = TRUE)
    }
  }
  rm(list = c("packages"))
  
  dir <- normalizePath(dirname(thisfile()))
  # Go to parent folder of .../drivers/driver.R
  dir <- file.path(dir, "..")
  cat(paste0("Setting working directory to \"", dir, "\"\n"))
  setwd(dir)
  rm(list=c("dir"))
  
  options(error = function() traceback(2))
  # options(warn = 2) # Treat warnings as errors
  
  #' Randomly initialize the .Random.seed field (does not exist by default when run in console via `Rscript` command)
  set.seed(round((as.numeric(Sys.time()) * 1000) %% .Machine$integer.max))
})
  
# Include guard
if (!exists("DRIVER_R", inherits = FALSE)) {
  DRIVER_R = TRUE
  
  #' @return An environment containing one or multiple methods and/or fields for creating instances of `Synthetic_Datastream_Iterators`
  #'         `Datastream_Visualization`, `Multivariate_Character_Vocabulary` and `Multivariate_Count_LM` instances from
  #'         JSON configuration files.
  #' @export
  #'
  #' @examples
  #' @md
  get_driver_env <- function() {
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
    other_scripts <- c(file.path(".", "datastream", "alphanum_iterators.R"),
                       file.path(".", "datastream", "language_models.R"),
                       file.path(".", "utils", "utils_lang_error.R"),
                       file.path(".", "utils", "utils_math_general.R"),
                       file.path(".", "utils", "utils_data_tools.R"),
                       file.path(".", "utils", "utils_data_structures.R"),
                       file.path(".", "utils", "utils_lang_typechecks.R"))   # e.g.: c(file.path(".", "utils", "utils_math_general.R"))
    if(length(other_scripts) > 0){
      invisible(lapply(other_scripts, FUN = 
                         function(script){
                           source(script, local = local_env)
                         }))
    }
    
    #' Create locked (and bindings-locked) environment holding the methods previously added to `other_scripts`.
    other_env <- new.env()
    #' TODO: Append `assign` calls for each of the scripts previously added to `other_scripts`.
    #'       Each call should be of the following pattern:
    #'         `assign("<name>", get_<name>_env(), envir = other_env)`
    #'       For examples, see below and the corresponding lines in `utils_lang_r6_baseclass.R`.
    #' __Example__: `assign("math_general", get_utils_math_general_env(), envir = other_env)`, if
    #'              `file.path(".", "utils", "utils_math_general.R")` was added to `other_scripts` in the lines before.
    assign("alphanum_iterators", get_alphanum_iterators_env(), envir = other_env)
    assign("language_models", get_language_models_env(), envir = other_env)
    assign("utils_lang_error", get_utils_lang_error_env(), envir = other_env)
    assign("utils_math_general", get_utils_math_general_env(), envir = other_env)
    assign("utils_data_tools", get_utils_data_tools_env(), envir = other_env)
    assign("utils_data_structures", get_utils_data_structures_env(), envir = other_env)
    assign("utils_lang_typechecks", get_utils_lang_typechecks_env(), envir = other_env)
    
    assign("ait", other_env$alphanum_iterators, envir = other_env)
    assign("lm", other_env$language_models, envir = other_env)
    assign("err", other_env$utils_lang_error, envir = other_env)
    assign("mg", other_env$utils_math_general, envir = other_env)
    assign("dts", other_env$utils_data_tools, envir = other_env)
    assign("ds", other_env$utils_data_structures, envir = other_env)
    assign("tcs", other_env$utils_lang_typechecks, envir = other_env)
    
    lockEnvironment(other_env, bindings = TRUE)
    
    # --------- Load required package dependencies -------    
    #' Append other packages to `package_dependencies` which are to be installed (if necessary) and loaded
    package_dependencies <- c("rjson") # e.g., c("R6","smoother","rowr","randomcoloR")
    
    invisible(source(file.path(".", "utils", "utils_lang_package.R"), local = TRUE))
    lang_package <- get_utils_lang_package_env()
    if(!lang_package$install_and_load(package_dependencies, stop_on_failure = TRUE)){
      stop("At least one required package could not be installed and/or loaded.")
    }
    
    #' Cleanup all but `local_env`, `other_env`
    rm(list=setdiff(ls(envir = local_env), c("other_env", "local_env")), envir = local_env)
    
    #' __Optional__ -------- Create private helper methods and/or fields here: -------
    
    create_json_obj <- function(json_file,
                                json_string,
                                json_obj){
      stopifnot(length(which(c(!is.null(json_file), !is.null(json_string), !is.null(json_obj)))) == 1)
      
      if(is.null(json_obj)){
        if(!is.null(json_file)){
          json_obj <- fromJSON(file = json_file)
        }
        else{
          json_obj <- fromJSON(json_str = json_string)
        }
      }
      return(json_obj)
    }
    
    create_char_voc <- function(json_obj, assertions_status){
      l <- local_env
      o <- local_env$other_env
      
      old_assert_status <- l$o$err$get_assertions_status()
      l$o$err$set_assertions_status(assertions_status)
      
      voc <- json_obj[["char_voc"]]
      if("char_voc_duplicates" %in% names(json_obj)){
        for(chardup in json_obj[["char_voc_duplicates"]]){
          
          l$o$err$assert_msg("Illegal argument.",
                            l$o$tcs$has_length_1(chardup, NA_on_fail = FALSE),
                            l$o$tcs$has_length_1(names(chardup), NA_on_fail = FALSE),
                            l$o$tcs$has_length_1(chardup[[names(chardup)]]),
                            l$o$tcs$is_integer(chardup[[names(chardup)]],
                                               accept_NULL = FALSE,
                                               accept_NaN = FALSE,
                                               accept_NA = FALSE,
                                               lower_bound = 0,
                                               lower_bound_inclusive = TRUE,
                                               upper_bound = Inf,
                                               upper_bound_inclusive = FALSE))
          char_name <- names(chardup)
          num <- chardup[[char_name]]
          
          if(num > 0){
            for(i in 1:num){
              name <- paste(char_name, formatC(i, width = floor(log10(num))+1, flag = "0"), sep = "_")
              l$o$err$assert_msg(paste0("Duplicating results in char vocabulary entry that is already present: ", name),
                                 !(name %in% voc))
              voc <- c(voc, name)
            }
          }
        }
      }
      
      l$o$err$set_assertions_status(old_assert_status)
      return(voc)
    }
    
    created_structures_env <- new.env()
    evalq({
      synthetic_datastream <- NULL
      synthetic_datastream_factory <- NULL
      synthetic_datastream_visualization <- NULL
      vocabulary <- NULL
      empty_lm_vocabulary <- NULL
      language_model_trainer <- NULL
      language_model <- NULL
      empty_language_model <- NULL  
    }, envir = created_structures_env)
    
    l <- local_env
    o <- local_env$other_env
    # -------- Create environment to be returned containing 
    #          one or multiple topically related methods and/or fields -------
    #' Use `local_env` to access the enclosing environment and `other_env`
    #' (or `local_env$other_env`) to access subenvironments from
    #' the scripts from `other_scripts`.
    #'
    result_env <- new.env()
    evalq(
      {
        get_last_synthetic_datastream <- function(){
          return(l$created_structures_env$synthetic_datastream)
        }
        
        get_last_synthetic_datastream_factory <- function(){
          return(l$created_structures_env$synthetic_datastream_factory)
        }
        
        get_last_synthetic_datastream_visualization <- function(){
          return(l$created_structures_env$synthetic_datastream_visualization)
        }
        
        get_last_vocabulary <- function(){
          return(l$created_structures_env$vocabulary)
        }
        
        get_last_empty_lm_vocabulary <- function(){
          return(l$created_structures_env$empty_lm_vocabulary)
        }
        
        get_last_language_model_trainer <- function(){
          return(l$created_structures_env$language_model_trainer)
        }
        
        get_last_language_model <- function(){
          return(l$created_structures_env$language_model)
        }
        
        get_last_empty_language_model <- function(){
          return(l$created_structures_env$empty_language_model)
        }
        
        create_synth_ds_it_fac_from_json <- function(json_file = NULL,
                                                     json_string = NULL,
                                                     json_obj = NULL){
          
          json_obj <- l$create_json_obj(json_file = json_file,
                                        json_string = json_string,
                                        json_obj = json_obj)
          
          assertions_status <- json_obj[["general"]][["assertions_status"]]
          sequences_file <- json_obj[["synth_ds_its_factory"]][["json_file"]]
          
          synth_fac <- l$o$ait$Synthetic_Datastream_Iterator_Factory$new(assertions_status = assertions_status,
                                                                         json_str = NULL, 
                                                                         json_file = sequences_file,
                                                                         json_obj = NULL) 
          
          assign("synthetic_datastream_factory", synth_fac, envir = l$created_structures_env)
          
          return(synth_fac)
        }
        
        create_synth_ds_its_from_json <- function(json_file = NULL,
                                                  json_string = NULL,
                                                  json_obj = NULL){
          json_obj <- l$create_json_obj(json_file = json_file,
                                        json_string = json_string,
                                        json_obj = json_obj)
          
          assertions_status <- json_obj[["general"]][["assertions_status"]]
          
          numeric_iterators_num <- json_obj[["general"]][["numeric_iterators_num"]]
          character_iterators_num <- json_obj[["general"]][["character_iterators_num"]]
          
          synth_fac <- result_env$create_synth_ds_it_fac_from_json(json_file = NULL,
                                                                    json_string = NULL,
                                                                    json_obj = json_obj)
          
          individual_subparams <- json_obj[["synth_ds_its"]][["individual_subparams"]]
          
          noise_decorator_params <- json_obj[["synth_ds_its"]][["noise_decorator_params"]]
          num_concat_iterator_params <- json_obj[["synth_ds_its"]][["num_concat_iterator_params"]]
          
          delay_decorator_params <- json_obj[["synth_ds_its"]][["delay_decorator_params"]]
          stopifnot(!("timestamp_name" %in% names(delay_decorator_params)))
          delay_decorator_params[["timestamp_name"]] <- json_obj[["general"]][["timestamp_name"]]
          
          output_as_frame <- json_obj[["synth_ds_its"]][["output_as_frame"]]
          ground_truth_name <- json_obj[["general"]][["ground_truth_name"]]
          idle_event_name <- json_obj[["general"]][["idle_event_name"]]
          NA_label <- json_obj[["general"]][["NA_label"]]
          #min_num_NA_padding_elements <- json_obj[["synth_ds_its"]][["min_num_NA_padding_elements"]]
          #max_num_NA_padding_elements <- json_obj[["synth_ds_its"]][["max_num_NA_padding_elements"]]
          debug_mode <- json_obj[["synth_ds_its"]][[".debug_mode"]]
          verbose <- json_obj[["synth_ds_its"]][[".verbose"]]
          NA_character_sequence_label <- json_obj[["synth_ds_its"]][[".NA_character_sequence_label"]]
          padding_NA_character_sequence_label <- json_obj[["synth_ds_its"]][[".padding_NA_character_sequence_label"]]
          inter_NA_character_sequence_label <- json_obj[["synth_ds_its"]][[".inter_NA_character_sequence_label"]]
          intra_NA_character_sequence_label <- json_obj[["synth_ds_its"]][[".intra_NA_character_sequence_label"]]
          NA_rnd_walk_label <- json_obj[["synth_ds_its"]][[".NA_rnd_walk_label"]]
          padding_NA_rnd_walk_label <- json_obj[["synth_ds_its"]][[".padding_NA_rnd_walk_label"]]
          inter_NA_rnd_walk_label <- json_obj[["synth_ds_its"]][[".inter_NA_rnd_walk_label"]]
          intra_NA_rnd_walk_label <- json_obj[["synth_ds_its"]][[".intra_NA_rnd_walk_label"]]
          
          lm <- result_env$create_multiv_count_lm_from_json(json_file = NULL,
                                                            json_string = NULL,
                                                            json_obj = json_obj)
          
          vocabulary <- lm$get_vocabulary()
          
          synth_ds_rnd_seed <- json_obj[["synth_ds_its"]][["synth_ds_rnd_seed"]]
          glob_env <- globalenv()
          
          first_call <- TRUE
          
          next_event_getter <- function(){
            old_assert_status <- l$o$err$get_assertions_status()
            l$o$err$set_assertions_status(assertions_status)
            
            if(first_call){ # First call always idle
              l$o$err$assert_msg("Inconsistent state.",
                                 identical(lm$.get_current_state(),
                                           lm$.get_idle_state()),
                                 lm$is_locked())
              next_it_id <- NULL
              next_word_id <- vocabulary$get_idle_id()
              first_call <<- FALSE
            }
            else{
              next_word_id <- lm$emit_word_id(proceed = TRUE)
              next_it_id <- vocabulary$get_id_dims(next_word_id)
              
              if(next_word_id == vocabulary$get_idle_id()){
                l$o$err$assert_msg("Inconsistent state.",
                                   length(next_it_id) == vocabulary$get_num_dims())
                next_it_id  <- NULL
              }
              else{
                l$o$err$assert_msg("Inconsistent state.",
                                   length(next_it_id) == 1)
                next_it_id <- unlist(next_it_id)
              }
            }
            next_seq_name <- vocabulary$get_word(next_word_id)
            params <- NULL
            
            if(!is.null(next_it_id) && next_it_id > numeric_iterators_num){
              params = list()
              params[["element"]] <- next_seq_name
              next_seq_name <- synth_fac$get_character_sequence_name()
            }
            
            result <- list(next_seq_name, next_it_id, params)
            
            l$o$err$set_assertions_status(old_assert_status)
            return(result) 
          }
        
          
          result <- l$o$ait$Synthetic_Datastream_Iterators$new(assertions_status = assertions_status,
                                                           synthetic_datastream_iterator_factory = synth_fac,
                                                           numeric_iterators_num = numeric_iterators_num,
                                                           character_iterators_num = character_iterators_num,
                                                           individual_subparams = individual_subparams,
                                                           noise_decorator_params = noise_decorator_params,
                                                           delay_decorator_params = delay_decorator_params,
                                                           num_concat_iterator_params = num_concat_iterator_params,
                                                           ground_truth_name = ground_truth_name,
                                                           idle_event_name = idle_event_name,
                                                           get_next_event_func = next_event_getter,
                                                           NA_label = NA_label,
                                                           #min_num_NA_padding_elements = min_num_NA_padding_elements,
                                                           #max_num_NA_padding_elements = max_num_NA_padding_elements,
                                                           .debug_mode = debug_mode,
                                                           .verbose = verbose,
                                                           output_as_frame = output_as_frame,
                                                           .NA_character_sequence_label = NA_character_sequence_label,
                                                           .padding_NA_character_sequence_label = padding_NA_character_sequence_label,
                                                           .inter_NA_character_sequence_label = inter_NA_character_sequence_label,
                                                           .intra_NA_character_sequence_label = intra_NA_character_sequence_label,
                                                           .NA_rnd_walk_label = NA_rnd_walk_label,
                                                           .padding_NA_rnd_walk_label = padding_NA_rnd_walk_label,
                                                           .inter_NA_rnd_walk_label = inter_NA_rnd_walk_label,
                                                           .intra_NA_rnd_walk_label = intra_NA_rnd_walk_label,
                                                           rnd_seed = synth_ds_rnd_seed)
          
          assign("synthetic_datastream", result, envir = l$created_structures_env)
          return(result)
        }
        
        create_ds_visualization_from_json <- function(json_file = NULL,
                                                      json_string = NULL,
                                                      json_obj = NULL){
          json_obj <- l$create_json_obj(json_file = json_file,
                                        json_string = json_string,
                                        json_obj = json_obj)
          
          assertions_status <- json_obj[["general"]][["assertions_status"]]
          
          old_assert_status <- l$o$err$get_assertions_status()
          l$o$err$set_assertions_status(assertions_status)
          
          numeric_iterators_num <- json_obj[["general"]][["numeric_iterators_num"]]
          character_iterators_num <- json_obj[["general"]][["character_iterators_num"]]
          timestamp_name <- json_obj[["general"]][["timestamp_name"]]
          ground_truth_name <- json_obj[["general"]][["ground_truth_name"]]
          idle_event_name <- json_obj[["general"]][["idle_event_name"]]
          NA_label <- json_obj[["general"]][["NA_label"]]
          
          replot_min_interval <- json_obj[["ds_visualization"]][["replot_min_interval"]]
          max_x_progress <- json_obj[["ds_visualization"]][["max_x_progress"]]
          x_axis_range <- json_obj[["ds_visualization"]][["x_axis_range"]]
          graph_labels <- json_obj[["ds_visualization"]][["graph_labels"]]
          y_axis_labels <- json_obj[["ds_visualization"]][["y_axis_labels"]]
          plot_width <- json_obj[["ds_visualization"]][["plot_width"]]
          plot_height <- json_obj[["ds_visualization"]][["plot_height"]]
          plot_height_per_graph <- json_obj[["ds_visualization"]][["plot_height_per_graph"]]
          window_label <- json_obj[["ds_visualization"]][["window_label"]]
          
          color_rnd_seed <- json_obj[["ds_visualization"]][["color_rnd_seed"]]
          
         
          first_plot_out_path <- json_obj[["ds_visualization"]][["first_plot_out_path"]]
         
          params <- list(assertions_status = assertions_status,
                         numeric_iterators_num = numeric_iterators_num,
                         character_iterators_num = character_iterators_num,
                         NA_label = NA_label,
                         replot_min_interval = replot_min_interval,
                         max_x_progress = max_x_progress,
                         x_axis_range = x_axis_range,
                         graph_labels = graph_labels,
                         y_axis_labels = y_axis_labels,
                         ground_truth_name = ground_truth_name,
                         timestamp_name = timestamp_name,
                         plot_width = plot_width,
                         plot_height = plot_height,
                         plot_height_per_graph = plot_height_per_graph,
                         window_label = window_label,
                         color_rnd_seed = color_rnd_seed,
                         first_plot_out_path = first_plot_out_path)
          
          if("first_plot_start" %in% names(json_obj[["ds_visualization"]])){
            first_plot_start <- json_obj[["ds_visualization"]][["first_plot_start"]]
            if(!is.null(first_plot_start)){
              params <- append(params, list(first_plot_start = first_plot_start))
            }
          }
           
          result <- do.call(l$o$ait$Datastream_Visualization$new, args = params)
          
          l$o$err$set_assertions_status(old_assert_status)
          assign("synthetic_datastream_visualization", result, envir = l$created_structures_env)
          return(result)
        }
        
        create_multivar_char_vocabulary_from_json <- function(json_file = NULL,
                                                              json_string = NULL,
                                                              json_obj = NULL){
          json_obj <- l$create_json_obj(json_file = json_file,
                                      json_string = json_string,
                                      json_obj = json_obj)
          
          assertions_status <- json_obj[["general"]][["assertions_status"]]
          recreate_voc <- json_obj[["general"]][["recreate_voc"]]
          
          voc_path <- json_obj[["multivar_char_voc"]][["json_file"]]
          if(recreate_voc){
            numeric_iterators_num <- json_obj[["general"]][["numeric_iterators_num"]]
            character_iterators_num <- json_obj[["general"]][["character_iterators_num"]]
            num_its <- numeric_iterators_num + character_iterators_num
            
            idle_event_name <- json_obj[["general"]][["idle_event_name"]]
            char_voc <- unlist(l$create_char_voc(json_obj[["multivar_char_voc"]], assertions_status))
            
            words_per_dim_weights <- json_obj[["multivar_char_voc"]][["words_per_dim_weights"]]
            if(length(words_per_dim_weights) == 1 && num_its > 1){
              words_per_dim_weights <- sapply(1:num_its,
                                          FUN = function(i) return(unlist(words_per_dim_weights)))
            }
            
            max_words_per_dim <- json_obj[["multivar_char_voc"]][["max_words_per_dim"]]
            if(length(max_words_per_dim) == 1 && num_its > 1){
              max_words_per_dim <- sapply(1:num_its,
                                              FUN = function(i) return(unlist(max_words_per_dim)))
            }
            
            min_words_per_dim <- json_obj[["multivar_char_voc"]][["min_words_per_dim"]]
            if(length(min_words_per_dim) == 1 && num_its > 1){
              min_words_per_dim <- sapply(1:num_its,
                                              FUN = function(i) return(unlist(min_words_per_dim)))
            }
            
            voc_rnd_seed <- json_obj[["multivar_char_voc"]][["voc_rnd_seed"]]
            
            sequences_file <- json_obj[["synth_ds_its_factory"]][["json_file"]]
            synth_fac <- l$o$ait$Synthetic_Datastream_Iterator_Factory$new(assertions_status = assertions_status,
                                                                       json_str = NULL, 
                                                                       json_file = sequences_file,
                                                                       json_obj = NULL)
            num_voc <- unlist(synth_fac$get_non_special_event_names())
            
            word_listlist <- list()
            
            if(!is.null(voc_rnd_seed)){
              glob_env <- globalenv()
              old_rnd_state <- get(x = ".Random.seed", envir = glob_env)
              set.seed(voc_rnd_seed)
            }
            
            if(numeric_iterators_num > 0){
              stopifnot(!is.numeric(num_voc))
              
              word_listlist <- append(word_listlist,
                                      l$o$dts$partition_data(
                                        data = num_voc, 
                                        partition_weights = words_per_dim_weights[1:numeric_iterators_num], 
                                        min_elems_per_partition = min_words_per_dim[1:numeric_iterators_num],
                                        max_elems_per_partition = max_words_per_dim[1:numeric_iterators_num],
                                        check_args = assertions_status,
                                        allow_subset_result = TRUE
                                      ))
            }
            
            if(character_iterators_num > 0){
              stopifnot(!is.numeric(char_voc))
              
              word_listlist <- append(word_listlist,
                                      l$o$dts$partition_data(
                                        data = char_voc, 
                                        partition_weights = words_per_dim_weights[(1 + numeric_iterators_num):num_its], 
                                        min_elems_per_partition = min_words_per_dim[(1 + numeric_iterators_num):num_its],
                                        max_elems_per_partition = max_words_per_dim[(1 + numeric_iterators_num):num_its],
                                        check_args = assertions_status,
                                        allow_subset_result = TRUE
                                      ))
            }
            allow_word_overlaps = json_obj[["multivar_char_voc"]][["allow_word_overlaps"]]
            
            result <- l$o$lm$Multivariate_Character_Vocabulary$new(assertions_status = assertions_status,
                                                               json_file = NULL,
                                                               json_string = NULL,
                                                               word_listlist = word_listlist,
                                                               idle_word = idle_event_name,
                                                               allow_word_overlaps = allow_word_overlaps)
            
            if(!is.null(voc_path)){
              result$write_vocabulary(json_file = voc_path, 
                                      overwrite = TRUE)
            }
            
            if(!is.null(voc_rnd_seed)){
              assign(x = ".Random.seed", value = old_rnd_state, envir = glob_env)
            }
          }
          else{
            result <- l$o$lm$Multivariate_Character_Vocabulary$new(assertions_status = assertions_status,
                                                               json_file = voc_path,
                                                               json_string = NULL,
                                                               word_listlist = NULL,
                                                               idle_word = NULL)
          }
          
          assign("vocabulary", result, envir = l$created_structures_env)
          
          return(result)
        }
        
        create_trainer_multiv_count_lm_from_json <- function(json_file = NULL,
                                                             json_string = NULL,
                                                             json_obj = NULL){
          json_obj <- l$create_json_obj(json_file = json_file,
                                        json_string = json_string,
                                        json_obj = json_obj)
          
          assertions_status <- json_obj[["general"]][["assertions_status"]]
          vocabulary <- result_env$create_multivar_char_vocabulary_from_json(json_file = NULL,
                                                                             json_string = NULL,
                                                                             json_obj = json_obj)
          max_history_length <- json_obj[["trainer_multivar_count_lm"]][["max_history_length"]]
          simple_smooth_factor <- json_obj[["trainer_multivar_count_lm"]][["simple_smooth_factor"]]
          max_num_edge_insert_iterations <- json_obj[["trainer_multivar_count_lm"]][["max_num_edge_insert_iterations"]]
          max_num_edge_weight_transitions <- json_obj[["trainer_multivar_count_lm"]][["max_num_edge_weight_transitions"]]
          transition_to_new_state_weight <- json_obj[["trainer_multivar_count_lm"]][["transition_to_new_state_weight"]]
          transition_to_existing_state_weight <- json_obj[["trainer_multivar_count_lm"]][["transition_to_existing_state_weight"]]
          min_transition_weight <- json_obj[["trainer_multivar_count_lm"]][["min_transition_weight"]]
          max_transition_weight <- json_obj[["trainer_multivar_count_lm"]][["max_transition_weight"]]
          
          first_plot_out_path <- json_obj[["trainer_multivar_count_lm"]][["first_plot_out_path"]]
          
          result <- l$o$lm$Trainer_Multivariate_Count_LM$new(assertions_status = assertions_status,
                                                         vocabulary = vocabulary,
                                                         
                                                         max_history_length = max_history_length,
                                                         simple_smooth_factor = simple_smooth_factor,
                                                         first_plot_out_path = first_plot_out_path,
                                                         max_num_edge_insert_iterations = max_num_edge_insert_iterations,
                                                         max_num_edge_weight_transitions = max_num_edge_weight_transitions,
                                                         transition_to_new_state_weight = transition_to_new_state_weight,
                                                         transition_to_existing_state_weight = transition_to_existing_state_weight,
                                                         min_transition_weight = min_transition_weight,
                                                         max_transition_weight = max_transition_weight)
          
          assign("language_model_trainer", result, envir = l$created_structures_env)
          
          return(result)
        }
        
        create_empty_multiv_count_lm_from_json <- function(json_file = NULL,
                                                          json_string = NULL,
                                                          json_obj = NULL){
          json_obj <- l$create_json_obj(json_file = json_file,
                                        json_string = json_string,
                                        json_obj = json_obj)

          assertions_status <- json_obj[["general"]][["assertions_status"]]
          idle_word <- json_obj[["general"]][["idle_event_name"]]
          num_numeric <- json_obj[["general"]][["numeric_iterators_num"]]
          num_character <- json_obj[["general"]][["character_iterators_num"]]
          numeric_words <- json_obj[["language_model"]][["num_voc"]]
          l$o$err$assert_msg("There should be two numeric words.",
                             length(numeric_words) == 2)
          character_words <- l$create_char_voc(json_obj[["language_model"]], assertions_status)
          max_history <- json_obj[["language_model"]][["max_history_length"]]
          smooth_factor <- json_obj[["language_model"]][["simple_smooth_factor"]]
          word_listlist <- vector(mode = "list", length = num_numeric + num_character)
          l$o$err$assert_msg("Zero dimensions.",
                             num_numeric + num_character > 0)
          for(i in 1:length(word_listlist)){
            if(i <= num_numeric){
              word_listlist[[i]] <- as.list(numeric_words)
            }
            else{
              word_listlist[[i]] <- as.list(character_words)
            }
          }
          
          vocabulary <- l$o$lm$Multivariate_Character_Vocabulary$new(
            assertions_status = assertions_status,                                  
            word_listlist = word_listlist,
            idle_word = idle_word,
            allow_word_overlaps = TRUE)
          
          assign("empty_lm_vocabulary", vocabulary, envir = l$created_structures_env)
          
          result <- l$o$lm$Multivariate_Count_LM$new(assertions_status = assertions_status,
                                                 vocabulary = vocabulary,
                                                 max_history_length = max_history,
                                                 simple_smooth_factor = smooth_factor)
          assign("empty_language_model", result, envir = l$created_structures_env)
          return(result)
        }
        
        create_multiv_count_lm_from_json <- function(json_file = NULL,
                                                     json_string = NULL,
                                                     json_obj = NULL){
          json_obj <- l$create_json_obj(json_file = json_file,
                                        json_string = json_string,
                                        json_obj = json_obj)
          assertions_status <- json_obj[["general"]][["assertions_status"]]
          recreate_lm <- json_obj[["general"]][["recreate_lm"]]
          lm_path <- json_obj[["multivar_count_lm"]][["json_file"]]
          plot_lm <- json_obj[["general"]][["plot_lm"]]
          
          first_plot_out_path <- json_obj[["multivar_count_lm"]][["first_plot_out_path"]]
          
          if(recreate_lm){
            lm_trainer <- result_env$create_trainer_multiv_count_lm_from_json(json_file = NULL,
                                                                              json_string = NULL,
                                                                              json_obj = json_obj)
            train_lm_rnd_seed <- json_obj[["trainer_multivar_count_lm"]][["train_lm_rnd_seed"]]
            result <- lm_trainer$train_new_lm(rnd_seed = train_lm_rnd_seed, 
                                    lock = TRUE)
            
            if(!is.null(lm_path)){
              result$write_lm(json_file = lm_path, 
                              overwrite = TRUE)
            }
          }
          else{
            result <- l$o$lm$Multivariate_Count_LM$new(assertions_status = assertions_status,
                                                   vocabulary = NULL,
                                                   max_history_length = NULL,
                                                   simple_smooth_factor = NULL,
                                                   json_file = lm_path,
                                                   json_string = NULL)
            assign("vocabulary", result$get_vocabulary(), envir = l$created_structures_env)
          }
          
          result$lock()
            
          if(plot_lm){
            plot_lm_rnd_seed <- json_obj[["multivar_count_lm"]][["plot_lm_rnd_seed"]]
            plot_width <- json_obj[["multivar_count_lm"]][["plot_width"]]
            plot_height <- json_obj[["multivar_count_lm"]][["plot_height"]]
            plot_state_names <- json_obj[["multivar_count_lm"]][["plot_state_names"]]
            
            result$plot_raw_adjacency_matrix_igraph (rnd_seed = plot_lm_rnd_seed,
                                                     plot_width = plot_width,
                                                     plot_height = plot_height,
                                                     plot_state_names = plot_state_names)
          }
          
          assign("language_model", result, envir = l$created_structures_env)
          
          return(result)
        }
        
        visualize_synth_ds_its <- function(json_file = NULL,
                                           json_string = NULL,
                                           json_obj = NULL){
          json_obj <- l$create_json_obj(json_file = json_file,
                                        json_string = json_string,
                                        json_obj = json_obj)
          
          assertions_status <- json_obj[["general"]][["assertions_status"]]
          
          synth_ds <- result_env$create_synth_ds_its_from_json(json_file = NULL,
                                                               json_string = NULL,
                                                               json_obj = json_obj)
          
          ds_visualization <- result_env$create_ds_visualization_from_json(json_file = NULL,
                                                                           json_string = NULL,
                                                                           json_obj = json_obj)
          
          old_assert_status <- l$o$err$get_assertions_status()
          l$o$err$set_assertions_status(assertions_status)
          
          num_its <- synth_ds$get_num_iterators()
          if(num_its > 0){
            vis <- FALSE
            while(!vis){
              next_elems <- synth_ds$get_next_all()
              l$o$err$assert_msg("Inconsistent state.",
                                 length(next_elems) == num_its)
              ds_visualization$add_data(elements = next_elems, 
                                        dimensions = 1:num_its, 
                                        replot = FALSE)
              
              if(ds_visualization$replot_possible()){
                ds_visualization$replot()
                vis <- TRUE
              }
            }
            l$o$err$set_assertions_status(old_assert_status)
          }
        }
        
        set_console_output <- function(console_file = NULL){
          if(is.null(console_file)){
            sink() 
            sink(type="message")
          }
          else{
            dir <- dirname(console_file)
            if(!dir.exists(dir)){
              dir.create(path = dir, showWarnings = FALSE, recursive = TRUE)
            }
            con <- file(console_file)
            sink(con, append=TRUE)
            sink(con, append=TRUE, type="message")
          }
        }
      },
      envir = result_env)
    lockEnvironment(local_env, bindings = TRUE)
    invisible(result_env)
  }
} # End of include guard