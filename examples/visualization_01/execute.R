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
#' @title SDG usage example based on configuration files in the `config` subfolder of the folder containing this script. 
#' 
#' @description Creates a new environment `execution_env`.
#' In `execution_env`, defines a parameter list `parameters` and
#' defines and executes the following three methods (in that order):
#' 
#' 1. `init()`:  Installs (if necessary) and loads required packages, 
#' sets working directory to the SDG root folder, 
#' redirects `stdout` and `stderr` output to a file if `parameters$print_to_sink` is set.
#' 2. `main()`:
#'     1. Create a driver environment `d` containing all driver methods (from `./drivers/driver.R`).
#'     2. Via `d`:
#'         1. Create instance `s` of `Synthetic_Datastream_Iterators` based on config JSON file `synth_ds_global_options.json`
#'         from the `config` subfolder of the folder containing this script.
#'         2. Create instance `v` of `Datastream_Visualization` based on the same JSON config file as `s`.
#'     3. Iteratively retrieve new data from `s` and add it to the visualization instance `v` until the latter has enough data elements
#'     to plot over the whole current x-axis range on all dimensions.
#'     4. When 3. is finished, actually print the plot via `v$replot()`.
#' 3. `cleanup()`: Restore `stdout` and `stderr` if `parameters$print_to_sink` is set.
#' @md
execution_env <- new.env()

evalq({
  parameters <- list(
    #' Set to `TRUE` if `stderr` and `stdout` shall be forwarded to a log file (in the same folder as this script)
    print_to_sink = FALSE
  )
  
  init <- function(){
    # tools for 'file_path_sans_ext'
    packages <- c("rprojroot", "tools")
    for(pkg in packages){
      if(!require(pkg, character.only = TRUE)){
        install.packages(pkg, dependencies = TRUE, repos = "http://cran.us.r-project.org")
        library(pkg, character.only = TRUE)
      }
    }
    
    thisfile1 <- normalizePath(thisfile())
    thisfolder <- dirname(thisfile1)
    parameters[["thisfolder"]] <<- thisfolder
    dir <- file.path(thisfolder, "..", "..")
    cat(paste0("Setting working directory to \"", dir, "\"\n"))
    setwd(dir)
    
    options(error = function() traceback(2))
    
    if(parameters$print_to_sink){
      log_file_name <- paste0(file_path_sans_ext(thisfile1), ".log")
      f <<- file(log_file_name, open = "wt")
      sink(file = f)
      sink(file = f, type = "message")
    }
  }
  
  main <- function(){
    source('./drivers/driver.R', local = TRUE)
    
    #' Create driver environment and instances of `Synthetic_Datastream_Iterators` and `Datastream_Visualization`
    d <- get_driver_env()
    path <- file.path(parameters$thisfolder, "config", "synth_ds_global_options.json")
    s <- d$create_synth_ds_its_from_json(json_file = path)
    v <- d$create_ds_visualization_from_json(json_file = path)
    
    stopifnot(s$get_num_iterators() > 0)
    
    num <- rep(x=0, times = s$get_num_iterators())
    max_ts <- rep(x=0, times = s$get_num_iterators())
    
    #' Replot only possible if for all dimensions `d`: Timestamp of last generated data element in dimension `d` is >= `next_x_axis_max`
    next_x_axis_max <- format(v$get_next_x_axis_max(), scientific = F, drop0trailing = T, trim = T)
    
    #' Progress output
    cat(paste0("\ntotal # of generated data elements per dimension:\tlargest timestamp in each dimension / lower bound timestamp required for next replot\n"))
    cat(paste0(toString(format(num, scientific = F, drop0trailing = T, trim = T)), "\t\t\t(", 
               toString(format(max_ts, scientific = F, drop0trailing = T, trim = T)), ") / ", next_x_axis_max, "\n"))
    
    while(!v$replot_possible()){
      data <- s$get_next_all()
      v$add_data(elements = data, dimensions = NULL, replot = FALSE)
      
      #' Values for progress output
      num <- num + sapply(data, FUN = function(subdata) {return (nrow(subdata))})
      max_ts <- sapply(1:s$get_num_iterators(), 
                       FUN = function(i) {
                         if(nrow(data[[i]]) == 0) {
                           return(max_ts[[i]])
                         }
                         else{
                           return(round(tail(data[[i]], n = 1)[[2]]))
                         }
                       })
      cat(paste0(toString(format(num, scientific = F, drop0trailing = T, trim = T)), "\t\t\t(", 
                 toString(format(max_ts, scientific = F, drop0trailing = T, trim = T)), ") / ", next_x_axis_max, "\n"))
    }
    v$replot()  
  }
  
  cleanup <- function(){
    if(parameters$print_to_sink){
      sink(type = "message")
      sink()
      close(f)
    }
  }
}, envir = execution_env)

execution_env$init()
execution_env$main()
execution_env$cleanup()
