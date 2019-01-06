#!/bin/bash

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

# Configures and executes an SDG instance based on
# the JSON config file at './configs/synth_ds_global_options.json' where '.' represents the folder
# containing this script.

# Helper method for error printing
error_msg()
{
    TITLE="Unsuccessful execution of this script via: \"$0 $CONSOLE_PARAMS\""
    "$PRINTF" "ERROR: $TITLE\n\tMessage: $1\n"
}
CONSOLE_PARAMS=$@

# Check tool availability
DIRNAME=$(which dirname)
READLINK=$(which readlink)
PRINTF=$(which printf)
RSCRIPT=$(which Rscript)

if [ -z "$DIRNAME" -o -z "$READLINK" -o -z "$PRINTF" -o -z "$RSCRIPT" ]; then
  error_msg "At least one required tool is missing. See \"Check tool availability\" paragraph of \"$0\" for more details."
  exit 1
fi

# Folder containing this script
DRIVER_CONFIG_FOLDER=$("$DIRNAME" "$0")
DRIVER_CONFIG_FOLDER=$("$READLINK" -e "$DRIVER_CONFIG_FOLDER")

# Driver config R script
DRIVER_CONFIG="${DRIVER_CONFIG_FOLDER}/execute.R"
# Composed commands to execute
EXEC_COMMAND="\"$RSCRIPT\" \"$DRIVER_CONFIG\""

# Execute commands
eval "$EXEC_COMMAND"
