#!/bin/bash

# Example script to run astrometry.net blind, ignoring all the other parameters set by CCDciel
# Parse $param to get the parameters that can be added selectively to the command
# There is two example of command, uncomment the one you need and add any other option required by you setup


if [ $# -ne 2 ]; then
  echo wrong number of parameters
  exit 1
fi

# arguments from CCDciel, the first with the filename, the second with all the other options for solve-field
infile=$1
param=$2

# split solve-field argument and declare a variable for each one
set -- $param
while [ $# -gt 0 ]; do
   if [[ $1 == *"--"* &&     # option start with --
      $2 != *"--"* &&        # next string is value, not another option
      $2 != "" ]]; then      # value is not empty
        v="${1/--/}"         # remove -- from name
        v="${v/-/}"          # variable name cannot contain -
        declare $v="$2"      # set the new variable
#        echo $v="$2"         # uncomment to list available variable
   fi
  shift
done

## real blind solving without any options
#echo "Use blind solving"
#echo "solve-field --overwrite --no-plots $infile"
#solve-field --overwrite --no-plots $infile

# half blind solving using scale but no coordinates
echo "Use half blind solving"
echo "solve-field --overwrite --no-plots --scale-low $scalelow --scale-high $scalehigh --scale-units $scaleunits $infile"
solve-field --overwrite --no-plots --scale-low $scalelow --scale-high $scalehigh --scale-units $scaleunits $infile

exit $?
