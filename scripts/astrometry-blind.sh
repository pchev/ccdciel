#!/bin/bash

# Example script to run astrometry.net blind, ignoring all the other parameters set by CCDciel
# parse $param if you need some specific parameters
#

if [ $# -ne 2 ]; then
  echo wrong number of parameters
  exit 1
fi

infile=$1
param=$2

echo "Use blind solving"
echo "solve-field --overwrite --no-plots $infile"

solve-field --overwrite --no-plots $infile

exit $?
