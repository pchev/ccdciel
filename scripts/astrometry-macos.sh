#!/bin/bash

# Example script to run astrometry.net on Mac OS X
#
# Install "Astrometry for OS X" from http://www.cloudmakers.eu/astrometry/
#
# Path to astrometry binaries:
export PATH=$PATH:/Applications/Astrometry.app/Contents/MacOS/
# Configuration file to define the data path:
cfg="$HOME/Library/Application Support/Astrometry/astrometry.cfg"
# Required options because of missing modules:
opt="--no-fits2fits --no-plots --no-remove-lines --no-verify-uniformize --sort-column FLUX --uniformize 0"

# extract parameters
param="${@:2}"
# extract file name
fn="${1}"
# source list file name
fxy="${fn/fits/xy}"

# extract sources
rm $fxy
image2xy -O -o $fxy $fn
if [ -e $fxy ]; then
  # solve
  solve-field $param $opt --config "$cfg"  $fxy
fi

