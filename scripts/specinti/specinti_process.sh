#!/bin/bash

# This batch file is started by specinti_process.script to process the spectra in background and save the result

specintiprogdir=${1}
specintiprog=${2}
specinticonfig=${3}
obj=${4}
workdir=${5}
capturedir=${6}
resultdir=${7}
archivedir=${8}
logfile=${9}
wine_prefix=${10}

if [[ -z $logfile ]]; then 
  echo Missing parameters
  sleep 5
  exit 1; 
fi

echo "$(date) : Start processing for $obj" >> "$logfile"

if [[ -n $wine_prefix ]]; then export WINEPREFIX="$wine_prefix"; fi

cd "$specintiprogdir"
wine "$specintiprog" "$specinticonfig"

if [[ $? = 0 ]]; then 
  lobj=$(echo $obj | tr [A-Z] [a-z] )
  mv "$workdir/_${lobj/ }"* "$resultdir"
  if [[ $archivedir != "none" ]]; then
    mv "$capturedir/$obj"* "$archivedir"
  fi
  echo "$(date) : Finish processing of $obj" >> "$logfile"
else
  echo "$(date) : Error $obj" >> "$logfile"
fi

echo Close in 30 sec ...
sleep 30
