#!/bin/bash

# Example script to run astrometry.net on a online api server.
#
# Tested with nova.astrometry.net and ANSVR
#
# script configuration :

# the server running the api
apiserver=https://nova.astrometry.net/api/

# get api key from the server i.e. http://nova.astrometry.net/
myapikey=$ccdcielapikey 

# python command, default is from CCDciel setup
pythoncmd=$ccdcielpython
# to use another python version:
#pythoncmd=/usr/bin/python3.11

# directory containing the client script
scriptdir=$ccdcielscriptdir

# the api client script (from https://github.com/dstndstn/astrometry.net/tree/master/net/client )
clientscript=$scriptdir/client.py 

# end of configuration

if [ $# -ne 2 ]; then
  echo wrong number of parameters
  exit 1
fi

infile=$1
param=$2
wcsfile=${infile/.fits/.wcs}
solvfile=${infile/.fits/.solved}

onlparam='--server='$apiserver' --apikey='$myapikey' --upload='$infile' --wait --private --wcs='$wcsfile 
# copy parameters to onlparam
set -- $param
while [[ "$#" > 1 ]]; do 
  case $1 in
    --scale-low) onlparam=$onlparam' --scale-lower='$2; shift ;;
    --scale-high) onlparam=$onlparam' --scale-upper='$2; shift ;;
    --scale-units) onlparam=$onlparam' --scale-units='$2; shift ;;
    --ra) onlparam=$onlparam' --ra='$2; shift ;;
    --dec) onlparam=$onlparam' --dec='$2; shift ;;
    --radius) onlparam=$onlparam' --radius='$2; shift ;;
    *) ;;
  esac; 
  shift;
done

echo $pythoncmd $clientscript $onlparam
$pythoncmd $clientscript $onlparam
rc=$?
if [[ $rc == 0 ]]; then
  echo 1 > $solvfile
fi
exit $rc
