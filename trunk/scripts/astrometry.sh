#!/bin/bash

# Example script to run astrometry.net on a remote host using ssh
#

# configure the remote host name
remotehost=127.0.0.1
# configure the temporary path in the remote host
remotepath=/tmp


if [ $# -ne 2 ]; then
  echo wrong number of parameters
  exit 1
fi

infile=$1
param=$2

# local files
wcsfile=${infile/.fits/.wcs}
solvfile=${infile/.fits/.solved}

# remote files
inf=$remotepath/${infile##*/}
wcsf=${inf/.fits/.wcs}
solvf=${inf/.fits/.solved}

# copy input to remote
echo scp $infile $remotehost:$inf
scp $infile $remotehost:$inf
if [[ $? != 0 ]]; then echo error; exit ; fi

# delete remote result
echo ssh $remotehost rm $solvf
ssh $remotehost rm $solvf
echo ssh $remotehost rm $wcsf
ssh $remotehost rm $wcsf

# remote solve command
echo ssh $remotehost solve-field $param $inf
ssh $remotehost solve-field $param $inf
if [[ $? != 0 ]]; then echo error; exit ; fi

# copy result to local
echo scp $remotehost:$wcsf $wcsfile
scp $remotehost:$wcsf $wcsfile
if [[ $? != 0 ]]; then echo error; exit ; fi

echo scp $remotehost:$solvf $solvfile
scp $remotehost:$solvf $solvfile
if [[ $? != 0 ]]; then echo error; exit ; fi

