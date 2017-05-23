#!/bin/bash

# Example script to run astrometry.net on a remote host using ssh
#

# configure the remote host name
remotehost=127.0.0.1
# configure the temporary path in the remote host
remotepath=/tmp

if [ $# -ne 3 ]; then
  echo wrong number of parameters
  exit 1
fi

infile=$1
outfile=$2
param=$3

inf=$remotepath/${infile##*/}
outf=$remotepath/${outfile##*/}
solvf=${inf/.fits/.solved}
solvfile=${infile/.fits/.solved}

echo scp $infile $remotehost:$inf
scp $infile $remotehost:$inf
if [[ $? != 0 ]]; then echo error; exit ; fi

echo ssh $remotehost rm $solvf
ssh $remotehost rm $solvf

echo ssh $remotehost solve-field $param --new-fits $outf $inf
ssh $remotehost solve-field $param --new-fits $outf $inf
if [[ $? != 0 ]]; then echo error; exit ; fi

echo scp $remotehost:$outf $outfile
scp $remotehost:$outf $outfile
if [[ $? != 0 ]]; then echo error; exit ; fi

echo scp $remotehost:$solvf $solvfile
scp $remotehost:$solvf $solvfile
if [[ $? != 0 ]]; then echo error; exit ; fi
