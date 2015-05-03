#!/bin/bash

OS_TARGET=$1
destdir=$2

if [ -z "$OS_TARGET=" ]; then
   export OS_TARGET==win32
fi

if [ -z "$destdir" ]; then
   export destdir=/tmp/ccdciel
fi

echo Install ccdciel $OS_TARGET to $destdir

install -m 755 -d $destdir

if [ $OS_TARGET = win32 ]; then 
  strip -v -o $destdir/ccdciel.exe src/ccdciel.exe 
fi
if [ $OS_TARGET = win64 ]; then
  strip -v -o $destdir/ccdciel.exe src/ccdciel.exe 
fi

