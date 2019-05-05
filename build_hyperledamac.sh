#!/bin/bash 

# make the hyperleda dmg installer

basedir=/Volumes/TmpInst/hyperleda   # Be sure this is set to a non existent directory, it is removed after the run!

mkdir -p $basedir


wd=`pwd`

  cp system_integration/MacOSX/hyperleda.pkgproj $basedir
  cd $basedir
  mkdir CCDciel
  mkdir CCDciel/data
  mkdir CCDciel/data/dso
  cd CCDciel/data/dso
  curl -L -o hyperleda.tar.xz http://sourceforge.net/projects/ccdciel/files/hyperleda/hyperleda_ccdciel.tar.xz
  tar xf hyperleda.tar.xz
  rm hyperleda.tar.xz
  mv hyperleda_ccdciel/share/ccdciel/data/dso/hyperleda.csv .
  rmdir hyperleda_ccdciel/share/ccdciel/data/dso
  rmdir hyperleda_ccdciel/share/ccdciel/data
  rmdir hyperleda_ccdciel/share/ccdciel
  rmdir hyperleda_ccdciel/share
  rmdir hyperleda_ccdciel
  cd $basedir
  packagesbuild -v hyperleda.pkgproj
  if [[ $? -ne 0 ]]; then exit 1;fi
  hdiutil create -anyowners -volname hyperleda-ccdciel -imagekey zlib-level=9 -format UDZO -srcfolder ./build hyperleda-ccdciel.dmg
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv hyperleda-ccdciel.dmg $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $basedir

