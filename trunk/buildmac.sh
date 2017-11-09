#!/bin/bash 

version=0.9.13

basedir=/tmp/ccdciel   # Be sure this is set to a non existent directory, it is removed after the run!

builddir=$basedir/ccdciel

if [[ -n $1 ]]; then
  configopt="fpc=$1"
fi
if [[ -n $2 ]]; then
  configopt=$configopt" lazarus=$2"
fi

wd=`pwd`

currentrev=$(LANG=C svn info . | grep Revision: | sed 's/Revision: //')

# delete old files
  rm ccdciel*.dmg
  rm -rf $basedir

# make x86_64 Mac version
  ./configure $configopt prefix=$builddir target=x86_64-darwin
  if [[ $? -ne 0 ]]; then exit 1;fi
  make clean
  make
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install
  if [[ $? -ne 0 ]]; then exit 1;fi
  # pkg
  sed -i.bak "18s/1.0/$version/"  $builddir/ccdciel.app/Contents/Info.plist
  rm $builddir/ccdciel.app/Contents/Info.plist.bak
  macdeployqt $builddir/ccdciel.app
  cp system_integration/MacOSX/ccdciel.packproj $basedir
  cp system_integration/MacOSX/readme.txt $basedir
  cd $basedir
  mv ccdciel "CCDciel"
  freeze -v ccdciel.packproj
  if [[ $? -ne 0 ]]; then exit 1;fi
  hdiutil create -anyowners -volname ccdciel-$version-$currentrev-x86_64-macosx -imagekey zlib-level=9 -format UDZO -srcfolder ./build ccdciel-$version-$currentrev-x86_64-macosx.dmg
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv ccdciel*.dmg $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $basedir

