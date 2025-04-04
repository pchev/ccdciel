#!/bin/bash 

version=$(grep 'ccdcielver' src/u_global.pas |head -1| cut -d\' -f2)

basedir=/tmp/ccdciel   # Be sure this is set to a non existent directory, it is removed after the run!

builddir=$basedir/ccdciel

if [[ -n $1 ]]; then
  configopt="fpc=$1"
fi
if [[ -n $2 ]]; then
  configopt=$configopt" lazarus=$2"
fi

wd=`pwd`

currentrev=$(git rev-list --count --first-parent HEAD)

# delete old files
  rm ccdciel-*-arm64.dmg
  rm -rf $basedir

# make arm64  Mac version
  ./configure $configopt prefix=$builddir target=aarch64-darwin
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=aarch64 clean
  make CPU_TARGET=aarch64
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install CPU_TARGET=aarch64
  if [[ $? -ne 0 ]]; then exit 1;fi
  # pkg
  sed -i.bak "18s/1.0/$version/"  $builddir/ccdciel.app/Contents/Info.plist
  rm $builddir/ccdciel.app/Contents/Info.plist.bak
  cp system_integration/MacOSX/ccdcielarm.pkgproj $basedir
  cp system_integration/MacOSX/readme.txt $basedir
  cd $basedir
  sed -i.bak "s/ccdciel_version/$version/g" ccdcielarm.pkgproj 
  rm ccdcielarm.pkgproj.bak
  sed -i.bak "s/ccdciel_version/$version/" readme.txt
  rm readme.txt.bak
  mv ccdciel "CCDciel"
  packagesbuild -v ccdcielarm.pkgproj
  if [[ $? -ne 0 ]]; then exit 1;fi
  cp readme.txt build/
  hdiutil create -anyowners -volname ccdciel-$version-$currentrev-macos-arm64 -imagekey zlib-level=9 -format UDZO -srcfolder ./build ccdciel-$version-$currentrev-macos-arm64.dmg
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv ccdciel*.dmg $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $basedir

