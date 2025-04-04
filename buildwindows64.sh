#!/bin/bash 

version=$(grep 'ccdcielver' src/u_global.pas |head -1| cut -d\' -f2)

builddir=/tmp/ccdcielwin  # Be sure this is set to a non existent directory, it is removed after the run!
export WINEPREFIX=~/.wine
innosetup="C:\Program Files (x86)\Inno Setup 6\ISCC.exe"  # Install under Wine from http://www.jrsoftware.org/isinfo.php
wine_build="Z:\tmp\ccdcielwin" # Change to match builddir, Z: is defined in ~/.wine/dosdevices

arch=$(arch)

# adjuste here the target you want to crossbuild
# You MUST crosscompile Freepascal and Lazarus for this targets! 

extratarget=",x86_64-linux"

# For win32 and win64 target you must also install the corresponding mingw-w64 to build the C library
#mingw32=/opt/mingw-w32/bin/
#mingw64=/opt/mingw-w64/bin/

if [[ -n $1 ]]; then
  configopt="fpc=$1"
fi
if [[ -n $2 ]]; then
  configopt=$configopt" lazarus=$2"
fi

save_PATH=$PATH
wd=`pwd`

currentrev=$(git rev-list --count --first-parent HEAD)

echo $version - $currentrev

# delete old files
  rm ccdciel*x64.zip
  rm ccdciel*x64.exe
  rm -rf $builddir

# make Windows x86_64 version
  rsync -a --exclude=.svn system_integration/Windows/installer/ccdciel/* $builddir
  mkdir $builddir/Data
  mkdir $builddir/Prog
  export PATH=$mingw64:$save_PATH
  ./configure $configopt prefix=$builddir/Data target=x86_64-win64$extratarget
  if [[ $? -ne 0 ]]; then exit 1;fi
  make OS_TARGET=win64 CPU_TARGET=x86_64 clean
  make OS_TARGET=win64 CPU_TARGET=x86_64
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_win64
  if [[ $? -ne 0 ]]; then exit 1;fi
  # zip
  cd $builddir/Data
  zip -r  ccdciel-$version-$currentrev-windows-x64.zip *
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv ccdciel*.zip $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # exe
  cd $builddir
  sed -i "/AppVerName/ s/V3/V$version/" ccdciel_64.iss
  sed -i "/OutputBaseFilename/ s/windows-x64/$version-$currentrev-windows-x64/" ccdciel_64.iss
  sed -i "s/ccdciel_version/$version/" Presetup/readme.txt
  wine "$innosetup" "$wine_build\ccdciel_64.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/ccdciel*.exe $wd

cd $wd
rm -rf $builddir

