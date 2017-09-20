#!/bin/bash 

version=0.9.4

builddir=/tmp/ccdciel  # Be sure this is set to a non existent directory, it is removed after the run!

arch=$(arch)

# adjuste here the target you want to crossbuild
# You MUST crosscompile Freepascal and Lazarus for this targets! 

unset extratarget

make_linuxarm=1

if [[ -n $1 ]]; then
  configopt="fpc=$1"
fi
if [[ -n $2 ]]; then
  configopt=$configopt" lazarus=$2"
fi

save_PATH=$PATH
wd=`pwd`

currentrev=$(LC_ALL=C svn info . | grep Revision: | sed 's/Revision: //')

echo $version - $currentrev


# delete old files
  rm ccdciel*.bz2
  rm ccdciel*.deb
  rm -rf $builddir

# make Linux arm version
if [[ $make_linuxarm ]]; then
  ./configure $configopt prefix=$builddir target=arm-linux$extratarget
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=arm OS_TARGET=linux clean
  make CPU_TARGET=arm OS_TARGET=linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install
  if [[ $? -ne 0 ]]; then exit 1;fi
  # tar
  cd $builddir
  cd ..
  tar cvjf ccdciel-$version-$currentrev-linux_arm.tar.bz2 ccdciel
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv ccdciel*.tar.bz2 $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # deb
  cd $wd
  rsync -a --exclude=.svn system_integration/Linux/debian $builddir
  cd $builddir
  mv bin debian/ccdcielarm/usr/
  mv share debian/ccdcielarm/usr/
  cd debian
  sz=$(du -s ccdciel/usr | cut -f1)
  sed -i "s/%size%/$sz/" ccdcielarm/DEBIAN/control
  sed -i "/Version:/ s/3/$version-$currentrev/" ccdcielarm/DEBIAN/control
  fakeroot dpkg-deb -Zxz --build ccdcielarm .
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv ccdciel*.deb $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $builddir
fi

