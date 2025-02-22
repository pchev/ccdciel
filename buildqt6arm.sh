#!/bin/bash 

version=$(grep 'ccdcielver' src/u_global.pas |head -1| cut -d\' -f2)
builddir=/tmp/ccdciel  # Be sure this is set to a non existent directory, it is removed after the run!

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
  rm ccdciel-qt6*.xz
  rm ccdciel-qt6*.deb
  rm -rf $builddir

# make Linux arm64 version
  ./configure $configopt prefix=$builddir target=aarch64-linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=aarch64 OS_TARGET=linux LCL_PLATFORM=qt6 clean
  make CPU_TARGET=aarch64 OS_TARGET=linux LCL_PLATFORM=qt6 opt_target=-k--build-id
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install CPU_TARGET=aarch64
  if [[ $? -ne 0 ]]; then exit 1;fi
  # tar
  cd $builddir
  cd ..
  tar cvJf ccdciel-qt6-$version-$currentrev-linux_arm64.tar.xz ccdciel
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv ccdciel*.tar.xz $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # deb
  cd $wd
  rsync -a --exclude=.svn system_integration/Linux/debian $builddir
  cd $builddir
  mkdir debian/ccdcielqt6arm/usr/
  mv bin debian/ccdcielqt6arm/usr/
  mv share debian/ccdcielqt6arm/usr/
  cd debian
  sz=$(du -s ccdcielqt6arm/usr | cut -f1)
  sed -i "s/%size%/$sz/" ccdcielqt6arm/DEBIAN/control
  sed -i "/Version:/ s/3/$version-$currentrev/" ccdcielqt6arm/DEBIAN/control
  fakeroot dpkg-deb -Zxz --build ccdcielqt6arm .
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv ccdciel*.deb $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $builddir

