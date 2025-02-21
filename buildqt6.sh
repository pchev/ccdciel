#!/bin/bash 
# 
# temporary script to build qt6 version with a specific lazarus version
#

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
  rm ccdciel*.xz
  rm ccdciel*.deb
  rm ccdciel*.rpm
  rm ccdciel*.zip
  rm ccdciel*.exe
  rm -rf $builddir


# make Linux x86_64 version
  ./configure $configopt prefix=$builddir target=x86_64-linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=x86_64 OS_TARGET=linux LCL_PLATFORM=qt6 clean
  make CPU_TARGET=x86_64 OS_TARGET=linux LCL_PLATFORM=qt6 opt_target=-k--build-id
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install
  if [[ $? -ne 0 ]]; then exit 1;fi
  # tar
  cd $builddir
  cd ..
  tar cvJf ccdciel-qt6-$version-$currentrev-linux_x86_64.tar.xz ccdciel
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv ccdciel*.tar.xz $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # deb
  cd $wd
  rsync -a --exclude=.svn system_integration/Linux/debian $builddir
  cd $builddir
  mkdir debian/ccdcielqt6/usr/
  mv bin debian/ccdcielqt6/usr/
  mv share debian/ccdcielqt6/usr/
  cd debian
  sz=$(du -s ccdcielqt6/usr | cut -f1)
  sed -i "s/%size%/$sz/" ccdcielqt6/DEBIAN/control
  sed -i "/Version:/ s/3/$version-$currentrev/" ccdcielqt6/DEBIAN/control
  fakeroot dpkg-deb -Zxz --build ccdcielqt6 .
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv ccdciel*.deb $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # rpm
  cd $wd
  rsync -a --exclude=.svn system_integration/Linux/rpm $builddir
  cd $builddir
  mkdir -p rpm/RPMS/x86_64
  mkdir -p rpm/RPMS/i386
  mkdir rpm/SRPMS
  mkdir rpm/tmp
  mkdir -p rpm/ccdciel/usr/
  mv debian/ccdcielqt6/usr/* rpm/ccdciel/usr/
  cd rpm
  sed -i "/Version:/ s/3/$version/"  SPECS/ccdcielqt6.spec
  sed -i "/Release:/ s/1/$currentrev/" SPECS/ccdcielqt6.spec
# rpm 4.7
  fakeroot rpmbuild  --buildroot "$builddir/rpm/ccdciel" --define "_topdir $builddir/rpm/" --define "_binary_payload w7.xzdio"  -bb SPECS/ccdcielqt6.spec
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv RPMS/x86_64/ccdciel*.rpm $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $builddir

