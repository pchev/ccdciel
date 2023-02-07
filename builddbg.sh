#!/bin/bash 

version=$(grep 'ccdcielver' src/u_global.pas |head -1| cut -d\' -f2)

builddir=/tmp/ccdciel  # Be sure this is set to a non existent directory, it is removed after the run!

arch=$(arch)

# adjuste here the target you want to crossbuild
# You MUST crosscompile Freepascal and Lazarus for this targets! 

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
  make CPU_TARGET=x86_64 OS_TARGET=linux clean
  make fpcopts="-O1 -g -gl"  CPU_TARGET=x86_64 OS_TARGET=linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make installdbg
  if [[ $? -ne 0 ]]; then exit 1;fi
  # tar
  cd $builddir
  cd ..
  tar cvJf ccdciel-$version-$currentrev-linux_x86_64_dbg.tar.xz ccdciel
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv ccdciel*.tar.xz $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # deb
  cd $wd
  rsync -a --exclude=.svn system_integration/Linux/debian $builddir
  cd $builddir
  mkdir debian/ccdciel64/usr/
  mv bin debian/ccdciel64/usr/
  mv share debian/ccdciel64/usr/
  cd debian
  sz=$(du -s ccdciel64/usr | cut -f1)
  sed -i "s/%size%/$sz/" ccdciel64/DEBIAN/control
  sed -i "/Version:/ s/3/$version-$currentrev-dbg/" ccdciel64/DEBIAN/control
  fakeroot dpkg-deb -Zxz --build ccdciel64 .
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv ccdciel*.deb $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $builddir

cd $wd
rm -rf $builddir

