#!/bin/bash 

version=$(grep 'ccdcielver' src/u_global.pas |head -1| cut -d\' -f2)

builddir=/tmp/ccdciel  # Be sure this is set to a non existent directory, it is removed after the run!

arch=$(arch)

# adjuste here the target you want to crossbuild
# You MUST crosscompile Freepascal and Lazarus for this targets! 

unset extratarget

unset make_linux32
unset make_linux64

if [[ $arch == i686 ]]; then 
   make_linux32=1
fi
if [[ $arch == x86_64 ]]; then 
   make_linux64=1
   extratarget=",x86_64-linux"
fi

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
  rm -rf $builddir

# make Linux i386 version
if [[ $make_linux32 ]]; then
  ./configure $configopt prefix=$builddir target=i386-linux$extratarget
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=i386 OS_TARGET=linux clean
  make CPU_TARGET=i386 OS_TARGET=linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install
  if [[ $? -ne 0 ]]; then exit 1;fi
  # tar
  cd $builddir
  cd ..
  tar cvJf ccdciel-$version-${currentrev}_linux_i386.tar.xz ccdciel
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv ccdciel*.tar.xz $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # deb
  cd $wd
  rsync -a --exclude=.svn system_integration/Linux/debian $builddir
  cd $builddir
  mkdir debian/ccdciel/usr/
  mv bin debian/ccdciel/usr/
  mv share debian/ccdciel/usr/
  cd debian
  sz=$(du -s ccdciel/usr | cut -f1)
  sed -i "s/%size%/$sz/" ccdciel/DEBIAN/control
  sed -i "/Version:/ s/3/$version-$currentrev/" ccdciel/DEBIAN/control
  fakeroot dpkg-deb -Zxz --build ccdciel .
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
  mv debian/ccdciel/usr/* rpm/ccdciel/usr/
  cd rpm
  sed -i "/Version:/ s/3/$version/"  SPECS/ccdciel.spec
  sed -i "/Release:/ s/1/$currentrev/" SPECS/ccdciel.spec
  setarch i386 fakeroot rpmbuild  --buildroot "$builddir/rpm/ccdciel" --define "_topdir $builddir/rpm/" --define "_binary_payload w7.xzdio" -bb SPECS/ccdciel.spec
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv RPMS/i386/ccdciel*.rpm $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $builddir
fi

# make Linux x86_64 version
if [[ $make_linux64 ]]; then
  ./configure $configopt prefix=$builddir target=x86_64-linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=x86_64 OS_TARGET=linux clean
  make CPU_TARGET=x86_64 OS_TARGET=linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install
  if [[ $? -ne 0 ]]; then exit 1;fi
  # tar
  cd $builddir
  cd ..
  tar cvJf ccdciel-$version-${currentrev}_linux_x86_64.tar.xz ccdciel
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
  sed -i "/Version:/ s/3/$version-$currentrev/" ccdciel64/DEBIAN/control
  fakeroot dpkg-deb -Zxz --build ccdciel64 .
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
  mv debian/ccdciel64/usr/* rpm/ccdciel/usr/
  cd rpm
  sed -i "/Version:/ s/3/$version/"  SPECS/ccdciel64.spec
  sed -i "/Release:/ s/1/$currentrev/" SPECS/ccdciel64.spec
# rpm 4.7
  fakeroot rpmbuild  --buildroot "$builddir/rpm/ccdciel" --define "_topdir $builddir/rpm/" --define "_binary_payload w7.xzdio"  -bb SPECS/ccdciel64.spec
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv RPMS/x86_64/ccdciel*.rpm $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $builddir
fi

