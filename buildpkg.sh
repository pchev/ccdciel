#!/bin/bash 

version=0.8.3

builddir=/tmp/ccdciel  # Be sure this is set to a non existent directory, it is removed after the run!
innosetup="C:\Program Files (x86)\Inno Setup 5\ISCC.exe"  # Install under Wine from http://www.jrsoftware.org/isinfo.php
wine_build="Z:\tmp\ccdciel" # Change to match builddir, Z: is defined in ~/.wine/dosdevices

arch=$(arch)

# adjuste here the target you want to crossbuild
# You MUST crosscompile Freepascal and Lazarus for this targets! 

unset extratarget

unset make_linux32
unset make_linux64
unset make_win32
unset make_win64

if [[ $arch == i686 ]]; then 
   make_linux32=1
fi
if [[ $arch == x86_64 ]]; then 
   make_linux64=1
   make_win32=1
#   make_win64=1
   extratarget=",x86_64-linux"
fi

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

currentrev=$(LC_ALL=C svn info . | grep Revision: | sed 's/Revision: //')

echo $version - $currentrev


# delete old files
  rm ccdciel*.xz
  rm ccdciel*.deb
  rm ccdciel*.rpm
  rm ccdciel*.zip
  rm ccdciel*.exe
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
  tar cvJf ccdciel-$version-$currentrev-linux_i386.tar.xz ccdciel
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv ccdciel*.tar.xz $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # deb
  cd $wd
  rsync -a --exclude=.svn system_integration/Linux/debian $builddir
  cd $builddir
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
  tar cvJf ccdciel-$version-$currentrev-linux_x86_64.tar.xz ccdciel
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv ccdciel*.tar.xz $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # deb
  cd $wd
  rsync -a --exclude=.svn system_integration/Linux/debian $builddir
  cd $builddir
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

# make Windows i386 version
if [[ $make_win32 ]]; then
  rsync -a --exclude=.svn system_integration/Windows/installer/ccdciel/* $builddir
  export PATH=$mingw32:$save_PATH
  ./configure $configopt prefix=$builddir/Data target=i386-win32$extratarget
  if [[ $? -ne 0 ]]; then exit 1;fi
  make OS_TARGET=win32 CPU_TARGET=i386 clean
  fpcopts="-O1 -Ch524288 -CX -XX -Xs" make OS_TARGET=win32 CPU_TARGET=i386
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_win
  if [[ $? -ne 0 ]]; then exit 1;fi
  # zip
  cd $builddir/Data
  zip -r  ccdciel-$version-$currentrev-windows.zip *
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv ccdciel*.zip $wd
  # exe
  cd $builddir
  sed -i "/AppVerName/ s/V3/V$version/" ccdciel.iss
  sed -i "/OutputBaseFilename/ s/windows/$version-$currentrev-windows/" ccdciel.iss
  wine "$innosetup" "$wine_build\ccdciel.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/ccdciel*.exe $wd

  cd $wd
  rm -rf $builddir
fi

# make Windows x86_64 version
if [[ $make_win64 ]]; then
  rsync -a --exclude=.svn system_integration/Windows/installer/ccdciel/* $builddir
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
  wine "$innosetup" "$wine_build\ccdciel_64.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/ccdciel*.exe $wd
fi

cd $wd
rm -rf $builddir

