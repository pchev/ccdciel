#!/bin/bash

# This script build the INDI library and drivers using the latest or a specific version.
# Before use, you must install the pre-requisites and configure a few options below.
# It install the drivers in /usr, this is intentional to not have multiple version if already installed from packages.
# It is not intended for developper, beware it can remove any local change in the source directory, if you want to use it see the option indiversion=local below.
#
# See https://github.com/indilib/indi for the pre-requisites.
# Something like:
# sudo apt-get install git cdbs dkms cmake fxload libev-dev libgps-dev libgsl-dev libraw-dev libusb-dev zlib1g-dev libftdi-dev libjpeg-dev libkrb5-dev libnova-dev libtiff-dev libfftw3-dev librtlsdr-dev libcfitsio-dev libgphoto2-dev build-essential libusb-1.0-0-dev libdc1394-dev libboost-regex-dev libcurl4-gnutls-dev libtheora-dev libxisf-dev

# Some additional 3rdparty drivers may also need: 
# sudo apt-get install libboost-dev liblimesuite-dev libftdi1-dev libavcodec-dev libavdevice-dev libzmq3-dev libudev-dev libpigpiod-if-dev libpigpiod-if2-1 pigpio-tools
#

######### User Setup ###################################
# you can change here how you want to build INDI 

# The directory to hold the INDI source code, it is automatically installed and updated here 
indisrc=~/source/indi

# The INDI version to compile, use "latest" for the last release, "master" for the development code or any version tag
# developer must specify "local", otherwise any local change are reverted before the compilation!
indiversion=latest
#indiversion=master
#indiversion=v2.1.5
#indiversion=local

# The list of additional library and drivers to install from indi-3rdparty.
# Let empty to compile all the libraries and drivers
unset INDI_LIBS
unset INDI_DRIVERS
# Or add name of specific library and driver to build, this save time and space by not installing drivers you not need
#declare -a INDI_LIBS=( libatik )
#declare -a INDI_DRIVERS=( indi-atik indi-eqmod )

# Additional build options for INDI cmake to activate specific features
unset indiopt
#indiopt='-DINDI_BUILD_WEBSOCKET=On'
# Additional make options, use -j with the number of processor core to speedup the compilation
makeopt=-j4


########## End of user setup #############################

# Ask root password now so in most case it can install when need without asking more
sudo echo "Build INDI"

# the first time, create the directory and clone the source code
if [[ ! -e $indisrc/indi ]]; then
  mkdir -p $indisrc
  cd $indisrc
  if [[ $? != 0 ]]; then echo error ; exit; fi
  git clone https://github.com/indilib/indi.git
  git clone https://github.com/indilib/indi-3rdparty.git
fi

# update the source code
cd $indisrc/indi
if [[ $? != 0 ]]; then echo error ; exit; fi
if [[ $indiversion != 'local' ]]; then
  git checkout .
  git checkout -q master
  git pull
  if [[ $indiversion = 'latest' ]]; then
     indiversion=$(git describe --tags --abbrev=0)
  fi
  git checkout -q $indiversion
  if [[ $? != 0 ]]; then echo error ; exit; fi
fi
git status

cd $indisrc/indi-3rdparty
if [[ $? != 0 ]]; then echo error ; exit; fi
if [[ $indiversion != 'local' ]]; then
  git checkout .
  git checkout -q master
  git pull
  git checkout -q $indiversion
  if [[ $? != 0 ]]; then echo error ; exit; fi
fi
git status

# prepare the build directory
rm -rf $indisrc/build
mkdir $indisrc/build
cd $indisrc/build
if [[ $? != 0 ]]; then echo error ; exit; fi

# build INDI
mkdir $indisrc/build/libindi
cd $indisrc/build/libindi
if [[ $? != 0 ]]; then echo error ; exit; fi
cmake $indiopt  -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE=Release $indisrc/indi
if [[ $? != 0 ]]; then echo error ; exit; fi
make $makeopt
if [[ $? != 0 ]]; then echo error ; exit; fi
sudo make install
if [[ $? != 0 ]]; then echo error ; exit; fi

# build INDI-3rdparty libraries
if [[ -z ${INDI_LIBS[0]} ]]; then
  # all libraries
  mkdir $indisrc/build/3rdparty-libs
  cd $indisrc/build/3rdparty-libs
  if [[ $? != 0 ]]; then echo error ; exit; fi
  cmake $indiopt -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE=Release -DBUILD_LIBS=1 $indisrc/indi-3rdparty
  if [[ $? != 0 ]]; then echo error ; exit; fi
  make $makeopt
  if [[ $? != 0 ]]; then echo error ; exit; fi
  sudo make install
  if [[ $? != 0 ]]; then echo error ; exit; fi 
else
  # only specific librairies 
  for lib in "${INDI_LIBS[@]}"
  do
    echo
    echo =======   $lib =======
    echo
    mkdir $indisrc/build/${lib}
    cd $indisrc/build/${lib}
    if [[ $? != 0 ]]; then echo error ; exit; fi
    cmake $indiopt -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE=Release $indisrc/indi-3rdparty/${lib}
    if [[ $? != 0 ]]; then echo error ; exit; fi
    make $makeopt
    if [[ $? != 0 ]]; then echo error ; exit; fi
    sudo make install
    if [[ $? != 0 ]]; then echo error ; exit; fi
  done
fi

# build INDI-3rdparty drivers
if [[ -z ${INDI_DRIVERS[0]} ]]; then 
  # all the drivers
  mkdir $indisrc/build/3rdparty
  cd $indisrc/build/3rdparty
  if [[ $? != 0 ]]; then echo error ; exit; fi
  cmake $indiopt -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE=Release $indisrc/indi-3rdparty
  if [[ $? != 0 ]]; then echo error ; exit; fi
  make $makeopt
  if [[ $? != 0 ]]; then echo error ; exit; fi
  sudo make install
  if [[ $? != 0 ]]; then echo error ; exit; fi 
else
  # only specific drivers
  for drv in "${INDI_DRIVERS[@]}"
  do
    echo
    echo ======= $drv =======
    echo
    mkdir $indisrc/build/${drv}
    cd $indisrc/build/${drv}
    if [[ $? != 0 ]]; then echo error ; exit; fi
    cmake $indiopt -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE=Release $indisrc/indi-3rdparty/${drv}
    if [[ $? != 0 ]]; then echo error ; exit; fi
    make $makeopt
    if [[ $? != 0 ]]; then echo error ; exit; fi
    sudo make install
    if [[ $? != 0 ]]; then echo error ; exit; fi
  done  
fi
