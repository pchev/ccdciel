#!/bin/bash

# This script build the INDI library and drivers using the latest or a specific version.
# Before use, you must install the pre-requisites and configure a few options below.
# It install the drivers in /usr, this is intentional to not keep multiple version if some are already installed from packages.
# It is not intended for developper, beware it remove any local change in the source directory.
#
# See https://github.com/indilib/indi for the pre-requisites.
# Something like:
#   sudo apt-get install libnova-dev libcfitsio-dev libusb-1.0-0-dev zlib1g-dev libgsl-dev build-essential cmake git libjpeg-dev libcurl4-gnutls-dev libtiff-dev libfftw3-dev
# Some additional 3rdparty drivers may also need: 
#   sudo apt-get install libftdi-dev libgps-dev libraw-dev libdc1394-22-dev libgphoto2-dev libboost-dev libboost-regex-dev librtlsdr-dev liblimesuite-dev libftdi1-dev libavcodec-dev libavdevice-dev
#

# User Setup 
# you can change here how you want to build INDI 

# The INDI version to compile, use "latest" for the last release, "master" for the development code or any version tag like v1.8.7
# developer must specify "local" to use any local change, otherwise they are reverted before the compilation!
indiversion=latest
#indiversion=master
#indiversion=v1.8.7
#indiversion=local

# The list of additional library and drivers to install from indi-3rdparty
# The name to put here is the directory name from https://github.com/indilib/indi-3rdparty
# Be sure to list all the required library first
declare -a INDI_DRIVERS=( libasi libatik libqhy indi-asi indi-atik indi-qhy indi-eqmod indi-dsi indi-gphoto )

# Additional build options for INDI cmake
unset indiopt
#indiopt='-DINDI_BUILD_WEBSOCKET=On'
# Additional make options, use -j with the number of processor core to speedup the compilation
unset makeopt
makeopt=-j4

# The directory to hold the INDI source code
indisrc=~/src/indi

# End of user setup

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

# build INDI-3rdparty
for drv in "${INDI_DRIVERS[@]}"
do
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
