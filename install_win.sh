#!/bin/bash

OS_TARGET=$1
destdir=$2

if [ -z "$OS_TARGET=" ]; then
   export OS_TARGET==win32
fi

if [ -z "$destdir" ]; then
   export destdir=/tmp/ccdciel
fi

echo Install ccdciel $OS_TARGET to $destdir

install -m 755 -d $destdir
install -m 755 -d $destdir/scripts
install -m 755 -d $destdir/scripts/siril
install -m 755 -d $destdir/scripts/python
install -m 755 -d $destdir/data
install -m 755 -d $destdir/data/stars
install -d -m 755 $destdir/data/dso
install -m 755 -d $destdir/data/language
install -m 755 -d $destdir/data/resources
install -m 755 -d $destdir/doc


if [ $OS_TARGET = win32 ]; then 
  strip -v -o $destdir/ccdciel.exe src/ccdciel.exe 
  strip -v -o $destdir/libccdcielwcs.dll library/wcs/libccdcielwcs.dll
  strip -v -o $destdir/libpasraw.dll library/raw/libpasraw.dll
  unzip -d $destdir system_integration/Windows/data/openssl-win32.zip
  unzip -d $destdir system_integration/Windows/data/zlib-win32.zip
  unzip -d $destdir system_integration/Windows/data/cfitsio-win32.zip
  unzip -d $destdir system_integration/Windows/data/exiv2-win32.zip
  unzip -d $destdir/scripts/python system_integration/Windows/data/python-3.8.10-embed-win32.zip
fi
if [ $OS_TARGET = win64 ]; then
  strip -v -o $destdir/ccdciel.exe src/ccdciel.exe 
  strip -v -o $destdir/libccdcielwcs.dll library/wcs/libccdcielwcs.dll
  strip -v -o $destdir/libpasraw.dll library/raw/libpasraw.dll
  unzip -d $destdir system_integration/Windows/data/openssl-win64.zip
  unzip -d $destdir system_integration/Windows/data/zlib-win64.zip
  unzip -d $destdir system_integration/Windows/data/cfitsio-win64.zip
  unzip -d $destdir system_integration/Windows/data/exiv2-win64.zip
  unzip -d $destdir/scripts/python system_integration/Windows/data/python-3.8.10-embed-amd64.zip
fi
unzip -d $destdir/scripts/python system_integration/Windows/data/python-pyserial.zip

install -v -m 644 scripts/ccdciel.py  $destdir/scripts/ccdciel.py
install -v -m 644 scripts/ccdciel.winembed  $destdir/scripts/python/ccdciel.py
install -v -m 755 scripts/astrometry.sh  $destdir/scripts/astrometry.sh
install -v -m 755 scripts/astrometry-blind.sh  $destdir/scripts/astrometry-blind.sh
install -v -m 755 scripts/astrometry-online.sh  $destdir/scripts/astrometry-online.sh
install -v -m 755 scripts/astrometry-online.bat  $destdir/scripts/astrometry-online.bat
install -v -m 644 scripts/client.py  $destdir/scripts/client.py
install -v -m 755 scripts/astrometry-macos.sh  $destdir/scripts/astrometry-macos.sh
install -v -m 755 scripts/siril/template_bias.ssf  $destdir/scripts/siril/template_bias.ssf
install -v -m 755 scripts/siril/template_dark.ssf  $destdir/scripts/siril/template_dark.ssf
install -v -m 755 scripts/siril/template_flat.ssf  $destdir/scripts/siril/template_flat.ssf
install -v -m 755 scripts/siril/template_light.ssf  $destdir/scripts/siril/template_light.ssf
install -v -m 755 scripts/siril/template_light.ssf  $destdir/scripts/siril/template_light_color.ssf
install -v -m 644 data/stars/focus_star_4   $destdir/data/stars/focus_star_4
install -v -m 644 data/stars/focus_star_5   $destdir/data/stars/focus_star_5
install -v -m 644 data/stars/focus_star_6   $destdir/data/stars/focus_star_6
install -v -m 644 data/stars/focus_star_7   $destdir/data/stars/focus_star_7
install -v -m 644 data/stars/focus_star_8   $destdir/data/stars/focus_star_8
install -v -m 644 data/dso/deep_sky.csv  $destdir/data/dso/deep_sky.csv
install -v -m 644 data/resources/smallcross.cur  $destdir/data/resources/smallcross.cur
install -v -m 644 data/resources/bigcross.cur  $destdir/data/resources/bigcross.cur
install -v -m 644 data/language/ccdciel.po      $destdir/data/language/ccdciel.en.po
install -v -m 644 data/language/ccdciel.en_GB.po $destdir/data/language/ccdciel.en_GB.po
install -v -m 644 data/language/ccdciel.cs.po   $destdir/data/language/ccdciel.cs.po
install -v -m 644 data/language/ccdciel.zh_CN.po   $destdir/data/language/ccdciel.zh_CN.po
install -v -m 644 data/language/ccdciel.de.po   $destdir/data/language/ccdciel.de.po
install -v -m 644 data/language/ccdciel.es.po   $destdir/data/language/ccdciel.es.po
install -v -m 644 data/language/ccdciel.fr.po   $destdir/data/language/ccdciel.fr.po
install -v -m 644 data/language/ccdciel.it.po   $destdir/data/language/ccdciel.it.po
install -v -m 644 data/language/ccdciel.ru.po   $destdir/data/language/ccdciel.ru.po
install -v -m 644 data/language/ccdciel_hints.po      $destdir/data/language/ccdciel_hints.en.po
install -v -m 644 data/language/ccdciel_hints.en_GB.po $destdir/data/language/ccdciel_hints.en_GB.po
install -v -m 644 data/language/ccdciel_hints.cs.po   $destdir/data/language/ccdciel_hints.cs.po
install -v -m 644 data/language/ccdciel_hints.zh_CN.po   $destdir/data/language/ccdciel_hints.zh_CN.po
install -v -m 644 data/language/ccdciel_hints.da.po   $destdir/data/language/ccdciel_hints.da.po
install -v -m 644 data/language/ccdciel_hints.de.po   $destdir/data/language/ccdciel_hints.de.po
install -v -m 644 data/language/ccdciel_hints.es.po   $destdir/data/language/ccdciel_hints.es.po
install -v -m 644 data/language/ccdciel_hints.fr.po   $destdir/data/language/ccdciel_hints.fr.po
install -v -m 644 data/language/ccdciel_hints.it.po   $destdir/data/language/ccdciel_hints.it.po
install -v -m 644 data/language/ccdciel_hints.ru.po   $destdir/data/language/ccdciel_hints.ru.po
install -v -m 644 doc/doc_ccdciel_en.pdf    $destdir/doc/doc_ccdciel_en.pdf

