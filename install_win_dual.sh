#!/bin/bash

OS_TARGET=$1
destdir=$2

if [ -z "$OS_TARGET=" ]; then
   export OS_TARGET==win32
fi

if [ -z "$destdir" ]; then
   export destdir=/tmp/ccdciel/Data
fi

echo Install ccdciel $OS_TARGET to $destdir

install -m 755 -d $destdir
install -m 755 -d $destdir/scripts
install -m 755 -d $destdir/scripts/siril
install -m 755 -d $destdir/data
install -m 755 -d $destdir/data/stars
install -d -m 755 $destdir/data/dso
install -m 755 -d $destdir/data/language
install -m 755 -d $destdir/data/resources
install -m 755 -d $destdir/doc


if [ $OS_TARGET = win32 ]; then 
  strip -v -o $destdir/../Prog/ccdciel.exe src/ccdciel.exe 
  strip -v -o $destdir/../Prog/libccdcielwcs.dll library/wcs/libccdcielwcs.dll
  strip -v -o $destdir/../Prog/libpasraw.dll library/raw/libpasraw.dll
  unzip -d $destdir/../Prog/lib32/ system_integration/Windows/data/openssl-win32.zip
  unzip -d $destdir/../Prog/lib32/ system_integration/Windows/data/zlib-win32.zip
  unzip -d $destdir/../Prog/lib32/ system_integration/Windows/data/cfitsio-win32.zip

fi
if [ $OS_TARGET = win64 ]; then
  strip -v -o $destdir/../Prog/ccdciel-x64.exe src/ccdciel.exe 
  strip -v -o $destdir/../Prog/libccdcielwcs-x64.dll library/wcs/libccdcielwcs.dll
  strip -v -o $destdir/../Prog/libpasraw-64.dll library/raw/libpasraw.dll
  unzip -d $destdir/../Prog/lib64/ system_integration/Windows/data/openssl-win64.zip
  unzip -d $destdir/../Prog/lib64/ system_integration/Windows/data/zlib-win64.zip
  unzip -d $destdir/../Prog/lib64/ system_integration/Windows/data/cfitsio-win32.zip
fi

install -v -m 644 scripts/scope_park.script  $destdir/scripts/scope_park.script
install -v -m 644 scripts/scope_unpark.script  $destdir/scripts/scope_unpark.script
install -v -m 644 scripts/T_ccd_temp_down.script  $destdir/scripts/T_ccd_temp_down.script
install -v -m 644 scripts/T_ccd_temp_up.script  $destdir/scripts/T_ccd_temp_up.script
install -v -m 644 scripts/T_scope_alignment.script  $destdir/scripts/T_scope_alignment.script 
install -v -m 644 scripts/T_eqmod_alignment.script  $destdir/scripts/T_eqmod_alignment.script 
install -v -m 755 scripts/astrometry.sh  $destdir/scripts/astrometry.sh
install -v -m 755 scripts/astrometry-online.sh  $destdir/scripts/astrometry-online.sh
install -v -m 755 scripts/astrometry-macos.sh  $destdir/scripts/astrometry-macos.sh
install -v -m 755 scripts/siril_bias.script  $destdir/scripts/siril_bias.script
install -v -m 755 scripts/siril_dark.script $destdir/scripts/siril_dark.script
install -v -m 755 scripts/siril_flat.script $destdir/scripts/siril_flat.script
install -v -m 755 scripts/siril_light.script $destdir/scripts/siril_light.script
install -v -m 755 scripts/siril/template_bias.ssf  $destdir/scripts/siril/template_bias.ssf
install -v -m 755 scripts/siril/template_dark.ssf  $destdir/scripts/siril/template_dark.ssf
install -v -m 755 scripts/siril/template_flat.ssf  $destdir/scripts/siril/template_flat.ssf
install -v -m 755 scripts/siril/template_light.ssf  $destdir/scripts/siril/template_light.ssf
install -v -m 644 data/stars/focus_star_4   $destdir/data/stars/focus_star_4
install -v -m 644 data/stars/focus_star_5   $destdir/data/stars/focus_star_5
install -v -m 644 data/stars/focus_star_6   $destdir/data/stars/focus_star_6
install -v -m 644 data/stars/focus_star_7   $destdir/data/stars/focus_star_7
install -v -m 644 data/stars/focus_star_8   $destdir/data/stars/focus_star_8
install -v -m 644 data/dso/deep_sky.csv  $destdir/data/dso/deep_sky.csv
install -v -m 644 data/resources/smallcross.cur  $destdir/data/resources/smallcross.cur
install -v -m 644 data/language/ccdciel.po      $destdir/data/language/ccdciel.en.po
install -v -m 644 data/language/ccdciel.en_GB.po $destdir/data/language/ccdciel.en_GB.po
install -v -m 644 data/language/ccdciel.de.po   $destdir/data/language/ccdciel.de.po
install -v -m 644 data/language/ccdciel.es.po   $destdir/data/language/ccdciel.es.po
install -v -m 644 data/language/ccdciel.fr.po   $destdir/data/language/ccdciel.fr.po
install -v -m 644 data/language/ccdciel.it.po   $destdir/data/language/ccdciel.it.po
install -v -m 644 data/language/ccdciel.ru.po   $destdir/data/language/ccdciel.ru.po
install -v -m 644 data/language/ccdciel_hints.po      $destdir/data/language/ccdciel_hints.en.po
install -v -m 644 data/language/ccdciel_hints.en_GB.po $destdir/data/language/ccdciel_hints.en_GB.po
install -v -m 644 data/language/ccdciel_hints.de.po   $destdir/data/language/ccdciel_hints.de.po
install -v -m 644 data/language/ccdciel_hints.es.po   $destdir/data/language/ccdciel_hints.es.po
install -v -m 644 data/language/ccdciel_hints.fr.po   $destdir/data/language/ccdciel_hints.fr.po
install -v -m 644 data/language/ccdciel_hints.it.po   $destdir/data/language/ccdciel_hints.it.po
install -v -m 644 data/language/ccdciel_hints.ru.po   $destdir/data/language/ccdciel_hints.ru.po
install -v -m 644 doc/doc_ccdciel_en.pdf    $destdir/doc/doc_ccdciel_en.pdf

