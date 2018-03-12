#!/bin/bash

destdir=$1

if [ -z "$destdir" ]; then
   export destdir=/tmp/ccdciel
fi

echo Install ccdciel to $destdir

install -d -m 755 $destdir
install -d -m 755 $destdir/ccdciel.app
install -d -m 755 $destdir/ccdciel.app/Contents
install -d -m 755 $destdir/ccdciel.app/Contents/MacOS
install -d -m 755 $destdir/ccdciel.app/Contents/Resources
install -v -m 644 system_integration/MacOSX/pkg/ccdciel.app/Contents/Info.plist $destdir/ccdciel.app/Contents/
install -v -m 644 system_integration/MacOSX/pkg/ccdciel.app/Contents/PkgInfo $destdir/ccdciel.app/Contents/
install -v -m 755 -s src/ccdciel  $destdir/ccdciel.app/Contents/MacOS/ccdciel
install -v -m 644 system_integration/MacOSX/pkg/ccdciel.app/Contents/Resources/README.rtf $destdir/ccdciel.app/Contents/Resources/
install -v -m 644 system_integration/MacOSX/pkg/ccdciel.app/Contents/Resources/ccdciel.icns $destdir/ccdciel.app/Contents/Resources/

install -v -m 755 library/wcs/libccdcielwcs.dylib  $destdir/libccdcielwcs.dylib

install -d -m 755 $destdir/scripts
install -d -m 755 $destdir/data
install -d -m 755 $destdir/data/stars
install -d -m 755 $destdir/data/language
install -d -m 755 $destdir/doc

install -v -m 644 scripts/scope_park.script  $destdir/scripts/scope_park.script
install -v -m 644 scripts/scope_unpark.script  $destdir/scripts/scope_unpark.script
install -v -m 644 scripts/T_ccd_temp_down.script  $destdir/scripts/T_ccd_temp_down.script
install -v -m 644 scripts/T_ccd_temp_up.script  $destdir/scripts/T_ccd_temp_up.script
install -v -m 644 scripts/T_scope_alignment.script  $destdir/scripts/T_scope_alignment.script 
install -v -m 644 scripts/T_eqmod_alignment.script  $destdir/scripts/T_eqmod_alignment.script 
install -v -m 755 scripts/astrometry.sh  $destdir/scripts/astrometry.sh
install -v -m 755 scripts/astrometry-online.sh  $destdir/scripts/astrometry-online.sh
install -v -m 755 scripts/astrometry-macos.sh  $destdir/scripts/astrometry-macos.sh
install -v -m 644 data/stars/focus_star_4   $destdir/data/stars/focus_star_4
install -v -m 644 data/stars/focus_star_5   $destdir/data/stars/focus_star_5
install -v -m 644 data/stars/focus_star_6   $destdir/data/stars/focus_star_6
install -v -m 644 data/stars/focus_star_7   $destdir/data/stars/focus_star_7
install -v -m 644 data/stars/focus_star_8   $destdir/data/stars/focus_star_8
install -v -m 644 data/language/ccdciel.po      $destdir/data/language/ccdciel.en.po
install -v -m 644 data/language/ccdciel.fr.po   $destdir/data/language/ccdciel.fr.po
install -v -m 644 data/language/ccdciel.it.po   $destdir/data/language/ccdciel.it.po
install -v -m 644 doc/doc_ccdciel_en.pdf    $destdir/doc/doc_ccdciel_en.pdf
