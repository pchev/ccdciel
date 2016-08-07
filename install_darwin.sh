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
install -v -m 644 scripts/scope_park.script  $destdir/scripts/scope_park.script
install -v -m 644 scripts/scope_unpark.script  $destdir/scripts/scope_unpark.script
install -v -m 644 scripts/T_ccd_temp_down.script  $destdir/scripts/T_ccd_temp_down.script
install -v -m 644 scripts/T_ccd_temp_up.script  $destdir/scripts/T_ccd_temp_up.script
install -v -m 644 scripts/T_scope_alignment.script  $destdir/scripts/T_scope_alignment.script 
