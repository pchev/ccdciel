#!/bin/bash

destdir=$1

cpu_target=$2

if [ -z "$destdir" ]; then
   export destdir=/tmp/ccdciel
fi

echo Install ccdciel to $destdir

install -m 755 -d $destdir
install -m 755 -d $destdir/bin
install -m 755 -d $destdir/share
install -m 755 -d $destdir/share/appdata
install -m 755 -d $destdir/share/applications
install -m 755 -d $destdir/share/doc
install -m 755 -d $destdir/share/doc/ccdciel
install -m 755 -d $destdir/share/pixmaps
install -m 755 -d $destdir/share/icons
install -m 755 -d $destdir/share/icons/hicolor
install -m 755 -d $destdir/share/icons/hicolor/48x48
install -m 755 -d $destdir/share/icons/hicolor/48x48/apps
install -m 755 -d $destdir/share/icons/hicolor/scalable
install -m 755 -d $destdir/share/icons/hicolor/scalable/apps
install -m 755 -d $destdir/share/ccdciel
install -m 755 -d $destdir/share/ccdciel/scripts
install -m 755 -d $destdir/share/ccdciel/data
install -m 755 -d $destdir/share/ccdciel/data/stars

install -v -m 755 -s src/ccdciel  $destdir/bin/ccdciel
install -v -m 644 scripts/scope_park.script  $destdir/share/ccdciel/scripts/scope_park.script
install -v -m 644 scripts/scope_unpark.script  $destdir/share/ccdciel/scripts/scope_unpark.script
install -v -m 644 scripts/T_ccd_temp_down.script  $destdir/share/ccdciel/scripts/T_ccd_temp_down.script
install -v -m 644 scripts/T_ccd_temp_up.script  $destdir/share/ccdciel/scripts/T_ccd_temp_up.script
install -v -m 644 scripts/T_scope_alignment.script  $destdir/share/ccdciel/scripts/T_scope_alignment.script 
install -v -m 644 scripts/T_eqmod_alignment.script  $destdir/share/ccdciel/scripts/T_eqmod_alignment.script 
install -v -m 644 data/stars/focus_star_4  $destdir/share/ccdciel/data/stars/focus_star_4 
install -v -m 644 data/stars/focus_star_5  $destdir/share/ccdciel/data/stars/focus_star_5 
install -v -m 644 data/stars/focus_star_6  $destdir/share/ccdciel/data/stars/focus_star_6 
install -v -m 644 data/stars/focus_star_7  $destdir/share/ccdciel/data/stars/focus_star_7 
install -v -m 644 data/stars/focus_star_8  $destdir/share/ccdciel/data/stars/focus_star_8 
install -v -m 644 system_integration/Linux/share/applications/ccdciel.desktop $destdir/share/applications/ccdciel.desktop
install -v -m 644 system_integration/Linux/share/appdata/ccdciel.appdata.xml $destdir/share/appdata/ccdciel.appdata.xml
install -v -m 644 system_integration/Linux/share/doc/ccdciel/changelog $destdir/share/doc/ccdciel/changelog
install -v -m 644 system_integration/Linux/share/doc/ccdciel/copyright $destdir/share/doc/ccdciel/copyright
install -v -m 644 system_integration/Linux/share/pixmaps/ccdciel.png $destdir/share/pixmaps/ccdciel.png
install -v -m 644 system_integration/Linux/share/icons/hicolor/48x48/apps/ccdciel.png $destdir/share/icons/hicolor/48x48/apps/ccdciel.png
