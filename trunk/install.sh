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

install -v -m 755 -s src/ccdciel  $destdir/bin/ccdciel
install -v -m 755 -s src/eqmodgui  $destdir/bin/eqmodgui
install -v -m 644 system_integration/Linux/share/applications/ccdciel.desktop $destdir/share/applications/ccdciel.desktop
install -v -m 644 system_integration/Linux/share/appdata/ccdciel.appdata.xml $destdir/share/appdata/ccdciel.appdata.xml
install -v -m 644 system_integration/Linux/share/doc/ccdciel/changelog $destdir/share/doc/ccdciel/changelog
install -v -m 644 system_integration/Linux/share/doc/ccdciel/copyright $destdir/share/doc/ccdciel/copyright
install -v -m 644 system_integration/Linux/share/pixmaps/ccdciel.png $destdir/share/pixmaps/ccdciel.png
install -v -m 644 system_integration/Linux/share/icons/hicolor/48x48/apps/ccdciel.png $destdir/share/icons/hicolor/48x48/apps/ccdciel.png
install -v -m 644 system_integration/Linux/share/icons/hicolor/scalable/apps/ccdciel.svg $destdir/share/icons/hicolor/scalable/apps/ccdciel.svg
install -v -m 644 system_integration/Linux/share/applications/eqmodgui.desktop $destdir/share/applications/eqmodgui.desktop
install -v -m 644 system_integration/Linux/share/pixmaps/eqmodgui.png $destdir/share/pixmaps/eqmodgui.png
install -v -m 644 system_integration/Linux/share/icons/hicolor/48x48/apps/eqmodgui.png $destdir/share/icons/hicolor/48x48/apps/eqmodgui.png
