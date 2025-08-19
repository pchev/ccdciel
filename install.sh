#!/bin/bash

destdir=$1

cpu_target=$2
unset armproc
if [[ $cpu_target =~ "arm" ]] || [[ $cpu_target =~ "aarch" ]]; then
   armproc=1
fi

if [ -z "$destdir" ]; then
   export destdir=/tmp/ccdciel
fi

echo Install ccdciel $cpu_target to $destdir

install -m 755 -d $destdir
install -m 755 -d $destdir/bin
install -m 755 -d $destdir/share
install -m 755 -d $destdir/share/applications
install -m 755 -d $destdir/share/metainfo
install -m 755 -d $destdir/share/doc
install -m 755 -d $destdir/share/doc/ccdciel
install -m 755 -d $destdir/share/pixmaps
install -m 755 -d $destdir/share/icons
install -m 755 -d $destdir/share/icons/hicolor
install -m 755 -d $destdir/share/icons/hicolor/32x32
install -m 755 -d $destdir/share/icons/hicolor/32x32/apps
install -m 755 -d $destdir/share/icons/hicolor/48x48
install -m 755 -d $destdir/share/icons/hicolor/48x48/apps
install -m 755 -d $destdir/share/icons/hicolor/96x96
install -m 755 -d $destdir/share/icons/hicolor/96x96/apps
install -m 755 -d $destdir/share/icons/hicolor/scalable
install -m 755 -d $destdir/share/icons/hicolor/scalable/apps
install -m 755 -d $destdir/share/ccdciel
install -m 755 -d $destdir/share/ccdciel/scripts
install -m 755 -d $destdir/share/ccdciel/scripts/indi
install -m 755 -d $destdir/share/ccdciel/data
install -m 755 -d $destdir/share/ccdciel/data/stars
install -m 755 -d $destdir/share/ccdciel/data/dso
install -m 755 -d $destdir/share/ccdciel/data/language
install -m 755 -d $destdir/share/ccdciel/data/resources
install -m 755 -d $destdir/share/ccdciel/data/jpleph
install -m 755 -d $destdir/share/ccdciel/doc

install -v -m 755 -s src/ccdciel  $destdir/bin/ccdciel
install -v -m 755 scripts/astrometry.sh  $destdir/share/ccdciel/scripts/astrometry.sh
install -v -m 755 scripts/astrometry-blind.sh  $destdir/share/ccdciel/scripts/astrometry-blind.sh
install -v -m 755 scripts/astrometry-online.sh  $destdir/share/ccdciel/scripts/astrometry-online.sh
install -v -m 755 scripts/astrometry-online.bat  $destdir/share/ccdciel/scripts/astrometry-online.bat
install -v -m 644 scripts/client.py  $destdir/share/ccdciel/scripts/client.py
install -v -m 755 scripts/astrometry-macos.sh  $destdir/share/ccdciel/scripts/astrometry-macos.sh
install -v -m 644 scripts/ccdciel.py  $destdir/share/ccdciel/scripts/ccdciel.py
install -v -m 644 scripts/indi/build_indi.sh  $destdir/share/ccdciel/scripts/indi/build_indi.sh
install -v -m 644 data/stars/focus_star_4  $destdir/share/ccdciel/data/stars/focus_star_4 
install -v -m 644 data/stars/focus_star_5  $destdir/share/ccdciel/data/stars/focus_star_5 
install -v -m 644 data/stars/focus_star_6  $destdir/share/ccdciel/data/stars/focus_star_6 
install -v -m 644 data/stars/focus_star_7  $destdir/share/ccdciel/data/stars/focus_star_7 
install -v -m 644 data/stars/focus_star_8  $destdir/share/ccdciel/data/stars/focus_star_8 
install -v -m 644 data/dso/deep_sky.csv  $destdir/share/ccdciel/data/dso/deep_sky.csv
install -v -m 644 data/resources/smallcross.cur  $destdir/share/ccdciel/data/resources/smallcross.cur
install -v -m 644 data/resources/bigcross.cur  $destdir/share/ccdciel/data/resources/bigcross.cur
install -v -m 644 data/jpleph/lnxp2000p2050.440  $destdir/share/ccdciel/data/jpleph/lnxp2000p2050.440
install -v -m 644 data/language/ccdciel.po     $destdir/share/ccdciel/data/language/ccdciel.en.po
install -v -m 644 data/language/ccdciel.en_GB.po $destdir/share/ccdciel/data/language/ccdciel.en_GB.po
install -v -m 644 data/language/ccdciel.cs.po  $destdir/share/ccdciel/data/language/ccdciel.cs.po
install -v -m 644 data/language/ccdciel.zh_CN.po  $destdir/share/ccdciel/data/language/ccdciel.zh_CN.po
install -v -m 644 data/language/ccdciel.da.po  $destdir/share/ccdciel/data/language/ccdciel.da.po
install -v -m 644 data/language/ccdciel.de.po  $destdir/share/ccdciel/data/language/ccdciel.de.po
install -v -m 644 data/language/ccdciel.es.po  $destdir/share/ccdciel/data/language/ccdciel.es.po
install -v -m 644 data/language/ccdciel.fr.po  $destdir/share/ccdciel/data/language/ccdciel.fr.po
install -v -m 644 data/language/ccdciel.it.po  $destdir/share/ccdciel/data/language/ccdciel.it.po
install -v -m 644 data/language/ccdciel.ru.po  $destdir/share/ccdciel/data/language/ccdciel.ru.po
install -v -m 644 data/language/ccdciel_hints.po     $destdir/share/ccdciel/data/language/ccdciel_hints.en.po
install -v -m 644 data/language/ccdciel_hints.en_GB.po $destdir/share/ccdciel/data/language/ccdciel_hints.en_GB.po
install -v -m 644 data/language/ccdciel_hints.cs.po  $destdir/share/ccdciel/data/language/ccdciel_hints.cs.po
install -v -m 644 data/language/ccdciel_hints.zh_CN.po  $destdir/share/ccdciel/data/language/ccdciel_hints.zh_CN.po
install -v -m 644 data/language/ccdciel_hints.de.po  $destdir/share/ccdciel/data/language/ccdciel_hints.de.po
install -v -m 644 data/language/ccdciel_hints.es.po  $destdir/share/ccdciel/data/language/ccdciel_hints.es.po
install -v -m 644 data/language/ccdciel_hints.fr.po  $destdir/share/ccdciel/data/language/ccdciel_hints.fr.po
install -v -m 644 data/language/ccdciel_hints.it.po  $destdir/share/ccdciel/data/language/ccdciel_hints.it.po
install -v -m 644 data/language/ccdciel_hints.ru.po  $destdir/share/ccdciel/data/language/ccdciel_hints.ru.po
install -v -m 644 doc/doc_ccdciel_en.pdf $destdir/share/ccdciel/doc/doc_ccdciel_en.pdf
install -v -m 644 system_integration/Linux/share/applications/net.ap_i.ccdciel.desktop $destdir/share/applications/net.ap_i.ccdciel.desktop
install -v -m 644 system_integration/Linux/share/metainfo/net.ap_i.ccdciel.metainfo.xml $destdir/share/metainfo/net.ap_i.ccdciel.metainfo.xml
install -v -m 644 system_integration/Linux/share/doc/ccdciel/changelog $destdir/share/doc/ccdciel/changelog
install -v -m 644 system_integration/Linux/share/doc/ccdciel/copyright $destdir/share/doc/ccdciel/copyright
install -v -m 644 system_integration/Linux/share/pixmaps/ccdciel.png $destdir/share/pixmaps/ccdciel.png
install -v -m 644 system_integration/Linux/share/icons/hicolor/32x32/apps/ccdciel.png $destdir/share/icons/hicolor/32x32/apps/ccdciel.png
install -v -m 644 system_integration/Linux/share/icons/hicolor/48x48/apps/ccdciel.png $destdir/share/icons/hicolor/48x48/apps/ccdciel.png
install -v -m 644 system_integration/Linux/share/icons/hicolor/96x96/apps/ccdciel.png $destdir/share/icons/hicolor/96x96/apps/ccdciel.png
install -v -m 644 system_integration/Linux/share/icons/hicolor/scalable/apps/ccdciel.svg $destdir/share/icons/hicolor/scalable/apps/ccdciel.svg
