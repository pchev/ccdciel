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
install -m 755 -d $destdir/share/applications
install -m 755 -d $destdir/share/metainfo
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
install -m 755 -d $destdir/share/ccdciel/scripts/siril
install -m 755 -d $destdir/share/ccdciel/data
install -m 755 -d $destdir/share/ccdciel/data/stars
install -m 755 -d $destdir/share/ccdciel/data/dso
install -m 755 -d $destdir/share/ccdciel/data/language
install -m 755 -d $destdir/share/ccdciel/data/resources
install -m 755 -d $destdir/share/ccdciel/doc

install -v -m 755 -s src/ccdciel  $destdir/bin/ccdciel
install -v -m 644 scripts/scope_park.script  $destdir/share/ccdciel/scripts/scope_park.script
install -v -m 644 scripts/scope_unpark.script  $destdir/share/ccdciel/scripts/scope_unpark.script
install -v -m 644 scripts/T_ccd_temp_down.script  $destdir/share/ccdciel/scripts/T_ccd_temp_down.script
install -v -m 644 scripts/T_ccd_temp_up.script  $destdir/share/ccdciel/scripts/T_ccd_temp_up.script
install -v -m 644 scripts/T_scope_alignment.script  $destdir/share/ccdciel/scripts/T_scope_alignment.script 
install -v -m 644 scripts/T_eqmod_alignment.script  $destdir/share/ccdciel/scripts/T_eqmod_alignment.script 
install -v -m 755 scripts/astrometry.sh  $destdir/share/ccdciel/scripts/astrometry.sh
install -v -m 755 scripts/astrometry-online.sh  $destdir/share/ccdciel/scripts/astrometry-online.sh
install -v -m 755 scripts/astrometry-macos.sh  $destdir/share/ccdciel/scripts/astrometry-macos.sh
install -v -m 755 scripts/siril_bias.script  $destdir/share/ccdciel/scripts/siril_bias.script
install -v -m 755 scripts/siril_dark.script $destdir/share/ccdciel/scripts/siril_dark.script
install -v -m 755 scripts/siril_flat.script $destdir/share/ccdciel/scripts/siril_flat.script
install -v -m 755 scripts/siril_light.script $destdir/share/ccdciel/scripts/siril_light.script
install -v -m 755 scripts/siril/template_bias.ssf  $destdir/share/ccdciel/scripts/siril/template_bias.ssf
install -v -m 755 scripts/siril/template_dark.ssf  $destdir/share/ccdciel/scripts/siril/template_dark.ssf
install -v -m 755 scripts/siril/template_flat.ssf  $destdir/share/ccdciel/scripts/siril/template_flat.ssf
install -v -m 755 scripts/siril/template_light.ssf  $destdir/share/ccdciel/scripts/siril/template_light.ssf
install -v -m 644 data/stars/focus_star_4  $destdir/share/ccdciel/data/stars/focus_star_4 
install -v -m 644 data/stars/focus_star_5  $destdir/share/ccdciel/data/stars/focus_star_5 
install -v -m 644 data/stars/focus_star_6  $destdir/share/ccdciel/data/stars/focus_star_6 
install -v -m 644 data/stars/focus_star_7  $destdir/share/ccdciel/data/stars/focus_star_7 
install -v -m 644 data/stars/focus_star_8  $destdir/share/ccdciel/data/stars/focus_star_8 
install -v -m 644 data/dso/deep_sky.csv  $destdir/share/ccdciel/data/dso/deep_sky.csv
install -v -m 644 data/resources/smallcross.cur  $destdir/share/ccdciel/data/resources/smallcross.cur
install -v -m 644 data/language/ccdciel.po     $destdir/share/ccdciel/data/language/ccdciel.en.po
install -v -m 644 data/language/ccdciel.en_GB.po $destdir/share/ccdciel/data/language/ccdciel.en_GB.po
install -v -m 644 data/language/ccdciel.es.po  $destdir/share/ccdciel/data/language/ccdciel.es.po
install -v -m 644 data/language/ccdciel.fr.po  $destdir/share/ccdciel/data/language/ccdciel.fr.po
install -v -m 644 data/language/ccdciel.it.po  $destdir/share/ccdciel/data/language/ccdciel.it.po
install -v -m 644 data/language/ccdciel_hints.po     $destdir/share/ccdciel/data/language/ccdciel_hints.en.po
install -v -m 644 data/language/ccdciel_hints.en_GB.po $destdir/share/ccdciel/data/language/ccdciel_hints.en_GB.po
install -v -m 644 data/language/ccdciel_hints.es.po  $destdir/share/ccdciel/data/language/ccdciel_hints.es.po
install -v -m 644 data/language/ccdciel_hints.fr.po  $destdir/share/ccdciel/data/language/ccdciel_hints.fr.po
install -v -m 644 data/language/ccdciel_hints.it.po  $destdir/share/ccdciel/data/language/ccdciel_hints.it.po
install -v -m 644 doc/doc_ccdciel_en.pdf $destdir/share/ccdciel/doc/doc_ccdciel_en.pdf
install -v -m 644 system_integration/Linux/share/applications/ccdciel.desktop $destdir/share/applications/ccdciel.desktop
install -v -m 644 system_integration/Linux/share/metainfo/ccdciel.appdata.xml $destdir/share/metainfo/ccdciel.appdata.xml
install -v -m 644 system_integration/Linux/share/doc/ccdciel/changelog $destdir/share/doc/ccdciel/changelog
install -v -m 644 system_integration/Linux/share/doc/ccdciel/copyright $destdir/share/doc/ccdciel/copyright
install -v -m 644 system_integration/Linux/share/pixmaps/ccdciel.png $destdir/share/pixmaps/ccdciel.png
install -v -m 644 system_integration/Linux/share/icons/hicolor/48x48/apps/ccdciel.png $destdir/share/icons/hicolor/48x48/apps/ccdciel.png
