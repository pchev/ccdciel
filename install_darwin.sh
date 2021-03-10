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
install -v -m 755 library/raw/libpasraw.dylib  $destdir/libpasraw.dylib
tar xvzf system_integration/MacOSX/data/cfitsio-mac.tgz -C $destdir
tar xvzf system_integration/MacOSX/data/exiv2-mac.tgz -C $destdir/ccdciel.app/Contents/MacOS

install -d -m 755 $destdir/scripts
install -m 755 -d $destdir/scripts/siril
install -d -m 755 $destdir/data
install -d -m 755 $destdir/data/stars
install -d -m 755 $destdir/data/dso
install -d -m 755 $destdir/data/language
install -m 755 -d $destdir/data/resources
install -d -m 755 $destdir/doc

install -v -m 644 scripts/ccdciel.py  $destdir/scripts/ccdciel.py
install -v -m 644 scripts/scope_park.script  $destdir/scripts/scope_park.script
install -v -m 644 scripts/scope_unpark.script  $destdir/scripts/scope_unpark.script
install -v -m 644 scripts/scope_position.script  $destdir/scripts/scope_position.script
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
install -v -m 644 data/language/ccdciel.cs.po   $destdir/data/language/ccdciel.cs.po
install -v -m 644 data/language/ccdciel.de.po   $destdir/data/language/ccdciel.de.po
install -v -m 644 data/language/ccdciel.es.po   $destdir/data/language/ccdciel.es.po
install -v -m 644 data/language/ccdciel.fr.po   $destdir/data/language/ccdciel.fr.po
install -v -m 644 data/language/ccdciel.it.po   $destdir/data/language/ccdciel.it.po
install -v -m 644 data/language/ccdciel.ru.po   $destdir/data/language/ccdciel.ru.po
install -v -m 644 data/language/ccdciel_hints.po      $destdir/data/language/ccdciel_hints.en.po
install -v -m 644 data/language/ccdciel_hints.en_GB.po $destdir/data/language/ccdciel_hints.en_GB.po
install -v -m 644 data/language/ccdciel_hints.cs.po   $destdir/data/language/ccdciel_hints.cs.po
install -v -m 644 data/language/ccdciel_hints.de.po   $destdir/data/language/ccdciel_hints.de.po
install -v -m 644 data/language/ccdciel_hints.es.po   $destdir/data/language/ccdciel_hints.es.po
install -v -m 644 data/language/ccdciel_hints.fr.po   $destdir/data/language/ccdciel_hints.fr.po
install -v -m 644 data/language/ccdciel_hints.it.po   $destdir/data/language/ccdciel_hints.it.po
install -v -m 644 data/language/ccdciel_hints.ru.po   $destdir/data/language/ccdciel_hints.ru.po
install -v -m 644 doc/doc_ccdciel_en.pdf    $destdir/doc/doc_ccdciel_en.pdf
