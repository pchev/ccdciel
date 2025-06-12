Summary: A CCD capture software intended for the amateur astronomer
Name: ccdciel-qt6
Version: 3
Release: 1
Group: Sciences/Astronomy
License: GPLv3+
URL: http://ccdciel.sourceforge.net
Packager: Patrick Chevalley
BuildRoot: %_topdir/%{name}
BuildArch: x86_64
Obsoletes: ccdciel
Provides: ccdciel
Conflicts: ccdciel
Requires: libpasastro  libglib-2.0.so.0 libQt6Pas.so.6()(64bit) libjpeg libpng espeak exiv2
AutoReqProv: no

%description
It include all the features required to perform digital imaging CCD observation of celestial objects.
Using the standard drivers protocol INDI and ASCOM it can connect and control the CCD camera, the focuser, the filter wheel and the telescope mount.

%files
%defattr(-,root,root)
/usr/bin/ccdciel
/usr/share/ccdciel
/usr/share/metainfo/net.ap_i.ccdciel.metainfo.xml
/usr/share/applications/net.ap_i.ccdciel.desktop
/usr/share/pixmaps/ccdciel.png
/usr/share/icons/hicolor/32x32/apps/ccdciel.png
/usr/share/icons/hicolor/48x48/apps/ccdciel.png
/usr/share/icons/hicolor/96x96/apps/ccdciel.png
/usr/share/icons/hicolor/scalable/apps/ccdciel.svg
/usr/share/doc/ccdciel
