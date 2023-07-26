Summary: A CCD capture software intended for the amateur astronomer
Name: ccdciel
Version: 3
Release: 1
Group: Sciences/Astronomy
License: GPLv3+
URL: http://ccdciel.sourceforge.net
Packager: Patrick Chevalley
BuildRoot: %_topdir/%{name}
BuildArch: i386
Provides: ccdciel
Requires: libpasastro glib2 qt5pas libjpeg libpng fpack espeak exiv2
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

