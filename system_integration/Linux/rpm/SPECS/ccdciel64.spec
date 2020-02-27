Summary: A CCD capture software intended for the amateur astronomer
Name: ccdciel
Version: 3
Release: 1
Group: Sciences/Astronomy
License: GPLv3+
URL: http://ccdciel.sourceforge.net
Packager: Patrick Chevalley
BuildRoot: %_topdir/%{name}
BuildArch: x86_64
Provides: ccdciel
Requires: libpasastro gtk2 glib2 pango libjpeg libpng fpack espeak exiv2
AutoReqProv: no

%description
It include all the features required to perform digital imaging CCD observation of celestial objects.
Using the standard drivers protocol INDI and ASCOM it can connect and control the CCD camera, the focuser, the filter wheel and the telescope mount.

%files
%defattr(-,root,root)
/usr/bin/ccdciel
/usr/share/ccdciel
/usr/share/metainfo/ccdciel.appdata.xml
/usr/share/applications/ccdciel.desktop
/usr/share/pixmaps/ccdciel.png
/usr/share/icons/hicolor/48x48/apps/ccdciel.png
/usr/share/doc/ccdciel

