#
# spec file for package ais-qt-static-devel (Version 4.4.x)
#
# Copyright 2008 Investment Science Corp.
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.
#
#
#
# When building a new version of Qt, just update the Version field
# and add an entry in the %changelog.
#

%if 0%{?suse_version}
BuildRequires: Mesa-devel cups-devel freetype2-devel gcc-c++ libjpeg-devel libmng-devel
BuildRequires: libpng-devel libtiff-devel pkgconfig sqlite-devel
BuildRequires: openssl-devel xorg-x11-devel dbus-1-devel glib2-devel
%endif

%if 0%{?fedora_version}
BuildRequires: gcc-c++ libstdc++-devel
BuildRequires: cups-devel freetype-devel libjpeg-devel libmng-devel libpng-devel libtiff-devel
BuildRequires: zlib-devel glib2-devel openssl-devel
BuildRequires: libICE-devel libSM-devel libXcursor-devel libXext-devel libXfixes-devel libXft-devel
BuildRequires: libXi-devel libXinerama-devel libXrandr-devel libXrender-devel libXt-devel libX11-devel xorg-x11-proto-devel libGL-devel libGLU-devel
%endif

Name:       qt-x11-opensource-src
License:    GPL
Group:      Development/Languages/Other
Version:    4.4.3
Release:    0
URL:        https://www.qtsoftware.com
Summary:    Qt for Open Source C++ development on Linux/X11
Source0:    %{name}-%{version}.tar.gz
BuildRoot:  %{_tmppath}/%{name}-%{version}-build

%description
Qt – A cross-platform application and UI framework.
Develop applications and user interfaces once, and deploy them across Windows, Mac, Linux/X11, embedded Linux, Windows CE and S60 (coming soon) without rewriting the source code.

%package -n ais-qt-static-devel
Summary:    Qt for Open Source C++ development on Linux/X11
Group:      Development/Languages/Other
%if 0%{?suse_version}
Requires:   zlib-devel c++_compiler pkgconfig
Requires:   freetype2-devel libmng-devel libpng-devel libtiff-devel  
Requires:   xorg-x11-devel Mesa-devel dbus-1-devel openssl-devel  
Requires:   glib2-devel
%endif
%if 0%{?fedora_version}
Requires: pkgconfig libjpeg-devel libpng-devel
Requires: zlib-devel openssl-devel
Requires: libICE-devel libSM-devel libXcursor-devel libXext-devel libXfixes-devel libXft-devel libXi-devel
Requires: libXinerama-devel libXrandr-devel libXrender-devel libXt-devel libX11-devel xorg-x11-proto-devel libGL-devel libGLU-devel
Requires: glib2-devel
%endif

%description -n ais-qt-static-devel
Qt – A cross-platform application and UI framework.
Develop applications and user interfaces once, and deploy them across Windows, Mac, Linux/X11, embedded Linux, Windows CE and S60 (coming soon) without rewriting the source code.

%prep
# extract the source and to into the qt-x11-opensource-src-x.x.x directory
%setup -q

%build
./configure -confirm-license \
			-prefix /usr \
			-bindir /usr/bin \
			-libdir %_libdir \
			-docdir /usr/share/qt4/doc \
			-headerdir /usr/include/qt4 \
			-datadir /usr/share/qt4 \
			-plugindir %_libdir/qt4/plugins \
			-translationdir /usr/share/qt4/translations \
			-sysconfdir /etc/xdg \
			-static \
			-fast \
			-silent \
			-qt-zlib \
			-qt-libtiff \
			-qt-libpng \
			-qt-libmng \
			-qt-libjpeg \
			-nomake examples \
			-nomake demos \
			-nomake docs \
			-nomake tools
make

%install
make INSTALL_ROOT=%buildroot install

%clean -n ais-qt-static-devel
rm -rf %buildroot

%post -n ais-qt-static-devel
/sbin/ldconfig

%postun -n ais-qt-static-devel
/sbin/ldconfig

%files -n ais-qt-static-devel
%defattr(-,root,root)
/usr/bin/*
%_libdir/*.*
%_libdir/pkgconfig/*
%dir /usr/include/qt4
%dir %_libdir/qt4
%dir /usr/share/qt4
%_libdir/qt4/*
/usr/include/qt4/*
/usr/share/qt4/*

%changelog
* Mon Jul 13 2009 - franklin.chua@gmail.com
- Moved to version 4.4.3
* Thu Apr 21 2009 - franklin.chua@gmail.com
- Initial version

