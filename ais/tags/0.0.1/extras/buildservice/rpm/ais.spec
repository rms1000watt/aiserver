#
# spec file for package ais-xxx (Version x.x.x)
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
# When building a new version of AIS, just update the Version field
# and add an entry in the %changelog.
#

%if 0%{?suse_version}
BuildRequires: gcc-c++ ais-qt-devel ais-mysql-devel pwdutils update-desktop-files
%endif

%if 0%{?fedora_version}
BuildRequires: gcc-c++ libstdc++-devel ais-qt-devel ais-mysql-devel
%endif

Name:       ais
License:    GPL
Group:      Development/Languages/Other
Version:    x.x.x
Release:    0
URL:        https://sourceforge.net/projects/aiserver/
Summary:    An IDE with High-Speed JIT Compiler for LISP
PreReq:     /usr/sbin/useradd /usr/sbin/groupadd
Source0:    %{name}-%{version}.tar.gz
BuildRoot:  %{_tmppath}/%{name}-%{version}-build

%description
AIS is a Development Platform providing a high-speed JIT compiler for LISP and JavaScript,
web server, object repositories, MySQL integration and libraries supporting a wide variety
of advanced genetic programming and symbolic regression techniques.

%package -n ais-common
Summary:    AIS common files
Group:      Development/Languages/Other

%description -n ais-common
This package includes files needed by all AIS packages.

%package -n ais-service
Summary:    AIS service
Requires:   ais-common
Group:      Development/Languages/Other

%description -n ais-service
This package includes the server binaries.

%package -n ais-ide
Summary:    AIS integrated development environment
Requires:   ais-common
Group:      Development/Languages/Other

%description -n ais-ide
This package includes the graphical client binaries.

%package -n ais-doc
Summary:    AIS documentation
Group:      Development/Languages/Other

%description -n ais-doc
This package includes the documentation.

%prep
# extract the source and to into the aiserver-x.x.x directory
%setup -q

%build
qmake -recursive aisdev.pro
make release

%install
make INSTALL_ROOT=%buildroot install
install -d %buildroot/var/log/ais
install -d %buildroot/var/lib/ais/mysqldata
install -d %buildroot/etc/init.d
install -d %buildroot/usr/share/applications
install -d %buildroot/usr/share/doc/ais/onlinedocs
install -d %buildroot/usr/share/pixmaps
install -m 755 extras/scripts/aissvc %buildroot/etc/init.d/aissvc
install -m 755 extras/scripts/ais-config %buildroot/usr/bin/ais-config
install -m 644 include/images/abase.XPM %buildroot/usr/share/pixmaps/aiserver.xpm
install -m 644 extras/desktop/ais-editor.desktop %buildroot/usr/share/applications/ais-editor.desktop
install -m 644 extras/desktop/ais-ride.desktop %buildroot/usr/share/applications/ais-ride.desktop
install -m 644 extras/desktop/ais-webide.desktop %buildroot/usr/share/applications/ais-webide.desktop
install -m 644 docs/onlinedocs/* %buildroot/usr/share/doc/ais/onlinedocs
%if 0%{?suse_version}
%suse_update_desktop_file ais-editor
%suse_update_desktop_file ais-ride
%suse_update_desktop_file ais-webide
%endif

%pre -n ais-common
if ! getent group ais > /dev/null; then
	# Adding system group: ais.
	/usr/sbin/groupadd -r ais > /dev/null
fi
if ! getent passwd ais > /dev/null; then
	# Adding system user: ais.
	/usr/sbin/useradd -r -o -g ais -c "AIS Service User" -d /var/lib/ais ais > /dev/null
fi

%post -n ais-common
chown -R ais:ais /etc/ais
chown -R ais:ais /usr/share/ais

%post -n ais-service
chown -R ais:ais /var/lib/ais
chown -R ais:ais /var/log/ais

%preun -n ais-service
%stop_on_removal aissvc

%postun -n ais-service
%restart_on_update aissvc
%insserv_cleanup

%clean
rm -rf %buildroot

%files -n ais-common
%defattr(-,root,root)
%dir /etc/ais
%dir /usr/share/ais
%config /etc/ais/aisinstall.ini
%config /etc/ais/rideinstall.ini
/usr/share/ais/*
/usr/bin/ais-config

%files -n ais-service
%defattr(-,root,root)
%dir /var/lib/ais
%dir /var/log/ais
/var/lib/ais/*
/usr/bin/aissvcdevexe
/etc/init.d/aissvc

%files -n ais-ide
%defattr(-,root,root)
/usr/bin/webidedevexe
/usr/bin/aiseditdevexe
/usr/bin/ridedevexe
/usr/share/pixmaps/aiserver.xpm
/usr/share/applications/ais-editor.desktop
/usr/share/applications/ais-ride.desktop
/usr/share/applications/ais-webide.desktop

%files -n ais-doc
%defattr(-,root,root)
%dir /usr/share/doc/ais
/usr/share/doc/ais/*

%changelog
* Mon Nov 23 2009 - franklin.chua@gmail.com
- Added Documentation
* Thu Jul 16 2009 - franklin.chua@gmail.com
- Added Desktop Menu files
* Wed Apr 22 2009 - franklin.chua@gmail.com
- Linked binaries with custom static Qt libraries
* Thu Apr 16 2009 - franklin.chua@gmail.com
- Update rules for separate packages
* Sun Apr 12 2009 - franklin.chua@gmail.om
- Updated rules for new files system
* Thu Mar 26 2009 - franklin.chua@gmail.com
- Added %%config directives
* Wed Mar 25 2009 - franklin.chua@gmail.com
- Added %%dir directives

