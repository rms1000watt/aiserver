#
# spec file for package ais-mysql-devel (Version 5.1.x)
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
# When building a new version of MySQL, just update the Version field
# and add an entry in the %changelog.
#

%if 0%{?suse_version}
BuildRequires: gcc-c++ glibc-devel zlib-devel openssl-devel procps ncurses-devel readline-devel tcpd-devel
%endif

%if 0%{?fedora_version}
BuildRequires: gcc-c++ libstdc++-devel glibc-devel zlib-devel openssl-devel procps ncurses-devel readline-devel tcp_wrappers-devel
%endif

Name:       mysql
License:    GPL
Group:      Development/Languages/Other
Version:    5.1.36
Release:    0
URL:        https://www.mysql.com
Summary:    MySQL development files for AIS
Source0:    %{name}-%{version}.tar.gz
BuildRoot:  %{_tmppath}/%{name}-%{version}-build

%description
A minimum set of MySQL Development files, designed for building AIS.

%package -n ais-mysql-devel
Summary:    MySQL development files for AIS
Group:      Development/Languages/Other
%if 0%{?suse_version}
Requires:   gcc-c++ glibc-devel zlib-devel openssl-devel tcpd-devel
%endif
%if 0%{?fedora_version}
Requires:   gcc-c++ glibc-devel zlib-devel openssl-devel tcp_wrappers-devel
%endif

%description -n ais-mysql-devel
A minimum set of MySQL Development files, designed for building AIS.

%prep
# extract the source and to into the mysql-x.x.x directory
%setup -q

%build
./configure	--prefix=%{_prefix} \
			--libdir=%{_libdir} \
			--libexecdir=%{_prefix}/sbin \
			--infodir=%{_infodir} \
			--mandir=%{_mandir} \
			--enable-assembler \
			--with-pthread \
			--with-ssl=/usr \
			--without-server \
			--with-embedded-server \
			--without-docs \
			--without-man
make

%install
make DESTDIR=%{buildroot} install
rm -rf %{buildroot}%{_prefix}/mysql-test
rm -rf %{buildroot}%{_prefix}/sql-bench

%clean -n ais-mysql-devel
rm -rf %{buildroot}

%post -n ais-mysql-devel
/sbin/ldconfig

%postun -n ais-mysql-devel
/sbin/ldconfig

%files -n ais-mysql-devel
%defattr(-,root,root)
%{_prefix}/bin/*
%{_prefix}/sbin/*
%dir %{_libdir}/mysql
%dir %{_prefix}/share/mysql
%dir %{_prefix}/include/mysql
%{_libdir}/mysql/*
%{_prefix}/share/mysql/*
%{_prefix}/share/aclocal/*
%{_prefix}/include/mysql/*

%changelog
* Mon Jul 13 2009 - franklin.chua@gmail.com
- Moved to version 5.1.36
* Wed Apr 22 2009 - franklin.chua@gmail.com
- Initial version

