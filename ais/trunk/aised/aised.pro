# /**********************************************************************************
# Copyright (C) 2008 Investment Science Corp.
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
# ***********************************************************************************/
include(../aisdev.prf)
RESOURCES = aisedit.qrc
INCLUDEPATH += ../include
DESTDIR = ..
CONFIG += warn_on qt thread exceptions
TEMPLATE = app
HEADERS += aisedit.h
SOURCES += main.cpp \
    aisedit.cpp
QT += xml
RC_FILE = aised.rc
CONFIG += build_all \
    debug_and_release
CONFIG(debug, debug|release) {
    PRE_TARGETDEPS += ../lib/libatexteditD.a \
                      ../lib/libautilitiesD.a
    LIBS += ../lib/libatexteditD.a \
            ../lib/libautilitiesD.a
    win32:TARGET = aiseditdebug
    unix:TARGET = aiseditdebugexe
}
else {
    PRE_TARGETDEPS += ../lib/libatextedit.a \
                      ../lib/libautilities.a
    LIBS += ../lib/libatextedit.a \
            ../lib/libautilities.a
    win32:TARGET = aiseditdev
    unix { 
        TARGET = aiseditdevexe
        target.path = $$AIS_BIN_DIR

        INSTALLS += target
    }
}

