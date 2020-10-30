#/**********************************************************************************
#    Copyright (C) 2008 Investment Science Corp.
#
#    This program is free software: you can redistribute it and/or modify
#
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#***********************************************************************************/

include(../aisdev.prf)
RESOURCES = aistest.qrc 
INCLUDEPATH += ../include
DESTDIR = ..
CONFIG += warn_on qt thread x11 exceptions console
TEMPLATE = app 
FORMS += aistest.ui 
HEADERS += aistest.h 
SOURCES += main.cpp \
           aistest.cpp 
QT += xml \
      network
CONFIG += build_all debug_and_release
CONFIG(debug, debug|release) {
    PRE_TARGETDEPS += ../lib/libappclientD.a \
                      ../lib/libautilitiesD.a
    LIBS += ../lib/libappclientD.a \
            ../lib/libautilitiesD.a
    win32:TARGET = testaisclientdebug
    unix:TARGET = testaisclientdebugexe
} else {
    PRE_TARGETDEPS += ../lib/libappclient.a \
                      ../lib/libautilities.a
    LIBS += ../lib/libappclient.a \
            ../lib/libautilities.a
    win32:TARGET = testaisclientdev
    unix:TARGET = testaisclientdevexe
}

