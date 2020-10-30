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
INCLUDEPATH += ../include
DESTDIR = ../lib 
CONFIG += warn_on qt thread staticlib exceptions
TEMPLATE = lib
HEADERS += ../include/aencrypt.h \
           ../include/aerrmsgs.h \
           ../include/aglobaldefs.h \
           ../include/aglobals.h \
           ../include/ais.h \
           ../include/anamedvalues.h \
           ../include/areqtypes.h \
           ../include/aringfile.h \
           ../include/ascriptparams.h \
           ../include/atimestamp.h \
           ../include/autilities.h 
SOURCES += aencrypt.cpp \
           anamedvalues.cpp \
           aringfile.cpp \
           atimestamp.cpp \
           autilities.cpp 
QT -= gui
CONFIG += build_all debug_and_release
CONFIG(debug, debug|release) {
    TARGET = autilitiesD
} else {
    TARGET = autilities
}
