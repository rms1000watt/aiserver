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
LANGUAGE = C
QMAKE_CFLAGS_RELEASE = -fno-strict-aliasing
QMAKE_CFLAGS_DEBUG += -fno-strict-aliasing
INCLUDEPATH += ../include \
               ../smtbase
DESTDIR = ../lib 
CONFIG += warn_on staticlib
TEMPLATE = lib
win32 {
HEADERS = fvmintelp3jit.h
SOURCES = fvmintelp3jit.c
}
unix {
    contains(DEFINES,_M32) {
    HEADERS = fvmintelp3jit.h
    SOURCES = fvmintelp3jit.c
    }
    contains(DEFINES,_M64) {
    HEADERS = fvmamd64jit.h
    SOURCES = fvmamd64jit.c
    }
}
CONFIG += build_all debug_and_release
CONFIG(debug, debug|release) {
    TARGET = jitD
} else {
    TARGET = jit
}

