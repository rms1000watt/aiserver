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
FORMS += afcnlistdialog.ui \
         afinddialog.ui \
         agodialog.ui \
         ahelpapidialog.ui \
         ahelpdialog.ui \
         ahelpfinddialog.ui \
         areplacedialog.ui \
         atabifydialog.ui \
         atabwidthdialog.ui \
         atestdialog.ui \
         ahelpalphadialog.ui \
         atexteditprefdialog.ui
HEADERS += afcnlistdialog.h \
           agodialog.h \
           ahelpapidialog.h \
           ahelpdialog.h \
           ahelpfinddialog.h \
           aiscpp.h \
           aishtml.h \
           aisjavascript.h \
           aislisp.h \
           aistext.h \
           ../include/aparameters.h \
           areplacedialog.h \
           atabifydialog.h \
           atabwidthdialog.h \
           atestdialog.h \
           atextcursor.h \
           atextdocument.h \
           atextrow.h \
           atextstaticdefs.h \
           aundo.h \
           afinddialog.h \
           ../include/atextedit.h \
           ahelpalphadialog.h \
           ../include/aoperationsdialog.h \
           ../include/aprefdialog.h \
           atexteditprefdialog.h
SOURCES += afcnlistdialog.cpp \
           afinddialog.cpp \
           agodialog.cpp \
           ahelpapidialog.cpp \
           ahelpdialog.cpp \
           ahelpfinddialog.cpp \
           areplacedialog.cpp \
           atabifydialog.cpp \
           atabwidthdialog.cpp \
           atestdialog.cpp \
           atextcursor.cpp \
           atextdocument.cpp \
           atextedit.cpp \
           atextrow.cpp \
           aundo.cpp \
           ahelpalphadialog.cpp \
           aoperationsdialog.cpp \
           aprefdialog.cpp \
           atexteditprefdialog.cpp
CONFIG += build_all debug_and_release
CONFIG(debug, debug|release) {
    TARGET = atexteditD
} else {
    TARGET = atextedit
}
