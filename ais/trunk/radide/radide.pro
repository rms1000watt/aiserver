# /**********************************************************************************
# Copyright (C) 2013 AIS Foundation.
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
include(../mysql.prf)
INCLUDEPATH += ../include
DESTDIR = ..
CONFIG += warn_on qt thread exceptions rtti
TEMPLATE = app
SOURCES += main.cpp
QT += xml network
RC_FILE = radide.rc
CONFIG += build_all debug_and_release 
CONFIG(debug, debug|release) { 
    PRE_TARGETDEPS += ../lib/libglueD.a \
                      ../lib/libsmtbaseD.a \
                      ../lib/libasessionmgrD.a \
                      ../lib/libaissvrD.a \
                      ../lib/libahttpsvrD.a \
                      ../lib/libaxmlsvrD.a \
                      ../lib/libaformsD.a \
                      ../lib/libaradguiD.a \
                      ../lib/libalogmgrD.a \
                      ../lib/libappclientD.a \
                      ../lib/libappsvrD.a \
                      ../lib/libasysmgrD.a \
                      ../lib/libausrmgrD.a \
                      ../lib/libatexteditD.a \
                      ../lib/libautilitiesD.a \
                      ../lib/libaismgrD.a \
                      ../lib/libafilewatcherD.a \
                      ../lib/libjitD.a
    LIBS += -L../lib \
            ../lib/libglueD.a \
            ../lib/libsmtbaseD.a \
            ../lib/libasessionmgrD.a \
            -lglueD \
            -lsmtbaseD \
            -lasessionmgrD \
            -laissvrD \
            -lahttpsvrD \
            -laxmlsvrD \            
            -laformsD \
            -laradguiD \
            -lalogmgrD \
            -lappclientD \
            -lappsvrD \
            -lasysmgrD \
            -lausrmgrD \
            -latexteditD \
            -lautilitiesD \
            -laismgrD \
            -lafilewatcherD \
            -ljitD \
            $$MYSQL_LIBS
    win32:TARGET = radidedebug
    unix:TARGET = radidedebugexe
}
else {
    PRE_TARGETDEPS += ../lib/libglue.a \
                      ../lib/libsmtbase.a \
                      ../lib/libasessionmgr.a \
                      ../lib/libaissvr.a \
                      ../lib/libahttpsvr.a \
                      ../lib/libaxmlsvr.a \
                      ../lib/libaforms.a \
                      ../lib/libaradgui.a \
                      ../lib/libalogmgr.a \
                      ../lib/libappclient.a \
                      ../lib/libappsvr.a \
                      ../lib/libasysmgr.a \
                      ../lib/libausrmgr.a \
                      ../lib/libatextedit.a \
                      ../lib/libautilities.a \
                      ../lib/libaismgr.a \
                      ../lib/libafilewatcher.a \
                      ../lib/libjit.a
    LIBS += -L../lib \
            ../lib/libglue.a \
            ../lib/libsmtbase.a \
            ../lib/libasessionmgr.a \
            -lglue \
            -lsmtbase \
            -lasessionmgr \
            -laissvr \
            -lahttpsvr \
            -laxmlsvr \            
            -laforms \
            -laradgui \
            -lalogmgr \
            -lappclient \
            -lappsvr \
            -lasysmgr \
            -lausrmgr \
            -latextedit \
            -lautilities \
            -laismgr \
            -lafilewatcher \
            -ljit \
            $$MYSQL_LIBS
    win32:TARGET = radidedev
    unix { 
        TARGET = radidedevexe
        target.path = $$AIS_BIN_DIR

        INSTALLS += target
    }
}

