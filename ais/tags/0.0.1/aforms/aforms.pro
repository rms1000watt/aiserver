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
FORMS += aadduserdialog.ui \
    aaboutdialog.ui \
    acabinetpage.ui \
    acabsettingsdialog.ui \
    aclosedialog.ui \
    aconnectmgr.ui \
    aconsolepage.ui \
    adebugpage.ui \
    alogondialog.ui \
    alogpage.ui \
    amanagecabinetsdialog.ui \
    anodevaluedialog.ui \
    aregisterdirdialog.ui \
    aremotefiledialog.ui \
    asaveasagentdialog.ui \
    aserverdialog.ui \
    aserverform.ui \
    ausermgmtpage.ui \
    asysresmonpage.ui \
    awatchdialog.ui \
    aideprefdialog.ui
HEADERS += aadduserdialog.h \
    aaboutdialog.h \
    acabinetpage.h \
    acabsettingsdialog.h \
    aclosedialog.h \
    aconnectmgr.h \
    aconsolepage.h \
    adebugpage.h \
    aeditortab.h \
    aeditpage.h \
    aform.h \
    alogondialog.h \
    alogpage.h \
    amanagecabinetsdialog.h \
    anodevaluedialog.h \
    apage.h \
    aregisterdirdialog.h \
    aremotefiledialog.h \
    asaveasagentdialog.h \
    aserverdialog.h \
    aserverform.h \
    asessionform.h \
    aservertreemodel.h \
    ausermgmtpage.h \
    asysresmonpage.h \
    aideprefdialog.h \
    awatchdialog.h \
    codeeditor.h \
    ../include/amainwindow.h
SOURCES += aadduserdialog.cpp \
    aaboutdialog.cpp \
    acabinetpage.cpp \
    acabsettingsdialog.cpp \
    aclosedialog.cpp \
    aconnectmgr.cpp \
    aconsolepage.cpp \
    adebugpage.cpp \
    aeditortab.cpp \
    aeditpage.cpp \
    aform.cpp \
    alogondialog.cpp \
    alogpage.cpp \
    amanagecabinetsdialog.cpp \
    amainwindow.cpp \
    anodevaluedialog.cpp \
    apage.cpp \
    aregisterdirdialog.cpp \
    aremotefiledialog.cpp \
    asaveasagentdialog.cpp \
    aserverdialog.cpp \
    aserverform.cpp \
    aservertreemodel.cpp \
    asessionform.cpp \
    ausermgmtpage.cpp \
    asysresmonpage.cpp \
    awatchdialog.cpp \
    codeeditor.cpp \
    aideprefdialog.cpp
RESOURCES = aforms.qrc
INCLUDEPATH += ../include \
    ./
DESTDIR = ../lib
CONFIG += warn_on \
    qt \
    thread \
    x11 \
    staticlib \
    exceptions \
    rtti
TEMPLATE = lib
CONFIG += build_all \
    debug_and_release
CONFIG(debug, debug|release):TARGET = aformsD
else:TARGET = aforms
