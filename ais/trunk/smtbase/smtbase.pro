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
include(../mysql.prf)
LANGUAGE = C
QMAKE_CFLAGS_RELEASE += -fno-strict-aliasing
QMAKE_CFLAGS_DEBUG += -fno-strict-aliasing
INCLUDEPATH += ../jit \
    ../include \
	./
DESTDIR = ../lib
CONFIG += warn_on staticlib
TEMPLATE = lib
HEADERS += ../include/fsmtbase.h \
    fcompile.h \
    foptimize.h \
    fconio.h \
    fcontrol.h \
    fconvert.h \
    fdatabas.h \
    fdatefnc.h \
    fdebug.h \
    fdefine.h \
    ffloat.h \
    flisp.h \
    flist.h \
    fmacro.h \
    fmake.h \
    fmath1.h \
    fmath2.h \
    fmath3.h \
    fmemory.h \
    fmysql1.h \
    fobject.h \
    fopt1.h \
    fopt2.h \
    fperfmon.h \
    fpred.h \
    fpred2.h \
    fproc.h \
    fpropty.h \
    fsforms1.h \
    fsforms2.h \
    fsforms3.h \
    fsim.h \
    fstatfnc.h \
    fstring.h \
    ftextfnc.h \
    futil1.h \
    futil2.h \
    futil3.h \
    fvmcode.h \
    fvmscpt.h \
    fvmscpt2.h \
    fwkspace.h \
    tlambda.h \
    tbitvec.h \
    tbytevec.h \
    tcontin.h \
    tcpx.h \
    tcpxvec.h \
    tdatabas.h \
    tdiction.h \
    tdirect.h \
    terror.h \
    tfltvec.h \
    tintvec.h \
    tmatrix.h \
    tneural.h \
    tnummat.h \
    tnumvec.h \
    tobject.h \
    tobjvec.h \
    tpair.h \
    tpcodvec.h \
    tbrick.h \
    tshortvec.h \
    tlongvec.h \
    tstring.h \
    tstruct.h \
    tsymbol.h \
    tvector.h \
    twkspace.h \
    ../include/fsmtbase.h
SOURCES += fcompile.c \
    foptimize.c \
    fconio.c \
    fcontrol.c \
    fconvert.c \
    fdatabas.c \
    fdatefnc.c \
    fdebug.c \
    fdefine.c \
    ffinance.c \
    ffloat.c \
    flisp.c \
    flist.c \
    fmacro.c \
    fmake.c \
    fmath1.c \
    fmath2.c \
    fmath3.c \
    fmemory.c \
    fmysql1.c \
    fobject.c \
    fopt1.c \
    fopt2.c \
    fpred.c \
    fpred2.c \
    fproc.c \
    fpropty.c \
    fsforms1.c \
    fsforms2.c \
    fsforms3.c \
    fsmtbase.c \
    fstatfnc.c \
    fstring.c \
    ftextfnc.c \
    futil1.c \
    futil2.c \
    futil3.c \
    fvmcode.c \
    fvmscpt.c \
    fvmscpt2.c \
    fwkspace.c \
    tlambda.c \
    tbitvec.c \
    tbytevec.c \
    tcontin.c \
    tcpx.c \
    tcpxvec.c \
    tdatabas.c \
    tdiction.c \
    tdirect.c \
    terror.c \
    tfltvec.c \
    tintvec.c \
    tmatrix.c \
    tneural.c \
    tnummat.c \
    tnumvec.c \
    tobject.c \
    tobjvec.c \
    tpair.c \
    tpcodvec.c \
    tbrick.c \
    tshortvec.c \
    tlongvec.c \
    tstring.c \
    tstruct.c \
    tsymbol.c \
    tvector.c \
    twkspace.c
CONFIG += build_all \
    debug_and_release
CONFIG(debug, debug|release):TARGET = smtbaseD
else:TARGET = smtbase
