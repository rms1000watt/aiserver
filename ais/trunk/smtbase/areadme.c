/**********************************************************************************
    Copyright (C) 2008 Investment Science Corp.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

***********************************************************************************/

#if 0
AReadMe.c

This file contains special instructions and notes for the release version of Smartbase.
The 2003 version of MSFT Developer Studio has a few bugs in the C++ optimizer.
The following source modules, in release mode, must be left with the Optimizer settings OFF.

Windows 32-bit build in release mode:

FConio.c			Causes runcore.sl to crash when compiled with C++ optimizer on.
FConvert.c			Causes DeepGreen to get !bad object! when compiled with C++ optimizer on.
FSmartbase.c		Causes Developer Studio to crash when compiled with C++ optimizer on.
FUtil1.c			Causes runcore.sl to crash when compiled with C++ optimizer on.

Windows 64-bit build in release mode:

asessionmgr project files - Causes webide.exe to crash during compilation of lisp Lambdas after import. 
asbglue.cpp			Causes webide.exe to crash on startup when compiled with Full Optimization.

smtbase project:
fcompile.c			x-Causes webide.exe to crash on startup when compiled with Full Optimization.
fconio.c			x-Causes webide.exe to crash on startup when compiled with Full Optimization.
flisp.c				x-Causes runquick64.sl to crash when compiled with Full Optimization.
fmacro.c			x-Causes runquick64.sl to get an !arglist! error when compiled with Full Optimization.
fmath2.c			x-Causes tableagt.sl to get !badIdxOrKey! when compiled with Full Optimization.
fobject.c			x-Causes runquick64.sl to crash when compiled with Full Optimization.
fopt1.c				x-Causes runquick64.sl to crash when compiled with Full Optimization.
fpropty.c			x-Causes listrule.sl to crash when compiled with Full Optimization.
fsforms3.c			x-Causes /libraries/astartup.sl to crash on startup when compiled with Full Optimization on.
fsmtbase.c			Causes Developer Studio to crash when compiled with Full Optimization.
futil1.c			x-Causes webide.exe to crash on startup when compiled with Full Optimization.
futil2.c			x-Causes webide.exe to crash on startup when compiled with Full Optimization.
fvmscpt.c			Causes runquick64.sl to crash when compiled with Full Optimization.
tLambda.c			x-Causes runquick64.sl to crash when compiled with Full Optimization.
tdatabas.c			x-Causes runcore64.sl to crash when compiled with Full Optimization.
tneural.c			x-Causes runquick64.sl to crash when compiled with Full Optimization.
tobject.c			x-Causes runquick64.sl to crash when compiled with Full Optimization.
tvector.c			x-Causes runquick64.sl to crash when compiled with Full Optimization.


LINUX 64-bit build in release mode:

fpred2.c			This file should be compiled without any optimization.
					Causes RefGuide to get !badIdxOrKey! when compiled with optimization level 2 or 3.
fconio.c			Works with level 2 optimizations with -fschedule-insns and -fschedule-insns2 removed. (Minimum Optimization)
					Causes runcore64 test to get a !runScript: Cannot open script...! error during the 3rd pass for lisptime.sl.
fsmtbase.c			Works with level 2 optimizations with -fschedule-insns and -fschedule-insns2 removed. (Minimum Optimization)
					Causes Segmentation fault when running runquick.sl

AUTHORS:            Michael F. Korns

MODIFICATIONS:  
06/27/2007			Added special instructions for compilation on release mode for Linux 64-bit version

#endif
