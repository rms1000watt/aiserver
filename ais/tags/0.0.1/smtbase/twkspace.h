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
TWorkspace.h

Interface of the Workspace CLASS which stores a list of the active Smartviews. 
The manifest typing system together with C++ methods controls and size the data 
items and the array itself.

PARENT:             TObjVector 

AUTHORS:            Michael F. Korns

#endif
 

#ifndef _H_TWorkspace
#define _H_TWorkspace
#include "tobjvec.h"
#include "tsymbol.h"

#ifdef _SMARTBASE
/*  Function declarations */
//extern TVAL			TWorkspace_AddNewValue		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL newValue);
extern TVAL			TWorkspace_DeleteView		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL viewId);
extern TVAL			TWorkspace_FindIndexByObject(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TObject* anObj);
extern void			TWorkspace_Init				(LpXCONTEXT gCP,LpTHREAD gTP);
extern TWorkspace*	TWorkspace_New				(LpXCONTEXT gCP,LpTHREAD gTP);

/*  Method to function conversions */

extern TVAL			TWorkspace_GetIV1			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL index1);
extern TVAL			TWorkspace_GetIV2			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL index2);
extern TVAL			TWorkspace_Load				(LpXCONTEXT gCP, LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern TVAL			TWorkspace_MakeWorkspace	(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);       
extern TVAL			TWorkspace_SetIV1			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue);
extern TVAL			TWorkspace_SetIV2			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL index2,TVAL newValue);
#endif
#endif

