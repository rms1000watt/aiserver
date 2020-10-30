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
TCpx.h

Implementation of the Cpx CLASS which stores a complex number with a real
and a complex part.

PARENT:             TObject 

AUTHORS:            T. L. Williams

#endif
 
#ifndef _H_TCpx
#define _H_TCpx

#include "tobject.h"

#define RealPart(tval)	((TCpx*)((tval).u.Object))->itsReal)
#define ImagPart(tval)	((TCpx*)((tval).u.Object))->itsImag)
#define Cpx(tval)		((TCpx*)((tval).u.Object))
#define asCpx(tvalp)	((TCpx*)(((TVAL*)tvalp)->u.Object))

//  The disk image size is just size of TCpx less itsObjectIndex and itsObjectType
//  as computed by SIZEOF_TCpxOnDisk.
typedef struct
{	REAL	itsReal;		//  Real part of complex number
	REAL	itsImag;		//  Imaginary part of complex number
}TCpxOnDisk;
#define TCpxOnDiskPtr(h,n)   ((TCpxOnDisk*)(_HMemoryPtr(h)+n))

#ifdef _SMARTBASE
//  Function declarations
extern void		TCpx_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL		TCpx_Compare	(LpXCONTEXT gCP, LpTHREAD gTP,TVAL left, TVAL right);
extern void		TCpx_ComputeSize(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,NUM* aSize);
extern TObject*	TCpx_Copy		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval);
extern TVAL		TCpx_Insert		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index,TVAL newValue);
extern TVAL		TCpx_Load		(LpXCONTEXT gCP, LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern TVAL		TCpx_MakeNew	(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TCpx*	TCpx_New		(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL     TCpx_Print		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, LpNUM size, LpCHAR buf);
extern HMemory	TCpx_Save		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,HMemory aHMemory);

#endif	// _SMARTBASE
#endif	//	_H_TCpx 

