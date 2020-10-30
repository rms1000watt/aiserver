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

#define _SMARTBASE
#if 0
TCpx.c

Implementation of the complex class which stores the real and imaginary values
of a complex number.

PARENT:             TObject 

AUTHORS:            T. L. Williams

#endif

#include "tlambda.h"
#include "fconio.h"
#include "tcpx.h"

/*	------------------------------------------------------------------------------------------
TCpx_Init

Initialize the TCpx class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.
	--------------------------------------------------------------------------------------- */
void    TCpx_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
//  Don't initialize more than once.
if (gCP->TCpx_Initialized)
	return;
gCP->TCpx_Initialized  = TRUE;

//  Initialize the new type for this class.
FSmartbase_NewType(gCP,
					gTP,
					TYCPX,
					(LpCHAR)"Complex",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&TCpx_MakeNew,
					&TObject_MarkNever,
					&TObject_GlobalMarkNever,
					&FObject_ObjAnyCnv,
					&TCpx_Compare,
					&FObject_SetIV1Never,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&FObject_GetIV1Never,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_MapObject,
					&TObject_MapcObject,
					&TCpx_Print,
					&TCpx_Load,
					&TCpx_Save,
					&TCpx_ComputeSize,
					&TCpx_Copy,
					&FObject_DoomNever);
}

/*	-------------------------------------------------------------------------------------------
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Set aSize to your size requirements.
	---------------------------------------------------------------------------------------- */
void    TCpx_ComputeSize(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,NUM* aSize)
{
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
selfTval = selfTval; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk;		// Object index(long) + type(short) + pad = 16 bytes on 64-bit platform
*aSize += sizeof(TCpxOnDisk);		// 2 doubles = 16 bytes
ALLIGNME(*aSize);					// No change
}

/*	-------------------------------------------------------------------------------------------
Compare

Compare 2 complex numbers.  Return EQUAL if left==right, LOW if left < right, else HIGH.
	---------------------------------------------------------------------------------------- */
TVAL TCpx_Compare(LpXCONTEXT gCP, LpTHREAD gTP,TVAL left, TVAL right)
{
REAL		r0, r1, i0, i1;	// Real and imaginary parts
StartFrame
DeclareOBJ(TCpx, lp);		// -> TCpx left structure
DeclareOBJ(TCpx, rp);		// -> TCpx right structure
EndFrame

lp = (TCpx*) left.u.Object;
rp = (TCpx*)right.u.Object;

// Extract real and imaginary parts
r0 = lp->itsReal;
i0 = lp->itsImag;
r1 = rp->itsReal;
i1 = rp->itsImag;

// Test for equality
if (r0 == r1 && i0 == i1)
	FrameExit(gCP->TObject_EQUAL)
// Test for right < left
if (r0 < r1 || (r0 == r1 && i0 < i1))
	FrameExit(gCP->TObject_LOW)
else
	FrameExit(gCP->TObject_HIGH)
}

/*	------------------------------------------------------------------------------------------
Copy

Make a copy of a TCpx structure.
	---------------------------------------------------------------------------------------- */
TObject* TCpx_Copy(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval)
{
StartFrame
DeclareOBJ(TCpx, cp);	// -> existing TCpx structure
DeclareOBJ(TCpx, ncp);	// -> new copy of TCpx structure
EndFrame

cp = (TCpx*)selfTval.u.Object;
ncp = TCpx_New(gCP, gTP);

// Copy values into new structure.
ncp->itsReal = cp->itsReal;
ncp->itsImag = cp->itsImag;

FrameExit((TObject*)ncp);
}

/*	----------------------------------------------------------------------------------------
Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.
	------------------------------------------------------------------------------------- */
TVAL TCpx_Load(LpXCONTEXT gCP, LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
StartFrame
DeclareOBJ(TCpx, np);		// -> new TCpx structure in heap
DeclareTVAL(ret);
EndFrame

*ret = gCP->TObject_VOID;

if (bResolve == 0)
{	np = TCpx_New(gCP, gTP);
	*ret = TObject_RegisterLoad(gCP, gTP, theFileID, (TObject*)np);
}
else
{	np = (TCpx*)TObject_CheckRegistration(gCP, gTP, theFileID);
	if (np != NULL)
	{	asTag(ret) = TYCPX;
		asObject(ret) = (TObject*)np;
		np->itsReal = TCpxOnDiskPtr(aHMemory, 0)->itsReal;
		np->itsImag = TCpxOnDiskPtr(aHMemory, 0)->itsImag;
	}
    else
		*ret = gCP->TObject_ERROR_INVALID;
}

FrameExit(*ret);
}

/*	-------------------------------------------------------------------------------------------
MakeNew
Return a Cpx object with the specified initial values.  

Note:   If no arguments are specified, return an error. 
        If only one argument is specified, fill Cpx with zeros. 
        If only one real is specified, fill the imaginary part with zero.
            
        (new Complex:)             =>  #C(0.0 0.0) 
        (new Complex: 1.0)         =>  #C(1.0 0.0) 
        (new Complex: 1.0 2.0)     =>  #C(1.0 2.0) 
	---------------------------------------------------------------------------------------- */
TVAL TCpx_MakeNew(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])        
{

StartFrame
DeclareTVAL(ret);
DeclareOBJ(TCpx, np);
EndFrame

// Create a TCpx structure in the heap.
np = TCpx_New(gCP, gTP);

//  Initialize the complex TVAL
ret->Tag		= TYCPX;
ret->u.Complex = np;

if (argc > 0)
	{	
	np->itsReal = asNumIndex(&argv[0]);
	if (argc > 1) np->itsImag = asNumIndex(&argv[1]);
	}

FrameExit(*ret);
}

/*	-------------------------------------------------------------------------------------------
New

Create a new TCpx structure on the heap. 
	---------------------------------------------------------------------------------------- */
TCpx* TCpx_New(LpXCONTEXT gCP, LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TCpx, np);		// -> new TCpx structure in the heap
EndFrame

//  This class must be initialized
if (!gCP->TCpx_Initialized) TCpx_Init(gCP,gTP);

np = (TCpx*)TObject_OperatorNew(gCP, gTP);
np->itsObjectType = TYCPX;
np->itsReal = 0.0;
np->itsImag = 0.0;
np->itsMaxItemIndex = 0;
np->itsNilArray = (HMChar)&np->itsImmediatePtr;
np->itsImmediatePtr = (char *)&np->itsReal;
FrameExit(np);
}

/*	-------------------------------------------------------------------------------------------
Print

Convert a Cpx object into an ascii string and append it to an output buffer. 
	---------------------------------------------------------------------------------------- */
TVAL TCpx_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size, LpCHAR buf)
{
StartFrame
DeclareTVAL(item);
DeclareOBJ(TCpx,cp);
DeclareTVAL(ec);
EndFrame

cp = selfTval.u.Complex;		// -> Cpx structure on the heap

//  Quit if the output string is already too long
if (*size + 30 > gCP->TObject_MaxOutputLen) FrameExit(gCP->TObject_FALSE);

// Show cpx prefix
buf[*size]      = '#';
buf[++(*size)]  = 'c';
buf[++(*size)]	= 0;

// Tack on the real part of the complex number
if ((cp->itsReal != 0.0) || (cp->itsImag == 0.0))
	{
	asTag(item) = TYREAL;
	asReal(item) = cp->itsReal;
	*ec = FConio_sprintn(gCP, gTP, buf, size, *item);
	_TObject_ErrorChk(*ec);
	}
    
// Tack on the imaginary part of the complex number
if (cp->itsImag != 0.0)
	{
	if ((cp->itsReal != 0.0) && (cp->itsImag >= 0.0))
		{
		buf[*size]      = '+';
		buf[++(*size)]  = 0;
		}
	asTag(item) = TYREAL;
	asReal(item) = cp->itsImag;
	*ec = FConio_sprintn(gCP,gTP,buf,size,*item);
	_TObject_ErrorChk(*ec);
	buf[*size]      = 'i';
	buf[++(*size)]  = 0;
	}
    
buf[++(*size)]  = 0;
FrameExit(gCP->TObject_TRUE);
}

/*	-------------------------------------------------------------------------------------------
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.
	---------------------------------------------------------------------------------------- */
HMemory TCpx_Save(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, HMemory aHMemory)
{
long Offset;
StartFrame
DeclareOBJ(TCpx, cp);
EndFrame

cp = (TCpx*)selfTval.u.Object;

TObjectOnDiskPtr(aHMemory, 0)->itsObjectType = cp->itsObjectType;
Offset = SIZEOF_TObjectOnDisk;			// Offset = 8
TCpxOnDiskPtr(aHMemory, Offset)->itsReal = cp->itsReal;
TCpxOnDiskPtr(aHMemory, Offset)->itsImag = cp->itsImag;
//#define TObjectOnDiskPtr(h,n)   ((TObjectOnDisk*)(_HMemoryPtr(h)+n))
//#define SIZEOF_TObjectOnDisk    ((NUM)&((TObjectOnDisk*)0)->itsObjectData)

FrameExit(aHMemory);
}
// The end
