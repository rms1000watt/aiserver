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
Tcpxvec.c

Implementation of complex vector which stores an array of complex numbers as
pairs of doubles.

PARENT:             TObject 

AUTHORS:            T. L. Williams

#endif

//	--------------------------------- IMPORTS ------------------------------------------------
#include "tlambda.h"
#include "fconio.h"
#include "tcpx.h"
#include "tcpxvec.h"

// ---------------------------------- LOCAL DECLARATIONS -----------------------------------
static TVAL TCpxVector_PrintPair(LpXCONTEXT gCP,LpTHREAD gTP,LpREAL rp,LpNUM sizep,LpCHAR buf);

/*	------------------------------------------------------------------------------------------
TCpxVector_Init

Initialize the Tnumcpx class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.
	--------------------------------------------------------------------------------------- */
void TCpxVector_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
//  Don't initialize more than once.
if (gCP->TCpxVector_Initialized)
	return;
gCP->TCpxVector_Initialized  = TRUE;

//  Initialize the new type for this class.
FSmartbase_NewType(gCP,
					gTP,
					TYCPXVECTOR,
					(LpCHAR)"CpxVector",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&TCpxVector_MakeNew,
					&TCpxVector_Mark,
					&TObject_GlobalMarkNever,
					&FObject_ObjAnyCnv,
					&FObject_CompareNever,
					&TCpxVector_SetIV1,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&TCpxVector_GetIV1,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_Map,
					&TObject_Mapc,
					&TCpxVector_Print,
					&TCpxVector_Load,
					&TCpxVector_Save,
					&TCpxVector_ComputeSize,
					&TCpxVector_Copy,
					&TCpxVector_Doomed);
}
/*	----------------------------------------------------------------------------------------
AddNewValue

Add a new value to the repeating portion of a complex vector.
	------------------------------------------------------------------------------------- */
TVAL TCpxVector_AddNewValue(LpXCONTEXT gCP,LpTHREAD gTP,TVAL tval,TVAL newValue)
{
StartFrame
DeclareOBJ(TCpxVector, vp);
DeclareTVAL(ndx);
EndFrame

vp = tval.u.CpxVector;
ndx->u.Int = vp->itsMaxItemIndex;
ndx->Tag = TYNUM;
FrameExit(TCpxVector_SetIV1(gCP, gTP, tval, *ndx, newValue));
}

/*	-------------------------------------------------------------------------------------------
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data on disk. 
Set aSize to your size requirements.
	---------------------------------------------------------------------------------------- */
void    TCpxVector_ComputeSize(LpXCONTEXT gCP, LpTHREAD gTP,TVAL tval,NUM* pSize)
{
TCpxVector* cp = tval.u.CpxVector;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
*pSize = SIZEOF_TObjectOnDisk;		// Object index(long) + type(short) + pad
*pSize += SIZEOF_TCpxVectorOnDisk;	// Max index (long) + cdr (tval)
*pSize += cp->itsMaxItemIndex * 2 * sizeof(REAL); // Size * 2 * 8 bytes
ALLIGNME(*pSize);					// No change
}

/*	-------------------------------------------------------------------------------------------
Compare

Compare 2 complex numbers.  Return EQUAL if left==right, LOW if left < right, else HIGH.
	---------------------------------------------------------------------------------------- */
TVAL TCpxVector_Compare(LpXCONTEXT gCP, LpTHREAD gTP,TVAL left, TVAL right)
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

Make a copy of a Complex vector.
	---------------------------------------------------------------------------------------- */
TObject* TCpxVector_Copy(LpXCONTEXT gCP, LpTHREAD gTP, TVAL tval)
{
NUM		ix, sz;			// loop index, array size
REAL	*sp, *dp;		// -> source array, -> destination array
StartFrame
DeclareTVAL(cpy);			// Holds tval for the new copy
DeclareOBJ(TCpxVector, vp);	// -> existing vector header structure
DeclareOBJ(TCpxVector,nvp);	// -> new copy of vector header structure
EndFrame

// Initialize
vp = tval.u.CpxVector;
sz = vp->itsMaxItemIndex;

// Construct a new complex vector of the same size as the original
cpy->Tag = TYCPXVECTOR;
cpy->u.CpxVector = nvp = TCpxVector_New(gCP, gTP);
if (sz > 0 && vp->itsCpxArray != NULL)
{	// Copy values into the header for the new vector.
	TCpxVector_SetMaxIndex(gCP, gTP, *cpy, sz);
	dp = (LpREAL)(*nvp->itsCpxArray);
	sp = (LpREAL)(*vp->itsCpxArray);
	for (ix = 0; ix < 2 * sz; ++ix)
		*dp++ = *sp++;
}
// Copy the cdr
if (vp->itsCdr.Tag != TYVOID)
	nvp->itsCdr = vp->itsCdr;

FrameExit((TObject*)nvp)
}
/*	------------------------------------------------------------------------------------------
Delete
Delete the indexed value from the repeating portion of this object.

Note:   All of the remaining values are moved down one position and the Vector is resized.
	---------------------------------------------------------------------------------------- */
TVAL TCpxVector_Delete(LpXCONTEXT gCP, LpTHREAD gTP, TVAL tval, TVAL index)
{
register LpREAL         dstp;		// -> destination element in array of pairs
register LpREAL         srcp;		// -> source element in array of pairs
register LpREAL         hltp;		// -> halt position
NUM                     di;			// index of complex value to be deleted
NUM						sz;			// new size of the vector
StartFrame
DeclareOBJ(TCpxVector, vp);
EndFrame

//  Make sure index is in range
vp = tval.u.CpxVector;
if (!isNumIndex(&index) || (di = asNumIndex(&index)) < 0 || di >= vp->itsMaxItemIndex)
    FrameExit(gCP->TObject_ERROR_INVALID)

//  Move all of the elements past index in the array down one position
if (vp->itsCpxArray != NULL && (sz = vp->itsMaxItemIndex - 1) >= 0)
{	hltp = (LpREAL)*vp->itsCpxArray;
	dstp = hltp + 2 * di;	// -> start of pair to be deleted
	srcp = dstp + 2;		// -> pair just past deleted pair
	hltp += 2 * sz;			// -> last element of the array
	while (srcp <= hltp)
		*dstp++ = *srcp++;

	//  Decrement the size of the Vector
	TCpxVector_SetMaxIndex(gCP, gTP, tval, sz);
}
FrameExit(gCP->TObject_OK)
}

/*	------------------------------------------------------------------------------------------
Doomed

Garbage collection is about to delete this object. Dispose of the array data.

Note:   This method should only be called by mark and sweep garbage collection!
        This method warns the object that it is about to be deleted. Garbage
        collection first warns all the doomed objects, then it deletes all doomed
        objects.
        
        Do close any files and clean up anything necessary here.
        Do free any resources which you have allocated of which you are the sole owner.

        Do not send delete or dispose messages to any referenced objects,
        Let garbage collection do this.

        Do not delete or release any of your own storage here.
        Let garbage collection do this.
	---------------------------------------------------------------------------------------- */
void    TCpxVector_Doomed(LpXCONTEXT gCP, LpTHREAD gTP,TVAL tval)
{
TCpxVector* vp = tval.u.CpxVector;

/* If immediate data space is used, do NOT try to release a memory handle. */
if (vp->itsImmediatePtr != NULL)
	{
	vp->itsMaxItemIndex = 0;																				 
	vp->itsCpxArray = NULL;
	vp->itsImmediatePtr = NULL;
	return;
	}

if (vp->itsCpxArray != NULL) FMemory_Free(gCP, gTP, (HMemory)vp->itsCpxArray);
vp->itsMaxItemIndex = 0;
vp->itsCpxArray = NULL;
}
/*	----------------------------------------------------------------------------------------
GetCdr

Return the size of the repeating portion of this object.
	------------------------------------------------------------------------------------- */
TVAL TCpxVector_GetCdr(LpXCONTEXT gCP, LpTHREAD gTP, TVAL tval)
{
TCpxVector* vp = tval.u.CpxVector;
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(vp->itsCdr);
}

/*	----------------------------------------------------------------------------------------
GetIV1

Return the indexed value from the repeating portion of this object.
	------------------------------------------------------------------------------------- */
TVAL TCpxVector_GetIV1(LpXCONTEXT gCP, LpTHREAD gTP,TVAL tval,TVAL ndx)
{
NUM			ix;			// index into array of pairs of doubles
LpREAL		rp;			// -> pairs of doubles
StartFrame
DeclareOBJ(TCpxVector, vp);
DeclareOBJ(TCpx, cp);
DeclareTVAL(ret);
EndFrame

//  Accept a numeric index
if (!isNumIndex(&ndx) || (ix = asNumIndex(&ndx)) < 0 || tval.Tag != TYCPXVECTOR)
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY)

// Make sure array index is in range
vp = tval.u.CpxVector;
if (ix >= vp->itsMaxItemIndex)
	FrameExit(gCP->TObject_VOID)

//  Save the new complex number in the ixth pair of doubles
cp = TCpx_New(gCP, gTP);
if (vp->itsCpxArray != NULL)
{	rp = (LpREAL)(*vp->itsCpxArray) + 2 * ix;		// -> ixth pair of doubles
	cp->itsReal = *rp++;
	cp->itsImag = *rp;
}
// Return ixth element in a Complex number tval
ret->Tag = TYCPX;
ret->u.Complex = cp;
FrameExit(*ret);
}

/*	----------------------------------------------------------------------------------------
Insert

Insert the indexed value in the repeating portion of this object. 

Note:   All of the higher values are moved up one position and the Vector is resized.
	------------------------------------------------------------------------------------- */
TVAL TCpxVector_Insert(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index,TVAL newValue)
{
register LpREAL         dstp;
register LpREAL         srcp;
register LpREAL         insp;
NUM                     sz;
NUM                     insertIndex;
StartFrame
DeclareOBJ(TCpxVector, vp);
DeclareOBJ(TCpx, cp);

EndFrame

//  Index must be numeric and in range
if (!isNumIndex(&index) || (insertIndex = asNumIndex(&index)) < 0 ||
	 (vp = selfTval.u.CpxVector) == NULL || insertIndex > vp->itsMaxItemIndex)
    FrameExit(gCP->TObject_ERROR_INVALID)

//  Move all of the remaining values in the Vector up one position
if (vp->itsCpxArray != NULL && newValue.Tag == TYCPX && newValue.u.Complex != NULL)
{	//  Increase the array size by one
	sz = vp->itsMaxItemIndex + 1;
	TCpxVector_SetMaxIndex(gCP, gTP, selfTval, sz);
	sz *= 2;								// New array size * 2
	dstp = (LpREAL)*vp->itsCpxArray + sz;	// -> just past last double
	srcp = dstp - 2;						// -> past next-to-last pair
	insp = (LpREAL)*vp->itsCpxArray + 2 * insertIndex;	// -> point of insertion in array
	while (srcp >= insp)
		*--dstp = *--srcp;

	//  Insert the new complex pair in the Vector at the specified position
	cp = newValue.u.Complex;
	*insp++ = cp->itsReal;
	*insp = cp->itsImag;
}
FrameExit(gCP->TObject_OK)
}

/*	----------------------------------------------------------------------------------------
GetMaxIndex

Return the size of the repeating portion of this object.
	------------------------------------------------------------------------------------- */
NUM TCpxVector_GetMaxIndex(LpXCONTEXT gCP, LpTHREAD gTP, TVAL tval)
{
TCpxVector* vp = tval.u.CpxVector;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(vp->itsMaxItemIndex);
}

/*	----------------------------------------------------------------------------------------
Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.
	------------------------------------------------------------------------------------- */
TVAL TCpxVector_Load(LpXCONTEXT gCP, LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
NUM			ix, sz;		// Loop index, size of source array.
REAL		*sp, *dp;	// -> array of source pairs, ->array of destination pairs

StartFrame
DeclareOBJ(TCpxVector, vp);	// -> new header structure in heap
DeclareOBJ(TCpxVectorOnDisk, odp);	// on-disk ptr
DeclareTVAL(ret);
EndFrame

*ret = gCP->TObject_VOID;

if (bResolve == 0)
{	vp = TCpxVector_New(gCP, gTP);
	*ret = TObject_RegisterLoad(gCP, gTP, theFileID, (TObject*)vp);
}
else
{	vp = (TCpxVector*)TObject_CheckRegistration(gCP, gTP, theFileID);
	if (vp != NULL)
	{	ret->Tag = TYCPXVECTOR;
		ret->u.CpxVector = vp;
		odp = (TCpxVectorOnDisk *)(*aHMemory);
		if ((sz = odp->itsMaxItemIndex) > 0)
		{	TCpxVector_SetMaxIndex(gCP, gTP, *ret, odp->itsMaxItemIndex);
			sp = (LpREAL)(odp->itsItemArray);
			dp = (LpREAL)(*vp->itsCpxArray);
			for (ix = 0; ix < sz * 2; ++ix)
				*dp++ = *sp++;
		}
	}
}

FrameExit(*ret);
}

/*	-------------------------------------------------------------------------------------------
MakeNew
Return a Cpx vector with the specified size initialized with the optional values provided.

argv[0]    - number of elements.
argv[1]... - initial values
argv[n]    - .  (a period)
argv[n+1]  - cdr values

Examples:
	(new CpxVector: 2)				=>	#(cpx| 0,0 0,0 0,0) 
	(new CpxVector: 2 1)			=>	#(cpx| 1,0 1,0 1,0) 
	(new CpxVector: 2 1 2 3)		=>	#(cpx| 1,2 3,0)
	(new CpxVector: "~" 1 2 3 4)	=>  #(cpx| 1,2 3,4)
	(new CpxVector: 2 1 2 3 4 . 5 6)=>	#(cpx| 1,2 3,4 . 5,6) 

Note:
	The first argument (CpxVector) is stripped off by FProperty_Factory.
	If no arguments are specified, set size to 0.
	If first arg is "~", set size to half the number of remaining args.
	If only one argument is specified, fill the vector with zeros. 
	If too few arguments are specified, fill the vector with repeating
	patterns of the specified initializers.
	If a period is encountered, assign the cdr with the next 2 values.
	MakeNew is also called by FMake_Vector for (new Vector: complex: ...)
	---------------------------------------------------------------------------------------- */
TVAL TCpxVector_MakeNew(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])        
{
NUM			size = 0;
NUM			index = 0;
NUM			vi;			// Vector index
NUM			ai = 1;		// Argument index
NUM			ni;			// Number of initializers		
NUM			ci = 0;		// Cdr index
StartFrame
DeclareTVAL(ret);
DeclareTVAL(ndx);
DeclareTVAL(fill);
DeclareOBJ(TCpxVector, vp);
DeclareOBJ(TCpx, cp);
EndFrame
 
//  The first argument should be the requested size.
if (argc > 0)
{	// If first arg is "~", size is 1/2 number of remaining args
	if (argv[0].Tag == TYTEXT && argv[0].u.Text[0] == '~')
		{	
		for (index = 1; (index < argc) && (isNumIndex(&argv[index])); ++index) 
			{
			if (argv[index].Tag == TYCPX) 
				size += 2;
			else
			if (isNumIndex(&argv[index])) 
				++size;
			}
		size = (size + 1) / 2;			// Rounds size up
		}
	else if (isNumIndex(&argv[0]))	// else, set size to first arg (if numeric)
		size = (NUM)asNumIndex(&argv[0]);
	else
		size = ai = 0;
}
// Skip over the size arg
if (ai > 0)
{	argv = &argv[1];
	--argc;
}
//  Initialize the returned complex vector
ret->Tag = TYCPXVECTOR;
ret->u.CpxVector = vp = TCpxVector_New(gCP, gTP);
if (size > 0)	
	TCpxVector_SetMaxIndex(gCP,gTP, *ret, size);

//  Initialize the vector only if necessary
if (argc <= 0)
	FrameExit(*ret)

// Initialize tval header to hold complex vector
ndx->Tag = TYNUM;
ndx->u.Int = 0;
cp = TCpx_New(gCP, gTP);
fill->Tag = TYCPX;
fill->u.Complex = cp;

// Scan remaining args to determine end of initializers, cdr
for (ni = argc, ai = 0; ai < argc; ++ai)
{	// A period denotes a cdr
	if (argv[ai].Tag == TYPCODE && argv[ai].u.Short == PERIODTOK)
	{	// Note end of initializers
		ni = ai;
		// If at least one more arg, make a note
		if (++ai < argc && argv[ai].Tag != TYVOID)
			ci = ai;
		break;
	}
	// Terminate list of initializers if arg is not complex and is not a number
	if (!isNumIndex(&argv[ai]) && argv[ai].Tag != TYCPX)
	{	ni = ai;
		break;
	}
}
// Now initialize complex array
for (ai = vi = 0; vi < size; ++vi)
{	// Wrap ai if at end
	if (ai >= ni) ai = 0;

	// Set index into complex number array
	ndx->u.Int = vi;

	// Convert one or 2 reals to complex
	if (argv[ai].Tag == TYCPX)
		TCpxVector_SetIV1(gCP, gTP, *ret, *ndx, argv[ai++]);
	else
	if (isNumIndex(&argv[ai]))
		{	
		cp->itsReal = (REAL)asNumIndex(&argv[ai]);
		if (++ai < ni && isNumIndex(&argv[ai]))
			{	
			cp->itsImag = (REAL)asNumIndex(&argv[ai]);
			++ai;
			}
		else
			cp->itsImag = 0.0;

		TCpxVector_SetIV1(gCP, gTP, *ret, *ndx, *fill);
		}
	else
		break;
}
//	Process the cdr, if any
if (ci > 0)
{	// Convert 2 numbers to complex
	if (isNumIndex(&argv[ci]) && ci + 1 < argc)
	{	cp->itsReal = (REAL)asNumIndex(&argv[ci]);
		++ci;
		if (isNumIndex(&argv[ci]))
			cp->itsImag = (REAL)asNumIndex(&argv[ci]);
		else
			cp->itsImag = 0.0;
		vp->itsCdr = *fill;
	}
	else
		vp->itsCdr = argv[ci];
}
FrameExit(*ret)
}

/*	-------------------------------------------------------------------------------------------
Mark

Garbage collection is marking this object. Mark any objects which you reference.

Note:   This method should only be called by mark and sweep garbage collection!
        Do send mark messages to any referenced objects,
	---------------------------------------------------------------------------------------- */
void    TCpxVector_Mark(LpXCONTEXT gCP, LpTHREAD gTP,TVAL tval)
{
TCpxVector*  vp = tval.u.CpxVector;

//  Mark the complex vector's tail(cdr) so its won't be garbage collected.
TObject_MarkTval(gCP, gTP, vp->itsCdr);
}

/*	-------------------------------------------------------------------------------------------
New

Create a new header in the heap for a Cpx vector.  Requires 28 bytes
	---------------------------------------------------------------------------------------- */
TCpxVector* TCpxVector_New(LpXCONTEXT gCP, LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TCpxVector, vp);	// -> new header structure in the heap
EndFrame

//  This class must be initialized
if (!gCP->TCpxVector_Initialized) TCpxVector_Init(gCP,gTP);

vp = (TCpxVector*)TObject_OperatorNew(gCP, gTP);
vp->itsObjectType = TYCPXVECTOR;
vp->itsMaxItemIndex = 0;
vp->itsCdr.Tag = TYVOID;
vp->itsCpxArray = NULL;
vp->itsImmediatePtr = NULL;
FrameExit(vp);
}

/*	-------------------------------------------------------------------------------------------
Print

Convert a Complex vector into an ascii string and append it to an output buffer. 
	---------------------------------------------------------------------------------------- */
TVAL TCpxVector_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL tval, LpNUM sizep, LpCHAR buf)
{
NUM		ix;			// Loop index
LpREAL	rp;			// -> array of pairs of reals
StartFrame
DeclareTVAL(ec);
DeclareOBJ(TCpxVector, vp);
DeclareOBJ(TCpx, cp);
EndFrame

//  Tack on the prefix
if (*sizep + 7 > gCP->TObject_MaxOutputLen) FrameExit(gCP->TObject_FALSE);
strcpy(buf + *sizep, "#(cpx| ");
*sizep += 7;

if ((vp = tval.u.CpxVector) != NULL)		// -> CpxVector header structure on the heap
{	if (vp->itsCpxArray != NULL)
	{	rp = (LpREAL)(*vp->itsCpxArray);
		for (ix = 0; ix < vp->itsMaxItemIndex; ++ix, rp += 2)
		{	// Print the next pair
			TCpxVector_PrintPair(gCP, gTP, rp, sizep, buf);
		}
	}
	// Print the cdr, if any
	if (vp->itsCdr.Tag != TYVOID)
	{	if (*sizep + 3 > gCP->TObject_MaxOutputLen) FrameExit(gCP->TObject_FALSE);
		strcpy(buf + *sizep, ". ");
		*sizep += 2;
		*ec = FConio_sprintn(gCP, gTP, buf, sizep, vp->itsCdr);
		_TObject_ErrorChk(*ec);
	}
}
// Tack on a closing paren 
buf[*sizep]   = ')';
buf[++*sizep] = '\0';
FrameExit(gCP->TObject_TRUE)
}

/*	-------------------------------------------------------------------------------------------
Print one pair of doubles

Print the real part and imaginary part of a complex number separated by spaces.
Note:
	rp must point to an array of at least 2 doubles 
	---------------------------------------------------------------------------------------- */
static TVAL TCpxVector_PrintPair(LpXCONTEXT gCP,LpTHREAD gTP,LpREAL rp,LpNUM sizep,LpCHAR buf)
{
StartFrame
DeclareTVAL(item);
DeclareTVAL(ec);
EndFrame

// Initialize
item->Tag = TYREAL;

// Print the first real
item->u.Real = *rp++;
buf[*sizep]  = '#';
buf[++*sizep]  = 'c';
buf[++*sizep]  = '\0';
*ec = FConio_sprintn(gCP, gTP, buf, sizep, *item);
_TObject_ErrorChk(*ec);
// Print the imaginary part
item->u.Real = *rp;
if (item->u.Real >= 0.0)
	{ 
	buf[*sizep]  = '+';
	buf[++*sizep]  = '\0';
	}
else
	{
	buf[*sizep]  = '\0';
	}
*ec = FConio_sprintn(gCP, gTP, buf, sizep, *item);
_TObject_ErrorChk(*ec);
buf[*sizep] = 'i';
buf[++*sizep] = ' ';
buf[++*sizep] = '\0';

FrameExit(gCP->TObject_TRUE)
}

/*	-------------------------------------------------------------------------------------------
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.
	---------------------------------------------------------------------------------------- */
HMemory TCpxVector_Save(LpXCONTEXT gCP, LpTHREAD gTP,TVAL tval, HMemory aHMemory)
{
NUM		ix, offs, sz;	// Loop index, offset into on-disk structure, size of real arrays
REAL	*sp, *dp;		// -> source, destination arrays of pairs of doubles
StartFrame
DeclareOBJ(TCpxVector, vp);		// -> source header in heap
DeclareOBJ(TCpxVectorOnDisk, odp);	// -> on-disk destination header
EndFrame

vp = tval.u.CpxVector;
TObjectOnDiskPtr(aHMemory,0)->itsObjectType = vp->itsObjectType;
offs = SIZEOF_TObjectOnDisk;
odp = (TCpxVectorOnDisk *)(_HMemoryPtr(aHMemory) + offs);
odp->itsCdr.Tag = TYVOID;

odp->itsMaxItemIndex = sz = vp->itsMaxItemIndex;
if (sz > 0)
{	sp = (LpREAL)(*vp->itsCpxArray);
	dp = (LpREAL)(odp->itsItemArray);
	for (ix = 0; ix < 2 * sz; ++ix)
		*dp++ = *sp++;
}
FrameExit(aHMemory)
}

/*	----------------------------------------------------------------------------------------
SetCdr

The cdr can be anything that will fit into a TVAL.  Typically, it is a complex number.
	------------------------------------------------------------------------------------- */
TVAL TCpxVector_SetCdr(LpXCONTEXT gCP, LpTHREAD gTP, TVAL tval, TVAL newCdr)
{
TCpxVector* vp = tval.u.CpxVector;

gTP = gTP; // NOOP to hide unused parameter warning message
vp->itsCdr = newCdr;

return(gCP->TObject_OK);
}

/*	----------------------------------------------------------------------------------------
SetIV1

Set the indexed value in the repeating portion of this object.
	------------------------------------------------------------------------------------- */
TVAL TCpxVector_SetIV1(LpXCONTEXT gCP, LpTHREAD gTP,TVAL tval,TVAL ndx, TVAL newValue)
{
NUM		ix;				// Index into first real of an array of pairs of doubles
LpREAL	rp;				// -> arrays of pairs of doubles in the heap

StartFrame
DeclareOBJ(TCpxVector, vp);
DeclareOBJ(TCpx, cp);
EndFrame

//  Accept a numeric index
if (!isNumIndex(&ndx) || (ix = asNumIndex(&ndx)) < 0 ||
		tval.Tag != TYCPXVECTOR || newValue.Tag != TYCPX)
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY)

//  Make sure array index is in range. If ix too large, then grow the range
vp = tval.u.CpxVector;
if (ix >= vp->itsMaxItemIndex)
    TCpxVector_SetMaxIndex(gCP, gTP, tval, ix+1);

//  Save the new complex number in the ixth pair of doubles
cp = newValue.u.Complex;
if (vp->itsCpxArray != NULL)
{	rp = (LpREAL)(*vp->itsCpxArray) + 2 * ix;		// -> ixth pair of doubles
	*rp++ = cp->itsReal;
	*rp   = cp->itsImag;
}
FrameExit(tval)
}

/*	----------------------------------------------------------------------------------------
SetMaxIndex

Set the maximum size of the repeating portion of this object.

Note:   Remember the repeating portion of this object is measured in integers!
	------------------------------------------------------------------------------------- */
TVAL TCpxVector_SetMaxIndex(LpXCONTEXT gCP, LpTHREAD gTP, TVAL tval, NUM newSize)
{
NUM			size;
NUM			n;
NUM			oldMaxItemIndex;
LpREAL		ptr;
StartFrame
DeclareOBJ(TCpxVector, vp);
EndFrame

size = 2 * sizeof(REAL) * newSize;		// Size in bytes

//  Do not allow negative lengths
if ((newSize < 0) || (tval.Tag != TYCPXVECTOR)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY)
vp = tval.u.CpxVector;
oldMaxItemIndex = vp->itsMaxItemIndex;

/* Use immediate data space (if the requested size will allow). */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if (size <= (NUM)_TCpxVector_ImmediateSpace)
	{
	if (vp->itsCpxArray == NULL) 
		{
		_FMemory_memset(vp->itsImmediateSpace,0,size);
		}
	else
		{
		_FMemory_memcpy(vp->itsImmediateSpace,(char*)CpxArray(tval),min(vp->itsMaxItemIndex*2*(NUM)sizeof(REAL),size));
		if ((vp->itsCpxArray != NULL) && (vp->itsImmediatePtr == NULL)) FMemory_Free(gCP,gTP,(HMemory)vp->itsCpxArray);
		}
	vp->itsCpxArray = (HMReal)&vp->itsImmediatePtr;
	vp->itsImmediatePtr = (CHAR*)&vp->itsImmediateSpace[0];
    vp->itsMaxItemIndex = newSize;

	if ((vp->itsCpxArray != NULL) && (vp->itsImmediatePtr == NULL)) 
		{
		_FMemory_memcpy(vp->itsImmediateSpace,CpxArray(tval),min(vp->itsMaxItemIndex*2*(NUM)sizeof(REAL),size));
		FMemory_Free(gCP,gTP,(HMemory)vp->itsCpxArray);
		}
	else
		{
		_FMemory_memset(vp->itsImmediateSpace,0,newSize*2*sizeof(REAL));
		}
	vp->itsCpxArray = (HMReal)&vp->itsImmediatePtr;
	vp->itsImmediatePtr = (CHAR*)&vp->itsImmediateSpace[0];
    vp->itsMaxItemIndex = newSize;
	FrameExit(gCP->TObject_OK);
	}
else
/*  Either create or resize the existing header */
vp = tval.u.CpxVector;
if (vp->itsCpxArray == NULL)
	{
	vp->itsCpxArray = (HMReal)FMemory_New(gCP, gTP, size,TRUE);
	vp->itsImmediatePtr = NULL;
	vp->itsMaxItemIndex = newSize;
	}
else
/* Existing data is in immediate data space. */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if ((vp->itsImmediatePtr != NULL) && (newSize != vp->itsMaxItemIndex))
	{
    vp->itsCpxArray = (HMReal)FMemory_New(gCP, gTP, size,TRUE);
	_FMemory_memcpy((char*)CpxArray(tval),vp->itsImmediateSpace,min(vp->itsMaxItemIndex*2*(NUM)sizeof(REAL),size));
	vp->itsImmediatePtr = NULL;
    vp->itsMaxItemIndex = newSize;
	}
else
if(newSize != vp->itsMaxItemIndex)
	{
	vp->itsCpxArray = (HMReal)FMemory_Resize(gCP, gTP,(HMemory)vp->itsCpxArray, size);
	vp->itsImmediatePtr = NULL;
	vp->itsMaxItemIndex = newSize;
	}

/* Initialize any skipped items (if necesssary). */
if (oldMaxItemIndex < newSize)
	{
	ptr = (LpREAL)&CpxArray(tval)[oldMaxItemIndex];
	for (n = oldMaxItemIndex; n < newSize; ++n)
		{
		*(ptr++) = 0.0;
		*(ptr++) = 0.0;
		}
	}
	
FrameExit(gCP->TObject_OK);
}
// The end
