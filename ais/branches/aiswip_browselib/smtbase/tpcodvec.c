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
TPcodeVector.c

Implementation of the Pcode vector class which stores a variable number of
pcodes in a linear array. The manifest typing system together with C++ methods 
control and size the the array.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif

#include "tpcodvec.h"
#include "tsymbol.h"
#include "tlambda.h"
#include "fconio.h"
#include "tdiction.h"

/*--------------------------------------------------------------------------------------- */
#if 0
TPcodeVector_Init

Initialize the TPcodeVector class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TPcodeVector_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
NUM             indexOf;
TVAL            tempValue;

/*  Don't initialize more than once. */
if (gCP->TPcodeVector_Initialized) return;
gCP->TPcodeVector_Initialized    = TRUE;

/*  Initialize the new type for this class. */
FSmartbase_NewType (gCP,
					gTP,
					 TYPCODEVECTOR,
					(LpCHAR)"PcodeVector",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&TPcodeVector_MakeNew,
					&TPcodeVector_Mark,
					&TObject_GlobalMarkNever,
					&FObject_ObjAnyCnv,
					&FObject_CompareNever,
					&TPcodeVector_SetIV1,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&TPcodeVector_GetIV1,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_MapObject,
					&TObject_MapcObject,
					&TPcodeVector_Print,
					&TPcodeVector_Load,
					&TPcodeVector_Save,
					&TPcodeVector_ComputeSize,
					&TPcodeVector_Copy,
					&TPcodeVector_Doomed);

/*  Initialize the argument modifiers for the virtual machine instructions. */
tempValue.Tag = TYNUM;
tempValue.u.Int = AMVOID; TSymbol_SetGlobalValue(gCP,gTP,(TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"amvoid")),tempValue);
tempValue.u.Int = AMSVOFFSET; TSymbol_SetGlobalValue(gCP,gTP,(TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"amsfoffset")),tempValue);
tempValue.u.Int = AMAVOFFSET; TSymbol_SetGlobalValue(gCP,gTP,(TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"amsboffset")),tempValue);
tempValue.u.Int = AMGVOFFSET; TSymbol_SetGlobalValue(gCP,gTP,(TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"amgvoffset")),tempValue);
tempValue.u.Int = AMTVOFFSET; TSymbol_SetGlobalValue(gCP,gTP,(TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"amfboffset")),tempValue);
tempValue.u.Int = AMREGISTER; TSymbol_SetGlobalValue(gCP,gTP,(TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"amregister")),tempValue);
tempValue.u.Int = AMPVOFFSET; TSymbol_SetGlobalValue(gCP,gTP,(TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ampvoffset")),tempValue);
tempValue.u.Int = AMCVOFFSET; TSymbol_SetGlobalValue(gCP,gTP,(TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"amcvoffset")),tempValue);
tempValue.u.Int = AMINTEGER; TSymbol_SetGlobalValue(gCP,gTP,(TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"aminteger")),tempValue);


/*  Initialize the legal modifiers for all virtual machine instructions */
for (indexOf = 0; indexOf < _VMMAXINSTRS; ++indexOf)
    {
    gCP->TPcodeVector_LegalModifiers[indexOf][0] = FAMFAST;
    gCP->TPcodeVector_LegalModifiers[indexOf][1] = FAMFAST;
    gCP->TPcodeVector_LegalModifiers[indexOf][2] = FAMFAST;
    }

gCP->TPcodeVector_LegalModifiers[VMRETURN][0] = FAMALL;
gCP->TPcodeVector_LegalModifiers[VMRETURN][1] = FAMVOID;
gCP->TPcodeVector_LegalModifiers[VMRETURN][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[VMARGCOUNT][0] = FAMFAST ;
gCP->TPcodeVector_LegalModifiers[VMARGCOUNT][1] = FAMVOID;
gCP->TPcodeVector_LegalModifiers[VMARGCOUNT][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[VMARGFETCH][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMARGFETCH][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMARGFETCH][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[VMAPPLY][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[VMAPPLY][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMAPPLY][2] = FAMFAST;

gCP->TPcodeVector_LegalModifiers[VMCALL][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[VMCALL][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMCALL][2] = FAMFAST;

gCP->TPcodeVector_LegalModifiers[VMJMPLT][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMJMPLT][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMJMPLT][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[VMJMPLE][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMJMPLE][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMJMPLE][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[VMJMPEQ][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMJMPEQ][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMJMPEQ][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[VMJMPNE][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMJMPNE][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMJMPNE][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[VMJMPGE][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMJMPGE][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMJMPGE][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[VMJMPGT][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMJMPGT][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMJMPGT][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[VMJUMP][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[VMJUMP][1] = FAMVOID;
gCP->TPcodeVector_LegalModifiers[VMJUMP][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[VMMOVE][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMMOVE][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMMOVE][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[VMMOVEI][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMMOVEI][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMMOVEI][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[VMMOVEN][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMMOVEN][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMMOVEN][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[VMDEBUGGER][0] = FAMALL ^ FAMVOID;
gCP->TPcodeVector_LegalModifiers[VMDEBUGGER][1] = FAMVOID;
gCP->TPcodeVector_LegalModifiers[VMDEBUGGER][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[VMONERROR][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMONERROR][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMONERROR][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[VMPOP][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMPOP][1] = FAMFAST | FAMVOID;
gCP->TPcodeVector_LegalModifiers[VMPOP][2] = FAMFAST | FAMVOID;

gCP->TPcodeVector_LegalModifiers[VMPUSH][0] = FAMALL ^ FAMVOID;
gCP->TPcodeVector_LegalModifiers[VMPUSH][1] = FAMALL;
gCP->TPcodeVector_LegalModifiers[VMPUSH][2] = FAMALL;

gCP->TPcodeVector_LegalModifiers[VMSELF][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMSELF][1] = FAMVOID;
gCP->TPcodeVector_LegalModifiers[VMSELF][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[VMSEND][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[VMSEND][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[VMSEND][2] = FAMFAST;

gCP->TPcodeVector_LegalModifiers[VMTESTESCAPE][0] = FAMVOID;
gCP->TPcodeVector_LegalModifiers[VMTESTESCAPE][1] = FAMVOID;
gCP->TPcodeVector_LegalModifiers[VMTESTESCAPE][2] = FAMVOID;

/*  Initialize the legal modifiers for all register virtual machine instructions */
for (indexOf = VMSTARTREGISTERINS; indexOf < _VMMAXINSTRS; ++indexOf)
    {
    gCP->TPcodeVector_LegalModifiers[indexOf][0] = FAMREGISTER;
    gCP->TPcodeVector_LegalModifiers[indexOf][1] = FAMREGISTER;
    gCP->TPcodeVector_LegalModifiers[indexOf][2] = FAMVOID;
    }

gCP->TPcodeVector_LegalModifiers[vmnatJmpLTInteger][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpLTInteger][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpLTInteger][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmnatJmpLEInteger][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpLEInteger][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpLEInteger][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmnatJmpEQInteger][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpEQInteger][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpEQInteger][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmnatJmpNEInteger][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpNEInteger][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpNEInteger][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmnatJmpGEInteger][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpGEInteger][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpGEInteger][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmnatJmpGTInteger][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpGTInteger][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpGTInteger][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmnatJmpLTNumber][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpLTNumber][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpLTNumber][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmnatJmpLENumber][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpLENumber][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpLENumber][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmnatJmpEQNumber][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpEQNumber][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpEQNumber][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmnatJmpNENumber][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpNENumber][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpNENumber][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmnatJmpGENumber][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpGENumber][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpGENumber][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmnatJmpGTNumber][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpGTNumber][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatJmpGTNumber][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmnatLoadCharacter][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatLoadCharacter][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatLoadCharacter][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmnatLoadFloat][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatLoadFloat][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatLoadFloat][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmnatLoadInteger][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatLoadInteger][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatLoadInteger][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmnatLoadLong][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatLoadLong][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatLoadLong][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmnatLoadNumber][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatLoadNumber][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatLoadNumber][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmnatLoadObject][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatLoadObject][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatLoadObject][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmnatLoadShort][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatLoadShort][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatLoadShort][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmnatSaveCharacter][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatSaveCharacter][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatSaveCharacter][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmnatSaveFloat][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatSaveFloat][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatSaveFloat][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmnatSaveInteger][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatSaveInteger][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatSaveInteger][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmnatSaveLong][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatSaveLong][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatSaveLong][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmnatSaveNumber][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatSaveNumber][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatSaveNumber][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmnatSaveObject][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatSaveObject][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatSaveObject][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmnatSaveShort][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatSaveShort][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmnatSaveShort][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregMoveImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregMoveImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregMoveImmediate][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregIncPointer][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregIncPointer][1] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregIncPointer][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregAddPointer][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregAddPointer][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregAddPointer][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregSubPointer][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregSubPointer][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSubPointer][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregAbsNumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregAbsNumber][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregAbsNumber][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregNumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregNumber][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregNumber][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregInteger][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregInteger][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregInteger][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregAddImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregAddImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregAddImmediate][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregAndImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregAndImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregAndImmediate][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregCosNumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregCosNumber][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregCosNumber][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregDivImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregDivImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregDivImmediate][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregDivrImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregDivrImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregDivrImmediate][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregJmpEQInteger][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpEQInteger][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpEQInteger][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJmpLTImmediate][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpLTImmediate][1] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregJmpLTImmediate][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJmpGTImmediate][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpGTImmediate][1] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregJmpGTImmediate][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJmpNEImmediate][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpNEImmediate][1] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregJmpNEImmediate][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJmpGEImmediate][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpGEImmediate][1] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregJmpGEImmediate][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJmpLEImmediate][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpLEImmediate][1] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregJmpLEImmediate][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJmpEQImmediate][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpEQImmediate][1] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregJmpEQImmediate][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJmpLTInteger][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpLTInteger][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpLTInteger][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJmpGTInteger][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpGTInteger][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpGTInteger][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJmpNEInteger][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpNEInteger][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpNEInteger][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJmpGEInteger][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpGEInteger][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpGEInteger][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJmpLEInteger][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpLEInteger][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpLEInteger][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJmpEQNumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpEQNumber][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpEQNumber][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJmpLTNumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpLTNumber][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpLTNumber][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJmpGTNumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpGTNumber][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpGTNumber][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJmpNENumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpNENumber][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpNENumber][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJmpGENumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpGENumber][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpGENumber][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJmpLENumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpLENumber][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJmpLENumber][2] = FAMINTEGER;

gCP->TPcodeVector_LegalModifiers[vmregJump][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregJump][1] = FAMVOID;
gCP->TPcodeVector_LegalModifiers[vmregJump][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregLoadAddress][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregLoadAddress][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregLoadAddress][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregLoadInteger][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregLoadInteger][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregLoadInteger][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregLoadTail][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregLoadTail][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregLoadTail][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregLoadDeclType][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregLoadDeclType][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregLoadDeclType][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregLoadType][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregLoadType][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregLoadType][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregLoadJmpPointer][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregLoadJmpPointer][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregLoadJmpPointer][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregLoadNumber][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregLoadNumber][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregLoadNumber][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregLogNumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregLogNumber][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregLogNumber][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregMulImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregMulImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregMulImmediate][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregObjPointer][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregObjPointer][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregObjPointer][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregObjLength][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregObjLength][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregObjLength][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregOrImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregOrImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregOrImmediate][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregPwrNumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregPwrNumber][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregPwrNumber][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregRefWord][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregRefWord][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregRefWord][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregRefXWord][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregRefXWord][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregRefXWord][2] = FAMFAST;

gCP->TPcodeVector_LegalModifiers[vmregRefXCharacter][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregRefXCharacter][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregRefXCharacter][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregRefXFloat][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregRefXFloat][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregRefXFloat][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregRefXInteger][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregRefXInteger][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregRefXInteger][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregRefXLong][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregRefXLong][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregRefXLong][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregRefXNumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregRefXNumber][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregRefXNumber][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregRefXShort][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregRefXShort][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregRefXShort][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregRunInHardware][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregRunInHardware][1] = FAMVOID;
gCP->TPcodeVector_LegalModifiers[vmregRunInHardware][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregSaveInteger][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSaveInteger][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregSaveInteger][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregSaveTail][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSaveTail][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregSaveTail][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregSaveTailImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregSaveTailImmediate][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregSaveTailImmediate][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregSaveDeclType][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSaveDeclType][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregSaveDeclType][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregSaveDeclTypeImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregSaveDeclTypeImmediate][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregSaveDeclTypeImmediate][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregSaveNumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSaveNumber][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregSaveNumber][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregSetCharImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregSetCharImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetCharImmediate][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregSetIntImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregSetIntImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetIntImmediate][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregSetLongImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregSetLongImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetLongImmediate][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregSetShortImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregSetShortImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetShortImmediate][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregSetXCharImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregSetXCharImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetXCharImmediate][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregSetXIntImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregSetXIntImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetXIntImmediate][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregSetXLongImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregSetXLongImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetXLongImmediate][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregSetXShortImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregSetXShortImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetXShortImmediate][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregSetXCharacter][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetXCharacter][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetXCharacter][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregSetXFloat][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetXFloat][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetXFloat][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregSetXInteger][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetXInteger][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetXInteger][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregSetXLong][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetXLong][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetXLong][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregSetXNumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetXNumber][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetXNumber][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregSetXShort][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetXShort][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetXShort][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregSetXWord][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregSetXWord][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetXWord][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregSetWord][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregSetWord][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSetWord][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregShlImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregShlImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregShlImmediate][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregShrImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregShrImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregShrImmediate][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregSinNumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSinNumber][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSinNumber][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregSqrtNumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSqrtNumber][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSqrtNumber][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregStringCompare][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregStringCompare][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregStringCompare][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregStringiCompare][0] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregStringiCompare][1] = FAMFAST;
gCP->TPcodeVector_LegalModifiers[vmregStringiCompare][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmregSubImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregSubImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregSubImmediate][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregTanNumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregTanNumber][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregTanNumber][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmregXorImmediate][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmregXorImmediate][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmregXorImmediate][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmvecBinary][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmvecBinary][1] = FAMVOID;
gCP->TPcodeVector_LegalModifiers[vmvecBinary][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmvecInitialize][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmvecInitialize][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmvecInitialize][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmvecLoop][0] = FAMVOID;
gCP->TPcodeVector_LegalModifiers[vmvecLoop][1] = FAMVOID;
gCP->TPcodeVector_LegalModifiers[vmvecLoop][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmvecNumScalar][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmvecNumScalar][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmvecNumScalar][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmvecNumVector][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmvecNumVector][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmvecNumVector][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmvecPop][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmvecPop][1] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmvecPop][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmvecPopNumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmvecPopNumber][1] = FAMVOID;
gCP->TPcodeVector_LegalModifiers[vmvecPopNumber][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmvecPush][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmvecPush][1] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmvecPush][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmvecPushNumber][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmvecPushNumber][1] = FAMVOID;
gCP->TPcodeVector_LegalModifiers[vmvecPushNumber][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmvecSetIncrements][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmvecSetIncrements][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmvecSetIncrements][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmvecSetPointers][0] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmvecSetPointers][1] = FAMREGISTER;
gCP->TPcodeVector_LegalModifiers[vmvecSetPointers][2] = FAMREGISTER;

gCP->TPcodeVector_LegalModifiers[vmvecSwapCC][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmvecSwapCC][1] = FAMVOID;
gCP->TPcodeVector_LegalModifiers[vmvecSwapCC][2] = FAMVOID;

gCP->TPcodeVector_LegalModifiers[vmvecUnary][0] = FAMINTEGER;
gCP->TPcodeVector_LegalModifiers[vmvecUnary][1] = FAMVOID;
gCP->TPcodeVector_LegalModifiers[vmvecUnary][2] = FAMVOID;

}

/*--------------------------------------------------------------------------------------- */
#if 0
TPcodeVector_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TPcodeVector_Load(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
OPCODE              ExtendedOpcode;     
UNUM                modifier[3];
NUM                 modIndex;
NUM                 vecIndex;
NUM                 anInteger;
StartFrame
DeclareOBJ(TPcodeVector,it);
DeclareOBJ(TObject,anObject);
DeclareTVAL(retTval);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;


/*  If the bResolve switch is zero (0), then we must create a new TProdeVector */
/*  and the TPcodeVector's handle to the Load Vector returning its file id. */
/*  This registration prevents recreating the same object more than once. */
if(!bResolve)
    {
    it = TPcodeVector_New(gCP,gTP);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    /*  Retrieve the proper TObject handle from the file  */
    /*  object id and the file Load Vector. */
    it = (TPcodeVector*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
        /*  Initialize the fixed fields of the TPcodeVector. */
        FObject_SetMaxIndex(gCP,gTP,(TObject*)it,TPcodeVectorOnDiskPtr(aHMemory,0)->itsMaxItemIndex);
        it->itsCurItemIndex = TPcodeVectorOnDiskPtr(aHMemory,0)->itsCurItemIndex;
        it->itsCdr = TObject_LoadTval(gCP,gTP,TPcodeVectorOnDiskPtr(aHMemory,0)->itsCdr);
                
        /*  Initialize the Pcode fields of the TPcodeVector, */
        /*  using the virtual machine instruction modifiers */
        /*  to properly recognize any object references imbedded */
        /*  in the instruction stream. */
		vecIndex = 0;
		while (vecIndex < it->itsCurItemIndex)
			{
			if (vecIndex >= it->itsCurItemIndex)
				{
				anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
				atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
				}
			else
				{
				/*  Separate the opcode into the six modifiers. */
				/*  Separate the opcode into the six modifiers. */
				ExtendedOpcode.Opcode = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
				atHMInt(it->itsInstructionArray,vecIndex++) = ExtendedOpcode.Opcode;
				modifier[0] = ExtendedOpcode.u.Am1;
				modifier[1] = ExtendedOpcode.u.Am2;
				modifier[2] = ExtendedOpcode.u.Am3;

				/*  Memory opcodes: Append to the Pcode Vector all remaining arguments. */
				if (ExtendedOpcode.u.Pcode < VMSTARTREGISTERINS)
					{

					/*  Loop through the modifier patterns for arguments which need */
					/*  to be marked. */
					for (modIndex = 0; modIndex < 3; ++modIndex)
						{
						switch (modifier[modIndex])
							{
							case AMVOID:
								break;
                
							case AMSVOFFSET:
							case AMAVOFFSET:
							case AMTVOFFSET:
							case AMPVOFFSET:
							case AMCVOFFSET:
							case AMREGISTER:
							case AMR07OFFST:
							case AMR08OFFST:
							case AMR09OFFST:
							case AMR10OFFST:
							case AMR11OFFST:
							case AMR12OFFST:
							case AMR13OFFST:
							case AMR14OFFST:
							case AMR15OFFST:
							case AMR16OFFST:
							case AMR17OFFST:
							case AMR18OFFST:
							case AMR19OFFST:
							case AMR20OFFST:
							case AMR21OFFST:
							case AMR22OFFST:
							case AMR23OFFST:
							case AMR24OFFST:
							case AMR25OFFST:
							case AMR26OFFST:
							case AMR27OFFST:
							case AMR28OFFST:
							case AMR29OFFST:
							case AMR30OFFST:
							case AMR31OFFST:
							case AMR32OFFST:
							case AMR33OFFST:
							case AMR34OFFST:
							case AMR35OFFST:
							case AMR36OFFST:
							case AMR37OFFST:
							case AMR38OFFST:
							case AMR39OFFST:
							case AMR40OFFST:
							case AMR41OFFST:
							case AMR42OFFST:
							case AMR43OFFST:
							case AMR44OFFST:
							case AMR45OFFST:
							case AMR46OFFST:
							case AMR47OFFST:
							case AMR48OFFST:
							case AMR49OFFST:
							case AMINTEGER:
								anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
								atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
								break;
                
							case AMGVOFFSET:
								anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
								anObject = (TObject*)TObject_CheckRegistration(gCP,gTP,anInteger);
								atHMInt(it->itsInstructionArray,vecIndex++) = (NUM)anObject;
								break;
                                
							default:
								break;
							}       
						}
					}
				else
				if (ExtendedOpcode.u.Pcode >= VMSTARTREGISTERINS)
					{
					switch (ExtendedOpcode.u.Pcode)
						{
						/* Register opcodes with no arguments. */
						case vmvecLoop:
							break;

						/* Register opcodes with one register argument. */
						case vmregJump:
						case vmvecPopNumber:
						case vmvecPushNumber:
							break;

						/* Register opcodes with two register arguments. */
						case vmregAbsNumber:
						case vmregInteger:
						case vmregNumber:
						case vmregAddInteger:
						case vmregAddNumber:
						case vmregAndInteger:
						case vmregCosNumber:
 						case vmregDivInteger:
						case vmregDivNumber:
						case vmregDivrInteger:
						case vmregDivrNumber:
						case vmregMoveInteger:
						case vmregMoveNumber:
						case vmregMulInteger:
						case vmregMulNumber:
						case vmregOrInteger:
						case vmregRefCharacter:
						case vmregRefFloat:
						case vmregRefInteger:
						case vmregRefNumber:
						case vmregRefShort:
						case vmregRefLong:
						case vmregSetCharacter:
						case vmregSetFloat:
						case vmregSetInteger:
						case vmregSetNumber:
						case vmregSetShort:
						case vmregSinNumber:
						case vmregSqrtNumber:
						case vmregSubInteger:
						case vmregSubNumber:
						case vmregTanNumber:
						case vmregXorInteger:
						case vmregShlInteger:
						case vmregShrInteger:
							break;

						/* Register opcodes with three register arguments. */
						case vmregLogNumber:
						case vmregPwrNumber:
						case vmregRefXCharacter:
						case vmregRefXFloat:
						case vmregRefXInteger:
						case vmregRefXLong:
						case vmregRefXNumber:
						case vmregRefXShort:
						case vmregSetXCharacter:
						case vmregSetXFloat:
						case vmregSetXInteger:
						case vmregSetXLong:
						case vmregSetXNumber:
						case vmregSetXShort:
						case vmvecSetIncrements:
						case vmvecSetPointers:
							break;

						/* Register jmpcc opcodes with two register arguments and a label displacement. */
						case vmregJmpEQInteger:
						case vmregJmpLTInteger:
						case vmregJmpGTInteger:
						case vmregJmpNEInteger:
						case vmregJmpGEInteger:
						case vmregJmpLEInteger:
						case vmregJmpEQNumber:
						case vmregJmpLTNumber:
						case vmregJmpGTNumber:
						case vmregJmpNENumber:
						case vmregJmpGENumber:
						case vmregJmpLENumber:
							/*  Convert the two register modifiers to register displacements. */
							modifier[0] = modifier[0] << 8;
							modifier[1] = modifier[1] << 8;
							if (modifier[2] == AMINTEGER)
								{
								anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
								atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
								}
							break;

						/* Register jmpcc opcodes with one register argument one immediate argument and a label displacement. */
						case vmregJmpEQImmediate:
						case vmregJmpLTImmediate:
						case vmregJmpGTImmediate:
						case vmregJmpNEImmediate:
						case vmregJmpGEImmediate:
						case vmregJmpLEImmediate:
							/*  Convert the two register modifiers to register displacements. */
							modifier[0] = modifier[0] << 8;
							if (modifier[1] == AMINTEGER)
								{
								anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
								atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
								}
							if (modifier[2] == AMINTEGER)
								{
								anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
								atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
								}
							break;

						/* Register opcodes with one immediate and one register argument. */
						case vmregAddImmediate:
						case vmregAndImmediate:
						case vmregDivImmediate:
						case vmregLoadJmpPointer:
						case vmregDivrImmediate:
						case vmregMoveImmediate:
						case vmregMulImmediate:
						case vmregOrImmediate:
						case vmregShlImmediate:
						case vmregShrImmediate:
						case vmregSubImmediate:
						case vmregXorImmediate:
						case vmregSetCharImmediate:
						case vmregSetIntImmediate:
						case vmregSetLongImmediate:
						case vmregSetShortImmediate:
						case vmvecNumVector:
						case vmvecInitialize:
							/*  Convert the register modifier to a register displacement. */
							if (modifier[0] == AMINTEGER)
								{
								anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
								atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
								}
							break;

						/* Register opcodes with one immediate and two register arguments. */
						case vmregAddPointer:
						case vmregSubPointer:
						case vmregSetXCharImmediate:
						case vmregSetXIntImmediate:
						case vmregSetXLongImmediate:
						case vmregSetXShortImmediate:
						case vmvecNumScalar:
							/*  Convert the register modifier to a register displacement. */
							if (modifier[0] == AMINTEGER)
								{
								anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
								atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
								}
							break;

						/* Register opcodes with two immediate and one register arguments. */
						case vmregIncPointer:
							/*  Convert the register modifier to a register displacement. */
							if (modifier[0] == AMINTEGER)
								{
								anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
								atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
								}
							if (modifier[1] == AMINTEGER)
								{
								anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
								atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
								}
							break;

						/* Register opcodes with one memory and one register argument. */
						case vmregSetWord:
						case vmregLoadAddress:
						case vmregLoadInteger:
						case vmregLoadTail:
						case vmregLoadDeclType:
						case vmregLoadType:
						case vmregLoadNumber:
						case vmregObjLength:
						case vmregObjPointer:
							switch(modifier[0])
								{
								case AMSVOFFSET:
								case AMAVOFFSET:
								case AMTVOFFSET:
								case AMPVOFFSET:
								case AMCVOFFSET:
								case AMREGISTER:
								case AMR07OFFST:
								case AMR08OFFST:
								case AMR09OFFST:
								case AMR10OFFST:
								case AMR11OFFST:
								case AMR12OFFST:
								case AMR13OFFST:
								case AMR14OFFST:
								case AMR15OFFST:
								case AMR16OFFST:
								case AMR17OFFST:
								case AMR18OFFST:
								case AMR19OFFST:
								case AMR20OFFST:
								case AMR21OFFST:
								case AMR22OFFST:
								case AMR23OFFST:
								case AMR24OFFST:
								case AMR25OFFST:
								case AMR26OFFST:
								case AMR27OFFST:
								case AMR28OFFST:
								case AMR29OFFST:
								case AMR30OFFST:
								case AMR31OFFST:
								case AMR32OFFST:
								case AMR33OFFST:
								case AMR34OFFST:
								case AMR35OFFST:
								case AMR36OFFST:
								case AMR37OFFST:
								case AMR38OFFST:
								case AMR39OFFST:
								case AMR40OFFST:
								case AMR41OFFST:
								case AMR42OFFST:
								case AMR43OFFST:
								case AMR44OFFST:
								case AMR45OFFST:
								case AMR46OFFST:
								case AMR47OFFST:
								case AMR48OFFST:
								case AMR49OFFST:
								case AMINTEGER:
									anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
									atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
									break;

								case AMGVOFFSET:
									anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
									anObject = (TObject*)TObject_CheckRegistration(gCP,gTP,anInteger);
									atHMInt(it->itsInstructionArray,vecIndex++) = (NUM)anObject;
									break;
								}
							break; 

						/* Register opcodes with one immediate and one memory argument. */
						case vmregSaveTailImmediate:
						case vmregSaveDeclTypeImmediate:
							if (modifier[0] == AMINTEGER)
								{
								anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
								atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
								}
							switch(modifier[1])
								{
								case AMSVOFFSET:
								case AMAVOFFSET:
								case AMTVOFFSET:
								case AMPVOFFSET:
								case AMCVOFFSET:
								case AMREGISTER:
								case AMR07OFFST:
								case AMR08OFFST:
								case AMR09OFFST:
								case AMR10OFFST:
								case AMR11OFFST:
								case AMR12OFFST:
								case AMR13OFFST:
								case AMR14OFFST:
								case AMR15OFFST:
								case AMR16OFFST:
								case AMR17OFFST:
								case AMR18OFFST:
								case AMR19OFFST:
								case AMR20OFFST:
								case AMR21OFFST:
								case AMR22OFFST:
								case AMR23OFFST:
								case AMR24OFFST:
								case AMR25OFFST:
								case AMR26OFFST:
								case AMR27OFFST:
								case AMR28OFFST:
								case AMR29OFFST:
								case AMR30OFFST:
								case AMR31OFFST:
								case AMR32OFFST:
								case AMR33OFFST:
								case AMR34OFFST:
								case AMR35OFFST:
								case AMR36OFFST:
								case AMR37OFFST:
								case AMR38OFFST:
								case AMR39OFFST:
								case AMR40OFFST:
								case AMR41OFFST:
								case AMR42OFFST:
								case AMR43OFFST:
								case AMR44OFFST:
								case AMR45OFFST:
								case AMR46OFFST:
								case AMR47OFFST:
								case AMR48OFFST:
								case AMR49OFFST:
								case AMINTEGER:
									anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
									atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
									break;

								case AMGVOFFSET:
									anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
									anObject = (TObject*)TObject_CheckRegistration(gCP,gTP,anInteger);
									atHMInt(it->itsInstructionArray,vecIndex++) = (NUM)anObject;
									break;
								}
							break;

				/* Register opcodes with two register and one memory argument. */
				case vmregRefXWord:
							switch(modifier[2])
								{
								case AMSVOFFSET:
								case AMAVOFFSET:
								case AMTVOFFSET:
								case AMPVOFFSET:
								case AMCVOFFSET:
								case AMREGISTER:
								case AMR07OFFST:
								case AMR08OFFST:
								case AMR09OFFST:
								case AMR10OFFST:
								case AMR11OFFST:
								case AMR12OFFST:
								case AMR13OFFST:
								case AMR14OFFST:
								case AMR15OFFST:
								case AMR16OFFST:
								case AMR17OFFST:
								case AMR18OFFST:
								case AMR19OFFST:
								case AMR20OFFST:
								case AMR21OFFST:
								case AMR22OFFST:
								case AMR23OFFST:
								case AMR24OFFST:
								case AMR25OFFST:
								case AMR26OFFST:
								case AMR27OFFST:
								case AMR28OFFST:
								case AMR29OFFST:
								case AMR30OFFST:
								case AMR31OFFST:
								case AMR32OFFST:
								case AMR33OFFST:
								case AMR34OFFST:
								case AMR35OFFST:
								case AMR36OFFST:
								case AMR37OFFST:
								case AMR38OFFST:
								case AMR39OFFST:
								case AMR40OFFST:
								case AMR41OFFST:
								case AMR42OFFST:
								case AMR43OFFST:
								case AMR44OFFST:
								case AMR45OFFST:
								case AMR46OFFST:
								case AMR47OFFST:
								case AMR48OFFST:
								case AMR49OFFST:
								case AMINTEGER:
									anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
									atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
									break;

								case AMGVOFFSET:
									anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
									anObject = (TObject*)TObject_CheckRegistration(gCP,gTP,anInteger);
									atHMInt(it->itsInstructionArray,vecIndex++) = (NUM)anObject;
									break;
								}
				break;
				/* Register opcodes with one memory and two register arguments. */
				case vmregSetXWord:
							switch(modifier[0])
								{
								case AMSVOFFSET:
								case AMAVOFFSET:
								case AMTVOFFSET:
								case AMPVOFFSET:
								case AMCVOFFSET:
								case AMREGISTER:
								case AMR07OFFST:
								case AMR08OFFST:
								case AMR09OFFST:
								case AMR10OFFST:
								case AMR11OFFST:
								case AMR12OFFST:
								case AMR13OFFST:
								case AMR14OFFST:
								case AMR15OFFST:
								case AMR16OFFST:
								case AMR17OFFST:
								case AMR18OFFST:
								case AMR19OFFST:
								case AMR20OFFST:
								case AMR21OFFST:
								case AMR22OFFST:
								case AMR23OFFST:
								case AMR24OFFST:
								case AMR25OFFST:
								case AMR26OFFST:
								case AMR27OFFST:
								case AMR28OFFST:
								case AMR29OFFST:
								case AMR30OFFST:
								case AMR31OFFST:
								case AMR32OFFST:
								case AMR33OFFST:
								case AMR34OFFST:
								case AMR35OFFST:
								case AMR36OFFST:
								case AMR37OFFST:
								case AMR38OFFST:
								case AMR39OFFST:
								case AMR40OFFST:
								case AMR41OFFST:
								case AMR42OFFST:
								case AMR43OFFST:
								case AMR44OFFST:
								case AMR45OFFST:
								case AMR46OFFST:
								case AMR47OFFST:
								case AMR48OFFST:
								case AMR49OFFST:
								case AMINTEGER:
									anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
									atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
									break;

								case AMGVOFFSET:
									anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
									anObject = (TObject*)TObject_CheckRegistration(gCP,gTP,anInteger);
									atHMInt(it->itsInstructionArray,vecIndex++) = (NUM)anObject;
									break;
								}
				break;

				/* Register opcodes with one register and one memory argument. */
						case vmregRefWord:
						case vmregSaveInteger:
						case vmregSaveTail:
						case vmregSaveDeclType:
						case vmregSaveNumber:
							switch(modifier[1])
								{
								case AMSVOFFSET:
								case AMAVOFFSET:
								case AMTVOFFSET:
								case AMPVOFFSET:
								case AMCVOFFSET:
								case AMREGISTER:
								case AMR07OFFST:
								case AMR08OFFST:
								case AMR09OFFST:
								case AMR10OFFST:
								case AMR11OFFST:
								case AMR12OFFST:
								case AMR13OFFST:
								case AMR14OFFST:
								case AMR15OFFST:
								case AMR16OFFST:
								case AMR17OFFST:
								case AMR18OFFST:
								case AMR19OFFST:
								case AMR20OFFST:
								case AMR21OFFST:
								case AMR22OFFST:
								case AMR23OFFST:
								case AMR24OFFST:
								case AMR25OFFST:
								case AMR26OFFST:
								case AMR27OFFST:
								case AMR28OFFST:
								case AMR29OFFST:
								case AMR30OFFST:
								case AMR31OFFST:
								case AMR32OFFST:
								case AMR33OFFST:
								case AMR34OFFST:
								case AMR35OFFST:
								case AMR36OFFST:
								case AMR37OFFST:
								case AMR38OFFST:
								case AMR39OFFST:
								case AMR40OFFST:
								case AMR41OFFST:
								case AMR42OFFST:
								case AMR43OFFST:
								case AMR44OFFST:
								case AMR45OFFST:
								case AMR46OFFST:
								case AMR47OFFST:
								case AMR48OFFST:
								case AMR49OFFST:
								case AMINTEGER:
									anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
									atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
									break;

								case AMGVOFFSET:
									anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
									anObject = (TObject*)TObject_CheckRegistration(gCP,gTP,anInteger);
									atHMInt(it->itsInstructionArray,vecIndex++) = (NUM)anObject;
									break;
								}
							break;

						/* Register opcodes with one immediate argument. */
						case vmregRunInHardware:
						case vmvecBinary:
						case vmvecSwapCC:
						case vmvecUnary:
							/*  Load the immediate value. */
							if (modifier[0] == AMINTEGER)
								{
								anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
								atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
								}
							break;

						/* Register opcodes with two immediate arguments. */
						case vmvecPop:
						case vmvecPush:
							/*  Load the immediate value. */
							if (modifier[0] == AMINTEGER)
								{
								anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
								atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
								}
							/*  Load the immediate value. */
							if (modifier[1] == AMINTEGER)
								{
								anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
								atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
								}
							break;

						/* Register opcodes with two memory and one register argument. */
						case vmregStringCompare:
						case vmregStringiCompare:
							switch(modifier[0])
								{
								case AMSVOFFSET:
								case AMAVOFFSET:
								case AMTVOFFSET:
								case AMPVOFFSET:
								case AMCVOFFSET:
								case AMREGISTER:
								case AMR07OFFST:
								case AMR08OFFST:
								case AMR09OFFST:
								case AMR10OFFST:
								case AMR11OFFST:
								case AMR12OFFST:
								case AMR13OFFST:
								case AMR14OFFST:
								case AMR15OFFST:
								case AMR16OFFST:
								case AMR17OFFST:
								case AMR18OFFST:
								case AMR19OFFST:
								case AMR20OFFST:
								case AMR21OFFST:
								case AMR22OFFST:
								case AMR23OFFST:
								case AMR24OFFST:
								case AMR25OFFST:
								case AMR26OFFST:
								case AMR27OFFST:
								case AMR28OFFST:
								case AMR29OFFST:
								case AMR30OFFST:
								case AMR31OFFST:
								case AMR32OFFST:
								case AMR33OFFST:
								case AMR34OFFST:
								case AMR35OFFST:
								case AMR36OFFST:
								case AMR37OFFST:
								case AMR38OFFST:
								case AMR39OFFST:
								case AMR40OFFST:
								case AMR41OFFST:
								case AMR42OFFST:
								case AMR43OFFST:
								case AMR44OFFST:
								case AMR45OFFST:
								case AMR46OFFST:
								case AMR47OFFST:
								case AMR48OFFST:
								case AMR49OFFST:
								case AMINTEGER:
									anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
									atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
									break;

								case AMGVOFFSET:
									anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
									anObject = (TObject*)TObject_CheckRegistration(gCP,gTP,anInteger);
									atHMInt(it->itsInstructionArray,vecIndex++) = (NUM)anObject;
									break;
								}
							switch(modifier[1])
								{
								case AMSVOFFSET:
								case AMAVOFFSET:
								case AMTVOFFSET:
								case AMPVOFFSET:
								case AMCVOFFSET:
								case AMREGISTER:
								case AMR07OFFST:
								case AMR08OFFST:
								case AMR09OFFST:
								case AMR10OFFST:
								case AMR11OFFST:
								case AMR12OFFST:
								case AMR13OFFST:
								case AMR14OFFST:
								case AMR15OFFST:
								case AMR16OFFST:
								case AMR17OFFST:
								case AMR18OFFST:
								case AMR19OFFST:
								case AMR20OFFST:
								case AMR21OFFST:
								case AMR22OFFST:
								case AMR23OFFST:
								case AMR24OFFST:
								case AMR25OFFST:
								case AMR26OFFST:
								case AMR27OFFST:
								case AMR28OFFST:
								case AMR29OFFST:
								case AMR30OFFST:
								case AMR31OFFST:
								case AMR32OFFST:
								case AMR33OFFST:
								case AMR34OFFST:
								case AMR35OFFST:
								case AMR36OFFST:
								case AMR37OFFST:
								case AMR38OFFST:
								case AMR39OFFST:
								case AMR40OFFST:
								case AMR41OFFST:
								case AMR42OFFST:
								case AMR43OFFST:
								case AMR44OFFST:
								case AMR45OFFST:
								case AMR46OFFST:
								case AMR47OFFST:
								case AMR48OFFST:
								case AMR49OFFST:
								case AMINTEGER:
									anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
									atHMInt(it->itsInstructionArray,vecIndex++) = anInteger;
									break;

								case AMGVOFFSET:
									anInteger = TPcodeVectorOnDiskPtr(aHMemory,0)->itsItemArray[vecIndex];
									anObject = (TObject*)TObject_CheckRegistration(gCP,gTP,anInteger);
									atHMInt(it->itsInstructionArray,vecIndex++) = (NUM)anObject;
									break;
								}
							break; 
						}
					
					}
				}
			}
        
        /*  Return the proper TPcodeVector value that we have initialized. */
        asTag(retTval) = it->itsObjectType;
        asObject(retTval) = (TObject*)it;
        }
    else
        *retTval = gCP->TObject_ERROR_INVALID;
    }

FrameExit(*retTval);
}


/*--------------------------------------------------------------------------------------- */
#if 0
IPcodeVector

Initialize a TPcodeVector object with a new tail(cdr) and an array of items.

#endif

void    TPcodeVector_IPcodeVector(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[],TVAL newCdr)
{
NUM             indexOf;
TPcodeVector*   self = (TPcodeVector*)asObject(&selfTval);

/*  Reshape the bit vectors's array to be the correct size. */
TPcodeVector_SetMaxIndex(gCP, gTP, selfTval, argc);

/*  Set the bit vector's array items. */
for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
    {
    if (asTag(&argv[indexOf]) == TYNUM)
        {
        atHMInt(self->itsInstructionArray,indexOf) = asInt(&argv[indexOf]);
        }
    else
    if (asTag(&argv[indexOf]) == TYREAL)
        {
        atHMInt(self->itsInstructionArray,indexOf) = asReal(&argv[indexOf]);
        }
    else
    if (asTag(&argv[indexOf]) == TYBOLE)
        {
        atHMInt(self->itsInstructionArray,indexOf) = asBool(&argv[indexOf]);
        }
    else
        {
        atHMInt(self->itsInstructionArray,indexOf) = 0;
        }
    }

/*  Set the Pcode vector's tail(cdr). */
self->itsCdr = newCdr;
}

/*--------------------------------------------------------------------------------------- */
#if 0
Mark

Garbage collection is marking this object. Mark any objects which you reference.

Note:   This method should only be called by mark and sweep garbage collection!
        Do send mark messages to any referenced objects,

#endif

void    TPcodeVector_Mark(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
OPCODE              ExtendedOpcode;
UNUM				opcode;     
UNUM                modifier[3];
NUM                 modIndex;
NUM                 vecIndex;
StartFrame
DeclareOBJ(TPcodeVector,self);
DeclareOBJ(TObject,anObject);
EndFrame

self = (TPcodeVector*)asObject(&selfTval);

/*  Mark the Pcode vector's Lisp tail(cdr) so it won't be garbage collected. */

TObject_MarkTval(gCP,gTP,self->itsCdr);

/*  Examine all pcodes in the Pcode Vector looking at the argument modifiers */
/*  for any Symbol, Object, or Tval arguments which must be marked. */
vecIndex = 0;
while (vecIndex < self->itsCurItemIndex)
    {
    /*  Separate the opcode into the six modifiers. */
    ExtendedOpcode.Opcode = atHMInt(self->itsInstructionArray,vecIndex++);
	opcode = ExtendedOpcode.u.Pcode;
    modifier[0] = ExtendedOpcode.u.Am1;
    modifier[1] = ExtendedOpcode.u.Am2;
    modifier[2] = ExtendedOpcode.u.Am3;

	/*  Memory opcodes: Append to the Pcode Vector all remaining arguments. */
	if (opcode < VMSTARTREGISTERINS)
		{

		/*  Loop through the modifier patterns for arguments which need */
		/*  to be marked. */
		for (modIndex = 0; modIndex < 3; ++modIndex)
			{
			switch (modifier[modIndex])
				{
				case AMVOID:
					break;
                
				case AMSVOFFSET:
				case AMAVOFFSET:
				case AMTVOFFSET:
				case AMPVOFFSET:
				case AMCVOFFSET:
				case AMREGISTER:
				case AMR07OFFST:
				case AMR08OFFST:
				case AMR09OFFST:
				case AMR10OFFST:
				case AMR11OFFST:
				case AMR12OFFST:
				case AMR13OFFST:
				case AMR14OFFST:
				case AMR15OFFST:
				case AMR16OFFST:
				case AMR17OFFST:
				case AMR18OFFST:
				case AMR19OFFST:
				case AMR20OFFST:
				case AMR21OFFST:
				case AMR22OFFST:
				case AMR23OFFST:
				case AMR24OFFST:
				case AMR25OFFST:
				case AMR26OFFST:
				case AMR27OFFST:
				case AMR28OFFST:
				case AMR29OFFST:
				case AMR30OFFST:
				case AMR31OFFST:
				case AMR32OFFST:
				case AMR33OFFST:
				case AMR34OFFST:
				case AMR35OFFST:
				case AMR36OFFST:
				case AMR37OFFST:
				case AMR38OFFST:
				case AMR39OFFST:
				case AMR40OFFST:
				case AMR41OFFST:
				case AMR42OFFST:
				case AMR43OFFST:
				case AMR44OFFST:
				case AMR45OFFST:
				case AMR46OFFST:
				case AMR47OFFST:
				case AMR48OFFST:
				case AMR49OFFST:
				case AMINTEGER:
					vecIndex++;
					break;
                
				case AMGVOFFSET:
					anObject = *(TObject**)&atHMInt(self->itsInstructionArray,vecIndex++);
					TObject_MarkObj(gCP,gTP,anObject);
					break;
                                
				default:
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					break;
				}       
			}
		}
	else
	if (opcode >= VMSTARTREGISTERINS)
		{
		switch (opcode)
			{
			/* Register opcodes with no arguments. */
			case vmvecLoop:
				if (modifier[0] != AMVOID)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				if (modifier[1] != AMVOID)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				if (modifier[2] != AMVOID)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				break;

			/* Register opcodes with one register argument. */
			case vmregJump:
			case vmvecPopNumber:
			case vmvecPushNumber:
				if (modifier[0] >= AMINTEGER)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				if (modifier[1] != AMVOID)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				if (modifier[2] != AMVOID)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				break;

			/* Register opcodes with two register arguments. */
			case vmregAbsNumber:
			case vmregInteger:
			case vmregNumber:
			case vmregAddInteger:
			case vmregAddNumber:
			case vmregAndInteger:
			case vmregCosNumber:
 			case vmregDivInteger:
			case vmregDivNumber:
			case vmregDivrInteger:
			case vmregDivrNumber:
			case vmregMoveInteger:
			case vmregMoveNumber:
			case vmregMulInteger:
			case vmregMulNumber:
			case vmregOrInteger:
			case vmregRefCharacter:
			case vmregRefFloat:
			case vmregRefInteger:
			case vmregRefNumber:
			case vmregRefShort:
			case vmregRefLong:
			case vmregSetCharacter:
			case vmregSetFloat:
			case vmregSetInteger:
			case vmregSetNumber:
			case vmregSetShort:
			case vmregSetLong:
			case vmregSinNumber:
			case vmregSqrtNumber:
			case vmregSubInteger:
			case vmregSubNumber:
			case vmregTanNumber:
			case vmregXorInteger:
			case vmregShlInteger:
			case vmregShrInteger:
				if (modifier[0] >= AMINTEGER)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				if (modifier[1] >= AMINTEGER)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				if (modifier[2] != AMVOID)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				break;

			/* Register opcodes with three register arguments. */
			case vmregLogNumber:
			case vmregPwrNumber:
			case vmregRefXCharacter:
			case vmregRefXFloat:
			case vmregRefXInteger:
			case vmregRefXLong:
			case vmregRefXNumber:
			case vmregRefXShort:
			case vmregSetXCharacter:
			case vmregSetXFloat:
			case vmregSetXInteger:
			case vmregSetXLong:
			case vmregSetXNumber:
			case vmregSetXShort:
			case vmvecSetIncrements:
			case vmvecSetPointers:
				if (modifier[0] >= AMINTEGER)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				if (modifier[1] >= AMINTEGER)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				if (modifier[2] >= AMINTEGER)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				break;

			/* Register jmpcc opcodes with two register arguments and a label displacement. */
			case vmregJmpEQInteger:
			case vmregJmpLTInteger:
			case vmregJmpGTInteger:
			case vmregJmpNEInteger:
			case vmregJmpGEInteger:
			case vmregJmpLEInteger:
			case vmregJmpEQNumber:
			case vmregJmpLTNumber:
			case vmregJmpGTNumber:
			case vmregJmpNENumber:
			case vmregJmpGENumber:
			case vmregJmpLENumber:
				if (modifier[0] >= AMINTEGER)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				if (modifier[1] >= AMINTEGER)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				if (modifier[2] != AMINTEGER) 
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				vecIndex++;
				break;

			/* Register jmpcc opcodes with one register argument one immediate argument and a label displacement. */
			case vmregJmpEQImmediate:
			case vmregJmpLTImmediate:
			case vmregJmpGTImmediate:
			case vmregJmpNEImmediate:
			case vmregJmpGEImmediate:
			case vmregJmpLEImmediate:
				if (modifier[0] >= AMINTEGER)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				if (modifier[1] != AMINTEGER) 
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				vecIndex++;
				if (modifier[2] != AMINTEGER) 
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				vecIndex++;
				break;

			/* Register opcodes with one immediate and one register argument. */
			case vmregAddImmediate:
			case vmregAndImmediate:
			case vmregDivImmediate:
			case vmregLoadJmpPointer:
			case vmregDivrImmediate:
			case vmregMoveImmediate:
			case vmregMulImmediate:
			case vmregOrImmediate:
			case vmregShlImmediate:
			case vmregShrImmediate:
			case vmregSubImmediate:
			case vmregXorImmediate:
			case vmregSetCharImmediate:
			case vmregSetIntImmediate:
			case vmregSetLongImmediate:
			case vmregSetShortImmediate:
			case vmvecNumVector:
			case vmvecInitialize:
				if (modifier[0] != AMINTEGER) 
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				vecIndex++;
				if (modifier[1] >= AMINTEGER)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				if (modifier[2] != AMVOID)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				break;

			/* Register opcodes with one immediate and two register argument. */
			case vmregAddPointer:
			case vmregSubPointer:
			case vmregSetXCharImmediate:
			case vmregSetXIntImmediate:
			case vmregSetXLongImmediate:
			case vmregSetXShortImmediate:
			case vmvecNumScalar:
				if (modifier[0] != AMINTEGER) 
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				vecIndex++;
				if (modifier[1] >= AMINTEGER)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				if (modifier[2] >= AMINTEGER)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				break;

			/* Register opcodes with two immediate and one register argument. */
			case vmregIncPointer:
				if (modifier[0] != AMINTEGER) 
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				vecIndex++;
				if (modifier[1] != AMINTEGER) 
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				vecIndex++;
				if (modifier[2] >= AMINTEGER)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				break;

			/* Register opcodes with one memory and one register argument. */
			case vmregSetWord:
			case vmregLoadAddress:
			case vmregLoadInteger:
			case vmregLoadTail:
			case vmregLoadDeclType:
			case vmregLoadType:
			case vmregLoadNumber:
			case vmregObjLength:
			case vmregObjPointer:
				switch(modifier[0])
					{
					case AMSVOFFSET:
					case AMAVOFFSET:
					case AMTVOFFSET:
					case AMPVOFFSET:
					case AMCVOFFSET:
					case AMREGISTER:
					case AMR07OFFST:
					case AMR08OFFST:
					case AMR09OFFST:
					case AMR10OFFST:
					case AMR11OFFST:
					case AMR12OFFST:
					case AMR13OFFST:
					case AMR14OFFST:
					case AMR15OFFST:
					case AMR16OFFST:
					case AMR17OFFST:
					case AMR18OFFST:
					case AMR19OFFST:
					case AMR20OFFST:
					case AMR21OFFST:
					case AMR22OFFST:
					case AMR23OFFST:
					case AMR24OFFST:
					case AMR25OFFST:
					case AMR26OFFST:
					case AMR27OFFST:
					case AMR28OFFST:
					case AMR29OFFST:
					case AMR30OFFST:
					case AMR31OFFST:
					case AMR32OFFST:
					case AMR33OFFST:
					case AMR34OFFST:
					case AMR35OFFST:
					case AMR36OFFST:
					case AMR37OFFST:
					case AMR38OFFST:
					case AMR39OFFST:
					case AMR40OFFST:
					case AMR41OFFST:
					case AMR42OFFST:
					case AMR43OFFST:
					case AMR44OFFST:
					case AMR45OFFST:
					case AMR46OFFST:
					case AMR47OFFST:
					case AMR48OFFST:
					case AMR49OFFST:
					case AMINTEGER:
						++vecIndex;
						break;

					case AMGVOFFSET:
						anObject = *(TObject**)&atHMInt(self->itsInstructionArray,vecIndex++);
						TObject_MarkObj(gCP,gTP,anObject);
						break;

					default:
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
						break;
					}
				if (modifier[1] >= AMINTEGER)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				if (modifier[2] != AMVOID)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				break; 

				/* Mark opcodes with one immediate argument. */
				case vmregRunInHardware:
				case vmvecBinary:
				case vmvecSwapCC:
				case vmvecUnary:
					if (modifier[0] != AMINTEGER) 
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					++vecIndex;
					if (modifier[1] != AMVOID) 
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					if (modifier[2] != AMVOID) 
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					break;

				/* Mark opcodes with two immediate arguments. */
				case vmvecPop:
				case vmvecPush:
					if (modifier[0] != AMINTEGER) 
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					++vecIndex;
					if (modifier[1] != AMINTEGER) 
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					++vecIndex;
					if (modifier[2] != AMVOID) 
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					break;

			/* Register opcodes with one immediate and one memory argument. */
			case vmregSaveTailImmediate:
			case vmregSaveDeclTypeImmediate:
				if (modifier[0] != AMINTEGER)
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE); 
				vecIndex++;
				switch(modifier[1])
					{
					case AMSVOFFSET:
					case AMAVOFFSET:
					case AMTVOFFSET:
					case AMPVOFFSET:
					case AMCVOFFSET:
					case AMREGISTER:
					case AMR07OFFST:
					case AMR08OFFST:
					case AMR09OFFST:
					case AMR10OFFST:
					case AMR11OFFST:
					case AMR12OFFST:
					case AMR13OFFST:
					case AMR14OFFST:
					case AMR15OFFST:
					case AMR16OFFST:
					case AMR17OFFST:
					case AMR18OFFST:
					case AMR19OFFST:
					case AMR20OFFST:
					case AMR21OFFST:
					case AMR22OFFST:
					case AMR23OFFST:
					case AMR24OFFST:
					case AMR25OFFST:
					case AMR26OFFST:
					case AMR27OFFST:
					case AMR28OFFST:
					case AMR29OFFST:
					case AMR30OFFST:
					case AMR31OFFST:
					case AMR32OFFST:
					case AMR33OFFST:
					case AMR34OFFST:
					case AMR35OFFST:
					case AMR36OFFST:
					case AMR37OFFST:
					case AMR38OFFST:
					case AMR39OFFST:
					case AMR40OFFST:
					case AMR41OFFST:
					case AMR42OFFST:
					case AMR43OFFST:
					case AMR44OFFST:
					case AMR45OFFST:
					case AMR46OFFST:
					case AMR47OFFST:
					case AMR48OFFST:
					case AMR49OFFST:
					case AMINTEGER:
						++vecIndex;
						break;

					case AMGVOFFSET:
						anObject = *(TObject**)&atHMInt(self->itsInstructionArray,vecIndex++);
						TObject_MarkObj(gCP,gTP,anObject);
						break;

					default:
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
						break;
					}
				if (modifier[2] != AMVOID) 
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
				break;

			/* Register opcodes with two register and one memory argument. */
			case vmregRefXWord:
				if (modifier[0] >= AMINTEGER) 
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
				if (modifier[1] >= AMINTEGER) 
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
				switch(modifier[2])
					{
					case AMSVOFFSET:
					case AMAVOFFSET:
					case AMTVOFFSET:
					case AMPVOFFSET:
					case AMCVOFFSET:
					case AMREGISTER:
					case AMR07OFFST:
					case AMR08OFFST:
					case AMR09OFFST:
					case AMR10OFFST:
					case AMR11OFFST:
					case AMR12OFFST:
					case AMR13OFFST:
					case AMR14OFFST:
					case AMR15OFFST:
					case AMR16OFFST:
					case AMR17OFFST:
					case AMR18OFFST:
					case AMR19OFFST:
					case AMR20OFFST:
					case AMR21OFFST:
					case AMR22OFFST:
					case AMR23OFFST:
					case AMR24OFFST:
					case AMR25OFFST:
					case AMR26OFFST:
					case AMR27OFFST:
					case AMR28OFFST:
					case AMR29OFFST:
					case AMR30OFFST:
					case AMR31OFFST:
					case AMR32OFFST:
					case AMR33OFFST:
					case AMR34OFFST:
					case AMR35OFFST:
					case AMR36OFFST:
					case AMR37OFFST:
					case AMR38OFFST:
					case AMR39OFFST:
					case AMR40OFFST:
					case AMR41OFFST:
					case AMR42OFFST:
					case AMR43OFFST:
					case AMR44OFFST:
					case AMR45OFFST:
					case AMR46OFFST:
					case AMR47OFFST:
					case AMR48OFFST:
					case AMR49OFFST:
					case AMINTEGER:
						++vecIndex;
						break;

					case AMGVOFFSET:
						anObject = *(TObject**)&atHMInt(self->itsInstructionArray,vecIndex++);
						TObject_MarkObj(gCP,gTP,anObject);
						break;
					}
				break;
			/* Register opcodes with one memory and two register arguments. */
			case vmregSetXWord:
				switch(modifier[0])
					{
					case AMSVOFFSET:
					case AMAVOFFSET:
					case AMTVOFFSET:
					case AMPVOFFSET:
					case AMCVOFFSET:
					case AMREGISTER:
					case AMR07OFFST:
					case AMR08OFFST:
					case AMR09OFFST:
					case AMR10OFFST:
					case AMR11OFFST:
					case AMR12OFFST:
					case AMR13OFFST:
					case AMR14OFFST:
					case AMR15OFFST:
					case AMR16OFFST:
					case AMR17OFFST:
					case AMR18OFFST:
					case AMR19OFFST:
					case AMR20OFFST:
					case AMR21OFFST:
					case AMR22OFFST:
					case AMR23OFFST:
					case AMR24OFFST:
					case AMR25OFFST:
					case AMR26OFFST:
					case AMR27OFFST:
					case AMR28OFFST:
					case AMR29OFFST:
					case AMR30OFFST:
					case AMR31OFFST:
					case AMR32OFFST:
					case AMR33OFFST:
					case AMR34OFFST:
					case AMR35OFFST:
					case AMR36OFFST:
					case AMR37OFFST:
					case AMR38OFFST:
					case AMR39OFFST:
					case AMR40OFFST:
					case AMR41OFFST:
					case AMR42OFFST:
					case AMR43OFFST:
					case AMR44OFFST:
					case AMR45OFFST:
					case AMR46OFFST:
					case AMR47OFFST:
					case AMR48OFFST:
					case AMR49OFFST:
					case AMINTEGER:
						++vecIndex;
						break;

					case AMGVOFFSET:
						anObject = *(TObject**)&atHMInt(self->itsInstructionArray,vecIndex++);
						TObject_MarkObj(gCP,gTP,anObject);
						break;
					}
				if (modifier[1] >= AMINTEGER) 
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
				if (modifier[2] >= AMINTEGER) 
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
				break;

			/* Register opcodes with one register and one memory argument. */
			case vmregRefWord:
			case vmregSaveInteger:
			case vmregSaveTail:
			case vmregSaveDeclType:
			case vmregSaveNumber:
				if (modifier[0] >= AMINTEGER) 
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
				switch(modifier[1])
					{
					case AMSVOFFSET:
					case AMAVOFFSET:
					case AMTVOFFSET:
					case AMPVOFFSET:
					case AMCVOFFSET:
					case AMREGISTER:
					case AMR07OFFST:
					case AMR08OFFST:
					case AMR09OFFST:
					case AMR10OFFST:
					case AMR11OFFST:
					case AMR12OFFST:
					case AMR13OFFST:
					case AMR14OFFST:
					case AMR15OFFST:
					case AMR16OFFST:
					case AMR17OFFST:
					case AMR18OFFST:
					case AMR19OFFST:
					case AMR20OFFST:
					case AMR21OFFST:
					case AMR22OFFST:
					case AMR23OFFST:
					case AMR24OFFST:
					case AMR25OFFST:
					case AMR26OFFST:
					case AMR27OFFST:
					case AMR28OFFST:
					case AMR29OFFST:
					case AMR30OFFST:
					case AMR31OFFST:
					case AMR32OFFST:
					case AMR33OFFST:
					case AMR34OFFST:
					case AMR35OFFST:
					case AMR36OFFST:
					case AMR37OFFST:
					case AMR38OFFST:
					case AMR39OFFST:
					case AMR40OFFST:
					case AMR41OFFST:
					case AMR42OFFST:
					case AMR43OFFST:
					case AMR44OFFST:
					case AMR45OFFST:
					case AMR46OFFST:
					case AMR47OFFST:
					case AMR48OFFST:
					case AMR49OFFST:
					case AMINTEGER:
						++vecIndex;
						break;

					case AMGVOFFSET:
						anObject = *(TObject**)&atHMInt(self->itsInstructionArray,vecIndex++);
						TObject_MarkObj(gCP,gTP,anObject);
						break;
					}
				if (modifier[2] != AMVOID) 
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
				break;

			/* Register opcodes with two memory and one register argument. */
			case vmregStringCompare:
			case vmregStringiCompare:
				switch(modifier[0])
					{
					case AMSVOFFSET:
					case AMAVOFFSET:
					case AMTVOFFSET:
					case AMPVOFFSET:
					case AMCVOFFSET:
					case AMREGISTER:
					case AMR07OFFST:
					case AMR08OFFST:
					case AMR09OFFST:
					case AMR10OFFST:
					case AMR11OFFST:
					case AMR12OFFST:
					case AMR13OFFST:
					case AMR14OFFST:
					case AMR15OFFST:
					case AMR16OFFST:
					case AMR17OFFST:
					case AMR18OFFST:
					case AMR19OFFST:
					case AMR20OFFST:
					case AMR21OFFST:
					case AMR22OFFST:
					case AMR23OFFST:
					case AMR24OFFST:
					case AMR25OFFST:
					case AMR26OFFST:
					case AMR27OFFST:
					case AMR28OFFST:
					case AMR29OFFST:
					case AMR30OFFST:
					case AMR31OFFST:
					case AMR32OFFST:
					case AMR33OFFST:
					case AMR34OFFST:
					case AMR35OFFST:
					case AMR36OFFST:
					case AMR37OFFST:
					case AMR38OFFST:
					case AMR39OFFST:
					case AMR40OFFST:
					case AMR41OFFST:
					case AMR42OFFST:
					case AMR43OFFST:
					case AMR44OFFST:
					case AMR45OFFST:
					case AMR46OFFST:
					case AMR47OFFST:
					case AMR48OFFST:
					case AMR49OFFST:
					case AMINTEGER:
						++vecIndex;
						break;

					case AMGVOFFSET:
						anObject = *(TObject**)&atHMInt(self->itsInstructionArray,vecIndex++);
						TObject_MarkObj(gCP,gTP,anObject);
						break;

					default:
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
						break;
					}
				switch(modifier[1])
					{
					case AMSVOFFSET:
					case AMAVOFFSET:
					case AMTVOFFSET:
					case AMPVOFFSET:
					case AMCVOFFSET:
					case AMREGISTER:
					case AMR07OFFST:
					case AMR08OFFST:
					case AMR09OFFST:
					case AMR10OFFST:
					case AMR11OFFST:
					case AMR12OFFST:
					case AMR13OFFST:
					case AMR14OFFST:
					case AMR15OFFST:
					case AMR16OFFST:
					case AMR17OFFST:
					case AMR18OFFST:
					case AMR19OFFST:
					case AMR20OFFST:
					case AMR21OFFST:
					case AMR22OFFST:
					case AMR23OFFST:
					case AMR24OFFST:
					case AMR25OFFST:
					case AMR26OFFST:
					case AMR27OFFST:
					case AMR28OFFST:
					case AMR29OFFST:
					case AMR30OFFST:
					case AMR31OFFST:
					case AMR32OFFST:
					case AMR33OFFST:
					case AMR34OFFST:
					case AMR35OFFST:
					case AMR36OFFST:
					case AMR37OFFST:
					case AMR38OFFST:
					case AMR39OFFST:
					case AMR40OFFST:
					case AMR41OFFST:
					case AMR42OFFST:
					case AMR43OFFST:
					case AMR44OFFST:
					case AMR45OFFST:
					case AMR46OFFST:
					case AMR47OFFST:
					case AMR48OFFST:
					case AMR49OFFST:
					case AMINTEGER:
						++vecIndex;
						break;

					case AMGVOFFSET:
						anObject = *(TObject**)&atHMInt(self->itsInstructionArray,vecIndex++);
						TObject_MarkObj(gCP,gTP,anObject);
						break;

					default:
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
						break;
					}
				if (modifier[2] >= AMINTEGER) 
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
				break; 
			}
		
		}
    }

FrameReturn;
}


/*--------------------------------------------------------------------------------------- */
#if 0
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

#endif

void    TPcodeVector_Doomed(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TPcodeVector*       self = (TPcodeVector*)asObject(&selfTval);

/* If immediate data space is used, do NOT try to release a memory handle. */
if (self->itsImmediatePtr != NULL)
	{
	self->itsMaxItemIndex = 0;																				 
	self->itsInstructionArray = NULL;
	self->itsImmediatePtr = NULL;
	return;
	}

FMemory_Free(gCP, gTP, (HMemory)self->itsInstructionArray);
self->itsMaxItemIndex = 0;
self->itsInstructionArray = NULL;
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TPcodeVector_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize)
{
TPcodeVector*       self = (TPcodeVector*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk;

*aSize += SIZEOF_TPcodeVectorOnDisk + (self->itsMaxItemIndex * sizeof(NUM));
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

#endif

HMemory TPcodeVector_Save(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory aHMemory)
{
UNUM                theOffset;
OPCODE              ExtendedOpcode;     
UNUM                modifier[3];
NUM                 modIndex;
NUM                 vecIndex;
NUM                 anInteger;
StartFrame
DeclareOBJ(TPcodeVector,self);
DeclareOBJ(TObject,anObject);
EndFrame

self = (TPcodeVector*)asObject(&selfTval);

TObjectOnDiskPtr(aHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsMaxItemIndex = self->itsMaxItemIndex;
TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsCurItemIndex = self->itsCurItemIndex;
TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsCdr = TObject_RegisterTval(gCP,gTP,self->itsCdr);

/*  Save the Pcode fields of the TPcodeVector, using */
/*  the virtual machine instruction modifiers to properly */
/*  recognize any object references imbedded in the */
/*  instruction stream. */
/*  Examine all pcodes in the Pcode Vector looking at the argument modifiers */
/*  for any Symbol, Object, or Tval arguments which must be marked. */
vecIndex = 0;
while (vecIndex < self->itsCurItemIndex)
    {
	if (vecIndex >= self->itsCurItemIndex)
		{
		anInteger = atHMInt(self->itsInstructionArray,vecIndex);
		TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
		}
	else
		{
		/*  Separate the opcode into the six modifiers. */
		/*  Separate the opcode into the six modifiers. */
		ExtendedOpcode.Opcode = atHMInt(self->itsInstructionArray,vecIndex);
		TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = ExtendedOpcode.Opcode;
		modifier[0] = ExtendedOpcode.u.Am1;
		modifier[1] = ExtendedOpcode.u.Am2;
		modifier[2] = ExtendedOpcode.u.Am3;

		/*  Memory opcodes: Append to the Pcode Vector all remaining arguments. */
		if (ExtendedOpcode.u.Pcode < VMSTARTREGISTERINS)
			{

			/*  Loop through the modifier patterns for arguments which need */
			/*  to be marked. */
			for (modIndex = 0; modIndex < 3; ++modIndex)
				{
				switch (modifier[modIndex])
					{
					case AMVOID:
						break;
                
					case AMSVOFFSET:
					case AMAVOFFSET:
					case AMTVOFFSET:
					case AMPVOFFSET:
					case AMCVOFFSET:
					case AMREGISTER:
					case AMR07OFFST:
					case AMR08OFFST:
					case AMR09OFFST:
					case AMR10OFFST:
					case AMR11OFFST:
					case AMR12OFFST:
					case AMR13OFFST:
					case AMR14OFFST:
					case AMR15OFFST:
					case AMR16OFFST:
					case AMR17OFFST:
					case AMR18OFFST:
					case AMR19OFFST:
					case AMR20OFFST:
					case AMR21OFFST:
					case AMR22OFFST:
					case AMR23OFFST:
					case AMR24OFFST:
					case AMR25OFFST:
					case AMR26OFFST:
					case AMR27OFFST:
					case AMR28OFFST:
					case AMR29OFFST:
					case AMR30OFFST:
					case AMR31OFFST:
					case AMR32OFFST:
					case AMR33OFFST:
					case AMR34OFFST:
					case AMR35OFFST:
					case AMR36OFFST:
					case AMR37OFFST:
					case AMR38OFFST:
					case AMR39OFFST:
					case AMR40OFFST:
					case AMR41OFFST:
					case AMR42OFFST:
					case AMR43OFFST:
					case AMR44OFFST:
					case AMR45OFFST:
					case AMR46OFFST:
					case AMR47OFFST:
					case AMR48OFFST:
					case AMR49OFFST:
					case AMINTEGER:
						anInteger = atHMInt(self->itsInstructionArray,vecIndex);
						TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
						break;
                
					case AMGVOFFSET:
						anInteger = atHMInt(self->itsInstructionArray,vecIndex);
						TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = (NUM)TObject_RegisterObject(gCP,gTP,(TObject*)anInteger);
						break;
                                
					default:
						break;
					}       
				}
			}
		else
		if (ExtendedOpcode.u.Pcode >= VMSTARTREGISTERINS)
			{
			switch (ExtendedOpcode.u.Pcode)
				{
				/* Register opcodes with no arguments. */
				case vmvecLoop:
					break;

				/* Register opcodes with one register argument. */
				case vmregJump:
				case vmvecPopNumber:
				case vmvecPushNumber:
					break;

				/* Register opcodes with two register arguments. */
				case vmregAbsNumber:
				case vmregInteger:
				case vmregNumber:
				case vmregAddInteger:
				case vmregAddNumber:
				case vmregAndInteger:
				case vmregCosNumber:
 				case vmregDivInteger:
				case vmregDivNumber:
				case vmregDivrInteger:
				case vmregDivrNumber:
				case vmregMoveInteger:
				case vmregMoveNumber:
				case vmregMulInteger:
				case vmregMulNumber:
				case vmregOrInteger:
				case vmregRefCharacter:
				case vmregRefFloat:
				case vmregRefInteger:
				case vmregRefNumber:
				case vmregRefShort:
				case vmregRefLong:
				case vmregSetCharacter:
				case vmregSetFloat:
				case vmregSetInteger:
				case vmregSetNumber:
				case vmregSetShort:
				case vmregSetLong:
				case vmregSinNumber:
				case vmregSqrtNumber:
				case vmregSubInteger:
				case vmregSubNumber:
				case vmregTanNumber:
				case vmregXorInteger:
				case vmregShlInteger:
				case vmregShrInteger:
					break;

				/* Register opcodes with three register arguments. */
				case vmregLogNumber:
				case vmregPwrNumber:
				case vmregRefXCharacter:
				case vmregRefXFloat:
				case vmregRefXInteger:
				case vmregRefXLong:
				case vmregRefXNumber:
				case vmregRefXShort:
				case vmregSetXCharacter:
				case vmregSetXFloat:
				case vmregSetXInteger:
				case vmregSetXLong:
				case vmregSetXNumber:
				case vmregSetXShort:
				case vmvecSetIncrements:
				case vmvecSetPointers:
					break;

				/* Register jmpcc opcodes with two register arguments and a label displacement. */
				case vmregJmpEQInteger:
				case vmregJmpLTInteger:
				case vmregJmpGTInteger:
				case vmregJmpNEInteger:
				case vmregJmpGEInteger:
				case vmregJmpLEInteger:
				case vmregJmpEQNumber:
				case vmregJmpLTNumber:
				case vmregJmpGTNumber:
				case vmregJmpNENumber:
				case vmregJmpGENumber:
				case vmregJmpLENumber:
					/*  Convert the two register modifiers to register displacements. */
					modifier[0] = modifier[0] << 8;
					modifier[1] = modifier[1] << 8;
					if (modifier[2] == AMINTEGER)
						{
						anInteger = atHMInt(self->itsInstructionArray,vecIndex);
						TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
						}
					break;

				/* Register jmpcc opcodes with one register argument one immediate and a label displacement. */
				case vmregJmpEQImmediate:
				case vmregJmpLTImmediate:
				case vmregJmpGTImmediate:
				case vmregJmpNEImmediate:
				case vmregJmpGEImmediate:
				case vmregJmpLEImmediate:
					modifier[0] = modifier[0] << 8;
					if (modifier[1] == AMINTEGER)
						{
						anInteger = atHMInt(self->itsInstructionArray,vecIndex);
						TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
						}
					if (modifier[2] == AMINTEGER)
						{
						anInteger = atHMInt(self->itsInstructionArray,vecIndex);
						TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
						}
					break;

				/* Register opcodes with one immediate and one register argument. */
				case vmregAddImmediate:
				case vmregAndImmediate:
				case vmregDivImmediate:
				case vmregLoadJmpPointer:
				case vmregDivrImmediate:
				case vmregMoveImmediate:
				case vmregMulImmediate:
				case vmregOrImmediate:
				case vmregShlImmediate:
				case vmregShrImmediate:
				case vmregSubImmediate:
				case vmregXorImmediate:
				case vmregSetCharImmediate:
				case vmregSetIntImmediate:
				case vmregSetLongImmediate:
				case vmregSetShortImmediate:
				case vmvecNumVector:
				case vmvecInitialize:
					/*  Convert the register modifier to a register displacement. */
					if (modifier[0] == AMINTEGER)
						{
						anInteger = atHMInt(self->itsInstructionArray,vecIndex);
						TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
						}
					break;

				/* Register opcodes with one immediate and two register argument. */
				case vmregAddPointer:
				case vmregSubPointer:
				case vmregSetXCharImmediate:
				case vmregSetXIntImmediate:
				case vmregSetXLongImmediate:
				case vmregSetXShortImmediate:
				case vmvecNumScalar:
					if (modifier[0] == AMINTEGER)
						{
						anInteger = atHMInt(self->itsInstructionArray,vecIndex);
						TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
						}
					if (modifier[1] == AMINTEGER)
						{
						anInteger = atHMInt(self->itsInstructionArray,vecIndex);
						TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
						}
					break;

				/* Register opcodes with one memory and one register argument. */
				case vmregSetWord:
				case vmregLoadAddress:
				case vmregLoadInteger:
				case vmregLoadTail:
				case vmregLoadDeclType:
				case vmregLoadType:
				case vmregLoadNumber:
				case vmregObjLength:
				case vmregObjPointer:
					switch(modifier[0])
						{
						case AMSVOFFSET:
						case AMAVOFFSET:
						case AMTVOFFSET:
						case AMPVOFFSET:
						case AMCVOFFSET:
						case AMREGISTER:
						case AMR07OFFST:
						case AMR08OFFST:
						case AMR09OFFST:
						case AMR10OFFST:
						case AMR11OFFST:
						case AMR12OFFST:
						case AMR13OFFST:
						case AMR14OFFST:
						case AMR15OFFST:
						case AMR16OFFST:
						case AMR17OFFST:
						case AMR18OFFST:
						case AMR19OFFST:
						case AMR20OFFST:
						case AMR21OFFST:
						case AMR22OFFST:
						case AMR23OFFST:
						case AMR24OFFST:
						case AMR25OFFST:
						case AMR26OFFST:
						case AMR27OFFST:
						case AMR28OFFST:
						case AMR29OFFST:
						case AMR30OFFST:
						case AMR31OFFST:
						case AMR32OFFST:
						case AMR33OFFST:
						case AMR34OFFST:
						case AMR35OFFST:
						case AMR36OFFST:
						case AMR37OFFST:
						case AMR38OFFST:
						case AMR39OFFST:
						case AMR40OFFST:
						case AMR41OFFST:
						case AMR42OFFST:
						case AMR43OFFST:
						case AMR44OFFST:
						case AMR45OFFST:
						case AMR46OFFST:
						case AMR47OFFST:
						case AMR48OFFST:
						case AMR49OFFST:
						case AMINTEGER:
							anInteger = atHMInt(self->itsInstructionArray,vecIndex);
							TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
							break;

						case AMGVOFFSET:
							anInteger = atHMInt(self->itsInstructionArray,vecIndex);
							TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = (NUM)TObject_RegisterObject(gCP,gTP,(TObject*)anInteger);
							break;
						}
					break; 

				/* Register opcodes with one immediate argument. */
				case vmregRunInHardware:
				case vmvecBinary:
				case vmvecSwapCC:
				case vmvecUnary:
					/*  Register the immediate value. */
					if (modifier[0] == AMINTEGER)
						{
						anInteger = atHMInt(self->itsInstructionArray,vecIndex);
						TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
						}
					break;

				/* Register opcodes with two immediate arguments. */
				case vmvecPop:
				case vmvecPush:
					/*  Register the immediate value. */
					if (modifier[0] == AMINTEGER)
						{
						anInteger = atHMInt(self->itsInstructionArray,vecIndex);
						TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
						}
					/*  Register the immediate value. */
					if (modifier[1] == AMINTEGER)
						{
						anInteger = atHMInt(self->itsInstructionArray,vecIndex);
						TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
						}
					break;

				/* Register opcodes with one immediate and one memory argument. */
				case vmregSaveTailImmediate:
				case vmregSaveDeclTypeImmediate:
					if (modifier[0] == AMINTEGER)
						{
						anInteger = atHMInt(self->itsInstructionArray,vecIndex);
						TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
						}
					switch(modifier[1])
						{
						case AMSVOFFSET:
						case AMAVOFFSET:
						case AMTVOFFSET:
						case AMPVOFFSET:
						case AMCVOFFSET:
						case AMREGISTER:
						case AMR07OFFST:
						case AMR08OFFST:
						case AMR09OFFST:
						case AMR10OFFST:
						case AMR11OFFST:
						case AMR12OFFST:
						case AMR13OFFST:
						case AMR14OFFST:
						case AMR15OFFST:
						case AMR16OFFST:
						case AMR17OFFST:
						case AMR18OFFST:
						case AMR19OFFST:
						case AMR20OFFST:
						case AMR21OFFST:
						case AMR22OFFST:
						case AMR23OFFST:
						case AMR24OFFST:
						case AMR25OFFST:
						case AMR26OFFST:
						case AMR27OFFST:
						case AMR28OFFST:
						case AMR29OFFST:
						case AMR30OFFST:
						case AMR31OFFST:
						case AMR32OFFST:
						case AMR33OFFST:
						case AMR34OFFST:
						case AMR35OFFST:
						case AMR36OFFST:
						case AMR37OFFST:
						case AMR38OFFST:
						case AMR39OFFST:
						case AMR40OFFST:
						case AMR41OFFST:
						case AMR42OFFST:
						case AMR43OFFST:
						case AMR44OFFST:
						case AMR45OFFST:
						case AMR46OFFST:
						case AMR47OFFST:
						case AMR48OFFST:
						case AMR49OFFST:
						case AMINTEGER:
							anInteger = atHMInt(self->itsInstructionArray,vecIndex);
							TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
							break;

						case AMGVOFFSET:
							anInteger = atHMInt(self->itsInstructionArray,vecIndex);
							TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = (NUM)TObject_RegisterObject(gCP,gTP,(TObject*)anInteger);
							break;
						}
					break;

				/* Register opcodes with one register and one memory argument. */
				case vmregRefXWord:
					switch(modifier[2])
						{
						case AMSVOFFSET:
						case AMAVOFFSET:
						case AMTVOFFSET:
						case AMPVOFFSET:
						case AMCVOFFSET:
						case AMREGISTER:
						case AMR07OFFST:
						case AMR08OFFST:
						case AMR09OFFST:
						case AMR10OFFST:
						case AMR11OFFST:
						case AMR12OFFST:
						case AMR13OFFST:
						case AMR14OFFST:
						case AMR15OFFST:
						case AMR16OFFST:
						case AMR17OFFST:
						case AMR18OFFST:
						case AMR19OFFST:
						case AMR20OFFST:
						case AMR21OFFST:
						case AMR22OFFST:
						case AMR23OFFST:
						case AMR24OFFST:
						case AMR25OFFST:
						case AMR26OFFST:
						case AMR27OFFST:
						case AMR28OFFST:
						case AMR29OFFST:
						case AMR30OFFST:
						case AMR31OFFST:
						case AMR32OFFST:
						case AMR33OFFST:
						case AMR34OFFST:
						case AMR35OFFST:
						case AMR36OFFST:
						case AMR37OFFST:
						case AMR38OFFST:
						case AMR39OFFST:
						case AMR40OFFST:
						case AMR41OFFST:
						case AMR42OFFST:
						case AMR43OFFST:
						case AMR44OFFST:
						case AMR45OFFST:
						case AMR46OFFST:
						case AMR47OFFST:
						case AMR48OFFST:
						case AMR49OFFST:
						case AMINTEGER:
							anInteger = atHMInt(self->itsInstructionArray,vecIndex);
							TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
							break;

						case AMGVOFFSET:
							anInteger = atHMInt(self->itsInstructionArray,vecIndex);
							TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = (NUM)TObject_RegisterObject(gCP,gTP,(TObject*)anInteger);
							break;
						}
					break;
				/* Register opcodes with one memory and two register arguments. */
				case vmregSetXWord:
					switch(modifier[0])
						{
						case AMSVOFFSET:
						case AMAVOFFSET:
						case AMTVOFFSET:
						case AMPVOFFSET:
						case AMCVOFFSET:
						case AMREGISTER:
						case AMR07OFFST:
						case AMR08OFFST:
						case AMR09OFFST:
						case AMR10OFFST:
						case AMR11OFFST:
						case AMR12OFFST:
						case AMR13OFFST:
						case AMR14OFFST:
						case AMR15OFFST:
						case AMR16OFFST:
						case AMR17OFFST:
						case AMR18OFFST:
						case AMR19OFFST:
						case AMR20OFFST:
						case AMR21OFFST:
						case AMR22OFFST:
						case AMR23OFFST:
						case AMR24OFFST:
						case AMR25OFFST:
						case AMR26OFFST:
						case AMR27OFFST:
						case AMR28OFFST:
						case AMR29OFFST:
						case AMR30OFFST:
						case AMR31OFFST:
						case AMR32OFFST:
						case AMR33OFFST:
						case AMR34OFFST:
						case AMR35OFFST:
						case AMR36OFFST:
						case AMR37OFFST:
						case AMR38OFFST:
						case AMR39OFFST:
						case AMR40OFFST:
						case AMR41OFFST:
						case AMR42OFFST:
						case AMR43OFFST:
						case AMR44OFFST:
						case AMR45OFFST:
						case AMR46OFFST:
						case AMR47OFFST:
						case AMR48OFFST:
						case AMR49OFFST:
						case AMINTEGER:
							anInteger = atHMInt(self->itsInstructionArray,vecIndex);
							TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
							break;

						case AMGVOFFSET:
							anInteger = atHMInt(self->itsInstructionArray,vecIndex);
							TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = (NUM)TObject_RegisterObject(gCP,gTP,(TObject*)anInteger);
							break;
						}
					break;

				/* Register opcodes with one register and one memory argument. */
				case vmregRefWord:
				case vmregSaveInteger:
				case vmregSaveTail:
				case vmregSaveDeclType:
				case vmregSaveNumber:
					switch(modifier[1])
						{
						case AMSVOFFSET:
						case AMAVOFFSET:
						case AMTVOFFSET:
						case AMPVOFFSET:
						case AMCVOFFSET:
						case AMREGISTER:
						case AMR07OFFST:
						case AMR08OFFST:
						case AMR09OFFST:
						case AMR10OFFST:
						case AMR11OFFST:
						case AMR12OFFST:
						case AMR13OFFST:
						case AMR14OFFST:
						case AMR15OFFST:
						case AMR16OFFST:
						case AMR17OFFST:
						case AMR18OFFST:
						case AMR19OFFST:
						case AMR20OFFST:
						case AMR21OFFST:
						case AMR22OFFST:
						case AMR23OFFST:
						case AMR24OFFST:
						case AMR25OFFST:
						case AMR26OFFST:
						case AMR27OFFST:
						case AMR28OFFST:
						case AMR29OFFST:
						case AMR30OFFST:
						case AMR31OFFST:
						case AMR32OFFST:
						case AMR33OFFST:
						case AMR34OFFST:
						case AMR35OFFST:
						case AMR36OFFST:
						case AMR37OFFST:
						case AMR38OFFST:
						case AMR39OFFST:
						case AMR40OFFST:
						case AMR41OFFST:
						case AMR42OFFST:
						case AMR43OFFST:
						case AMR44OFFST:
						case AMR45OFFST:
						case AMR46OFFST:
						case AMR47OFFST:
						case AMR48OFFST:
						case AMR49OFFST:
						case AMINTEGER:
							anInteger = atHMInt(self->itsInstructionArray,vecIndex);
							TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
							break;

						case AMGVOFFSET:
							anInteger = atHMInt(self->itsInstructionArray,vecIndex);
							TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = (NUM)TObject_RegisterObject(gCP,gTP,(TObject*)anInteger);
							break;
						}
					break;

				/* Register opcodes with two memory and one register argument. */
				case vmregStringCompare:
				case vmregStringiCompare:
					switch(modifier[0])
						{
						case AMSVOFFSET:
						case AMAVOFFSET:
						case AMTVOFFSET:
						case AMPVOFFSET:
						case AMCVOFFSET:
						case AMREGISTER:
						case AMR07OFFST:
						case AMR08OFFST:
						case AMR09OFFST:
						case AMR10OFFST:
						case AMR11OFFST:
						case AMR12OFFST:
						case AMR13OFFST:
						case AMR14OFFST:
						case AMR15OFFST:
						case AMR16OFFST:
						case AMR17OFFST:
						case AMR18OFFST:
						case AMR19OFFST:
						case AMR20OFFST:
						case AMR21OFFST:
						case AMR22OFFST:
						case AMR23OFFST:
						case AMR24OFFST:
						case AMR25OFFST:
						case AMR26OFFST:
						case AMR27OFFST:
						case AMR28OFFST:
						case AMR29OFFST:
						case AMR30OFFST:
						case AMR31OFFST:
						case AMR32OFFST:
						case AMR33OFFST:
						case AMR34OFFST:
						case AMR35OFFST:
						case AMR36OFFST:
						case AMR37OFFST:
						case AMR38OFFST:
						case AMR39OFFST:
						case AMR40OFFST:
						case AMR41OFFST:
						case AMR42OFFST:
						case AMR43OFFST:
						case AMR44OFFST:
						case AMR45OFFST:
						case AMR46OFFST:
						case AMR47OFFST:
						case AMR48OFFST:
						case AMR49OFFST:
						case AMINTEGER:
							anInteger = atHMInt(self->itsInstructionArray,vecIndex);
							TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
							break;

						case AMGVOFFSET:
							anInteger = atHMInt(self->itsInstructionArray,vecIndex);
							TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = (NUM)TObject_RegisterObject(gCP,gTP,(TObject*)anInteger);
							break;
						}
					switch(modifier[1])
						{
						case AMSVOFFSET:
						case AMAVOFFSET:
						case AMTVOFFSET:
						case AMPVOFFSET:
						case AMCVOFFSET:
						case AMREGISTER:
						case AMR07OFFST:
						case AMR08OFFST:
						case AMR09OFFST:
						case AMR10OFFST:
						case AMR11OFFST:
						case AMR12OFFST:
						case AMR13OFFST:
						case AMR14OFFST:
						case AMR15OFFST:
						case AMR16OFFST:
						case AMR17OFFST:
						case AMR18OFFST:
						case AMR19OFFST:
						case AMR20OFFST:
						case AMR21OFFST:
						case AMR22OFFST:
						case AMR23OFFST:
						case AMR24OFFST:
						case AMR25OFFST:
						case AMR26OFFST:
						case AMR27OFFST:
						case AMR28OFFST:
						case AMR29OFFST:
						case AMR30OFFST:
						case AMR31OFFST:
						case AMR32OFFST:
						case AMR33OFFST:
						case AMR34OFFST:
						case AMR35OFFST:
						case AMR36OFFST:
						case AMR37OFFST:
						case AMR38OFFST:
						case AMR39OFFST:
						case AMR40OFFST:
						case AMR41OFFST:
						case AMR42OFFST:
						case AMR43OFFST:
						case AMR44OFFST:
						case AMR45OFFST:
						case AMR46OFFST:
						case AMR47OFFST:
						case AMR48OFFST:
						case AMR49OFFST:
						case AMINTEGER:
							anInteger = atHMInt(self->itsInstructionArray,vecIndex);
							TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = anInteger;
							break;

						case AMGVOFFSET:
							anInteger = atHMInt(self->itsInstructionArray,vecIndex);
							TPcodeVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[vecIndex++] = (NUM)TObject_RegisterObject(gCP,gTP,(TObject*)anInteger);
							break;
						}
					break; 
				}
			
			}
		}
    }

FrameExit(aHMemory);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Copy

Make a copy of a TPcodeVector.

#endif

TObject*    TPcodeVector_Copy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TPcodeVector,self);
DeclareOBJ(TPcodeVector,theCopy);
DeclareTVAL(tmpTval);
EndFrame

self = (TPcodeVector*)selfTval.u.Object;

theCopy = TPcodeVector_New(gCP,gTP);

tmpTval->u.Object = (TObject*)theCopy;
tmpTval->Tag = theCopy->itsObjectType;

TPcodeVector_SetMaxIndex(gCP, gTP, *tmpTval, self->itsMaxItemIndex);
TPcodeVector_SetCdr(gCP,gTP,*tmpTval, self->itsCdr);

if (self->itsInstructionArray != NULL)
    {
    _FMemory_memcpy((LpCHAR)&atHMInt(theCopy->itsInstructionArray,0),(LpCHAR)&atHMInt(self->itsInstructionArray,0),(LONG)(self->itsMaxItemIndex*sizeof(NUM)));
    }

FrameExit((TObject*)theCopy);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetMaxIndex

Return the maximum size of the repeating portion of this object.

#endif

NUM TPcodeVector_GetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TPcodeVector*       self = (TPcodeVector*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(self->itsMaxItemIndex);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetMaxIndex

Set the maximum size of the repeating portion of this object.

Note:   Rember the repeating portion of this object is measured in Pcodes!

#endif

TVAL TPcodeVector_SetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM newRepeats)
{
NUM			n;
NUM			oldMaxItemIndex;
LpNUM		ptr;
StartFrame
DeclareOBJ(TPcodeVector,self);
EndFrame

self = (TPcodeVector*)selfTval.u.Object;

/*  Do not allow a resize for negative lengths */
if (newRepeats < 0) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
oldMaxItemIndex = self->itsMaxItemIndex;

/* Use immediate data space (if the requested size will allow). */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if (newRepeats <= (NUM)(_TPcodeVector_ImmediateSpace/(NUM)sizeof(NUM)))
	{
	if (self->itsInstructionArray == NULL) 
		{
		_FMemory_memset(self->itsImmediateSpace,_VMRETURNVOID,newRepeats*sizeof(NUM));
		}
	else
		{
		_FMemory_memcpy(self->itsImmediateSpace,(char*)PcodeArray(selfTval),min(newRepeats,self->itsMaxItemIndex)*sizeof(NUM));
		if ((self->itsInstructionArray != NULL) && (self->itsImmediatePtr == NULL)) FMemory_Free(gCP,gTP,(HMemory)self->itsInstructionArray);
		}
	self->itsInstructionArray = (HMInt)&self->itsImmediatePtr;
	self->itsImmediatePtr = (CHAR*)&self->itsImmediateSpace[0];
    self->itsMaxItemIndex = newRepeats;
	}
else
/*  Either create or resize the item array handle. */
if (self->itsInstructionArray == NULL)
    {
    self->itsInstructionArray = (HMInt)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(NUM)),TRUE);
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
    }
else
/* Existing data is in immediate data space. */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if ((self->itsImmediatePtr != NULL) && (newRepeats != self->itsMaxItemIndex))
	{
    self->itsInstructionArray = (HMInt)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(NUM)),TRUE);
	_FMemory_memcpy((char*)PcodeArray(selfTval),self->itsImmediateSpace,min(newRepeats,self->itsMaxItemIndex)*sizeof(NUM));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	}
else
if (newRepeats != self->itsMaxItemIndex)
    {
    self->itsInstructionArray = (HMInt)FMemory_Resize(gCP, gTP, (HMemory)self->itsInstructionArray,(LONG)(newRepeats*sizeof(NUM)));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
    }

/* Initialize any skipped items (if necesssary). */
if (oldMaxItemIndex < newRepeats)
	{
	ptr = (LpNUM)&PcodeArray(selfTval)[oldMaxItemIndex];
	for (n = oldMaxItemIndex; n < newRepeats; ++n)
		{
		*(ptr++) = 0;
		}
	}
	
FrameExit(gCP->TObject_OK);

}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIndexedValue

Return the indexed value from the repeating portion of this object.

#endif

TVAL TPcodeVector_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index)
{
NUM                 indexOf;
StartFrame
DeclareOBJ(TPcodeVector,self);
DeclareTVAL(retValue);
EndFrame

self = (TPcodeVector*)selfTval.u.Object;


/*  We only accept numeric indices. */
if (isNumIndex(&index))
    indexOf = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
/*  Make sure array index is in range. */
if ((indexOf < 0) || (indexOf >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/*  Convert the nth Pcode item into a tval. */
asInt(retValue) = atHMInt(self->itsInstructionArray,indexOf);
asTag(retValue) = TYNUM;
FrameExit(*retValue);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIndexedValue

Set the indexed value in the repeating portion of this object.

Note:   

#endif

TVAL TPcodeVector_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index,TVAL newValue)
{
TYPE                tag;
NUM                 indexOf;
StartFrame
DeclareOBJ(TPcodeVector,self);
DeclareTVAL(ret);
EndFrame

self = (TPcodeVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    indexOf = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/*  Make sure array index is in range. If too large, then grow the */
/*  array dynamically to receive the new value. */
if (indexOf < 0)
	{
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}
else
if (indexOf >= self->itsMaxItemIndex)
    TPcodeVector_SetMaxIndex(gCP, gTP, selfTval, indexOf+1);

/*  Save the tval as the nth Pcode item. */

switch(tag = asTag(&newValue))
    {
    case TYNUM:
        atHMInt(self->itsInstructionArray,indexOf) = asInt(&newValue);
    break;
    
    case TYREAL:
        atHMInt(self->itsInstructionArray,indexOf) = asReal(&newValue);
    break;
    
    case TYBOLE:
        atHMInt(self->itsInstructionArray,indexOf) = asBool(&newValue);
    break;
    
    default:
        atHMInt(self->itsInstructionArray,indexOf) = 0;
    break;
    }

asTag(ret) = self->itsObjectType;
asObject(ret) = (TObject*)self;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
AddNewValue

Add a new value to the repeating portion of this object.

Note:   

#endif

TVAL TPcodeVector_AddNewValue(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL newValue)
{
StartFrame
DeclareOBJ(TPcodeVector,self);
DeclareTVAL(index);
EndFrame

self = (TPcodeVector*)asObject(&selfTval);
asInt(index) = self->itsMaxItemIndex;
asTag(index) = TYNUM;

FrameExit(TPcodeVector_SetIV1(gCP, gTP, selfTval, *index, newValue));
}

/*--------------------------------------------------------------------------------------- */
#if 0
Delete

Delete the indexed value from the repeating portion of this object.

Note:   All of the remaining values are moved down one position and
        the Vector is resized.

#endif

TVAL TPcodeVector_Delete(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index)
{
register LpNUM          targetPtr;
register LpNUM          sourcePtr;
register LpNUM          haltPtr;
NUM                     deleteIndex;
StartFrame
DeclareOBJ(TPcodeVector,self);
EndFrame
self = (TPcodeVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    deleteIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  Make sure array index is in range. */
if ((deleteIndex < 0) || (deleteIndex >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Move all of the remaining values in the array down one position */

sourcePtr = &atHMInt(self->itsInstructionArray,deleteIndex+1);
targetPtr = &atHMInt(self->itsInstructionArray,deleteIndex);
haltPtr = &atHMInt(self->itsInstructionArray, self->itsMaxItemIndex);
while (sourcePtr < haltPtr)
    {
    *(targetPtr++) = *(sourcePtr++);
    }
    
/*  Resize the Vector down one position */
TPcodeVector_SetMaxIndex(gCP, gTP, selfTval, self->itsMaxItemIndex-1);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Insert

Insert the indexed value in the repeating portion of this object. 

Note:   All of the haigher values are moved up one position and
        the Vector is resized.

#endif

TVAL TPcodeVector_Insert(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index,TVAL newValue)
{
register LpNUM          targetPtr;
register LpNUM          sourcePtr;
register LpNUM          insertPtr;
NUM                     insertIndex;
StartFrame
DeclareOBJ(TPcodeVector,self);
EndFrame
self = (TPcodeVector*)asObject(&selfTval);


/*  We only accept numeric indices. */
if (isNumIndex(&index))
    insertIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  Make sure array index is in range. */
if ((insertIndex < 0) || (insertIndex >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Resize the array up one position */
TPcodeVector_SetMaxIndex(gCP, gTP, selfTval, self->itsMaxItemIndex+1);

/*  Move all of the remaining values in the array up one position */

sourcePtr = &atHMInt(self->itsInstructionArray, (self->itsMaxItemIndex-2));
targetPtr = &atHMInt(self->itsInstructionArray, (self->itsMaxItemIndex-1));
insertPtr = &atHMInt(self->itsInstructionArray,insertIndex);
while (sourcePtr >= insertPtr)
    {
    *(targetPtr--) = *(sourcePtr--);
    }
        
/*  Save the new value into the Pcode vector. */
FrameExit(TPcodeVector_SetIV1(gCP, gTP, selfTval, index,newValue));
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetCdr

Return the Lisp tail(cdr) of this object.

Note:   All TObject children must have this method. Even objects which do
        not contain a Lisp head(car) or tail(cdr), must at least respond
        to this message.
 
#endif

TVAL TPcodeVector_GetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TPcodeVector*       self = (TPcodeVector*)asObject(&selfTval);
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(self->itsCdr);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetCdr

Set the Lisp tail(cdr) of this object.

Note:   All TObject children must have this method. Even objects which do
        not contain a Lisp head(car) or tail(cdr), must at least respond
        to this message.
 
#endif

TVAL TPcodeVector_SetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL newValue)
{
TPcodeVector*       self = (TPcodeVector*)asObject(&selfTval);

gTP = gTP; // NOOP to hide unused parameter warning message
self->itsCdr = newValue;
return(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TPcodeVector_New

Create a new TPcodeVector.

#endif

TPcodeVector*   TPcodeVector_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TPcodeVector,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TPcodeVector_Initialized) TPcodeVector_Init(gCP,gTP);

self = (TPcodeVector*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYPCODEVECTOR;
self->itsMaxItemIndex = 0;
self->itsCurItemIndex = 0;
self->itsInstructionArray = NULL;
self->itsImmediatePtr = NULL;
FrameExit(self);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Print

Convert a PcodeVector object into an ascii string and append it to an output buffer. 

#endif

TVAL     TPcodeVector_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size, LpCHAR buf)
{
NUM                     indexOf;
StartFrame	
DeclareOBJ(TPcodeVector,self);
DeclareTVAL(ec);
DeclareTVAL(item);
EndFrame

self = (TPcodeVector*)asObject(&selfTval);

/*  Quit if the output string is already too long */

if (*size + 2 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);

/*  Show Vector prefix */

buf[*size]      = '#';
buf[++(*size)]  = '(';
buf[++(*size)]  = 'p';
buf[++(*size)]  = 'c';
buf[++(*size)]  = 'd';
buf[++(*size)]  = '|';
buf[++(*size)]  = ' ';
buf[++(*size)]	= 0;

for(indexOf = 0; indexOf < self->itsMaxItemIndex; indexOf++)
    {
	*item = FSmartbase_Ref(gCP,gTP,2,selfTval,TINT(indexOf));
    *ec = FConio_sprintn(gCP,gTP,buf,size,*item);
    _TObject_ErrorChk(*ec);
    
     if (*size + 2 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
        
    buf[*size]      = ' ';
    buf[++(*size)]  = 0;
    }


/*  Show Vector suffix */

 if (*size + 2 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);
    
buf[*size]      = ')';
buf[++(*size)]  = 0;

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0

TPcodeVector_MakeNew

Return a Vector object of the specified size (argv[0]) with the specified
initial values and optional cdr value.  

Note:   If no arguments are specified, return an error. 
        If only one argument is specified, fill the Vector with zeros. 
        If too few arguments are specified, fill the Vector with repeating
            patterns of the specified initializers.
        If the special character '.' is encountered, assign the cdr value. 
            
        (new PcodeVector 5)             =>      #(0 0 0 0 0) 
        (new PcodeVector 5 1)           =>      #(1 1 1 1 1) 
        (new PcodeVector 5 1 2 3)       =>      #(1 2 3 1 2) 
        (new PcodeVector 5 1 2 3 . 6)   =>      #(1 2 3 1 2 . 6) 
        
#endif

TVAL TPcodeVector_MakeNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
NUM                     size = 0;
NUM                     sizeIndex = 1;
NUM                     vectorIndex = 0;
NUM                     startIndex = sizeIndex + 1;
NUM                     argIndex = sizeIndex + 1;
NUM                     cdrIndex = argc;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
DeclareTVAL(ndx);
DeclareTVAL(fill);
DeclareTVAL(ivTval);
DeclareOBJ(TPcodeVector,pp);
EndFrame
 
/*  The first argument should be the requested size. */

if (argc == 0)
    {
    size = 0;
    }
else
    {
    if (!isNumIndex(&argv[sizeIndex]))
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    else
        {
        size = isNumIndex(&argv[sizeIndex]);
        }
    
    if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    asTag(ndx) = TYNUM;

    /*  This is a request to construct a Pcode vector. */
    
    asTag(ret) = TYVOID;
    asObj(ret) = NIL;

    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
    if(isERROR(err))
        {
        FrameExit(*err);
        }
    else
        size = asInt(err);
    
    if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
    pp = TPcodeVector_New(gCP,gTP);
    FObject_SetMaxIndex(gCP,gTP,(TObject*)pp, size);
    
    /*  Initialize the Pcode Vector object */
    
    asTag(ret)		= TYPCODEVECTOR;
    asObject(ret)   = (TObject*)pp;
    asTag(fill)     = TYNUM;
    asInt(fill)     = 0;
    
    /*  Initialize the vector only if necessary (important time saving). */
    if (argc > startIndex)
        {
        asObject(ivTval) = (TObject*)pp;
        asTag(ivTval) = pp->itsObjectType;
        while (vectorIndex < size)
            {
            if (argIndex >= argc) argIndex = startIndex;
            if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
                {
                cdrIndex = argIndex + 1;
                argIndex = startIndex;
                }
            if (argIndex < argc) *fill = argv[argIndex++];
            
            asInt(ndx) = vectorIndex++;
        
            (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,  *ivTval, *ndx, *fill);
            }
         
        
        /*  Save the optional cdr argument */
        if ((argIndex < argc) && 
            (asTag(&argv[argIndex]) == TYPCODE) && 
            (asShort(&argv[argIndex]) == PERIODTOK))
            {
            cdrIndex = argIndex + 1;
            }
        if (cdrIndex < argc)
            {
            FObject_SetCdr(gCP,gTP,(TObject*)pp, argv[cdrIndex]);
            }
        }
    }


FrameExit(*ret);
}
