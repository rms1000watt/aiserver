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
TPcodeVector.h

Implementation of the Pcode vector CLASS which stores a variable number of
Pcodes in a linear array. The manifest typing system together with C++ methods 
control and size the the array.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

CHANGE HISTORY
Version     Date        Who     Change
1.0001      8/03/2006   raj     Added OPCODE for AMD64 JIT
												--------------- ---------------
#endif
 
#ifndef _H_TPcodeVector
#define _H_TPcodeVector

#include "tobject.h"



/*  Maximum number of Integer and Number Registers to Allocate. */
#define MAXREGALLOCATION		16

/*  Size of a TVAL superimposed onto a Pcode Vector */
#define     TVALWORD        ((sizeof(TVAL) + (sizeof(NUM)-1)) / sizeof(NUM))

#define PcodeArray(tval)        ((LpNUM)*((TPcodeVector*)((tval).u.Object))->itsInstructionArray)
#define PcodeVector(tval)       ((TPcodeVector*)((tval).u.Object))
#define asPcodeVector(atval)    ((TPcodeVector*)(((TVAL*)atval)->u.Object))
    
/*  The disk structure format */
typedef struct
    {
    NUM                 itsMaxItemIndex;
    NUM                 itsCurItemIndex;
    TVAL                itsCdr;         
    NUM                 itsItemArray[1];    
    }TPcodeVectorOnDisk;
#define TPcodeVectorOnDiskPtr(h,n)  ((TPcodeVectorOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TPcodeVectorOnDisk   ((NUM)&((TPcodeVectorOnDisk*)0)->itsItemArray)

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */
/*  The arguments will be the same as for TPcodeVectorOnDiskPtr. */

#define TPcodeVectorOnDiskData(h,n) (NUM*)(_HMemoryPtr(h)+n+SIZEOF_TPcodeVectorOnDisk)

#ifdef _SMARTBASE
/*  Virtual machine instruction opcode declarations  */
#define     VMRETURN					0
#define     VMADD						1
#define     VMADDI						2
#define     VMAND						3
#define     VMAPPLY						4
#define     VMARGCOUNT					5
#define     VMARGFETCH					6
#define     VMCALL						7
#define     VMDIV						8
#define     VMDIVI						9
#define     VMDIVR						10
#define     VMDIVRI						11
#define     VMIADD						12
#define     VMIDIV						13
#define     VMIDIVR						14
#define     VMIMUL						15
#define     VMISUB						16

#define     VMJMPEQ						17  /* Note: The conditional jump instructions must  */
#define     VMJMPLT						18  /*       be kept in this relative order so the   */
#define     VMJMPGT						19  /*       computations in FOptimize2_boolJmp will */
#define     VMJMPNE						20  /*       work properly.                          */
#define     VMJMPGE						21  /*                                               */
#define     VMJMPLE						22  /* Note: End of pcode order restrictions         */

#define     VMJUMP						23
#define     VMMOVE						24
#define     VMMUL						25
#define     VMMULI						26
#define     VMNADD						27
#define     VMIAND						28
#define     VMIANDB						29
#define     VMNDIV						30
#define     VMNDIVR						31
#define     VMDEBUGGER					32
#define     VMNMUL						33
#define     VMIOR						34
#define     VMIORB						35
#define     VMNSUB						36
#define     VMIXOR						37
#define     VMIXORB						38
#define     VMONERROR					39
#define     VMOR						40
#define     VMPOP						41
#define     VMPUSH						42
#define     VMREF						43
#define     VMSELF						44
#define     VMSEND						45
#define     VMSET						46
#define     VMSHL						47
#define     VMSHR						48
#define     VMSUB						49
#define     VMSUBI						50
#define     VMXOR						51
										
#define     VMADDN						52
#define     VMDIVN						53
#define     VMMOVEI						54
#define     VMMULN						55
#define     VMSUBN						56

#define     VMCADD						57
#define     VMCDIV						58
#define     VMMOVEN						59
#define     VMCMUL						60
#define     VMCSUB						61

											/* Extended strong typed instructions.           */
											/*       (New in Version 3.01)			         */
											/* These new instructions allow faster memory    */
											/* to memory computation at the expense of       */
											/* strong typing and loss of generality.         */
#define     VMREFTEXT					62
#define     VMREFSTRING					63
#define     VMSETSTRING					64
#define     VMREFSYMBOL					65
#define     VMREFVECTOR					66
#define     VMSETVECTOR					67
#define     VMREFSTRVALUE				68
#define     VMSETSTRVALUE				69
#define     VMREFSTRKEY					70
#define     VMSETSTRKEY					71
#define     VMREFDICVALUE				72
#define     VMSETDICVALUE				73
#define     VMREFDICKEY					74
#define     VMSETDICKEY					75
#define     VMREFDIRVALUE				76
#define     VMSETDIRVALUE				77
#define     VMREFDIRKEY					78
#define     VMSETDIRKEY					79
#define     VMREFBITVECTOR				80
#define     VMSETBITVECTOR				81
#define     VMREFBYTVECTOR				82
#define     VMSETBYTVECTOR				83
#define     VMREFPCDVECTOR				84
#define     VMSETPCDVECTOR				85
#define     VMREFOBJVECTOR				86
#define     VMSETOBJVECTOR				87
#define     VMREFINTVECTOR				88
#define     VMSETINTVECTOR				89
#define     VMREFNUMVECTOR				90
#define     VMSETNUMVECTOR				91
#define     VMREFFLTVECTOR				92
#define     VMSETFLTVECTOR				93
#define     VMREFMATRIX					94
#define     VMSETMATRIX					95
#define     VMREFNUMMATRIX				96
#define     VMSETNUMMATRIX				97
#define     VMTESTESCAPE				98

#define     vmnatAddInteger				99
#define     vmnatAddNumber  			100
#define     vmnatAndInteger				101
#define     vmnatDivInteger				102
#define     vmnatDivNumber				103
#define     vmnatDivrInteger			104  
#define     vmnatDivrNumber				105  

#define     vmnatJmpEQInteger			106 /* Note: The conditional jump instructions must  */		
#define     vmnatJmpLTInteger			107 /*       be kept in this relative order so the   */
#define     vmnatJmpGTInteger			108 /*       computations in FOptimize2_boolJmp will */
#define     vmnatJmpNEInteger			109 /*       work properly.                          */
#define     vmnatJmpGEInteger			110 /*                                               */
#define     vmnatJmpLEInteger			111 /* Note: End of pcode order restrictions         */

#define     vmnatJmpEQNumber			112 /* Note: The conditional jump instructions must  */
#define     vmnatJmpLTNumber			113 /*       be kept in this relative order so the   */
#define     vmnatJmpGTNumber			114 /*       computations in FOptimize2_boolJmp will */
#define     vmnatJmpNENumber			115 /*       work properly.                          */
#define     vmnatJmpGENumber			116 /*                                               */
#define     vmnatJmpLENumber			117 /* Note: End of pcode order restrictions         */

#define     vmnatMulInteger 			118  
#define     vmnatMulNumber				119  
#define     vmnatOrInteger				120
#define     vmnatLoadCharacter			121  
#define     vmnatLoadFloat				122
#define     vmnatLoadInteger			123
#define     vmnatLoadLong				124
#define     vmnatLoadNumber				125
#define     vmnatLoadObject				126  
#define     vmnatLoadShort      		127  
#define     vmnatSaveCharacter   		128  
#define     vmnatSaveFloat		   		129  
#define     vmnatSaveInteger	   		130  
#define     vmnatSaveLong				131
#define     vmnatSaveNumber		   		132  
#define     vmnatSaveObject		   		133  
#define     vmnatSaveShort				134
#define     vmnatShlInteger				135
#define     vmnatShrInteger				136
#define     vmnatSubInteger				137  
#define     vmnatSubNumber				138  
#define     vmnatXorInteger				139

											/* Extended register to register instructions.	 */
											/*           (New in Version 4.00)			     */
											/* These new instructions allow faster register  */
											/* to register computation at the expense of     */
											/* strong typing and loss of generality.         */
											/* Note: The register to register instructions   */
											/*       are expressed in upper/lower case style */
											/*       to further differentiate them from the  */
											/*       memory to memory vm instructions.       */
#define     VMSTARTREGISTERINS			140

#define     vmregAddImmediate			140
#define     vmregAddInteger				141
#define     vmregAddNumber				142
#define     vmregAddPointer	     		143
#define     vmregAndImmediate			144  
#define     vmregAndInteger				145  
#define     vmregDivImmediate			146  
#define     vmregDivInteger				147  
#define     vmregDivNumber				148  
#define     vmregIncPointer				149  

#define     vmregJmpEQImmediate			150 /* Note: The conditional jump instructions must  */		
#define     vmregJmpLTImmediate			151 /*       be kept in this relative order so the   */
#define     vmregJmpGTImmediate			152 /*       computations in FOptimize2_boolJmp will */
#define     vmregJmpNEImmediate			153 /*       work properly.                          */
#define     vmregJmpGEImmediate			154 /*                                               */
#define     vmregJmpLEImmediate			155 /* Note: End of pcode order restrictions         */

#define     vmregJmpEQInteger			156 /* Note: The conditional jump instructions must  */		
#define     vmregJmpLTInteger			157 /*       be kept in this relative order so the   */
#define     vmregJmpGTInteger			158 /*       computations in FOptimize2_boolJmp will */
#define     vmregJmpNEInteger			159 /*       work properly.                          */
#define     vmregJmpGEInteger			160 /*                                               */
#define     vmregJmpLEInteger			161 /* Note: End of pcode order restrictions         */
 
#define     vmregJmpEQNumber			162 /* Note: The conditional jump instructions must  */		
#define     vmregJmpLTNumber			163 /*       be kept in this relative order so the   */
#define     vmregJmpGTNumber			164 /*       computations in FOptimize2_boolJmp will */
#define     vmregJmpNENumber			165 /*       work properly.                          */
#define     vmregJmpGENumber			166 /*                                               */
#define     vmregJmpLENumber			167 /* Note: End of pcode order restrictions         */
  
#define     vmregJump					168  
#define     vmregLoadAddress			169  
#define     vmregLoadDeclType			170
#define     vmregLoadInteger			171  
#define     vmregLoadJmpPointer			172
#define     vmregLoadNumber				173
#define     vmregLoadTail				174
#define     vmregLoadType				175
#define     vmregDivrImmediate			176
#define     vmregDivrInteger			177
#define     vmregDivrNumber				178  
#define     vmregMoveImmediate			179  
#define     vmregMoveInteger			180  
#define     vmregMoveNumber				181  
#define     vmregMulImmediate			182  
#define     vmregMulInteger				183  
#define     vmregMulNumber				184  
#define     vmregObjLength				185
#define     vmregObjPointer				186
#define     vmregOrImmediate			187
#define     vmregOrInteger				188
#define     vmregRefCharacter			189
#define     vmregRefFloat				190  
#define     vmregRefInteger				191  
#define		vmregRefLong				192
#define     vmregRefNumber				193  
#define     vmregRefShort				194  
#define     vmregRefWord				195
#define     vmregRefXCharacter			196  
#define     vmregRefXFloat				197 
#define     vmregRefXInteger			198
#define		vmregRefXLong				199
#define     vmregRefXNumber				200
#define     vmregRefXShort				201
#define     vmregRefXWord				202
#define		vmregRunInHardware			203
#define     vmregSaveDeclType			204
#define     vmregSaveDeclTypeImmediate	205  
#define     vmregSaveInteger			206
#define     vmregSaveNumber				207  
#define     vmregSaveTail				208
#define     vmregSaveTailImmediate		209  
#define     vmregSetCharacter			210  
#define     vmregSetCharImmediate		211
#define     vmregSetFloat				212  
#define     vmregSetInteger				213  
#define     vmregSetIntImmediate		214
#define		vmregSetLong				215
#define     vmregSetLongImmediate		216
#define     vmregSetNumber				217  
#define     vmregSetShort				218  
#define     vmregSetShortImmediate		219
#define     vmregSetWord				220
#define     vmregSetXCharacter			221
#define     vmregSetXCharImmediate		222
#define     vmregSetXFloat				223
#define     vmregSetXInteger			224
#define     vmregSetXIntImmediate		225
#define		vmregSetXLong				226
#define     vmregSetXLongImmediate		227
#define     vmregSetXNumber				228
#define     vmregSetXShort				229  
#define     vmregSetXShortImmediate		230
#define     vmregSetXWord				231
#define     vmregShlImmediate			232  
#define     vmregShlInteger				233  
#define     vmregShrImmediate			234  
#define     vmregShrInteger				235  
#define     vmregStringCompare			236  
#define     vmregStringiCompare			237 
#define     vmregSubImmediate			238  
#define     vmregSubInteger				239
#define     vmregSubNumber				240
#define     vmregSubPointer				241
#define     vmregXorImmediate			242  
#define     vmregXorInteger				243  

#define     vmregAbsNumber				244
#define     vmregLogNumber				245
#define     vmregPwrNumber				246
#define     vmregSqrtNumber				247
#define     vmregSinNumber				248
#define     vmregCosNumber				249
#define     vmregTanNumber				250  

#define     vmvecBinary					251  
#define     vmvecInitialize				252  
#define     vmvecLoop					253  
#define     vmvecNumScalar				254  
#define     vmvecNumVector				255
#define     vmvecPop					256  
#define     vmvecPopNumber				257  
#define     vmvecPush					258  
#define     vmvecPushNumber				259  
#define     vmvecSetIncrements			260  
#define     vmvecSetPointers			261  
#define     vmvecSwapCC					262  
#define     vmvecUnary					263  
 
#define     vmregInteger				264
#define     vmregNumber					265

/*  MAXIMUM VM INSTRUCTIONS DECLARATION */
/*  This key macro is ALSO defined in "fsmtbase.h".	*/
/*  WARNING: Keep these two definitions identical	*/
#define     _VMMAXINSTRS				266

/*  Virtual machine compiler directive pseudo-instructions */
#define     VMCALLARG					_VMMAXINSTRS + 1
#define     VMNOP						_VMMAXINSTRS + 2
#define     VMOPT						_VMMAXINSTRS + 3

#define     _VMRETURNVOID				-256	/* 0x00FFFFFF or 0xFFFFFFFFFFFFFF */
    
/*  Virtual machine instruction argument modifier declarations */
#define     AMGVOFFSET              0
#define     AMSVOFFSET              1
#define     AMAVOFFSET              2
#define     AMTVOFFSET              3
#define     AMPVOFFSET              4
#define     AMCVOFFSET              5
#define     AMREGISTER	            6
#define     AMR07OFFST	            7
#define     AMR08OFFST	            8
#define     AMR09OFFST	            9
#define     AMR10OFFST	            10
#define     AMR11OFFST	            11
#define     AMR12OFFST	            12
#define     AMR13OFFST	            13
#define     AMR14OFFST	            14
#define     AMR15OFFST	            15
#define     AMR16OFFST	            16
#define     AMR17OFFST	            17
#define     AMR18OFFST	            18
#define     AMR19OFFST	            19
#define     AMR20OFFST	            20
#define     AMR21OFFST	            21
#define     AMR22OFFST	            22
#define     AMR23OFFST	            23
#define     AMR24OFFST	            24
#define     AMR25OFFST	            25
#define     AMR26OFFST	            26
#define     AMR27OFFST	            27
#define     AMR28OFFST	            28
#define     AMR29OFFST	            29
#define     AMR30OFFST	            30
#define     AMR31OFFST	            31
#define     AMR32OFFST	            32
#define     AMR33OFFST	            33
#define     AMR34OFFST	            34
#define     AMR35OFFST	            35
#define     AMR36OFFST	            36
#define     AMR37OFFST	            37
#define     AMR38OFFST	            38
#define     AMR39OFFST	            39
#define     AMR40OFFST	            40
#define     AMR41OFFST	            41
#define     AMR42OFFST	            42
#define     AMR43OFFST	            43
#define     AMR44OFFST	            44
#define     AMR45OFFST	            45
#define     AMR46OFFST	            46
#define     AMR47OFFST	            47
#define     AMR48OFFST	            48
#define     AMR49OFFST	            49
#define     MAXRGOFFST	            49

#define     AMINTEGER               50
#define     AMWORD                  51
#define     AMVOID                 127

/*  Virtual machine instruction argument modifier flags */
#define     FAMVOID                 0x01
#define     FAMRGOFFSET             0x02
#define     FAMREGISTER             0x04
#define     FAMINTEGER              0x08
#define     FAMALL                  0xFF
#define		FAMFAST					(FAMRGOFFSET | FAMREGISTER)

/*  Function declarations */
extern  TVAL		  TPcodeVector_AddNewValue	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL newValue);
extern  TVAL          TPcodeVector_Delete		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index);
extern  TVAL          TPcodeVector_GetCdr		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  NUM           TPcodeVector_GetMaxIndex  (LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  void          TPcodeVector_Init			(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL          TPcodeVector_Insert		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index, TVAL newValue);
extern  void		  TPcodeVector_IPcodeVector	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[],TVAL newCdr);
extern  TPcodeVector* TPcodeVector_New			(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL          TPcodeVector_SetCdr		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL newValue);
extern  TVAL          TPcodeVector_SetMaxIndex  (LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM newRepeats);

/* Method to function conversions */

extern  void          TPcodeVector_ComputeSize  (LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM* aSize);
extern  TObject*      TPcodeVector_Copy         (LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  void          TPcodeVector_Doomed		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  TVAL          TPcodeVector_GetIV1		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1);
extern  TVAL          TPcodeVector_Load			(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern  TVAL          TPcodeVector_MakeNew		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  void          TPcodeVector_Mark			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  HMemory       TPcodeVector_Save         (LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory anHMemory);
extern  TVAL          TPcodeVector_SetIV1		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue);
extern  TVAL		  TPcodeVector_Print		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size, LpCHAR buf);

/*  The opcode structure format */
#ifdef _M32
typedef union {ULONG	Opcode;
				struct {unsigned	Pcode : 11;
						unsigned	Am1 : 7;
						unsigned	Am2 : 7;
						unsigned	Am3 : 7;
                      } u;
              } OPCODE, *LpOPCODE;
#else
#ifdef BIGENDIAN
typedef union {UNUM	Opcode;
               struct {SHORT	Pcode;
                       SHORT	Am1;
                       SHORT	Am2;
                       SHORT	Am3;
                      } u;
              } OPCODE, *LpOPCODE;
#else
typedef union {UNUM	Opcode;
               struct {SHORT	Am3;
                       SHORT	Am2;
                       SHORT	Am1;
                       SHORT	Pcode;
                      } u;
              } OPCODE, *LpOPCODE;
#endif
#endif
#endif
#endif 
