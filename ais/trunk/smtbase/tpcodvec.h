/**********************************************************************************
    Copyright (C) 2013 Analytic Research Foundation

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
6.0000      4/30/2013   mfk     Added a large number of unsigned integer instructions
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
#define     VMADDU						3
#define     VMAND						4
#define     VMAPPLY						5
#define     VMARGCOUNT					6
#define     VMARGFETCH					7
#define     VMCALL						8
#define     VMDIV						9
#define     VMDIVI						10
#define     VMDIVU						11
#define     VMDIVR						12
#define     VMDIVRI						13
#define     VMDIVRU						14
#define     VMIADD						15
#define     VMIDIV						16
#define     VMIDIVR						17
#define     VMIMUL						18
#define     VMISUB						19
#define     VMUADD						20
#define     VMUDIV						21
#define     VMUDIVR						22
#define     VMUMUL						23
#define     VMUSUB						24

#define     VMJMPEQ						25  /* Note: The conditional jump instructions must  */
#define     VMJMPLT						26  /*       be kept in this relative order so the   */
#define     VMJMPGT						27  /*       computations in FOptimize2_boolJmp will */
#define     VMJMPNE						28  /*       work properly.                          */
#define     VMJMPGE						29  /*                                               */
#define     VMJMPLE						30  /* Note: End of pcode order restrictions         */

#define     VMJUMP						31
#define     VMMOVE						32
#define     VMMUL						33
#define     VMMULI						34
#define     VMMULU						35
#define     VMNADD						36
#define     VMIAND						37
#define     VMIANDB						38
#define     VMNDIV						39
#define     VMNDIVR						40
#define     VMDEBUGGER					41
#define     VMNMUL						42
#define     VMIOR						43
#define     VMIORB						44
#define     VMNSUB						45
#define     VMIXOR						46
#define     VMIXORB						47
#define     VMONERROR					48
#define     VMOR						49
#define     VMPOP						50
#define     VMPUSH						51
#define     VMREF						52
#define     VMSELF						53
#define     VMSEND						54
#define     VMSET						55
#define     VMSHL						56
#define     VMSHR						57
#define     VMSUB						58
#define     VMSUBI						59
#define     VMSUBU						60
#define     VMXOR						61
										
#define     VMADDN						62
#define     VMDIVN						63
#define     VMMOVEI						64
#define     VMMOVEU						65
#define     VMMULN						66
#define     VMSUBN						67

#define     VMCADD						68
#define     VMCDIV						69
#define     VMMOVEN						70
#define     VMCMUL						71
#define     VMCSUB						72

											/* Extended strong typed instructions.           */
											/*       (New in Version 3.01)			         */
											/* These new instructions allow faster memory    */
											/* to memory computation at the expense of       */
											/* strong typing and loss of generality.         */
#define     VMREFTEXT					73
#define     VMREFSTRING					74
#define     VMSETSTRING					75
#define     VMREFSYMBOL					76
#define     VMREFVECTOR					77
#define     VMSETVECTOR					78
#define     VMREFSTRVALUE				79
#define     VMSETSTRVALUE				80
#define     VMREFSTRKEY					81
#define     VMSETSTRKEY					82
#define     VMREFDICVALUE				83
#define     VMSETDICVALUE				84
#define     VMREFDICKEY					85
#define     VMSETDICKEY					86
#define     VMREFDIRVALUE				87
#define     VMSETDIRVALUE				88
#define     VMREFDIRKEY					89
#define     VMSETDIRKEY					90
#define     VMREFBITVECTOR				91
#define     VMSETBITVECTOR				92
#define     VMREFBYTVECTOR				93
#define     VMSETBYTVECTOR				94
#define     VMREFPCDVECTOR				95
#define     VMSETPCDVECTOR				96
#define     VMREFOBJVECTOR				97
#define     VMSETOBJVECTOR				98
#define     VMREFINTVECTOR				99
#define     VMSETINTVECTOR				100
#define     VMREFNUMVECTOR				101
#define     VMSETNUMVECTOR				102
#define     VMREFFLTVECTOR				103
#define     VMSETFLTVECTOR				104
#define     VMREFMATRIX					105
#define     VMSETMATRIX					106
#define     VMREFNUMMATRIX				107
#define     VMSETNUMMATRIX				108
#define     VMTESTESCAPE				109

#define     vmnatAddInteger				110
#define     vmnatAddNumber  			111
#define     vmnatAndInteger				112
#define     vmnatDivInteger				113
#define     vmnatDivNumber				114
#define     vmnatDivrInteger			115  
#define     vmnatDivrNumber				116  

#define     vmnatJmpEQInteger			117 /* Note: The conditional jump instructions must  */		
#define     vmnatJmpLTInteger			118 /*       be kept in this relative order so the   */
#define     vmnatJmpGTInteger			119 /*       computations in FOptimize2_boolJmp will */
#define     vmnatJmpNEInteger			120 /*       work properly.                          */
#define     vmnatJmpGEInteger			121 /*                                               */
#define     vmnatJmpLEInteger			122 /* Note: End of pcode order restrictions         */

#define     vmnatJmpEQUInteger			123 /* Note: The conditional jump instructions must  */		
#define     vmnatJmpLTUInteger			124 /*       be kept in this relative order so the   */
#define     vmnatJmpGTUInteger			125 /*       computations in FOptimize2_boolJmp will */
#define     vmnatJmpNEUInteger			126 /*       work properly.                          */
#define     vmnatJmpGEUInteger			127 /*                                               */
#define     vmnatJmpLEUInteger			128 /* Note: End of pcode order restrictions         */

#define     vmnatJmpEQNumber			129 /* Note: The conditional jump instructions must  */
#define     vmnatJmpLTNumber			130 /*       be kept in this relative order so the   */
#define     vmnatJmpGTNumber			131 /*       computations in FOptimize2_boolJmp will */
#define     vmnatJmpNENumber			132 /*       work properly.                          */
#define     vmnatJmpGENumber			133 /*                                               */
#define     vmnatJmpLENumber			134 /* Note: End of pcode order restrictions         */

#define     vmnatMulInteger 			135  
#define     vmnatMulNumber				136  
#define     vmnatOrInteger				137
#define     vmnatLoadCharacter			138  
#define     vmnatLoadFloat				139
#define     vmnatLoadInteger			140
#define     vmnatLoadUInteger			141
#define     vmnatLoadLong				142
#define     vmnatLoadNumber				143
#define     vmnatLoadObject				144  
#define     vmnatLoadShort      		145  
#define     vmnatSaveCharacter   		146  
#define     vmnatSaveFloat		   		147  
#define     vmnatSaveInteger	   		148  
#define     vmnatSaveLong				149
#define     vmnatSaveNumber		   		150  
#define     vmnatSaveObject		   		151  
#define     vmnatSaveShort				152
#define     vmnatShlInteger				153
#define     vmnatShrInteger				154
#define     vmnatSubInteger				155  
#define     vmnatSubNumber				156  
#define     vmnatXorInteger				157

											/* Extended register to register instructions.	 */
											/*           (New in Version 4.00)			     */
											/* These new instructions allow faster register  */
											/* to register computation at the expense of     */
											/* strong typing and loss of generality.         */
											/* Note: The register to register instructions   */
											/*       are expressed in upper/lower case style */
											/*       to further differentiate them from the  */
											/*       memory to memory vm instructions.       */
#define     VMSTARTREGISTERINS			158

#define     vmregAddImmediate			158
#define     vmregAddInteger				159
#define     vmregAddNumber				160
#define     vmregAddPointer	     		161
#define     vmregAndImmediate			162  
#define     vmregAndInteger				163  
#define     vmregDivImmediate			164  
#define     vmregDivInteger				165  
#define     vmregDivNumber				166  
#define     vmregIncPointer				167  

#define     vmregJmpEQImmediate			168 /* Note: The conditional jump instructions must  */		
#define     vmregJmpLTImmediate			169 /*       be kept in this relative order so the   */
#define     vmregJmpGTImmediate			170 /*       computations in FOptimize2_boolJmp will */
#define     vmregJmpNEImmediate			171 /*       work properly.                          */
#define     vmregJmpGEImmediate			172 /*                                               */
#define     vmregJmpLEImmediate			173 /* Note: End of pcode order restrictions         */

#define     vmregJmpEQUImmediate		174 /* Note: The conditional jump instructions must  */		
#define     vmregJmpLTUImmediate		175 /*       be kept in this relative order so the   */
#define     vmregJmpGTUImmediate		176 /*       computations in FOptimize2_boolJmp will */
#define     vmregJmpNEUImmediate		177 /*       work properly.                          */
#define     vmregJmpGEUImmediate		178 /*                                               */
#define     vmregJmpLEUImmediate		179 /* Note: End of pcode order restrictions         */

#define     vmregJmpEQInteger			180 /* Note: The conditional jump instructions must  */		
#define     vmregJmpLTInteger			181 /*       be kept in this relative order so the   */
#define     vmregJmpGTInteger			182 /*       computations in FOptimize2_boolJmp will */
#define     vmregJmpNEInteger			183 /*       work properly.                          */
#define     vmregJmpGEInteger			184 /*                                               */
#define     vmregJmpLEInteger			185 /* Note: End of pcode order restrictions         */
 
#define     vmregJmpEQUInteger			186 /* Note: The conditional jump instructions must  */		
#define     vmregJmpLTUInteger			187 /*       be kept in this relative order so the   */
#define     vmregJmpGTUInteger			188 /*       computations in FOptimize2_boolJmp will */
#define     vmregJmpNEUInteger			189 /*       work properly.                          */
#define     vmregJmpGEUInteger			190 /*                                               */
#define     vmregJmpLEUInteger			191 /* Note: End of pcode order restrictions         */
 
#define     vmregJmpEQNumber			192 /* Note: The conditional jump instructions must  */		
#define     vmregJmpLTNumber			193 /*       be kept in this relative order so the   */
#define     vmregJmpGTNumber			194 /*       computations in FOptimize2_boolJmp will */
#define     vmregJmpNENumber			195 /*       work properly.                          */
#define     vmregJmpGENumber			196 /*                                               */
#define     vmregJmpLENumber			197 /* Note: End of pcode order restrictions         */
  
#define     vmregJump					198  
#define     vmregLoadAddress			199  
#define     vmregLoadDeclType			200
#define     vmregLoadInteger			201  
#define     vmregLoadJmpPointer			202
#define     vmregLoadNumber				203
#define     vmregLoadTail				204
#define     vmregLoadType				205
#define     vmregDivrImmediate			206
#define     vmregDivrInteger			207
#define     vmregDivrNumber				208  
#define     vmregMoveImmediate			209  
#define     vmregMoveInteger			210  
#define     vmregMoveNumber				211  
#define     vmregMulImmediate			212  
#define     vmregMulInteger				213  
#define     vmregMulNumber				214  
#define     vmregObjLength				215
#define     vmregObjPointer				216
#define     vmregOrImmediate			217
#define     vmregOrInteger				218
#define     vmregRefCharacter			219
#define     vmregRefFloat				220  
#define     vmregRefInteger				221  
#define		vmregRefLong				222
#define     vmregRefNumber				223  
#define     vmregRefShort				224  
#define     vmregRefWord				225
#define     vmregRefXCharacter			226  
#define     vmregRefXFloat				227 
#define     vmregRefXInteger			228
#define		vmregRefXLong				229
#define     vmregRefXNumber				230
#define     vmregRefXShort				231
#define     vmregRefXWord				232
#define		vmregRunInHardware			233
#define     vmregSaveDeclType			234
#define     vmregSaveDeclTypeImmediate	235  
#define     vmregSaveInteger			236
#define     vmregSaveUInteger			237
#define     vmregSaveNumber				238  
#define     vmregSaveTail				239
#define     vmregSaveTailImmediate		240  
#define     vmregSetCharacter			241  
#define     vmregSetCharImmediate		242
#define     vmregSetFloat				243  
#define     vmregSetInteger				244  
#define     vmregSetIntImmediate		245
#define		vmregSetLong				246
#define     vmregSetLongImmediate		247
#define     vmregSetNumber				248  
#define     vmregSetShort				249  
#define     vmregSetShortImmediate		250
#define     vmregSetWord				251
#define     vmregSetXCharacter			252
#define     vmregSetXCharImmediate		253
#define     vmregSetXFloat				254
#define     vmregSetXInteger			255
#define     vmregSetXIntImmediate		256
#define		vmregSetXLong				257
#define     vmregSetXLongImmediate		258
#define     vmregSetXNumber				259
#define     vmregSetXShort				260  
#define     vmregSetXShortImmediate		261
#define     vmregSetXWord				262
#define     vmregShlImmediate			263  
#define     vmregShlInteger				264  
#define     vmregShrImmediate			265  
#define     vmregShrInteger				266  
#define     vmregStringCompare			267  
#define     vmregStringiCompare			268 
#define     vmregSubImmediate			269 
#define     vmregSubInteger				270
#define     vmregSubNumber				271
#define     vmregSubPointer				272
#define     vmregXorImmediate			273  
#define     vmregXorInteger				274  

#define     vmregAbsNumber				275
#define     vmregLogNumber				276
#define     vmregPwrNumber				277
#define     vmregSqrtNumber				278
#define     vmregSinNumber				279
#define     vmregCosNumber				280
#define     vmregTanNumber				281  

#define     vmvecBinary					282  
#define     vmvecInitialize				283  
#define     vmvecLoop					284  
#define     vmvecNumScalar				285  
#define     vmvecNumVector				286
#define     vmvecPop					287  
#define     vmvecPopNumber				288  
#define     vmvecPush					289  
#define     vmvecPushNumber				290  
#define     vmvecSetIncrements			291  
#define     vmvecSetPointers			292  
#define     vmvecSwapCC					293  
#define     vmvecUnary					294  
 
#define     vmregInteger				295
#define     vmregNumber					296

/*  MAXIMUM VM INSTRUCTIONS DECLARATION */
/*  This key macro is ALSO defined in "fsmtbase.h".	*/
/*  WARNING: Keep these two definitions identical	*/
#define     _VMMAXINSTRS				297		/* WARNING!!! Must set _VMMAXINSTRS in FSmartbase.h equal to this value!!! */

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
