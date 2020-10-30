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

#define _C_FVMSCRIPT2
#define _SMARTBASE

#if 0
FVmScript2.c

Implementation of the Virtual Machine Procedure engine utility functions.

This source file contains the main utility functions for the VmScript 
Procedure object. A Procedure object contains a compiled VmScript formula. 
Adopted from Scheme and other dialects of Lisp, it is one of the main 
mechanisms for storing Lisp functions or scripts in SmartLisp.

PARENT:             None. 

AUTHORS:            Michael F. Korns

#endif

#include "fvmscpt2.h"
#include "fdebug.h"
#include "tdirect.h"
#include "terror.h"


extern  BOLE    FConio_sprintf      (LpXCONTEXT gCP, LpTHREAD gTP, LpCHAR dest,LpCHAR fmt, ...);


/*--------------------------------------------------------------------------------------- */
#if 0
FVmScript2_UpdateBindings

Update the value bindings in the Tv of the passed procedure object to contain the correct
values as extracted from the Fb given.
        
#endif

TVAL FVmScript2_UpdateBindings(LpXCONTEXT gCP,LpTHREAD gTP,TLambda* self,NUM argc,TVAL argv[], LpTVAL Fb)
{
NUM                     cn;
StartFrame
DeclareOBJ(TStructure,tmpEnv);
EndFrame

argc = argc; // NOOP to hide unused parameter warning message
if (self->TemporaryVariables != NIL)
    {
    tmpEnv = self->TemporaryVariables;
    for (cn = 0; cn < tmpEnv->itsMaxItemIndex; ++cn)
        {
        atHMBind(tmpEnv->itsDictionaryArray,cn).Value = Fb[cn];
        }
    }
    
if (self->ArgumentVariables != NIL)
    {
    tmpEnv = self->ArgumentVariables;
    for (cn = 0; cn < tmpEnv->itsMaxItemIndex; ++cn)
        {
        atHMBind(tmpEnv->itsDictionaryArray,cn).Value = argv[cn];
        }
    }
    

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
/*
FVmScript2_DebugManager

Make sure all cells and ranges referenced in the VmScript Procedure are 
established. This allows the VmScript emulator to assume that referenced
cells exist, which allows a faster emulator.
*/        
#endif

void FVmScript2_DebugManager(LpXCONTEXT gCP,	/*  Context Pointer */
							 LpTHREAD gTP,		/*  Thread Pointer */						 
							 TLambda* self,		/*  Procedure object being evaluated */
                             NUM argc,          /*  Count of Arguments passed to procedure */
                             TVAL argv[],       /*  Arguments passed to procedure */
                             TPcodeVector* Pc,  /*  Pcode vector register */
                             TStructure* Pv,	/*  Persistent variable register */
                             TStructure* Sv,	/*  Class variable register */
                             LpTVAL Rb,			/*  Register base register */
                             LpREAL Vs,	        /*  Vector Processing Number Stack */
                             NUM VsTopOfStack,	/*  Vector Processing Number Stack */
                             LpTVAL Fb,         /*  Frame base register */
                             LpNUM Ip,          /*  Instruction pointer */
                             BOLE nestedDbgSwt, /*  TRUE if nested debug break */
                             TVAL DbgError)     /*  Debug Error if nested debug break */
{
/*  VM DEBUGGER LOCAL VARIABLE DECLARATIONS */
/*  Note:   These variables should be kept to an absolute necessary */
/*          minimum, as they eat up large (approc 132 bytes) of C  */
/*          system stack space with each SmartLisp recursion.  */
TVAL                    iargument;              /*  Debugger temporary value */
TVAL                    iindex;                 /*  Debugger temporary value */
TVAL                    isource;                /*  Debugger temporary value */
NUM                     i;                      /*  Temporary index variable */
NUM                     n;                      /*  Temporary index variable */
NUM                     startIndex = 0;         /*  Temporary index variable */
NUM                     aCtr = 0;               /*  Temporary index variable */
NUM                     rowMax;                 /*  Maximum row counter */
NUM                     row;                    /*  Temporary row counter */
NUM                     oldRow;                 /*  Temporary previous row counter */
NUM                     nArgument;              /*  Debugger integer argument */
NUM                     nSource;                /*  Debugger integer argument */
NUM                     nLength;                /*  Debugger integer argument */
NUM                     nIndex;                 /*  Debugger integer argument */
NUM                     Instruction;            /*  Debugger instruction counter */
NUM                     SafeSI;                 /*  Debugger gc safe temp storage */
NUM                     Selection = 0;          /*  Debugger source line selection */
CHAR                    cmdLine[1024];           /*  Debugger command line string */
CHAR                    prompt[256];            /*  Debugger prompt line string */
CHAR                    title[256];             /*  Debugger window title string */
BOLE                    saveSwt;                /*  Debugger save error switch */
LpTVAL                  varp = NULL;            /*  Debugger variable bindings pointer */
LpCHAR                  tArgumentP[10];         /*  Debugger trace argument pointers */
CHAR                    nbuf[10];               /*  Debugger line number buffer */
LpCHAR                  Modifiers;              /*  Modifiers pointer for push */
LpCHAR                  sp;                     /*  Debugger Source pointer */
BOLE					aSameLambda = FALSE;
BOLE					aShowVars = FALSE;

StartFrame
DeclareTVAL(tmp);
DeclareTVAL(tmp1);
DeclareTVAL(tmp2);
DeclareTVAL(tmp3);
DeclareTVAL(breakList);
DeclareTVAL(err);
DeclareTVALArray(prmv,3);

EndFrame

argc = argc; // NOOP to hide unused parameter warning message
Pv = Pv; // NOOP to hide unused parameter warning message
/*  ===================================================================== */
/*  Here we manage the virtual machine's debugger and trace facility.     */
/*  ===================================================================== */

/*  Construct the command line for the debugDialog display. */
if (nestedDbgSwt)
    {
    Instruction = (Ip - &atHMInt(Pc->itsInstructionArray,0));
	strncpy(title,ErrorArray(DbgError),200);
    title[200] = '.';
    title[201] = '.';
    title[202] = '.';
    title[203] = '!';
    title[204] = 0;
    sprintf(prompt,"%s at ["INTFORMAT"]",title,Instruction);
    }
else
    {
	/*  Build the title for the debugDialog display. */
	iargument.Tag = self->itsObjectType;
	iargument.u.Object = (TObject*)self;
	isource = FDebug_GetLambdaBind(gCP,gTP,1,&iargument);
	if (isource.Tag == TYVOID) isource = TSTRING("..empty..");
	FConio_sprintf(gCP,gTP,title,"%a.In.Binding = %a",&iargument,&isource);

	/* Set up prompt line as: "Current Ip at [nnn] in Lambda foo = #<Lambda 1234>" */
    Instruction = (Ip - 1 - &atHMInt(Pc->itsInstructionArray,0));
    sprintf(prompt,"Current Ip at ["INTFORMAT"] in %s",Instruction,title);
    }

/*  Save gc protected space on the stack for temporary results. */
SafeSI = TopOfStack;
TopOfStack += 2;
saveSwt = gTP->TObject_ErrorSwt;
gTP->DebugSuspended = TRUE;

/*  Save the procedure's permanent variables into the global "pv" variable. */
TSymbol_MakeUnique(gCP,gTP,"pv")->itsGlobalValue = TOBJ(self->PersistantVariables);
/*  Save the procedure's permanent  constant variables into the global "cv" variable. */
TSymbol_MakeUnique(gCP,gTP,"cv")->itsGlobalValue = TOBJ(self->ConstantVariables);
/*  Save the procedure's Interface variables into the global "in" variable. */
TSymbol_MakeUnique(gCP,gTP,"in")->itsGlobalValue = TOBJ(self->Interfaces);

/*  Save the procedure's register variables into the global "rv" variable. */
if (self->RegisterVariables == NIL)
    {
    TSymbol_MakeUnique(gCP,gTP,"rv")->itsGlobalValue = TOBJ(self->RegisterVariables);
    }
else
    {
    tmp->u.Object = (TObject*)TStructure_Copy(gCP,gTP,TOBJ(self->RegisterVariables));
    tmp->Tag = tmp->u.Object->itsObjectType;
    TSymbol_MakeUnique(gCP,gTP,"rv")->itsGlobalValue = *tmp;
    
    for (n = 0; n < tmp->u.Structure->itsMaxItemIndex; n++)
        {
		if (atHMBind(tmp->u.Structure->itsDictionaryArray,n).Value.DeclaredType == TYREAL)
			{
			atHMBind(tmp->u.Structure->itsDictionaryArray,n).Value.Tag = TYREAL;
			atHMBind(tmp->u.Structure->itsDictionaryArray,n).Value.u.Real = Rb[n].u.Real;
			}
		else
			{
			atHMBind(tmp->u.Structure->itsDictionaryArray,n).Value.Tag = TYNUM;
			atHMBind(tmp->u.Structure->itsDictionaryArray,n).Value.u.Int = Rb[n].u.Int;
			}
        }

	/*  Save the procedure's internal vector processing number stack into the global "vs" variable. */
	if (VsTopOfStack > 0)
		{
		for (n = 1; n <= VsTopOfStack; n++)
			{
			prmv[0] = TSTRING("vs[");
			prmv[1] = TINT(n);
			prmv[2] = TSTRING("]");
			*tmp3 = FUtil2_Append(gCP,gTP,3,&prmv[0]);		
			*err = TStructure_AddNewValue(gCP,gTP,*tmp,*tmp3,TREAL(Vs[n]));
			}
		}
    }


/*  Save the procedure's temporary variables into the global "tv" variable. */
if (self->TemporaryVariables == NIL)
    {
    TSymbol_MakeUnique(gCP,gTP,"tv")->itsGlobalValue = TOBJ(self->TemporaryVariables);
    }
else
    {
    tmp->u.Object = (TObject*)TStructure_Copy(gCP,gTP,TOBJ(self->TemporaryVariables));
    tmp->Tag = tmp->u.Object->itsObjectType;
    TSymbol_MakeUnique(gCP,gTP,"tv")->itsGlobalValue = *tmp;
    
    for (n = 0; n < asStructure(tmp)->itsMaxItemIndex; n++)
        {
        atHMBind(asStructure(tmp)->itsDictionaryArray,n).Value = Fb[n]; 
        }
    }

/*  Save the procedure's  constant variables into the global "sv" variable. */
if (Sv == NIL) 
    {
    TSymbol_MakeUnique(gCP,gTP,"sv")->itsGlobalValue = gCP->Tval_VOID;
    }
else
    {
    TSymbol_MakeUnique(gCP,gTP,"sv")->itsGlobalValue.u.Structure = Sv;
    TSymbol_MakeUnique(gCP,gTP,"sv")->itsGlobalValue.Tag = TYSTRUCTURE;
    }

/*  Save the procedure's argument variables into the global "av" variable. */
if (self->ArgumentVariables == NIL)
    {
    TSymbol_MakeUnique(gCP,gTP,"av")->itsGlobalValue = TOBJ(self->ArgumentVariables);
    }
else
    {
    tmp->u.Object = (TObject*)TStructure_Copy(gCP,gTP,TOBJ(self->ArgumentVariables));
    tmp->Tag = tmp->u.Object->itsObjectType;
    TSymbol_MakeUnique(gCP,gTP,"av")->itsGlobalValue = *tmp;
    
    for (n = 0; n < asStructure(tmp)->itsMaxItemIndex; n++)
        {
        atHMBind(asStructure(tmp)->itsDictionaryArray,n).Value = argv[n]; 
        }
    }

/*  Disassemble the current procedure for the debugDialog display. */
DisassembleSource:
iargument.Tag = self->itsObjectType;
iargument.u.Object = (TObject*)self;

gTP->TvalStack[SafeSI] = TGVALUE("_debuggerDisassembly");

/* use previously constructed disassembly (already assigned to gTP->TvalStack[SafeSI] above) */
if (gTP->TvalStack[SafeSI].Tag != TYSYMBOL && (gTP->FVmscript2_OldSelf == self) && (gTP->FVmscript2_OldDisassemblyStyle == gTP->FVmscript2_DisassemblyStyle));
else
if (gTP->FVmscript2_DisassemblyStyle == 0)
	{
	/* We display either source or assembler as available. */
    gTP->TvalStack[SafeSI] = FSmartbase_Eval(gCP,gTP,TGVALUE("disassemble"),1,iargument,*tmp1);
	}
else
if (gTP->FVmscript2_DisassemblyStyle == 1)
	{
	/* We display only assembler. */
	*tmp1 = TSYMBOL("asm");
    gTP->TvalStack[SafeSI] = FSmartbase_Eval(gCP,gTP,TGVALUE("disassemble"),2,iargument,*tmp1);
	}
else
if (gTP->FVmscript2_DisassemblyStyle == 2)
	{
	/* We display only assembler. */
	*tmp1  = TSYMBOL("source");
    gTP->TvalStack[SafeSI] = FSmartbase_Eval(gCP,gTP,TGVALUE("disassemble"),2,iargument,*tmp1);
	}
    
FSmartbase_SetSymbolValue(gCP,gTP,"_debuggerDisassembly",gTP->TvalStack[SafeSI]);
gTP->TvalStack[SafeSI+1] = FSmartbase_Eval(gCP,gTP,TGVALUE("stringToVector"),3,gTP->TvalStack[SafeSI],TSTRING("\n\r"),TBOOL(TRUE));
if ((gTP->FVmscript2_OldSelf == self) && (gTP->FVmscript2_OldDisassemblyStyle == gTP->FVmscript2_DisassemblyStyle))
{
	// Need to do re-work on checking when to send a "same source" string to the client
	aSameLambda = FALSE;
}
gTP->FVmscript2_OldSelf = self;
gTP->FVmscript2_OldDisassemblyStyle = gTP->FVmscript2_DisassemblyStyle;

/*  Set the selection to the current instruction displacement.  */
/*  Note: We scan through the source lines until we find a line */
/*        matching our current displacement.                    */
if (gTP->TvalStack[SafeSI+1].Tag == TYVECTOR)
    {
	oldRow = row = 0;
    rowMax = asVector(&gTP->TvalStack[SafeSI+1])->itsMaxItemIndex;
    for (Selection = 0; Selection < rowMax; ++Selection)
        {
        *tmp = FSmartbase_Ref(gCP,gTP,2,gTP->TvalStack[SafeSI+1],TINT(Selection));
        if (tmp->Tag == TYTEXT)
            Modifiers = &tmp->u.Text[0];
        else
            Modifiers = (char*)*asString(tmp)->itsCString;
        
        /*  Copy displacement number display chars to buffer. */
		oldRow = row;
        strncpy(nbuf,&Modifiers[0],7);
        nbuf[6] = 0;
        FObject_atoi(gCP, gTP, &row, nbuf);
        if (Instruction < row) 
			{--Selection;
			// If we have the same source code, send "..same source.." to client.
			if (aSameLambda == TRUE)
				gTP->TvalStack[SafeSI+1] = FSmartbase_Eval(gCP,gTP,TGVALUE("stringToVector"),3,TSTRING("000000: ..same source..\n\r"),TSTRING("\n\r"),TBOOL(TRUE));
			if ((gTP->DebugFlags & _FSmartbase_TRACEON) && 
				(gTP->DebugBreakIP == -1) && 
				(gTP->DebugBreakCount == 0) && 
				(gTP->DebugBreakExp.Tag == TYVOID))
					{
					/* Note: If we are in instruction trace mode,  */
					/* make sure we trace to the next source line. */
					if (Instruction != oldRow) goto ResumeNextInstruction;
					}
			 goto ClientDebugDialog;
			}
        }
    }
--Selection; if (Selection < 0) Selection = 0;  
if ((gTP->DebugFlags & _FSmartbase_TRACEON) && 
	(gTP->DebugBreakIP == -1) && 
	(gTP->DebugBreakCount == 0) && 
	(gTP->DebugBreakExp.Tag == TYVOID))
		{
		/* Note: If we are in instruction trace mode,  */
		/* make sure we trace to the next source line. */
		if (Instruction != row) goto ResumeNextInstruction;
		}

if (aSameLambda == TRUE)
	gTP->TvalStack[SafeSI+1] = FSmartbase_Eval(gCP,gTP,TGVALUE("stringToVector"),3,TSTRING("000000: ..same source..\n\r"),TSTRING("\n\r"),TBOOL(TRUE));

/*  Communicate with the  client via the debugger dialog function. */
ClientDebugDialog:
*tmp1 = TSTRING(title);
*tmp2 = TSTRING(prompt);
gTP->TvalStack[SafeSI] = FSmartbase_Eval(gCP,gTP,TGVALUE("debugDialog"),5,
											*tmp1,
											*tmp2,
											gTP->TvalStack[SafeSI+1],
											TINT(Selection),
											TBOOL(FALSE));
                                
ClientDebugDialogReturn:
/*  Never quit unless the user presses the Cancel button or hits excape. */
if ((gTP->TvalStack[SafeSI].Tag != TYBOLE) || (gTP->TvalStack[SafeSI].u.Bool != FALSE))
    {
    /*  ===================================================================== */
    /*  Here we manage the virtual machine's debugger commands. */
    /*  ===================================================================== */
    FSmartbase_CnvToText(gCP,gTP,cmdLine,1023,gTP->TvalStack[SafeSI]);
    switch (cmdLine[0])
        {
        /*  "b" set the specified break point depending on arguments */
        case 'b':
            /*  Reset the trace flags to stop at the next instruction. */
            gTP->DebugBreakProc = self;
            gTP->DebugBreakIP = 0;
		    gTP->DebugBreakOpcode = VMRETURN;
            gTP->DebugBreakCount = 0;
            gTP->DebugBreakExp = gCP->Tval_VOID;

            /*  Are there arguments with this set break point command? */
            if (cmdLine[1] == ',')
                {
                /*  Isolate any set break point command arguments. */
                tArgumentP[0] = &cmdLine[2];
                if ((tArgumentP[1] = strchr(tArgumentP[0],',')) != NULL)
                    {
                    *tArgumentP[1] = 0;
                    if ((tArgumentP[2] = strchr(++tArgumentP[1],',')) != NULL)
                        {
                        *tArgumentP[2] = 0;
                        ++tArgumentP[2];
                        }
                    }
                
                /* Is the first argument a procedure name? */
                nArgument = 0;
                if ((tArgumentP[nArgument] != NULL) && ISALPHA((NUM)*tArgumentP[nArgument]))
                    {
                    *tmp = TGVALUE(tArgumentP[nArgument++]);
                    if (tmp->Tag == TYLAMBDA)
                        gTP->DebugBreakProc = tmp->u.Lambda;
                    else
                        goto UnknownCommand;
                    }
                else
                    gTP->DebugBreakProc = self;
                
                /* The next argument must be empty or an instruction displacement. */
                if ((tArgumentP[nArgument] != NULL) && ISDIGIT((NUM)*tArgumentP[nArgument]))
                    {
                    nSource = 0;
                    *tmp = FSmartbase_recNumber(gCP,gTP,tArgumentP[nArgument++],&nSource);
                    if (tmp->Tag == TYNUM)
                        {
                        gTP->DebugBreakIP = tmp->u.Int;
		                gTP->DebugBreakOpcode = VMRETURN;
                        }
                    else
                        goto UnknownCommand;
                    }
                    
				/* ********************************************************* */
				/* Sets a breakpoint in the specified Lambda at the           */
				/* specified instruction displacement in the Pcode Vector.   */
				/* ********************************************************* */
				if (gTP->DebugBreakProc->Interfaces == NIL) 
					{
					/* Make sure the Lambda.In slot holds a Structure object. */
					gTP->DebugBreakProc->Interfaces = TStructure_New(gCP,gTP);
					}
				*breakList = FSmartbase_Ref(gCP,gTP,2,gTP->DebugBreakProc->Interfaces,TOBJ(gCP->TLambda_BreakList));
				if (breakList->Tag != TYDIRECTORY)
					{
					/* Make sure the Lambda.In.breakList slot holds a Directory object. */
					breakList->u.Directory = TDirectory_New(gCP,gTP);
					breakList->Tag = TYDIRECTORY;
					*err = FSmartbase_Set(gCP,gTP,3,gTP->DebugBreakProc->Interfaces,TOBJ(gCP->TLambda_BreakList),*breakList);
					if (err->Tag != TYSTRUCTURE) goto UnknownCommand; 
					}
				/* Set the break point in the break list */
				*tmp = FSmartbase_Ref(gCP,gTP,2,*breakList,TINT(gTP->DebugBreakIP));
				if (tmp->Tag != TYVOID) goto ContinueSetBreakpoint; 
				*tmp = FSmartbase_Ref(gCP,gTP,2,gTP->DebugBreakProc->PcodeVector,TINT(gTP->DebugBreakIP));
				if (tmp->Tag != TYNUM) goto UnknownCommand; 
				*err = FSmartbase_Set(gCP,gTP,3,*breakList,TINT(gTP->DebugBreakIP),*tmp);
				if (err->Tag != TYDIRECTORY) goto UnknownCommand;
				tmp->u.Int = (VMDEBUGGER * 256 * 256 * 256);
				tmp->Tag = TYNUM; 
				*tmp = FSmartbase_Set(gCP,gTP,3,gTP->DebugBreakProc->PcodeVector,TINT(gTP->DebugBreakIP),*tmp);
				if (tmp->Tag != TYPCODEVECTOR) goto UnknownCommand; 
				
				/* Reset the debug flags to instruction trace off */
				ContinueSetBreakpoint: 
				gTP->DebugBreakProc = NULL;
                }
            else
				{
                goto UnknownCommand;
                }

            goto DisplayVariables;
            break;
            
        /*  "c" clear the specified break point depending on arguments */
        case 'c':
            /*  Reset the trace flags to stop at the next instruction. */
            gTP->DebugBreakProc = self;
            gTP->DebugBreakIP = 0;
		    gTP->DebugBreakOpcode = VMRETURN;
            gTP->DebugBreakCount = 0;
            gTP->DebugBreakExp = gCP->Tval_VOID;

            /*  Do we wish to clear break points from any Lambda in the system? */
            if ((cmdLine[1] == 'c') && (cmdLine[2] == 0))
                {
                /* Clear all break points from any Lambda in the system */   
				gTP->DebugBreakProc = NULL;
				gTP->DebugBreakIP = -1;
				*err = FDebug_ClearBreakpoints(gCP,gTP,gTP->DebugBreakProc,gTP->DebugBreakIP);
				
				gTP->DebugBreakProc = NULL;
				gTP->DebugBreakIP = -1;
				gTP->DebugBreakCount = 0;
				gTP->EngineStateChanged = TRUE;
                }
            else
            /*  Are there arguments with this clear break point command? */
            if (cmdLine[1] == ',')
                {
                /*  Isolate any set break point command arguments. */
                tArgumentP[0] = &cmdLine[2];
                if ((tArgumentP[1] = strchr(tArgumentP[0],',')) != NULL)
                    {
                    *tArgumentP[1] = 0;
                    if ((tArgumentP[2] = strchr(++tArgumentP[1],',')) != NULL)
                        {
                        *tArgumentP[2] = 0;
                        ++tArgumentP[2];
                        }
                    }
                
                /* Is the first argument a procedure name? */
                nArgument = 0;
                if ((tArgumentP[nArgument] != NULL) && ISALPHA((NUM)*tArgumentP[nArgument]))
                    {
                    *tmp = TGVALUE(tArgumentP[nArgument++]);
                    if (tmp->Tag == TYLAMBDA)
                        gTP->DebugBreakProc = tmp->u.Lambda;
                    else
                        goto UnknownCommand;
                    }
                else
                    gTP->DebugBreakProc = self;
                
                /* The next argument must be empty or an instruction displacement. */
                if ((tArgumentP[nArgument] != NULL) && ISDIGIT((NUM)*tArgumentP[nArgument]))
                    {
                    nSource = 0;
                    *tmp = FSmartbase_recNumber(gCP,gTP,tArgumentP[nArgument++],&nSource);
                    if (tmp->Tag == TYNUM)
                        {
                        gTP->DebugBreakIP = tmp->u.Int;
                        }
                    else
                        gTP->DebugBreakIP = -1;
                    }
                    
				*err = FDebug_ClearBreakpoints(gCP,gTP,gTP->DebugBreakProc,gTP->DebugBreakIP);
				
				gTP->DebugBreakProc = NULL;
				gTP->DebugBreakIP = -1;
				gTP->DebugBreakCount = 0;
				gTP->EngineStateChanged = TRUE;
                }
            else
				{
                /* Clear all break points in the current Lambda */   
				gTP->DebugBreakProc = self;
				gTP->DebugBreakIP = -1;
				*err = FDebug_ClearBreakpoints(gCP,gTP,gTP->DebugBreakProc,gTP->DebugBreakIP);
				
				gTP->DebugBreakProc = NULL;
				gTP->DebugBreakIP = -1;
				gTP->DebugBreakCount = 0;
				gTP->EngineStateChanged = TRUE;
                }

            goto DisplayVariables;
            break;
            
        /*  "d" displays the procedures assembly level instructions */
        case 'd':
            /*  Display either source or assembler instructions as the default */
            gTP->FVmscript2_DisassemblyStyle = 0;
            if (cmdLine[1] == 0)
                {
                /*  Display either source or assembler instructions */
                gTP->FVmscript2_DisassemblyStyle = 0;
                }
            else
            if ((cmdLine[1] == ',') && (cmdLine[2] == 'a') && (cmdLine[3] == 0))
                {
                /*  display assembler instructions only */
                gTP->FVmscript2_DisassemblyStyle = 1;
                }
            else
            if ((cmdLine[1] == ',') && (cmdLine[2] == 's') && (cmdLine[3] == 0))
                {
                /*  display source instructions only */
                gTP->FVmscript2_DisassemblyStyle = 2;
                }

            goto DisassembleSource;
           break;
        
        /*  "g" turns off instruction trace and continue execution */
        /*  Note: Make sure to close the debug dialog dialog box. */
        case 'g':
            /* Turn off instruction trace. */
            if (gTP->DebugFlags & _FSmartbase_TRACEON)
                gTP->DebugFlags ^= _FSmartbase_TRACEON;
			gTP->DebugTraceOn = FALSE;
            gTP->DebugBreakProc = NULL;
            gTP->DebugBreakIP = -1;
		    gTP->DebugBreakOpcode = VMRETURN;
            gTP->DebugBreakCount = 0;
            gTP->DebugBreakExp = gCP->Tval_VOID;
			gTP->EngineStateChanged = TRUE;
            goto ResumeNextInstruction;
            break;
        
        /*  "t" traces to the next instruction depending on arguments */
        case 't':
            /*  Reset the trace flags to stop at the next instruction. */
            gTP->DebugBreakProc = NULL;
            gTP->DebugBreakIP = -1;
		    gTP->DebugBreakOpcode = VMRETURN;
            gTP->DebugBreakCount = 0;
            gTP->DebugBreakExp = gCP->Tval_VOID;
            /*  Are there no further arguments with this trace command? */
            if (cmdLine[1] == 0)
                {
                /*  trace to the next instruction in any proc anywhere (step into). */
				gTP->DebugFlags |= _FSmartbase_TRACEON;
				gTP->DebugTraceOn = (gTP->DebugFlags & _FSmartbase_TRACEON)?TRUE:FALSE;
				gTP->EngineStateChanged = TRUE;
                goto ResumeNextInstruction;
                }
            else
            /*  Are there arguments with this trace command? */
            if (cmdLine[1] == ',')
                {
                /*  Isolate any trace command arguments. */
                tArgumentP[0] = &cmdLine[2];
                if ((tArgumentP[1] = strchr(tArgumentP[0],',')) != NULL)
                    {
                    *tArgumentP[1] = 0;
                    if ((tArgumentP[2] = strchr(++tArgumentP[1],',')) != NULL)
                        {
                        *tArgumentP[2] = 0;
                        ++tArgumentP[2];
                        }
                    }
                
                /* Is the first argument a procedure name? */
                nArgument = 0;
                if ((tArgumentP[nArgument] != NULL) && ISALPHA((NUM)*tArgumentP[nArgument]))
                    {
                    *tmp = TGVALUE(tArgumentP[nArgument++]);
                    if (tmp->Tag == TYLAMBDA)
                        gTP->DebugBreakProc = tmp->u.Lambda;
                    else
                        goto UnknownCommand;
                    }
                else
                    gTP->DebugBreakProc = self;
                
                /* The next argument must be empty or an instruction displacement. */
                if ((tArgumentP[nArgument] != NULL) && ISDIGIT((NUM)*tArgumentP[nArgument]))
                    {
                    nSource = 0;
                    *tmp = FSmartbase_recNumber(gCP,gTP,tArgumentP[nArgument++],&nSource);
                    if (tmp->Tag == TYNUM)
                        {
                        gTP->DebugBreakIP = tmp->u.Int;
		                gTP->DebugBreakOpcode = VMRETURN;
                        }
                    else
                        goto UnknownCommand;
                    }
                    
                /* The next argument must be empty or an instruction count. */
                if ((tArgumentP[nArgument] != NULL) && ISDIGIT((NUM)*tArgumentP[nArgument]))
                    {
                    nSource = 0;
                    *tmp = FSmartbase_recNumber(gCP,gTP,tArgumentP[nArgument++],&nSource);
                    if (tmp->Tag == TYNUM)
                        gTP->DebugBreakCount = tmp->u.Int;
                    else
                        goto UnknownCommand;
                   }

				/*  trace to the specified instruction in the specified proc until after the specified count (if any). */
				gTP->DebugFlags |= _FSmartbase_TRACEON;
				gTP->DebugTraceOn = (gTP->DebugFlags & _FSmartbase_TRACEON)?TRUE:FALSE;
				gTP->EngineStateChanged = TRUE;
				goto ResumeNextInstruction;
                }
            else
            /*  trace until the expression is true? */
            if (cmdLine[1] == '=')
                {
                gTP->TvalStack[SafeSI] = TSTRING(&cmdLine[2]);
                gTP->TvalStack[SafeSI] =  FLisp_Lisp(gCP,gTP,1,&gTP->TvalStack[SafeSI]);
                if (isERROR(&gTP->TvalStack[SafeSI])) goto UnknownCommand;
                gTP->TvalStack[SafeSI] =  FCompile_Compile(gCP,gTP,1,&gTP->TvalStack[SafeSI]);
                if (isERROR(&gTP->TvalStack[SafeSI])) goto UnknownCommand;
                gTP->DebugBreakExp = gTP->TvalStack[SafeSI];
				gTP->DebugFlags |= _FSmartbase_TRACEON;
				gTP->DebugTraceOn = (gTP->DebugFlags & _FSmartbase_TRACEON)?TRUE:FALSE;
				gTP->EngineStateChanged = TRUE;
                goto ResumeNextInstruction;
                }
            else
                goto UnknownCommand;
            break;
            
        /*  "v" displays the procedures current variable bindings */
        case 'v':
            gTP->TvalStack[SafeSI] = TSTRING(""); 
			i = 0;
			aCtr = 1;
            /*  Display the procedures current variable bindings. */
            if (cmdLine[aCtr] == 0)
                {
				ShowRequestedVariables:
				startIndex = -2;
				gCP->FDebug_ShowRegVars = TRUE;
            	gTP->FVmscript2_VariablesToShow = 5;
            	for(i = startIndex; i < gTP->FVmscript2_VariablesToShow; i++)
                	{
					aShowVars = FALSE;
                	/*  Select correct display prefix. */
                	switch (i)
                    	{
                    	case -2: /* Interfaces .. always show */
                        	iargument = TOBJ(((TLambda*)self)->Interfaces);
                        	iindex = TSTRING("In.");
							aShowVars = gCP->FDebug_ShowInterfaceVars;
                        	break;
                    	case -1: /* Class variables .. always show */
                        	iargument = TOBJ(((TLambda*)self)->ClassVariables);
                        	iindex = TSTRING("Sv.");
							aShowVars = gCP->FDebug_ShowClassVars;
                        	break;
                    	case 0: /* Register variables .. always show */
                        	iargument = TGVALUE("rv");
                        	iindex = TSTRING("rv.");
							aShowVars = gCP->FDebug_ShowRegVars;
                        	break;
                    	case 1: /* Argument variables */ 
                        	iargument = TOBJ(((TLambda*)self)->ArgumentVariables);
                        	iindex = TSTRING("av.");
							aShowVars = gCP->FDebug_ShowArgVars;
                        	break;
                    	case 2: /* Temporary variables */
                        	iargument = TOBJ(((TLambda*)self)->TemporaryVariables); 
                        	iindex = TSTRING("tv."); 
							aShowVars = gCP->FDebug_ShowTempVars;
                        	break;
                    	case 3: /* Persistant Variables */
                        	iargument = TOBJ(((TLambda*)self)->PersistantVariables);
                        	iindex = TSTRING("pv."); 
							aShowVars = gCP->FDebug_ShowPersistentVars;
                        	break;
                    	case 4: /* Constant Variables */
                        	iargument = TOBJ(((TLambda*)self)->ConstantVariables);
                        	iindex = TSTRING("cv."); 
							aShowVars = gCP->FDebug_ShowConstVars;
                        	break;
                    	}

                	/*  Build the variable = binding displays for each variable. */
                	if (aShowVars == TRUE && iargument.Tag == TYSTRUCTURE)
                    	{
                    	for(n = 0; n < asStructure(&iargument)->itsMaxItemIndex; n++)
                        	{
                        	/*  Select correct variable binding pointer. */
                        	switch (i)
                            	{
                            	case -2: /* Class variables */
                                	varp = &atHMBind(asStructure(&iargument)->itsDictionaryArray,n).Value; 
                                	break;
                            	case -1: /* Interfaces */
                                	varp = &atHMBind(asStructure(&iargument)->itsDictionaryArray,n).Value; 
                                	break;
                            	case 0: /* Register variables */
                                	varp = &atHMBind(asStructure(&iargument)->itsDictionaryArray,n).Value; 
                                	break;
                            	case 1: /* Argument variables */
                                	varp = &argv[n]; 
                                	break;
                            	case 2: /* Temporary variables */
                                	varp = &Fb[n]; 
                                	break;
                            	case 3: /* Persistant variables */
                                	varp = &atHMBind(asStructure(&iargument)->itsDictionaryArray,n).Value; 
                                	break;
                            	case 4: /* Class variables */
                                	varp = &atHMBind(asStructure(&iargument)->itsDictionaryArray,n).Value; 
                                	break;
                            	}
 
                        	/*  Make sure no line is too long. */

							*tmp2 = FSmartbase_Eval(gCP,gTP,TGVALUE("append"),2,TSTRING(" = "),*varp);
							nLength = FSmartbase_StringLen(gCP,gTP,tmp2);
							if (nLength > 100)
								{
								*tmp2 = FSmartbase_Eval(gCP,gTP,TGVALUE("mid"),3,*tmp2,TINT(0),TINT(100));
								*tmp2 = FSmartbase_Eval(gCP,gTP,TGVALUE("append"),2,*tmp2,TSTRING("..."));
								}

							/*  Make sure no line contains delimiter characters. */

							nLength = FSmartbase_StringLen(gCP,gTP,tmp2);
							if (tmp2->Tag == TYTEXT)
								sp = &tmp2->u.Text[0];
							else
								sp = FSmartbase_VectorPtr(gCP,gTP,*tmp2);

							for (nIndex = 0; nIndex < nLength; ++nIndex)
								{
								if (sp[nIndex] < 32) sp[nIndex] = 1;
								}

                        	/*  Construct the variable = binding display. */

                        	asObject(&isource) = (TObject*)atHMBind(asStructure(&iargument)->itsDictionaryArray,n).Key;
                        	asTag(&isource) = asObject(&isource)->itsObjectType;
                        	gTP->TvalStack[SafeSI] = FSmartbase_Eval(gCP,gTP,TGVALUE("append"),5,gTP->TvalStack[SafeSI],iindex,isource,*tmp2,TSTRING("\r"));
							}
                    	}
                	}
            	gTP->TvalStack[SafeSI+1] = FSmartbase_Eval(gCP,gTP,TGVALUE("stringToVector"),3,gTP->TvalStack[SafeSI],TSTRING("\n\r"),TBOOL(TRUE));
				// Leave the switch and send the list of variables to the client
				break;
                }
            /*  Display all of the procedures current variable bindings. */
            if ((cmdLine[aCtr] == ',') && (cmdLine[aCtr+1] == 'v'))
                {
                gTP->FVmscript2_VariablesToShow = 5;
				startIndex = -2;
                goto DisplayVariables;
                }
            else
            /*  Display the procedures argument variable bindings. */
            if ((cmdLine[aCtr] == ',') && (cmdLine[aCtr+1] == 'a'))
                {
                gTP->FVmscript2_VariablesToShow = 2;
				startIndex = 1;
                goto DisplayVariables;
                }
            else
            /*  Display the procedures temporary variable bindings. */
            if ((cmdLine[aCtr] == ',') && (cmdLine[aCtr+1] == 't'))
                {
                gTP->FVmscript2_VariablesToShow = 3;
				startIndex = 2;
                goto DisplayVariables;
                }
            else
            /*  Display the procedures persistant variable bindings. */
            if ((cmdLine[aCtr] == ',') && (cmdLine[aCtr+1] == 'p'))
                {
                gTP->FVmscript2_VariablesToShow = 4;
				startIndex = 3;
                goto DisplayVariables;
                }
            else
            /*  Display the procedures constant variable bindings. */
            if ((cmdLine[aCtr] == ',') && (cmdLine[aCtr+1] == 'c'))
                {
                gTP->FVmscript2_VariablesToShow = 5;
				startIndex = 4;
                goto DisplayVariables;
                }
            else
            /*  Display the procedures interface variable bindings. */
            if ((cmdLine[aCtr] == ',') && (cmdLine[aCtr+1] == 'i'))
                {
                gTP->FVmscript2_VariablesToShow = -1;
				startIndex = -2;
                goto DisplayVariables;
                }
            else
            /*  Display the procedures class variable bindings. */
            if ((cmdLine[aCtr] == ',') && (cmdLine[aCtr+1] == 's'))
                {
                gTP->FVmscript2_VariablesToShow = 0;
				startIndex = -1;
                goto DisplayVariables;
                }
            else
            /*  Display the procedures register variable bindings. */
            if ((cmdLine[aCtr] == ',') && (cmdLine[aCtr+1] == 'r'))
                {
                gTP->FVmscript2_VariablesToShow = 1;
				startIndex = 0;
                goto DisplayVariables;
                }
            else
                goto UnknownCommand;

			DisplayVariables:
            for(i = startIndex; i < gTP->FVmscript2_VariablesToShow; i++)
                {
                /*  Select correct display prefix. */
                switch (i)
                    {
                    case -2: /* Interfaces .. always show */
                        iargument = TOBJ(((TLambda*)self)->Interfaces);
                        iindex = TSTRING("In.");
                        break;
                    case -1: /* Class variables .. always show */
                        iargument = TOBJ(((TLambda*)self)->ClassVariables);
                        iindex = TSTRING("Sv.");
                        break;
                    case 0: /* Register variables .. always show */
                        iargument = TGVALUE("rv");
                        iindex = TSTRING("rv.");
                        break;
                    case 1: /* Argument variables */ 
                        iargument = TOBJ(((TLambda*)self)->ArgumentVariables);
                        iindex = TSTRING("av.");
                        break;
                    case 2: /* Temporary variables */
                        iargument = TOBJ(((TLambda*)self)->TemporaryVariables); 
                        iindex = TSTRING("tv."); 
                        break;
                    case 3: /* Persistant Variables */
                        iargument = TOBJ(((TLambda*)self)->PersistantVariables);
                        iindex = TSTRING("pv."); 
                        break;
                    case 4: /* Constant Variables */
                        iargument = TOBJ(((TLambda*)self)->ConstantVariables);
                        iindex = TSTRING("cv."); 
                        break;
                    }
                    
                /*  Build the variable = binding displays for each variable. */
                if (iargument.Tag == TYSTRUCTURE)
                    {
                    for(n = 0; n < asStructure(&iargument)->itsMaxItemIndex; n++)
                        {
                        /*  Select correct variable binding pointer. */
                        switch (i)
                            {
                            case -2: /* Class variables */
                                varp = &atHMBind(asStructure(&iargument)->itsDictionaryArray,n).Value; 
                                break;
                            case -1: /* Interfaces */
                                varp = &atHMBind(asStructure(&iargument)->itsDictionaryArray,n).Value; 
                                break;
                            case 0: /* Register variables */
                                varp = &atHMBind(asStructure(&iargument)->itsDictionaryArray,n).Value; 
                                break;
                            case 1: /* Argument variables */
                                varp = &argv[n]; 
                                break;
                            case 2: /* Temporary variables */
                                varp = &Fb[n]; 
                                break;
                            case 3: /* Persistant variables */
                                varp = &atHMBind(asStructure(&iargument)->itsDictionaryArray,n).Value; 
                                break;
                            case 4: /* Class variables */
                                varp = &atHMBind(asStructure(&iargument)->itsDictionaryArray,n).Value; 
                                break;
                            }
                    
                        /*  Make sure no line is too long. */
    
						*tmp2 = FSmartbase_Eval(gCP,gTP,TGVALUE("append"),2,TSTRING(" = "),*varp);
						nLength = FSmartbase_StringLen(gCP,gTP,tmp2);
						if (nLength > 100)
							{
							*tmp2 = FSmartbase_Eval(gCP,gTP,TGVALUE("mid"),3,*tmp2,TINT(0),TINT(100));
							*tmp2 = FSmartbase_Eval(gCP,gTP,TGVALUE("append"),2,*tmp2,TSTRING("..."));
							}
 
						/*  Make sure no line contains delimiter characters. */
    
						nLength = FSmartbase_StringLen(gCP,gTP,tmp2);
						if (tmp2->Tag == TYTEXT)
							sp = &tmp2->u.Text[0];
						else
							sp = FSmartbase_VectorPtr(gCP,gTP,*tmp2);

						for (nIndex = 0; nIndex < nLength; ++nIndex)
							{
							if (sp[nIndex] < 32) sp[nIndex] = 1;
							}
						
                        /*  Construct the variable = binding display. */
    
                        asObject(&isource) = (TObject*)atHMBind(asStructure(&iargument)->itsDictionaryArray,n).Key;
                        asTag(&isource) = asObject(&isource)->itsObjectType;
                        gTP->TvalStack[SafeSI] = FSmartbase_Eval(gCP,gTP,TGVALUE("append"),5,gTP->TvalStack[SafeSI],iindex,isource,*tmp2,TSTRING("\r"));
						}
                    }
                }
            gTP->TvalStack[SafeSI+1] = FSmartbase_Eval(gCP,gTP,TGVALUE("stringToVector"),3,gTP->TvalStack[SafeSI],TSTRING("\n\r"),TBOOL(TRUE));
            break;
    
        /*  "s" sets the group of variable listing to be sent to the client */
        case 's':
			if( cmdLine[1] == '=' )
			{
				aShowVars = FALSE;
            	gTP->TvalStack[SafeSI] = TSTRING(""); 
				gCP->FDebug_ShowRegVars = FALSE;
				gCP->FDebug_ShowTempVars = FALSE;
				gCP->FDebug_ShowArgVars = FALSE;
				gCP->FDebug_ShowClassVars = FALSE;
				gCP->FDebug_ShowConstVars = FALSE;
				gCP->FDebug_ShowInterfaceVars = FALSE;
				gCP->FDebug_ShowPersistentVars = FALSE;
				aCtr = 2;
				while(cmdLine[aCtr] != 0)
				{
					switch(cmdLine[aCtr])
					{
					case 'r':
                		gCP->FDebug_ShowRegVars = TRUE;
					break;
					case 't':
                		gCP->FDebug_ShowTempVars = TRUE;
					break;
					case 'a':
                		gCP->FDebug_ShowArgVars = TRUE;
					break;
					case 's':
                		gCP->FDebug_ShowClassVars = TRUE;
					break;
					case 'c':
                		gCP->FDebug_ShowConstVars = TRUE;
					break;
					case 'i':
                		gCP->FDebug_ShowInterfaceVars = TRUE;
					break;
					case 'p':
                		gCP->FDebug_ShowPersistentVars = TRUE;
					break;
					case 'v':
                		aShowVars = TRUE;
					break;
					}
					aCtr++;
				}
				if( aShowVars == TRUE )
					goto ShowRequestedVariables;
			}
            break;

        /*  "x" traces to next instruction in current procedure (step over) */
        case 'x':
            gTP->DebugBreakProc = self;
            gTP->DebugBreakIP = -1;
			gTP->DebugFlags |= _FSmartbase_TRACEON;
			gTP->DebugTraceOn = (gTP->DebugFlags & _FSmartbase_TRACEON)?TRUE:FALSE;
			gTP->EngineStateChanged = TRUE;
		    gTP->DebugBreakOpcode = VMRETURN;
            gTP->DebugBreakCount = 0;
            gTP->DebugBreakExp = gCP->Tval_VOID;
            goto ResumeNextInstruction;
            break;
            
        /*  "=" evaluates the expression following the = symbol */
        case '=':
            gTP->TvalStack[SafeSI] = FSmartbase_Evals(gCP,gTP,&cmdLine[1],FALSE);
            if ((gTP->TvalStack[SafeSI].Tag != TYTEXT) && (gTP->TvalStack[SafeSI].Tag != TYSTRING) && (gTP->TvalStack[SafeSI].Tag != TYSYMBOL))
                gTP->TvalStack[SafeSI] = FSmartbase_Eval(gCP,gTP,TGVALUE("string"),1,gTP->TvalStack[SafeSI]);
            gTP->TvalStack[SafeSI+1] = FSmartbase_Eval(gCP,gTP,TGVALUE("stringToVector"),3,gTP->TvalStack[SafeSI],TSTRING("\n\r"),TBOOL(TRUE));
            *tmp1 = TSTRING(title);
			*tmp2 = TSTRING("");
			gTP->TvalStack[SafeSI] = FSmartbase_Eval(gCP,gTP,TGVALUE("debugDialog"),5,
														*tmp1,
														*tmp2,
														gTP->TvalStack[SafeSI+1],
														TINT(0),
														TBOOL(FALSE));
            goto ClientDebugDialogReturn;
            break;
            
        /*  "p" displays debug browsable procedure list. */
        case 'p':
            gTP->TvalStack[SafeSI] = FSmartbase_Eval(gCP,gTP,TGVALUE("debugBrowsableProcs"),0);
            if (gTP->TvalStack[SafeSI].Tag == TYVOID)
				{
				gTP->TvalStack[SafeSI] = TSTRING("No Unlocked Lambdas Active");
				}
			gTP->TvalStack[SafeSI+1] = FSmartbase_Eval(gCP,gTP,TGVALUE("stringToVector"),3,gTP->TvalStack[SafeSI],TSTRING("\n\r"),TBOOL(TRUE));
            break;
    
        /*  "?" displays debug dialog help information. */
        case '?':
            gTP->TvalStack[SafeSI] = TSTRING(" ? Valid AIS debugger query expressions:\r b,instr Set break point at the specified instruction (displacement).\r b,Lambda Set break point at the specified Lambda.\r b,Lambda,instr Set break point at the specified instruction (displacement).\r c Clear all break points in the current Lambda.\r cc Clear all break points in any Lambda anywhere in the system.\r c,Lambda Clear all break points in the specified Lambda.\r c,Lambda,instr Clear the break point in the specified Lambda ***at the specified instruction (displacement).\r d Display only source instructions (if available otherwise ***display DRM assembler instructions).\r d,a Display DRM assembler instructions.\r d,s Display only source instructions.\r g Turn debugger instruction-trace-mode off and resume execution.\r p Display unlocked active Lambdas currently in memory.\r t Trace one instruction (step into).\r t,instr Trace to the specified instruction (displacement).\r t,Lambda Trace until reaching the specified Lambda.\r t,Lambda,instr Trace until reaching the specified instruction (displacement).\r t,Lambda,instr,count Trace until reaching the specified ***instruction (displacement) the specified number of times (count).\r t=expr Trace instructions until the expression is true (only global ***variables references are allowed).\r v Return string containing all current variables.\r v,a Return string containing only argument variables.\r v,t Return string containing only temporary variables.\r v,p Return string containing only persistant variables.\r v,c Return string containing only constant variables.\r v,v Return string containing all current variables.\r x Trace one instruction (step over).\r =expr Return the evaluated Lisp expression as a string (only global ***variable references allowed).\r q Quit the debugger");
            gTP->TvalStack[SafeSI+1] = FSmartbase_Eval(gCP,gTP,TGVALUE("stringToVector"),3,gTP->TvalStack[SafeSI],TSTRING("\n\r"),TBOOL(TRUE));
            break;
    
        /*  "q" quits the debugger. */
        case 'q':
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);
            break;
    
        /*  If we don't recognize the command, then display what? */
        UnknownCommand:
        default:
            *tmp1 = TSTRING(title);
			*tmp2 = TSTRING("what?");
			gTP->TvalStack[SafeSI] = FSmartbase_Eval(gCP,gTP,TGVALUE("debugDialog"),5,
														*tmp1,
														*tmp2,
														gTP->TvalStack[SafeSI+1],
														TINT(0),
														TBOOL(FALSE));
            goto ClientDebugDialogReturn;
            break;
        }
    
    Selection = 0;
    goto ClientDebugDialog;
    }
else
    /*  If the user presses the cancel button or hits escape we must throw */
    {
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);
    }

/*  Return gc safe temporary storage to the stack. */
ResumeNextInstruction:
/* The debugger client must be notified of any change in the engine debug flags. */
if (gTP->EngineStateChanged == TRUE)
	{
	FSmartbase_Eval(gCP,gTP,TGVALUE("debugDialog"),0,gCP->Tval_VOID);
	}
TopOfStack = SafeSI;
gTP->TObject_ErrorSwt = saveSwt;
gTP->DebugSuspended = FALSE;
/* Check for a client escape request. */
if ((*gCP->_Host_Escape)((POINTER)gCP,gTP)) 
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

FrameReturn

}
