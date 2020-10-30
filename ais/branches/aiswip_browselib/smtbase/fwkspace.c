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

#define _C_FWORKSPACE
#define _SMARTBASE
#if 0
FWorkspace.c

Implementation of the procedure library which services the Workspace class.

AUTHORS:            Michael F. Korns

#endif

#include "fwkspace.h"
#include "fvmscpt.h"

/*  Define the new type variables for this Class.    */

/*  LISP GLOBAL OBJECT DECLARATIONS */

BOLE        FWorkspace_Initialized = FALSE;

/*--------------------------------------------------------------------------------------- */
#if 0
FWorkspace_Init

Initialize the Workspace procedure library.  

#endif

TVAL FWorkspace_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ec);
EndFrame
 

if (gCP->FWorkspace_Initialized == FALSE)
    {
    gCP->FWorkspace_Initialized = TRUE;
    
	*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"clear",(LpFUNC)&FWorkspace_Clear);
	ExitOnError(*ec);
   }

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FWorkspace_Clear


(gCP,gTP,clear)

#endif

TVAL FWorkspace_Clear(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM                     cn;
TVAL                    viewIndex;
TVAL                    workspaceTval;
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TObjVector,vp);
DeclareOBJ(TWorkspace,workspace);
DeclareOBJ(TObject,anObj);
DeclareTVAL(ret);
DeclareTVAL(indexProc);
DeclareTVALArray(prmv,4);

EndFrame

/* Invoke the doClear member function of the Lambda (if any). */

if ((argc == 1) && (argv[0].Tag == TYLAMBDA))
    {
    /* Invoke the doClear subfunction of the Lambda. */
    *indexProc = TLambda_GetPvMember(gCP,gTP,argv[0],"doClear");
    ExitOnError(*indexProc);
    *ret = FSmartbase_Evalv(gCP,gTP,*indexProc,0,&prmv[0]);
    FrameExit(*ret);
    }


/*  Clear the specified ObjectRepository. */

if ((argc == 1) && (argv[0].Tag == TYOBJREPOSITORY))
    {
	*ret = TDatabase_Clear(gCP,gTP,argc,argv);
    FrameExit(*ret);
    }
        
/*  Clear the specified NeuralNet. */

if ((argc >= 1) && isNeuralNet(argv[0]))
    {
	*ret = TNeuralNet_Clear(gCP,gTP,argc,argv);
    FrameExit(*ret);
    }
        
/*  Clear the current Workspace. */

if ((argc == 0) ||
    ((argc == 1) && (asTag(&argv[0]) == TYGLOBALS || asTag(&argv[0]) == TYWORKSPACE)))
    {
	/* Notify the client that the server is about to be reset. */
	FSmartbase_Evalv(gCP,gTP,TGVALUE("serverDoomed"),0,NULL);

	/* Delete each object, currently being viewed by the client, */
	/* from the client viewing list and from the Workspace. */
    workspaceTval = TGVALUE("_currentViews");
    if (workspaceTval.Tag == TYWORKSPACE)
        {
        workspace = ((TWorkspace*)asObject(&workspaceTval));
        viewIndex.Tag = TYNUM;
        viewIndex.u.Int = 0;
        while (workspace->itsMaxItemIndex > 0)
            {
            /*  We delete any Lambdas left in the workspace. */
            
            TWorkspace_DeleteView(gCP,gTP,workspaceTval, viewIndex);
            }
        }
        
	/*  Clear the global symbol table. Every symbol (which is not locked) has its */
	/*	global value set to #void. The next garbage collection will then clean up. */
	/*  Note:	To increase the speed of Symbol lookup, we use an alphabetically */
	/*			sorted symbol vector as a symbol table. */

    for (cn = 0; cn < gCP->TSymbol_SymbolTable_ActiveCount; ++cn)
        {
		aSymbol = (TSymbol*)atHMObject(gCP->TSymbol_SymbolTable->itsObjectArray,cn);

		if (TESTON)
			{
			if (!_VALIDOBJ(aSymbol) || 
				(aSymbol->itsObjectType != TYSYMBOL) ||
				((aSymbol->itsImmediatePtr == NULL) && ((aSymbol->itsCString == NULL) || (((LpMHANDLEINFO)aSymbol->itsCString)->Free == TRUE)))
				)
				{
				/* WARNING: We have detected a corrupted symbol table. */
				TSymbol_CheckSymbolTable(gCP,gTP);
				FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
				}
			}

        if (aSymbol->itsGlobalLock == 0)
            {
            aSymbol->itsGlobalValue = gCP->TObject_VOID;
            }
        }

    /*  Insure that all remaining open files are closed */
    
    (*gCP->_Host_Closef)((POINTER)gCP,gTP,-1, 1);
    TObject_MarkAndSweep(gCP,gTP);
    FrameExit(gCP->TObject_TRUE);
    }
    

FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}
