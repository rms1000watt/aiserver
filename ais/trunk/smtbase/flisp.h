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
FLisp.h

This file contains some of the procedures required to support the SmartLisp "lisp" and "morph"
commands. These two procedures make up the preprocessing phase of the compiler.

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FLisp
#define _H_FLisp
#include "tcontin.h"
#include "tpair.h"
#include "tintvec.h"
#include "tobjvec.h"

/*  Macro definitions */
    
#define _FLisp_StackAlloc    32
#define _FLisp_CVRSize       32

#define _FLisp_PairsToArray(p, a, max)                                       \
{int cn;                                                                     \
for(a[0] = p, cn = 1; cn < max && asTag(&a[cn-1]->itsCdr) == TYPAIR; cn++)   \
     a[cn] = asPair(&a[cn-1]->itsCdr);                                       \
}

#define isLexMorphSymbol(a) (((asTag((LpTVAL)(a)) == TYSYMBOL) || (asTag((LpTVAL)(a)) == TYQUOTEDSYMBOL)) && (((LpTVAL)(a))->u.Object != NIL))
#define isLexMorphPair(a)   (((asTag((LpTVAL)(a)) == TYPAIR) || (asTag((LpTVAL)(a)) == TYQUOTEDPAIR)) && (((LpTVAL)(a))->u.Object != NIL))

/*  A macro is provided to tell whether a given type could be associated with a compound variable */
/*  reference. */

#define _FLisp_CVRType(type) (type == TYSYMBOL || type == TYPAIR || type == TYQUOTEDSYMBOL || \
type == TYQUOTEDPAIR )

/*  Debugger related macros */

#define DEBUGLINEBREAK				1 

#define _FLisp_MakeStructure	 -100
#define _FLisp_MakeList          -101
#define _FLisp_MakeVector        -102
#define _FLisp_FreeNode          -103
#define _FLisp_MakeDirectory     -104
#define _FLisp_MakeDictionary    -105
#define _FLisp_MakeMatrix        -106
#define _FLisp_MakeTypeVector    -107
#define _FLisp_MakeComplex       -108
#define _FLisp_MakeBrick         -109
#define _FLisp_MakeBrickInit     -110
#define _FLisp_MakeBrickList     -111
#define _FLisp_MakeBrickFinal    -112
#define _FLisp_MakeTemplate      -113

#define _FLisp_TextChunk         (NUM)(MAXAISWORDTEXTLEN-1)

/*--------------------------------------------------------------------------------------- */
#if 0
MACROS for FLisp_Lisp.

#endif

/*  A macro is provided to scan forward in a text stream to get to the next non-white space character. */

#define _FLisp_NextNWSChar(pSrc, pCh)    \
{                                       \
    LpCHAR  pTmp = (LpCHAR) pSrc;       \
    while(ISSPACE(*pCh = *pTmp++)) {}   \
}

/*  A macro is provided to format a TVAL with either a symbol or a quoted symbol. */

#define _FLisp_MakeSymbolTval(lpt, sym, nQuotes)         \
{                                                       \
    asTag(lpt) = nQuotes ? TYQUOTEDSYMBOL : TYSYMBOL;   \
    asQuoteCnt(lpt) = (unsigned char)nQuotes;           \
    asObject(lpt) = sym;                                \
}

/*  A macro is provided to examine a list and see if it needs to be extended to create an available */
/*  car. */

#define _FLisp_GetFreeNode(list)            \
if (asTag(&list->itsCar) != TYLEX)          \
{											\
    list->itsCdr = gCP->Tval_VOID;         \
    asObject(&list->itsCdr) = (TObject*)TPair_New(gCP,gTP);\
    asTag(&list->itsCdr) = TYPAIR;          \
    list = (TPair*)asObject(&list->itsCdr); \
    }

/*  A macro is provided to examine add an atom to a list */

#define _FLisp_AddAtom(stackNdx, list, atom) \
_FLisp_GetFreeNode(list)                     \
list->itsCar = atom;                

/*  Rather than using recusive descent we emulate it by managing a stack of pointers into the */
/*  parse tree which we are constructing. This allows us to avoid the overhead of function calls. */

/*  A macro is provided to pop the stack which we use to manage the parse tree. */

#define _FLisp_Pop(stack, stackNdx, curPair)                    \
    if(--stackNdx < 0)  goto MatchParen;                        \
    if(asTag(&curPair->itsCar) == TYLEX)                        \
        curPair->itsCar = gCP->Tval_VOID;                      \
    curPair = (TPair*)atHMObject(stack->itsObjectArray,stackNdx);\
    if(asTag(&curPair->itsCar) == TYQUOTEDPAIR)                 \
        quoteCnt -= asQuoteCnt(&curPair->itsCar);
    
/*  A macro is provided to push into a new node in the parse tree. */

#define _FLisp_Push(stack, stackNdx, nQuotes, curPair)           \
    _FLisp_GetFreeNode(curPair)                                  \
    if(stackNdx + 1 == stack->itsMaxItemIndex)                  \
        FObject_SetMaxIndex(gCP,gTP,(TObject*)stack, stackNdx + 1 + _FLisp_StackAlloc);\
    atHMObject(stack->itsObjectArray,stackNdx) = (TObject*)curPair;\
    stackNdx++;                                                 \
    curPair->itsCar = gCP->Tval_VOID;                          \
    if(nQuotes)                                                 \
        {                                                       \
        asObject(&curPair->itsCar) = (TObject*)TPair_New(gCP,gTP);\
        asTag(&curPair->itsCar) = TYQUOTEDPAIR;                 \
        asQuoteCnt(&curPair->itsCar) = (unsigned char)nQuotes;  \
        }                                                       \
    else                                                        \
        {                                                       \
        asObject(&curPair->itsCar) = (TObject*)TPair_New(gCP,gTP);\
        asTag(&curPair->itsCar) = TYPAIR;                       \
        }                                                       \
    quoteCnt += nQuotes;                                        \
    curPair = (TPair*)asObject(&curPair->itsCar);               \
    asTag(&curPair->itsCar) = TYLEX;


/*--------------------------------------------------------------------------------------- */
#if 0
MACROS for FLisp_MorphList

#endif

/*  Rather than using recusive descent we emulate it by managing a stack of pointers into the */
/*  parse tree which we are traversing. This allows us to avoid the overhead of function calls. */

/*  A macro is provided to save our context as we descend into the parse tree to identify nodes */
/*  which require macro substitution. */

#define _FLisp_SaveContext(stack, stackNdx, curPair)         \
    if(stackNdx + 1 == stack->itsMaxItemIndex)              \
        FObject_SetMaxIndex(gCP,gTP,(TObject*)stack, stackNdx + 1 + _FLisp_StackAlloc);\
    atHMObject(stack->itsObjectArray,stackNdx) = (TObject*)curPair;\
    stackNdx++;                                             \
    curPair = (TPair*)asObject(&curPair->itsCar);

/*  A macro is provided to restore our context as we rise out of the parse tree. */

#define _FLisp_RestoreContext(stack, stackNdx, curPair)      \
    if(--stackNdx < 0)  goto MatchParen;                    \
    curPair = (TPair*)atHMObject(stack->itsObjectArray,stackNdx);               


/*  Function declarations */

extern  TVAL    FLisp_Morph			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);    
extern  TVAL    FLisp_Lisp			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FLisp_Parser		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[], HMChar hSrc, NUM extendedLex);

extern  TVAL    FLisp_Init			(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FLisp_MorphList		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theInput,TVAL altRules,TVAL altFailure);
extern  TVAL    FLisp_CompileMe		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpCHAR theSource);
extern  TVAL    FLisp_HCompileMe	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory hSource);
extern  TVAL    FLisp_SaveSrcText	(LpXCONTEXT gCP,LpTHREAD gTP,TVector* srcVector, NUM start, NUM end, HMChar text);
extern  TVAL    FLisp_MakeUniqueDbg	(LpXCONTEXT gCP,LpTHREAD gTP,TIntVector* theSpace, NUM pairOID, NUM srcNdx);
extern  TVAL    FLisp_BSearchDbg	(LpXCONTEXT gCP,LpTHREAD gTP,TIntVector* theSpace, NUM theData, COMPARE* compareCode);
extern  TVAL	FLisp_Error			(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR errorMsg,LpCHAR inputStr, NUM curPos, NUM inputLen);

#endif

