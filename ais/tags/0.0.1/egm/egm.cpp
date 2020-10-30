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

/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/egm/egm.cpp
									Evolutionary Grid Machine

CHANGE HISTORY
Version	Date		Who		Change
1.0057	3/18/2005	tlw		Update documentation
							---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	---------------------------------------- IMPORTS ----------------------------------------------
//#include <windows.h>
//#include <time.h>
#include <stdlib.h>
#include <ctype.h>

//	--------------------------------------- DECLARATIONS -------------------------------------------

extern "C" { // includes for modules written in C
	#include "fsmtbase.h" // SmartBase engine declarations
	}


/* Notes on SmartBase Types
C-type 	typedef	Tag		Vectortype	VectorTag     SmartBase type
long	NUM		TYNUM	TIntVector	TYINTVECTOR   integer
long	LONG	none	none		none
float	FLOAT	TYREAL	TFltVector	TYFLTVECTOR   float				
double	REAL	TYREAL	TNumVector	TYNUMVECTOR   number
short	SHORT	TYSHORT	TShtVector	TYSHORTVECTOR short
*/



// egm.cpp : Defines the entry point for the DLL application.
//

#define PUBLIC extern _declspec(dllexport)


BOOL APIENTRY DllMain( HANDLE hModule, 
                      DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{

    return TRUE;
}
// Forward declarations
PUBLIC TVAL math_egm_TrainMachine(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
PUBLIC TVAL math_egm_RunMachine(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
PUBLIC TVAL egm_Test(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);

/*-----------------------------------------------------------------
registerFunctions
Arguments:
	none
Returns:
	result
------------------------------------------------------------------*/
PUBLIC LPCSTR __getLibName () {
	LPCSTR name = "egm Version 1.0";
	return (name);
	}

PUBLIC TVAL __registerLibFunctions(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]) {
	char			*pBuf;
	long			BufLen;
	StartFrame
	TVAL		ec;
	EndFrame
	ec = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"math_egm_TrainMachine",(LpFUNC)&math_egm_TrainMachine);
	ec = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"math_egm_RunMachine",(LpFUNC)&math_egm_RunMachine);
	ec = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"egm_Test",(LpFUNC)&egm_Test);
	FrameExit(ec);
	}

PUBLIC TVAL egm_Test(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]) {
// (loadLib "egm")(defun foo() pvars:(v me) (setq v (new Vector: 3)) (setq me (myself)) (egm_Test me.Pv) (writeln v))(foo)
	StartFrame
	DeclareTVAL(ec);
	DeclareTVAL(pvars);
	DeclareTVAL(s);
	EndFrame
	Stack(pvars) = argv[0];
	Stack(s) = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),3,TSYMBOL("Structure"),TSYMBOL("hello"),TSYMBOL("there"));
	Stack(ec) = FSmartbase_Set(gCP,gTP,4,Stack(pvars),TSYMBOL("v"),TINT(0),Stack(s));
	FrameExit(Stack(ec));
	}


#define MAXROWS 1000
#define MAXESTPERIODS 24
#define MAXBUCKETSIZE 5
#define MAXBUCKETS 1000
#define MAXFIELDS 60
#define MAXDEPTH 3
#define BIGPOSNUM 3.731151215141E+303
#define BIGNEGNUM -3.731151215141E+303
#define BIGPOSINT 2147483647
#define BIGNEGINT -2147483647
#define MAXY 3
/***********************************************************************************************************************
math_egm_TrainMachine - This function performs multi-period training using a grid machine on estimator Lambda memories.
Arguments:
	pvars		- this argument contains a TYSTRUCTURE OR TYLAMBDA that holds the content of a estimator Lambda pvar memory.
	TimeToTrain	- this argument contains the number of seconds that this function is allowed to spend training for Processing type Jitter
	ProcessingType - this argument contains a symbol indicating processing type of: Jitter: All: Core: Secondary: Evolve:
	EstMemories - this optional argument contains a vector of previous estimator Lambda memories used for training

Summary of Training Process
math_egm_TrainMachine expects to receive estimator Lambda memories of a specific form - see the Lisp implementation of 
math.egm for details on the structure of egm Lambdas. The discussion below assumes you have read the math.egm source and
have a basic understanding of an egm Lambda's pvars.

TrainMachine does an exhaustive search, limited by the amount of time you allow it, to find a "best fit" algorithm using 
a pseudo-regression technique. The algorithm has the following general outline:
SELECT GENOME // the estimator Lambda keep track of the genome permutations processed so far
WHILE (MORETIME AND MOREGENOME)
	FOR EACH ESTMEMORY
		GET MATRIX FROM ESTMEMORY
		CALCULATE SORTVALUES FOR EACH ROW OF MATRIX // using independent variables in matrix
		SORT MATRIX ASCENDING BY SORTVALUES AND DIVIDE INTO BUCKETS
		FIND BEST TWO BUCKETS
		FIND BEST MORPH USING BEST TWO BUCKETS
		SCORE ROWS IN MATRIX USING BEST MORPH // score using dependent variables in matrix
		ACCUMMULATE SCORES INTO BUCKETS VECTOR 
		FOR EACH BUCKET IN BUCKETS VECTOR
			IF BUCKET IS BETTER THAN LOWEST IN TOPGRIDSEL VECTOR THEN
				REPLACE TOPGRIDSEL ITEM // a top grid selector item is keyed by genome, morph index and bucket
			END
		END
	END
	SELECT NEXT GENOME
END

Each estimator Lambda contains a genome vector which defines an algorithm for computing a sortvalue for each row in an
estimator's XY matrix. The matrix contains NumX columns of independent variables and NumY columns of dependent variables.

A genome is a vector of short values. Each vector element is an index into the independent variables 
(field expressions) in the XY data matrix. The genome definition, and other static values, are used to calculate 
"sortvalues" for each row in the XY data matrix. The estimator Lambda will permute the genome vector through as 
many permutations as permited by the TimeToTrain argument. Note that the Lambda stores the current genome so 
that subsequent calls to the Lambda allow it to pick up where it left off in its training.

The sortvalues are used to create a sorted index into the XY matrix. The index is then divided into NumBuckets.
An average score of each bucket is computed by adding up the values of the independent variable of the XY rows 
falling into that bucket. This is the bucket's score. Note that if there are multiple independent variables then
there is a separate score for each independent variable.

A GridSelector is defined by a genome vector, morphvector index and bucket. Each GridSelector has associated with it a 
score - this is the bucket score just described.

Each estimator Lambda contains a TopGridSelector vector for each independent variable. Each of these vectors contain
NumTopGridSelector entries. The TopGridSelector contains only those GridSelectors that have the highest scores 
for that independent variable.

If no EstMemories are passed then only the XY matrix in the pvars argument is trained against. If EstMemories 
are passed then bucket scores are computed and saved against each XY matrix in each EstMemory passed, in addition 
to the XY matrix that is part of the pvars argument. An average bucket score is computed for each bucket across 
all of the periods. 

NOTE: It is important that the calling Lambda pass the same collection of EstMemories to this function each
time it is called - the previosPeriods vector in the Lambdas pvars contain the keys (usually dates) used
to identify this collection of EstMemories. Otherwise the TopGridSelector contents are not defined 
and will be unusable.

NOTE: There are NumY TopGridSelector vectors - one for each dependent variable.



************************************************************************************************************************/
PUBLIC TVAL math_egm_TrainMachine(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]) {
	int		c,d,i,j,k,r,y,m,oc,p;	// loop counters and indicies
	// Coefficient vector for grid machine calc. This can be regenerated using the following
	// (setq Coefficients (new Vector: 1 1.0))
	// (loop for i from 1 to 10 (setq Coefficients[i] (/ 10 i)))
	REAL	BestBucketRanks[MAXY][2];
	REAL	BestBucketRank1;
	REAL	BestBucketRank2;
	SHORT	BestBucketRankIdx1;
	SHORT	BestBucketRankIdx2;
	SHORT	BestBucketRanksIdx[MAXY][2];
	SHORT	BestMorphIdx;
	REAL	BestTotalRank;
	int		bucket;					// index to a grid sction overlayed onto XY
	bool	bucketeven;
	REAL	bucketpair[MAXBUCKETSIZE * 2];
	REAL	bucketrank;
	REAL	bucketscore;
	int		bucketsize;				// local, not the BucketSize in pvars
	int		BucketSize;				// local, not the BucketSize in pvars - this is always the full bucketsize (not adjusted for short sets)
	SHORT	bucketwincount;
	static const REAL Coefficients[] = {1.0, 10.0, 5.0, 3.333333333333, 2.5, 2.0, 1.666666666667, 1.428571428571, 1.25, 1.111111111111};
	REAL	coef;					// single coefficient value
	int		CoreExpCount;			// number of independent variables from xy matrix included in core genome population
	bool	CoreDone;				// Flag indicating that core population genomes have been processed
	int		D;						// current depth of genome
	int		end;
	int		endrow;
	REAL	endTime;				// time to stop iterations
	int		index;
	int		idx;
	long	inserts;				// counter of insertion of new top scores in this iteration (debugging aid only)
	bool	isSecondary;			// Flag indicating which population the current genome belongs to
	long	genomesTried = 0;		// counter of number of genomes processed in this iteration (debugging aid only)
	REAL	lowRank;	
	SHORT	mi;						// index into morph vector entry mv
	NUM		MaxDepth;
	short	maxmorphrows;
	bool	MorphEven;
	short	morphidx;
	short	mrow;
	short	moffset;
	bool	Morph;
	short	MorphDistance;
	static const MorphVectors[126][10] = 
				{{0,1,2,3,4,5,6,7,8,9},
				{0,1,2,3,5,4,6,7,8,9},
				{0,1,2,3,6,4,5,7,8,9},
				{0,1,2,3,7,4,5,6,8,9},
				{0,1,2,3,8,4,5,6,7,9},
				{0,1,2,3,9,4,5,6,7,8},
				{0,1,2,4,5,3,6,7,8,9},
				{0,1,2,4,6,3,5,7,8,9},
				{0,1,2,4,7,3,5,6,8,9},
				{0,1,2,4,8,3,5,6,7,9},
				{0,1,2,4,9,3,5,6,7,8},
				{0,1,2,5,6,3,4,7,8,9},
				{0,1,2,5,7,3,4,6,8,9},
				{0,1,2,5,8,3,4,6,7,9},
				{0,1,2,5,9,3,4,6,7,8},
				{0,1,2,6,7,3,4,5,8,9},
				{0,1,2,6,8,3,4,5,7,9},
				{0,1,2,6,9,3,4,5,7,8},
				{0,1,2,7,8,3,4,5,6,9},
				{0,1,2,7,9,3,4,5,6,8},
				{0,1,2,8,9,3,4,5,6,7},
				{0,1,3,4,5,2,6,7,8,9},
				{0,1,3,4,6,2,5,7,8,9},
				{0,1,3,4,7,2,5,6,8,9},
				{0,1,3,4,8,2,5,6,7,9},
				{0,1,3,4,9,2,5,6,7,8},
				{0,1,3,5,6,2,4,7,8,9},
				{0,1,3,5,7,2,4,6,8,9},
				{0,1,3,5,8,2,4,6,7,9},
				{0,1,3,5,9,2,4,6,7,8},
				{0,1,3,6,7,2,4,5,8,9},
				{0,1,3,6,8,2,4,5,7,9},
				{0,1,3,6,9,2,4,5,7,8},
				{0,1,3,7,8,2,4,5,6,9},
				{0,1,3,7,9,2,4,5,6,8},
				{0,1,3,8,9,2,4,5,6,7},
				{0,1,4,5,6,2,3,7,8,9},
				{0,1,4,5,7,2,3,6,8,9},
				{0,1,4,5,8,2,3,6,7,9},
				{0,1,4,5,9,2,3,6,7,8},
				{0,1,4,6,7,2,3,5,8,9},
				{0,1,4,6,8,2,3,5,7,9},
				{0,1,4,6,9,2,3,5,7,8},
				{0,1,4,7,8,2,3,5,6,9},
				{0,1,4,7,9,2,3,5,6,8},
				{0,1,4,8,9,2,3,5,6,7},
				{0,1,5,6,7,2,3,4,8,9},
				{0,1,5,6,8,2,3,4,7,9},
				{0,1,5,6,9,2,3,4,7,8},
				{0,1,5,7,8,2,3,4,6,9},
				{0,1,5,7,9,2,3,4,6,8},
				{0,1,5,8,9,2,3,4,6,7},
				{0,1,6,7,8,2,3,4,5,9},
				{0,1,6,7,9,2,3,4,5,8},
				{0,1,6,8,9,2,3,4,5,7},
				{0,1,7,8,9,2,3,4,5,6},
				{0,2,3,4,5,1,6,7,8,9},
				{0,2,3,4,6,1,5,7,8,9},
				{0,2,3,4,7,1,5,6,8,9},
				{0,2,3,4,8,1,5,6,7,9},
				{0,2,3,4,9,1,5,6,7,8},
				{0,2,3,5,6,1,4,7,8,9},
				{0,2,3,5,7,1,4,6,8,9},
				{0,2,3,5,8,1,4,6,7,9},
				{0,2,3,5,9,1,4,6,7,8},
				{0,2,3,6,7,1,4,5,8,9},
				{0,2,3,6,8,1,4,5,7,9},
				{0,2,3,6,9,1,4,5,7,8},
				{0,2,3,7,8,1,4,5,6,9},
				{0,2,3,7,9,1,4,5,6,8},
				{0,2,3,8,9,1,4,5,6,7},
				{0,2,4,5,6,1,3,7,8,9},
				{0,2,4,5,7,1,3,6,8,9},
				{0,2,4,5,8,1,3,6,7,9},
				{0,2,4,5,9,1,3,6,7,8},
				{0,2,4,6,7,1,3,5,8,9},
				{0,2,4,6,8,1,3,5,7,9},
				{0,2,4,6,9,1,3,5,7,8},
				{0,2,4,7,8,1,3,5,6,9},
				{0,2,4,7,9,1,3,5,6,8},
				{0,2,4,8,9,1,3,5,6,7},
				{0,2,5,6,7,1,3,4,8,9},
				{0,2,5,6,8,1,3,4,7,9},
				{0,2,5,6,9,1,3,4,7,8},
				{0,2,5,7,8,1,3,4,6,9},
				{0,2,5,7,9,1,3,4,6,8},
				{0,2,5,8,9,1,3,4,6,7},
				{0,2,6,7,8,1,3,4,5,9},
				{0,2,6,7,9,1,3,4,5,8},
				{0,2,6,8,9,1,3,4,5,7},
				{0,2,7,8,9,1,3,4,5,6},
				{0,3,4,5,6,1,2,7,8,9},
				{0,3,4,5,7,1,2,6,8,9},
				{0,3,4,5,8,1,2,6,7,9},
				{0,3,4,5,9,1,2,6,7,8},
				{0,3,4,6,7,1,2,5,8,9},
				{0,3,4,6,8,1,2,5,7,9},
				{0,3,4,6,9,1,2,5,7,8},
				{0,3,4,7,8,1,2,5,6,9},
				{0,3,4,7,9,1,2,5,6,8},
				{0,3,4,8,9,1,2,5,6,7},
				{0,3,5,6,7,1,2,4,8,9},
				{0,3,5,6,8,1,2,4,7,9},
				{0,3,5,6,9,1,2,4,7,8},
				{0,3,5,7,8,1,2,4,6,9},
				{0,3,5,7,9,1,2,4,6,8},
				{0,3,5,8,9,1,2,4,6,7},
				{0,3,6,7,8,1,2,4,5,9},
				{0,3,6,7,9,1,2,4,5,8},
				{0,3,6,8,9,1,2,4,5,7},
				{0,3,7,8,9,1,2,4,5,6},
				{0,4,5,6,7,1,2,3,8,9},
				{0,4,5,6,8,1,2,3,7,9},
				{0,4,5,6,9,1,2,3,7,8},
				{0,4,5,7,8,1,2,3,6,9},
				{0,4,5,7,9,1,2,3,6,8},
				{0,4,5,8,9,1,2,3,6,7},
				{0,4,6,7,8,1,2,3,5,9},
				{0,4,6,7,9,1,2,3,5,8},
				{0,4,6,8,9,1,2,3,5,7},
				{0,4,7,8,9,1,2,3,5,6},
				{0,5,6,7,8,1,2,3,4,9},
				{0,5,6,7,9,1,2,3,4,8},
				{0,5,6,8,9,1,2,3,4,7},
				{0,5,7,8,9,1,2,3,4,6},
				{0,6,7,8,9,1,2,3,4,5}};
 
	REAL	mv;						// morph vector entry
	int		NumBuckets;
	int		NumEst;					// Number of estimator memories passed in optional third argument
	int		NumP;					// Number of periods .. ie: number of estimator memories passed in optional third argument
	int		NumGenome;
	int		NumGenomeSub1;
	int		NumMorphVectors = 126;	// number of morphvectors in MorphVectors array
	int		NumMorphVector = 10;	// number of elements in each MorphVector - note that this is 2 * bucketsize
	int		NumRows;
	int		NumTopGridSel;
	int		NumTopGridSelCore;
	int		NumTopGridSelSec;
	int		NumX;
	int		NumXSub1;
	int		NumY;
	int		NumYSub1;
	REAL	numWins;
	LpREAL	pBucketScore;
	LpREAL	pBucketPctWin;
	int		pos;					// index into multi-dimensional arrays
	LpREAL	pMorphVectors;			// pointer to start of MorphVectors in pvars
	LpSHORT pMorphVector;			// pointer to a MorphVector in MorphVectors
	LpSHORT	pGenome;				// pointer to start of Genome vector in pvars
	LpSHORT	pGenomeCopy;
	LpSHORT pLowRanksIndex;
	LpSHORT	pLowRanksIndexCore;		// index into LowRanksIndexCore vector in pvars
	LpSHORT pLowRanksIndexSec;		// index into LowRanksIndexSec vector in pvars
	LpREAL	pLowRanks;
	LpREAL	pLowRanksCore;			// pointer to vector of low ranks in pvars
	LpREAL  pLowRanksSec;			// pointer to vector of low ranks in pvars
	int		posBase;
	int		procType;				// type of processing to perform: 0=All 1=Core 2=Secondary
	LpREAL	pScores;				// pointer to start of Scores vector in pvars
	LpNUM	pStack;
	LpSHORT	pStocks;
	LpSHORT	pStockIDs;				// pointer to vector of stock ids in pvars
	LpREAL	pWins;
	LpREAL	*pXYrows;				// pointer to start of 2 dimensional array of pointers to start of each rows vector of real values
	REAL	rank;
	bool	rankByWinPct;
	int		row;					// row index into XY
	bool	SecondaryDone;			// Flag indicating secondary genome population has been processed.
	int		size;
	// Scale is a vector of constants used in the grid machine calc. It can be easily
	// calculated but is used in this fashion for speed of processing. Be sure to change
	// this vector if you allow a MaxDepth of more than 10!
	static const REAL Scale[] = {1.0, 1.0, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, 10000000.0, 100000000.0};
	REAL	scale;					// single scale value
	REAL	score;					// score accumulator
	REAL	scoreAccumulate;		// second scoreAccumulator
	REAL	Score;					// score value
	REAL	sortval;				// sort value accumulator
	REAL	startTime;
	SHORT	stocks[MAXBUCKETSIZE];	// accumulator vector for stocks picked by individual grid selectors (used for diversity analysis in debugging phase)
	DOUBLE	tag;
	int		ThisPeriod;				// period index for the current estimator Lambda.
	REAL	thisScore;				// score accumulator
	REAL	timeToRun;				// assinged from argument
	REAL	TotalRank;

	REAL	winpct;					// percentage of winning picks in a bucket. ex: (/ numWins BucketSize)
	SHORT	v;
	SHORT	xi;						// index into rowTemp for x (independent) variables
	int		Xlist[MAXROWS + 1];
	REAL	Xsort[MAXROWS];			// interim vector result of score for a genome
	int		XsortIndex[MAXROWS];	// sorted index into xy based on scores in Xsort
	REAL	xvalue;					// xvalue from a XY row. ex: (setq xvalue rowTemp[xi])
	int		yrow;
	int		yOffsetIntoXY;			// index into rowTemp for y (dependent) variables

	StartFrame
	DeclareTVAL(BucketScore);
	DeclareTVAL(BucketPctWin);
	DeclareTVAL(ec);
	DeclareTVAL(estMemories);
	DeclareTVAL(Genome);
	DeclareTVAL(GenomeCopy);
	DeclareTVAL(GridSelector);
	DeclareTVAL(LowRanksCore);
	DeclareTVAL(LowRanksSec);
	DeclareTVAL(LowRanksIndexCore);
	DeclareTVAL(LowRanksIndexSec);
	DeclareTVAL(pvars);
	DeclareTVAL(Scores);
	DeclareTVAL(Stocks);
	DeclareTVAL(TopGridSelCore);
	DeclareTVAL(TopGridSelSec);
	DeclareTVAL(TopGridSelElement);
	DeclareTVAL(Wins);
	DeclareTVAL(val);
	DeclareTVAL(Xlist2);
	DeclareTVAL(XsortIndex2);		// debugging aid
	DeclareTVAL(XY);
	DeclareTVAL(XYrow);
	DeclareTVAL(XYrows);
	EndFrame

	// Get arguments
	if (argc < 3) FrameExit(TERROR((LpCHAR) "Error:math_egm_TrainMachine arglist - 3 arguments required")); // make sure there are enough arguments
	if (argc > 4) FrameExit(TERROR((LpCHAR) "Error:math_egm_TrainMachine arglist - more than 4 arguments passed")); // make sure there are enough arguments

	// First argument is pvars structure of egm Lambda instance 
	if (argv[0].Tag != TYLAMBDA) FrameExit(TERROR((LpCHAR) "Error:math_egm_TrainMachine - 1st argument must be pvars of egm instance"));
	Stack(pvars) = argv[0];

	// Second argument is time one of timeToRun 	- A number holding number of seconds to process. Pass 0 for unlimited processing time.
	if (!isNumIndex(&argv[1])) FrameExit(TERROR((LpCHAR) "Error:math_egm_TrainMachine - 2nd argument must be number of seconds to train"));
	timeToRun = (REAL)asNumIndex(&argv[1]);

	// Third argument is a number that specifies type of processing to do:
	// 0 Jitter		- process incrementally for TimeToTrain specified
	// 1 All		- Process both Core and Secondary genome populations
	// 2 Core		- Process only Core genome population
	// 3 Secondary	- Process Secondary population
	// 4 Evolve		- Process Core if CoreDone = false. Process Secondary if SecondaryDone=false.

	if (!isNumIndex(&argv[2])) FrameExit(TERROR((LpCHAR) "Error:math_egm_TrainMachine - 3rd argument must be procType number: 0==Jitter: 1==All: 2==Core: 3==Secondary: 4==Evolve:"));
	procType = (INT) asNumIndex(&argv[2]);
	if (procType < 0 || procType > 4) 
		FrameExit(TERROR((LpCHAR) "Error:math_egm_TrainMachine - 3rd argument must be procType with range 0..4"));

	// Third argument (optional) is object vector of esitmator memories of previous periods to use in evaluating genomes
	if (argc == 4) {
		if (argv[3].Tag != TYOBJVECTOR) FrameExit(TERROR((LpCHAR) "Error:math_egm_TrainMachine - 3rd argument must be object vector of egm instance memories"));
		Stack(estMemories) = argv[3];
		NumEst = FSmartbase_VectorLen(gCP,gTP,Stack(estMemories));
		Stack(estMemories) = FSmartbase_Set(gCP,gTP,3,Stack(estMemories),TINT(NumEst),Stack(pvars));
		NumP = NumEst + 1; // Account for local XY and this is the final number of periods to process
		}
	else {
		Stack(estMemories) = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),4,TSYMBOL("Vector"),TSYMBOL("object"),TINT(1),Stack(pvars));
		NumP = 1;
		}

	Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("MaxDepth"));
	if (Stack(val).Tag != TYNUM) FrameExit(TERROR("Error:math_egm_trainMachine: MaxDepth not an integer."));
	MaxDepth = (int)asNumIndex(&Stack(val));

	Stack(val)=FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("NumX"));
	if (Stack(val).Tag != TYNUM) FrameExit(TERROR("Error:math_egm_trainMachine: NumX not an integer."));
	NumX = (NUM)asNumIndex(&Stack(val));

	Stack(Genome) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("Genome"));
	if (Stack(Genome).Tag != TYSHORTVECTOR) FrameExit(TERROR("Error:math_egm_trainMachine: Genome not vector of shorts"));
	pGenome = (LpSHORT) FSmartbase_VectorPtr(gCP,gTP,Stack(Genome));
	NumGenome = FSmartbase_VectorLen(gCP,gTP,Stack(Genome));

	// Get endtime based on time to run
	endTime = clock() + (timeToRun * (REAL) CLOCKS_PER_SEC); // from time.h
	startTime = clock();

	Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("CoreDone"));
	if (Stack(val).Tag != TYBOLE) FrameExit(TERROR("Error:math_egm_trainMachine: CoreDone not a boolean value."));
	CoreDone = (asInt(&Stack(val)) > 0) ? true : false;

	Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("SecondaryDone"));
	if (Stack(val).Tag != TYBOLE) FrameExit(TERROR("Error:math_egm_trainMachine: SecondaryDone not a boolean value."));
	SecondaryDone = (asInt(&Stack(val)) > 0) ? true : false;

	// Check to make sure more training is required for processing type Evolve:
	if (procType == 4 && CoreDone && SecondaryDone) goto FINISHED;

	// Check to make sure more trainng is required for processing type of JItter. We do this by examining the current 
	// genome to see if we have processed all of the permutations. 
	NumXSub1 = NumX - 1;
	if (procType == 0 && NumGenome == MaxDepth) {
		d = 0;
		while ((d < NumGenome) && (pGenome[d] == NumXSub1)) d++;
		if (d == NumGenome) goto FINISHED;
	}

	// Get NumRows so we can size pXYrows data 
	Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("NumRows"));
	if (Stack(val).Tag != TYNUM) FrameExit(TERROR("Error:math_egm_trainMachine: NumRows not an integer."));
	NumRows = (int)asNumIndex(&Stack(val));

	// Allocate memory to hold the pXYrows data - we use the engine's memory manager here
	size = sizeof(LpREAL);
	Stack(XYrows) = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),3,TSYMBOL("Vector"),TSYMBOL("byte"),TINT(size * NumP * NumRows));
	pXYrows = (LpREAL *) FSmartbase_VectorPtr(gCP,gTP,Stack(XYrows));

	for(p=0; p<NumP; ++p) {
		Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(estMemories),TINT(p));
		if ((Stack(val).Tag != TYSTRUCTURE) && (Stack(val).Tag != TYLAMBDA)) FrameExit(TERROR("Error:math_egm_trainMachine: estMemories element not a structure"));
		Stack(XY) = FSmartbase_Ref(gCP,gTP,2,Stack(val),TSYMBOL("XY"));
		if (Stack(XY).Tag != TYOBJVECTOR) FrameExit(TERROR("Error:math_egm_trainMachine: XY not vector of objects"));
		for(i=0; i<NumRows; ++i) {
			Stack(XYrow) = FSmartbase_Ref(gCP,gTP,2,Stack(XY),TINT(i));
			if (Stack(XYrow).Tag != TYNUMVECTOR) FrameExit(TERROR("Error:math_egm_trainMachine: XY row not vector of reals"));
			pos = i + (p * NumRows);
			pXYrows[pos] = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,Stack(XYrow));
			// XYrows[p][i] = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,Stack(XYrow));
			}//i
		}//p



	Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("BucketSize"));
	if (Stack(val).Tag != TYNUM) FrameExit(TERROR("Error:math_egm_trainMachine: BucketSize not an integer."));
	BucketSize = (int)asNumIndex(&Stack(val));
	// The egm machine only works with a bucket size of 5. Note that this restriction is not pervasive throughtout this
	// code. It is only a restriction because of the morphvector implementation. However, emperical evidence suggests that
	// 5 items in each bucket is a "sweet spot" so we will "hard code" this parameter.
	if (BucketSize != 5) FrameExit(TERROR("Error:math_egm_trainMachine: BucketSize not 5."));

	Stack(val)=FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("NumY"));
	if (Stack(val).Tag != TYNUM) FrameExit(TERROR("Error:math_egm_trainMachine: NumY not an integer."));
	NumY = (NUM)asNumIndex(&Stack(val));
	
	Stack(val)=FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("NumTopGridSelCore"));
	if (Stack(val).Tag != TYNUM) FrameExit(TERROR("Error:math_egm_trainMachine: NumTopGridSelCore not an integer."));
	NumTopGridSelCore = (NUM)asNumIndex(&Stack(val));

	Stack(val)=FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("NumTopGridSelSec"));
	if (Stack(val).Tag != TYNUM) FrameExit(TERROR("Error:math_egm_trainMachine: NumTopGridSelSec not an integer."));
	NumTopGridSelSec = (NUM)asNumIndex(&Stack(val));

	// Get pvar vectors we will update using FSmartbase_Set
	Stack(TopGridSelCore) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("TopGridSelCore"));
	if (Stack(TopGridSelCore).Tag != TYOBJVECTOR) FrameExit(TERROR("Error:math_egm_trainMachine: TopGridSelCore not vector of objects"));

	Stack(TopGridSelSec) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("TopGridSelSec"));
	if (Stack(TopGridSelSec).Tag != TYOBJVECTOR) FrameExit(TERROR("Error:math_egm_trainMachine: TopGridSelSec not vector of objects"));

	Stack(LowRanksCore) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("LowRanksCore"));
	if (Stack(LowRanksCore).Tag != TYNUMVECTOR) FrameExit(TERROR("Error:math_egm_trainMachine: LowRanksCore not vector of reals"));
	pLowRanksCore = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,Stack(LowRanksCore));

	Stack(LowRanksSec) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("LowRanksSec"));
	if (Stack(LowRanksSec).Tag != TYNUMVECTOR) FrameExit(TERROR("Error:math_egm_trainMachine: LowRanksSec not vector of reals"));
	pLowRanksSec = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,Stack(LowRanksSec));

	Stack(LowRanksIndexCore) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("LowRanksIndexCore"));
	if (Stack(LowRanksIndexCore).Tag != TYSHORTVECTOR) FrameExit(TERROR("Error:math_egm_trainMachine: LowRanksIndexCore not vector of shorts"));
	pLowRanksIndexCore = (LpSHORT)FSmartbase_VectorPtr(gCP,gTP,Stack(LowRanksIndexCore));

	Stack(LowRanksIndexSec) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("LowRanksIndexSec"));
	if (Stack(LowRanksIndexSec).Tag != TYSHORTVECTOR) FrameExit(TERROR("Error:math_egm_trainMachine: LowRanksIndexSec not vector of shorts"));
	pLowRanksIndexSec = (LpSHORT)FSmartbase_VectorPtr(gCP,gTP,Stack(LowRanksIndexSec));
	
	Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("StockIDs"));
	if (Stack(val).Tag != TYSHORTVECTOR) FrameExit(TERROR("Error:math_egm_trainMachine: StockIDs not vector of shorts"));
	pStockIDs = (LpSHORT)FSmartbase_VectorPtr(gCP,gTP,Stack(val));

	Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("RankByWinPct"));
	if (Stack(val).Tag != TYBOLE) FrameExit(TERROR("Error:math_egm_trainMachine: RankByWinPct not a boolean value."));
	rankByWinPct = (asInt(&Stack(val)) > 0) ? true : false;

	Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("Morph"));
	if (Stack(val).Tag != TYBOLE) FrameExit(TERROR("Error:math_egm_trainMachine: Morph not a boolean value."));
	Morph = (asInt(&Stack(val)) > 0) ? true : false;

	Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("CoreExpCount"));
	if (Stack(val).Tag != TYNUM) FrameExit(TERROR("Error:math_egm_trainMachine: CoreExpCount not a number value."));
	CoreExpCount = (int)asNumIndex(&Stack(val));


	// Initilize some useful range values
	D = (int) NumGenome; 
	NumGenomeSub1 = NumGenome - 1;
	NumYSub1 = NumY - 1;
	NumBuckets = (int) (((NumRows % BucketSize) == 0) ?  NumRows / BucketSize : NumRows / BucketSize + 1);
	ThisPeriod = NumP - 1; // This is the period index for the current estimator Lambda

	// Allocate memory to hold large multi-dimensional arrays used in this function
	// WARNING - it is up to you to make sure you don't reference outside the allocated memory!!!!!
	Stack(BucketScore) = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),3,TSYMBOL("Vector"),TSYMBOL("number"),TINT(NumBuckets));
	pBucketScore = (LpREAL) FSmartbase_VectorPtr(gCP,gTP,Stack(BucketScore));

	Stack(BucketPctWin) = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),3,TSYMBOL("Vector"),TSYMBOL("number"),TINT(NumBuckets));
	pBucketPctWin = (LpREAL) FSmartbase_VectorPtr(gCP,gTP,Stack(BucketPctWin));

	Stack(Scores) = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),3,TSYMBOL("Vector"),TSYMBOL("number"),TINT(NumY * NumRows));
	pScores = (LpREAL) FSmartbase_VectorPtr(gCP,gTP,Stack(Scores));

	Stack(Wins) = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),3,TSYMBOL("Vector"),TSYMBOL("number"),TINT(NumY * NumRows));
	pWins = (LpREAL) FSmartbase_VectorPtr(gCP,gTP,Stack(Wins));

	while(true) {
		// Determine if this is genome belongs to the secondary population
		isSecondary = false;

		for (d=0; d < NumGenome; ++d)
			if (pGenome[d] >= CoreExpCount) {
				isSecondary = true; 
				break;
			}

		switch (procType) {
		case 0: // processing type == Jitter
			if (clock() >= endTime) goto FINISHED;
			break;
		case 1: // processing type == All
			break;
		case 2: // If processing type == Core we skip genomes from the Secondary population
			if (isSecondary) goto SKIPCALC;
			break;
		case 3:	// If processing type == Secondary we skip genomes from the Core population
			if (!isSecondary) goto SKIPCALC;
			break;
		case 4: // If processing type == Evolve then skip genomes from Core population only if CoreDone == true
			if (!isSecondary && CoreDone) goto SKIPCALC;
		}

		// Clear score accumulators
		for (y=0; y<NumY; ++y) { // for each independent variable
			posBase = (y * NumRows);
			end = posBase + NumRows;
			for(i=posBase; i<end; ++i) { // for each row
				// [i] is like [y][row]
				pScores[i] = 0.0;
				pWins[i] = 0.0;
				}//i
			}//y

		++genomesTried;
		// Generate values for current genome and build Xlist
		// Xlist is a linked list used to generate XsortIndex ie: list sort 
		for(p=0; p<NumP; ++p) { // For each period
			int listStart = -1;
			// Clear Xsort, XsortIndex and Xlist
			for(i=0; i < MAXROWS; ++i) {
				Xsort[i] = 0;
				XsortIndex[i] = 0;
				Xlist[i] = -1;
				}
			Xlist[MAXROWS] = -1; //Xlist is one element longer than the other vectors
			for(row=0; row<NumRows; ++row) { // For each row
				j = 0;
				sortval = 0;
				// Use the genome to calculate a sortval
				for(d=0; d<NumGenome; ++d, ++j) { // NumGenome is the length of the Genome Vector
					xi = pGenome[d];
					//xvalue = pXYrows[p][row][xi];
					pos = row + (p * NumRows); // [p][row]
					xvalue = pXYrows[pos][xi];
					if (j == 0)
						sortval += (Scale[j] * Coefficients[j] * xvalue);
					else				
						sortval += (Scale[j] * (NUM) (Coefficients[j] * xvalue));
					}
				Xsort[row] = sortval;
				// Notes on Simple List Insertion Sort
				// Insert into Xlist -- this is the sorted linked list
				// The Xsort[] array is the array of sort values. The values in this array do not move.
				// The Xsort[] array grows by one with each iteration through this loop. Each new sort value
				// is inserted at element position row.
				// The Xlist[] array contains integer values that are the index of another element in the
				// Xlist[] array or the seed value of -1 which means the element is not yet assigned.
				// Xlist[] array positions relate to Xsort array postions with an offset of one. For instance,
				// Xlist[2] is related to Xsort[1]. This allows the Xlist[0] element to contain the index to
				// the first Xlist[] element that relates to an Xsort[] element. This makes the Xlist array
				// one element longer than the Xsort[] array.
				// The objective of the fully populated Xlist[] array is to allow the traversal of the Xlist[]
				// array by using the values in Xlist[] as indicies into other Xlist[] elements. When on a given
				// Xlist[] element you can find the sorted value in the Xsort[] array using the Xlist[] element
				// index - 1. 
				// Xlist[] is built using the simple insertion logic below:
				k = row + 1; // k is the index of the Xlist[] element related to the row index in Xsort
				c = 0; // c is the current Xlist index we use to traverse the Xlist elements.
				LOOP:
				oc = Xlist[c]; // oc is the next Xlist element to check
				if (oc == -1) { // Add to end of chain. Remember that we seeded the Xlist array with -1 values.
					Xlist[c] = k; // Ok so we reached the end of the current Xlist[] so just insert the relative position k.
								  // Remember that k is the Xlist[] element index related to Xsort[] element index row.
					goto DONE;
				}
				if (Xsort[oc - 1] > sortval) { // Insert into chain if previous index value is greater than current sortvalue.
					// Remember that sortvalue is equal to Xsort[row] and that Xsort[row] is related to Xlist[k] and that k = row + 1.
					Xlist[k] = oc; 
					Xlist[c] = k;
					goto DONE;
				}
				c = Xlist[c]; // get index for next linked list index.
				goto LOOP;
				DONE:
				;
				}//row

			// Generate the XsortIndex[] array
			// Once constructed, this array will contian the element indicies into Xsort[] for 
			// an ascending sort. We generate this list by simply walking the Xlist[] array.
			c = Xlist[0];	// Remember that Xlist[0] is not related to a Xsort[] element - it only
							// holds the index into the first Xlist[] element that does relate to an Xsort[]
							// element.
			i = 0;
			while (c != -1) { 
				XsortIndex[i++] = c - 1; 
				c = Xlist[c];
				}

			// Accumulate independent variable values, for period, into scores array
			// Also find and make a note of the two highest scoring buckets for each independent variable
			for (y=0; y<NumY; ++y) { // for each independent variable
				posBase = y * NumRows;
				yOffsetIntoXY = NumX + y; // y variables follow x varaibles in XY
				row = 0;
				while (row < NumRows) {
						pos = XsortIndex[row] + (p * NumRows);
						score = pXYrows[pos][yOffsetIntoXY];
						yrow = row + posBase; // [y][row]
						pScores[yrow] += score; // pScores contains scores for row summed across periods!
						if (score > 0.0) pWins[yrow] += 1; // Note the counting of wins is specific to a period and dependent variable!
						++row;
					}//while (row < NumRows)
				}//y
			}//p -- for each period

		// At this point we have calculated the following:
		// pScores contains the score values, summed across periods, by dependent variable by row for the current genome
		// pWins contains the win count, summed across periods, by dependent variable by row for the current genome
		
		for (y=0; y<NumY; ++y) { // for each independent variable
			posBase = y * NumRows;
			yOffsetIntoXY = NumX + y; // y variables follow x varaibles in XY
			BestBucketRank1 = BIGNEGNUM;
			BestBucketRank2 = BIGNEGNUM;
			BestBucketRankIdx1 = -1;
			BestBucketRankIdx2 = -1;

			bucket = 0;
			bucketsize = BucketSize; // reset bucketsize to full size		
			row = 0;

			if (Morph) {
				// Find the number of rows that contain an even number of buckets to allow us to "morph" these
				// buckets later. Ignore odd buckets and or partial buckes because they can not be morped later.
				maxmorphrows = ((int) NumRows / (BucketSize * 2)) * (BucketSize * 2);

				// First pass through rows to find best two buckets
				while (row < maxmorphrows) {
					endrow = row + bucketsize;
					bucketrank = 0.0;
					bucketscore = 0.0;
					bucketwincount = 0;
					for (; row < endrow; ++row) { // Accumulate values into a bucket and place in scores array
						yrow = row + posBase; // [y][row]
						bucketscore = pScores[yrow]; // pScores contains scores for row summed across periods!
						bucketwincount += (SHORT)pWins[yrow];
					}//row
					if (bucketsize == BucketSize) { // Do not include short bucket in best buckets test. We can't morph it later!
						bucketrank = rankByWinPct ? ( bucketwincount / bucketsize) : bucketscore;
						if (bucketrank > BestBucketRank1) { 
							BestBucketRank1 = bucketrank;
							BestBucketRankIdx1 = bucket;
						}
						else 
						if (bucketrank > BestBucketRank2) {
							BestBucketRank2 = bucketrank;
							BestBucketRankIdx2 = bucket;
						}
					}
					++bucket;
					}//while (row < NumRows)
			
				// Using the best two buckets, find the best morphVector 
				// Start by filling bucketpair array with the contents of the best two buckets.
				// Note that we fill the bucketpair based on the setting of rankWinPct.
				row = (BucketSize * BestBucketRankIdx1);
				idx = 0;
				endrow = row + BucketSize; 
				if (rankByWinPct)
					while (row < endrow) {
						yrow = row + posBase;
						bucketpair[idx++] = pWins[yrow];
						row++;
						}
				else
					while (row < endrow) {
						yrow = row + posBase;
						bucketpair[idx++] = pScores[yrow];
						row++;
						}
				row = (BucketSize * BestBucketRankIdx2);
				endrow = row + BucketSize;
				if (rankByWinPct)
					while (row < endrow) {
						yrow = row + posBase;
						bucketpair[idx++] = pWins[yrow];
						row++;
						}
				else
					while (row < endrow) {
						yrow = row + posBase;
						bucketpair[idx++] = pScores[yrow];
						row++;
						}

				// Now, apply morphs to find "best" morph on our two best buckets
				BestTotalRank = BIGNEGNUM;
				for (m=0; m<NumMorphVectors; ++m) {
					TotalRank = 0.0;
					for (i=0; i < BucketSize; ++i) {
						TotalRank += bucketpair[MorphVectors[m][i]];
						}
					if (TotalRank > BestTotalRank) {
						BestTotalRank = TotalRank;
						BestMorphIdx = m;
						}
					}//m

	// FSmartbase_Writeln(gCP,gTP,1,TINT(BestMorphIdx));
					row = 0;
					bucket = 0;
					bucketsize = BucketSize; // reset bucketsize to full size
					MorphEven = ((BestBucketRankIdx1 % 2) == 0);
					MorphDistance = (BestBucketRankIdx1 > BestBucketRankIdx2) ? BestBucketRankIdx1 - BestBucketRankIdx2 : BestBucketRankIdx2 - BestBucketRankIdx1;
				// Second pass through rows to score all rows with selected morph
				while (row < NumRows) {
					endrow = row + bucketsize;
					if (row >= maxmorphrows) { // don't morph odd bucket or short bucket at end of set
						if (endrow > NumRows) { // accomodate short last bucket
							bucketsize = NumRows - row;
							endrow = NumRows;
							}
						numWins = 0;
						score = 0;
						while (row < endrow) {
							yrow = row + posBase;
							numWins += pWins[yrow];
							score += pScores[yrow];
							++row;
							}//row
						}
					else {// Use morphvector to rearrange rows as we accumulate values
						numWins = 0;
						score = 0;
						bucketeven = (( bucket % 2) == 0);
						if ((bucketeven && MorphEven) || (!bucketeven && !MorphEven)) { // Morph using first morphvector part
							morphidx = 0;
							moffset = MorphDistance * BucketSize;
							}
						else { // Morph using second morphvector part
							morphidx = BucketSize; // The morph vector is always 2 * BucketSize in length.
							moffset = -(MorphDistance * BucketSize);
							}
						i = 0;
						while (row < endrow) {
							mrow = moffset + MorphVectors[BestMorphIdx][i + morphidx];
							if (mrow > maxmorphrows) mrow = mrow - maxmorphrows; // wrap around if past end
							if (mrow < 0) mrow = maxmorphrows + mrow; // wrap around if before  beginning
							++i;
							yrow = mrow + posBase;
							numWins += pWins[yrow];
							score += pScores[yrow];
							++row;
							}//row
						}

					pBucketScore[bucket] = score / bucketsize / NumP;
					pBucketPctWin[bucket] = numWins / bucketsize / NumP;

					bucket++;
					}// while on row

				}// if (Morph) ...
			else { // Not doing morph
				row = 0;
				bucket = 0;
				bucketsize = BucketSize;
				while (row < NumRows) {
					endrow = row + bucketsize;
					if (endrow > NumRows) { // accomodate short last bucket
						bucketsize = NumRows - row;
						endrow = NumRows;
					}
					numWins = 0;
					score = 0;
					while (row < endrow) {
						yrow = row + posBase;
						numWins += pWins[yrow];
						score += pScores[yrow];
						++row;
						}//row
										
					pBucketScore[bucket] = score / bucketsize / NumP;
					pBucketPctWin[bucket] = numWins / bucketsize / NumP;

					bucket++;

				} // while

			} // not doing morph


			if (isSecondary) {
				pLowRanks = pLowRanksSec;
				pLowRanksIndex = pLowRanksIndexSec;
				NumTopGridSel = NumTopGridSelSec;
			}
			else {
				pLowRanks = pLowRanksCore;
				pLowRanksIndex = pLowRanksIndexCore;
				NumTopGridSel = NumTopGridSelCore;
			}

			// Check if GridSelector should be in TopGridSelCore vector
			for(bucket=0; bucket<NumBuckets; ++bucket) { // for each grid selector bucket
				score = pBucketScore[bucket];
				winpct = pBucketPctWin[bucket];

				rank = rankByWinPct ? winpct : score;
				if (rank > pLowRanks[y]) { // Found a winner! Do more calcs and save it.
					// Accumulate stocks
					row = bucket * BucketSize;
					endrow = row + BucketSize;
					if (endrow > NumRows) {
						endrow = NumRows;
						bucketsize = endrow - row;
						}
					for(i=row,j=0;i<endrow;++i,++j) { // accumulate stocks
						// TBD use the morph vector spec to find k
						k = XsortIndex[i];  // Note that we always use the last XsortIndex processed - this should be the current 
											// estimator memory!
						stocks[j] = pStockIDs[k];
						}
					
					// Determine if any of this genome's bucket score should be placed in a top scores list
					// Remember that there is one top scores list for each y (dependent) variable.
					// score				current score
					// LowRanksCore[y] 			Vector of lowest rank for each y (dependent) variable
					// LowRanksIndexCore[y]		Vector of lowest rank indices into TopGridSelCore[y][]
					// TopGridSelCore[y]		Vector of Vectors of top grid selectors. ie: genome + bucket
					// index				local latest low rank index 
					// lowRank				local latest low rank value 

					// Insert rank into top scores list. 
					// Note: insertion into this list is so infrequent that it is not worth 
					// maintaining the list in sorted order. The list is a simple object vector.
					
					index = pLowRanksIndex[y];
					// Create new vector of stocks
					Stack(Stocks) = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),3,TSYMBOL("Vector"),TSYMBOL("short"),TINT(bucketsize));
					if (Stack(Stocks).Tag != TYSHORTVECTOR) {
						FrameExit(TERROR("Error:math_egm_trainMachine: Stocks not vector of shorts"));
						}
					pStocks = (LpSHORT)FSmartbase_VectorPtr(gCP,gTP,Stack(Stocks));
					for (i=0;i<bucketsize;++i) pStocks[i] = stocks[i];

					// Create copy of genome vector
					Stack(GenomeCopy) = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),3,TSYMBOL("Vector"),TSYMBOL("short"),TINT(NumGenome));
					if (Stack(GenomeCopy).Tag != TYSHORTVECTOR) {
						FrameExit(TERROR("Error:math_egm_trainMachine: GenomeCopy not vector of shorts"));
						}
					pGenomeCopy = (LpSHORT)FSmartbase_VectorPtr(gCP,gTP,Stack(GenomeCopy));
					for (i=0;i<NumGenome;++i) pGenomeCopy[i] = pGenome[i]; // remainder of genome
					
					if (Morph)
					Stack(GridSelector) = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),21,
						TSYMBOL("Structure"),
						TSYMBOL("Genome"),	Stack(GenomeCopy), // genome
						TSYMBOL("Bucket"),	TINT(bucket),
						// Add in morphvector specification here
						TSYMBOL("BestMorphIdx"), TINT(BestMorphIdx),
						TSYMBOL("MorphDistance"), TINT(MorphDistance),
						TSYMBOL("MorphEven"), TINT(MorphEven),
						TSYMBOL("Rank"),	TREAL(rank), // rank is score or winpct based on rankByWinPct flag
						TSYMBOL("Score"),	TREAL(score),
						TSYMBOL("WinPct"),	TREAL(winpct),
						TSYMBOL("Stocks"),	Stack(Stocks), // stocks
						TSYMBOL("NumEst"),	TINT(NumEst)
						);
					else
					Stack(GridSelector) = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),15,
						TSYMBOL("Structure"),
						TSYMBOL("Genome"),	Stack(GenomeCopy), // genome
						TSYMBOL("Bucket"),	TINT(bucket),
						TSYMBOL("Rank"),	TREAL(rank), // rank is score or winpct based on rankByWinPct flag
						TSYMBOL("Score"),	TREAL(score),
						TSYMBOL("WinPct"),	TREAL(winpct),
						TSYMBOL("Stocks"),	Stack(Stocks), // stocks
						TSYMBOL("NumEst"),	TINT(NumEst)
						);


	//FSmartbase_Writeln(gCP,gTP,1,Stack(GridSelector));

					// (setq TopGridSelCore[y][index] GridSelector)
					if (isSecondary)
						Stack(TopGridSelElement) = FSmartbase_Ref(gCP,gTP,2,Stack(TopGridSelSec),TINT(y));
					else
						Stack(TopGridSelElement) = FSmartbase_Ref(gCP,gTP,2,Stack(TopGridSelCore),TINT(y));

					Stack(ec) = FSmartbase_Set(gCP,gTP,3,Stack(TopGridSelElement),TINT(index),Stack(GridSelector));

					// An insertion makes the summarized analysis invalid. Set the Summerized flag
					// so a subsequent call to summerize will be made.
					Stack(ec) = FSmartbase_Set(gCP,gTP,3,Stack(pvars),TSYMBOL("Summerized"),TBOOL(FALSE));

					// find new LowScores[y] and LowScoresIndex[y] values
					// Note: Again this operation is very infrequent so it is not worth
					// optimizing it to any great extent
					lowRank = BIGPOSNUM;
					for(i=0;i< NumTopGridSel; ++i) {
						Stack(GridSelector) = FSmartbase_Ref(gCP,gTP,2,Stack(TopGridSelElement),TINT(i));
						if (Stack(GridSelector).Tag == TYVOID) {
							index = i;
							lowRank = BIGNEGNUM;
							goto LOW_SCORE_DONE;
							}
						else {
							Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(GridSelector),TSYMBOL("Rank"));
							rank = asReal(&Stack(val));
							if (rank < lowRank) {
								index = i;
								lowRank = rank;
								}
							}
						}//i
					LOW_SCORE_DONE:
					// reset lowestScore for training expression
					pLowRanks[y] = lowRank;
					pLowRanksIndex[y] = index;

					} // end of insert
				}//bucket
			}//y - dependent variable index

//FSmartbase_Writeln(gCP,gTP,2,TSTRING("Genome="),Stack(Genome));

		SKIPCALC: // we jump here if we want to skip the calcs and just go to the next permutation in the genome
				  // This jump may be performed for processing types of Core and Secondary

			// By this point we have tried all of the morph vectors so we permute the remainder of the genome.
			for(d=NumGenome-1;d>=0;--d) { // NumGenome is the length of the Genome vector
				v = pGenome[d]; // v is the current field expression index for position d in the genome
				if (v < NumXSub1) { // if the field expression index is less than the number of field expressions less one, we bump it
					pGenome[d] = v + 1; // bump the field expression index by one
					for(i=d+1; i<NumGenome; ++i) // reset all field expression index to the right of positon d to 0
						pGenome[i] = 0;
					goto CONTINUE;
				}
			}

		// Check if we can extend genome or if we have reached max depth
		if (D >= MaxDepth) goto FINISHED;
		D++; // D is the depth of the genome
		// new values are 0 which are valid values field expression indicies and morph table index
		Stack(Genome) = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),4,TSYMBOL("Vector"),TSYMBOL("short"),TINT(D), TINT(0));
		pGenome = (LpSHORT)FSmartbase_VectorPtr(gCP,gTP,Stack(Genome));
		FSmartbase_Set(gCP,gTP,3,Stack(pvars),TSYMBOL("Genome"),Stack(Genome));
		NumGenome = (NUM) D;
		FSmartbase_Set(gCP,gTP,3,Stack(pvars),TSYMBOL("NumGenome"),TINT(NumGenome));
		NumGenomeSub1 = NumGenome - 1;
		FSmartbase_Set(gCP,gTP,3,Stack(pvars),TSYMBOL("NumGenomeSub1"),TINT(NumGenomeSub1));

		CONTINUE:
//FSmartbase_Writeln(gCP,gTP,2,TSTRING("Genome="),Stack(Genome));
		;
	}// end of while on time

	FINISHED:
	startTime = (REAL)((int)(((clock() - startTime) / (REAL) CLOCKS_PER_SEC) * 100)) / 100.0; // from time.h

	FSmartbase_Writeln(gCP,gTP,4,TSTRING("Genomes Tried "),TINT(genomesTried),TSTRING(" in "),TREAL(startTime));
	FrameExit(TBOOL(TRUE)); 
	}
/*********************************************************************
* RUN MACHINE
**********************************************************************/
/* Note on local stack space used by arrays in this routine
The following calculation is a rough guesstimate of the stack space consumed by the arrays in this routine
	LpREAL	pXYrows[MAXROWS];			4 * 1000		= 4000		
	REAL	rangeScores[MAXY];			8 * 3			=   24
	long	rangeCounts[MAXY];			8 * 3			=   24
	REAL	rangePcts[MAXY];			8 * 3			=   24
	REAL	recScores[MAXY][MAXROWS];	8 * 3 * 1000	=24000
	int		recCounts[MAXY][MAXROWS];	4 * 3 * 1000	=12000
	REAL	recPcts[MAXY][MAXROWS];		8 * 3 * 1000	=24000
	int		Xlist[MAXROWS + 1];			4 * 1001		= 4004
	REAL	Xsort[MAXROWS];				8 * 1000		= 8000
	int		XsortIndex[MAXROWS];		4 * 1000		= 4000
												  tot   80,076
	80K is not too large. I won't bother putting these arrays into the engines memory.
*/

PUBLIC TVAL math_egm_RunMachine(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]) {
	int		b,c,d,i,j,k,r,y,m,n,oc,p,s;		// loop counters and indicies
	SHORT	BestMorphIdx;
	int		bucket;					// index to a grid sction overlayed onto XY
	bool	bucketeven;
	int		bucketsize;				// local, not the BucketSize in pvars
	int		BucketSize;				// local, not the BucketSize in pvars - this is always the full bucketsize (not adjusted for short sets)
	// Coefficient vector for grid machine calc. This can be regenerated using the following
	// (setq Coefficients (new Vector: 1 1.0))
	// (loop for i from 1 to 10 (setq Coefficients[i] (/ 10 i)))
	static const REAL Coefficients[] = {1.0, 10.0, 5.0, 3.333333333333, 2.5, 2.0, 1.666666666667, 1.428571428571, 1.25, 1.111111111111};
	REAL	coef;					// single coefficient value
	long	count;
	int		endi;
	SHORT	endrow;
	REAL	maxScore;
	SHORT	maxmorphrows;
	REAL	maxPct;
	long	maxCount;
	SHORT	mi;						// index into morph vector entry mv
	REAL	minScore;
	REAL	minScores[MAXY];
	long	minCount;
	long	minCounts[MAXY];
	REAL	minPct;
	REAL	minPcts[MAXY];
	bool	Morph;					// Morph used flag
	SHORT	MorphDistance;
	bool	MorphEven;
	SHORT	morphidx;
	SHORT	mrow;
	SHORT	moffset;
	static const MorphVectors[126][10] = 
				{{0,1,2,3,4,5,6,7,8,9},
				{0,1,2,3,5,4,6,7,8,9},
				{0,1,2,3,6,4,5,7,8,9},
				{0,1,2,3,7,4,5,6,8,9},
				{0,1,2,3,8,4,5,6,7,9},
				{0,1,2,3,9,4,5,6,7,8},
				{0,1,2,4,5,3,6,7,8,9},
				{0,1,2,4,6,3,5,7,8,9},
				{0,1,2,4,7,3,5,6,8,9},
				{0,1,2,4,8,3,5,6,7,9},
				{0,1,2,4,9,3,5,6,7,8},
				{0,1,2,5,6,3,4,7,8,9},
				{0,1,2,5,7,3,4,6,8,9},
				{0,1,2,5,8,3,4,6,7,9},
				{0,1,2,5,9,3,4,6,7,8},
				{0,1,2,6,7,3,4,5,8,9},
				{0,1,2,6,8,3,4,5,7,9},
				{0,1,2,6,9,3,4,5,7,8},
				{0,1,2,7,8,3,4,5,6,9},
				{0,1,2,7,9,3,4,5,6,8},
				{0,1,2,8,9,3,4,5,6,7},
				{0,1,3,4,5,2,6,7,8,9},
				{0,1,3,4,6,2,5,7,8,9},
				{0,1,3,4,7,2,5,6,8,9},
				{0,1,3,4,8,2,5,6,7,9},
				{0,1,3,4,9,2,5,6,7,8},
				{0,1,3,5,6,2,4,7,8,9},
				{0,1,3,5,7,2,4,6,8,9},
				{0,1,3,5,8,2,4,6,7,9},
				{0,1,3,5,9,2,4,6,7,8},
				{0,1,3,6,7,2,4,5,8,9},
				{0,1,3,6,8,2,4,5,7,9},
				{0,1,3,6,9,2,4,5,7,8},
				{0,1,3,7,8,2,4,5,6,9},
				{0,1,3,7,9,2,4,5,6,8},
				{0,1,3,8,9,2,4,5,6,7},
				{0,1,4,5,6,2,3,7,8,9},
				{0,1,4,5,7,2,3,6,8,9},
				{0,1,4,5,8,2,3,6,7,9},
				{0,1,4,5,9,2,3,6,7,8},
				{0,1,4,6,7,2,3,5,8,9},
				{0,1,4,6,8,2,3,5,7,9},
				{0,1,4,6,9,2,3,5,7,8},
				{0,1,4,7,8,2,3,5,6,9},
				{0,1,4,7,9,2,3,5,6,8},
				{0,1,4,8,9,2,3,5,6,7},
				{0,1,5,6,7,2,3,4,8,9},
				{0,1,5,6,8,2,3,4,7,9},
				{0,1,5,6,9,2,3,4,7,8},
				{0,1,5,7,8,2,3,4,6,9},
				{0,1,5,7,9,2,3,4,6,8},
				{0,1,5,8,9,2,3,4,6,7},
				{0,1,6,7,8,2,3,4,5,9},
				{0,1,6,7,9,2,3,4,5,8},
				{0,1,6,8,9,2,3,4,5,7},
				{0,1,7,8,9,2,3,4,5,6},
				{0,2,3,4,5,1,6,7,8,9},
				{0,2,3,4,6,1,5,7,8,9},
				{0,2,3,4,7,1,5,6,8,9},
				{0,2,3,4,8,1,5,6,7,9},
				{0,2,3,4,9,1,5,6,7,8},
				{0,2,3,5,6,1,4,7,8,9},
				{0,2,3,5,7,1,4,6,8,9},
				{0,2,3,5,8,1,4,6,7,9},
				{0,2,3,5,9,1,4,6,7,8},
				{0,2,3,6,7,1,4,5,8,9},
				{0,2,3,6,8,1,4,5,7,9},
				{0,2,3,6,9,1,4,5,7,8},
				{0,2,3,7,8,1,4,5,6,9},
				{0,2,3,7,9,1,4,5,6,8},
				{0,2,3,8,9,1,4,5,6,7},
				{0,2,4,5,6,1,3,7,8,9},
				{0,2,4,5,7,1,3,6,8,9},
				{0,2,4,5,8,1,3,6,7,9},
				{0,2,4,5,9,1,3,6,7,8},
				{0,2,4,6,7,1,3,5,8,9},
				{0,2,4,6,8,1,3,5,7,9},
				{0,2,4,6,9,1,3,5,7,8},
				{0,2,4,7,8,1,3,5,6,9},
				{0,2,4,7,9,1,3,5,6,8},
				{0,2,4,8,9,1,3,5,6,7},
				{0,2,5,6,7,1,3,4,8,9},
				{0,2,5,6,8,1,3,4,7,9},
				{0,2,5,6,9,1,3,4,7,8},
				{0,2,5,7,8,1,3,4,6,9},
				{0,2,5,7,9,1,3,4,6,8},
				{0,2,5,8,9,1,3,4,6,7},
				{0,2,6,7,8,1,3,4,5,9},
				{0,2,6,7,9,1,3,4,5,8},
				{0,2,6,8,9,1,3,4,5,7},
				{0,2,7,8,9,1,3,4,5,6},
				{0,3,4,5,6,1,2,7,8,9},
				{0,3,4,5,7,1,2,6,8,9},
				{0,3,4,5,8,1,2,6,7,9},
				{0,3,4,5,9,1,2,6,7,8},
				{0,3,4,6,7,1,2,5,8,9},
				{0,3,4,6,8,1,2,5,7,9},
				{0,3,4,6,9,1,2,5,7,8},
				{0,3,4,7,8,1,2,5,6,9},
				{0,3,4,7,9,1,2,5,6,8},
				{0,3,4,8,9,1,2,5,6,7},
				{0,3,5,6,7,1,2,4,8,9},
				{0,3,5,6,8,1,2,4,7,9},
				{0,3,5,6,9,1,2,4,7,8},
				{0,3,5,7,8,1,2,4,6,9},
				{0,3,5,7,9,1,2,4,6,8},
				{0,3,5,8,9,1,2,4,6,7},
				{0,3,6,7,8,1,2,4,5,9},
				{0,3,6,7,9,1,2,4,5,8},
				{0,3,6,8,9,1,2,4,5,7},
				{0,3,7,8,9,1,2,4,5,6},
				{0,4,5,6,7,1,2,3,8,9},
				{0,4,5,6,8,1,2,3,7,9},
				{0,4,5,6,9,1,2,3,7,8},
				{0,4,5,7,8,1,2,3,6,9},
				{0,4,5,7,9,1,2,3,6,8},
				{0,4,5,8,9,1,2,3,6,7},
				{0,4,6,7,8,1,2,3,5,9},
				{0,4,6,7,9,1,2,3,5,8},
				{0,4,6,8,9,1,2,3,5,7},
				{0,4,7,8,9,1,2,3,5,6},
				{0,5,6,7,8,1,2,3,4,9},
				{0,5,6,7,9,1,2,3,4,8},
				{0,5,6,8,9,1,2,3,4,7},
				{0,5,7,8,9,1,2,3,4,6},
				{0,6,7,8,9,1,2,3,4,5}};
	REAL	mv;						// morph vector entry
	int		numEst;
	int		NumGenome;
	int		NumMorphVectors;
	int		NumMorphVector;
	int		NumRows;
	int		NumTopGridSel;
	int		NumTopGridSelCount;		// Actual number of top grid selectors processed
	int		NumTopGridSelCore;	
	int		NumTopGridSelSec;
	int		NumY;
	int		NumX;
	int		pop;
	LpSHORT	pMorphVector;			// pointer to start of MorphVector in pvars
	LpSHORT	pGenome;				// pointer to start of Genome vector in pvars
	LpSHORT	pRecFields;
	LpSHORT	pTemp;
	LpREAL	pXYrows[MAXROWS];		// array of pointers to start of each rows real vector of values
	REAL	rangeScore;
	REAL	rangeScores[MAXY];
	long	rangeCounts[MAXY];
	REAL	rangePcts[MAXY];
	short	recFields[MAXY][MAXROWS][MAXFIELDS][MAXDEPTH];
	REAL	recScore;
	REAL	recScores[MAXY][MAXROWS];
	int		recCount;
	int		recCounts[MAXY][MAXROWS];
	REAL	recPct;
	REAL	recPcts[MAXY][MAXROWS];
	int		row;					// row index into XY
	// Scale is a vector of constants used in the grid machine calc. It can be easily
	// calculated but is used in this fashion for speed of processing. Be sure to change
	// this vector if you allow a MaxDepth of more than 10!
	static const REAL Scale[] = {1.0, 1.0, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, 10000000.0, 100000000.0};
	REAL	scale;					// single scale value
	REAL	score;					// score accumulator
	REAL	sortval;				// sort value accumulator
	REAL	startTime;
	SHORT	stocks[MAXBUCKETSIZE];	// accumulator vector for stocks picked by individual grid selectors (used for diversity analysis in debugging phase)
	REAL	winPct;					// percentage of winning picks in a bucket. ex: (/ numWins BucketSize)
	SHORT	v;
	SHORT	xi;						// index into rowTemp for x (independent) variables
	int		Xlist[MAXROWS + 1];
	REAL	Xsort[MAXROWS];		// interim vector result of score for a genome
	int		XsortIndex[MAXROWS];	// sorted index into xy based on scores in Xsort
	REAL	xvalue;					// xvalue from a XY row. ex: (setq xvalue rowTemp[xi])

	StartFrame
	DeclareTVAL(Bucket);
	DeclareTVAL(cursor);
	DeclareTVAL(ec);
	DeclareTVAL(egmInstancePvars);
	DeclareTVAL(estVector);
	DeclareTVAL(Genome);
	DeclareTVAL(GridSelector);
	DeclareTVAL(ID);
	DeclareTVAL(inputCursorLambda);
	DeclareTVAL(pvars);
	DeclareTVAL(record);
	DeclareTVAL(rowVector);
	DeclareTVAL(RecFields);
	DeclareTVAL(ScoreStructure);
	DeclareTVAL(SpecialSituationNotes);
	DeclareTVAL(temp);
	DeclareTVAL(TopGridSel);
	DeclareTVAL(TopGridSelCore);
	DeclareTVAL(TopGridSelSec);
	DeclareTVAL(GridSelectors);
	DeclareTVAL(val);
	DeclareTVAL(XsortIndex2);
	DeclareTVAL(Xsort2);
	DeclareTVAL(xy);
	DeclareTVAL(XYrow);
	EndFrame

	// Get arguments
	if (argc < 2) FrameExit(TERROR((LpCHAR) "math_egm_TrainMachine arglist - 2+ arguments required")); // make sure there are enough arguments

	// First argument is pvars structure of egm Lambda instance 
// the following line should work? Mike?
//	if (!isObject(&argv[0])) FrameExit(TERROR((LpCHAR) "math_egm_TrainMachine - 1st argument must be pvars of egm instance"));
	Stack(pvars) = argv[0];

	// Second argument is cursor that runMachine should place ranks into
	Stack(cursor) = argv[1];

	// Third argument is optional vector of estimator Lambda memories for previous periods
	//if (!isNumIndex(&argv[1])) FrameExit(TERROR((LpCHAR) "math_egm_TrainMachine - 2nd argument must be number of seconds to train"));
	if (argc > 2) 
		Stack(estVector) = argv[2];
	else
		Stack(estVector) = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),3,TSYMBOL("Vector"),TSYMBOL("object"),TINT(0));

	if (Stack(estVector).Tag != TYDIRECTORY) FrameExit(TERROR("Error:math_egm_runMachine: estVector not directory object"));
	numEst = (int)FSmartbase_VectorLen(gCP,gTP,Stack(estVector));

	startTime = clock();

	// Get local copies of pvar values
	Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("NumRows"));
	if (Stack(val).Tag != TYNUM) FrameExit(TERROR("Error:math_egm_runMachine: NumRows not an integer."));
	NumRows = (int)asNumIndex(&Stack(val));

	// _inputCursor uses previously initialized pvars: baseExpressions, fieldExpressions and trainExpressions
	Stack(inputCursorLambda) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("_inputCursor"));
	Stack(val) = FSmartbase_Eval(gCP,gTP,Stack(inputCursorLambda),1,Stack(cursor)); 
	Stack(xy) = FSmartbase_Ref(gCP,gTP,2,Stack(val),TSYMBOL("xy"));

	if (Stack(xy).Tag != TYOBJVECTOR) FrameExit(TERROR("Error:math_egm_runMachine: XY not vector of objects"));
	// Get pointers to vectors of values in pvars
	for(i=0; i < NumRows; i++) {
		Stack(XYrow) = FSmartbase_Ref(gCP,gTP,2,Stack(xy),TINT(i));
		if (Stack(XYrow).Tag != TYNUMVECTOR) FrameExit(TERROR("Error:math_egm_runMachine: XY row not vector of reals"));
		pXYrows[i] = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,Stack(XYrow));
		}

	Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("BucketSize"));
	if (Stack(val).Tag != TYNUM) FrameExit(TERROR("Error:math_egm_runMachine: BucketSize not an integer."));
	BucketSize = (int)asNumIndex(&Stack(val));

	Stack(val)=FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("NumY"));
	if (Stack(val).Tag != TYNUM) FrameExit(TERROR("Error:math_egm_runMachine: NumY not an integer."));
	NumY = (NUM)asNumIndex(&Stack(val));

	Stack(val)=FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("NumX"));
	if (Stack(val).Tag != TYNUM) FrameExit(TERROR("Error:math_egm_runMachine: NumX not an integer."));
	NumX = (NUM)asNumIndex(&Stack(val));

	Stack(val)=FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("NumTopGridSelCore"));
	if (Stack(val).Tag != TYNUM) FrameExit(TERROR("Error:math_egm_runMachine: NumTopGridSelCore not an integer."));
	NumTopGridSelCore = (NUM)asNumIndex(&Stack(val));

	Stack(val)=FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("NumTopGridSelSec"));
	if (Stack(val).Tag != TYNUM) FrameExit(TERROR("Error:math_egm_runMachine: NumTopGridSelSec not an integer."));
	NumTopGridSelSec = (NUM)asNumIndex(&Stack(val));

	Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("Morph"));
	Morph = (asInt(&Stack(val)) == 1);

	// Perform summerize on instances if necessary and do cross period analysis if any
	for(p=0; p<numEst; ++p) {
		Stack(egmInstancePvars) = FSmartbase_Ref(gCP,gTP,3,Stack(estVector),TINT(p),TINT(1));
		if (Stack(egmInstancePvars).Tag != TYSTRUCTURE) FrameExit(TERROR("math_egm_runMachine: egmInstancePvar not structure."));
		Stack(ec) = FSmartbase_Ref(gCP,gTP,2,Stack(egmInstancePvars),TSYMBOL("BucketSize"));
		if (Stack(ec).Tag != TYNUM) FrameExit(TERROR("Error:math_egm_runMachine: est memory bucketsize not an integer."));
		y = asInt(&Stack(ec));

		Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(egmInstancePvars),TSYMBOL("Summerized"));
		if (Stack(val).Tag != TYBOLE) FrameExit(TERROR("Error:math_egm_runMachine: est memory summersized not a boolean."));
		if (asBool(&Stack(val)) != TRUE) {
			Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(pvars),TSYMBOL("summerize"));
			Stack(egmInstancePvars) = FSmartbase_Eval(gCP,gTP,Stack(val),1,Stack(egmInstancePvars));
			Stack(ec) = FSmartbase_Eval(gCP,gTP,TGVALUE("summerize"),1,Stack(egmInstancePvars));
			Stack(ec) = FSmartbase_Set(gCP,gTP,3,Stack(estVector),TINT(p),Stack(egmInstancePvars));		
			}
		}

	// Find the number of rows that contain an even number of buckets to allow us to "morph" these
	// buckets later. Ignore odd buckets and or partial buckes because they can not be morped later.
	maxmorphrows = ((int) NumRows / (BucketSize * 2)) * (BucketSize * 2);

	for(y=0; y<NumY; ++y) { // For each dependent variable
		// Clear accumulators for current dependent variables
		for(r=0; r<NumRows; ++r) {
			recScores[y][r] = 0;
			recPcts[y][r] = 0;
			recCounts[y][r] = 0;
			for(j=0; j<MAXFIELDS; ++j)
				for (k=0; k<MAXDEPTH; ++k)
					recFields[y][r][j][k] = 0;
			}//r


		for(p=0; p<numEst; ++p) {
			// Grab the top grid selectors list from the estimator Lambda memory for period p
			Stack(egmInstancePvars) = FSmartbase_Ref(gCP,gTP,3,Stack(estVector),TINT(p),TINT(1));
			if (Stack(egmInstancePvars).Tag != TYSTRUCTURE) FrameExit(TERROR("Error:math_egm_runMachine: egmInstancePvar not structure."));


			NumTopGridSelCount = 0;

			for(pop=0; pop<2; ++pop) { // for each population (core and secondary)

				if (pop == 0) {
					Stack(TopGridSel) = FSmartbase_Ref(gCP,gTP,2,Stack(egmInstancePvars),TSYMBOL("TopGridSelCore"));
					NumTopGridSel = NumTopGridSelCore;
				}
				else {
					Stack(TopGridSel) = FSmartbase_Ref(gCP,gTP,2,Stack(egmInstancePvars),TSYMBOL("TopGridSelSec"));
					NumTopGridSel = NumTopGridSelSec;
				}

					
			
				if (Stack(TopGridSel).Tag != TYOBJVECTOR) FrameExit(TERROR("Error:math_egm_runMachine: est memory TopGridSelCore or TopGridSelSec not vector."));
				Stack(GridSelectors) = FSmartbase_Ref(gCP,gTP,2,Stack(TopGridSel),TINT(y));
				if (Stack(GridSelectors).Tag != TYOBJVECTOR) FrameExit(TERROR("Error:math_egm_runMachine: est memory GridSelectors not structure."));


				// Apply each of the grid selectors to the cursor's xy matrix
				for(s=0; s<NumTopGridSel; ++s) {
				
					Stack(GridSelector) = FSmartbase_Ref(gCP,gTP,2,Stack(GridSelectors),TINT(s));
//					if (Stack(GridSelector).Tag != TYSTRUCTURE) FrameExit(TERROR("Error:math_egm_runMachine: est memory GridSelector not structure."));
					if (Stack(GridSelector).Tag == TYVOID) 
						continue; // no grid selector so go to next s index 
					++NumTopGridSelCount;

					Stack(Genome) = FSmartbase_Ref(gCP,gTP,2,Stack(GridSelector),TSYMBOL("Genome"));
					pGenome = (LpSHORT)FSmartbase_VectorPtr(gCP,gTP,Stack(Genome));
					NumGenome = (SHORT)FSmartbase_VectorLen(gCP,gTP,Stack(Genome));

					Stack(Bucket) = FSmartbase_Ref(gCP,gTP,2,Stack(GridSelector),TSYMBOL("Bucket"));
					bucket = asInt(&Stack(Bucket));

					Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(GridSelector),TSYMBOL("Score"));
					score = asReal(&Stack(val));

					Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(GridSelector),TSYMBOL("WinPct"));
					winPct = asReal(&Stack(val));

					if (Morph) {
						Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(GridSelector),TSYMBOL("BestMorphIdx"));
						BestMorphIdx = (SHORT) asInt(&Stack(val));

						Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(GridSelector),TSYMBOL("MorphDistance"));
						MorphDistance = (SHORT)asInt(&Stack(val));

						Stack(val) = FSmartbase_Ref(gCP,gTP,2,Stack(GridSelector),TSYMBOL("MorphEven"));
						MorphEven = (asInt(&Stack(val)) == 1);
						}



			// writeln for debugging 
	//Stack(ec) = FSmartbase_Writeln(gCP,gTP,8,TSTRING("pop="),TINT(pop),TSTRING(" TopGridSel["),TINT(y),TSTRING("]["),TINT(bucket),TSTRING("]"),Stack(GridSelector));
					
					// Clear Xsort, XsortIndex and Xlist
					for(i=0; i < MAXROWS; ++i) {
						Xsort[i] = 0;
						XsortIndex[i] = 0;
						Xlist[i] = -1;
						}
					Xlist[MAXROWS] = -1; //Xlist is one element longer than the other vectors

					// Generate values for current genome and build Xlist
					// Xlist is a linked list used to generate XsortIndex ie: list sort 
					int listStart = -1;
					for(row=0; row < NumRows; ++row) {
						j = 0;
						sortval = 0;
						for(d=0; d < NumGenome; ++d, ++j) {
							xi = pGenome[d];
							xvalue = pXYrows[row][xi];
							if (j == 0)
								sortval += (Scale[j] * Coefficients[j] * xvalue);
							else				
								sortval += (Scale[j] * (NUM) (Coefficients[j] * xvalue));
							}
						Xsort[row] = sortval;
					// Notes on Simple List Insertion Sort
					// Insert into Xlist -- this is the sorted linked list
					// The Xsort[] array is the array of sort values. The values in this array do not move.
					// The Xsort[] array grows by one with each iteration through this loop. Each new sort value
					// is inserted at element position i.
					// The Xlist[] array contains integer values that are the index of another element in the
					// Xlist[] array or the seed value of -1 which means the element is not yet assigned.
					// Xlist[] array positions relate to Xsort array postions with an offset of one. For instance,
					// Xlist[2] is related to Xsort[1]. This allows the Xlist[0] element to contain the index to
					// the first Xlist[] element that relates to an Xsort[] element. This makes the Xlist array
					// one element longer than the Xsort[] array.
					// The objective of the fully populated Xlist[] array is to allow the traversal of the Xlist[]
					// array by using the values in Xlist[][ as indicies into other Xlist[] elements. When on a given
					// Xlist[] element you can find the sorted value in the Xsort[] array using the Xlist[] element
					// index - 1. 
					// Xlist[] is built using the simple insertion logic below:
						k = row + 1;
						c = 0;
						LOOP:
						oc = Xlist[c];
						if (oc == -1) { // Add to end of chain
							Xlist[c] = k;
							goto DONE;
							}
						if (Xsort[oc - 1] > sortval) { // Insert into chain
							Xlist[k] = oc;
							Xlist[c] = k;
							goto DONE;
							}
						c = Xlist[c]; // get index for next linked list index.
						goto LOOP;
						DONE:
						;
						}//row

					// Generate the XsortIndex vector
					//(while (<> (setq c xi[c]) -1) (begin (setq result[i] v[(- c 1)]) (setq i (+ i 1))))
					c = Xlist[0];
					i = 0;
					while (c != -1) { 
						XsortIndex[i++] = c - 1; 
						c = Xlist[c];
						}

					row = bucket * BucketSize;
					endrow = row + BucketSize;
					if (endrow > NumRows) endrow = NumRows; // accomodate short last bucket

					if (Morph) {
						// For each stock selected by the current bucket we need to accumulate the
						// score and winPct associated with the bucket into that stock. XsortIndex and the 
						// morphvector specification is used to find the correct stocks.
						// First we calculate the morphing values for current grid selector
						bucketeven = (( bucket % 2) == 0);
						if ((bucketeven && MorphEven) || (!bucketeven && !MorphEven)) { // Morph using first morphvector part
							morphidx = 0;
							moffset = MorphDistance * BucketSize;
							}
						else { // Morph using second morphvector part
							morphidx = BucketSize; // The morph vector is always 2 * BucketSize in length.
							moffset = -(MorphDistance * BucketSize);
							}
						i = 0;
						while(row < endrow) {
							// mrow is index into xy for stock falling ino this bucket at row 
							// Now use the morphvector specification for bucket
							if (row < maxmorphrows) { // do not morph odd remainder or partial bucket 
								mrow = moffset + MorphVectors[BestMorphIdx][i + morphidx];
								if (mrow > maxmorphrows) mrow = mrow - maxmorphrows; // wrap around if past end
								if (mrow < 0) mrow = maxmorphrows + mrow; // wrap around if before  beginning
								++i;
								}
							else 
								mrow = row;
							mrow = XsortIndex[mrow]; 
							recScores[y][mrow] += score;
							recCounts[y][mrow]++;
							recPcts[y][mrow] += winPct;
							for(d=0; d < NumGenome; ++d)
								recFields[y][mrow][pGenome[d]][d]++;
							++row;
							}//row
						} // morph
					else { // Don't morph
						while (row < endrow) {
							mrow = XsortIndex[row]; 
							recScores[y][mrow] += score;
							recCounts[y][mrow]++;
							recPcts[y][mrow] += winPct;
							for(d=0; d < NumGenome; ++d)
								recFields[y][mrow][pGenome[d]][d]++;
							++row;
							}
						} // Don't mroph
					}//s
				}//pop
			}//p

			minScore = BIGPOSNUM;
			maxScore = BIGNEGNUM;
			minCount = BIGPOSINT;
			maxCount = BIGNEGINT;
			minPct = BIGPOSNUM;
			maxPct = BIGNEGNUM;

			for(n=0; n<NumRows; ++n) {
				count = recCounts[y][n];
				if (count > 0) {
//	FSmartbase_Writeln(gCP,gTP,4,TSTRING("recCount["),TINT(n),TSTRING("]="),TREAL(recCounts[y][n]));
					if (count < minCount) minCount = count;
					if (count > maxCount) maxCount = count;
					score = recScores[y][n] / count;
					recScores[y][n] = score;
					if (score < minScore) minScore = score;
					if (score > maxScore) maxScore = score;
					winPct = recPcts[y][n] / count;
					recPcts[y][n] = winPct;
					if (winPct < minPct) minPct = winPct;
					if (winPct > maxPct) maxPct = winPct;
					}
				}//n
			minScores[y] = minScore;
			minCounts[y]= minCount;
			minPcts[y] = minPct;
			rangeScores[y] = maxScore - minScore;
			rangeCounts[y] = maxCount - minCount;
			rangePcts[y] = maxPct - minPct;

/*
Stack(ec) = FSmartbase_Writeln(gCP,gTP,1,TSTRING(""));

Stack(ec) = FSmartbase_Writeln(gCP,gTP,2,TSTRING("maxCount="),TREAL(maxCount));
Stack(ec) = FSmartbase_Writeln(gCP,gTP,2,TSTRING("minCount="),TREAL(minCount));

Stack(ec) = FSmartbase_Writeln(gCP,gTP,2,TSTRING("maxScore="),TREAL(maxScore));
Stack(ec) = FSmartbase_Writeln(gCP,gTP,2,TSTRING("minScore="),TREAL(minScore));

Stack(ec) = FSmartbase_Writeln(gCP,gTP,2,TSTRING("maxPct="),TREAL(maxPct));
Stack(ec) = FSmartbase_Writeln(gCP,gTP,2,TSTRING("minPct="),TREAL(minPct));

Stack(ec) = FSmartbase_Writeln(gCP,gTP,1,TSTRING(""));


//(writeln "minCounts["y"]=" minCounts[y])
Stack(ec) = FSmartbase_Writeln(gCP,gTP,4,TSTRING("minCounts["),TINT(y),TSTRING("]="),TREAL(minCounts[y]));
//(writeln "minScores["y"]=" minScores[y])
Stack(ec) = FSmartbase_Writeln(gCP,gTP,4,TSTRING("minScores["),TINT(y),TSTRING("]="),TREAL(minScores[y]));
//(writeln "minPcts["y"]=" minPcts[y])
Stack(ec) = FSmartbase_Writeln(gCP,gTP,4,TSTRING("minPcts["),TINT(y),TSTRING("]="),TREAL(minPcts[y]));
//(writeln "rangeScores["y"]=" rangeScores[y])
Stack(ec) = FSmartbase_Writeln(gCP,gTP,4,TSTRING("rangeScores["),TINT(y),TSTRING("]="),TREAL(rangeScores[y]));
//(writeln "rangeCounts["y"]=" rangeCounts[y])
Stack(ec) = FSmartbase_Writeln(gCP,gTP,4,TSTRING("rangeCounts["),TINT(y),TSTRING("]="),TREAL(rangeCounts[y]));
//(writeln "rangePcts["y"]=" rangePcts)
Stack(ec) = FSmartbase_Writeln(gCP,gTP,4,TSTRING("rangePcts["),TINT(y),TSTRING("]="),TREAL(rangePcts[y]));
*/
		}//y

FSmartbase_Writeln(gCP,gTP,2,TSTRING("NumTopGridSelCount"),TINT(NumTopGridSelCount));


		for(n=0; n<NumRows; ++n) {
			for(y=0; y<NumY; ++y) {
				recCount = recCounts[y][n];
	
				if (recCount > 0) {
					recScore = recScores[y][n];
					recPct = recPcts[y][n];
					rangeScore = rangeScores[y];

					Stack(rowVector) = FSmartbase_Ref(gCP,gTP,2,Stack(cursor),TSYMBOL("rowVector"));
					Stack(record) = FSmartbase_Ref(gCP,gTP,2,Stack(rowVector),TINT(n));
					Stack(ID) = FSmartbase_Ref(gCP,gTP,2,Stack(record),TSYMBOL("ID"));
					Stack(RecFields) = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),3,TSYMBOL("Vector"),TSYMBOL("object"),TINT(NumX));
					//pRecFields = (LpSHORT)FSmartbase_VectorPtr(gCP,gTP,Stack(RecFields));
					for(j=0; j < NumX; ++j) {
						Stack(temp) = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),3,TSYMBOL("Vector"),TSYMBOL("short"),TINT(MAXDEPTH));
						pTemp = (LpSHORT)FSmartbase_VectorPtr(gCP,gTP,Stack(temp));
						for(k=0;k < MAXDEPTH;++k)
							pTemp[k] = recFields[y][n][j][k];
						Stack(ec) = FSmartbase_Set(gCP,gTP,3,Stack(RecFields),TINT(j),Stack(temp));
						if (Stack(ec).Tag == TYERROR) FrameExit(TERROR("Error:math_egm_runMachine: could not set recFields count."));
					}
					Stack(SpecialSituationNotes) = FSmartbase_Ref(gCP,gTP,2,Stack(record),TSYMBOL("SpecialSituationNotes"));
					if (Stack(SpecialSituationNotes).Tag == TYVOID) { 
						Stack(SpecialSituationNotes) = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),3,TSYMBOL("Vector"),TSYMBOL("object"),TINT(NumY));
						Stack(ec) = FSmartbase_Set(gCP,gTP,3,Stack(record),TSYMBOL("SpecialSituationNotes"),Stack(SpecialSituationNotes));
						if (Stack(ec).Tag == TYERROR) FrameExit(TERROR("Error:math_egm_runMachine: could not set SpecialSituationNotes."));
						}

					Stack(ScoreStructure) = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),17,
								TSYMBOL("Structure"),
								TSYMBOL("Avg"),
								TREAL(recScore / recCount * 100.0),
								TSYMBOL("Rnk"),
								TREAL((recScore - minScore) / rangeScore * 100.0),
//								TREAL( (recCount - minCount) / rangeCounts[y] ),
								TSYMBOL("Count"),
								TINT(recCount),
								TSYMBOL("RnkCount"),
								TREAL( (recCount - minCount) / rangeCounts[y] * 100.0),
								TSYMBOL("WinPct"),
								TREAL( recPct * 100.0 ),
								TSYMBOL("CountPct"),
								TREAL( recCount / (numEst * NumTopGridSelCount) * 100.0),
								TSYMBOL("ID"),
								Stack(ID),
								TSYMBOL("RecFields"),
								Stack(RecFields)
								);

					Stack(ec) = FSmartbase_Set(gCP,gTP,3,Stack(SpecialSituationNotes),TINT(y),Stack(ScoreStructure));
					if (Stack(ec).Tag == TYERROR) FrameExit(TERROR("Error:math_egm_runMachine: could not set SpecialSituationNotes."));
					}
				}//y

			}//n

	startTime = (REAL)((int)(((clock() - startTime) / (REAL) CLOCKS_PER_SEC) * 100)) / 100.0; // from time.h
	FSmartbase_Writeln(gCP,gTP,2,TSTRING("runMachine completed in "),TREAL(startTime));
	FrameExit(TBOOL(TRUE)); 
	}
