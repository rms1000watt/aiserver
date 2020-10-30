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

#define _C_TNeuralNet
#define _SMARTBASE
#if 0
TNeuralNet.c

Interface for the NeuralNet object which supports the functions required for the
implementation of Back Propagation Neural Nets. TNeuralNet is a defined structure 
of TStructure.

AUTHORS:            Michael F. Korns

#endif

#include "tneural.h"
#include "tobjvec.h"
#include "tsymbol.h"
#include "fpropty.h"



/*--------------------------------------------------------------------------------------- */
#if 0
TNeuralNet_Init

Initialize the TNeuralNet class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TNeuralNet_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareTVALArray(prmv,4);
EndFrame

/*  Don't initialize more than once. */
if (gCP->TNeuralNet_Initialized) return;
gCP->TNeuralNet_Initialized  = TRUE;

/*  Register the NeuralNet structure to SmartBase */

gCP->TNeuralNet_NeuralNetSym = TSymbol_MakeUnique(gCP,gTP,"NeuralNet");
FObject_Perm(gCP,gTP,(TObject*)gCP->TNeuralNet_NeuralNetSym,TRUE);

FSmartbase_Evals(gCP,
				 gTP,
				"(defineStructure NeuralNet: "
                                    "input: "
                                    "hidden: "
                                    "output: "
                                    "momentum: "
                                    "learning: "
                                    "filter: "
                                    "theta:"
                                    "notes:"
                                    "error:)"
                 ,FALSE);

gCP->TNeuralNet_NeuralLayerSym = TSymbol_MakeUnique(gCP,gTP,"NeuralLayer");
FObject_Perm(gCP,gTP,(TObject*)gCP->TNeuralNet_NeuralLayerSym,TRUE);

FSmartbase_Evals(gCP,
				 gTP,
				"(defineStructure NeuralLayer: "
                                    "inputs: "
                                    "outputs: "
                                    "weights: "
                                    "biases: "
                                    "deltas: "
                                    "errors:)"
                 ,FALSE);

gCP->TNeuralNet_ContinuousSym = TSymbol_MakeUnique(gCP,gTP,"continuous");
FObject_Perm(gCP,gTP,(TObject*)gCP->TNeuralNet_ContinuousSym,TRUE);
gCP->TNeuralNet_SigmoidalSym = TSymbol_MakeUnique(gCP,gTP,"sigmoid");
FObject_Perm(gCP,gTP,(TObject*)gCP->TNeuralNet_SigmoidalSym,TRUE);
gCP->TNeuralNet_BinarySym = TSymbol_MakeUnique(gCP,gTP,"binary");
FObject_Perm(gCP,gTP,(TObject*)gCP->TNeuralNet_BinarySym,TRUE);
gCP->TNeuralNet_BipolarSym = TSymbol_MakeUnique(gCP,gTP,"bipolar");
FObject_Perm(gCP,gTP,(TObject*)gCP->TNeuralNet_BipolarSym,TRUE);

/*  Register all NeuralNet procedures to SmartBase */

FSmartbase_RegisterCProcedure(gCP,gTP,"makeNeuralNet",(LpFUNC)&TNeuralNet_MakeNeuralNet);

/*  Register all NeuralNet methods to SmartBase */

prmv[0] = TSYMBOL("NeuralNet");
prmv[1] = TSYMBOL("clear");
prmv[2] = TFUNCTION(TNeuralNet_Clear);
FProperty_AddMethod(gCP,gTP,3,&prmv[0]);

prmv[0] = TSYMBOL("NeuralNet");
prmv[1] = TSYMBOL("getWeights");
prmv[2] = TFUNCTION(TNeuralNet_GetWeights);
FProperty_AddMethod(gCP,gTP,3,&prmv[0]);

prmv[0] = TSYMBOL("NeuralNet");
prmv[1] = TSYMBOL("propagateForward");
prmv[2] = TFUNCTION(TNeuralNet_PropagateForward);
FProperty_AddMethod(gCP,gTP,3,&prmv[0]);

prmv[0] = TSYMBOL("NeuralNet");
prmv[1] = TSYMBOL("propagateBackward");
prmv[2] = TFUNCTION(TNeuralNet_PropagateBackward);
FProperty_AddMethod(gCP,gTP,3,&prmv[0]);

prmv[0] = TSYMBOL("NeuralNet");
prmv[1] = TSYMBOL("propagateUserError");
prmv[2] = TFUNCTION(TNeuralNet_PropagateUserError);
FProperty_AddMethod(gCP,gTP,3,&prmv[0]);

prmv[0] = TSYMBOL("NeuralNet");
prmv[1] = TSYMBOL("computeError");
prmv[2] = TFUNCTION(TNeuralNet_ComputeError);
FProperty_AddMethod(gCP,gTP,3,&prmv[0]);

FrameReturn;
}

/*--------------------------------------------------------------------------------------- */
#if 0
TNeuralNet_MakeNeuralNet

Create a NeuralNet Object from the specified arguments.

		(makeNeuralNet inputs outputs filter)

		(makeNeuralNet inputs hidden outputs filter)

		(makeNeuralNet weightsVector)

Note:   This procedure is used in place of the standard SmartLisp factory procedure.

#endif

TVAL    TNeuralNet_MakeNeuralNet(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM         NumberOfInputs;
NUM         NumberOfOutputs;
NUM         NumberOfHidden;
NUM         i;
TVAL		newCmd = TFUNCTION(FProperty_Factory);
TVAL		Temp;
StartFrame
DeclareTVAL(makevector);
DeclareTVAL(NeuralLayer);
DeclareTVAL(aOV);
DeclareTVAL(aNL);
DeclareTVAL(aNN);
DeclareTVAL(ret);
DeclareTVAL(factory);
DeclareTVAL(NeuralNet);
DeclareTVAL(Vector);
DeclareTVAL(NumberSym);
DeclareTVAL(ObjectSym);
DeclareTVAL(filter);
DeclareTVAL(makeNeuralNet);
DeclareTVAL(clear);
EndFrame

*makevector  = TGVALUE("makeVector");
*NeuralLayer   = TGVALUE("NeuralLayer");
*makeNeuralNet   = TFUNCTION(TNeuralNet_MakeNeuralNet);
*clear   = TFUNCTION(TNeuralNet_Clear);

/*  Create the raw NeuralNet object with all members set to #void. */

*factory = TSYMBOL("NeuralNet");
*NeuralNet = *aNN = FSmartbase_Eval(gCP,gTP,newCmd,1,*factory);

/*  Check the mandatory arguments. */

if ((argc == 1) && (argv[0].Tag == TYVECTOR))
    {
	/* Make a NeuralNet from the inputs, output, and filter arguments */
	/* previously saved in the NeuralNet weights vector. */
    
	*Vector = argv[0];
	*aNN = FSmartbase_Eval(gCP,gTP,*makeNeuralNet,4,
										TvalArray(*Vector)[0],
										TvalArray(*Vector)[1],
										TvalArray(*Vector)[2],
										TvalArray(*Vector)[3]);
	ExitOnError(*aNN);

	/* Restore the arguments previously saved in the NeuralNet weights vector. */

	*aNN = FSmartbase_Eval(gCP,gTP,*clear,2,*aNN,*Vector);
	FrameExit(*aNN);
    }
else
if (argc == 3)
	/* Restore the arguments previously saved in the NeuralNet weights vector. */
    {
	if (!isNumIndex(&argv[0]))
		{
		*ret = TERROR("!makeNeuralNet:  Expecting number of inputs argument to be a number!");
		FrameExit(*ret);
		}
	else
	if (!isNumIndex(&argv[1]))
		{
		*ret = TERROR("!makeNeuralNet:  Expecting number of outputs argument to be a number!");
		FrameExit(*ret);
		}
	else
	if (argv[2].Tag != TYSYMBOL)
		{
		*ret = TERROR("!makeNeuralNet:  Expecting filter argument to be a symbol!");
		FrameExit(*ret);
		}

	/* Save all the mandatory arguments. */

	NumberOfInputs = asNumIndex(&argv[0]);
	NumberOfOutputs = asNumIndex(&argv[1]);
	*filter = argv[2];

	/* Compute the proper number of hidden  */
	/* layer neural units using the Hecht-  */
	/* Nielsen theorem.                     */

	NumberOfHidden = (2 * NumberOfInputs) + 1;
	}
else
if (argc == 4)
	/* Restore the arguments previously saved in the NeuralNet weights vector. */
    {
	if (!isNumIndex(&argv[0]))
		{
		*ret = TERROR("!makeNeuralNet:  Expecting number of inputs argument to be a number!");
		FrameExit(*ret);
		}
	else
	if ((!isNumIndex(&argv[1])) || (asNumIndex(&argv[1]) < 0))
		{
		*ret = TERROR("!makeNeuralNet:  Expecting number of hidden argument to be a positive number!");
		FrameExit(*ret);
		}
	else
	if (!isNumIndex(&argv[2]))
		{
		*ret = TERROR("!makeNeuralNet:  Expecting number of outputs argument to be a number!");
		FrameExit(*ret);
		}
	else
	if (argv[3].Tag != TYSYMBOL)
		{
		*ret = TERROR("!makeNeuralNet:  Expecting filter argument to be a symbol!");
		FrameExit(*ret);
		}

	/* Save all the mandatory arguments. */

	NumberOfInputs = asNumIndex(&argv[0]);
	NumberOfHidden = asNumIndex(&argv[1]);
	NumberOfOutputs = asNumIndex(&argv[2]);
	*filter = argv[3];
	}
else
    {
	*ret = TERROR("!makeNeuralNet: invalid number of arguments !");
	FrameExit(*ret);
    }

/* Create the input NeuralLayer record. */
/* Note: In the input layer the inputs  */
/*       and the outputs are identical. */

*NumberSym = TSYMBOL("number");
*ObjectSym = TSYMBOL("object");

TNeuralNet_input(*aNN) = *aNL = FSmartbase_Eval(gCP,gTP,newCmd,1,*NeuralLayer);
TNeuralLayer_inputs(*aNL) = Temp = FSmartbase_Eval(gCP,gTP,*makevector,2,*NumberSym,TINT(NumberOfInputs));
TNeuralLayer_outputs(*aNL) = TNeuralLayer_inputs(*aNL);
TNeuralLayer_errors(*aNL) = FSmartbase_Eval(gCP,gTP,*makevector,2,*NumberSym,TINT(NumberOfInputs));
TNeuralLayer_biases(*aNL) = gCP->Tval_VOID;
TNeuralLayer_deltas(*aNL) = FSmartbase_Eval(gCP,gTP,*makevector,2,*ObjectSym,TINT(NumberOfInputs));
TNeuralLayer_weights(*aNL) = FSmartbase_Eval(gCP,gTP,*makevector,2,*ObjectSym,TINT(NumberOfInputs));
for (i = 0; i < NumberOfInputs; ++i)
    {
    *aOV = TNeuralLayer_weights(*aNL);
	*Vector = FSmartbase_Eval(gCP,gTP,*makevector,2,*NumberSym,TINT(NumberOfHidden));
    ObjArray(*aOV)[i] = Vector->u.Object;
    *aOV = TNeuralLayer_deltas(*aNL);
    *Vector = FSmartbase_Eval(gCP,gTP,*makevector,2,*NumberSym,TINT(NumberOfHidden));
    ObjArray(*aOV)[i] = Vector->u.Object;
    }

/* Create the inside hidden NeuralLayer records. */
/* Note: In the hidden layers the inputs and the */
/*       outputs are different. The outputs are  */
/*       obtained by applying the activation     */
/*       function to the inputs.                 */
    
TNeuralNet_hidden(*aNN) = *aNL = FSmartbase_Eval(gCP,gTP,newCmd,1,*NeuralLayer);
TNeuralLayer_inputs(*aNL) = FSmartbase_Eval(gCP,gTP,*makevector,2,*NumberSym,TINT(NumberOfHidden));
TNeuralLayer_outputs(*aNL) = FSmartbase_Eval(gCP,gTP,*makevector,2,*NumberSym,TINT(NumberOfHidden));
TNeuralLayer_biases(*aNL) = FSmartbase_Eval(gCP,gTP,*makevector,2,*NumberSym,TINT(NumberOfHidden));
TNeuralLayer_errors(*aNL) = FSmartbase_Eval(gCP,gTP,*makevector,2,*NumberSym,TINT(NumberOfHidden));
TNeuralLayer_deltas(*aNL) = FSmartbase_Eval(gCP,gTP,*makevector,2,*ObjectSym,TINT(NumberOfHidden));
TNeuralLayer_weights(*aNL) = FSmartbase_Eval(gCP,gTP,*makevector,2,*ObjectSym,TINT(NumberOfHidden));
for (i = 0; i < NumberOfHidden; ++i)
    {
    *aOV = TNeuralLayer_weights(*aNL);
    *Vector = FSmartbase_Eval(gCP,gTP,*makevector,2,*NumberSym,TINT(NumberOfOutputs));
    ObjArray(*aOV)[i] = Vector->u.Object;
    *aOV = TNeuralLayer_deltas(*aNL);
    *Vector = FSmartbase_Eval(gCP,gTP,*makevector,2,*NumberSym,TINT(NumberOfOutputs));
    ObjArray(*aOV)[i] = Vector->u.Object;
    }

/* Create the output NeuralLayer record. */
/* Note: In the hidden layers the inputs and the */
/*       outputs are different. The outputs are  */
/*       obtained by applying the activation     */
/*       function to the inputs.                 */
    
TNeuralNet_output(*aNN) = *aNL = FSmartbase_Eval(gCP,gTP,newCmd,1,*NeuralLayer);
TNeuralLayer_inputs(*aNL) = FSmartbase_Eval(gCP,gTP,*makevector,2,*NumberSym,TINT(NumberOfOutputs));
TNeuralLayer_outputs(*aNL) = FSmartbase_Eval(gCP,gTP,*makevector,2,*NumberSym,TINT(NumberOfOutputs));
TNeuralLayer_errors(*aNL) = FSmartbase_Eval(gCP,gTP,*makevector,2,*NumberSym,TINT(NumberOfOutputs));
TNeuralLayer_biases(*aNL) = FSmartbase_Eval(gCP,gTP,*makevector,2,*NumberSym,TINT(NumberOfOutputs));
TNeuralLayer_weights(*aNL) = gCP->Tval_VOID;
TNeuralLayer_deltas(*aNL) = gCP->Tval_VOID;

/* Create the output NeuralLayer filter. */
/* Note: The output layers may be user selected  */
/*       to produce continuous, sigmoidal,       */
/*       binary, or bipolar output.              */

TNeuralNet_filter(*aNN) = *filter;


FrameExit(*NeuralNet);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TNeuralNet_GetWeights

Return the weights and the internal state of the NeuralNet Object so it can be recreated later. 

(getWeights NeuralNet)

#endif

TVAL    TNeuralNet_GetWeights(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM			weightsVectorOutputIndex = 0;
NUM         NumberOfInputs;
NUM         NumberOfOutputs;
NUM         NumberOfHidden;
NUM         i;
NUM         j;
NUM         k;
LpREAL      hiddenBIASVector;
LpREAL      outputBIASVector;
LpREAL      inputDELTAVector;
LpREAL      hiddenDELTAVector;
LpREAL      inputWEIGHTVector;
LpREAL      hiddenWEIGHTVector;
StartFrame
DeclareTVAL(Vector);
DeclareTVAL(ret);
DeclareTVAL(weights);
DeclareTVAL(makeVector);
DeclareTVAL(hiddenWeightVector);
DeclareTVAL(inputWeightVector);
DeclareTVAL(deltas);
DeclareTVAL(inputDeltaVector);
DeclareTVAL(hiddenDeltaVector);
DeclareTVAL(biases);
DeclareTVAL(ec);
DeclareTVAL(aNN);
DeclareTVAL(length);
DeclareTVAL(random);
DeclareTVAL(limit);
EndFrame

*length  = TGVALUE("length");
*random  = TGVALUE("random");
*limit   = TINT(1);
*makeVector  = TGVALUE("makeVector");



/*  Check the mandatory arguments. */

if (argc != 1)
    {
	*ret = TERROR("!getWeights (neural net): incorrect number of argument!");
	FrameExit(*ret);
    }

if (!isNeuralNet(argv[0]))
    {
	*ret = TERROR("!getWeights (neural net): Expecting argument one to be a neuralNet!");
	FrameExit(*ret);
    }

/* Save all the mandatory arguments. */

*aNN = argv[0];

/* Create the weights vector which is to be returned. */

*Vector = FSmartbase_Eval(gCP,gTP,*makeVector,1,TINT(0));
ExitOnError(*Vector);


/* Set the number of inputs, number of outputs, and the output filter type,
   so that the NeuralNet can be recreated later using the makeNeuralNet function. */

*deltas = TNeuralLayer_deltas(TNeuralNet_input(*aNN));
*weights = TNeuralLayer_weights(TNeuralNet_input(*aNN));
*ec = FSmartbase_Eval(gCP,gTP,*length,1,*weights);
ExitOnError(*ec);
NumberOfInputs = ec->u.Int;
*ec = FSmartbase_Set(gCP,gTP,3,*Vector,TINT(weightsVectorOutputIndex++),TINT(NumberOfInputs));
ExitOnError(*ec);

/* NumberOfHidden = (2 * NumberOfInputs) + 1; */
*ec = FSmartbase_Eval(gCP,gTP,*length,1,TNeuralLayer_weights(TNeuralNet_hidden(*aNN)));
ExitOnError(*ec);
NumberOfHidden = ec->u.Int;
*ec = FSmartbase_Set(gCP,gTP,3,*Vector,TINT(weightsVectorOutputIndex++),TINT(NumberOfHidden));
ExitOnError(*ec);

*biases = TNeuralLayer_biases(TNeuralNet_output(*aNN));
outputBIASVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*biases);
*ec = FSmartbase_Eval(gCP,gTP,*length,1,*biases);
ExitOnError(*ec);
NumberOfOutputs = ec->u.Int;
*ec = FSmartbase_Set(gCP,gTP,3,*Vector,TINT(weightsVectorOutputIndex++),TINT(NumberOfOutputs));
ExitOnError(*ec);

*ec = FSmartbase_Set(gCP,gTP,3,*Vector,TINT(weightsVectorOutputIndex++),TNeuralNet_filter(*aNN));
ExitOnError(*ec);

/* Return all NeuralNet global variables. */

*ec = FSmartbase_Set(gCP,gTP,3,*Vector,TINT(weightsVectorOutputIndex++),TNeuralNet_learning(*aNN));
ExitOnError(*ec);

*ec = FSmartbase_Set(gCP,gTP,3,*Vector,TINT(weightsVectorOutputIndex++),TNeuralNet_momentum(*aNN));
ExitOnError(*ec);

*ec = FSmartbase_Set(gCP,gTP,3,*Vector,TINT(weightsVectorOutputIndex++),TNeuralNet_theta(*aNN));
ExitOnError(*ec);

*ec = FSmartbase_Set(gCP,gTP,3,*Vector,TINT(weightsVectorOutputIndex++),TNeuralNet_error(*aNN));
ExitOnError(*ec);

*ec = FSmartbase_Set(gCP,gTP,3,*Vector,TINT(weightsVectorOutputIndex++),TNeuralNet_notes(*aNN));
ExitOnError(*ec);

/* Return the weights in the input NeuralLayer record. */

*deltas = TNeuralLayer_deltas(TNeuralNet_input(*aNN));
*weights = TNeuralLayer_weights(TNeuralNet_input(*aNN));
*ec = FSmartbase_Eval(gCP,gTP,*length,1,*weights);
ExitOnError(*ec);
NumberOfInputs = ec->u.Int;
for (i = 0; i < NumberOfInputs; ++i)
    {
    *inputWeightVector = FSmartbase_Ref(gCP,gTP,2,*weights,TINT(i));
    ExitOnError(*inputWeightVector);
    inputWEIGHTVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*inputWeightVector);
    NumberOfHidden = FSmartbase_VectorLen(gCP,gTP,*inputWeightVector);
    *inputDeltaVector = FSmartbase_Ref(gCP,gTP,2,*deltas,TINT(i));
    ExitOnError(*inputDeltaVector);
    inputDELTAVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*inputDeltaVector);
    for (j = 0; j < NumberOfHidden; ++j)
        {
		*ec = FSmartbase_Set(gCP,gTP,3,*Vector,TINT(weightsVectorOutputIndex++),TREAL(inputWEIGHTVector[j]));
		ExitOnError(*ec);
		/* Delta vectors not implemented yet...
		*ec = FSmartbase_Set(gCP,gTP,3,*Vector,TINT(weightsVectorOutputIndex++),TREAL(inputDELTAVector[j]));
		ExitOnError(*ec);
		*/
        }
    }


/* Return the weights in the hidden NeuralLayer record. */

*biases = TNeuralLayer_biases(TNeuralNet_hidden(*aNN));
hiddenBIASVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*biases);
*deltas = TNeuralLayer_deltas(TNeuralNet_hidden(*aNN));
*weights = TNeuralLayer_weights(TNeuralNet_hidden(*aNN));
*ec = FSmartbase_Eval(gCP,gTP,*length,1,*weights);
ExitOnError(*ec);
NumberOfHidden = ec->u.Int;
for (j = 0; j < NumberOfHidden; ++j)
    {
	*ec = FSmartbase_Set(gCP,gTP,3,*Vector,TINT(weightsVectorOutputIndex++),TREAL(hiddenBIASVector[j]));
	ExitOnError(*ec);
    *hiddenDeltaVector = FSmartbase_Ref(gCP,gTP,2,*deltas,TINT(j));
    ExitOnError(*hiddenDeltaVector);
    hiddenDELTAVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*hiddenDeltaVector);
    *hiddenWeightVector = FSmartbase_Ref(gCP,gTP,2,*weights,TINT(j));
    ExitOnError(*hiddenWeightVector);
    hiddenWEIGHTVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*hiddenWeightVector);
    NumberOfOutputs = FSmartbase_VectorLen(gCP,gTP,*hiddenWeightVector);
    for (k = 0; k < NumberOfOutputs; ++k)
        {
		*ec = FSmartbase_Set(gCP,gTP,3,*Vector,TINT(weightsVectorOutputIndex++),TREAL(hiddenWEIGHTVector[k]));
		ExitOnError(*ec);
		/* Delta vectors not implemented yet...
		*ec = FSmartbase_Set(gCP,gTP,3,*Vector,TINT(weightsVectorOutputIndex++),TREAL(hiddenDELTAVector[k]));
		ExitOnError(*ec);
		*/
}
    }


/* Return the biases in the output NeuralLayer record. */

*biases = TNeuralLayer_biases(TNeuralNet_output(*aNN));
outputBIASVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*biases);
*ec = FSmartbase_Eval(gCP,gTP,*length,1,*biases);
ExitOnError(*ec);
NumberOfOutputs = ec->u.Int;
for (k = 0; k < NumberOfOutputs; ++k)
    {
	*ec = FSmartbase_Set(gCP,gTP,3,*Vector,TINT(weightsVectorOutputIndex++),TREAL(outputBIASVector[k]));
	ExitOnError(*ec);
    }


FrameExit(*Vector);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TNeuralNet_PropagateForward

Propagate the signal forward in the specified NeuralNet Object. The input signal is
propagated forward through each hidden layer to produce new values in the output layer.

(propagateForward aNeuralNet inVector)

Note:   This procedure propagates the input signal through each hidden layer
        and produces new values in the output layer.

#endif

TVAL    TNeuralNet_PropagateForward(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM         NumberOfInputs;
NUM         NumberOfOutputs;
NUM         NumberOfHidden;
NUM         INi;
NUM         HDj;
NUM         OTk;
NUM         n;
REAL        theta;
NUM         filterID;
REAL        sum;
LpREAL      argInputVector;
LpREAL      inputINVector;
LpREAL      inputOUTVector;
LpREAL      hiddenINVector;
LpREAL      hiddenOUTVector;
LpREAL      hiddenBIASVector;
LpREAL      outputINVector;
LpREAL      outputOUTVector;
LpREAL      outputBIASVector;
LpREAL      weightSignalVector;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(inputNeuralLayer);
DeclareTVAL(hiddenNeuralLayer);
DeclareTVAL(outputNeuralLayer);
DeclareTVAL(weightMatrix);
DeclareTVAL(weightVector);
DeclareTVAL(aNN);
DeclareTVAL(ec);
DeclareTVAL(length);
EndFrame

*length  = TGVALUE("length");

/*  Check the mandatory arguments. */


if (argc < 1)
    {
	*ret = TERROR("!propagateForward: Not enough arguments!");
	FrameExit(*ret);
	}
if (argc > 2)
    {
	*ret = TERROR("!propagateForward: Too Many arguments!");
	FrameExit(*ret);
    }

if (!isNeuralNet(argv[0]))
    {
	*ret = TERROR("!propagateForward:Expecting argument 1 to be a neuralNet!");
	FrameExit(*ret);
    }

if  ((argc == 2) && (argv[1].Tag != TYNUMVECTOR)) 
    {
	*ret = TERROR("!propagateForward:Expecting argument 2 to be a number vector!");
	FrameExit(*ret);
    }


/* Save all the mandatory arguments. */

*aNN = argv[0];
*ec = TNeuralNet_theta(*aNN);
theta = (ec->Tag == TYREAL) ? ec->u.Real : (ec->Tag == TYNUM) ? ec->u.Int : 0 ;
filterID = TNeuralNet_Continuous;
*ec = TNeuralNet_filter(*aNN);
if (ec->Tag == TYSYMBOL)
    {
    if (ec->u.Object == (TObject*)gCP->TNeuralNet_ContinuousSym) 
        filterID = TNeuralNet_Continuous;
    else
    if (ec->u.Object == (TObject*)gCP->TNeuralNet_SigmoidalSym) 
        filterID = TNeuralNet_Sigmoidal;
    else
    if (ec->u.Object == (TObject*)gCP->TNeuralNet_BinarySym) 
        filterID = TNeuralNet_Binary;
    else
    if (ec->u.Object == (TObject*)gCP->TNeuralNet_BipolarSym) 
        filterID = TNeuralNet_Bipolar;
    }

/* Get the number of inputs to the net. */
/* Note: In the input layer the inputs  */
/*       and the outputs are identical. */

*inputNeuralLayer = TNeuralNet_input(*aNN);
inputINVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_inputs(*inputNeuralLayer));
NumberOfInputs = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_inputs(*inputNeuralLayer));

/*  Copy the input signal vector to the input   */
/*  NeuralLayer record input vector. Check      */
/*  that the number of inputs do match exactly. */
/*  Note: Because the inputs and outputs of the */
/*        input layer are identical, this also  */
/*        copies the input signals to the input */
/*        layer's outputs as well.              */

if (argc == 2)
    {
    argInputVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,argv[1]);
    n = FSmartbase_VectorLen(gCP,gTP,argv[1]);
    if (NumberOfInputs > n)
        {
		*ret = TERROR("!propagateForward (neural net):argument 2 has too few inputs!");
		FrameExit(*ret);
        }
    
    /*  Copy the argument input vector argument to the input NeuralLayer record input vector. */
	/*  Note: Each input must be within the closed interval [0, 1] or an error is raised.     */
    
    for (INi = 0; INi < NumberOfInputs; ++INi)
        {
		if ((argInputVector[INi] >= 0) && (argInputVector[INi] <= 1))
			{
			inputINVector[INi] = argInputVector[INi];
			}
		else
			{
			*ret = TERROR("!propagateForward: each input must be within the closed interval [0, 1]!");
			FrameExit(*ret);
			}
        }
    }
    
/* Propagate the signal from the input NeuralLayer record to */
/* the inputs and outputs of the hidden NeuralLayer record.  */

*inputNeuralLayer = TNeuralNet_input(*aNN);
ExitOnError(*inputNeuralLayer);
*hiddenNeuralLayer = TNeuralNet_hidden(*aNN);
ExitOnError(*hiddenNeuralLayer);
*weightMatrix = TNeuralLayer_weights(*inputNeuralLayer);
inputOUTVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_outputs(*inputNeuralLayer));
hiddenINVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_inputs(*hiddenNeuralLayer));
hiddenBIASVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_biases(*hiddenNeuralLayer));
hiddenOUTVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_outputs(*hiddenNeuralLayer));
NumberOfHidden = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_inputs(*hiddenNeuralLayer));
NumberOfInputs = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_inputs(*inputNeuralLayer));
for (HDj = 0; HDj < NumberOfHidden; ++HDj)
    {
    /* Compute the sum of the weight vector times the inputs.   */

    sum = hiddenBIASVector[HDj];
    for (INi = 0; INi < NumberOfInputs; ++INi)
        {
        *weightVector = FSmartbase_Ref(gCP,gTP,2,*weightMatrix,TINT(INi));
        ExitOnError(*weightVector);
        weightSignalVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*weightVector);
        sum += inputOUTVector[INi] * weightSignalVector[HDj];
        }

    /*  Now save the weighted sum in the input cells for the hidden layer. */

	hiddenINVector[HDj] = sum;

    /*  Now invoke the neural activation function */
	/*  to covert weighted input to output values. */

    hiddenOUTVector[HDj] = (2.0 / (1.0 + exp(-hiddenINVector[HDj]))) - 1;
	}


/* Propagate the signal through the hidden NeuralLayer record  */
/* to the inputs and outputs of the output NeuralLayer record. */

*hiddenNeuralLayer = TNeuralNet_hidden(*aNN);
ExitOnError(*hiddenNeuralLayer);
*outputNeuralLayer = TNeuralNet_output(*aNN);
ExitOnError(*outputNeuralLayer);
hiddenOUTVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_outputs(*hiddenNeuralLayer));
NumberOfHidden = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_inputs(*hiddenNeuralLayer));
NumberOfOutputs = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_inputs(*outputNeuralLayer));
*weightMatrix = TNeuralLayer_weights(*hiddenNeuralLayer);
outputBIASVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_biases(*outputNeuralLayer));
outputINVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_inputs(*outputNeuralLayer));
outputOUTVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_outputs(*outputNeuralLayer));
NumberOfOutputs = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_inputs(*outputNeuralLayer));
for (OTk = 0; OTk < NumberOfOutputs; ++OTk)
    {
    /* Compute the sum of the weight vector times the inputs. */

    sum = outputBIASVector[OTk];
    for (HDj = 0; HDj < NumberOfHidden; ++HDj)
        {
        *weightVector = FSmartbase_Ref(gCP,gTP,2,*weightMatrix,TINT(HDj));
        ExitOnError(*weightVector);
        weightSignalVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*weightVector);
        sum += hiddenOUTVector[HDj] * weightSignalVector[OTk];
        }
   
		/*  Now save the weighted sum in the input cells for the hidden layer. */

		outputINVector[OTk] = sum;

    /*  Now invoke the output filter function. */

    switch (filterID)
        {
        case TNeuralNet_Sigmoidal:
            outputOUTVector[OTk] = 1.0 / (1.0 + exp(-outputINVector[OTk]));
            break;
        
        case TNeuralNet_Binary:
            outputOUTVector[OTk] = (outputINVector[OTk] < theta) ? 0 : 1;
            break;
        
        case TNeuralNet_Bipolar:
            outputOUTVector[OTk] = (outputINVector[OTk] < theta) ? -1 : 1;
            break;
        
        case TNeuralNet_Continuous:
        default:
            outputOUTVector[OTk] = outputINVector[OTk];
            break;
        }
    }


FrameExit(argv[0]);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TNeuralNet_PropagateBackward

Propagate the output error for the specified NeuralNet Object. The output signal is
compared to the specified goal vector (target) and the resulting error is propagated
backward through each layer of the NeuralNet. The target must be a number vector.

(propagateBackward aNeuralNet target)

Note:   This procedure computes the output error for the NeuralNet and propagates
        the error deltas backward through each layer of the NeuralNet. This also
        adjusts the weights for each layer, and effectively trains the NeuralNet.

#endif

TVAL    TNeuralNet_PropagateBackward(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM         NumberOfInputs;
NUM         NumberOfOutputs;
NUM         NumberOfHidden;
NUM			INi;
NUM			HDj;
NUM			OTk;
NUM         n;
REAL        theta;
REAL        learning;
REAL        momentum;
NUM         filterID;
REAL        delta;
REAL        sum;
LpREAL      hiddenBiasVector;
LpREAL      outputBiasVector;
LpREAL      targetSignalVector;
LpREAL      outputSignalVector;

LpREAL      inputOUTVector;
LpREAL      inputDeltaVector;
LpREAL      inputWeightVector;
LpREAL      inputERRVector;
LpREAL      hiddenOUTVector;
LpREAL      hiddenDeltaVector;
LpREAL      hiddenWeightVector;
LpREAL      hiddenERRVector;
LpREAL      outputERRVector;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(inputNeuralLayer);
DeclareTVAL(hiddenNeuralLayer);
DeclareTVAL(outputNeuralLayer);
DeclareTVAL(weightMatrix);
DeclareTVAL(weightVector);
DeclareTVAL(deltaMatrix);
DeclareTVAL(deltaVector);
DeclareTVAL(aNN);
DeclareTVAL(ec);
DeclareTVAL(length);
DeclareTVAL(random);
EndFrame

*length  = TGVALUE("length");
*random  = TGVALUE("random");

/*  Check the mandatory arguments. */

if (argc < 2)
    {
	*ret = TERROR("!propagateBackward: Not enough arguments!");
	FrameExit(*ret);
    }

if (argc > 2)
    {
	*ret = TERROR("!propagateBackward: Not enough arguments!");
	FrameExit(*ret);
    }

if (!isNeuralNet(argv[0]))
    {
	*ret = TERROR("!propagateBackward: Expecting argument 1 to be a neuralNet!");
	FrameExit(*ret);
    }


if  (argv[1].Tag != TYNUMVECTOR)
    {
	*ret = TERROR("!propagateBackward: Expecting argument 2 to be a number vector!");
	FrameExit(*ret);
    }

    
/* Save all the mandatory arguments. */

*aNN = argv[0];
*ec = TNeuralNet_theta(*aNN);
theta = (ec->Tag == TYREAL) ? ec->u.Real : (ec->Tag == TYNUM) ? ec->u.Int : 0 ;
*ec = TNeuralNet_momentum(*aNN);
momentum = (ec->Tag == TYREAL) ? ec->u.Real : (ec->Tag == TYNUM) ? ec->u.Int : 0 ;
*ec = TNeuralNet_learning(*aNN);
learning = (ec->Tag == TYREAL) ? ec->u.Real : (ec->Tag == TYNUM) ? ec->u.Int : 0 ;
filterID = TNeuralNet_Continuous;
*ec = TNeuralNet_filter(*aNN);
if (ec->Tag == TYSYMBOL)
    {
    if (ec->u.Object == (TObject*)gCP->TNeuralNet_ContinuousSym) 
        filterID = TNeuralNet_Continuous;
    else
    if (ec->u.Object == (TObject*)gCP->TNeuralNet_SigmoidalSym) 
        filterID = TNeuralNet_Sigmoidal;
    else
    if (ec->u.Object == (TObject*)gCP->TNeuralNet_BinarySym) 
        filterID = TNeuralNet_Binary;
    else
    if (ec->u.Object == (TObject*)gCP->TNeuralNet_BipolarSym) 
        filterID = TNeuralNet_Bipolar;
    }

/* Get the number of outputs, and the error vector. */

*outputNeuralLayer = TNeuralNet_output(*aNN);
outputSignalVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_outputs(*outputNeuralLayer));
NumberOfOutputs = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_outputs(*outputNeuralLayer));
outputERRVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_errors(*outputNeuralLayer));

/*  Compare the target vector to the NeuralLayer record output */
/*  vector. Check that the number of outputs do match exactly. */

targetSignalVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,argv[1]);
n = FSmartbase_VectorLen(gCP,gTP,argv[1]);
if (NumberOfOutputs > n)
    {
	*ret = TERROR("!propagateBackward: argument 2 has too few output signals!");
	FrameExit(*ret);
    }

/*  Compare the target vector to the NeuralLayer record output vector. */
/*  Note: The output signals are taken from the right of the target vector. */

n = n - NumberOfOutputs;
sum = 0;
for (OTk = 0; OTk < NumberOfOutputs; ++OTk)
    {
	/* Compute the output versus target signal error */
    delta = (targetSignalVector[OTk + n] - outputSignalVector[OTk]);
    sum += (delta * delta);

	/* Multiply the output error by the derivative of the output filter function. */
    switch (filterID)
        {
        case    TNeuralNet_Sigmoidal:
            outputERRVector[OTk] = delta * (outputSignalVector[OTk] * (1.0 - outputSignalVector[OTk]));
            break;
    
        case    TNeuralNet_Binary:
            outputERRVector[OTk] = delta * (1);
            break;
    
        case    TNeuralNet_Bipolar:
            outputERRVector[OTk] = delta * (1);
            break;
    
        case    TNeuralNet_Continuous:
        default:
            outputERRVector[OTk] = delta * (1);
            break;
        }
    }
TNeuralNet_error(*aNN) = TREAL(sum);


/* Back propagate the errors from the output NeuralLayer */
/* record to the hidden NeuralLayer record. */

*hiddenNeuralLayer = TNeuralNet_hidden(*aNN);
ExitOnError(*hiddenNeuralLayer);
*weightMatrix = TNeuralLayer_weights(*hiddenNeuralLayer);
*deltaMatrix = TNeuralLayer_deltas(*hiddenNeuralLayer);
hiddenERRVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_errors(*hiddenNeuralLayer));
hiddenOUTVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_outputs(*hiddenNeuralLayer));
NumberOfHidden = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_errors(*hiddenNeuralLayer));
for (HDj = 0; HDj < NumberOfHidden; ++HDj)
    {
    hiddenERRVector[HDj] = 0;
    
    *weightVector = FSmartbase_Ref(gCP,gTP,2,*weightMatrix,TINT(HDj));
    ExitOnError(*weightVector);
    hiddenWeightVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*weightVector);

    *deltaVector = FSmartbase_Ref(gCP,gTP,2,*deltaMatrix,TINT(HDj));
    ExitOnError(*deltaVector);
    hiddenDeltaVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*deltaVector);

    for (OTk = 0; OTk < NumberOfOutputs; ++OTk)
        {
        /*  Use the output NeuralLayer error combined with each connection */
        /*  weight to compute the effective error factor in the hidden NeuralLayer. */
    
		hiddenERRVector[HDj] = hiddenERRVector[HDj] + (outputERRVector[OTk] * hiddenWeightVector[OTk]);
        }

	    /*  Multiply the error by the derivative of the hidden layer activation function. */
    
        hiddenERRVector[HDj] = hiddenERRVector[HDj] * (hiddenOUTVector[HDj] * (1.0 - hiddenOUTVector[HDj]));

    }


/* Adjust the bias weights of the output NeuralLayer record. */
/* Note: The biases are also adjusted using momentum.  */

*outputNeuralLayer = TNeuralNet_output(*aNN);
outputBiasVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_biases(*outputNeuralLayer));
outputERRVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_errors(*outputNeuralLayer));
NumberOfOutputs = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_outputs(*outputNeuralLayer));
for (OTk = 0; OTk < NumberOfOutputs; ++OTk)
    {
	delta = learning * outputERRVector[OTk];

    if ((delta < _MINREAL) && (delta > -_MINREAL)) delta = 0;
    if ((delta > _MAXREAL) || (delta < -_MAXREAL)) delta = 0;

    outputBiasVector[OTk] += delta;
    if (!((outputBiasVector[OTk] < _MAXREAL) && (outputBiasVector[OTk] > -_MAXREAL)))
        {
        outputBiasVector[OTk] = FSmartbase_Eval(gCP,gTP,*random,1,TINT(1)).u.Real - 5;
        }
    }

/* Adjust the weights of the hidden NeuralLayer record. */
/* Note: The deltas are also adjusted using momentum.   */

*hiddenNeuralLayer = TNeuralNet_hidden(*aNN);
hiddenOUTVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_outputs(*hiddenNeuralLayer));
inputERRVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_errors(*hiddenNeuralLayer));
*deltaMatrix = TNeuralLayer_deltas(*hiddenNeuralLayer);
*weightMatrix = TNeuralLayer_weights(*hiddenNeuralLayer);
*outputNeuralLayer = TNeuralNet_output(*aNN);
outputERRVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_errors(*outputNeuralLayer));
NumberOfOutputs = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_outputs(*outputNeuralLayer));
for (HDj = 0; HDj < NumberOfHidden; ++HDj)
    {
    /* Compute the new weight delta. */

    *weightVector = FSmartbase_Ref(gCP,gTP,2,*weightMatrix,TINT(HDj));
    ExitOnError(*weightVector);
    hiddenWeightVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*weightVector);
    *deltaVector = FSmartbase_Ref(gCP,gTP,2,*deltaMatrix,TINT(HDj));
    ExitOnError(*deltaVector);
    hiddenDeltaVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*deltaVector);
    for (OTk = 0; OTk < NumberOfOutputs; ++OTk)
        {
		delta = learning * outputERRVector[OTk] * hiddenOUTVector[HDj];

        if ((delta < _MINREAL) && (delta > -_MINREAL)) delta = 0;
        if ((delta > _MAXREAL) || (delta < -_MAXREAL)) delta = 0;

		hiddenDeltaVector[OTk] = delta;
        hiddenWeightVector[OTk] += delta;

        if (!((hiddenWeightVector[OTk] < _MAXREAL) && (hiddenWeightVector[OTk] > -_MAXREAL)))
            {
            hiddenWeightVector[OTk] = FSmartbase_Eval(gCP,gTP,*random,1,TINT(1)).u.Real - .5;
            }
       }
    }


/* Adjust the bias weights of the hidden NeuralLayer record. */
/* Note: The biases are also adjusted using momentum.  */

*hiddenNeuralLayer = TNeuralNet_hidden(*aNN);
hiddenBiasVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_biases(*hiddenNeuralLayer));
hiddenERRVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_errors(*hiddenNeuralLayer));
NumberOfHidden = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_outputs(*hiddenNeuralLayer));
for (HDj = 0; HDj < NumberOfHidden; ++HDj)
    {
	delta = learning * hiddenERRVector[HDj];
    if ((delta < _MINREAL) && (delta > -_MINREAL)) delta = 0;
    if ((delta > _MAXREAL) || (delta < -_MAXREAL)) delta = 0;

    hiddenBiasVector[HDj] += delta;
    if (!((hiddenBiasVector[HDj] < _MAXREAL) && (hiddenBiasVector[HDj] > -_MAXREAL)))
        {
        hiddenBiasVector[HDj] = FSmartbase_Eval(gCP,gTP,*random,1,TINT(1)).u.Real - .5;
        }
    }


/* Adjust the weights of the input NeuralLayer record. */
/* Note: The deltas are also adjusted using momentum.  */

*inputNeuralLayer = TNeuralNet_input(*aNN);
*hiddenNeuralLayer = TNeuralNet_hidden(*aNN);
*deltaMatrix = TNeuralLayer_deltas(*inputNeuralLayer);
*weightMatrix = TNeuralLayer_weights(*inputNeuralLayer);
inputOUTVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_outputs(*inputNeuralLayer));
hiddenERRVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_errors(*hiddenNeuralLayer));
NumberOfHidden = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_outputs(*hiddenNeuralLayer));
NumberOfInputs = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_outputs(*inputNeuralLayer));
for (INi = 0; INi < NumberOfInputs; ++INi)
    {
    /* Compute the new weight delta. */

    *weightVector = FSmartbase_Ref(gCP,gTP,2,*weightMatrix,TINT(INi));
    ExitOnError(*weightVector);
    inputWeightVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*weightVector);
    *deltaVector = FSmartbase_Ref(gCP,gTP,2,*deltaMatrix,TINT(INi));
    ExitOnError(*deltaVector);
    inputDeltaVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*deltaVector);
    for (HDj = 0; HDj < NumberOfHidden; ++HDj)
        {
		delta = learning * hiddenERRVector[HDj] * inputOUTVector[INi];

		if ((delta < _MINREAL) && (delta > -_MINREAL)) delta = 0;
		if ((delta > _MAXREAL) || (delta < -_MAXREAL)) delta = 0;

		inputDeltaVector[HDj] = delta;
		inputWeightVector[HDj] += delta;

        if (!((inputWeightVector[HDj] < _MAXREAL) && (inputWeightVector[HDj] > -_MAXREAL)))
            {
            inputWeightVector[HDj] = FSmartbase_Eval(gCP,gTP,*random,1,TINT(1)).u.Real - .5;
            }
        }
    }


FrameExit(argv[0]);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TNeuralNet_PropagateUserError

Propagate the user error for the specified NeuralNet Object. The user error signal 
is propagated backward through each layer of the NeuralNet. 
The user error must be a number vector.

(propagateUserError aNeuralNet uError)

Note:   This procedure acquires the user error for the NeuralNet and propagates
        the error deltas backward through each layer of the NeuralNet. This also
        adjusts the weights for each layer, and effectively trains the NeuralNet.

#endif

TVAL    TNeuralNet_PropagateUserError(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM         NumberOfInputs;
NUM         NumberOfOutputs;
NUM         NumberOfHidden;
NUM			INi;
NUM			HDj;
NUM			OTk;
NUM         n;
REAL        theta;
REAL        learning;
REAL        momentum;
NUM         filterID;
REAL        delta;
REAL        sum;
LpREAL      hiddenBiasVector;
LpREAL      outputBiasVector;
LpREAL      targetSignalVector;
LpREAL      outputSignalVector;

LpREAL      inputOUTVector;
LpREAL      inputDeltaVector;
LpREAL      inputWeightVector;
LpREAL      inputERRVector;
LpREAL      hiddenOUTVector;
LpREAL      hiddenDeltaVector;
LpREAL      hiddenWeightVector;
LpREAL      hiddenERRVector;
LpREAL      outputERRVector;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(inputNeuralLayer);
DeclareTVAL(hiddenNeuralLayer);
DeclareTVAL(outputNeuralLayer);
DeclareTVAL(weightMatrix);
DeclareTVAL(weightVector);
DeclareTVAL(deltaMatrix);
DeclareTVAL(deltaVector);
DeclareTVAL(aNN);
DeclareTVAL(ec);
DeclareTVAL(length);
DeclareTVAL(random);
EndFrame

*length  = TGVALUE("length");
*random  = TGVALUE("random");

/*  Check the mandatory arguments. */

if (argc < 2)
    {
	*ret = TERROR("!propagateUserError: Not enough arguments!");
	FrameExit(*ret);
    }

if (argc > 2)
    {
	*ret = TERROR("!propagateUserError: Not enough arguments!");
	FrameExit(*ret);
    }

if (!isNeuralNet(argv[0]))
    {
	*ret = TERROR("!propagateUserError: Expecting argument 1 to be a neuralNet!");
	FrameExit(*ret);
    }


if  (argv[1].Tag != TYNUMVECTOR)
    {
	*ret = TERROR("!propagateUserError: Expecting argument 2 to be a number vector!");
	FrameExit(*ret);
    }

    
/* Save all the mandatory arguments. */

*aNN = argv[0];
*ec = TNeuralNet_theta(*aNN);
theta = (ec->Tag == TYREAL) ? ec->u.Real : (ec->Tag == TYNUM) ? ec->u.Int : 0 ;
*ec = TNeuralNet_momentum(*aNN);
momentum = (ec->Tag == TYREAL) ? ec->u.Real : (ec->Tag == TYNUM) ? ec->u.Int : 0 ;
*ec = TNeuralNet_learning(*aNN);
learning = (ec->Tag == TYREAL) ? ec->u.Real : (ec->Tag == TYNUM) ? ec->u.Int : 0 ;
filterID = TNeuralNet_Continuous;
*ec = TNeuralNet_filter(*aNN);
if (ec->Tag == TYSYMBOL)
    {
    if (ec->u.Object == (TObject*)gCP->TNeuralNet_ContinuousSym) 
        filterID = TNeuralNet_Continuous;
    else
    if (ec->u.Object == (TObject*)gCP->TNeuralNet_SigmoidalSym) 
        filterID = TNeuralNet_Sigmoidal;
    else
    if (ec->u.Object == (TObject*)gCP->TNeuralNet_BinarySym) 
        filterID = TNeuralNet_Binary;
    else
    if (ec->u.Object == (TObject*)gCP->TNeuralNet_BipolarSym) 
        filterID = TNeuralNet_Bipolar;
    }

/* Get the number of outputs, and the error vector. */

*outputNeuralLayer = TNeuralNet_output(*aNN);
outputSignalVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_outputs(*outputNeuralLayer));
NumberOfOutputs = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_outputs(*outputNeuralLayer));
outputERRVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_errors(*outputNeuralLayer));

/*  Use the target vector for the NeuralLayer record output error */
/*  delta vector. Check that the number of outputs do match exactly. */

targetSignalVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,argv[1]);
n = FSmartbase_VectorLen(gCP,gTP,argv[1]);
if (NumberOfOutputs > n)
    {
	*ret = TERROR("!propagateUserError: argument 2 has too few output signals!");
	FrameExit(*ret);
    }

/*  Compare the target vector to the NeuralLayer record output vector. */
/*  Note: The output error signals are taken from the right of the target vector. */

n = n - NumberOfOutputs;
sum = 0;
for (OTk = 0; OTk < NumberOfOutputs; ++OTk)
    {
	/* Compute the output versus target signal error */
    delta = targetSignalVector[OTk + n];
    sum += (delta * delta);

	/* Multiply the output error by the derivative of the output filter function. */
    switch (filterID)
        {
        case    TNeuralNet_Sigmoidal:
            outputERRVector[OTk] = delta * (outputSignalVector[OTk] * (1.0 - outputSignalVector[OTk]));
            break;
    
        case    TNeuralNet_Binary:
            outputERRVector[OTk] = delta * (1);
            break;
    
        case    TNeuralNet_Bipolar:
            outputERRVector[OTk] = delta * (1);
            break;
    
        case    TNeuralNet_Continuous:
        default:
            outputERRVector[OTk] = delta * (1);
            break;
        }
    }
TNeuralNet_error(*aNN) = TREAL(sum);


/* Back propagate the errors from the output NeuralLayer */
/* record to the hidden NeuralLayer record. */

*hiddenNeuralLayer = TNeuralNet_hidden(*aNN);
ExitOnError(*hiddenNeuralLayer);
*weightMatrix = TNeuralLayer_weights(*hiddenNeuralLayer);
*deltaMatrix = TNeuralLayer_deltas(*hiddenNeuralLayer);
hiddenERRVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_errors(*hiddenNeuralLayer));
hiddenOUTVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_outputs(*hiddenNeuralLayer));
NumberOfHidden = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_errors(*hiddenNeuralLayer));
for (HDj = 0; HDj < NumberOfHidden; ++HDj)
    {
    hiddenERRVector[HDj] = 0;
    
    *weightVector = FSmartbase_Ref(gCP,gTP,2,*weightMatrix,TINT(HDj));
    ExitOnError(*weightVector);
    hiddenWeightVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*weightVector);

    *deltaVector = FSmartbase_Ref(gCP,gTP,2,*deltaMatrix,TINT(HDj));
    ExitOnError(*deltaVector);
    hiddenDeltaVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*deltaVector);

    for (OTk = 0; OTk < NumberOfOutputs; ++OTk)
        {
        /*  Use the output NeuralLayer error combined with each connection */
        /*  weight to compute the effective error factor in the hidden NeuralLayer. */
    
		hiddenERRVector[HDj] = hiddenERRVector[HDj] + (outputERRVector[OTk] * hiddenWeightVector[OTk]);
        }

	    /*  Multiply the error by the derivative of the hidden layer activation function. */
    
        hiddenERRVector[HDj] = hiddenERRVector[HDj] * (hiddenOUTVector[HDj] * (1.0 - hiddenOUTVector[HDj]));

    }


/* Adjust the bias weights of the output NeuralLayer record. */
/* Note: The biases are also adjusted using momentum.  */

*outputNeuralLayer = TNeuralNet_output(*aNN);
outputBiasVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_biases(*outputNeuralLayer));
outputERRVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_errors(*outputNeuralLayer));
NumberOfOutputs = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_outputs(*outputNeuralLayer));
for (OTk = 0; OTk < NumberOfOutputs; ++OTk)
    {
	delta = learning * outputERRVector[OTk];

    if ((delta < _MINREAL) && (delta > -_MINREAL)) delta = 0;
    if ((delta > _MAXREAL) || (delta < -_MAXREAL)) delta = 0;

    outputBiasVector[OTk] += delta;
    if (!((outputBiasVector[OTk] < _MAXREAL) && (outputBiasVector[OTk] > -_MAXREAL)))
        {
        outputBiasVector[OTk] = FSmartbase_Eval(gCP,gTP,*random,1,TINT(1)).u.Real - 5;
        }
    }

/* Adjust the weights of the hidden NeuralLayer record. */
/* Note: The deltas are also adjusted using momentum.   */

*hiddenNeuralLayer = TNeuralNet_hidden(*aNN);
hiddenOUTVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_outputs(*hiddenNeuralLayer));
inputERRVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_errors(*hiddenNeuralLayer));
*deltaMatrix = TNeuralLayer_deltas(*hiddenNeuralLayer);
*weightMatrix = TNeuralLayer_weights(*hiddenNeuralLayer);
*outputNeuralLayer = TNeuralNet_output(*aNN);
outputERRVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_errors(*outputNeuralLayer));
NumberOfOutputs = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_outputs(*outputNeuralLayer));
for (HDj = 0; HDj < NumberOfHidden; ++HDj)
    {
    /* Compute the new weight delta. */

    *weightVector = FSmartbase_Ref(gCP,gTP,2,*weightMatrix,TINT(HDj));
    ExitOnError(*weightVector);
    hiddenWeightVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*weightVector);
    *deltaVector = FSmartbase_Ref(gCP,gTP,2,*deltaMatrix,TINT(HDj));
    ExitOnError(*deltaVector);
    hiddenDeltaVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*deltaVector);
    for (OTk = 0; OTk < NumberOfOutputs; ++OTk)
        {
		delta = learning * outputERRVector[OTk] * hiddenOUTVector[HDj];

        if ((delta < _MINREAL) && (delta > -_MINREAL)) delta = 0;
        if ((delta > _MAXREAL) || (delta < -_MAXREAL)) delta = 0;

		hiddenDeltaVector[OTk] = delta;
        hiddenWeightVector[OTk] += delta;

        if (!((hiddenWeightVector[OTk] < _MAXREAL) && (hiddenWeightVector[OTk] > -_MAXREAL)))
            {
            hiddenWeightVector[OTk] = FSmartbase_Eval(gCP,gTP,*random,1,TINT(1)).u.Real - .5;
            }
       }
    }


/* Adjust the bias weights of the hidden NeuralLayer record. */
/* Note: The biases are also adjusted using momentum.  */

*hiddenNeuralLayer = TNeuralNet_hidden(*aNN);
hiddenBiasVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_biases(*hiddenNeuralLayer));
hiddenERRVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_errors(*hiddenNeuralLayer));
NumberOfHidden = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_outputs(*hiddenNeuralLayer));
for (HDj = 0; HDj < NumberOfHidden; ++HDj)
    {
	delta = learning * hiddenERRVector[HDj];
    if ((delta < _MINREAL) && (delta > -_MINREAL)) delta = 0;
    if ((delta > _MAXREAL) || (delta < -_MAXREAL)) delta = 0;

    hiddenBiasVector[HDj] += delta;
    if (!((hiddenBiasVector[HDj] < _MAXREAL) && (hiddenBiasVector[HDj] > -_MAXREAL)))
        {
        hiddenBiasVector[HDj] = FSmartbase_Eval(gCP,gTP,*random,1,TINT(1)).u.Real - .5;
        }
    }


/* Adjust the weights of the input NeuralLayer record. */
/* Note: The deltas are also adjusted using momentum.  */

*inputNeuralLayer = TNeuralNet_input(*aNN);
*hiddenNeuralLayer = TNeuralNet_hidden(*aNN);
*deltaMatrix = TNeuralLayer_deltas(*inputNeuralLayer);
*weightMatrix = TNeuralLayer_weights(*inputNeuralLayer);
inputOUTVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_outputs(*inputNeuralLayer));
hiddenERRVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_errors(*hiddenNeuralLayer));
NumberOfHidden = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_outputs(*hiddenNeuralLayer));
NumberOfInputs = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_outputs(*inputNeuralLayer));
for (INi = 0; INi < NumberOfInputs; ++INi)
    {
    /* Compute the new weight delta. */

    *weightVector = FSmartbase_Ref(gCP,gTP,2,*weightMatrix,TINT(INi));
    ExitOnError(*weightVector);
    inputWeightVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*weightVector);
    *deltaVector = FSmartbase_Ref(gCP,gTP,2,*deltaMatrix,TINT(INi));
    ExitOnError(*deltaVector);
    inputDeltaVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*deltaVector);
    for (HDj = 0; HDj < NumberOfHidden; ++HDj)
        {
		delta = learning * hiddenERRVector[HDj] * inputOUTVector[INi];

		if ((delta < _MINREAL) && (delta > -_MINREAL)) delta = 0;
		if ((delta > _MAXREAL) || (delta < -_MAXREAL)) delta = 0;

		inputDeltaVector[HDj] = delta;
		inputWeightVector[HDj] += delta;

        if (!((inputWeightVector[HDj] < _MAXREAL) && (inputWeightVector[HDj] > -_MAXREAL)))
            {
            inputWeightVector[HDj] = FSmartbase_Eval(gCP,gTP,*random,1,TINT(1)).u.Real - .5;
            }
        }
    }


FrameExit(argv[0]);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TNeuralNet_ComputeError

Compute the output error for the specified NeuralNet Object. The output signal is
compared to the specified goal vector (target) and the resulting error is computed
and placed in the Error property of the NeuralNet. The target must be a number vector.

(computeError aNeuralNet target)

Note:   This procedure computes the output error for the NeuralNet and places
        the error in the Error property of the NeuralNet. This does not adjust
        the weights for each layer, and does NOT train the NeuralNet.

#endif

TVAL    TNeuralNet_ComputeError(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM         NumberOfOutputs;
NUM			OTk;
NUM         n;
REAL        theta;
REAL        learning;
REAL        momentum;
NUM         filterID;
REAL        delta;
REAL        sum;
LpREAL      targetSignalVector;
LpREAL      outputSignalVector;

LpREAL      outputERRVector;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(outputNeuralLayer);
DeclareTVAL(aNN);
DeclareTVAL(ec);
DeclareTVAL(length);
DeclareTVAL(random);
EndFrame

*length  = TGVALUE("length");
*random  = TGVALUE("random");

/*  Check the mandatory arguments. */

if (argc < 2)
    {
	*ret = TERROR("!computeError: Not enough arguments!");
	FrameExit(*ret);
    }

if (argc > 2)
    {
	*ret = TERROR("!computeError: Not enough arguments!");
	FrameExit(*ret);
    }

if (!isNeuralNet(argv[0]))
    {
	*ret = TERROR("!computeError: Expecting argument 1 to be a neuralNet!");
	FrameExit(*ret);
    }


if  (argv[1].Tag != TYNUMVECTOR)
    {
	*ret = TERROR("!computeError: Expecting argument 2 to be a number vector!");
	FrameExit(*ret);
    }

    
/* Save all the mandatory arguments. */

*aNN = argv[0];
*ec = TNeuralNet_theta(*aNN);
theta = (ec->Tag == TYREAL) ? ec->u.Real : (ec->Tag == TYNUM) ? ec->u.Int : 0 ;
*ec = TNeuralNet_momentum(*aNN);
momentum = (ec->Tag == TYREAL) ? ec->u.Real : (ec->Tag == TYNUM) ? ec->u.Int : 0 ;
*ec = TNeuralNet_learning(*aNN);
learning = (ec->Tag == TYREAL) ? ec->u.Real : (ec->Tag == TYNUM) ? ec->u.Int : 0 ;
filterID = TNeuralNet_Continuous;
*ec = TNeuralNet_filter(*aNN);
if (ec->Tag == TYSYMBOL)
    {
    if (ec->u.Object == (TObject*)gCP->TNeuralNet_ContinuousSym) 
        filterID = TNeuralNet_Continuous;
    else
    if (ec->u.Object == (TObject*)gCP->TNeuralNet_SigmoidalSym) 
        filterID = TNeuralNet_Sigmoidal;
    else
    if (ec->u.Object == (TObject*)gCP->TNeuralNet_BinarySym) 
        filterID = TNeuralNet_Binary;
    else
    if (ec->u.Object == (TObject*)gCP->TNeuralNet_BipolarSym) 
        filterID = TNeuralNet_Bipolar;
    }

/* Get the number of outputs, and the error vector. */

*outputNeuralLayer = TNeuralNet_output(*aNN);
outputSignalVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_outputs(*outputNeuralLayer));
NumberOfOutputs = FSmartbase_VectorLen(gCP,gTP,TNeuralLayer_outputs(*outputNeuralLayer));
outputERRVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,TNeuralLayer_errors(*outputNeuralLayer));

/*  Compare the target vector to the NeuralLayer record output */
/*  vector. Check that the number of outputs do match exactly. */

targetSignalVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,argv[1]);
n = FSmartbase_VectorLen(gCP,gTP,argv[1]);
if (NumberOfOutputs > n)
    {
	*ret = TERROR("!propagateBackward: argument 2 has too few output signals!");
	FrameExit(*ret);
    }

/*  Compare the target vector to the NeuralLayer record output vector. */
/*  Note: The output signals are taken from the right of the target vector. */

n = n - NumberOfOutputs;
sum = 0;
for (OTk = 0; OTk < NumberOfOutputs; ++OTk)
    {
	/* Compute the output versus target signal error */
    delta = (targetSignalVector[OTk + n] - outputSignalVector[OTk]);
    sum += (delta * delta);

	/* Multiply the output error by the derivative of the output filter function. */
    switch (filterID)
        {
        case    TNeuralNet_Sigmoidal:
            outputERRVector[OTk] = delta * (outputSignalVector[OTk] * (1.0 - outputSignalVector[OTk]));
            break;
    
        case    TNeuralNet_Binary:
            outputERRVector[OTk] = delta * (1);
            break;
    
        case    TNeuralNet_Bipolar:
            outputERRVector[OTk] = delta * (1);
            break;
    
        case    TNeuralNet_Continuous:
        default:
            outputERRVector[OTk] = delta * (1);
            break;
        }
    }
TNeuralNet_error(*aNN) = TREAL(sum);



FrameExit(TNeuralNet_error(*aNN));
}

/*--------------------------------------------------------------------------------------- */
#if 0
TNeuralNet_Clear

Reset the specified NeuralNet Object so it can be retrained. The weights are all
randomized.

(clear NeuralNet seed)

(clear NeuralNet)

Note:   This procedure destroys all the previous training.

#endif

TVAL    TNeuralNet_Clear(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
BOLE		pseudoSW = FALSE;
NUM			weightsVectorOutputIndex = 0;
NUM         NumberOfInputs;
NUM         NumberOfOutputs;
NUM         NumberOfHidden;
NUM         i;
NUM         j;
NUM         k;
LpREAL      hiddenBIASVector;
LpREAL      outputBIASVector;
LpREAL      inputDELTAVector;
LpREAL      hiddenDELTAVector;
LpREAL      inputWEIGHTVector;
LpREAL      hiddenWEIGHTVector;
StartFrame
DeclareTVAL(Vector);
DeclareTVAL(ret);
DeclareTVAL(weights);
DeclareTVAL(hiddenWeightVector);
DeclareTVAL(inputWeightVector);
DeclareTVAL(deltas);
DeclareTVAL(inputDeltaVector);
DeclareTVAL(hiddenDeltaVector);
DeclareTVAL(biases);
DeclareTVAL(ec);
DeclareTVAL(aNN);
DeclareTVAL(length);
DeclareTVAL(random);
DeclareTVAL(limit);
DeclareTVAL(seed);
DeclareTVAL(srandom);
DeclareTVAL(temp);
EndFrame

*length  = TGVALUE("length");
*random  = TGVALUE("random");
*limit   = TINT(1);


/*  Check the mandatory arguments. */


if (!isNeuralNet(argv[0]))
    {
	*ret = TERROR("!clear: (neural net) Expecting argument one to be a neuralNet!");
	FrameExit(*ret);
    }
else
if ((argc == 2) && (isNumIndex(&argv[1])))
    {
	*seed = TSYMBOL("seed");
	*srandom = TGVALUE("srandom");
	*ec = FSmartbase_Set(gCP,gTP,3,*srandom,*seed,argv[1]);
	ExitOnError(*ec);
	pseudoSW = TRUE;
	*random  = *srandom;
	argc = argc - 1;
    }
else
if ((argc == 2) && (argv[1].Tag == TYVECTOR))
    {
	goto SetWeights;
    }
else
if (argc != 1)
    {
	*ret = TERROR("!clear (neural net): Incorrect number of arguments!");
	FrameExit(*ret);
    }

/* Save all the mandatory arguments. */

*aNN = argv[0];

/* Clear all NeuralNet global variables. */

TNeuralNet_learning(*aNN) = TREAL(.25);
TNeuralNet_momentum(*aNN) = TREAL(.10);
TNeuralNet_theta(*aNN) = TINT(0);
TNeuralNet_error(*aNN) = TINT(0);

/* Randomize the weights in the input NeuralLayer record. */

*deltas = TNeuralLayer_deltas(TNeuralNet_input(*aNN));
*weights = TNeuralLayer_weights(TNeuralNet_input(*aNN));
*ec = FSmartbase_Eval(gCP,gTP,*length,1,*weights);
ExitOnError(*ec);
NumberOfInputs = ec->u.Int;
for (i = 0; i < NumberOfInputs; ++i)
    {
    *inputWeightVector = FSmartbase_Ref(gCP,gTP,2,*weights,TINT(i));
    ExitOnError(*inputWeightVector);
    inputWEIGHTVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*inputWeightVector);
    NumberOfHidden = FSmartbase_VectorLen(gCP,gTP,*inputWeightVector);
    *inputDeltaVector = FSmartbase_Ref(gCP,gTP,2,*deltas,TINT(i));
    ExitOnError(*inputDeltaVector);
    inputDELTAVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*inputDeltaVector);
    for (j = 0; j < NumberOfHidden; ++j)
        {
        inputWEIGHTVector[j] = FSmartbase_Eval(gCP,gTP,*random,1,TREAL(1.0)).u.Real - .5;
        inputDELTAVector[j] = 0;
        }
    }


/* Randomize the weights in the hidden NeuralLayer record. */

*biases = TNeuralLayer_biases(TNeuralNet_hidden(*aNN));
hiddenBIASVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*biases);
*deltas = TNeuralLayer_deltas(TNeuralNet_hidden(*aNN));
*weights = TNeuralLayer_weights(TNeuralNet_hidden(*aNN));
*ec = FSmartbase_Eval(gCP,gTP,*length,1,*weights);
ExitOnError(*ec);
NumberOfHidden = ec->u.Int;
for (j = 0; j < NumberOfHidden; ++j)
    {
    hiddenBIASVector[j] = FSmartbase_Eval(gCP,gTP,*random,1,TREAL(1.0)).u.Real - .5;
    *hiddenDeltaVector = FSmartbase_Ref(gCP,gTP,2,*deltas,TINT(j));
    ExitOnError(*hiddenDeltaVector);
    hiddenDELTAVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*hiddenDeltaVector);
    *hiddenWeightVector = FSmartbase_Ref(gCP,gTP,2,*weights,TINT(j));
    ExitOnError(*hiddenWeightVector);
    hiddenWEIGHTVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*hiddenWeightVector);
    NumberOfOutputs = FSmartbase_VectorLen(gCP,gTP,*hiddenWeightVector);
    for (k = 0; k < NumberOfOutputs; ++k)
        {
        hiddenWEIGHTVector[k] = FSmartbase_Eval(gCP,gTP,*random,1,TREAL(1.0)).u.Real - .5;
        hiddenDELTAVector[k] = 0;
        }
    }


/* Randomize the biases in the output NeuralLayer record. */

*biases = TNeuralLayer_biases(TNeuralNet_output(*aNN));
outputBIASVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*biases);
*ec = FSmartbase_Eval(gCP,gTP,*length,1,*biases);
ExitOnError(*ec);
NumberOfOutputs = ec->u.Int;
for (k = 0; k < NumberOfOutputs; ++k)
    {
    outputBIASVector[k] = FSmartbase_Eval(gCP,gTP,*random,1,TREAL(1.0)).u.Real - .5;
    }


FrameExit(argv[0]);



SetWeights:
/******************************************************/
/*  Set Weights                                       */
/******************************************************/

/* Save all the mandatory arguments. */

*aNN = argv[0];
*Vector = argv[1];

/* Compare number of inputs in NeuralNet with number of inputs saved in the weights vector. */

*deltas = TNeuralLayer_deltas(TNeuralNet_input(*aNN));
*weights = TNeuralLayer_weights(TNeuralNet_input(*aNN));
*ec = FSmartbase_Eval(gCP,gTP,*length,1,*weights);
ExitOnError(*ec);
NumberOfInputs = ec->u.Int;
*temp = FSmartbase_Ref(gCP,gTP,2,*Vector,TINT(weightsVectorOutputIndex++));
ExitOnError(*temp);
if (NumberOfInputs != asNumIndex(temp))
    {
	*ret = TERROR("!clear (neural net): Mismatch in number of inputs!");
	FrameExit(*ret);
    }

/* Compare number of hidden units in NeuralNet with number of hidden units saved in the weights vector. */

*weights = TNeuralLayer_weights(TNeuralNet_hidden(*aNN));
*ec = FSmartbase_Eval(gCP,gTP,*length,1,*weights);
ExitOnError(*ec);
NumberOfHidden = ec->u.Int;
*temp = FSmartbase_Ref(gCP,gTP,2,*Vector,TINT(weightsVectorOutputIndex++));
ExitOnError(*temp);
if (NumberOfHidden != asNumIndex(temp))
    {
	*ret = TERROR("!clear (neural net): Mismatch in number of hidden units!");
	FrameExit(*ret);
    }

/* Compare number of outputs in NeuralNet with number of outputs saved in the weights vector. */

*biases = TNeuralLayer_biases(TNeuralNet_hidden(*aNN));
hiddenBIASVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*biases);
*deltas = TNeuralLayer_deltas(TNeuralNet_hidden(*aNN));
*weights = TNeuralLayer_weights(TNeuralNet_hidden(*aNN));
*ec = FSmartbase_Eval(gCP,gTP,*length,1,*weights);
ExitOnError(*ec);
NumberOfHidden = ec->u.Int;

*biases = TNeuralLayer_biases(TNeuralNet_output(*aNN));
outputBIASVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*biases);
*ec = FSmartbase_Eval(gCP,gTP,*length,1,*biases);
ExitOnError(*ec);
NumberOfOutputs = ec->u.Int;
*temp = FSmartbase_Ref(gCP,gTP,2,*Vector,TINT(weightsVectorOutputIndex++));
ExitOnError(*temp);
if (NumberOfOutputs != asNumIndex(temp))
    {
	*ret = TERROR("!clear (neural net): Mismatch in number of outputs!");
	FrameExit(*ret);
    }

/* Compare the filter in the NeuralNet with the filter saved in the weights vector. */

*temp = FSmartbase_Ref(gCP,gTP,2,*Vector,TINT(weightsVectorOutputIndex++));
ExitOnError(*temp);
*ec = FPredicate2_FullCompare(gCP,gTP,*temp,TNeuralNet_filter(*aNN));
if (isERROR(ec) || !isCompareEQ(ec))
    {
	*ret = TERROR("!clear (neural net): Mismatch in filter methods!");
	FrameExit(*ret);
    }

/* Restore all NeuralNet global variables. */

*temp = FSmartbase_Ref(gCP,gTP,2,*Vector,TINT(weightsVectorOutputIndex++));
ExitOnError(*temp);
TNeuralNet_learning(*aNN) = *temp;

*temp = FSmartbase_Ref(gCP,gTP,2,*Vector,TINT(weightsVectorOutputIndex++));
ExitOnError(*temp);
TNeuralNet_momentum(*aNN) = *temp;

*temp = FSmartbase_Ref(gCP,gTP,2,*Vector,TINT(weightsVectorOutputIndex++));
ExitOnError(*temp);
TNeuralNet_theta(*aNN) = *temp;

*temp = FSmartbase_Ref(gCP,gTP,2,*Vector,TINT(weightsVectorOutputIndex++));
ExitOnError(*temp);
TNeuralNet_error(*aNN) = *temp;

*temp = FSmartbase_Ref(gCP,gTP,2,*Vector,TINT(weightsVectorOutputIndex++));
ExitOnError(*temp);
TNeuralNet_notes(*aNN) = *temp;

/* Restore the weights in the input NeuralLayer record. */

*deltas = TNeuralLayer_deltas(TNeuralNet_input(*aNN));
*weights = TNeuralLayer_weights(TNeuralNet_input(*aNN));
*ec = FSmartbase_Eval(gCP,gTP,*length,1,*weights);
ExitOnError(*ec);
NumberOfInputs = ec->u.Int;
for (i = 0; i < NumberOfInputs; ++i)
    {
    *inputWeightVector = FSmartbase_Ref(gCP,gTP,2,*weights,TINT(i));
    ExitOnError(*inputWeightVector);
    inputWEIGHTVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*inputWeightVector);
    NumberOfHidden = FSmartbase_VectorLen(gCP,gTP,*inputWeightVector);
    *inputDeltaVector = FSmartbase_Ref(gCP,gTP,2,*deltas,TINT(i));
    ExitOnError(*inputDeltaVector);
    inputDELTAVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*inputDeltaVector);
    for (j = 0; j < NumberOfHidden; ++j)
        {
        inputWEIGHTVector[j] = FSmartbase_Ref(gCP,gTP,2,*Vector,TINT(weightsVectorOutputIndex++)).u.Real;
		/* Delta vectors not implemented yet...
		inputDELTAVector[j] = FSmartbase_Ref(gCP,gTP,2,*Vector,TINT(weightsVectorOutputIndex++)).u.Real;
		*/
        inputDELTAVector[j] = 0;
        }
    }


/* Randomize the weights in the hidden NeuralLayer record. */

*biases = TNeuralLayer_biases(TNeuralNet_hidden(*aNN));
hiddenBIASVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*biases);
*deltas = TNeuralLayer_deltas(TNeuralNet_hidden(*aNN));
*weights = TNeuralLayer_weights(TNeuralNet_hidden(*aNN));
*ec = FSmartbase_Eval(gCP,gTP,*length,1,*weights);
ExitOnError(*ec);
NumberOfHidden = ec->u.Int;
for (j = 0; j < NumberOfHidden; ++j)
    {
    hiddenBIASVector[j] = FSmartbase_Ref(gCP,gTP,2,*Vector,TINT(weightsVectorOutputIndex++)).u.Real;
    *hiddenDeltaVector = FSmartbase_Ref(gCP,gTP,2,*deltas,TINT(j));
    ExitOnError(*hiddenDeltaVector);
    hiddenDELTAVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*hiddenDeltaVector);
    *hiddenWeightVector = FSmartbase_Ref(gCP,gTP,2,*weights,TINT(j));
    ExitOnError(*hiddenWeightVector);
    hiddenWEIGHTVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*hiddenWeightVector);
    NumberOfOutputs = FSmartbase_VectorLen(gCP,gTP,*hiddenWeightVector);
    for (k = 0; k < NumberOfOutputs; ++k)
        {
        hiddenWEIGHTVector[k] = FSmartbase_Ref(gCP,gTP,2,*Vector,TINT(weightsVectorOutputIndex++)).u.Real;
		/* Delta vectors not implemented yet...
        hiddenDELTAVector[k] = FSmartbase_Ref(gCP,gTP,2,*Vector,TINT(weightsVectorOutputIndex++)).u.Real;
		*/
        hiddenDELTAVector[k] = 0;
        }
    }


/* Randomize the biases in the output NeuralLayer record. */

*biases = TNeuralLayer_biases(TNeuralNet_output(*aNN));
outputBIASVector = (LpREAL)FSmartbase_VectorPtr(gCP,gTP,*biases);
*ec = FSmartbase_Eval(gCP,gTP,*length,1,*biases);
ExitOnError(*ec);
NumberOfOutputs = ec->u.Int;
for (k = 0; k < NumberOfOutputs; ++k)
    {
    outputBIASVector[k] = FSmartbase_Ref(gCP,gTP,2,*Vector,TINT(weightsVectorOutputIndex++)).u.Real;
    }


FrameExit(argv[0]);
}
