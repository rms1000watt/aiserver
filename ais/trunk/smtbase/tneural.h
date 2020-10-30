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
TNeuralNet.h 

Interface for the NeuralNet object which supports the functions required for the
implementation of Back Propagation Neural Nets. TNeuralNet is a defined structure 
of TStructure.

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TNeuralNet
#define _H_TNeuralNet

#include "tstruct.h"

/*  Type predicate macro for the NeuralNet class. */

#define isNeuralNet(tval)   ((tval.Tag == TYSTRUCTURE) && (Structure(tval)->itsCdr.Tag == TYSYMBOL) && (Structure(tval)->itsCdr.u.Symbol == gCP->TNeuralNet_NeuralNetSym))
#define isNeuralLayer(tval) ((tval.Tag == TYSTRUCTURE) && (Structure(tval)->itsCdr.Tag == TYSYMBOL) && (Structure(tval)->itsCdr.u.Symbol == gCP->TNeuralNet_NeuralLayerSym))


#ifdef _SMARTBASE

/*  Declare all member macros for the NeuralNet class. */

#define TNeuralNet_input(tval)      BindArray(tval)[0].Value
#define TNeuralNet_hidden(tval)     BindArray(tval)[1].Value
#define TNeuralNet_output(tval)     BindArray(tval)[2].Value
#define TNeuralNet_momentum(tval)   BindArray(tval)[3].Value
#define TNeuralNet_learning(tval)   BindArray(tval)[4].Value
#define TNeuralNet_filter(tval)     BindArray(tval)[5].Value
#define TNeuralNet_theta(tval)      BindArray(tval)[6].Value
#define TNeuralNet_notes(tval)      BindArray(tval)[7].Value
#define TNeuralNet_error(tval)      BindArray(tval)[8].Value

/*  Declare all member macros for the NeuralLayer class. */

#define TNeuralLayer_inputs(tval)	BindArray(tval)[0].Value
#define TNeuralLayer_outputs(tval)  BindArray(tval)[1].Value
#define TNeuralLayer_weights(tval)  BindArray(tval)[2].Value
#define TNeuralLayer_biases(tval)   BindArray(tval)[3].Value
#define TNeuralLayer_deltas(tval)   BindArray(tval)[4].Value
#define TNeuralLayer_errors(tval)   BindArray(tval)[5].Value

/*  Declare all output functions for this Class. */

#define     TNeuralNet_Continuous       0
#define     TNeuralNet_Sigmoidal        1
#define     TNeuralNet_Binary           2
#define     TNeuralNet_Bipolar          3

/*  Function declarations */

extern  void    TNeuralNet_Init					(LpXCONTEXT gCP,LpTHREAD gTP);

extern  TVAL	TNeuralNet_Clear				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL	TNeuralNet_GetWeights			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL    TNeuralNet_MakeNeuralNet		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL    TNeuralNet_PropagateForward		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL    TNeuralNet_PropagateBackward	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL    TNeuralNet_PropagateUserError	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL    TNeuralNet_ComputeError			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

#endif
#endif 
