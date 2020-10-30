;;/**********************************************************************************
;;    Copyright (C) 2008 Investment Science Corp.
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;***********************************************************************************/
;;
;;*************************************
;;*************************************
;; Exported Lambda File Cabinet Document
;;*************************************
;;*************************************

;;**EXPORTKEY**:%README
;#text#
;; *******************************************************************
;; name:     SVM
;; 
;; summary:  A series of support vector machine regression and machine
;;           learning Lambdas. Each SVM Lambda is a self-contained individual
;;           making them easier to integrate into your application.
;;
;;           There are several support vector machine learning algorithms
;;           represented herein (including Dr Platt's SMO algorithm). In
;;           addition there are several proprietary SVM algorithms, which
;;           perform much better than SMO.   
;;
;;			 Note1: Support Vector Machine regression can be highly accurate
;;				    when solving non-linear regression problems. However, the
;;                  accuracy varies from excellent to poor depending upon
;;                  the ratio of: the chosen Gaussian Sample Size (maxSVSize);
;;                  the number of regression variables (M); and the THEORETICAL
;;                  number of variables created by the kernel function to
;;                  make the non-linear problem linearly solvable. A simplified
;;                  example would be as follows.
;;
;;					Solving a quadratic regression problem with variableCount == 3,
;;                  y = sum{m from 0 until variableCount}(Cm*Xm*Xm), and 
;;                  a kernel function of vectorSquareInnerProduct, is very
;;                  accurate with a Gaussian Sample Size (maxSVSize) of 10.
;;                  However, if the variableCount is increased to 10, then the
;;                  accuracy completely breaks down and is not restored until
;;                  the Gaussian Sample Size is increased to around 100. An
;;                  explanation is as follows.
;;
;;					In order to make the quadratic regression linearly tractable,
;;                  the vectorSquareInnerProduct performs an on-the-fly squaring
;;                  of each training point vector. Thus, with a training point
;;                  vector of size three, the vectorSquareInnerProduct creates
;;                  the following on-the-fly THEORETICAL new training point:
;;					kernel(X1,X2,X3) => (X1,X2,X3,X1*X1,X2*X2,X3*X3,X1*X2,X1*X3,X2*X3).
;;                  Clearly the problem is now linearly solvable because the squared
;;                  variables are now terms in the THEORETICAL on-the-fly linear regression
;;                  created by the kernel. NOTICE however, that the THEORETICAL linear
;;                  regression, created on-the-fly by the kernel function, has nine variables
;;                  not three variables as in the original quadratic problem. Unless the
;;                  number of training points is greater than nine, and the Gaussian 
;;                  sample size is greater than nine, the on-the-fly linear regression
;;                  will not have enough data to get accurate results.
;; 
;;           Note2: See Cristianini, "Support Vector Machines", page 169.
;; *******************************************************************

















;;**EXPORTKEY**:gsmRegress
(defun gsmRegress(x y ...)
;; *******************************************************************
;; name:     gsmRegress
;; 
;; summary:  Trains a support vector machine regression Lambda, using the
;;           Sequential Error Estimation (SEE) algorithm and returns the 
;;           trained estimator Lambda. (multiple regression network)
;;
;;           The Sequential Error Estimation (SEE) algorithm, allows a
;;           single kernel function or a vector of kernel functions to 
;;           build a support vector regression network linked with a
;;           multiple regression model.
;;            
;;           Sequential sample sets of the training examples are used
;;           to build multiple support vector regression models of the
;;           training data. The best of these models are linked together,
;;           via multiple regression, to form a composite model. This
;;           sequential sampling method automatically supports out-of-sample
;;           testing during the training phase; and, allows larger training
;;           data sets to be estimated in a fraction of the time required
;;           for an exhaustive Gramm matrix regression.
;;
;;           The model error is calculated based upon the absolute error of
;;           each estimate (calculated as a percent of the target variable).
;;           An error estimation grid can be constructed which penializes the
;;           model error for each grid slot whose average Y values are not
;;           sequentially increasing. This method enhances model fit in cases
;;           with low signal-to-noise ratios.  
;;
;; Parms:    x:         The N by M vector array representing the original observations
;;                      in the form of:    x x ... x
;;                                         x x ... x
;;                                             ... 
;;                                         x x ... x
;;           y   		The N vector of dependent variables.
;;           maxGen  	(Optional)The maximum generation count before halting training.
;;           verboseSW  (Optional)True iff we are to set verbose mode on.
;;           kernelID   (Optional)The SVM regression kernel.
;;
;; Return:   Lambda: 	The trained SEE regression model Lambda.
;;
;;			 Note1: Support Vector Machine regression can be highly accurate
;;				    when solving non-linear regression problems. However, the
;;                  accuracy varies from excellent to poor depending upon
;;                  the ratio of: the chosen Gaussian Sample Size (mySampleSize);
;;                  the number of regression variables (M); and the THEORETICAL
;;                  number of variables created by the kernel function to
;;                  make the non-linear problem linearly solvable. A simplified
;;                  example would be as follows.
;;
;;					Solving a quadratic regression problem with variableCount == 3,
;;                  y = sum{m from 0 until variableCount}(Cm*Xm*Xm), and 
;;                  a kernel function of vectorSquareInnerProduct, is very
;;                  accurate with a Gaussian Sample Size (mySampleSize) of 10.
;;                  However, if the variableCount is increased to 10, then the
;;                  accuracy completely breaks down and is not restored until
;;                  the Gaussian Sample Size is increased to around 100. An
;;                  explanation is as follows.
;;
;;					In order to make the quadratic regression linearly tractable,
;;                  the vectorSquareInnerProduct performs an on-the-fly squaring
;;                  of each training point vector. Thus, with a training point
;;                  vector of size three, the vectorSquareInnerProduct creates
;;                  the following on-the-fly THEORETICAL new training point:
;;					kernel(X1,X2,X3) => (X1,X2,X3,X1*X1,X2*X2,X3*X3,X1*X2,X1*X3,X2*X3).
;;                  Clearly the problem is now linearly solvable because the squared
;;                  variables are now terms in the THEORETICAL on-the-fly linear regression
;;                  created by the kernel. NOTICE however, that the THEORETICAL linear
;;                  regression, created on-the-fly by the kernel function, has nine variables
;;                  not three variables as in the original quadratic problem. Unless the
;;                  number of training points is greater than nine, and the Gaussian 
;;                  sample size is greater than nine, the on-the-fly linear regression
;;                  will not have enough data to get accurate results.
;; 
;;           Note2: See Cristianini, "Support Vector Machines", page 169.
;;           Note3: This SVM regression Lambda uses the Sequential Error Estimation (SEE)
;;                  algorithm.
;; *******************************************************************
    pvars:(;; Public option settings
           (Integer:myBaggingCnt 03)			;; The count of support vector clusters to use in building the final model.
           (Integer:myBaggingPct 1.50)	     	;; The NLSE score higher than which we begin bagging of support vector clusters to use in building the final model.
           (Integer:myDefaultSampleSize 512)	;; The default size of the training sample sets, selected from the total training examples, during each iterative regression (8 columns cubed).
           ;; Public variables
           Lambda:myModel				        ;; The final and best SEE regression model. 
           Integer:Generations          		;; The number of training cycles used to train the current SEE regression model. 
           Integer:GenerationMax        		;; The maximum number of training cycles before training is halted at any error rate. 
           kernel   		       				;; The current support vector machine kernel (compositeKernel).
           Integer:mySampleSize     			;; Size of the training sample sets, selected from the total training examples, during each iterative regression. 
           Integer:mySampleCount       	 		;; Count of the training sample sets, selected from the total training examples, during each iterative regression. 
           mySampleHistory              		;; History of the training sample sets, selected from the total training examples, during each iterative regression. 
           mySVMParent                  		;; The parent Lambda of this SVM regression Lambda community. 
           myVerboseSW      					;; True iff we are to display progress on the console. 
           Integer:M							;; The number of elements in each training example (independent variables). 
           Integer:N							;; The number of training examples (independent variables). 
           IntVector:sortedY            		;; The N vector of target points, dependent variable, sorted in ascending order. 
           NumVector:W                  		;; The weight coefficient vector for the SEE regression model.
           ObjVector:WX                 		;; The object vector of support vectors for the SEE regression model.
           X                       				;; The N x M matrix of training examples, independent variables, for training the SEE regression model. 
           NumVector:Y                  		;; The N vector of training example targets (dependent variable). 
           Number:YAvg                  		;; The average of Y (dependent variable). 
           Number:YStd                  		;; The standard deviation of Y (dependent variable). 
           ;; System Settings
           ;; Public child methods
           clear			       				;; Clear the current support vector machine.
           completeKernel  	            		;; A built in complete kernel used for machine learning.
           compositeKernel              		;; A built in composite kernel used for machine learning.
           createSvmLambda			       		;; Return an Lambda ready to compute the svm output for a specified input vector.
           kernel   		       				;; Return the svm kernel output for the SEE regression model.
	       multipleRegression           		;; Performs a Gaussian multiple regression on the N x M+1 matrix
           scoreTCEandNLSE	                    ;; Compute the tail classification error and the normalized least squared error for the specified support vector block. 
           svmTraining		       				;; Train the svm machine on the specified training examples.
           trainRegressionModel		    		;; Incrementally train the SEE regression model.
           ;; Private maintenance child methods
           selfTest                				;; The self test method for this Lambda. 
           ;; Private maintenance templates
           (myHistoryTemplate #{Step: #void W: #void WX: #void NLSE: #void TCE: #void}) 
           ) ;; end of persistent variables
    ;; ***************************
    ;; Define Public Child Lambdas.
    ;; ***************************
    ;; Clear the current support vector machine.
    (defun clear()
       (setq kernel (lambda(k) k))
       (setq M 0)
       (setq N 0)
       (setq Generations 0) 
       (setq sortedY #void)
       (setq W #void)
       (setq X #void)
       (setq Y #void)
       (setq mySampleHistory #void)
       true) ; end clear
    ;; A built in complete kernel used for machine learning.
    (defun completeKernel(NumVector:X NumVector:Y)
       regs:(m M n N)
       regs:(NumPointer:pX NumPointer:pY (Number:fudge .0000000001))
       regs:(Number:kx (Number:xx 1.0) Number:ex Number:x Number:y Number:dy)
       ;; The following complete kernel function has proven superior, for
       ;; regression applications, than multiple user defined kernels. The
       ;; inspiration for this composite kernel was the body of research
       ;; on Radial Basis Functions for SVM classifiers (Google "Radial Basis Function").
       ;; Experimentation with composite dot product and kernel operators,
       ;; produced this composite kernel.
       ;;  
       ;; Notes on experimental trials:
       ;;   The completeKernel is far more computationally expensive than the compositeKernel,
       ;;   requiring SVM regressions to use approximately twice the elapsed computation time.
       ;;   In experimental trials on the selfTest nine base problems, the completeKernel performed 
       ;;   slightly better than the compositeKernel albeit requiring far more computation resources. 
       ;;   However, in experimental trials on stock market historical data, the compositeKernel 
       ;;   greatly outperformed the completeKernel and required less computation time.
       ;;   
       (setq M (length X))
       (setq N (length Y))
       (if (<> M N) (error "gsmRegress.compositeKernel: X and Y vectors must be the same length"))
       (setq pX X)
       (setq pY Y)
       (loop for n from 0 until N do
         (setq x pX[n])
         (setq y pY[n])
         (setq dy (- x y)) ;; simple difference
         (+= kx (* x y))   ;; dot product
         (+= kx (* dy dy)) ;; Euclidean distance
         (+= kx (* (tan x) (tan y))) ;; dot tan product 
         (+= kx (* (tanh x) (tanh y))) ;; dot tanh product 
         (+= kx (* (log (+ (abs x) fudge)) (log (+ (abs y) fudge)))) ;; dot log product
         (+= kx (* (sin x) (sin y))) ;; dot sin product
	     (+= kx (* (cos x) (cos y))) ;; dot cos product
         ) ; end N loop
       (+= xx (sqrt (abs kx))) ;; sublinear kernel
       (+= xx kx) ;; linear kernel
       (+= xx (* kx kx)) ;; square kernel
       (+= xx (* kx kx kx)) ;; cubic kernel
       (+= xx (setq ex (- (/ 2.0 (+ 1.0 (exp (- kx)))) 1.0))) ;; Gaussian continuous kernel [-1,+1]
       (+= xx (if (> ex 0.0) 1.0 -1.0)) ;; Gaussian bipolar kernel [-1,+1]
       (+= xx (tanh kx)) ;; hypertangent kernel
       xx) ; end completeKernel
    ;; A built in composite kernel used for machine learning.
    (defun compositeKernel(NumVector:X NumVector:Y)
       regs:(m M n N)
       regs:(NumPointer:pX NumPointer:pY)
       regs:(Number:kx (Number:xx 1.0) Number:ex Number:x Number:y Number:dy)
       ;; The following composite kernel function has proven superior, for
       ;; regression applications, than multiple user defined kernels. The
       ;; inspiration for this composite kernel was the body of research
       ;; on Radial Basis Functions for SVM classifiers (Google "Radial Basis Function").
       ;; Experimentation with composite dot product and kernel operators,
       ;; produced this composite kernel.
       ;;  
       ;; Notes on experimental trials:
       ;;   The completeKernel is far more computationally expensive than the compositeKernel,
       ;;   requiring SVM regressions to use approximately twice the elapsed computation time.
       ;;   In experimental trials on the selfTest nine base problems, the completeKernel performed 
       ;;   slightly better than the compositeKernel albeit requiring far more computation resources. 
       ;;   However, in experimental trials on stock market historical data, the compositeKernel 
       ;;   greatly outperformed the completeKernel and required less computation time.
       ;; 
       (setq M (length X))
       (setq N (length Y))
       (if (<> M N) (error "gsmRegress.compositeKernel: X and Y vectors must be the same length"))
       (setq pX X)
       (setq pY Y)
       (loop for n from 0 until N do
         (setq x pX[n])
         (setq y pY[n])
         (setq dy (- x y)) ;; simple difference
         (+= kx (* x y))   ;; dot product
         (+= kx (* dy dy)) ;; Euclidean distance
         ) ; end N loop
       (+= xx kx) ;; linear kernel
       (+= xx (* kx kx)) ;; square kernel
       (+= xx (* kx kx kx)) ;; cubic kernel
       (+= xx (setq ex (- (/ 2.0 (+ 1.0 (exp (- kx)))) 1.0))) ;; Gaussian continuous kernel [-1,+1]
       (+= xx (if (> ex 0.0) 1.0 -1.0)) ;; Gaussian bipolar kernel [-1,+1]
       (+= xx (tanh kx)) ;; hypertangent kernel
       xx) ; end compositeKernel
	;; summary:  Performs a Gaussian multiple regression on the N by M+1 matrix
	;; Parms:    MXY:     The N by M+1 matrix representing the original observations
	;;                    in the form of:    x x ... x y
	;;                                       x x ... x y
	;;                                           ... 
	;;                                       x x ... x y
	;; Return:   C:       The M coefficient vector for the regression.
	;; Note1:    Average error statistics are computed as a percent of the target (dependent variable).
	;; Note2:    See Sedgewick[2] chap 37.
	(defun multipleRegression(NumMatrix:MXY)
	    vars:(NumMatrix:Xt NumVector:C)
	    ;; Perform a least squares regression on all the factors.
	    (setq Xt (|Gv:makeGaussianMatrix| MXY))
	    (setq Xt (|Gv:matrixGaussianEliminate| Xt true))
	    (setq C (|Gv:matrixGaussianSubstitute| Xt))
	    ;; Return the coefficient vector for the regression.
	    C) ; end multipleRegression
    ; Return an Lambda ready to compute the svm output for a specified input points.
    (defun createSvmLambda(...)
       regs:(k K mm MM nn NN nw NW)
       vars:(NumVector:hW ObjVector:hWX)
       vars:(Lambda sampleSVCluster)
       (setq Lambda (eval "(lambda(x) pvars:(Expressions Integer:M Integer:N Strategy Kernel ObjVector:WX NumVector:W Number:WW Number:NLSE Number:TCE) regs:(n NN Number:xn Number:wn Number:ey NumPointer:pW) (setq NN (length W)) (setq pW W) (loop for n from 0 until NN do (setq wn pW[n]) (if (<> wn 0.0) then (begin (setq xn (Kernel x WX[n])) (*= xn wn) (+= ey xn)  )) ) (*= ey WW) ey)"))
       (setq Lambda.Strategy svmSEE:)
       ;; Sort the sample history support vector clusters by NLSE score.
       (sort mySampleHistory (lambda(x y) (< x.NLSE y.NLSE)))
       ;; Count all the support vectors, with non-zero coefficients, for each trained model in the sample history vector.
       (if (< mySampleHistory[0].NLSE myBaggingPct) then (setq K 1) else (setq K (min myBaggingCnt (length mySampleHistory))))
       (setq Lambda.WW (if (<= K 0) then 0.0 else (/ 1.0 (number K))))
       (setq Lambda.Kernel kernel)
       (loop for k from 0 until K do
         (setq sampleSVCluster mySampleHistory[k])
         (setq hW sampleSVCluster.W)
         (setq NN (length hW))
         (loop for nn from 0 until NN do (if (<> hW[nn] 0.0) (++ NW)))
         ) ; end first K loop
       (setq Lambda.N NW)
       (setq Lambda.W (new Vector: Number: NW))
       (setq Lambda.WX (new Vector: Object: NW))
       (setq Lambda.M M)
       ;; Average all the support vectors, with non-zero coefficients, for each trained model in the sample history vector.
       (loop for k from 0 until K do
         (setq sampleSVCluster mySampleHistory[k])
         (setq hW sampleSVCluster.W)
         (setq hWX sampleSVCluster.WX)
         (setq NN (length hW))
         (loop for nn from 0 until NN do 
           (if (<> hW[nn] 0.0)
               (begin
                 (setq Lambda.W[nw] hW[nn])
                 (setq Lambda.WX[nw] hWX[nn])
                 (++ nw)
               )) ; end if
           ) ; end N loop
         ) ; end second K loop
       (scoreTCEandNLSE Lambda.Pv)
       (setq myModel Lambda)
       Lambda) ; end createSvmLambda
    ;; Compute the tail classification error and the normalized least squared error for the specified support vector block.
    (defun scoreTCEandNLSE(sampleSVCluster)
       regs:(k n mm MM nn NN NL NH buckets) 
       regs:(Number:tcEY Number:tcY Number:y Number:ey Number:nlse Number:err Number:wn Number:dot) 
       vars:(NumVector:x NumVector:wx NumVector:EY IntVector:sortedEY) 
       ;; Compute the EY vector from the support vector block.
       (setq NN (length Y))
       (setq EY (new Vector: Number: NN))
       (loop for nn from 0 until NN do
         (setq x X[nn])
         (setq y Y[nn])
         (setq ey  0.0)
         (setq MM (length sampleSVCluster.W))
         (setq wx sampleSVCluster.W)
         (loop for mm from 0 until MM do
           (setq wn wx[mm])
           (if (<> wn 0.0)
               (begin
                 (setq dot (kernel sampleSVCluster.WX[mm] x))
                 (+= ey (* wn dot))
               )) ; end if
           ) ; end ey loop
         (setq EY[nn] ey)
         ) ; end EY loop 
       ;; Classify the top 10% and bottom 10% of Y using the specified Lambda.
       (setq sortedEY (|Gv:sort| EY < true))
	   (setq NN (length EY))
	   (setq InvN (/ 1.0 (number NN)))
       (setq buckets 10)
	   (setq NL (/ NN buckets))
	   (setq NH (- NN NL))
	   (loop for n from 0 until NN do
	     (setq ey EY[n])
	     (setq y Y[n])
         (setq err (- ey y))
         (+= nlse (* err err))
	     (setq k sortedY[n])
	     (setq y Y[k])
         (if (< n NL) (-= tcY y))
         (if (>= n NH) (+= tcY y))
	     (setq k sortedEY[n])
	     (setq y Y[k])
         (if (< n NL) (-= tcEY y))
         (if (>= n NH) (+= tcEY y))
	     ) ; end scoring loop
       (/= nlse (number NN))
       (setq nlse (sqrt nlse))
       (/= nlse YStd)
       (setq tcEY (/ (- 2.0 (+ 1.0 (/ tcEY tcY))) 2.0))
       (setq sampleSVCluster.NLSE nlse)
       (setq sampleSVCluster.TCE tcEY)
       Lambda) ; end scoreTCEandNLSE
    ;; Train the svm machine on the specified inputs and model.
	;; Parms:    x:         The N by M vector array representing the original observations
	;;                      in the form of:    x x ... x
	;;                                         x x ... x
	;;                                             ... 
	;;                                         x x ... x
	;;           y   		The N vector of dependent variables.
    ;;           maxGen  	The maximum generation count before halting training.
    ;;           verboseSW  True iff we are to set verbose mode on.
    ;;
	;; Return:   Lambda: 	The trained SEE regression model Lambda.
    (defun svmTraining(x y maxGen verboseSW kernelID)
        regs:(k K m n NN SN begK)
        vars:(Lambda sampleSVCluster)
	    ;; Clear support vector machine for retraining.
	    (clear)
	    ;; Retrieve any optional arguments and perform setup.
	    (setq GenerationMax maxGen)
	    (setq myVerboseSW verboseSW)
	    (if (or (<> (isVector x) true) (<> (isVector x[0]) true)) (error "gsmRegress: X argument must be a Vector Array of rank 2"))
	    (setq m (length x[0]))
	    (setq n (length x))
	    (if (or (<> (isVector y) true) (<> (length y) n)) (error "gsmRegress: Y argument must be a Vector of length the same as X"))
	    ;; Initialize the persistent variables before proceeding with training.
	    (setq X x)
	    (setq Y y)
        (setq YAvg (avg Y)) 
        (setq YStd (stdev Y)) 
	    (setq M m)
	    (setq N n)
	    ;; Initialize the user specified support vector machine kernel.
	    (cond
	      ((or (= kernelID "default") (= kernelID #void)) (setq kernel compositeKernel))
	      ((= kernelID "binary") (setq kernel |Gv:vectorBinaryInnerProduct|))
	      ((= kernelID "bipolar") (setq kernel |Gv:vectorBinaryInnerProduct|))
	      ((= kernelID "complete") (setq kernel completeKernel))
	      ((= kernelID "composite") (setq kernel compositeKernel))
	      ((= kernelID "cosine") (setq kernel |Gv:vectorCosineInnerProduct|))
	      ((= kernelID "cube") (setq kernel |Gv:vectorCubeInnerProduct|))
	      ((= kernelID "exp") (setq kernel |Gv:vectorExpInnerProduct|))
	      ((= kernelID "linear") (setq kernel |Gv:vectorInnerProduct|))
	      ((= kernelID "log") (setq kernel |Gv:vectorLogInnerProduct|))
	      ((= kernelID "quart") (setq kernel |Gv:vectorQuartInnerProduct|))
	      ((= kernelID "quint") (setq kernel |Gv:vectorQuintInnerProduct|))
	      ((= kernelID "sigmoid") (setq kernel |Gv:vectorSigmoidInnerProduct|))
	      ((= kernelID "sine") (setq kernel |Gv:vectorSineInnerProduct|))
	      ((= kernelID "square") (setq kernel |Gv:vectorSquareInnerProduct|))
	      ((= kernelID "tan") (setq kernel |Gv:vectorTanInnerProduct|))
	      ((= kernelID "tanh") (setq kernel |Gv:vectorTanhInnerProduct|))
	      (else (error (append "gsmRegress.svmTraining: unknown SVM kernel identifier [" kernelID "]")))
	      ) ; end cond 
        (setq sortedY (sort Y < true))
        (setq W #void)
	    ;; Run multiple generations taking the first one to achieve success.
	    ;; Note: If no generation achieves success, we take the best so far.
        (setq mySampleSize myDefaultSampleSize)     
        (if (< N mySampleSize) (setq mySampleSize N))
        (setq mySampleCount (/ N mySampleSize))
        (if (< mySampleCount 1) then (begin (setq mySampleSize N) (setq mySampleCount 1)))
        (setq mySampleHistory (new Vector:))
	    (setq Generations 0)
	    ;; Train the next generation.
	    (while (< Generations GenerationMax) do
	       (trainRegressionModel)
	       (++ Generations)
	       ) ; end while
	    ;; Generate the final absolute error score after training.
	    HaltTraining::
        (setq Lambda (createSvmLambda))
	    (if myVerboseSW (writeln "gsmRegress: Final Model Generations = [" Generations "], SV's = [" Lambda.N "]"))       
        Lambda) ; end svmTraining
    ;; Incrementally train the SEE model on the two training points.
    (defun trainRegressionModel()
       regs:(k K m n rm sn)
       regs:(Number:dotProduct Number:y Number:xk Number:xn)
       regs:(IntPointer:pSV IntPointer:pSortedY NumPointer:pXn NumPointer:pXk NumPointer:pRM)
       regs:(Number:avgY Number:avgErr)
       regs:(Integer:NN                  ;; Number of total training examples.
             Integer:SN                  ;; Number of seed support vectors to use in Gaussian initialization.
             Integer:firstStep           ;; Example first step size for use in support vector selection.
             Integer:stepInc             ;; Example selection step size for use in support vector selection.
             Number:weightFactor         ;; Weight factor for use in support vector merging.
             ) ; end register variables
       vars:(NumVector:CS                ;; The Gaussian regression coefficients for the support vectors.
             Structure:sampleSVCluster   ;; The history record for the nth sample set.
             NumMatrix:RM 				 ;; The regression matrix regressing the support vectors against all of the training points in X.
             IntVector:sortedHistories	 ;; The Integer Vector of the sorted sample set histories.
             IntVector:SV				 ;; The indices of the current support vector set. 
             NumVector:Xk 				 ;; The Number Vector, of the independent variables, for the kth training example.
             NumVector:Xn 				 ;; The Number Vector, of the independent variables, for the nth training example.
             ObjVector:tWX 				 ;; The Object Vector, of the support vectors, for the nth sample set.
             ) ; end temporary variables
       ;; Zero the errors where there are already support vectors.
       (setq firstStep (integer Generations))
       (if (>= firstStep mySampleCount) (goto TrainingCompleted:))
       (setq NN N)
       ;; Extend the support vectors with the examples with the worst percent errors
       (setq SN mySampleSize)
       (setq SV (new Vector: Integer: SN))
       (setq stepInc mySampleCount)
       (setq pSV SV)
       (setq pSortedY sortedY)
       (setq n firstStep)
       (vmregRunInHardware start:)
       (loop for sn from 0 until SN do
		  (setq pSV[sn] pSortedY[n])
          (+= n stepInc)
          ) ; end loop 
       (vmregRunInHardware stop:)
       (setq SN (length SV))
       ;; Construct the support vectors regression matrix.
       ;; Note: We attempt to regress a linear model of the support vectors
       ;;       against all of the training points in X. The Regression
       ;;       Matrix is of the following form:
       ;;          kernel(X[0],X[SV[0]]) ... kernel(X[0],X[SV[SN]]) Y[0]
       ;;          kernel(X[1],X[SV[0]]) ... kernel(X[1],X[SV[SN]]) Y[1]
       ;;                  ...           ...              ...
       ;;          kernel(X[N],X[SV[0]]) ... kernel(X[N],X[SV[SN]]) Y[N]
       (setq RM #void)
       (setq RM (new Matrix: number: 2 NN (addi SN 1)))
       (vmregRunInHardware start:)
       (setq pSV SV)
       (setq pRM RM)
       (setq rm -1)
       (loop for n from 0 until NN do
           (loop for m from 0 until SN do
              (setq k pSV[m])
              (setq Xk X[k])
              (setq Xn X[n])
              (setq dotProduct (kernel Xk Xn))
              (++ rm)
              (setq pRM[rm] dotProduct)
              ) ; end SN loop
           (++ rm)
           (setq pRM[rm] Y[n])
           ) ; end NN loop
       (vmregRunInHardware stop:)
       ;; Solve for the Gaussian coefficients of the support vectors.
       ;; Note: We perform a Gaussian linear regression on the Regression
       ;;       Matrix, returning an SN Vector of coefficients, which we
       ;;       set as the weights, in the SVM  model, of each of the 
       ;;       support vectors respectively. All other non-support-vector
       ;;       weights, in the initial SVM model, are set to zero.
       (setq CS (multipleRegression RM))
       (setq K (length mySampleHistory))
       (setq mySampleHistory[K] (setq sampleSVCluster (new myHistoryTemplate)))
       (setq sampleSVCluster.Step Generations)
       (setq sampleSVCluster.W CS)
       (setq sampleSVCluster.WX (setq tWX (new Vector: Object: SN)))
       (vmregRunInHardware start:)
       (loop for m from 0 until SN do
          (setq k SV[m])
          (setq tWX[m] X[k])
          ) ; end loop
       (vmregRunInHardware stop:)
       (scoreTCEandNLSE sampleSVCluster)
       ;; This generation of SVM model training is complete.
       TrainingCompleted::
       (if myVerboseSW (writeln "gsmRegress: completed training of generation [" Generations "]")) 
       ;; Return success
       true) ; end trainRegressionModel
    ;; ****************************************
    ;; Define Private Maintenance Child Lambdas.
    ;; ****************************************
    ;; The self test method for this Lambda.
    (defun selfTest(Test Ns Ms maxGen verboseSW kernelID)
       vars:(k m n g G y ey C c X Y Yv avgY stdY
             Lambda nlse err pct properties 
             startTime endTime startTimeT endTimeT
             (checkResults true)
             (tol 0.0) (errStop 0.01) (Cs 1.0)
             ) ; end temporary variables
       (clear)
       (setq startTimeT (getTickCount 0))
       (setq srandom.seed 8192.0)
       (setq myVerboseSW verboseSW)      
       ;; Select the requested test case
       ;; Test Case linear 
       (if (= Test linear:)
           (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: We support a bias by having X[0] == 1 for all N.
		       ;; Note2: This algorithm seems to work well when N is at least 25 times M.
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do 
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (setq y (+ y (* X[n][m] C[m])))
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: linear")
		       (setq Lambda (setq Lambda (gsmRegress.svmTraining X Y maxGen verboseSW kernelID)))
		       (if (= myVerboseSW false) (writeln "gsmRegress: N = [" Ns "], M = [" Lambda.M "], Generations = [" Generations  "], SV's = [" Lambda.N "]")) 
               (if (= checkResults true)
                   (begin
                      (setq nlse 0.0)
                      (setq avgY (avg Y))
                      (setq stdY (stdev Y))
		              (loop for n from 0 until N do
		                 (setq y Y[n])
		                 (setq ey (Lambda X[n]))
                         (setq err (abs (- ey y)))
                         (setq pct (/ err stdY)) 
		                 (+= nlse (* err err))
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" ey "] y=[" y "] err=[" err "] err%=[" pct "]"))
		                 ) ; end N loop
                      (setq nlse (/ (sqrt (/ nlse N)) stdY)) 
		              (writeln "gsmRegress: nlse=[" nlse "], avgY=[" avgY "], stdY=[" stdY "]")
		           )) ; end if
          )) ; end Test Case linear
       ;; Test Case linearSigmoid 
       (if (= Test linearSigmoid:)
           (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: The inputs, X, are restricted to the sigmoid domain.
		       ;; Note2: We support a bias by having X[0] == 1 for all N.
		       ;; Note3: This algorithm seems to work well when N is at least 25 times M.
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 1.0) .5))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do 
		          	 (setq X[n][m] (- (srandom .999999999) 0))
		             (setq y (+ y (* X[n][m] C[m])))
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: linearSigmoid")
		       (setq Lambda (setq Lambda (gsmRegress.svmTraining X Y maxGen verboseSW kernelID)))
		       (if (= myVerboseSW false) (writeln "gsmRegress: N = [" Ns "], M = [" Lambda.M "], Generations = [" Generations  "], SV's = [" Lambda.N "]")) 
               (if (= checkResults true)
                   (begin
                      (setq nlse 0.0)
                      (setq avgY (avg Y))
                      (setq stdY (stdev Y))
		              (loop for n from 0 until N do
		                 (setq y Y[n])
		                 (setq ey (Lambda X[n]))
                         (setq err (abs (- ey y)))
                         (setq pct (/ err stdY)) 
		                 (+= nlse (* err err))
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" ey "] y=[" y "] err=[" err "] err%=[" pct "]"))
		                 ) ; end N loop
                      (setq nlse (/ (sqrt (/ nlse N)) stdY)) 
		              (writeln "gsmRegress: nlse=[" nlse "], avgY=[" avgY "], stdY=[" stdY "]")
		           )) ; end if
          )) ; end Test Case linearSigmoid
       ;; Test Case srandom 
       (if (= Test srandom:)
           (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: We support a bias by having X[0] == 1 for all N.
		       ;; Note2: This algorithm seems to work well when N is at least 25 times M.
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (setq y (+ y (* X[n][m] C[m])))
		             ) ; end M loop
		          (setq Y[n] (+ (* y .8) (* y (srandom .4))))
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: srandom")
		       (setq Lambda (setq Lambda (gsmRegress.svmTraining X Y maxGen verboseSW kernelID)))
		       (if (= myVerboseSW false) (writeln "gsmRegress: N = [" Ns "], M = [" Lambda.M "], Generations = [" Generations  "], SV's = [" Lambda.N "]")) 
               (if (= checkResults true)
                   (begin
                      (setq nlse 0.0)
                      (setq avgY (avg Y))
                      (setq stdY (stdev Y))
		              (loop for n from 0 until N do
		                 (setq y Y[n])
		                 (setq ey (Lambda X[n]))
                         (setq err (abs (- ey y)))
                         (setq pct (/ err stdY)) 
		                 (+= nlse (* err err))
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" ey "] y=[" y "] err=[" err "] err%=[" pct "]"))
		                 ) ; end N loop
                      (setq nlse (/ (sqrt (/ nlse N)) stdY)) 
		              (writeln "gsmRegress: nlse=[" nlse "], avgY=[" avgY "], stdY=[" stdY "]")
		           )) ; end if
          )) ; end Test Case srandom
       ;; Test Case mixedRandom 
       (if (= Test mixedRandom:)
           (begin
		       ;; Create a test polynomial linear model where y = C[0]*X[0] + C[1]*X[1] + C[2]*X[2] ...
		       ;; Create a test polynomial square model where y = C[0]*X[0]*X[0] + C[1]*X[1]*X[1] + C[2]*X[2]*X[2] ...
		       ;; Create a test polynomial sin model where y = C[0]*sin(X[0]) + C[1]*sin(X[1]) + C[2]*sin(X[2]) ...
		       ;; Create a test polynomial log model where y = C[0]*log(abs(X[0])+.000001) + C[1]*log(abs(X[1])+.000001) + C[1]*log(abs(X[2])+.000001) ...
               ;; These four models are mixed together and random noise is added.
		       ;; Note1: We support a bias by having X[0] == 1 for all N.
		       ;; Note2: This algorithm seems to work well when N is at least 25 times M.
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
                  (setq k (modi n 4)) 
 		          (setq X[n][0] 1.0)
 		          (setq X[n][1] (number k))
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 2 until M do
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
                     ;; Mix the four models together
                     (cond
                        ;; Linear model
                        ((= k 0) (setq y (+ y (* X[n][m] C[m]))))
                        ;; Square model
                        ((= k 1) (setq y (+ y (* X[n][m] X[n][m] C[m]))))
                        ;; Sine model
                        ((= k 2) (setq y (+ y (* (|Gv:sin| X[n][m]) C[m]))))
                        ;; Log model
                        (else (setq y (+ y (* (|Gv:log| (+ .000001 (|Gv:abs| X[n][m]))) C[m]))))
                        ) ; end cond
		             ) ; end M loop
		          (setq Y[n] (+ (* y .8) (* y (srandom .4))))
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: mixedRandom")
		       (setq Lambda (gsmRegress.svmTraining X Y maxGen verboseSW kernelID))
		       (if (= myVerboseSW false) (writeln "gsmRegress: N = [" Ns "], M = [" Lambda.M "], Generations = [" Generations  "], SV's = [" Lambda.N "]")) 
               (if (= checkResults true)
                   (begin
                      (setq nlse 0.0)
                      (setq avgY (avg Y))
                      (setq stdY (stdev Y))
		              (loop for n from 0 until N do
		                 (setq y Y[n])
		                 (setq ey (Lambda X[n]))
                         (setq err (abs (- ey y)))
                         (setq pct (/ err stdY)) 
		                 (+= nlse (* err err))
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" ey "] y=[" y "] err=[" err "] err%=[" pct "]"))
		                 ) ; end N loop
                      (setq nlse (/ (sqrt (/ nlse N)) stdY)) 
		              (writeln "gsmRegress: nlse=[" nlse "], avgY=[" avgY "], stdY=[" stdY "]")
		           )) ; end if
          )) ; end Test Case mixedRandom
       ;; Test Case randomSigmoid 
       (if (= Test randomSigmoid:)
           (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: The inputs, X, are restricted to the sigmoid domain.
		       ;; Note2: We support a bias by having X[0] == 1 for all N.
		       ;; Note3: This algorithm seems to work well when N is at least 25 times M.
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 1.0) .5))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do 
		          	 (setq X[n][m] (- (srandom .999999999) 0))
		             (setq y (+ y (* X[n][m] C[m])))
		             ) ; end M loop
		          (setq Y[n] (+ (* y .8) (* y (srandom .4))))
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: randomSigmoid")
		       (setq Lambda (gsmRegress.svmTraining X Y maxGen verboseSW kernelID))
		       (if (= myVerboseSW false) (writeln "gsmRegress: N = [" Ns "], M = [" Lambda.M "], Generations = [" Generations  "], SV's = [" Lambda.N "]")) 
               (if (= checkResults true)
                   (begin
                      (setq nlse 0.0)
                      (setq avgY (avg Y))
                      (setq stdY (stdev Y))
		              (loop for n from 0 until N do
		                 (setq y Y[n])
		                 (setq ey (Lambda X[n]))
                         (setq err (abs (- ey y)))
                         (setq pct (/ err stdY)) 
		                 (+= nlse (* err err))
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" ey "] y=[" y "] err=[" err "] err%=[" pct "]"))
		                 ) ; end N loop
                      (setq nlse (/ (sqrt (/ nlse N)) stdY)) 
		              (writeln "gsmRegress: nlse=[" nlse "], avgY=[" avgY "], stdY=[" stdY "]")
		           )) ; end if
          )) ; end Test Case randomSigmoid
       ;; Test Case cube 
       (if (= Test cube:)
           (begin
		       ;; Create a test polynomial regression where y = -11.2 + C[0]*X[0] - C[1]*(X[1]**3) + C[2]*X[2] - C[3]*(X[3]**3) ...
		       ;; Note: We support a bias by having X[0] == 1 for all N.
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (if (isOdd m)
			             (setq y (+ y (* X[n][m] C[m])))
			             (setq y (+ y (* X[n][m] X[n][m] X[n][m] C[m])))
		                 ) ; end if
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: cube")
		       (setq Lambda (gsmRegress.svmTraining X Y maxGen verboseSW kernelID))
		       (if (= myVerboseSW false) (writeln "gsmRegress: N = [" Ns "], M = [" Lambda.M "], Generations = [" Generations  "], SV's = [" Lambda.N "]")) 
               (if (= checkResults true)
                   (begin
                      (setq nlse 0.0)
                      (setq avgY (avg Y))
                      (setq stdY (stdev Y))
		              (loop for n from 0 until N do
		                 (setq y Y[n])
		                 (setq ey (Lambda X[n]))
                         (setq err (abs (- ey y)))
                         (setq pct (/ err stdY)) 
		                 (+= nlse (* err err))
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" ey "] y=[" y "] err=[" err "] err%=[" pct "]"))
		                 ) ; end N loop
                      (setq nlse (/ (sqrt (/ nlse N)) stdY)) 
		              (writeln "gsmRegress: nlse=[" nlse "], avgY=[" avgY "], stdY=[" stdY "]")
		           )) ; end if
          )) ; end Test Case cube
       ;; Test Case square 
       (if (= Test square:)
           (begin
		       ;; Create a test polynomial regression where y = -11.2 + C[0]*X[0] - C[1]*(X[1]**2) + C[2]*X[2] - C[3]*(X[3]**2) ...
		       ;; Note: We support a bias by having X[0] == 1 for all N.
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (if (isOdd m)
			             (setq y (+ y (* X[n][m] C[m])))
			             (setq y (+ y (* X[n][m] X[n][m] C[m])))
		                 ) ; end if
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: square")
		       (setq Lambda (gsmRegress.svmTraining X Y maxGen verboseSW kernelID))
		       (if (= myVerboseSW false) (writeln "gsmRegress: N = [" Ns "], M = [" Lambda.M "], Generations = [" Generations  "], SV's = [" Lambda.N "]")) 
               (if (= checkResults true)
                   (begin
                      (setq nlse 0.0)
                      (setq avgY (avg Y))
                      (setq stdY (stdev Y))
		              (loop for n from 0 until N do
		                 (setq y Y[n])
		                 (setq ey (Lambda X[n]))
                         (setq err (abs (- ey y)))
                         (setq pct (/ err stdY)) 
		                 (+= nlse (* err err))
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" ey "] y=[" y "] err=[" err "] err%=[" pct "]"))
		                 ) ; end N loop
                      (setq nlse (/ (sqrt (/ nlse N)) stdY)) 
		              (writeln "gsmRegress: nlse=[" nlse "], avgY=[" avgY "], stdY=[" stdY "]")
		           )) ; end if
          )) ; end Test Case square
       ;; Test Case tan 
       (if (= Test tan:)
           (begin
		       ;; Create a test polynomial regression where y = -11.2 + C[0]*X[0] - C[1]*(X[1]**2) + C[2]*X[2] - C[3]*(X[3]**2) ...
		       ;; Note: We support a bias by having X[0] == 1 for all N.
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (setq y (+ y (* (tan X[n][m]) C[m])))
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: tan")
		       (setq Lambda (gsmRegress.svmTraining X Y maxGen verboseSW kernelID))
		       (if (= myVerboseSW false) (writeln "gsmRegress: N = [" Ns "], M = [" Lambda.M "], Generations = [" Generations  "], SV's = [" Lambda.N "]")) 
               (if (= checkResults true)
                   (begin
                      (setq nlse 0.0)
                      (setq avgY (avg Y))
                      (setq stdY (stdev Y))
		              (loop for n from 0 until N do
		                 (setq y Y[n])
		                 (setq ey (Lambda X[n]))
                         (setq err (abs (- ey y)))
                         (setq pct (/ err stdY)) 
		                 (+= nlse (* err err))
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" ey "] y=[" y "] err=[" err "] err%=[" pct "]"))
		                 ) ; end N loop
                      (setq nlse (/ (sqrt (/ nlse N)) stdY)) 
		              (writeln "gsmRegress: nlse=[" nlse "], avgY=[" avgY "], stdY=[" stdY "]")
		           )) ; end if
          )) ; end Test Case tan
       ;; Test Case log 
       (if (= Test log:)
           (begin
		       ;; Create a test polynomial regression where y = -11.2 + C[0]*X[0] - C[1]*(X[1]**2) + C[2]*X[2] - C[3]*(X[3]**2) ...
		       ;; Note: We support a bias by having X[0] == 1 for all N.
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (setq y (+ y (* (log (+ 1.0 (abs X[n][m]))) C[m])))
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: log")
		       (setq Lambda (gsmRegress.svmTraining X Y maxGen verboseSW kernelID))
		       (if (= myVerboseSW false) (writeln "gsmRegress: N = [" Ns "], M = [" Lambda.M "], Generations = [" Generations  "], SV's = [" Lambda.N "]")) 
               (if (= checkResults true)
                   (begin
                      (setq nlse 0.0)
                      (setq avgY (avg Y))
                      (setq stdY (stdev Y))
		              (loop for n from 0 until N do
		                 (setq y Y[n])
		                 (setq ey (Lambda X[n]))
                         (setq err (abs (- ey y)))
                         (setq pct (/ err stdY)) 
		                 (+= nlse (* err err))
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" ey "] y=[" y "] err=[" err "] err%=[" pct "]"))
		                 ) ; end N loop
                      (setq nlse (/ (sqrt (/ nlse N)) stdY)) 
		              (writeln "gsmRegress: nlse=[" nlse "], avgY=[" avgY "], stdY=[" stdY "]")
		           )) ; end if
          )) ; end Test Case log
       (writeln "gsmRegress.selfTest: completed in [" (/ (setq endTimeT (getTickCount startTimeT)) 60.0) "] minutes.")       
       Lambda) ; end selfTest
    ;; *****************
    ;; Begin main logic.
    ;; ***************** 
    vars:(Lambda svmLambda (maxGen 1) (verboseSW false) kernelID)

    ;; Create and train a new support vector machine.
    (if (>= (argCount) 3) (setq maxGen (argFetch 2)))
    (if (>= (argCount) 4) (setq verboseSW (argFetch 3)))
    (if (>= (argCount) 5) (setq kernelID (argFetch 4)))
    (setq svmLambda (new (myself)))
    (setq svmLambda.mySVMParent svmLambda)
    (setq Lambda (svmLambda.svmTraining x y maxGen verboseSW kernelID))
    Lambda) ; end gsmRegress














;;**EXPORTKEY**:smoRegress
(defun smoRegress(x y kernelID ...)
;; *******************************************************************
;; name:     smoRegress
;; 
;; summary:  Trains a support vector machine SMO regression Lambda,
;;           and returns the trained Lambda. 
;;
;; Parms:    x:         The N by M vector array representing the original observations
;;                      in the form of:    x x ... x
;;                                         x x ... x
;;                                             ... 
;;                                         x x ... x
;;           y   		The N vector of dependent variables.
;;           kernelID   The kernel identifier to be used for support vector machine training.
;;                        #void			A linear dot product kernel.  
;;                        "linear"		A linear dot product kernel.  
;;                        "square"		A squared linear dot product kernel.  
;;                        "cube"		A cubed linear dot product kernel.  
;;                        Lambda		    Any user supplied modified dot product kernel.  
;;                        function		Any user supplied modified dot product kernel.  
;;           tollerance (Optional) The regression error tollerance as a percent of Y (defaults to 0.1).
;;           maxErr  	(Optional) The maximum error before halting training as a percent of Y (defaults to .05).
;;           maxGen  	(Optional) The maximum generation count before halting training (defaults to 1).
;;           maxSVSize  (Optional) The Gaussian sample size (maximum number of support vectors to use during Gaussian initialization -- defaults to 100).
;;           verboseSW  (Optional) True iff we are to set verbose mode on (defaults to false).
;; Return:   result: 	The result structure containing the trained SMO regression model results, where
;;		  	    			result.Error		Contains the final tollerant error (expressed as a percent of each target value)
;;		  	    			result.Generations	Contains the number of generations used during training
;;							result.Weights 		Contains the weight number vector after training
;;							result.Ey 			Contains the estimate errors for each training example
;;		        			result.Py 			Contains the estimate errors for each training example (expressed as a percent of each target value)
;;
;;			 Note1: Support Vector Machine regression can be highly accurate
;;				    when solving non-linear regression problems. However, the
;;                  accuracy varies from excellent to poor depending upon
;;                  the ratio of: the chosen Gaussian Sample Size (maxSVSize);
;;                  the number of regression variables (M); and the THEORETICAL
;;                  number of variables created by the kernel function to
;;                  make the non-linear problem linearly solvable. A simplified
;;                  example would be as follows.
;;
;;					Solving a quadratic regression problem with variableCount == 3,
;;                  y = sum{m from 0 until variableCount}(Cm*Xm*Xm), and 
;;                  a kernel function of vectorSquareInnerProduct, is very
;;                  accurate with a Gaussian Sample Size (maxSVSize) of 10.
;;                  However, if the variableCount is increased to 10, then the
;;                  accuracy completely breaks down and is not restored until
;;                  the Gaussian Sample Size is increased to around 100. An
;;                  explanation is as follows.
;;
;;					In order to make the quadratic regression linearly tractable,
;;                  the vectorSquareInnerProduct performs an on-the-fly squaring
;;                  of each training point vector. Thus, with a training point
;;                  vector of size three, the vectorSquareInnerProduct creates
;;                  the following on-the-fly THEORETICAL new training point:
;;					kernel(X1,X2,X3) => (X1,X2,X3,X1*X1,X2*X2,X3*X3,X1*X2,X1*X3,X2*X3).
;;                  Clearly the problem is now linearly solvable because the squared
;;                  variables are now terms in the THEORETICAL on-the-fly linear regression
;;                  created by the kernel. NOTICE however, that the THEORETICAL linear
;;                  regression, created on-the-fly by the kernel function, has nine variables
;;                  not three variables as in the original quadratic problem. Unless the
;;                  number of training points is greater than nine, and the Gaussian 
;;                  sample size is greater than nine, the on-the-fly linear regression
;;                  will not have enough data to get accurate results.
;; 
;;           Note2: See Cristianini, "Support Vector Machines", page 169.
;; *******************************************************************
    pvars:(;; Public variables
           Number:ETollerance           ;; The current forgiving absolute error tollerance for the SMO regression model (in percent of target). 
           NumVector:Dy                 ;; The cached absolute delta support vector output while training the SMO regression model. 
           NumVector:Ey                 ;; The cached support vector output while training the SMO regression model. 
           Number:Error                 ;; The current absolute error for the SMO regression model (in percent of target). 
           Number:ErrorMax           	;; The maximum absolute error for the SMO regression model (in percent of target). 
           Number:Generations           ;; The number of training cycles used to train the current SMO regression model. 
           Integer:GenerationMax        ;; The maximum number of training cycles before training is halted at any error rate. 
           Integer:GenerationMaxN       ;; The maximum number of training cycles, times the number of target points, before training is halted at any error rate. 
           NumVector:K1		 			;; The N vector of cached kernel values for kernel(X[N1],X[n]) used during training.
           NumVector:K2		 			;; The N vector of cached kernel values for kernel(X[N2],X[n]) used during training.
           Number:K11      				;; Cached value of kerne(N1,N1).
	       Number:K12      				;; Cached value of kerne(N1,N2).
           Number:K22      				;; Cached value of kerne(N2,N2).
           kernel   		       		;; The current support vector machine kernel (user specified).
           (myVerboseSW false)			;; True iff we are to display progress on the console. 
           (myCLearningSW true)		    ;; False iff we are to use the builtin SVM C Function for our learning. 
           (Integer:mySVSize 100)		;; Size of the seed support vector group during initialization. 
           Integer:M					;; The number of elements in each training point (independent variables). 
           Integer:N					;; The number of target points (independent variables). 
           Integer:N1					;; The first training point to improve (selected by heurism). 
           Integer:N2					;; The second training point to improve (selected by heurism). 
           Py                      		;; The cached absolute delta support vector output while training the SMO regression model (in percent of target). 
           Integer:worstN2				;; The training point with the worst percentage error. 
           NumVector:W                  ;; The best guess weight coefficient N vector for the SMO regression model.
           X                       		;; The N X M vector array of training points (independent variables). 
           NumVector:Y                  ;; The N vector of target points (dependent variables). 
           ;; Public child methods
           clear			       		;; Clear the current support vector machine.
           computeDeltaError	   		;; Return the regression error for the currently proposed changes in the SMO coefficients.
           computeError	           		;; Return the regression error for the the SMO coefficients.
           computeKKMatrixForN1		    ;; Compute the Nx2 matrix of cached kernel values for kernel(X[N1],X[n]) and kernel(X[N2],X[n]).
           computeKKMatrixForN2   		;; Compute the Nx2 matrix of cached kernel values for kernel(X[N1],X[n]) and kernel(X[N2],X[n]).
           examineExample               ;; Train the svm SMO regression model on the specified example.
           initFromGaussian             ;; Initialize the svm SMO regression model, before training, with a guassian estimate of the weights.
           kernel   		       		;; Return the svm kernel output for the specified two input points.
	       multipleRegression           ;; Performs a Gaussian multiple regression on the N x M+1 matrix
           svmLambda			       		;; Return an Lambda ready to compute the svm output for specified input points.
           svmOutput		       		;; Return the svm output for the specified input point.
           svmTraining		       		;; Train the svm machine on the specified inputs and model.
           takeStep		                ;; Incrementally train the SMO model on the two training points.
           ;; Private variables
           ;; Private child methods
           ;; Private maintenance child methods
           selfTest                		;; The self test method for this Lambda. 
           ) ;; end of persistent variables
    vars:(m n p rankX ey sum oldError tollerance maxErr result)
    ;; ***************************
    ;; Define Public Child Lambdas.
    ;; ***************************
    ;; Clear the current support vector machine.
    (defun clear()
       (setq kernel (lambda(k) k))
       (setq W #void)
       (setq Ey #void)
       (setq Dy #void)
       (setq Py #void)
       (setq Error 0.0)
       (setq M 0)
       (setq N 0)
       (setq X #void)
       true) ; end clear
    ;; Return the regression error for the currently proposed changes in the SMO coefficients.
    ;; Note1: The regression error is computed as a percent of the target.
    ;; Note2: Any regression error within the tollerance limit, ETollerance, is treated as NO error.
    (defun computeDeltaError(Number:a1 Number:a2)
       regs:(Number:ra1 Number:ra2)
       regs:(Integer:m Integer:n Integer:RN1 Integer:RN2 Integer:RN)
       regs:(NumPointer:pDy NumPointer:pPy NumPointer:pEy NumPointer:pY)
       regs:(NumPointer:pW NumPointer:pK1 NumPointer:pK2)
       regs:(Number:etol Number:Yn)
       regs:(Number:ey Number:dy Number:pct)
       regs:((Integer:rworstN2 0) (Number:err 0.0) (Number:RZero 0.0))
	   ;; Recompute the SMO model error with old bias.
	   (setq Dy (new Vector: Number: N))
	   (setq Py (new Vector: Number: N))
	   ;; Initialize to registers for speed.
       (setq ra1 a1)
       (setq ra2 a2)
       (setq pDy Dy)
       (setq pPy Py)
       (setq pY Y)
       (setq pW W)
       (setq pEy Ey)
       (setq pK1 K1)
       (setq pK2 K2)
	   (setq RN1 N1)
	   (setq RN2 N2)
	   (setq RN N)
	   (setq etol ETollerance)
	   ;; Run main loop in registers for extra speed.
       ;; Note: This is a mission critical Lambda and MUST run extra fast.
       (vmregRunInHardware start:)
	   (loop for n  from 0 until RN do
	      (setq dy (* (- ra1 pW[RN1]) pK1[n]))
	      (setq dy (+ dy (* (- ra2 pW[RN2]) pK2[n])))
	      (setq pDy[n] (setq ey (+ pEy[n] dy)))
          (setq Yn pY[n])
          (setq pct (- pY[n] ey))
          (if (<> Yn RZero) then (/= pct Yn))
          (setq pct (|Gv:abs| pct))
	      (setq pPy[n] pct)
	      (setq dy pPy[rworstN2])
	      (if (> pct dy) then (setq rworstN2 n))
          (setq pct (- pct etol))
          (setq pct (|Gv:abs| pct))
	      (+= err pct)
	      ) ; end error loop
       (vmregRunInHardware stop:)
       (/= err RN)
       (setq worstN2 rworstN2) 
       err) ; end computeDeltaError
    ;; Return the regression error for the currently trained SMO model.
    ;; Note1: The regression error is computed as a percent of the target.
    ;; Note2: Any regression error within the tollerance limit, ETollerance, is treated as NO error.
    (defun computeError(recalcSW)
       regs:(m n RN)
       regs:(Number:etol Number:pct Number:ey Number:dy Number:Yn)
       regs:((Integer:rworstN2 0) (Number:err 0.0) (Number:RZero 0.0))
       regs:(NumPointer:pEy NumPointer:pY NumPointer:pPy)
       ;; Initialize (if necessary)
       (if (= Ey #void) then (setq Ey (new Vector: Number: N)))
	   (if (= Py #void) then (setq Py (new Vector: Number: N)))
	   (setq etol ETollerance)
	   (setq RN N)
       ;; We recompute the original error if requested
       (if (= recalcSW true) then (loop for n from 0 until RN do (setq Ey[n] (svmOutput X[n]))))
       ;; Initialize to registers for extra speed
       (setq pY Y)
       (setq pEy Ey)
       (setq pPy Py)
       ;; We recompute the average absolute percent error
       ;; Note: This is a mission critical Lambda and MUST run extra fast.
       (vmregRunInHardware start:)
	   (loop for n from 0 until RN do
          (setq ey pEy[n]) 
          (setq Yn pY[n]) 
          (setq pct (- Yn ey))
	      (if (<> Yn RZero) then (/= pct Yn))
          (setq pct (|Gv:abs| pct))
	      (setq pPy[n] pct)
	      (setq dy pPy[rworstN2])
	      (if (> pct dy) then (setq rworstN2 n))
          (-= pct etol)
          (setq pct (|Gv:abs| pct))
	      (+= err pct)
	      ) ; end error loop
       (vmregRunInHardware stop:)
       (setq worstN2 rworstN2) 
	   (/= err RN)
       err) ; end computeError
    ;; Compute the 2xN matrix of cached kernel values for kernel(X[N1],X[n]) and kernel(X[N2],X[n]).
    (defun computeKKMatrixForN1()
       regs:(n)
	   (loop for n from 0 until N do
	      (setq K1[n] (kernel X[N1] X[n]))
	      ) ; end error loop
       true) ; end computeKKMatrixForN1
    ;; Compute the 2xN matrix of cached kernel values for kernel(X[N1],X[n]) and kernel(X[N2],X[n]).
    (defun computeKKMatrixForN2()
       regs:(n)
	   (loop for n  from 0 until N do
	      (setq K2[n] (kernel X[N2] X[n]))
	      ) ; end error loop
       true) ; end computeKKMatrixForN2
    ;; Train the svm SMO regression model on the specified example.
    (defun examineExample(i)
       regs:(m n) 
       (setq N2 i)
       (computeKKMatrixForN2)
       (setq K22 K2[N2])
       ;; Loop through each training example beginning at a random point.
       (setq m (integer (random (* .999999999999 N))))
       (loop for n from 0 until N do
	      ;; Select the second training point at random.
	      (setq N1 (modi (addi n m) N))
	      ;; Improve only training points with an unacceptable error.
	      (if (and (> (- Py[N1] ETollerance) ErrorMax) (<> N1 N2)) (if (takeStep) (return 1)))
          ) ; end loop
       0) ; end examineExample
    ;; Initialize the svm SMO regression model, before training, with a guassian estimate of the weights.
    (defun initFromGaussian()
       regs:(Integer:rm 
             Integer:k 
             Integer:m 
             Integer:n 
             Integer:sn
             Integer:NN                  ;; Number of original training examples.
             Integer:SN                  ;; Number of seed support vectors to use in Gaussian initialization.
             Integer:stepInc             ;; Example selection step size for use in support vector selection.
             ) ; end register variables
       regs:(IntPointer:pSV IntPointer:pSortedY)
       vars:(NumVector:CS                ;; The Gaussian regression coefficients for the support vectors.
             NumMatrix:RM 				 ;; The regression matrix regressing the support vectors against all of the training points in X.
             IntVector:sortedY 		     ;; The Vector of Y indices in ascending sorted order.
             IntVector:SV 				 ;; The Vector of support vector indices to use in Gaussian initialization.
             ) ; end temporary variables
       ;; Determine the initial number of support vectors.
       ;; Note: The initial number of support vectors is
       ;;       heuristically limited to the size supplied
       ;;       by the user.
       (setq sortedY (sort Y < true))
       (setq pSortedY sortedY)
       (setq NN N)
       (if (> NN mySVSize) (setq SN mySVSize) (setq SN NN))
       (setq SV (new Vector: Integer: SN))
       (setq stepInc (/ NN SN))
       (if (< stepInc 1) then (setq stepInc 1))
       (setq n 0)
       (setq pSV SV)
       (vmregRunInHardware start:)
       (loop for sn from 0 until SN do
		  (setq pSV[sn] pSortedY[n])
          (+= n stepInc)
          ) ; end loop 
       (vmregRunInHardware stop:)
       ;; Construct the support vectors regression matrix.
       ;; Note: We attempt to regress a linear model of the support vectors
       ;;       against all of the training points in X. The Regression
       ;;       Matrix is of the following form:
       ;;          kernel(X[0],X[SV[0]]) ... kernel(X[0],X[SV[SN]]) Y[0]
       ;;          kernel(X[1],X[SV[0]]) ... kernel(X[1],X[SV[SN]]) Y[1]
       ;;                  ...           ...              ...
       ;;          kernel(X[N],X[SV[0]]) ... kernel(X[N],X[SV[SN]]) Y[N]
       (setq W #void)
       (setq RM (new Matrix: number: 2 N (addi SN 1)))
       (setq rm -1)
       (loop for n from 0 until N do
           (loop for m from 0 until SN do
              (setq k SV[m])
              (setq RM[(++ rm)] (kernel X[n] X[k]))
              ) ; end SN loop
           (setq RM[(++ rm)] Y[n])
           ) ; end N loop
       ;; Solve for the Gaussian coefficients of the support vectors.
       ;; Note: We perform a Gaussian linear regression on the Regression
       ;;       Matrix, returning an SN Vector of coefficients, which we
       ;;       set as the weights, in the SVM  model, of each of the 
       ;;       support vectors respectively. All other non-support-vector
       ;;       weights, in the initial SVM model, are set to zero.
       (setq CS (multipleRegression RM))
       (setq W (new Vector: Number: N))
       (loop for m from 0 until SN do
          (setq k SV[m])
          (setq W[k] CS[m])
          ) ; end loop
       ;; Complete the SVM model initialization.
       (setq Error (computeError true))
       (if myVerboseSW (writeln "smoRegress: Gaussian initialization complete.")) 
       true) ; end initFromGaussian
	;; summary:  Performs a Gaussian multiple regression on the N by M+1 matrix
	;; Parms:    MXY:     The N by M+1 matrix representing the original observations
	;;                    in the form of:    x x ... x y
	;;                                       x x ... x y
	;;                                           ... 
	;;                                       x x ... x y
	;; Return:   C:       The M coefficient vector for the regression.
	;; Note1:    Average error statistics are computed as a percent of the target (dependent variable).
	;; Note2:    See Sedgewick[2] chap 37.
	(defun multipleRegression(NumMatrix:MXY)
	    vars:(NumMatrix:Xt NumVector:C)
	    ;; Perform a least squares regression on all the factors.
	    (setq Xt (|Gv:makeGaussianMatrix| MXY))
	    (setq Xt (|Gv:matrixGaussianEliminate| Xt true))
	    (setq C (|Gv:matrixGaussianSubstitute| Xt))
	    ;; Return the coefficient vector for the regression.
	    C) ; end multipleRegression
    ; Return an Lambda ready to compute the svm output for specified input points.
    (defun svmLambda()
       regs:(n m)
       vars:(Lambda)
       (setq Lambda (eval "(lambda(NumVector:x) pvars:(Strategy W X N M kernel ETollerance Error) regs:(Integer:n Integer:m Number:ey Number:ky) (setq ey 0.0) (loop for n from 0 until N do (if (<> W[n] 0.0) (setq ky (* W[n] (kernel x X[n]))) (setq ky 0.0)) (setq ey (+ ey ky)) ) ey)"))
       (setq Lambda.Strategy svmSMO:)
       (setq Lambda.W (new Vector: Number:))
       (setq Lambda.X (new Vector: object:))
       ;; Drop all but the support vectors for the trained model.
       (setq m 0)
       (loop for n from 0 until N do 
          (if (<> W[n] 0.0)
              (begin
                 (setq Lambda.W[m] W[n])
                 (setq Lambda.X[m] X[n])
                 (++ m)
              )) ; end if
          ) ; end loop
       (setq Lambda.N m)
       (setq Lambda.M M)
       (setq Lambda.kernel kernel)
       (setq Lambda.ETollerance ETollerance)
       (setq Lambda.Error Error)
       Lambda) ; end svmLambda
    ;; Return the svm output for the specified input point.
    ;; Note1: x must be a vector of length M
    ;; Note2: The SMO model says: y ~= sum{n=0-N,W[n]*Y[n]*Kernel(X[n],x)}
    (defun svmOutput(NumVector:x)
       regs:(Integer:n Integer:m Number:ey Number:ky)
       (setq ey 0.0)
       (loop for n from 0 until N do
          (if (<> W[n] 0.0) (setq ky (* W[n] (kernel x X[n]))) (setq ky 0.0))
          (setq ey (+ ey ky))
          ) ; end SMO model loop
       ey) ; end svmOutput
    ;; Train the svm machine on the specified inputs and model.
	;; Parms:    x:         The N by M vector array representing the original observations
	;;                      in the form of:    x x ... x
	;;                                         x x ... x
	;;                                             ... 
	;;                                         x x ... x
	;;           y   		The N vector of dependent variables.
	;;           kernelID   The kernel identifier to be used for support vector machine training.
	;;                        #void			A linear dot product kernel.  
	;;                        "linear"		A linear dot product kernel.  
	;;                        "square"		A squared linear dot product kernel.  
	;;                        "cube"		A cubed linear dot product kernel.  
	;;                        Lambda		    Any user supplied modified dot product kernel.  
	;;                        function		Any user supplied modified dot product kernel.  
	;;           tollerance (Optional) The regression error tollerance as a percent of Y (defaults to 0.1).
	;;           maxErr  	(Optional) The maximum error before halting training as a percent of Y (defaults to .05).
	;;           maxGen  	(Optional) The maximum generation count before halting training (defaults to 1).
	;; Return:   result: 	The result structure containing the trained SMO regression model results, where
	;;		  	    			result.Error		Contains the final tollerant error (expressed as a percent of each target value)
	;;		  	    			result.Generations	Contains the number of generations used during training
	;;							result.Weights 		Contains the weight number vector after training
	;;							result.Ey 			Contains the estimate errors for each training example
	;;		        			result.Py 			Contains the estimate errors for each training example (expressed as a percent of each target value)
    (defun svmTraining(x y kernelID ...)
        regs:(m n)
        vars:(examineAll numChanged Number:tollerance Number:maxErr result)
	    ;; Clear support vector machine for retraining.
	    (clear)
	    ;; Retrieve any optional arguments and perform setup.
	    (setq tollerance (if (>= (argCount) 4) (argFetch 3) .1))
        (if (= tollerance 0.0) (setq tollerance 1.0e-320))
	    (setq maxErr (if (>= (argCount) 5) (argFetch 4) .05))
        (if (= maxErr 0.0) (setq maxErr .000001))
	    (setq GenerationMax (if (>= (argCount) 6) (integer (argFetch 5)) 1))
	    (if (or (<> (isVector x) true) (<> (isVector x[0]) true)) (error "smoRegress: X argument must be a Vector Array of rank 2"))
	    (setq m (length x[0]))
	    (setq n (length x))
	    (setq GenerationMaxN (* GenerationMax n))
	    (if (or (<> (isVector y) true) (<> (length y) n)) (error "smoRegress: Y argument must be a Vector of length the same as X"))
	    ;; Initialize the user specified support vector machine kernel.
	    (cond
	      ((= kernelID #void) (setq kernel ^vectorInnerProduct))
	      ((= kernelID "binary") (setq kernel ^vectorBinaryInnerProduct))
	      ((= kernelID "bipolar") (setq kernel ^vectorBipolarInnerProduct))
	      ((= kernelID "cube") (setq kernel ^vectorCubeInnerProduct))
	      ((= kernelID "exp") (setq kernel ^vectorExpInnerProduct))
	      ((= kernelID "cosine") (setq kernel ^vectorCosineInnerProduct))
	      ((= kernelID "linear") (setq kernel ^vectorInnerProduct))
	      ((= kernelID "log") (setq kernel ^vectorLogInnerProduct))
	      ((= kernelID "quart") (setq kernel ^vectorQuartInnerProduct))
	      ((= kernelID "quint") (setq kernel ^vectorQuintInnerProduct))
	      ((= kernelID "sine") (setq kernel ^vectorSineInnerProduct))
	      ((= kernelID "sigmoid") (setq kernel ^vectorSigmoidInnerProduct))
	      ((= kernelID "square") (setq kernel ^vectorSquareInnerProduct))
	      ((= kernelID "tan") (setq kernel ^vectorTanInnerProduct))
	      ((= kernelID "tanh") (setq kernel ^vectorTanhInnerProduct))
	      (else (setq kernel kernelID))
	      ) ; end cond 
	    ;; Save the initial untrained svm SMO model.
	    (setq ErrorMax maxErr)
	    (setq X x)
	    (setq Y y)
	    (setq M m)
	    (setq N n)
	    (setq W (new Vector: Number: N))
	    (setq Ey (new Vector: Number: N))
		;; Generate the initial absolute error scores as a percent of target.
	    (setq Py (new Vector: Number: N 1.0))
		(setq ETollerance tollerance)
		(setq Error (- 1.0 ETollerance))
	    ;; Use the built-in SMO C Function for faster training.
	    (if (= myCLearningSW true)
	        (begin	      
		  	    (setq result (|Gv:svmRegression| X Y kernel ETollerance maxErr GenerationMax mySVSize myVerboseSW))
		  	    (setq Error result.Error)
		  	    (setq Generations (* result.Generations N))
		  	    (setq W result.Weights)
		        (setq Ey result.Ey)
		        (setq Py result.Py)
		        (goto HaltTraining:)
            )) ; end if
	    ;; Run multiple generations taking the first one to achieve success.
	    ;; Note: If no generation achieves success, we take the best so far.
	    (setq Dy (new Vector: Number: N))
        (setq K1 (new Vector: Number: N))
        (setq K2 (new Vector: Number: N))
        (if (> mySVSize 0) (initFromGaussian)) 
	    (setq Generations 1)
	    (setq examineAll true)
	    (setq numChanged 0)
	    ;; Train the next generation.
	    (while (and (>= Error ErrorMax) (< Generations GenerationMaxN)) do
	       (++ Generations)
	       (setq numChanged 0) 
	       ;; Loop through each training example looking for weights of zero.
	       ;; Note: We improve only the training point with the largest error,
	       ;;       or we improve any training points which currently have
	       ;;       no contribution to the model estimate (weight == zero).
	       (loop for n from 0 until N do
	          (if (= W[n] 0) 
	              (begin
				     (setq N2 n)
				     (computeKKMatrixForN2)
				     (setq K22 K2[N2])
				     ;; Select the second training example at a random point.
				     (setq N1 N2)(while (<> N1 N2) do (setq N1 (integer (random (* .999999999999 N)))))
				     ;; Improve only training points with no contribution to the model estimate (weight == zero).
					 (takeStep)
	       	  		 (if (or (<= Error ErrorMax) (>= Generations GenerationMaxN)) (goto HaltTraining:))
	              )) ; end if
	          ) ; end loop
	       ;; Improve the training point with the worst error.
	       (setq N2 worstN2)
	       (setq numChanged (+ numChanged (examineExample N2)))
	       (if (or (<= Error ErrorMax) (>= Generations GenerationMaxN)) (goto HaltTraining:))
	       ;; Loop through each training example looking to improve the error.
	       ;; Note: We improve only the training points with unacceptable errors,
	       ;;       or we improve any training point depending on the setting
	       ;;       of the examineAll switch.
	       (loop for n from 0 until N do
	          (if (= examineAll true) 
	              (setq numChanged (+ numChanged (examineExample n)))
	              (if (> (- Py[n] ETollerance) ErrorMax) (setq numChanged (+ numChanged (examineExample n))))
	              ) ; end if
	       	  (if (or (<= Error ErrorMax) (>= Generations GenerationMaxN)) (goto HaltTraining:))
	          ) ; end loop
	       (if (= examineAll true) 
	           (setq examineAll false)
	           (if (= numChanged 0) (setq examineAll true))
	           ) ; end if
	       ) ; end while
	    ;; Generate the final absolute error score after training.
	    HaltTraining::
	    (if (<> myCLearningSW true) (setq Error (computeError true)))
	    (if myVerboseSW (writeln "smoRegress: Final Model Generations = [" (integer (/ Generations N))  "], ETollerance = [" ETollerance  "], Error = [" Error "]"))       
	    (setq result (new Structure: Error: Error Generations: (integer (/ Generations N)) Weights: W Ey: Ey Py: Py))
        result) ; end svmTraining
    ;; Incrementally train the SMO model on the two training points.
    ;; Note1: The regression error is computed as a percent of the target.
    ;; Note2: Any regression error within the tollerance limit, ETollerance, is treated as NO error.
    (defun takeStep()
       regs:(Integer:n Integer:m)  
       vars:((Integer:retryCount 0) (Integer:maxRetries 4))
       vars:(Number:high Number:highError Number:mid Number:midError Number:low Number:lowError)
       vars:(Number:W1 Number:W2 Number:D1 Number:D2 Number:E1 Number:E2 Number:err Number:k)  
       vars:(Number:bestError Number:bestW1 Number:bestW2 Number:bestM Number:multiplier)
	   (++ Generations)
       (computeKKMatrixForN1)
       (setq K11 K1[N1])
       (setq K12 K2[N2])
       (setq k (- (+ K11 K22) (* 2 K12)))
       Retry::
       (setq E1 (- Ey[N1] Y[N1]))
       (setq E2 (- Ey[N2] Y[N2]))
       ;; Compute new guesses for both W1 and W2 coefficient deltas.
       (if (<> k 0.0) (setq D1 (/ (- E2 E1) k)) (setq D1 (- E2 E1)))
       (if (<> k 0.0) (setq D2 (/ (- E1 E2) k)) (setq D2 (- E1 E2)))
       (setq bestError Error)
       (setq high 2.0)
       (setq mid 0.0)
       (setq low -2.0)
       
	   ;; Attempt to improve the SVM model regression error at the high point.
       ;; Note: By computing the proposed changes to the model, from the new
	   ;;       two point weight guesses, in a seperate delta estimate area,
       ;;		we can easily throw these changes away if they do not lead 
	   ;;		to an overall improvement in the total model error.
	   (setq multiplier high)
	   (setq W2 (+ W[N2] (* multiplier D2)))
	   (setq W1 (+ W[N1] (* multiplier D1)))
       ;; Compute new delta regression error for the two points.
       (setq err (computeDeltaError W1 W2))
	   (setq highError err)
       (if (< err bestError) 
           (begin 
           	  (setq bestError err)
           	  (setq bestM multiplier)
           	  (setq bestW1 W1)
           	  (setq bestW2 W2)
           )) ; end inner if
       
	   ;; Attempt to improve the SVM model regression error at the low point.
       ;; Note: By computing the proposed changes to the model, from the new
	   ;;       two point weight guesses, in a seperate delta estimate area,
       ;;		we can easily throw these changes away if they do not lead 
	   ;;		to an overall improvement in the total model error.
	   (setq multiplier low)
	   (setq W2 (+ W[N2] (* multiplier D2)))
	   (setq W1 (+ W[N1] (* multiplier D1)))
       ;; Compute new delta regression error for the two points.
       (setq err (computeDeltaError W1 W2))
	   (setq lowError err)
       (if (< err bestError) 
           (begin 
           	  (setq bestError err)
           	  (setq bestM multiplier)
           	  (setq bestW1 W1)
           	  (setq bestW2 W2)
           )) ; end inner if
       
	   ;; Attempt to improve the SVM model regression error at the mid point.
	   ;; Note: Since the initial mid point is the result of no change at all,
	   ;;		 we can use the current SVM model Error as the result of the
	   ;;		 mid point improvement attempt.
	   (setq midError Error)
       
	   ;; Perform a binary search of iterative attempts at improving the SVM model error. 
	   ;; Note1: This heuristic search works well because changes the SVM model produce 
	   ;;        monotonically increasing or decreasing error rates. Therefore experimental 
	   ;;        iteration via binary search is cheaper than any other technique. 
	   ;; Note2: Since we originally initialized the weight vector to all zeros, 
	   ;;        we already know that the starting model estimate for each target 
	   ;;        point is zero. 
	   ;; Note3: This is an important time savings in that we never 
	   ;;        have to compute the full model target estimates completely. 
	   ;;        Throughout the whole algorithm, the model target estimates are 
	   ;;        kept constantly updated with incremental computation only. 
	   ;; Note4: By computing the proposed changes to the model, from the new 
	   ;;		  two point weight guesses, in a seperate delta estimate area, 
	   ;;		  we can easily throw these changes away if they do not lead 
	   ;;		  to an overall improvement in the total model error. 
       (loop for m from 0 until 8 do
           (if (< lowError midError)
               then
               (begin
				  (setq high mid)
				  (setq highError midError)
				  (setq mid (+ low (/ (- high low) 2)))
               ) ; end then
               else
               (begin
				  (setq low mid)
				  (setq lowError midError)
				  (setq mid (+ low (/ (- high low) 2)))
               )) ; end if

		   ;; Attempt to improve the SVM model regression error at the mid point. 
		   ;; Note: By computing the proposed changes to the model, from the new
		   ;;		 two point weight guesses, in a seperate delta estimate area,
		   ;;		 we can easily throw these changes away if they do not lead
		   ;;		 to an overall improvement in the total model error.
           (setq multiplier mid)
	       (setq W2 (+ W[N2] (* multiplier D2)))
	       (setq W1 (+ W[N1] (* multiplier D1)))
	       ;; Compute new delta regression error for the two points.
	       (setq err (computeDeltaError W1 W2))
	       (setq midError err)
	       (if (< err bestError) 
	           (begin 
	           	  (setq bestError err)
	           	  (setq bestM multiplier)
	           	  (setq bestW1 W1)
	           	  (setq bestW2 W2)
	           )) ; end inner if
           ) ; end loop
       ;; Don't update these two points unless there is an improvement.
       (if (< bestError Error)
           (begin
	           (setq W1 bestW1)   
	           (setq W2 bestW2)   
		       (setq err (computeDeltaError W1 W2))
               (if myVerboseSW (writeln "smoRegress: Generations = [" (integer (/ Generations N))  "], L=[" bestM "], N1=[" N1 "], N2=[" N2 "], ETollerance=[" ETollerance "], OldErr=[" Error "], Error=[" err "]")) 
               (setq Error err)
               (setq Ey Dy)
		       (setq W[N1] W1)
		       (setq W[N2] W2)
		       ;; Recompute the average absolute error using the new error tollerance limit.
			   (if (<= err ErrorMax) (setq Error (computeError false)))
			   (if (or (>= Generations GenerationMaxN) (<= Error ErrorMax)) (return true))
		       ;; Perform several repeated retries at improving the error.
		       ;; Note: This heuristic search works well because, even at this point, 
		       ;;       we do not have to recompute any of the kernels. Therefore
		       ;;       experimental iteration here is cheaper than any other technique.
			   (if (< (++ retryCount) maxRetries) (goto Retry:))
               (return true)
           )) ; end if
       ;; Return failure
       false) ; end takeStep
    ;; ****************************************
    ;; Define Private Maintenance Child Lambdas.
    ;; ****************************************
    ;; The self test method for this Lambda.
    (defun selfTest(Test Ms Ns Gs Ss)
       vars:(k m n g G y ey C c X Y Yv avgY avgTopEy topEyCnt
             Lambda kernelID err Net pct 
             startTime endTime startTimeT endTimeT
             (checkResults true)
             (tol 0.0) (errStop 0.0) (Cs 1.0)
             ) ; end temporary variables
       (clear)
       (setq myVerboseSW false)
       (setq startTimeT (getTickCount 0))
       (setq srandom.seed 8192.0)      
       ;; Select the requested test case
       (cond
         ;; Test Case linear 
         ((= Test linear:)
          (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: We support a bias by having X[0] == 1 for all N.
		       ;; Note2: This algorithm seems to work well when N is at least 25 times M.
		       (setq kernelID ^vectorInnerProduct)
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do 
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (setq y (+ y (* X[n][m] C[m])))
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: linear")
		       (smoRegress X Y kernelID tol errStop Gs Ss)
		       (if (= myVerboseSW false) (writeln "smoRegress: N = [" N "], M = [" M "], Generations = [" (integer (/ Generations N))  "], ETollerance=[" ETollerance "], Error=[" Error "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
                      (setq Lambda (svmLambda))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (if (> y avgY) then (begin (++ topEyCnt) (+= avgTopEy Y[n])))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "smoRegress: err=[" err "], avgY=[" avgY "], avgTopEy=[" (if (<> topEyCnt 0) (/ avgTopEy topEyCnt) 0.0) "], avgTopEy%=[" (if (and (<> avgY 0.0) (<> topEyCnt 0)) (text (/ avgTopEy topEyCnt avgY) "##.00%") "NA") "]")
		           )) ; end if
          )) ; end Test Case linear
         ;; Test Case linearSigmoid 
         ((= Test linearSigmoid:)
          (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: The inputs, X, are restricted to the sigmoid domain.
		       ;; Note2: We support a bias by having X[0] == 1 for all N.
		       ;; Note3: This algorithm seems to work well when N is at least 25 times M.
		       (setq kernelID ^vectorInnerProduct)
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 1) .5))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do 
		          	 (setq X[n][m] (- (srandom .999999999) 0))
		             (setq y (+ y (* X[n][m] C[m])))
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: linearSigmoid")
		       (smoRegress X Y kernelID tol errStop Gs Ss)
		       (if (= myVerboseSW false) (writeln "smoRegress: N = [" N "], M = [" M "], Generations = [" (integer (/ Generations N))  "], ETollerance=[" ETollerance "], Error=[" Error "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
                      (setq Lambda (svmLambda))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (if (> y avgY) then (begin (++ topEyCnt) (+= avgTopEy Y[n])))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "smoRegress: err=[" err "], avgY=[" avgY "], avgTopEy=[" (if (<> topEyCnt 0) (/ avgTopEy topEyCnt) 0.0) "], avgTopEy%=[" (if (and (<> avgY 0.0) (<> topEyCnt 0)) (text (/ avgTopEy topEyCnt avgY) "##.00%") "NA") "]")
		           )) ; end if
          )) ; end Test Case linear
         ;; Test Case srandom 
         ((= Test srandom:)
          (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: We support a bias by having X[0] == 1 for all N.
		       ;; Note2: This algorithm seems to work well when N is at least 25 times M.
		       (setq kernelID ^vectorInnerProduct)
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (setq y (+ y (* X[n][m] C[m])))
		             ) ; end M loop
		          (setq Y[n] (+ (* y .8) (* y (srandom .4))))
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: srandom")
		       (smoRegress X Y kernelID tol errStop Gs Ss)
		       (if (= myVerboseSW false) (writeln "smoRegress: N = [" N "], M = [" M "], Generations = [" (integer (/ Generations N))  "], ETollerance=[" ETollerance "], Error=[" Error "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
                      (setq Lambda (svmLambda))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (if (> y avgY) then (begin (++ topEyCnt) (+= avgTopEy Y[n])))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "smoRegress: err=[" err "], avgY=[" avgY "], avgTopEy=[" (if (<> topEyCnt 0) (/ avgTopEy topEyCnt) 0.0) "], avgTopEy%=[" (if (and (<> avgY 0.0) (<> topEyCnt 0)) (text (/ avgTopEy topEyCnt avgY) "##.00%") "NA") "]")
		           )) ; end if
          )) ; end Test Case srandom
         ;; Test Case mixedRandom 
         ((= Test mixedRandom:)
          (begin
		       ;; Create a test polynomial linear model where y = C[0]*X[0] + C[1]*X[1] + C[2]*X[2] ...
		       ;; Create a test polynomial square model where y = C[0]*X[0]*X[0] + C[1]*X[1]*X[1] + C[2]*X[2]*X[2] ...
		       ;; Create a test polynomial sin model where y = C[0]*sin(X[0]) + C[1]*sin(X[1]) + C[2]*sin(X[2]) ...
		       ;; Create a test polynomial log model where y = C[0]*log(abs(X[0])+.000001) + C[1]*log(abs(X[1])+.000001) + C[1]*log(abs(X[2])+.000001) ...
               ;; These four models are mixed together and random noise is added.
		       ;; Note1: We support a bias by having X[0] == 1 for all N.
		       ;; Note2: This algorithm seems to work well when N is at least 25 times M.
		       (setq kernelID ^vectorCubeInnerProduct)
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
                     ;; Mix the four models together
                     (setq k (modi n 4)) 
                     (cond
                        ;; Linear model
                        ((= k 0) (setq y (+ y (* X[n][m] C[m]))))
                        ;; Square model
                        ((= k 1) (setq y (+ y (* X[n][m] X[n][m] C[m]))))
                        ;; Sine model
                        ((= k 2) (setq y (+ y (* (|Gv:sin| X[n][m]) C[m]))))
                        ;; Log model
                        (else (setq y (+ y (* (|Gv:log| (+ .000001 (|Gv:abs| X[n][m]))) C[m]))))
                        ) ; end cond
		             ) ; end M loop
		          (setq Y[n] (+ (* y .8) (* y (srandom .4))))
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: mixedRandom")
		       (smoRegress X Y kernelID tol errStop Gs Ss)
		       (if (= myVerboseSW false) (writeln "smoRegress: N = [" N "], M = [" M "], Generations = [" (integer (/ Generations N))  "], ETollerance=[" ETollerance "], Error=[" Error "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
                      (setq Lambda (svmLambda))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (if (> y avgY) then (begin (++ topEyCnt) (+= avgTopEy Y[n])))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "smoRegress: err=[" err "], avgY=[" avgY "], avgTopEy=[" (if (<> topEyCnt 0) (/ avgTopEy topEyCnt) 0.0) "], avgTopEy%=[" (if (and (and (<> avgY 0.0) (<> topEyCnt 0)) (<> topEyCnt 0)) (text (/ avgTopEy topEyCnt avgY) "##.00%") "NA") "]")
		           )) ; end if
          )) ; end Test Case mixedRandom
         ;; Test Case randomSigmoid 
         ((= Test randomSigmoid:)
          (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: The inputs, X, are restricted to the sigmoid domain.
		       ;; Note2: We support a bias by having X[0] == 1 for all N.
		       ;; Note3: This algorithm seems to work well when N is at least 25 times M.
		       (setq kernelID ^vectorInnerProduct)
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 1) .5))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do 
		          	 (setq X[n][m] (- (srandom .999999999) 0))
		             (setq y (+ y (* X[n][m] C[m])))
		             ) ; end M loop
		          (setq Y[n] (+ (* y .8) (* y (srandom .4))))
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: randomSigmoid")
		       (smoRegress X Y kernelID tol errStop Gs Ss)
		       (if (= myVerboseSW false) (writeln "smoRegress: N = [" N "], M = [" M "], Generations = [" (integer (/ Generations N))  "], ETollerance=[" ETollerance "], Error=[" Error "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
                      (setq Lambda (svmLambda))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (if (> y avgY) then (begin (++ topEyCnt) (+= avgTopEy Y[n])))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "smoRegress: err=[" err "], avgY=[" avgY "], avgTopEy=[" (if (<> topEyCnt 0) (/ avgTopEy topEyCnt) 0.0) "], avgTopEy%=[" (if (and (<> avgY 0.0) (<> topEyCnt 0)) (text (/ avgTopEy topEyCnt avgY) "##.00%") "NA") "]")
		           )) ; end if
          )) ; end Test Case linear
         ;; Test Case square 
         ((= Test square:)
          (begin
		       ;; Create a test polynomial regression where y = -11.2 + C[0]*X[0] - C[1]*(X[1]**2) + C[2]*X[2] - C[3]*(X[3]**2) ...
		       ;; Note: We support a bias by having X[0] == 1 for all N.
		       (setq kernelID ^vectorSquareInnerProduct)
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (if (isOdd m)
			             (setq y (+ y (* X[n][m] C[m])))
			             (setq y (+ y (* X[n][m] X[n][m] C[m])))
		                 ) ; end if
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: square")
		       (smoRegress X Y kernelID tol errStop Gs Ss)
		       (if (= myVerboseSW false) (writeln "smoRegress: N = [" N "], M = [" M "], Generations = [" (integer (/ Generations N))  "], ETollerance=[" ETollerance "], Error=[" Error "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
                      (setq Lambda (svmLambda))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (if (> y avgY) then (begin (++ topEyCnt) (+= avgTopEy Y[n])))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "smoRegress: err=[" err "], avgY=[" avgY "], avgTopEy=[" (if (<> topEyCnt 0) (/ avgTopEy topEyCnt) 0.0) "], avgTopEy%=[" (if (and (<> avgY 0.0) (<> topEyCnt 0)) (text (/ avgTopEy topEyCnt avgY) "##.00%") "NA") "]")
		           )) ; end if
          )) ; end Test Case square
         ;; Test Case linearSquare 
         ((= Test linearSquare:)
          (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: We support a bias by having X[0] == 1 for all N.
		       (setq kernelID ^vectorSquareInnerProduct)
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do 
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (setq y (+ y (* X[n][m] C[m])))
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: linearSquare")
		       (smoRegress X Y kernelID tol errStop Gs Ss)
		       (if (= myVerboseSW false) (writeln "smoRegress: N = [" N "], M = [" M "], Generations = [" (integer (/ Generations N))  "], ETollerance=[" ETollerance "], Error=[" Error "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
                      (setq Lambda (svmLambda))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (if (> y avgY) then (begin (++ topEyCnt) (+= avgTopEy Y[n])))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "smoRegress: err=[" err "], avgY=[" avgY "], avgTopEy=[" (if (<> topEyCnt 0) (/ avgTopEy topEyCnt) 0.0) "], avgTopEy%=[" (if (and (<> avgY 0.0) (<> topEyCnt 0)) (text (/ avgTopEy topEyCnt avgY) "##.00%") "NA") "]")
		           )) ; end if
          )) ; end Test Case square
         ;; Test Case tan 
         ((= Test tan:)
          (begin
		       ;; Create a test polynomial regression where y = -11.2 + C[0]*X[0] - C[1]*(X[1]**2) + C[2]*X[2] - C[3]*(X[3]**2) ...
		       ;; Note: We support a bias by having X[0] == 1 for all N.
		       (setq kernelID ^vectorTanInnerProduct)
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (setq y (+ y (* (tan X[n][m]) C[m])))
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: tan")
		       (smoRegress X Y kernelID tol errStop Gs Ss)
		       (if (= myVerboseSW false) (writeln "smoRegress: N = [" N "], M = [" M "], Generations = [" (integer (/ Generations N))  "], ETollerance=[" ETollerance "], Error=[" Error "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
                      (setq Lambda (svmLambda))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (if (> y avgY) then (begin (++ topEyCnt) (+= avgTopEy Y[n])))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "smoRegress: err=[" err "], avgY=[" avgY "], avgTopEy=[" (if (<> topEyCnt 0) (/ avgTopEy topEyCnt) 0.0) "], avgTopEy%=[" (if (and (<> avgY 0.0) (<> topEyCnt 0)) (text (/ avgTopEy topEyCnt avgY) "##.00%") "NA") "]")
		           )) ; end if
          )) ; end Test Case tan
         ((= Test sine:)
          (begin
		       ;; Create a test polynomial regression where y = -11.2 + C[0]*X[0] - C[1]*(X[1]**2) + C[2]*X[2] - C[3]*(X[3]**2) ...
		       ;; Note: We support a bias by having X[0] == 1 for all N.
		       (setq kernelID |Gv:vectorSineInnerProduct|)
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (setq y (+ y (* (sin X[n][m]) C[m])))
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: sine")
		       (smoRegress X Y kernelID tol errStop Gs Ss)
		       (if (= myVerboseSW false) (writeln "smoRegress: N = [" N "], M = [" M "], Generations = [" (integer (/ Generations N))  "], ETollerance=[" ETollerance "], Error=[" Error "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
                      (setq Lambda (svmLambda))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (if (> y avgY) then (begin (++ topEyCnt) (+= avgTopEy Y[n])))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "smoRegress: err=[" err "], avgY=[" avgY "], avgTopEy=[" (if (<> topEyCnt 0) (/ avgTopEy topEyCnt) 0.0) "], avgTopEy%=[" (if (and (<> avgY 0.0) (<> topEyCnt 0)) (text (/ avgTopEy topEyCnt avgY) "##.00%") "NA") "]")
		           )) ; end if
          )) ; end Test Case sine
         ;; Test Case log 
         ((= Test log:)
          (begin
		       ;; Create a test polynomial regression where y = -11.2 + C[0]*X[0] - C[1]*(X[1]**2) + C[2]*X[2] - C[3]*(X[3]**2) ...
		       ;; Note: We support a bias by having X[0] == 1 for all N.
		       (setq kernelID ^vectorLogInnerProduct)
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (setq y (+ y (* (log (+ 1.0 (abs X[n][m]))) C[m])))
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: log")
		       (smoRegress X Y kernelID tol errStop Gs Ss)
		       (if (= myVerboseSW false) (writeln "smoRegress: N = [" N "], M = [" M "], Generations = [" (integer (/ Generations N))  "], ETollerance=[" ETollerance "], Error=[" Error "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
                      (setq Lambda (svmLambda))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (if (> y avgY) then (begin (++ topEyCnt) (+= avgTopEy Y[n])))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "smoRegress: err=[" err "], avgY=[" avgY "], avgTopEy=[" (if (<> topEyCnt 0) (/ avgTopEy topEyCnt) 0.0) "], avgTopEy%=[" (if (and (<> avgY 0.0) (<> topEyCnt 0)) (text (/ avgTopEy topEyCnt avgY) "##.00%") "NA") "]")
		           )) ; end if
          )) ; end Test Case log
         ) ; end cond
       (writeln "smoRegress.selfTest: completed in [" (/ (setq endTimeT (getTickCount startTimeT)) 60.0) "] minutes.")       
       Lambda) ; end selfTest
    ;; *****************
    ;; Begin main logic.
    ;; ***************** 
    ;; Retrieve any optional arguments.
    (setq tollerance (if (>= (argCount) 4) (argFetch 3) .1))
    (setq maxErr (if (>= (argCount) 5) (argFetch 4) .05))
    (setq GenerationMax (if (>= (argCount) 6) (integer (argFetch 5)) 1))
    (setq mySVSize (if (>= (argCount) 7) (argFetch 6) 100))
    (setq myVerboseSW (if (>= (argCount) 8) (argFetch 7) false))
    ;; Train a new support vector machine Lambda.
    (svmTraining x y kernelID tollerance maxErr GenerationMax)
    (setq result (svmLambda))
    result) ; end smoRegress
















;;**EXPORTKEY**:svmRegress
(defun svmRegress(x y kernelID ...)
;; *******************************************************************
;; name:     svmRegress
;; 
;; summary:  Trains a support vector machine regression Lambda, using the
;;           Sequential Error Estimation (SEE) algorithm and returns the 
;;           trained estimator Lambda. (multiple regression network)
;;
;;           The Sequential Error Estimation (SEE) algorithm, allows a
;;           single kernel function or a vector of kernel functions to 
;;           build a support vector regression network linked with a
;;           multiple regression model.
;;            
;;           Sequential sample sets of the training examples are used
;;           to build multiple support vector regression models of the
;;           training data. The best of these models are linked together,
;;           via multiple regression, to form a composite model. This
;;           sequential sampling method automatically supports out-of-sample
;;           testing during the training phase; and, allows larger training
;;           data sets to be estimated in a fraction of the time required
;;           for an exhaustive Gramm matrix regression.
;;
;;           The model error is calculated based upon the absolute error of
;;           each estimate (calculated as a percent of the target variable).
;;           An error estimation grid can be constructed which penializes the
;;           model error for each grid slot whose average Y values are not
;;           sequentially increasing. This method enhances model fit in cases
;;           with low signal-to-noise ratios.  
;;
;; Parms:    x:         The N by M vector array representing the original observations
;;                      in the form of:    x x ... x
;;                                         x x ... x
;;                                             ... 
;;                                         x x ... x
;;           y   		The N vector of dependent variables.
;;           kernelID   The kernel identifier to be used for support vector machine training (may also be a vector of kernel functions).
;;                        "all"			A composite vector of svm regression kernel functions.  
;;                        "linear"		A linear dot product kernel.  
;;                        "square"		A squared linear dot product kernel.  
;;                        "cube"		A cubed linear dot product kernel.  
;;                        Lambda		    Any user supplied modified dot product kernel.  
;;                        function		Any user supplied modified dot product kernel.
;;           properties (Optional) The Structure of SVM training settings.
;;             ETollerance  (Property) The regression error tollerance as a percent of Y.
;;             GridErr      (Property) Size of the sequential error estimation grid.
;;             MaxErr  	    (Property) The maximum error before halting training as a percent of Y.
;;             MaxGen  	    (Property) The maximum generation count before halting training.
;;             MaxLayers    (Property) The maximum number of svm layers before halting training.
;;             ModelCount  	(Property) The maximum number of svm regression models to link via multiple regression.
;;             OverrideSW   (Property) True iff we are to use the user specified sample size without override.
;;             UserSVSize   (Property) The Gaussian sample size (maximum number of support vectors to use during Gaussian initialization).
;;             VerboseSW    (Property) True iff we are to set verbose mode on.
;; Return:   result: 	The result structure containing the trained SEE regression model results, where
;;		  	    			result.Error		Contains the final tollerant error (expressed as a percent of each target value)
;;		  	    			result.Generations	Contains the number of generations used during training
;;							result.Weights 		Contains the weight number vector after training
;;							result.Support 		Contains the support vectors after training
;;
;;			 Note1: Support Vector Machine regression can be highly accurate
;;				    when solving non-linear regression problems. However, the
;;                  accuracy varies from excellent to poor depending upon
;;                  the ratio of: the chosen Gaussian Sample Size (mySampleSize);
;;                  the number of regression variables (M); and the THEORETICAL
;;                  number of variables created by the kernel function to
;;                  make the non-linear problem linearly solvable. A simplified
;;                  example would be as follows.
;;
;;					Solving a quadratic regression problem with variableCount == 3,
;;                  y = sum{m from 0 until variableCount}(Cm*Xm*Xm), and 
;;                  a kernel function of vectorSquareInnerProduct, is very
;;                  accurate with a Gaussian Sample Size (mySampleSize) of 10.
;;                  However, if the variableCount is increased to 10, then the
;;                  accuracy completely breaks down and is not restored until
;;                  the Gaussian Sample Size is increased to around 100. An
;;                  explanation is as follows.
;;
;;					In order to make the quadratic regression linearly tractable,
;;                  the vectorSquareInnerProduct performs an on-the-fly squaring
;;                  of each training point vector. Thus, with a training point
;;                  vector of size three, the vectorSquareInnerProduct creates
;;                  the following on-the-fly THEORETICAL new training point:
;;					kernel(X1,X2,X3) => (X1,X2,X3,X1*X1,X2*X2,X3*X3,X1*X2,X1*X3,X2*X3).
;;                  Clearly the problem is now linearly solvable because the squared
;;                  variables are now terms in the THEORETICAL on-the-fly linear regression
;;                  created by the kernel. NOTICE however, that the THEORETICAL linear
;;                  regression, created on-the-fly by the kernel function, has nine variables
;;                  not three variables as in the original quadratic problem. Unless the
;;                  number of training points is greater than nine, and the Gaussian 
;;                  sample size is greater than nine, the on-the-fly linear regression
;;                  will not have enough data to get accurate results.
;; 
;;           Note2: See Cristianini, "Support Vector Machines", page 169.
;;           Note3: This SVM regression Lambda uses the Sequential Error Estimation (SEE)
;;                  algorithm.
;; *******************************************************************
    pvars:(;; Public variables
           Number:Error                 ;; The current absolute error for the SEE regression model (in percent of target). 
           Number:ErrorMax           	;; The maximum absolute error for the SEE regression model (in percent of target). 
           Number:ETollerance           ;; The current forgiving absolute error tollerance for the SEE regression model (in percent of target). 
           Integer:Generations          ;; The number of training cycles used to train the current SEE regression model. 
           Integer:GenerationMax        ;; The maximum number of training cycles before training is halted at any error rate. 
           kernel   		       		;; The current support vector machine kernel (user specified).
           kernelChoices                ;; The current support vector machine kernel choices (automatically assigned).
           Vector:KX                    ;; The kernel vector for the SEE regression model.
           Integer:myErrorGridSize      ;; The size of the sequential error estimation grid. 
           NumVector:myErrorGrid        ;; The sequential error estimation grid for the SEE regression model.
           Integer:myLayers     	    ;; The current number of svm regression network layers.
           Integer:myMaxLayers     	    ;; The maximum number of svm regression network layers.
           Integer:myModelCount     	;; The maximum number of svm regression models to link via multiple regression.
           myOverrideSW     			;; True iff we are to use the user specified sample size without override. 
           Integer:mySampleSize     	;; Size of the training sample sets, selected from the total training examples, during each iterative regression. 
           Integer:mySampleCount        ;; Count of the training sample sets, selected from the total training examples, during each iterative regression. 
           mySampleHistory              ;; History of the training sample sets, selected from the total training examples, during each iterative regression. 
           mySVMParent                  ;; The parent Lambda of this SVM regression Lambda community. 
           myVerboseSW      			;; True iff we are to display progress on the console. 
           Integer:M					;; The number of elements in each training example (independent variables). 
           Integer:N					;; The number of training examples (independent variables). 
           IntVector:sortedY            ;; The N vector of target points, dependent variable, sorted in ascending order. 
           NumVector:W                  ;; The weight coefficient vector for the SEE regression model.
           ObjVector:WX                 ;; The object vector of support vectors for the SEE regression model.
           X                       		;; The N x M matrix of training examples, independent variables, for training the SEE regression model. 
           NumVector:Y                  ;; The N vector of training examples (dependent variables). 
           ;; Public child methods
           clear			       		;; Clear the current support vector machine.
           compositeKernel              ;; A built in composite kernel used for machine learning.
           computeError	           		;; Return the average percent error for all of the training examples.
           createCompositeModel	        ;; Create the best composite regression model from the individual support vector regressions from each training generation.
           createHiddenLayerArray       ;; Create the hidden layer training array for training the next support vector layer.
           kernel   		       		;; Return the svm kernel output for the SEE regression model.
	       multipleRegression           ;; Performs a Gaussian multiple regression on the N x M+1 matrix
           svmLambda			       		;; Return an Lambda ready to compute the svm output for specified input vector.
           svmMultipleLayerLambda		;; Return a multiple layer Lambda ready to compute the svm output for specified input vector.
           svmOutput		       		;; Return the svm output for the specified input vector.
           svmTraining		       		;; Train the svm machine on the specified training examples.
           trainRegressionModel		    ;; Incrementally train the SEE regression model.
           ;; Private maintenance child methods
           selfTest                		;; The self test method for this Lambda. 
           ;; Private maintenance templates
           (myHistoryTemplate #{Step: #void Layer: #void Error: #void KX: #void W: #void WX: #void Ey: #void Py: #void Composite: #void}) 
           ) ;; end of persistent variables
    ;; ***************************
    ;; Define Public Child Lambdas.
    ;; ***************************
    ;; Clear the current support vector machine.
    (defun clear()
       (setq kernel (lambda(k) k))
       (setq Error 0.0)
       (setq M 0)
       (setq N 0)
       (setq Generations 0) 
       (setq sortedY #void)
       (setq W #void)
       (setq X #void)
       (setq Y #void)
       (setq mySampleHistory #void)
       true) ; end clear
    ;; A built in composite kernel used for machine learning.
    (defun compositeKernel(NumVector:x NumVector:y)
       regs:( Number:kx (Number:xx 1.0) Number:ex Number:lx)
       (setq kx (|Gv:vectorInnerProduct| x y))
       (setq ex (- (/ 2.0 (+ 1.0 (exp (- kx)))) 1.0))
       (setq lx (log (+ 1.0 (abs kx)))) 
       (+= xx kx) ;; linear
       (+= xx (if (> ex 0.0) 1.0 0.0)) ;; binary
       (+= xx (if (> ex 0.0) 1.0 -1.0)) ;; bipolar
       (+= xx (cos kx)) ;; cosine
       (+= xx ex) ;; exponent
       (+= xx lx) ;; log
       (+= xx (/ 1.0 (+ 1.0 (exp (- kx))))) ;; sigmoid  
       (+= xx (/ 1.0 (+ 1.0 (exp (- kx))))) ;; sigmoid  
       (+= xx (sin kx)) ;; sine
       (+= xx (tan kx)) ;; tangent
       (+= xx (tanh kx)) ;; hypertangent
       (setq xx (* xx xx xx))
       xx) ; end compositeKernel
    ;; Return the average percent error for all of the training examples.
    ;; Note1: The regression error is computed as a percent of the target (dependent variable).
    ;; Note2: Any regression error within the tollerance limit, ETollerance, is treated as NO error.
    (defun computeError(Structure:HRecord)
       regs:(k m n NN)
       regs:(Number:etol Number:pct Number:dy Number:ey Number:Yn)
       regs:(Number:Pyk Number:Pym)
       regs:((Number:err 0.0) (Number:RZero 0.0) Number:QAdj (Number:QSum 0.0))
       regs:(NumPointer:pY NumPointer:pEy NumPointer:pPy)
       vars:(NumVector:Ey NumVector:Py IntVector:sortedEy)
       ;; Initialize (if necessary)
	   (setq etol ETollerance)
	   (setq NN N)
       (setq pY Y)
       (setq HRecord.Ey (setq Ey (new Vector: Number: NN)))
       (setq HRecord.Py #void)
       (setq pEy Ey)
       ;; We recompute the average absolute percent error
       ;; Note: This is a mission critical Lambda and MUST run extra fast.
       (vmregRunInHardware start:)
	   (loop for n from 0 until NN do
          (setq ey (svmOutput X[n] HRecord))
          (setq pEy[n] ey) 
          (setq Yn pY[n]) 
          (setq dy (- ey Yn))
	      (if (<> Yn RZero) then (/= dy Yn))
          (setq pct (|Gv:abs| dy))
          (if (> pct etol) (-= pct etol) (setq pct 0.0))
	      (+= err pct)
	      ) ; end error loop
       (vmregRunInHardware stop:)
	   (/= err NN)
       (setq HRecord.Error err)
       (setq HRecord.ETollerance ETollerance)
       ;; Compute error estimation grid and adjust percent error score accordingly.
       ;; Note: We compute the average Y value for each slot in the error estimation
       ;;       grid. Ideally sequentially increasing grid slots should have 
       ;;       sequentially increasing average Y values. For every grid slot where
       ;;       this ideal condition is NOT true, we penalize the error score.
       (if (> myErrorGridSize 1)
           (begin
		      (setq sortedEy (sort Ey < true))
              (setq HRecord.Py (setq Py (new Vector: Number: myErrorGridSize)))
              (setq pY Y)
		      (setq pPy Py)
              (setq pEy Ey)
		      (setq QAdj (/ myErrorGridSize NN))
		      (vmregRunInHardware start:)
			  (loop for n from 0 until NN do
		         (setq m sortedEy[n])
		         (setq Yn pY[m])
		         (*= Yn QAdj)
		         (setq k QSum)
                 (setq Pyk pPy[k])
		         (+= Pyk Yn) 
		         (setq pPy[k] Pyk) 
		         (+= QSum QAdj) 
			     ) ; end error loop
		      (vmregRunInHardware stop:)
		      ;; Penalize the final error score for every grid slot
              ;; which fails a simple sequential increase test.
		      (vmregRunInHardware start:)
              (setq m 0)
              (loop for k from 1 until myErrorGridSize do 
                (setq Pym pPy[m])
                (setq Pyk pPy[k])
		        (if (<= Pyk Pym) then (*= err 10.0))
                (++ m)
		        ) ; end K loop  
		      (vmregRunInHardware stop:)
              (setq HRecord.Error err)
           )) ;; end compute error esitmation grid
       err) ; end computeError
    ;; Create the best composite regression model from the individual support vector regressions from each training generation.
    (defun createCompositeModel()
       regs:(k K m MM n NN rm (KC 2))
       regs:(Number:wk Number:cc)
       regs:(NumPointer:pRM NumPointer:pEy NumPointer:pEy NumPointer:pY)
       vars:(Structure:HRecord Structure:HR)
       vars:(NumVector:CS NumVector:WK NumMatrix:RM NumVector:Ey)
       ;; Do NOT create a composite model if there are less than two sampled regression models.
       (setq K (length mySampleHistory))
       (if (< K KC) (return mySampleHistory[0]))
       (sort mySampleHistory (lambda(x y) (< x.Error y.Error)))
       ;; Form a composite model which combines the previous sampled regressions.
       ;; Note: We use another multiple regression to compute the optimal
       ;;       coefficients for merging the previous support vector regression models.
       (setq NN N)
       (setq RM (new Matrix: Number: 2 NN (addi KC 1)))
       (vmregRunInHardware start:)
       (setq pY Y)
       (setq pRM RM)
       (setq rm -1)
       (loop for n from 0 until NN do
          (loop for k from 0 until KC do
            (setq HR mySampleHistory[k])
            (setq Ey HR.Ey)
            (setq pEy Ey)
            (++ rm)(setq pRM[rm] pEy[n])
            ) ; end K loop
          (++ rm)(setq pRM[rm] pY[n])
          ) ; end NN loop
       (vmregRunInHardware stop:)
       ;; Solve for the best Gaussian coefficients for merging the support vector models.
       (setq CS (multipleRegression RM))
       (setq mySampleHistory[K] (setq HRecord (new myHistoryTemplate)))
       (setq HRecord.Step Composite:)
       (setq HRecord.W (new Vector: Number:))
       (setq HRecord.WX (new Vector: Object:))
       (setq HRecord.KX (new Vector:))
       (vmregRunInHardware start:)
       (loop for k from 0 until KC do
          (setq HR mySampleHistory[k])
          (setq WK (copy HR.W))
          (setq cc CS[k])
          (setq MM (length WK))
          (loop for m from 0 until MM do
             (setq wk WK[m])
             (*= wk cc)
             (setq WK[m] wk)
             ) ; end m
          (setq HRecord.W (append HRecord.W WK))
          (setq HRecord.WX (append HRecord.WX HR.WX))
          (setq HRecord.KX (append HRecord.KX HR.KX))
          ) ; end K loop
       (vmregRunInHardware stop:)
       (computeError HRecord)
       (if (> Error HRecord.Error)
           (begin
             (setq Error HRecord.Error)
             (setq W HRecord.W)
             (setq KX HRecord.KX)
             (setq WX HRecord.WX)
             (setq myErrorGrid HRecord.Py)
           )) ; end if
       HRecord) ; end createCompositeModel
    ;; Create the hidden layer training array for training the next support vector layer.
    (defun createHiddenLayerArray(Lambda Integer:modelCount)
       regs:(k K n NN)
       vars:(tX TX)
       ;; Do NOT create a hidden layer if there are less than two sampled regression models.
       (setq K (length mySampleHistory))
       (if (or (< modelCount 2) (< K modelCount)) (return #void))
       (sort mySampleHistory (lambda(x y) (< x.Error y.Error)))
       ;; Form a composite model which combines the previous sampled regressions.
       ;; Note: We use another multiple regression to compute the optimal
       ;;       coefficients for merging the previous support vector regression models.
       (setq NN N)
       (setq TX (new Vector: Object: NN))
       (setq Lambda.hiddenLambdas (new Vector: Object: K))
       (loop for k from 0 until modelCount do (setq Lambda.hiddenLambdas[k] (svmLambda mySampleHistory[k])))
       (loop for n from 0 until NN do
          (setq TX[n] (setq tX (new Vector: Number: modelCount)))
          (loop for k from 0 until modelCount do
            (setq tX[k] mySampleHistory[k].Ey[n])
            ) ; end K loop
          ) ; end NN loop
       TX) ; end createHiddenLayerArray
	;; summary:  Performs a Gaussian multiple regression on the N by M+1 matrix
	;; Parms:    MXY:     The N by M+1 matrix representing the original observations
	;;                    in the form of:    x x ... x y
	;;                                       x x ... x y
	;;                                           ... 
	;;                                       x x ... x y
	;; Return:   C:       The M coefficient vector for the regression.
	;; Note1:    Average error statistics are computed as a percent of the target (dependent variable).
	;; Note2:    See Sedgewick[2] chap 37.
	(defun multipleRegression(NumMatrix:MXY)
	    vars:(NumMatrix:Xt NumVector:C)
	    ;; Perform a least squares regression on all the factors.
	    (setq Xt (|Gv:makeGaussianMatrix| MXY))
	    (setq Xt (|Gv:matrixGaussianEliminate| Xt true))
	    (setq C (|Gv:matrixGaussianSubstitute| Xt))
	    ;; Return the coefficient vector for the regression.
	    C) ; end multipleRegression
    ; Return an Lambda ready to compute the svm output for specified input points.
    (defun svmLambda(...)
       regs:(n m NW)
       vars:(NumVector:hW Vector:hKX ObjVector:hWX)
       vars:(Lambda HRecord)
       (setq Lambda (eval "(lambda(x) pvars:(Number:Error NumVector:ErrorGrid Number:ETollerance Integer:M Integer:N Strategy ObjVector:WX Vector:KX NumVector:W) regs:(n NN Number:xn Number:wn Number:ey NumPointer:pW) (setq NN (length W)) (setq pW W) (loop for n from 0 until NN do (setq wn pW[n]) (if (<> wn 0.0) then (begin (setq xn (KX[n] x WX[n])) (*= xn wn) (+= ey xn)  )) ) ey)"))
       (setq Lambda.Strategy svmSEE:)
       (if (>= (argCount) 1) (setq HRecord (argFetch 0)) (setq HRecord mySampleHistory[0]))
       ;; Drop all but the support vectors for the trained model.
       (setq hW HRecord.W)
       (setq hKX HRecord.KX)
       (setq hWX HRecord.WX)
       (setq NW (length hW))
       (setq m 0)(loop for n from 0 until NW do (if (<> hW[n] 0.0) (++ m)))
       (setq Lambda.N m)
       (setq Lambda.W (new Vector: Number: m))
       (setq Lambda.KX (new Vector: m))
       (setq Lambda.WX (new Vector: Object: m))
       (setq m 0)
       (loop for n from 0 until NW do 
          (if (<> hW[n] 0.0)
              (begin
                 (setq Lambda.W[m] hW[n])
                 (setq Lambda.KX[m] hKX[n])
                 (setq Lambda.WX[m] hWX[n])
                 (++ m)
              )) ; end if
          ) ; end loop
       (setq Lambda.M M)
       (setq Lambda.ETollerance HRecord.ETollerance)
       (setq Lambda.ErrorGrid HRecord.Py)
       (setq Lambda.Error HRecord.Error)
       Lambda) ; end svmLambda
    ; Return a multiple layer Lambda ready to compute the svm output for specified input points.
    (defun svmMultipleLayerLambda(kernelID)
       regs:(n m modelCount)
       vars:(NumVector:hW Vector:hKX ObjVector:hWX)
       vars:(TX Lambda nextLambda HRecord properties)
       (setq Lambda (eval "(lambda(x) pvars:(Number:Error NumVector:ErrorGrid  Number:ETollerance Integer:M Integer:N Strategy Lambda:outputLambda ObjVector:hiddenLambdas Integer:N Integer:eyM) regs:(m MM Number:xn Number:wn Number:ey) vars:(NumVector:eyX) (setq MM eyM) (setq eyX (new Vector: Number: eyM)) (loop for m from 0 until MM do (setq eyX[m] (hiddenLambdas[m] x)) (setq ey (outputLambda eyX))) ey)"))
       (setq Lambda.Strategy svmSEE:)
	   ;; Setup the properties for the next layer of svm training.
       (setq modelCount myModelCount)
       (setq properties (new Structure: GridErr: myErrorGridSize
                                        MaxGen: GenerationMax
                                        MaxLayers: myMaxLayers
                                        CurrentLayer: myLayers
                                        ModelCount: modelCount
                                        OverrideSW: myOverrideSW
                                        UserSVSize: mySampleSize
                                        VerboseSW: myVerboseSW
                                        ETollerance: ETollerance
                                        MaxErr: ErrorMax
                               )) ; end new properties
       ;; Train the output layer support vector Lambda, using the hidden layer Y estimates as inputs.
       (setq Lambda.eyM modelCount)
       (setq Lambda.M M)
       (setq Lambda.N N)
       (setq TX (createHiddenLayerArray Lambda modelCount))
       (setq nextLambda (mySVMParent TX Y kernelID properties))
       ;; Place together the support vectors layers for the final trained model.
       (setq Lambda.outputLambda nextLambda)
       (setq Lambda.ETollerance nextLambda.ETollerance)
       (setq Lambda.ErrorGrid nextLambda.ErrorGrid)
       (setq Lambda.Error nextLambda.Error)
       ;; Never return a layered model with greater error than the current best model
       (if (>= Lambda Error) (setq Lambda (svmLambda)))
       Lambda) ; end svmMultipleLayerLambda
    ;; Return the svm output for the specified input point.
    ;; Note1: x must be a vector of length M
    ;; Note2: The SEE model says: y ~= sum{n=0-N,W[n]*Y[n]*Kernel(X[n],x)}
    (defun svmOutput(NumVector:x Structure:HRecord)
       regs:(n NN)
       regs:(Number:xn Number:wn Number:ey)
       regs:(NumPointer:pW)
       vars:(NumVector:tW ObjVector:tWX Vector:tKX)
       (setq tW HRecord.W)
       (setq tWX HRecord.WX)
       (setq tKX HRecord.KX)
       (setq NN (length tW))
       (setq pW tW)
       (loop for n from 0 until NN do
          (setq wn pW[n])
          (if (<> wn 0.0)
              then
              (begin 
                (setq xn (tKX[n] x tWX[n]))
                (*= xn wn)
                (+= ey xn)
              )) ; end if        
          ) ; end NN model loop
       ey) ; end svmOutput
    ;; Train the svm machine on the specified inputs and model.
	;; Parms:    x:         The N by M vector array representing the original observations
	;;                      in the form of:    x x ... x
	;;                                         x x ... x
	;;                                             ... 
	;;                                         x x ... x
	;;           y   		The N vector of dependent variables.
	;;           kernelID   The kernel identifier to be used for support vector machine training.
	;;                        #void			A linear dot product kernel.  
	;;                        "linear"		A linear dot product kernel.  
	;;                        "square"		A squared linear dot product kernel.  
	;;                        "cube"		A cubed linear dot product kernel.  
	;;                        Lambda		    Any user supplied modified dot product kernel.  
	;;                        function		Any user supplied modified dot product kernel.  
    ;;           properties The Structure of SVM training settings.
    ;;             ETollerance  (Property) The regression error tollerance as a percent of Y.
    ;;             GridErr      (Property) Size of the sequential error estimation grid.
    ;;             MaxErr  	    (Property) The maximum error before halting training as a percent of Y.
    ;;             MaxGen  	    (Property) The maximum generation count before halting training.
    ;;             MaxLayers    (Property) The maximum number of svm layers before halting training.
    ;;             ModelCount  	(Property) The maximum number of svm regression models to link via multiple regression.
    ;;             OverrideSW   (Property) True iff we are to use the user specified sample size without override.
    ;;             UserSVSize   (Property) The Gaussian sample size (maximum number of support vectors to use during Gaussian initialization).
    ;;             VerboseSW    (Property) True iff we are to set verbose mode on.
	;; Return:   result: 	The result structure containing the trained SEE regression model results, where
	;;		  	    			result.Error		Contains the final tollerant error (expressed as a percent of each target value)
	;;		  	    			result.Generations	Contains the number of generations used during training
	;;							result.Weights 		Contains the weight number vector after training
    ;;							result.Support 		Contains the support vectors after training
    (defun svmTraining(x y kernelID properties)
        regs:(k K m n NN SN minSV begK)
        vars:(result HRecord userSVSize)
	    ;; Clear support vector machine for retraining.
	    (clear)
	    ;; Retrieve any optional arguments and perform setup.
	    (setq myErrorGridSize properties.GridErr)
	    (setq GenerationMax properties.MaxGen)
	    (setq myMaxLayers properties.MaxLayers)
	    (setq myLayers properties.CurrentLayer)
	    (setq myModelCount properties.ModelCount)
	    (setq myOverrideSW properties.OverrideSW)
	    (setq userSVSize properties.UserSVSize)
	    (setq myVerboseSW properties.VerboseSW)
		(setq ETollerance properties.ETollerance)
	    (setq ErrorMax properties.MaxErr)
	    (if (or (<> (isVector x) true) (<> (isVector x[0]) true)) (error "svmRegress: X argument must be a Vector Array of rank 2"))
	    (setq m (length x[0]))
	    (setq n (length x))
	    (if (or (<> (isVector y) true) (<> (length y) n)) (error "svmRegress: Y argument must be a Vector of length the same as X"))
	    ;; Initialize the untrained svm SEE model.
	    (setq X x)
	    (setq Y y)
	    (setq M m)
	    (setq N n)
	    ;; Initialize the user specified support vector machine kernel.
	    (cond
	      ((or (= kernelID "all") (= kernelID "default") (= kernelID #void)) (begin (setq kernelChoices (new Vector: 13 |Gv:vectorInnerProduct| |Gv:vectorSquareInnerProduct| |Gv:vectorCubeInnerProduct| |Gv:vectorSigmoidInnerProduct| |Gv:vectorExpInnerProduct| |Gv:vectorLogInnerProduct| |Gv:vectorTanInnerProduct| |Gv:vectorTanhInnerProduct| |Gv:vectorSineInnerProduct| |Gv:vectorCosineInnerProduct| |Gv:vectorBinaryInnerProduct| |Gv:vectorBipolarInnerProduct| compositeKernel)) (setq minSV (* M M M))))
	      ((or (= kernelID "binary") (= kernelID |Gv:vectorBinaryInnerProduct|)) (begin (setq kernelChoices (new Vector: 1 |Gv:vectorBinaryInnerProduct|)) (setq minSV (* M M M))))
	      ((or (= kernelID "bipolar") (= kernelID |Gv:vectorBipolarInnerProduct|)) (begin (setq kernelChoices (new Vector: 1 |Gv:vectorBipolarInnerProduct|)) (setq minSV (* M M M))))
	      ((= kernelID "composite") (begin (setq kernelChoices (new Vector: 1 compositeKernel)) (setq minSV (* M M M))))
	      ((or (= kernelID "cosine") (= kernelID |Gv:vectorCosineInnerProduct|)) (begin (setq kernelChoices (new Vector: 1 |Gv:vectorCosineInnerProduct|)) (setq minSV (* M M M))))
	      ((or (= kernelID "cube") (= kernelID |Gv:vectorCubeInnerProduct|)) (begin (setq kernelChoices (new Vector: 1 |Gv:vectorCubeInnerProduct|)) (setq minSV (* M M M))))
	      ((or (= kernelID "exp") (= kernelID |Gv:vectorExpInnerProduct|)) (begin (setq kernelChoices (new Vector: 1 |Gv:vectorExpInnerProduct|)) (setq minSV (* M M M))))
	      ((or (= kernelID "linear") (= kernelID |Gv:vectorInnerProduct|)) (begin (setq kernelChoices (new Vector: 1 |Gv:vectorInnerProduct|)) (setq minSV M)))
	      ((or (= kernelID "log") (= kernelID |Gv:vectorLogInnerProduct|)) (begin (setq kernelChoices (new Vector: 1 |Gv:vectorLogInnerProduct|)) (setq minSV (* M M M))))
	      ((or (= kernelID "quart") (= kernelID |Gv:vectorQuartInnerProduct|)) (begin (setq kernelChoices (new Vector: 1 |Gv:vectorQuartInnerProduct|)) (setq minSV (* M M M M))))
	      ((or (= kernelID "quint") (= kernelID |Gv:vectorQuintInnerProduct|)) (begin (setq kernelChoices (new Vector: 1 |Gv:vectorQuintInnerProduct|)) (setq minSV (* M M M M M))))
	      ((or (= kernelID "sigmoid") (= kernelID |Gv:vectorSigmoidInnerProduct|)) (begin (setq kernelChoices (new Vector: 1 |Gv:vectorSigmoidInnerProduct|)) (setq minSV (* M M M))))
	      ((or (= kernelID "sine") (= kernelID |Gv:vectorSineInnerProduct|)) (begin (setq kernelChoices (new Vector: 1 |Gv:vectorSineInnerProduct|)) (setq minSV (* M M M))))
	      ((or (= kernelID "square") (= kernelID |Gv:vectorSquareInnerProduct|)) (begin (setq kernelChoices (new Vector: 1 |Gv:vectorSquareInnerProduct|)) (setq minSV (* M M))))
	      ((or (= kernelID "tan") (= kernelID |Gv:vectorTanInnerProduct|)) (begin (setq kernelChoices (new Vector: 1 |Gv:vectorTanInnerProduct|)) (setq minSV (* M M M))))
	      ((or (= kernelID "tanh") (= kernelID |Gv:vectorTanhInnerProduct|)) (begin (setq kernelChoices (new Vector: 1 |Gv:vectorTanhInnerProduct|)) (setq minSV (* M M M))))
	      ((isVector kernelID) (begin (setq kernelChoices kernelID) (setq minSV M)))           
	      (else  (begin (setq kernelChoices (new Vector: 1 kernelID)) (setq minSV M)))
	      ) ; end cond 
		;; Initialize the persistent variables before proceeding with training.
        (setq sortedY (sort Y < true))
		(setq Error 1.0e300)
        (setq W #void)
	    ;; Run multiple generations taking the first one to achieve success.
	    ;; Note: If no generation achieves success, we take the best so far.
        (if (and (= myOverrideSW true) (< userSVSize minSV)) (setq mySampleSize minSV) (setq mySampleSize userSVSize))      
        (if (< N mySampleSize) (error "svmRegress: too few examples (N) for columns (M) and choice of kernel"))
        (setq mySampleCount (/ N mySampleSize))
        (if (< mySampleCount 1) then (begin (setq mySampleSize N) (setq mySampleCount 1)))
        (setq mySampleHistory (new Vector:))
	    (setq Generations 0)
	    ;; Train the next generation.
        (setq K (length kernelChoices))
	    (while (and (>= Error ErrorMax) (< Generations GenerationMax)) do
           (loop for k from 0 until K do
              (setq kernel kernelChoices[k])
	          (trainRegressionModel)
              ) ; end kernel loop
	       (++ Generations)
	       ) ; end while
        ;; Create the best possible composite regression model.
        (createCompositeModel)
        ;; Create training result structure, and clean up after learning.
        (++ myLayers)
        (if (>= myLayers myMaxLayers) 
	        (setq result (svmLambda))
	        (setq result (svmMultipleLayerLambda kernelID))
	        ) ; end if
	    ;; Generate the final absolute error score after training.
	    HaltTraining::
        (if (= result #void) (setq result (svmLambda)))
	    (if myVerboseSW (writeln "svmRegress: Final Model Generations = [" Generations "], SVMs = [" (length W) "], ETollerance = [" ETollerance "], Error = [" Error "]"))       
        result) ; end svmTraining
    ;; Incrementally train the SEE model on the two training points.
    ;; Note1: The regression error is computed as a percent of the target.
    ;; Note2: Any regression error within the tollerance limit, ETollerance, is treated as NO error.
    (defun trainRegressionModel()
       regs:(k K m n rm sn)
       regs:(Number:dotProduct Number:y Number:xk Number:xn)
       regs:(IntPointer:pSV IntPointer:pSortedY NumPointer:pXn NumPointer:pXk NumPointer:pRM)
       regs:(Number:avgY Number:avgErr)
       regs:(Integer:NN                  ;; Number of total training examples.
             Integer:SN                  ;; Number of seed support vectors to use in Gaussian initialization.
             Integer:firstStep           ;; Example first step size for use in support vector selection.
             Integer:stepInc             ;; Example selection step size for use in support vector selection.
             Number:weightFactor         ;; Weight factor for use in support vector merging.
             ) ; end register variables
       vars:(NumVector:CS                ;; The Gaussian regression coefficients for the support vectors.
             ObjVector:HRecord  		 ;; The history record for the nth sample set.
             NumMatrix:RM 				 ;; The regression matrix regressing the support vectors against all of the training points in X.
             IntVector:sortedHistories	 ;; The Integer Vector of the sorted sample set histories.
             IntVector:SV				 ;; The indices of the current support vector set. 
             NumVector:Xk 				 ;; The Number Vector, of the independent variables, for the kth training example.
             NumVector:Xn 				 ;; The Number Vector, of the independent variables, for the nth training example.
             Vector:tKX 				 ;; The Vector, of the kernel functions, for the nth sample set.
             ObjVector:tWX 				 ;; The Object Vector, of the support vectors, for the nth sample set.
             ) ; end temporary variables
       ;; Zero the errors where there are already support vectors.
       (setq firstStep (integer (+ Generations (* myLayers GenerationMax))))
       (if (>= firstStep mySampleCount) (goto TrainingCompleted:))
       (setq K (length SV))
       (setq NN N)
       ;; Extend the support vectors with the examples with the worst percent errors
       (setq SN mySampleSize)
       (setq SV (new Vector: Integer: SN))
       (setq stepInc mySampleCount)
       (setq pSV SV)
       (setq pSortedY sortedY)
       (setq n firstStep)
       (vmregRunInHardware start:)
       (loop for sn from 0 until SN do
		  (setq pSV[sn] pSortedY[n])
          (+= n stepInc)
          ) ; end loop 
       (vmregRunInHardware stop:)
       (setq SN (length SV))
       ;; Construct the support vectors regression matrix.
       ;; Note: We attempt to regress a linear model of the support vectors
       ;;       against all of the training points in X. The Regression
       ;;       Matrix is of the following form:
       ;;          kernel(X[0],X[SV[0]]) ... kernel(X[0],X[SV[SN]]) Y[0]
       ;;          kernel(X[1],X[SV[0]]) ... kernel(X[1],X[SV[SN]]) Y[1]
       ;;                  ...           ...              ...
       ;;          kernel(X[N],X[SV[0]]) ... kernel(X[N],X[SV[SN]]) Y[N]
       (setq RM #void)
       (setq RM (new Matrix: number: 2 NN (addi SN 1)))
       (vmregRunInHardware start:)
       (setq pSV SV)
       (setq pRM RM)
       (setq rm -1)
       (loop for n from 0 until NN do
           (loop for m from 0 until SN do
              (setq k pSV[m])
              (setq Xk X[k])
              (setq Xn X[n])
              (setq dotProduct (kernel Xk Xn))
              (++ rm)
              (setq pRM[rm] dotProduct)
              ) ; end SN loop
           (++ rm)
           (setq pRM[rm] Y[n])
           ) ; end NN loop
       (vmregRunInHardware stop:)
       ;; Solve for the Gaussian coefficients of the support vectors.
       ;; Note: We perform a Gaussian linear regression on the Regression
       ;;       Matrix, returning an SN Vector of coefficients, which we
       ;;       set as the weights, in the SVM  model, of each of the 
       ;;       support vectors respectively. All other non-support-vector
       ;;       weights, in the initial SVM model, are set to zero.
       (setq CS (multipleRegression RM))
       (setq K (length mySampleHistory))
       (setq mySampleHistory[K] (setq HRecord (new myHistoryTemplate)))
       (setq HRecord.Step Generations)
       (setq HRecord.W CS)
       (setq HRecord.WX (setq tWX (new Vector: Object: SN)))
       (setq HRecord.KX (setq tKX (new Vector: SN)))
       (vmregRunInHardware start:)
       (loop for m from 0 until SN do
          (setq k SV[m])
          (setq tKX[m] kernel)
          (setq tWX[m] X[k])
          ) ; end loop
       (vmregRunInHardware stop:)
       ;; Score the current Generation's regression model.
       (computeError HRecord)
       (if (> Error HRecord.Error)
           (begin
              (setq Error HRecord.Error)
              (setq W HRecord.W)
              (setq KX HRecord.KX)
              (setq WX HRecord.WX)
              (setq myErrorGrid HRecord.Py)
           )) ; end if
       ;; This generation of SVM model training is complete.
       TrainingCompleted::
       (if myVerboseSW (writeln "svmRegress: next generation complete.")) 
       ;; Return success
       true) ; end trainRegressionModel
    ;; ****************************************
    ;; Define Private Maintenance Child Lambdas.
    ;; ****************************************
    ;; The self test method for this Lambda.
    (defun selfTest(Test kernelID Ms Ns Gs Lc Mc Ss Eg Os)
       vars:(k m n g G y ey C c X Y Yv avgY avgTopEy topEyCnt
             Lambda err Net pct properties 
             startTime endTime startTimeT endTimeT
             (checkResults true)
             (tol 0.0) (errStop 0.01) (Cs 1.0)
             ) ; end temporary variables
       (clear)
       (setq properties (new Structure: ETollerance: 0.00 GridErr: Eg MaxErr: 0.01 MaxGen: Gs MaxLayers: Lc ModelCount: Mc OverrideSW: Os UserSVSize: Ss VerboseSW: (setq myVerboseSW false)))
       (setq startTimeT (getTickCount 0))
       (setq srandom.seed 8192.0)      
       ;; Select the requested test case
       ;; Test Case linear 
       (if (or (= Test all:) (= Test linear:))
           (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: We support a bias by having X[0] == 1 for all N.
		       ;; Note2: This algorithm seems to work well when N is at least 25 times M.
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do 
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (setq y (+ y (* X[n][m] C[m])))
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: linear")
		       (setq Lambda (setq Lambda (svmRegress.svmTraining X Y kernelID properties)))
		       (if (= myVerboseSW false) (writeln "svmRegress: N = [" Ns "], M = [" Lambda.M "], Generations = [" Generations  "], Layers = [" myMaxLayers  "], SVM's = [" Lambda.N "], ETollerance=[" Lambda.ETollerance "], Error=[" Lambda.Error "], ErrorGrid=[" (string Lambda.ErrorGrid true) "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "svmRegress: err=[" err "], avgY=[" avgY "]")
		           )) ; end if
          )) ; end Test Case linear
       ;; Test Case linearSigmoid 
       (if (or (= Test all:) (= Test linearSigmoid:))
           (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: The inputs, X, are restricted to the sigmoid domain.
		       ;; Note2: We support a bias by having X[0] == 1 for all N.
		       ;; Note3: This algorithm seems to work well when N is at least 25 times M.
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 1) .5))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do 
		          	 (setq X[n][m] (- (srandom .999999999) 0))
		             (setq y (+ y (* X[n][m] C[m])))
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: linearSigmoid")
		       (setq Lambda (setq Lambda (svmRegress.svmTraining X Y kernelID properties)))
		       (if (= myVerboseSW false) (writeln "svmRegress: N = [" Ns "], M = [" Lambda.M "], Generations = [" Generations  "], Layers = [" myMaxLayers  "], SVM's = [" Lambda.N "], ETollerance=[" Lambda.ETollerance "], Error=[" Lambda.Error "], ErrorGrid=[" (string Lambda.ErrorGrid true) "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (if (> y avgY) then (begin (++ topEyCnt) (+= avgTopEy Y[n])))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "svmRegress: err=[" err "], avgY=[" avgY "]")
		           )) ; end if
          )) ; end Test Case linearSigmoid
       ;; Test Case srandom 
       (if (or (= Test all:) (= Test srandom:))
           (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: We support a bias by having X[0] == 1 for all N.
		       ;; Note2: This algorithm seems to work well when N is at least 25 times M.
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (setq y (+ y (* X[n][m] C[m])))
		             ) ; end M loop
		          (setq Y[n] (+ (* y .8) (* y (srandom .4))))
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: srandom")
		       (setq Lambda (setq Lambda (svmRegress.svmTraining X Y kernelID properties)))
		       (if (= myVerboseSW false) (writeln "svmRegress: N = [" Ns "], M = [" Lambda.M "], Generations = [" Generations  "], Layers = [" myMaxLayers  "], SVM's = [" Lambda.N "], ETollerance=[" Lambda.ETollerance "], Error=[" Lambda.Error "], ErrorGrid=[" (string Lambda.ErrorGrid true) "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (if (> y avgY) then (begin (++ topEyCnt) (+= avgTopEy Y[n])))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "svmRegress: err=[" err "], avgY=[" avgY "]")
		           )) ; end if
          )) ; end Test Case srandom
       ;; Test Case mixedRandom 
       (if (or (= Test all:) (= Test mixedRandom:))
           (begin
		       ;; Create a test polynomial linear model where y = C[0]*X[0] + C[1]*X[1] + C[2]*X[2] ...
		       ;; Create a test polynomial square model where y = C[0]*X[0]*X[0] + C[1]*X[1]*X[1] + C[2]*X[2]*X[2] ...
		       ;; Create a test polynomial sin model where y = C[0]*sin(X[0]) + C[1]*sin(X[1]) + C[2]*sin(X[2]) ...
		       ;; Create a test polynomial log model where y = C[0]*log(abs(X[0])+.000001) + C[1]*log(abs(X[1])+.000001) + C[1]*log(abs(X[2])+.000001) ...
               ;; These four models are mixed together and random noise is added.
		       ;; Note1: We support a bias by having X[0] == 1 for all N.
		       ;; Note2: This algorithm seems to work well when N is at least 25 times M.
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
                  (setq k (modi n 4)) 
 		          (setq X[n][0] 1.0)
 		          (setq X[n][1] (number k))
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 2 until M do
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
                     ;; Mix the four models together
                     (cond
                        ;; Linear model
                        ((= k 0) (setq y (+ y (* X[n][m] C[m]))))
                        ;; Square model
                        ((= k 1) (setq y (+ y (* X[n][m] X[n][m] C[m]))))
                        ;; Sine model
                        ((= k 2) (setq y (+ y (* (|Gv:sin| X[n][m]) C[m]))))
                        ;; Log model
                        (else (setq y (+ y (* (|Gv:log| (+ .000001 (|Gv:abs| X[n][m]))) C[m]))))
                        ) ; end cond
		             ) ; end M loop
		          (setq Y[n] (+ (* y .8) (* y (srandom .4))))
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: mixedRandom")
		       (setq Lambda (svmRegress.svmTraining X Y kernelID properties))
		       (if (= myVerboseSW false) (writeln "svmRegress: N = [" Ns "], M = [" Lambda.M "], Generations = [" Generations  "], Layers = [" myMaxLayers  "], SVM's = [" Lambda.N "], ETollerance=[" Lambda.ETollerance "], Error=[" Lambda.Error "], ErrorGrid=[" (string Lambda.ErrorGrid true) "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (if (> y avgY) then (begin (++ topEyCnt) (+= avgTopEy Y[n])))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "svmRegress: err=[" err "], avgY=[" avgY "]")
		           )) ; end if
          )) ; end Test Case mixedRandom
       ;; Test Case randomSigmoid 
       (if (or (= Test all:) (= Test randomSigmoid:))
           (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: The inputs, X, are restricted to the sigmoid domain.
		       ;; Note2: We support a bias by having X[0] == 1 for all N.
		       ;; Note3: This algorithm seems to work well when N is at least 25 times M.
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 1) .5))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do 
		          	 (setq X[n][m] (- (srandom .999999999) 0))
		             (setq y (+ y (* X[n][m] C[m])))
		             ) ; end M loop
		          (setq Y[n] (+ (* y .8) (* y (srandom .4))))
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: randomSigmoid")
		       (setq Lambda (svmRegress.svmTraining X Y kernelID properties))
		       (if (= myVerboseSW false) (writeln "svmRegress: N = [" Ns "], M = [" Lambda.M "], Generations = [" Generations  "], Layers = [" myMaxLayers  "], SVM's = [" Lambda.N "], ETollerance=[" Lambda.ETollerance "], Error=[" Lambda.Error "], ErrorGrid=[" (string Lambda.ErrorGrid true) "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (if (> y avgY) then (begin (++ topEyCnt) (+= avgTopEy Y[n])))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "svmRegress: err=[" err "], avgY=[" avgY "]")
		           )) ; end if
          )) ; end Test Case randomSigmoid
       ;; Test Case square 
       (if (or (= Test all:) (= Test square:))
           (begin
		       ;; Create a test polynomial regression where y = -11.2 + C[0]*X[0] - C[1]*(X[1]**2) + C[2]*X[2] - C[3]*(X[3]**2) ...
		       ;; Note: We support a bias by having X[0] == 1 for all N.
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (if (isOdd m)
			             (setq y (+ y (* X[n][m] C[m])))
			             (setq y (+ y (* X[n][m] X[n][m] C[m])))
		                 ) ; end if
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: square")
		       (setq Lambda (svmRegress.svmTraining X Y kernelID properties))
		       (if (= myVerboseSW false) (writeln "svmRegress: N = [" Ns "], M = [" Lambda.M "], Generations = [" Generations  "], Layers = [" myMaxLayers  "], SVM's = [" Lambda.N "], ETollerance=[" Lambda.ETollerance "], Error=[" Lambda.Error "], ErrorGrid=[" (string Lambda.ErrorGrid true) "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (if (> y avgY) then (begin (++ topEyCnt) (+= avgTopEy Y[n])))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "svmRegress: err=[" err "], avgY=[" avgY "]")
		           )) ; end if
          )) ; end Test Case square
       ;; Test Case tan 
       (if (or (= Test all:) (= Test tan:))
           (begin
		       ;; Create a test polynomial regression where y = -11.2 + C[0]*X[0] - C[1]*(X[1]**2) + C[2]*X[2] - C[3]*(X[3]**2) ...
		       ;; Note: We support a bias by having X[0] == 1 for all N.
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (setq y (+ y (* (tan X[n][m]) C[m])))
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: tan")
		       (setq Lambda (svmRegress.svmTraining X Y kernelID properties))
		       (if (= myVerboseSW false) (writeln "svmRegress: N = [" Ns "], M = [" Lambda.M "], Generations = [" Generations  "], Layers = [" myMaxLayers  "], SVM's = [" Lambda.N "], ETollerance=[" Lambda.ETollerance "], Error=[" Lambda.Error "], ErrorGrid=[" (string Lambda.ErrorGrid true) "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (if (> y avgY) then (begin (++ topEyCnt) (+= avgTopEy Y[n])))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "svmRegress: err=[" err "], avgY=[" avgY "]")
		           )) ; end if
          )) ; end Test Case tan
       ;; Test Case log 
       (if (or (= Test all:) (= Test log:))
           (begin
		       ;; Create a test polynomial regression where y = -11.2 + C[0]*X[0] - C[1]*(X[1]**2) + C[2]*X[2] - C[3]*(X[3]**2) ...
		       ;; Note: We support a bias by having X[0] == 1 for all N.
		       (setq c Cs)
		       (setq M Ms)
		       (setq N Ns)
		       (setq X (new Vector: object: N))
		       (setq Y (new Vector: Number: N))
		       (setq C (new Vector: Number: M))
		       (setq C[0] c)
		       (loop for m from 1 until M do
		          (setq C[m] (- (srandom 100.0) 50.0))
		          ) ; end C loop
		       (loop for n from 0 until N do
		          (setq X[n] (new Vector: Number: M))
		          (setq X[n][0] 1)
		          (setq y (* C[0] X[n][0]))
		          (loop for m from 1 until M do
		          	 (setq X[n][m] (- (srandom 100.0) 50.0))
		             (setq y (+ y (* (log (+ 1.0 (abs X[n][m]))) C[m])))
		             ) ; end M loop
		          (setq Y[n] y)
		          ) ; end N loop
		       ;; Train on the test case.
		       (writeln _eol "Starting test case: log")
		       (setq Lambda (svmRegress.svmTraining X Y kernelID properties))
		       (if (= myVerboseSW false) (writeln "svmRegress: N = [" Ns "], M = [" Lambda.M "], Generations = [" Generations  "], Layers = [" myMaxLayers  "], SVM's = [" Lambda.N "], ETollerance=[" Lambda.ETollerance "], Error=[" Lambda.Error "], ErrorGrid=[" (string Lambda.ErrorGrid true) "]")) 
               (if (= checkResults true)
                   (begin
                      (setq err 0.0)
                      (setq avgTopEy 0.0)
                      (setq topEyCnt 0)
                      (setq avgY (avg Y))
		              (loop for n from 0 until N do
		                 (setq y (Lambda X[n]))
                         (if (> y avgY) then (begin (++ topEyCnt) (+= avgTopEy Y[n])))
                         (setq pct (- Y[n] y))
                         (if (<> Y[n] 0.0) (/= pct Y[n])) 
                         (if (< pct 0.0) (setq pct (- 0.0 pct)))
                         (setq pct (- pct tol))
                         (if (< pct 0.0) (setq pct 0.0))
		                 (+= err pct)
                         (if (= (modi n (divi N 10)) 0) (writeln "[" n "] ey=[" y "] y=[" Y[n] "] err=[" (- Y[n] y) "] err%=[" pct "]"))
		                 ) ; end N loop
                      (/= err N) 
		              (writeln "svmRegress: err=[" err "], avgY=[" avgY "]")
		           )) ; end if
          )) ; end Test Case log
       (writeln "svmRegress.selfTest: completed in [" (/ (setq endTimeT (getTickCount startTimeT)) 60.0) "] minutes.")       
       Lambda) ; end selfTest
    ;; *****************
    ;; Begin main logic.
    ;; ***************** 
    vars:(result properties svmLambda)
    ;; Retrieve any optional arguments.
    (setq properties (if (>= (argCount) 4) (argFetch 3) (new Structure: ETollerance: 0.00 GridErr: 0 MaxErr: 0.01 MaxGen: 1 CurrentLayer: 0 MaxLayers: 1 ModelCount: 6 OverrideSW: true UserSVSize: 100 VerboseSW: false))) 
    ;; Train a new support vector machine Lambda.
    (setq svmLambda (new (myself)))
    (setq svmLambda.mySVMParent svmLambda)
    (setq result (svmLambda.svmTraining x y kernelID properties))
    result) ; end svmRegress














