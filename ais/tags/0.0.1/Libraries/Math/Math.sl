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

;;**EXPORTKEY**:math
(defun math()
;; *******************************************************************
;;  Summary: The math Lambda is a global Lambda containing a number of 
;;           mathematical constants and functions useful in machine 
;;           learning and numeric programming. This Lambda supports a 
;;           library of basic vector and matrix mathematical functions, 
;;           including vector product, vector dot product, multiple 
;;           linear regression, matrix inversion, support vector machines, 
;;           evolutionary regression, symbolic math, and many more.
;; 
;; Args:     none.
;; Return:   true         Always returns true.
;; *******************************************************************
    pvars:(;; Public child methods
           arrayFillColumn             ;; Returns the N by M vector array with the nth column filled as specified.
           arrayRegressionGuestimate   ;; Returns the guestimated coefficients for a multivariable regression with constant term inserted.
           arrayWidrowHoffRegression   ;; Returns the coefficients from a multivariable regression with constant term inserted (Widrow Hoff iterative).
           blackScholesCollar          ;; Return the Black-Scholes theoretical profit for a ratio collar from the specified data.
           blackScholesCollarRatio     ;; Return the Black-Scholes theoretical call ratio for a debit/credit neutral ratio collar from the specified data.
           blackScholesDelta           ;; Return the Black-Scholes theoretical delta for an option from the specified data.
           blackScholesATMPrice        ;; Return the Black-Scholes theoretical price for an at-the-money option from the specified data (adjusting for put-call parity).
           blackScholesParityPrice     ;; Return the Black-Scholes theoretical price for an option from the specified data (adjusting for put-call parity).
           blackScholesPrice           ;; Return the Black-Scholes theoretical price for an option from the specified data.
           coefficientsToFormula       ;; Returns a javaFilter formula from a sparse coefficient vector and a field list.
           convertToArray              ;; Convert the input arguments to a vector array.
           convertToColumnVector       ;; Return a column vector extracted from the nth column of the input array.
           convertToMatrix             ;; Convert a rank one or two vector or a rank one matrix into a rank two matrix
           convertToMatrixC            ;; Convert a rank one or two vector or a rank one matrix into a rank two matrix and insert a constant column
           convertToArray              ;; Convert a rank one or two vector or a rank one matrix into a vector array
           copyToMatrix                ;; Copy a rank one or two vector or a rank one matrix into a rank two matrix
           correlation                 ;; Returns the correlation coefficient of the two vectors. 
           covariance                  ;; Returns the covariance of the two vectors. 
           cummNormalDensity           ;; Return the cummulative normal distribution function.
           cursorRegress               ;; Return the best fit multiple regression for the table fields specified.
           cursorToArray               ;; Returns a regression vector array from the cursor and table fields specified.
           cursorToMatrix              ;; Returns a regression matrix from the cursor and table fields specified.
           cursorToVector              ;; Returns a regression dependent variable vector from the cursor and table field specified.
           defaultRegressNet           ;; Returns an Lambda using the coefficients from a segmented multiple regression.
           egm                         ;; Creates a new evolutionary grid machine Lambda instance.
           evolveExpr                  ;; Explores the generation of field expressions using GE.
           gaussianEliminate           ;; Triangulates the M by M+1 coefficient array
           gaussianSubstitute          ;; Returns the M coefficient vector from a triangulated array
           linearRegress               ;; Returns a vector containing the coefficients resulting from a linear regression on an array. 
           makeGaussianArray           ;; Returns The M by M+1 array representing the coefficients derivatives.
           makeGaussianMatrix          ;; Returns The M by M+1 matrix representing the coefficients derivatives.
           makeGramArray               ;; Returns the N by N array of the dot product of the input vectors.
           makeGramMatrix              ;; Returns the N by N matrix of the dot product of the input vectors.
           matrixColToVector           ;; Returns The and N number vector which is the specified column of the N by M matrix input matrix.
           matrixColDotProducts        ;; Returns The and M by M matrix of all possible column dot products for the input M by N matrix.
           matrixCrossCorrelation      ;; Returns The N by (M*M) cross correlation matrix of the N by M input matrix.
           matrixDiagonal              ;; Returns the N by N diagonal matrix with the specified numbers only along the diagonal.
           matrixDualGramRegress       ;; Returns the coefficients for a multivariable regression in dual Gramm form (exact gaussian).
           matrixDualGramRegressC      ;; Returns the coefficients for a multivariable regressionwith constant term inserted in dual Gramm form (exact gaussian).
           matrixFillColumn            ;; Returns the N by M matrix with the nth column filled as specified.
           matrixFillRow               ;; Returns the N by M matrix with the nth row filled as specified.
           matrixGaussian              ;; Returns The M by M+1 matrix representing the derivatives of the coefficients for a system of linear equations.
           matrixGaussianEliminate     ;; Triangulates the M by M+1 coefficient matrix
           matrixGaussianSolve         ;; Returns the M coefficient vector solution, C, to an M by M+1 system of linear equations (in matrix notation C*X = Y).
           matrixGaussianSubstitute    ;; Returns the M coefficient vector from a triangulated matrix
           matrixIdentity              ;; Returns the N by N identity matrix with 1's only along the diagonal.
           matrixInvert                ;; Returns the inversion of the N by N input matrix.
           matrixInvertUpperTriangular ;; Returns the inversion of the N by N upper triangular input matrix.
           matrixLower                 ;; Returns the lower triangular component matrix of the N by N input matrix.
           matrixMultipleRegress       ;; Returns the coefficients from a multivariable regression (exact gaussian).
           matrixMultipleRegressC      ;; Returns the coefficients from a multivariable regression with constant term inserted (exact gaussian).
           matrixMultipleRegression    ;; Returns the coefficients from a multivariable regression (exact gaussian).
           matrixMultipleRegressionC   ;; Returns the coefficients from a multivariable regression with constant term inserted (exact gaussian).
           matrixMultiply              ;; Returns the K by N matrix product from multiplying  A times B, where A is a K by M matrix, and B is an M by N matrix.
           matrixNormalize             ;; Normalizes the column values in a matrix.
           matrixRowDotProducts        ;; Returns The and N by N matrix of all possible row dot products for the input M by N matrix.
           matrixRowToVector           ;; Returns The and M number vector which is the specified row of the N by M matrix input matrix.
           matrixTranspose             ;; Returns The N by M matrix transposition of the M by N input matrix.
           matrixTriangulate           ;; Triangulates the M by M matrix using gaussian substitution with row pivoting.
           matrixUpper                 ;; Returns the upper triangular component matrix of the N by N input matrix.
           matrixWidrowHoffRegression  ;; Returns the coefficients from a multivariable regression with constant term inserted (Widrow Hoff iterative).
           multipleRegress             ;; Returns the sparse M coefficient vector for the factors with the best least squares fits of the variables.
           multivariableRegress        ;; Returns the coefficients from a multivariable regression (exact gaussian).
           multivariableRegressC       ;; Returns the coefficients from a multivariable regression with constant term inserted (exact gaussian).
           multivariableRegressIC      ;; Returns the coefficients from a multivariable regression with constant term inserted (approximate evolutionary).
           normalizeArray              ;; Normalizes the column values in a array.
           numericRegress              ;; Returns an Lambda with numeric coefficients optimized against an objective function.
           percentileGridMachine       ;; Returns a percentile grid machine estimator Lambda.
           projectRegress              ;; Return the best fit multiple regression for the miner project strategies specified.
           projectToArray              ;; Returns a regression array from the cursor and project strategies specified.
           regress                     ;; Returns a vector containing the coefficients resulting from a linear regression. 
           regressNet                  ;; Returns an Lambda using the coefficients from a segmented multiple regression.
           regressNetBinaryRegress     ;; Returns an Lambda using the coefficients from a segmented multiple regression.
           regressNetCart2             ;; Returns an Lambda using the coefficients from a segmented multiple regression.
           regressNetCart3             ;; Returns an Lambda using the coefficients from a segmented multiple regression.
           regressNetMultiSegment      ;; Returns an Lambda using the coefficients from a segmented multiple regression.
           regressNetTwoSegment        ;; Returns an Lambda using the coefficients from a segmented multiple regression.
           regressTree                 ;; Returns an Lambda using coefficient from a tree of segmented multiple regressions.
           sigmoidizeArray             ;; Convert the column values, in a array, to sigmoid values.
           smoRegress                  ;; Trains a support vector machine SMO regression Lambda, and returns the trained Lambda.
           symbolicMath                ;; Performs symbolic mathematics on ascii input formulas
           symbolicRegress             ;; Returns an Lambda with both formula format and numeric coefficients optimized against an objective function.
           timeRegress                 ;; Returns a vector containing the coefficients resulting from a linear time regression on an array. 
           vectorAdd                   ;; Returns the vector sum of two input vectors.
           vectorDeltas     	       ;; Convert an N vector into an N-1 vector of deltas.
           vectorDeltaPercents         ;; Convert an N vector into an N-1 vector of deltas (expressed as percent differences).
           vectorDivide                ;; Returns the vector quotient of two input vectors.
           vectorDotProduct            ;; Returns the scalar dot product of two input vectors.
           vectorProduct               ;; Returns the vector product of two input vectors.
           vectorSub                   ;; Returns the vector difference of two input vectors.
           ) ;; end of persistent variables
    ;;***********************
    ;; Default methods
    ;;***********************
    (defun defaultRegressNet(w)
        (writeln _eol "Calling regressNetCart3P w -1 20 5")
        (regressNetCart3P w -1 20 5)
    )

    (defun defaultRegressTree(w m)
        (regressTree w m 4 30)

        ;; (regressNetCart3 w  m  12 5)             ;; 12 splits max, 5 samples/node min.
        ;; (regressNetCart2 w  m  12 5)             ;; 12 splits max, 5 samples/node min.
        ;; (regressNetBinaryRegress w  m  10 30)    ;; 10 splits max, 30 samples/segment min.
        ;; (regressNetMultiSegment w m 10 30)       ;; Not any different than for RegressNet
        ;; (regressNetTwoSegment w m 10 30)         ;; 10 segments max., 30 samples/segment min
        ;; (regressTree w m 4 30)
    )

    ;;***********************
    ;; Black-Scholes Functions
    ;;***********************
    ;;***********************
    ;; blackScholesCollar
    ;; Return the Black-Scholes theoretical profit for a ratio collar from the specified data.
    ;; Note: Computes the total position profit as a percent of the current stock price.
    ;;       The ratio collar position assumes the stock purchased long and is 100% put at market strike, 
    ;;       and covered calls are sold, at market strike, on 75% of stock position, leaving 25% of the stock 
    ;;       unencumbered on the upside. Both calls and puts are of the same expiration date and have the
    ;;       same strike price (the purchase price of the stock).
    ;;
    ;; Example:
    ;;         Buy 10000 shares ABC long at $100 per share on 2000/01/01. 
    ;;         BPO 100 puts at a strike of $100 expiring on 2001/01/01. 
    ;;         SCO 75 calls at a strike of $100 expiring on 2001/01/01. 
    ;;
    ;; args:   buyDaysExp       ;; Number of days to option expiration at purchase.
    ;;         sellDaysExp      ;; Number of days to option expiration at sale.
    ;;         sellStockCost    ;; Stock price at time of sale as a percent of stock purchase price (i.e. 120% or 76% etc.).
    ;;         openVolatility   ;; The annualized volatility the stock at position open.
    ;;         stockYield       ;; Annual dividend yield for stock as a percent of purchase price.
    ;;         riskFreeYield    ;; Annual risk free interest rate.
    ;;         sellCallRatio    ;; (Optional)Ratio of calls that are to be sold-to-open.
    ;;         closeVolatility  ;; (Optional)The annualized volatility for the stock at position close.
    ;; return: profit           ;; The Black-Scholes theoretical profit for the position on the sale date.
    ;;***********************
	(defun blackScholesCollar(buyDaysExp sellDaysExp sellStockCost openVolatility stockYield riskFreeYield ...)
	  vars:((buyStockCost 100%))
      vars:(openCollarCost closeCollarCost closeVolatility)
      vars:(buyPutCost sellPutCost sellCallCost buyCallCost sellCallRatio)
	  vars:(profit)
	   
	  ;; Compute open collar position cost if stock is 100% put at market and covered calls are sold on stock position enough to cover cost of puts.
      ;; Note: All costs assume that the stock originally cost $1.00 (computed as 100%).
	  (setq buyPutCost (math.blackScholesATMPrice put: buyStockCost buyDaysExp openVolatility stockYield riskFreeYield))
	  (setq sellCallCost (math.blackScholesATMPrice call: buyStockCost buyDaysExp openVolatility stockYield riskFreeYield))
	  (if (>= (argCount) 7) (setq sellCallRatio (argFetch 6)) (setq sellCallRatio #void))
      (if (= sellCallRatio #void) (setq sellCallRatio (/ buyPutCost sellCallCost)))
	  (if (>= (argCount) 8) (setq closeVolatility (argFetch 7)) (setq closeVolatility openVolatility))
	  (setq openCollarCost (+ buyStockCost buyPutCost (* -1.0 sellCallRatio sellCallCost)))
      
	   
	  ;; Compute close collar position cost if stock is [sellStockCost]% and we sell puts at market and buy back covered calls at market.
      ;; Note: All costs assume that the stock originally cost $1.00 (computed as 100%) and the sell stock price is a percent of the original $1.00 cost.
	  (setq sellPutCost (math.blackScholesATMPrice put: sellStockCost sellDaysExp closeVolatility stockYield riskFreeYield))
	  (setq buyCallCost (math.blackScholesATMPrice call: sellStockCost sellDaysExp closeVolatility stockYield riskFreeYield))
	  (setq closeCollarCost (+ sellStockCost sellPutCost (* -1.0 sellCallRatio buyCallCost)))
	   
	  ;; Compute the profit from ratio collar position.
	  (setq profit (/ (- closeCollarCost openCollarCost) openCollarCost))
	
	  profit) ; end blackScholesCollar
    ;;***********************
    ;; blackScholesCollarRatio
    ;; Return the Black-Scholes theoretical call ratio for a debit/credit neutral ratio collar from the specified data.
    ;; Note: Computes the total position profit as a percent of the current stock price.
    ;;       The ratio collar position assumes the stock purchased long and is 100% put at market strike, 
    ;;       and covered calls are sold, at market strike, on 75% of stock position, leaving 25% of the stock 
    ;;       unencumbered on the upside. Both calls and puts are of the same expiration date and have the
    ;;       same strike price (the purchase price of the stock).
    ;;
    ;; Example:
    ;;         Buy 10000 shares ABC long at $100 per share on 2000/01/01. 
    ;;         BPO 100 puts at a strike of $100 expiring on 2001/01/01. 
    ;;         SCO 75 calls at a strike of $100 expiring on 2001/01/01. 
    ;;
    ;; args:   buyDaysExp       ;; Number of days to option expiration at purchase.
    ;;         annualVol        ;; The annualized volatility for the stock at purchase.
    ;;         stockYield       ;; Annual dividend yield for stock as a percent of purchase price.
    ;;         riskFreeYield    ;; Annual risk free interest rate.
    ;; return: ratio            ;; The Black-Scholes theoretical call ratio for the position on the buy date.
    ;;***********************
	(defun blackScholesCollarRatio(buyDaysExp annualVol stockYield riskFreeYield)
	  vars:((buyStockCost 100%))
      vars:(openCollarCost closeCollarCost)
      vars:(buyPutCost sellPutCost sellCallCost buyCallCost sellCallRatio)
	   
	  ;; Compute open collar position cost if stock is 100% put at market and covered calls are sold on stock position enough to cover cost of puts.
      ;; Note: All costs assume that the stock originally cost $1.00 (computed as 100%).
	  (setq buyPutCost (math.blackScholesATMPrice put: buyStockCost buyDaysExp annualVol stockYield riskFreeYield))
	  (setq sellCallCost (math.blackScholesATMPrice call: buyStockCost buyDaysExp annualVol stockYield riskFreeYield))
	  (setq sellCallRatio (/ buyPutCost sellCallCost))
	  sellCallRatio) ; end blackScholesCollarRatio
    ;;***********************
    ;; blackScholesDelta
    ;; Return the Black-Scholes theoretical delta for an option from the specified data.
    ;; Note: Computes the option price as a percent of the current stock price,
    ;;       therefore, the stock price is considered to be 1, and the variance is the
    ;;       daily percent difference variance.
    ;;
    ;; args:   optType          ;; Option type (either "call" or "put").
    ;;         strikePct        ;; Strike price as a percent of stock price.
    ;;         daysExp          ;; Number of days until expiration.
    ;;         annualVol        ;; Annualized volatility
    ;;         stockYield       ;; Annual dividend yield for stock as a percent.
    ;;         longBondYield    ;; Annual long bond interest rate.
    ;; return: delta            ;; The Black-Scholes theoretical delta for an option from the specified data.
    ;;***********************
    (defun blackScholesDelta(optType strikePct daysExp annualVol stockYield longBondYield)
       vars:(d1 d2 vt
             dailyRiskFree dailyYield totalYield optDelta
             (stockPrice 1)
             ) ; end temporary variables
       ;; If the option is expiring today, then it is worth its equity only.
       (if (<= annualVol 0) (setq annualVol .145))
       (if (<= daysExp 0) 
           (begin
              (cond
                ((= optType "call")
                 (return 0)
                 ) ; end call case
                ((= optType "put")
                 (return 0)
                 ) ; end call case
                (else 
                 (error "blackScholesPrice: invalid option type [" optType "]")
                 ) ; end else case
                ) ; end cond
           )) ; end
       ;; Compute the dailyRiskFree rate of return
       (setq dailyRiskFree (sub1 (expt (add1 longBondYield) (/ 1 364))))      
       (setq dailyYield (sub1 (expt (add1 stockYield) (/ 1 364))))      
       (setq totalYield (sub1 (expt (add1 dailyYield) daysExp)))      
       ;; Compute the d1, d2, v, & vt interim variables of the model.
       (setq d1 (* (+ dailyRiskFree (/ (* annualVol annualVol) 2)) )(/ daysExp 364))
       (setq d1 (+ d1 (log (/ strikePct))))
       (setq vt (* annualVol (sqrt (/ daysExp 364))))
       (setq d1 (/ d1 vt))
       (setq optDelta (cummNormalDensity d1))
       ;; Return the theoretical option delta as a percent of the stock price.
       (if (= optType "put") (setq optDelta (- optDelta)))  
       optDelta) ; end blackScholesDelta
    ;;***********************
    ;; blackScholesATMPrice
    ;; Return the Black-Scholes theoretical price for an at-the-money option from the specified 
    ;;  data adjusting the option price for put-call parity.
    ;;
    ;; Note1: The option strike price is always assumed to be 100% (at-the-money)
    ;; Note2: The stock price is always assumed to be a percent of the strike price
    ;;         (i.e. 150% if the stock has risen, or 50% if the stock has fallen)
    ;;
    ;; args:   optType          ;; Option type (either "call" or "put").
    ;;         stockPrice       ;; Stock price as a percent of at-the-money strike price (i.e. 150% if the stock has risen, or 50% if the stock has fallen)
    ;;         daysExp          ;; Number of days until expiration.
    ;;         annualVol        ;; Annualized volatility
    ;;         stockYield       ;; Annual dividend yield for stock as a percent.
    ;;         moneyRate        ;; Annual risk free rate of return.
    ;; return: price            ;; The Black-Scholes theoretical price for an option as a percent of the at-the-money strike price (100%).
    ;;***********************
    (defun blackScholesATMPrice(optType stockPrice daysExp annualVol stockYield moneyRate)
       vars:(d1 d2 vt (defaultVolatility .145)
             dailyRiskFree dailyYield totalYield optPrice 
             (strikePrice 100%) strikePct 
             ) ; end temporary variables
       ;; If the option is expiring today, then it is worth its equity only.
       (if (<= daysExp 0) 
           (begin
              (cond
                ((= optType "call")
                 (return (max 0 (- stockPrice strikePrice)))
                 ) ; end call case
                ((= optType "put")
                 (return (max 0 (- strikePrice stockPrice)))
                 ) ; end call case
                (else 
                 (error "blackScholesATMPrice: invalid option type [" optType "]")
                 ) ; end else case
                ) ; end cond
           )) ; end
       ;; If the daily variance is less than zero, then set it to a nominal default value.
       (if (<= annualVol 0) (setq annualVol defaultVolatility))
       ;; Compute the strike percent as a percent of the current stock price.
       (setq strikePct (/ stockPrice))
       ;; Compute the dailyRiskFree rate of return
       (setq dailyRiskFree (sub1 (expt (add1 moneyRate) (/ 1 364))))      
       (setq dailyYield (sub1 (expt (add1 stockYield) (/ 1 364))))      
       (setq totalYield (sub1 (expt (add1 dailyYield) daysExp)))      
       ;; Compute the d1, d2, v, & vt interim variables of the model.
       (setq d1 (* (+ dailyRiskFree (/ (* annualVol annualVol) 2)) (/ daysExp 364)))
       ;(setq d1 (+ d1 (log (/ strikePct))))
       (setq d1 (+ d1 (log stockPrice)))
       (setq vt (* annualVol (sqrt (/ daysExp 364))))
       (setq d1 (/ d1 vt))
       (setq d2 (- d1 vt))
       ;; Compute the theoretical base Black-Scholes price variable of the model.
       (setq optPrice (exp (- (* dailyRiskFree daysExp)))) 
       (setq optPrice (* strikePct optPrice (cummNormalDensity d2))) 
       (setq optPrice (- (cummNormalDensity d1) optPrice)) 
       ;; Adjust all put prices for in-the-money equity.
       (if (= optType "put")  
           (begin
             ;; Return the theoretical put price as a percent of the stock price.
             (setq optPrice (sub1 (+ optPrice strikePct totalYield)))
             ;; Adjust Put prices for in-the-money equity.
             (if (< stockPrice 100%) (*= optPrice stockPrice))
           )) ; end if  
       ;; Adjust all call prices for put-call parity and in-the-money equity.
       (if (= optType "call") 
           (begin
             ;; Call prices for put-call parity.
             (if (< strikePct 1.0)
                 (+= optPrice (* (/ daysExp 365.0) strikePct (- moneyRate stockYield)))
                 (+= optPrice (* (/ daysExp 365.0) (/ 1.0 strikePct) (- moneyRate stockYield)))
                 ) ; end if
             ;; Adjust Call prices for in-the-money equity.
             (if (> stockPrice 100%) (*= optPrice stockPrice))
           )) ; end if  
       optPrice) ; end blackScholesATMPrice
    ;;***********************
    ;; blackScholesParityPrice
    ;; Return the Black-Scholes theoretical price for an option from the specified data
    ;;  adjusting the price for put-call parity.
    ;; Note: Computes the option price as a percent of the current stock price,
    ;;       therefore, the stock price is considered to be 1, and the variance is the
    ;;       daily percent difference variance.
    ;;
    ;; args:   optType          ;; Option type (either "call" or "put").
    ;;         strikePct        ;; Strike price as a percent of stock price.
    ;;         daysExp          ;; Number of days until expiration.
    ;;         annualVol        ;; Annualized volatility
    ;;         stockYield       ;; Annual dividend yield for stock as a percent.
    ;;         longBondYield    ;; Annual long bond interest rate.
    ;; return: price            ;; The Black-Scholes theoretical price for an option from the specified data.
    ;;***********************
    (defun blackScholesParityPrice(optType strikePct daysExp annualVol stockYield longBondYield)
       vars:(d1 d2 vt
             dailyRiskFree dailyYield totalYield optPrice 
             (stockPrice 1)
             ) ; end temporary variables
       ;; If the option is expiring today, then it is worth its equity only.
       (if (<= annualVol 0) (setq annualVol .145))
       (if (<= daysExp 0) 
           (begin
              (cond
                ((= optType "call")
                 (return (max 0 (- 1 strikePct)))
                 ) ; end call case
                ((= optType "put")
                 (return (max 0 (- strikePct 1)))
                 ) ; end call case
                (else 
                 (error "blackScholesPrice: invalid option type [" optType "]")
                 ) ; end else case
                ) ; end cond
           )) ; end
       ;; Compute the dailyRiskFree rate of return
       (setq dailyRiskFree (sub1 (expt (add1 longBondYield) (/ 1 364))))      
       (setq dailyYield (sub1 (expt (add1 stockYield) (/ 1 364))))      
       (setq totalYield (sub1 (expt (add1 dailyYield) daysExp)))      
       ;; Compute the d1, d2, v, & vt interim variables of the model.
       (setq d1 (* (+ dailyRiskFree (/ (* annualVol annualVol) 2)) (/ daysExp 364)))
       (setq d1 (+ d1 (log (/ strikePct))))
       (setq vt (* annualVol (sqrt (/ daysExp 364))))
       (setq d1 (/ d1 vt))
       (setq d2 (- d1 vt))
       ;; Compute the theoretical call price variable of the model.
       (setq optPrice (exp (- (* dailyRiskFree daysExp)))) 
       (setq optPrice (* strikePct optPrice (cummNormalDensity d2))) 
       (setq optPrice (- (cummNormalDensity d1) optPrice)) 
       ;; Return the theoretical option price as a percent of the stock price.
       (if (= optType "put") (setq optPrice (sub1 (+ optPrice strikePct totalYield))))  
       ;; Adjust all call prices for put-call parity.
       (if (= optType "call") 
           (begin
             (if (< strikePct 1.0)
                 (+= optPrice (* (/ daysExp 365.0) strikePct (- longBondYield stockYield)))
                 (+= optPrice (* (/ daysExp 365.0) (/ 1.0 strikePct) (- longBondYield stockYield)))
                 ) ; end if
           )) ; end if  
       optPrice) ; end blackScholesParityPrice
    ;;***********************
    ;; blackScholesPrice
    ;; Return the Black-Scholes theoretical price for an option from the specified data.
    ;; Note: Computes the option price as a percent of the current stock price,
    ;;       therefore, the stock price is considered to be 1, and the variance is the
    ;;       daily percent difference variance.
    ;;
    ;; args:   optType          ;; Option type (either "call" or "put").
    ;;         strikePct        ;; Strike price as a percent of stock price.
    ;;         daysExp          ;; Number of days until expiration.
    ;;         annualVol        ;; Annualized Volatility
    ;;         stockYield       ;; Annual dividend yield for stock as a percent.
    ;;         longBondYield    ;; Annual long bond interest rate.
    ;; return: price            ;; The Black-Scholes theoretical price for an option from the specified data.
    ;;***********************
    (defun blackScholesPrice(optType strikePct daysExp annualVol stockYield longBondYield)
       vars:(d1 d2 vt
             dailyRiskFree dailyYield totalYield optPrice 
             (stockPrice 1)
             ) ; end temporary variables
       ;; If the option is expiring today, then it is worth its equity only.
       (if (<= annualVol 0) (setq annualVol .145))
       (if (<= daysExp 0) 
           (begin
              (cond
                ((= optType "call")
                 (return (max 0 (- 1 strikePct)))
                 ) ; end call case
                ((= optType "put")
                 (return (max 0 (- strikePct 1)))
                 ) ; end call case
                (else 
                 (error "blackScholesPrice: invalid option type [" optType "]")
                 ) ; end else case
                ) ; end cond
           )) ; end
       ;; Compute the dailyRiskFree rate of return
       (setq dailyRiskFree (sub1 (expt (add1 longBondYield) (/ 1 364))))      
       (setq dailyYield (sub1 (expt (add1 stockYield) (/ 1 364))))      
       (setq totalYield (sub1 (expt (add1 dailyYield) daysExp)))      
       ;; Compute the d1, d2, v, & vt interim variables of the model.
       (setq d1 (* (+ dailyRiskFree (/ (* annualVol annualVol) 2)) (/ daysExp 364)))
       (setq d1 (+ d1 (log (/ strikePct))))
       (setq vt (* annualVol (sqrt (/ daysExp 364))))
       (setq d1 (/ d1 vt))
       (setq d2 (- d1 vt))
       ;; Compute the theoretical call price variable of the model.
       (setq optPrice (exp (- (* dailyRiskFree daysExp)))) 
       (setq optPrice (* strikePct optPrice (cummNormalDensity d2))) 
       (setq optPrice (- (cummNormalDensity d1) optPrice)) 
       ;; Return the theoretical option price as a percent of the stock price.
       (if (= optType "put") (setq optPrice (sub1 (+ optPrice strikePct totalYield))))  
       optPrice) ; end blackScholesPrice
    ;;***********************
    ;; cummNormalDensity
    ;; Return the cummulative normal distribution function.
    ;;***********************
    (defun cummNormalDensity(d)
       vars:(x y z (rf 10000))
       ;; Compute the value of y.
       (setq y (/ (add1 (* .2316419 (abs d)))))
       ;; Compute the value of z.
       (setq z (* .3989423 (exp (/ (* d d) -2))))      
       ;; Compute the value of x.
       (setq x (* 1.330274 (expt y 5)))
       (setq x (- x (* 1.821256 (expt y 4))))
       (setq x (+ x (* 1.781478 (expt y 3))))
       (setq x (- x (* .356538 (expt y 2))))
       (setq x (+ x (* .3193815 y)))
       (setq x (* z x ))
       (if (isPositive d) (setq x (- 1 x)))      
       ;; Return the cummulative normal distribution function.
       (setq x (/ (round (* x rf)) rf))     
       x) ; end cummNormalDensity
    ;;***********************
    ;; matrixColToVector
    ;; Returns The and N number vector which is the specified column of the N by M matrix input matrix.
    ;;***********************
    (defun matrixColToVector(X c N)
       vars:(n col)
       ;; Create the column number vector to be returned.
       (setq col (new Vector: number: N))
       (loop for n from 0 until N do
       	  (setq col[n] X[n c])
       	  ) ; end col loop      
       col) ; end matrixColToVector
    ;;***********************
    ;; matrixRowToVector
    ;; Returns The and M number vector which is the specified row of the N by M matrix input matrix.
    ;;***********************
    (defun matrixRowToVector(X r M)
       vars:(m row)
       ;; Create the row number vector to be returned.
       (setq row (new Vector: number: M))
       (loop for m from 0 until M do
       	  (setq row[m] X[r m])
       	  ) ; end row loop      
       row) ; end matrixRowToVector
    ;;***********************
    ;; Math object properties
    ;;***********************
    (define E (exp 1))             ;; Base of natural logarithms (2.718...)
    (define LN2 (log 2))           ;; Natural logarithm of 2 (0.693...)
    (define LN10 (log 10))         ;; Natural logarithm of 10 (2.302...)
    (define LOG2E (log2 E))        ;; Base 2 logarithm of e (1.442...)          
    (define LOG10E (log10 E))      ;; Base 10 logarithm of e (0.434...)      
    (define PI (pi))               ;; Ratio of circumference to diameter (3.141...)
    (define SQRT1_2 (sqrt .5))     ;; Square root of one half (0.707...)       
    (define SQRT2 (sqrt 2))        ;; Square root of two (1.414...)         
    ;;***********************
    ;; Math object methods
    ;;***********************
    (define abs ^abs)              ;; Returns absolute value of a single argument.
    (define acos ^acos)            ;; Returns arc cosine of a single argument.
    (define asin ^asin)            ;; Returns arc sine of a single argument.
    (define atan ^atan)            ;; Returns arc tangent of a single argument.
    (defun atan2(n m)              ;; Returns angle of polar coordinate for two arguments.
        (error "atan2 not implemented yet"))
    (defun ceil(n)                 ;; Rounds up to next integer for one argument.
        (round (+ n .5)))
    (define cos ^cos)              ;; Returns cosine of a single argument.
    (define exp ^exp)              ;; Returns e raised to the power of a single argument.
    (define floor ^round)          ;; Rounds down to next integer for one argument.
    (define log ^log)              ;; Returns the natural logarithm for one argument.
    (define max ^max)              ;; Returns the maximum of two arguments.
    (define min ^min)              ;; Returns the minimum of two arguments.
    (define pow ^expt)             ;; Returns the power of two arguments.
    (defun random() (^random 1))   ;; Returns a random number between zero and one.
    (define round ^round)          ;; Rounds to the closest integer.
    (define sin ^sin)              ;; Returns sine of a single argument.
    (define sqrt ^sqrt)            ;; Returns square root of a single argument.
    (define tan ^tan)              ;; Returns the tangent of a single argument.
    ;; *******************************************************************
    ;;  Define main initialization code.
    ;; *******************************************************************
    (symbolicMath)
    true) ;; end math

;; *******************************************************************
;;  Define Global Functions.
;; *******************************************************************
 
(defun between(target,low,high) (and (>= target low) (<= target high)))

;; *******************************************************************
;;  Define Global Macros.
;; *******************************************************************

(define BIGNEGNUM -1.797693134862E+308)   ;; Largest possible negative number         
(define BIGPOSNUM  1.797693134862E+308)   ;; Largest possible positive number         
(define LOWPOSNUM  2.680137958338E-308)   ;; Smallest possible positive number         
(define LOWNEGNUM  -2.680137958338E-308)  ;; Smallest possible negativenumber         
(defmacro setint(y x) (list 'begin (list vmndivr: 1.0 x y) (list vmnsub: y x y)))
(defmacro setfrc(y x) (list vmndivr: 1.0 x y))
(defun pdiv(x y) (if (= y 0) x (/ x y)))
(defun pmod(x y) (if (= y 0) x (mod x y)))
(defun ninteger(x) (- x (fraction x)))
(defmacro numchk(x) (list (symbol "if") (list |<|: 'BIGPOSNUM x) 'BIGPOSNUM (list (symbol "if") (list |>|: 'BIGNEGNUM x) 'BIGNEGNUM x)))
(defmacro numCheck(x) (list (symbol "if") 
                            (list |<|: 'BIGPOSNUM x) 
                                'BIGPOSNUM 
                                (list (symbol "if") 
                                      (list |>|: 'BIGNEGNUM x) 
                                           'BIGNEGNUM 
			                               (list (symbol "if")
			                                     (list and: 
			                                           (list |<|: 'LOWNEGNUM x) 
			                                           (list |>|: 'LOWPOSNUM x)) 
			                                           0.0 
			                                           x)))
         ) ; end numCheck
(defmacro validate(x) (list (symbol "if") 
                            (list |<|: 'BIGPOSNUM x) 
                                (list error: "InvalidNumber") 
                                (list (symbol "if") 
                                      (list |>|: 'BIGNEGNUM x) 
                                           (list error: "InvalidNumber") 
			                               (list (symbol "if")
			                                     (list and: 
			                                           (list |<|: 'LOWNEGNUM x) 
			                                           (list |>|: 'LOWPOSNUM x)) 
			                                           0.0 
			                                           x)))
         ) ; end validate













;;**EXPORTKEY**:math:arrayFillColumn
(defriend math:arrayFillColumn(w i x)
;; *******************************************************************
;; name:     arrayFillColumn
;; 
;; summary:  Destructively fills the ith column vector with the input value.
;; Parms:    w:       An M by M array.
;;           i:       The ith column vector to fill.
;;           x:       The value to fill (a vector or a number).
;; Return:   w:       The filled input array.
;; *******************************************************************
    vars:(n N)
    (setq N (length w))
    ;; Fill the column with the proper values.
    (cond
       ;; Fill the column with a number.
       ((isNumber x)
        (loop for n from 0 until N do (setq w[n][i] x))
        ) ; end number
       ;; Fill the column with a vector.
       (else
        (loop for n from 0 until N do (setq w[n][i] x[n]))
        ) ; end vector
       ) ; end cond
    w) ; end arrayFillColumn



















































;;**EXPORTKEY**:math:arrayRegressionGuestimate
(defriend math:arrayRegressionGuestimate(W ...)
;; *******************************************************************
;; name:     arrayRegressionGuestimate
;; 
;; summary:  The arrayRegressionGuestimate returns a vector containing 
;;           the guestimated coefficients for a multivariable regression. 
;;           The guestimated coefficients result from averaging a series 
;;           of simple linear regressions on pairs of variables, each 
;;           independent variable paired with the dependent variable.
;;
;; Parms:    W:       The N by M+1 array representing the original observations
;;                    in the form of:    x x x ...  y
;;                                       x x x ...  y
;;                                           ... 
;;                                       x x x ...  y
;;           Y:       (Optional) The N vector containing the dependent values.
;; Return:   C:       The M+1 coefficient vector with the constant inserted 
;;                    in first position.
;; *******************************************************************
	vars:(m n N M M+1 C vt xmean ymean numerator denominator x Y)
	;; Collect and validate the arguments
	(setq W (convertToArray W))
	(if (= (argCount) 2) (setq Y (argFetch 1)))
	;; Initialize and perform startup activities
	(setq N (length W))
	(if (= Y #void)
	    then
	    (begin
			(setq M+1 (length W[0]))
			(setq M (sub1 M+1))
			(setq Y (new Vector: number: N))
		    (loop for n from 0 until N do
		       (setq Y[n] W[n][M])    
		       ) ; end n loop
        )
        else
	    (begin
			(setq M (length W[0]))
			(setq M+1 (add1 M))
        )) ; end if
   	(setq ymean (avg Y))
	(setq x (new Vector: number: N))
	(setq C (new Vector: number: M+1))
	;; Find guestimates for each of the M coefficients and the constant.
	;; Note: We select the guestimates by averaging the linear regression
	;;       coefficients for each pair of variables.
    (loop for m from 0 until M do    
        (loop for n from 0 until N do
		   (setq x[n] W[n][m])
		   ) ; end n loop
	   	(setq xmean (avg x))
	   	(setq numerator (vectorDotProduct (setq vt (vectorSub x xmean)) Y)) 
	   	(setq denominator (vectorDotProduct vt vt))
	   	(if (<> denominator 0) (setq C[(add1 m)] (/ numerator denominator M)))
	   	(setq C[0] (+ C[0] (/ (- ymean (* C[1] xmean)) M)))
   	    ) ; end m loop
	C) ; arrayRegressionGuestimate















;;**EXPORTKEY**:math:arrayWidrowHoffRegression
(defriend math:arrayWidrowHoffRegression(X Y Gmax err ...)
;; *******************************************************************
;; summary:  Returns the dense M coefficient vector giving the coefficients
;;           with the best least squares fit of the variables with a constant
;;           term inserted in the model. The method used is the Widrow-Hoff
;;           iterative approximation.
;; Parms:    X:        The N by M array representing the original observations
;;                     of the independent variables.
;;           Y:        The N vector representing the original observations
;;                     of the dependent variable.
;;           Gmax:     The maximum number of optimization trials (generations) to attempt before
;;                     returning the best set of coefficients available at that time.
;;           err:      A minimum error value which would terminate further optimization 
;;                     trials and return the best set of coefficients at that time. This
;;                     minimum error is expressed as the absolute value of the average error.
;;           RfSW:     (Optional)If present and true, return a linear regression Lambda, Rf,
;;                     with coefficient vector, Rf.C, and Rf.Error set to the proper values.
;;           printSW:  (Optional)If present and true, display each regression interation on the console
;; Return:   C:        The M+2 coefficient vector (with M+1th = error), 
;;                     AND the 0th term being an inserted constant.
;; Note:     See Cristianini, "Support Vector Machines", page 23.
;; *******************************************************************
    vars:(C B m M n N oldC oldB oldError currentError 
          dotProduct (learningRate .025) generationCount 
          Rf RfSW printSW y ey stdErr avgErr minErr maxErr avgY)
    ;; Convert the input into the proper form.
    (setq X (convertToArray X))
    (if (or (not (isVector X)) (not (isVector Y)) (<= (setq M (length X[0])) 0) (<> (setq N (length X)) (length Y))) (error "math.arrayWidrowHoffRegression: invalid input arguments"))
    (if (>= (argCount) 5) (setq RfSW (argFetch 4)) (setq RfSW false))
    (if (>= (argCount) 6) (setq printSW (argFetch 5)) (setq printSW false))
    ;; Initialize the coefficient vector and the constant.
    ;; Note: Make initial guesses using the average of the column vectors.
    (if (= printSW true) (writeln ""))
    ;; Set the initial coefficient estimates.
    (setq avgY (avg Y))
    (setq C (arrayRegressionGuestimate W Y)) 
    (setq B C[0])
    (setq C (delete C 0))
    (setq currentError BIGPOSNUM)
    (setq learningRate (/ learningRate N))
    ;; Perform a single iterative pass through the observation matrix, X.
    NextGeneration::
    (++ generationCount)
    (setq oldB B)
    (setq oldC (copy C))
    (setq oldError currentError)
    (setq currentError 0)
    (setq stdErr 0)
    (setq avgErr 0)
    (setq minErr BIGPOSNUM)
    (setq maxErr 0)
    (loop for n from 0 until N do 
       ;; Compute the dot product of the coefficient vector, C, and the observation vector X[n].
       (setq dotProduct 0)
       (loop for m from 0 until M do
          (setq dotProduct (+ dotProduct (* C[m] X[n][m])))
          ) ; end m loop
       ;; Compute the estimate error and the sum of the squared estimate errors.
       (setq ey (- (+ B dotProduct) Y[n]))
       (setq currentError (setq avgErr (+ avgErr (/ (abs ey) N))))
       (setq stdErr (+ stdErr (/ (* ey ey) N)))
       (setq minErr (min minErr (abs ey)))
       (setq maxErr (max maxErr (abs ey)))
       ;; Adjust the coefficient vector, C, and the constant, B, using the learning rate.
       (loop for m from 0 until M do
          (setq C[m] (- C[m] (* learningRate ey X[n][m])))
          ) ; end m loop
       (setq B (- B (* learningRate ey)))
       ) ; end n loop
    (if (= printSW true) (writeln "G=[" (integer generationCount) ",E=[" currentError "],L=[" learningRate "],B=[" B "],C=(" (mid (string C true) 6 100000) ")"))
    ;; Adjust the learning rate (if necessary).
    ;; Note: Constant adjustment of the learning rate is an extremely important
    ;;       heuristic in order to obtain fast convergence for this algorithm.  
    (cond
      ((> (abs currentError) BIGPOSNUM) 
       (begin 
          (setq learningRate (* learningRate .1)) 
	      (setq B oldB)
	      (setq C oldC)
	      (setq currentError oldError)
       )) ; end case
      ((= oldError currentError) (setq learningRate (* learningRate 1.1)))
      ((< oldError currentError)
       (begin
          (setq learningRate (* learningRate .9))
		  (setq B (+ oldB (* .5 (- B oldB))))
          (loop for m from 0 until M do
		     (setq C[m] (+ oldC[m] (* .5 (- C[m] oldC[m]))))
             ) ; end M loop 
		  (setq currentError (+ oldError (* .5 (- currentError oldError))))
          (if (= printSW true) (writeln "G=[" (integer generationCount) ",E=[" currentError "],L=[" learningRate "],B=[" B "],C=(" (mid (string C true) 6 100000) ")"))
       )) ; end case
      ) ; end cond
    ;; Try another generation of training (if necessary). 
    (if (and (< generationCount Gmax) (> currentError err)) (goto NextGeneration:))
    ;; If requested, return a linear regression Lambda, Rf, with
    ;; coefficient vector, Rf.C, and Rf.Error set accordingly.
    (if (= RfSW true)
        (begin
            (setq Rf (math.numericRegress.makeLinearEvolve M))
		    (setq Rf.C (insert C 0 B))
		    (setq Rf.Error stdErr)
		    (setq Rf.AvgErr avgErr)
		    (setq Rf.MinErr minErr)
		    (setq Rf.MaxErr maxErr)
		    (setq Rf.AvgY avgY)
		    (setq Rf.G generationCount)
		    (setq Rf.P 1)
		    (return Rf)
		)); end if
    ;; Return the coefficient vector with the error in the M+1st slot,
    ;; and the constant, B, in the zeroth slot.
    (setq C (insert C 0 B))
    (setq C[(length C)] currentError)
    C) ; end arrayWidrowHoffRegression








































;;**EXPORTKEY**:math:coefficientsToFormula
(defriend math:coefficientsToFormula(coefficients fieldList)
;; *******************************************************************
;; summary:  Returns a javaFilter formula from the sparse M coefficient 
;;           vector and a field list. 
;;           
;; Parms:    coefficients:  The M+1 coefficient vector (0 = constant term, and e = M+1 term).
;;           fieldList:     The field list with which the formula will be constructed.
;; Return:   formula:       The javaFilter formula representing the regression.
;; *******************************************************************
    vars:(m M formula)
    ;; Extract the lengths of the coefficients and field list.
    (setq N (length cursor.rowVector))
    (setq M+1 (length fieldList))
    (setq M (subi M+1 1))
    ;; Initialize the formula with the axis constant.
    (setq formula (string coefficients[0]))
    ;; Build the formula term by term (zero coefficient terms are ignored).
    (loop for m from 1 until M+1 do
       (if (<> coefficients[m] 0)
           (setq formula (append formula " + (" coefficients[m] " * " fieldList[(subi m 1)] ")"))
           ) ; end if
       ) ; end M loop
    ;; Return the regression coefficient vector.
    formula) ; end coefficientsToFormula







































































;;**EXPORTKEY**:math:convertToArray
(defriend math:convertToArray(X ...)
;; *******************************************************************
;; name:     convertToArray
;; 
;; summary:  Convert the input arguments to a vector array. A rank one
;;           vector or matrix is converted into a rank two column
;;           vector array. A rank two matrix is converted into a rank
;;           two vector array. If the optional dependent variable vector
;;           Y is present, a rank two XY vector array is always returned.         
;;
;;       	Note1: A math Lambda vector array must be an object vector of
;;                  length N, each element of which is a number vector of
;;                  length N.
;;           Note2: If the argument is not a rank one or rank two vector or
;;                  a rank one matrix, no conversion is performed.
;;
;; Parms:    X:       The rank one or rank two vector or matrix to be converted.
;;           Y:       (Optional) A vector of dependent variable values.
;; Return:   X:       The converted vector array
;; *******************************************************************
    vars:(k K m M n N AB T sum rankA Y Ny)
    ;; Capture the vecor of dependent values (if present).
    (if (= (argCount) 2) (setq Y (argFetch 1)))
    (setq Ny (if (isVector Y) 1 0))
    ;; Perform conversion to a vector array (if necessary).
    (cond    
     ;; Perform conversion of X from a vector to a vector array (if necessary).
     ((or (and (isVector X) (= (length X[0]) 0)) (and (isMatrix X) (= (length (rank X)) 1)))
        (begin
           (setq N (length X))
           (setq T (new Vector: object: 1 (new Vector: number: N)))
           (loop for n from 0 until N do
             (setq T[0][n] X[n])
             ) ; end loop
           (if (= Ny 1) (setq T[0][n] Y[0]))
           (setq X T)
        )) ; end case
     ;; Perform conversion of X from a vector to a vector array (if necessary).
     ((and (isVector X) (>= (setq M (length X[0])) 0) (isVector Y))
        (begin
           (setq N (length X))
           (setq T (new Vector: object: N))
           (loop for n from 0 until N do
             (setq T[n] (copy X[n]))
             (if (= Ny 1) (setq T[n][M] Y[n]))
             ) ; end loop
           (setq X T)
        )) ; end case
     ;; Perform conversion of X from a vector array to a matrix (if necessary).
     ((and (isMatrix X) (= (length (rank X)) 2))
        (begin
           (setq rankA (rank X))
           (setq N rankA[0])
           (setq M rankA[1])
           (setq T (new Vector: object: N))
           (loop for n from 0 until N do
              (setq T[n] (new Vector: number: (addi M Ny)))
              (loop for m from 0 until M do
                (setq T[n][m] X[n m])
                ) ; end m loop
              (if (= Ny 1) (setq T[n][M] Y[n]))
              ) ; end n loop
           (setq X T)
        )) ; end case
      ) ; end conversion cond
    X) ; end convertToArray















;;**EXPORTKEY**:math:convertToColumnVector
(defchild math:convertToColumnVector(w i)
;; *******************************************************************
;; summary:  Returns the vector extracted from the ith column of the
;;           input array: #(w[0][i] w[1][i] .... w[M][i])
;; Parms:    w:       An M by N vector array.
;;           i:       The ith column from which to extract the vector.
;; Return:   v:       The ith column vector of length M.
;; *******************************************************************
    vars:(j m n v)
    (setq m (length w))
    (setq v (new Vector: m))
    (loop for j from 0 until m do
            (setq v[j] w[j][i]))
    v) ; end convertToColumnVector












































































;;**EXPORTKEY**:math:convertToMatrix
(defriend math:convertToMatrix(X ...)
;; *******************************************************************
;; name:     convertToMatrixC
;; 
;; summary:  Convert a rank one or two vector or a rank one matrix
;;           into a rank two matrix.
;;
;; Parms:    X:       The rank one or rank two vector
;;           Y:       (Optional) A vector of dependent variable values.
;; Return:   T:       The proper rank two number matrix.
;;
;; Note:     If the argument is not a rank one or rank two vector or
;;           a rank one matrix, no conversion is performed.
;; *******************************************************************
    vars:(k K m M n N N+1 AB T sum rankA rankB Y)
    ;; Capture the vecor of dependent values (if present).
    (if (= (argCount) 2) (setq Y (argFetch 1)) (if (isMatrix X) (return X)))
    ;; Perform conversion to a matrix (if necessary).
    (cond    
     ;; Perform conversion of X from a vector to a column matrix while inserting Y (if necessary).
     ((or (and (isVector X) (isVector Y) (= (length X[0]) 0)) (and  (isVector Y) (isMatrix X) (= (length (rank X)) 1)))
        (begin
           (setq N (length X))
           (if (<> N (length Y)) (error "math.convertToMatrix: optional Y vector wrong length"))
           (setq T (new Matrix: number: 2 N 2))
           (loop for n from 0 until N do
             (setq T[n 0] X[n])
             (setq T[n 1] Y[n])
             ) ; end loop
           (setq X T)
        )) ; end case
     ;; Perform conversion of X from a vector to a column matrix (if necessary).
     ((or (and (isVector X) (= (length X[0]) 0)) (and (isMatrix X) (= (length (rank X)) 1)))
        (begin
           (setq N (length X))
           (setq T (new Matrix: number: 2 N 1))
           (loop for n from 0 until N do
             (setq T[n 0] X[n])
             ) ; end loop
           (setq X T)
        )) ; end case
     ;; Perform conversion of X from a vector array to a matrix while inserting Y (if necessary).
     ((and (isVector X) (isVector Y) (> (length X[0]) 0) (isNumber X[0][0]))
        (begin
           (setq M (length X))
           (setq N (length X[0]))
           (setq N+1 (addi N 1))
           (if (<> M (length Y)) (error "math.convertToMatrix: optional Y vector wrong length"))
           (setq T (new Matrix: number: 2 M N+1))
           (loop for m from 0 until M do
              (loop for n from 0 until N do
                (setq T[m n] X[m][n])
                ) ; end n loop
              (setq T[m n] Y[m])
              ) ; end m loop
           (setq X T)
        )) ; end case
     ;; Perform conversion of X from a vector array to a matrix (if necessary).
     ((and (isVector X) (> (length X[0]) 0) (isNumber X[0][0]))
        (begin
           (setq M (length X))
           (setq N (length X[0]))
           (setq T (new Matrix: number: 2 M N))
           (loop for m from 0 until M do
              (loop for n from 0 until N do
                (setq T[m n] X[m][n])
                ) ; end n loop
              ) ; end m loop
           (setq X T)
        )) ; end case
     ;; Perform conversion of X from a matrix to a matrix with Y inserted (if necessary).
     ((and (isMatrix X) (isVector Y))
        (begin
           (setq rankA (rank X))
           (setq M rankA[0])
           (setq N rankA[1])
           (setq N+1 (addi N 1))
           (if (<> M (length Y)) (error "math.convertToMatrix: optional Y vector wrong length"))
           (setq T (new Matrix: number: 2 M N+1))
           (loop for m from 0 until M do
              (loop for n from 0 until N do
                (setq T[m n] X[m n])
                ) ; end n loop
              (setq T[m n] Y[m])
              ) ; end m loop
           (setq X T)
        )) ; end case
      ) ; end conversion cond
    X) ; end convertToMatrix












































































;;**EXPORTKEY**:math:convertToMatrixC
(defriend math:convertToMatrixC(X ...)
;; *******************************************************************
;; name:     convertToMatrixC
;; 
;; summary:  Convert a rank one or two vector or a rank one matrix
;;           into a rank two matrix and with a constant column 
;;           inserted before the first column.
;;
;; Parms:    X:       The rank one or rank two vector
;;           Y:       (Optional) A vector of dependent variable values.
;; Return:   T:       The proper rank two number matrix with a constant
;;                    column inserted.
;;
;; Note:     If the argument is not a rank one or rank two vector or
;;           a rank one matrix, no conversion is performed.
;; *******************************************************************
    vars:(k K m M n N N+1 AB T sum rankA rankB Y)
    ;; Capture the vecor of dependent values (if present).
    (if (= (argCount) 2) (setq Y (argFetch 1)))
    ;; Perform conversion to a row matrix (if necessary).
    (cond    
     ;; Perform conversion of X from a vector to a column matrix while inserting Y (if necessary).
     ((or (and (isVector X) (isVector Y) (= (length X[0]) 0)) (and  (isVector Y) (isMatrix X) (= (length (rank X)) 1)))
        (begin
           (setq N (length X))
           (if (<> N (length Y)) (error "math.convertToMatrix: optional Y vector wrong length"))
           (setq T (new Matrix: number: 2 N 3))
           (loop for n from 0 until N do
             (setq T[n 0] 1)
             (setq T[n 1] X[n])
             (setq T[n 2] Y[n])
             ) ; end loop
           (setq X T)
        )) ; end case
     ;; Perform conversion of X from a vector to a column matrix (if necessary).
     ((or (and (isVector X) (= (length X[0]) 0)) (and (isMatrix X) (= (length (rank X)) 1)))
        (begin
           (setq N (length X))
           (setq T (new Matrix: number: 2 N 2))
           (loop for n from 0 until N do
             (setq T[n 0] 1)
             (setq T[n 1] X[n])
             ) ; end loop
           (setq X T)
        )) ; end case
     ;; Perform conversion of X from a vector array to a matrix while inserting Y (if necessary).
     ((and (isVector X) (isVector Y) (> (length X[0]) 0) (isNumber X[0][0]))
        (begin
           (setq M (length X))
           (setq N (add1 (length X[0])))
           (setq N+1 (addi N 1))
           (if (<> M (length Y)) (error "math.convertToMatrix: optional Y vector wrong length"))
           (setq T (new Matrix: number: 2 M N+1))
           (loop for m from 0 until M do
              (setq T[m 0] 1)
              (loop for n from 1 until N do
                (setq T[m n] X[m][(sub1 n)])
                ) ; end n loop
              (setq T[m n] Y[m])
              ) ; end m loop
           (setq X T)
        )) ; end case
     ;; Perform conversion of X from a vector array to a matrix (if necessary).
     ((and (isVector X) (> (length X[0]) 0) (isNumber X[0][0]))
        (begin
           (setq M (length X))
           (setq N (add1 (length X[0])))
           (setq T (new Matrix: number: 2 M N))
           (loop for m from 0 until M do
              (setq T[m 0] 1)
              (loop for n from 1 until N do
                (setq T[m n] X[m][(sub1 n)])
                ) ; end n loop
              ) ; end m loop
           (setq X T)
        )) ; end case
     ;; Perform conversion of X from a matrix to a matrix with Y inserted (if necessary).
     ((and (isMatrix X) (isVector Y))
        (begin
           (setq rankA (rank X))
           (setq M rankA[0])
           (setq N (add1 rankA[1]))
           (setq N+1 (addi N 1))
           (if (<> M (length Y)) (error "math.convertToMatrix: optional Y vector wrong length"))
           (setq T (new Matrix: number: 2 M N+1))
           (loop for m from 0 until M do
              (setq T[m 0] 1)
              (loop for n from 1 until N do
                (setq T[m n] X[m (sub1 n)])
                ) ; end n loop
              (setq T[m n] Y[m])
              ) ; end m loop
           (setq X T)
        )) ; end case
     ;; Perform conversion of X from a matrix to a matrix with constant inserted.
     ((isMatrix X)
        (begin
           (setq rankA (rank X))
           (setq M rankA[0])
           (setq N (add1 rankA[1]))
           (setq T (new Matrix: number: 2 M N))
           (loop for m from 0 until M do
              (setq T[m 0] 1)
              (loop for n from 1 until N do
                (setq T[m n] X[m (sub1 n)])
                ) ; end n loop
              ) ; end m loop
           (setq X T)
        )) ; end case
      ) ; end conversion cond
    X) ; end convertToMatrixC












































































;;**EXPORTKEY**:math:copyToMatrix
(defriend math:copyToMatrix(A)
;; *******************************************************************
;; name:     copyToMatrix
;; 
;; summary:  Copy a rank one or two vector or a rank one matrix
;;           into a rank two matrix.
;;
;; Parms:    A:       The rank one or rank two vector
;; Return:   A:       The proper rank two number matrix
;;
;; Note:     If the argument is not a rank one or rank two vector or
;;           a rank one matrix, no conversion is performed.
;; *******************************************************************
    vars:(k K m M n N AB T sum rankA rankB)
    ;; Perform conversion to a row matrix (if necessary).
    (cond    
     ;; Perform conversion of A from a vector to a column matrix (if necessary).
     ((or (and (isVector A) (= (length A[0]) 0)) (and (isMatrix A) (= (length (rank A)) 1)))
        (begin
           (setq N (length A))
           (setq T (new Matrix: number: 2 N 1))
           (loop for n from 0 until N do
             (setq T[n 0] A[n])
             ) ; end loop
           (setq A T)
        )) ; end case
     ;; Perform conversion of A from a vector array to a matrix (if necessary).
     ((and (isVector A) (> (length A[0]) 0) (isNumber A[0][0]))
        (begin
           (setq M (length A))
           (setq N (length A[0]))
           (setq T (new Matrix: number: 2 M N))
           (loop for m from 0 until M do
              (loop for n from 0 until N do
                (setq T[m n] A[m][n])
                ) ; end n loop
              ) ; end m loop
           (setq A T)
        )) ; end case
     ;; Perform copy of A from a matrix to a matrix (if necessary).
     ((isMatrix A)
        (begin
           (setq A (copy A))
        )) ; end case
      ) ; end conversion cond
    A) ; end copyToMatrix












































































;;**EXPORTKEY**:math:correlation
(defriend math:correlation(x y)
;; *******************************************************************
;; name:     covariance
;; 
;; summary:  Returns the correlation coefficient statistics 
;;           for the two vectors.
;; Parms:    x:       A vector of length N.
;;           y:       A vector of length N.
;; Return:   v:       The correlation coefficient of x and y
;; *******************************************************************
    vars:(m n (v 0) xmean ymean costd)
    (setq n (length x))
    (setq m (length y))
    (if (<> n m)
        (error 
           (append  "correlation: vectors not the same length"
                    " (length x)=" n
                    " (length y)=" m
                    " x=" x 
                    " y=" y)))
    (if (<= n 2) (return 0))
    (setq xmean (avg x))
    (setq ymean (avg y))
    (setq v (/ (vectorDotProduct (vectorSub x xmean) (vectorSub y ymean)) (subi n 1)))
    (setq costd (* (stdev x) (stdev y)))
    (if (<> costd 0) (setq v (/ v (* (stdev x) (stdev y)))) (setq v 0))
    v) ; correlation












































































;;**EXPORTKEY**:math:covariance
(defriend math:covariance(x y)
;; *******************************************************************
;; name:     covariance
;; 
;; summary:  Returns covariance statistics of the two vectors.
;; Parms:    x:       A vector of length N.
;;           y:       A vector of length N.
;; Return:   v:       The covariance of x and y
;; *******************************************************************
    vars:(m n (v 0) xmean ymean)
    (setq n (length x))
    (setq m (length y))
    (if (<> n m)
        (error 
           (append  "covariance: vectors not the same length"
                    " (length x)=" n
                    " (length y)=" m
                    " x=" x 
                    " y=" y)))
    (if (<= n 2) (return 0))
    (setq xmean (avg x))
    (setq ymean (avg y))
    (setq v (/ (vectorDotProduct (vectorSub x xmean) (vectorSub y ymean)) (subi n 1))) 
    v) ; covariance












































































;;**EXPORTKEY**:math:cursorRegress
(defriend math:cursorRegress(cursor fieldList)
;; *******************************************************************
;; summary:  Returns the sparse M coefficient vector for the fields 
;;           with the best least squares fit of the variables.
;;           The field list is used to construct an N by M+1 array
;;           where the M+1 column represents the dependent variable
;;           (the last field named in the field list).
;;           
;; Example:  w:         The N by M+1 array representing the field list
;;                      in the form of:    x x ... x y
;;                                         x x ... x y
;;                                             ... 
;;                                         x x ... x y
;;
;; Parms:    cursor:    A memory cursor from which the N by M+1 array will be extracted.
;;           fieldList: The field list with which the N by M+1 array will be extracted.
;;                      (Note: the field list must contain value column name Symbols).
;; Return:   vc:        The M+1 coefficient vector (0 = constant term, and e = M+1 term).
;; Note:     The cursor is NOT restored before the regression array is
;;           extracted. This enables conditional regression on partial row sets.
;; 
;; Globals:  dataMineLib
;; *******************************************************************
    vars:(vc w m M M+1 n N)
    ;; Extract the least squares error from multiple linear regressions
    (setq N (length cursor.rowVector))
    (setq M+1 (length fieldList))
    (setq M (subi M+1 1))
    ;; Extract the N by M+1 array for use in multiple regression.
    (setq w (new Vector: N))
    (loop for n from 0 until N do
       (setq w[n] (new Vector: M+1))
       (loop for m from 0 until M+1 do
          (setq w[n][m] cursor.rowVector[n][fieldList[m]])
          ) ; end M loop
       ) ; end N loop
    ;; Perform a multiple regression on the extracted array.
    (setq vc (multipleRegress w))
    ;; Return the regression coefficient vector.
    vc) ; end cursorRegress







































































;;**EXPORTKEY**:math:cursorToArray
(defriend math:cursorToArray(cursor fieldList)
;; *******************************************************************
;; summary:  Returns a regression array from the cursor and table
;;           fields specified. 
;;
;;           The field list is used to construct an N by M+1 array
;;           where the M+1 column represents the dependent variable
;;           (the last field named in the field list).
;;           
;; Example:  w:         The N by M+1 array representing the field list
;;                      in the form of:    x x ... x y
;;                                         x x ... x y
;;                                             ... 
;;                                         x x ... x y
;;
;; Parms:    cursor:    A memory cursor from which the N by M+1 array will be extracted.
;;           fieldList: The field list with which the N by M+1 array will be extracted.
;;                      (Note: the field list must contain value column name Symbols).
;; Return:   w:         The regression array with the dependent variable in the last column.
;; Note:     The cursor is NOT restored before the regression array is
;;           extracted. This enables conditional regression on partial row sets.
;; 
;; Globals:  dataMineLib
;; *******************************************************************
    vars:(vc w m M M+1 n N)
    ;; Extract the least squares error from multiple linear regressions
    (setq N (length cursor.rowVector))
    (setq M+1 (length fieldList))
    (setq M (subi M+1 1))
    ;; Extract the N by M+1 array for use in multiple regression.
    (setq w (new Vector: N))
    (loop for n from 0 until N do
       (setq w[n] (new Vector: M+1))
       (loop for m from 0 until M+1 do
          (setq w[n][m] cursor.rowVector[n][fieldList[m]])
          ) ; end M loop
       ) ; end N loop
    ;; Return the regression array.
    w) ; end cursorToArray















;;**EXPORTKEY**:math:cursorToMatrix
(defriend math:cursorToMatrix(cursor fieldList)
;; *******************************************************************
;; summary:  Returns a regression matrix from the cursor and table
;;           fields specified. 
;;
;;           The field list is used to construct an N by M+1 matrix
;;           where the M+1 column represents the dependent variable
;;           (the last field named in the field list).
;;           
;; Example:  w:         The N by M+1 matrix representing the field list
;;                      in the form of:    x x ... x y
;;                                         x x ... x y
;;                                             ... 
;;                                         x x ... x y
;;
;; Parms:    cursor:    A memory cursor from which the N by M+1 matrix will be extracted.
;;           fieldList: The field list with which the N by M+1 matrix will be extracted.
;;                      (Note: the field list must contain value column name Symbols).
;; Return:   w:         The regression matrix with the dependent variable in the last column.
;; Note:     The cursor is NOT restored before the regression matrix is
;;           extracted. This enables conditional regression on partial row sets.
;; 
;; Globals:  dataMineLib
;; *******************************************************************
    vars:(vc w m M M+1 n N)
    ;; Extract the least squares error from multiple linear regressions
    (setq N (length cursor.rowVector))
    (setq M+1 (length fieldList))
    (setq M (subi M+1 1))
    ;; Extract the N by M+1 matrix for use in multiple regression.
    (setq w (new Matrix: number: 2 N M+1))
    (loop for n from 0 until N do
       (loop for m from 0 until M+1 do
          (setq w[n m] cursor.rowVector[n][fieldList[m]])
          ) ; end M loop
       ) ; end N loop
    ;; Return the regression array.
    w) ; end cursorToMatrix















;;**EXPORTKEY**:math:cursorToVector
(defriend math:cursorToVector(cursor fieldName ...)
;; *******************************************************************
;; summary:  Returns a vector containing the values from the specified
;;           column from the cursor specified. 
;;
;; Parms:    cursor:    A memory cursor from which the column values will be extracted.
;;           fieldName: The field name (or numeric index) specifying the column to be extracted.
;;           stringSW:  (Optional) True iff row numbers are to be prefixed to each value;
;;                                 otherwise, a number vector of column values is to be returned. 
;; Return:   v:         The vector of column values.
;;
;; Note:     The cursor is NOT restored before the column vector is extracted.
;; Globals:  dataMineLib
;; *******************************************************************
    vars:(V n N numberSW)
    ;; Extract the column data from each row into a vector.
    (setq stringSW (if (>= (argCount) 3) (argFetch 2) false))
    (setq N (length cursor.rowVector))
    (if (= stringSW true) (setq V (new Vector: N)) (setq V (new Vector: number: N)))
    (loop for n from 0 until N do
       (if (= stringSW true)
           (setq V[n] (append "[" n "] " (string cursor.rowVector[n][fieldName])))
           (setq V[n] cursor.rowVector[n][fieldName])
           ) ; end if
       ) ; end N loop
    ;; Return the regression array.
    V) ; end cursorToVector


















;;**EXPORTKEY**:math:egm
(deforphan math:egm(...)
;; *******************************************************************
;; summary:  Creates a new evolutionary grid machine Lambda instance.
;;           Form 1: create a new egm instance with initial training information
;;           (setq egmInstance (math.egm cursor modelStructure trainedToDate previousPeriods))
;;           Form 2: create a new egm instance with previously saved egm state information
;;           (setq egmInstance (math.egm s)) ;; s is structure containing saved pvars from former instance of egm
;; 
;;			Call Form 1 to ready machine for training on a summary table cursor.
;; 			See egm.setMyPvars for logic behind Form 2.
;;			The cursor is reduced to a dense XY sigmod vector array (grid) using the 
;;			supplied baseExpressions, fieldExpressions and trainExpressions.
;;
;;           Evolutionary grid machines (egm) are quasi regression engines which
;;           learn and make regression estimates on XY vector arrays such as:
;;
;;               XY:  An NumRows by NumX + NumY array representing the original observations
;;                    in the form of:    x x x ...  y y ...
;;                                       x x x ...  y y ...
;;                                           ... 
;;                                       x x x ...  y y ...
;;
;;					NumRows is the number of rows in the observation set
;;					NumX is the number of independent columns (x values)
;;					NumY is the number of dependent columns (y values) for which 
;;					   regressions are to be peformed
;;
;;			The XY vector array (grid) is constructued by applying the baseExpressions against
;;			the cursor to reduce the cursor to NumRows. Then the fieldExpressions 
;;			are applied to the cursor to produce NumX independent columns (x values). Then
;;			the trainExpressions are applied to the cursor to produce NumY dependent
;;			columns (y values). Then the XY grid independent columns are crunched to dense 
;;			sigmod values.
;;
;;			The resulting XY grid is stored in the egm pvars. Each egm instances is 
;;			stored persistently by the calling applciation. In egm.selfTest, each egm
;;			instance is stored in memory during the test. In deepGreen applications each
;;			egm instance is stored in a repository to provide persistence across machine
;;			training and runs - see egm.trainMachine and egm.runMachine.
;;
;;			egmInstance.trainMachine
;;			This Lambda trains (performs the quasi regression) on the XY grid owned by 
;;			the instance 		
;;
;;			egmInstance.runMachine
;;			This Lambda applies one or more trained instances to a table of data.
;;
;;
;; Form 1 Parms:    
;	(setq estimator (math.egm cursor modelStructure previousPeriods))
; 	Args:
;		cursor			- dataMineLib table containing estimator Lambda data. The table must contain
;							a column called CurrentDate and this date must be the date of the
;							independent values associated with the table. All rows in the table must
;							have the same date. Note that other values are extracted from the table 
;							for populating the estimator Lambda's XY matrix using the BaseFilters, 
;							CutFilters and TrainingExression passed as members of the modelStructure 
;							argument described next. 
;			
;		modelStructure	- Structure containing:
;			.BaseFilters		- BaseFilters reduce the input table to a set sutable for use.
;			.Cabinet			- AnalystName
;			.CutFilters		- CutFilters are the field expressions (independent varaibles)
;			.CoreExpCount		- Number of filter expressions belonging to core genome population
;			.MaxDepth		- Maximum depth of field expression cross correlation to peform. Can not exceed 10.
;			.Morph			- Boolean flag indicating weather to use morning or not
;			.Bucketsize		- Bucketsize (usual and default is 5)
;			.NumTopGridSelCore- Number of top Grid selectors to keep for core genome population
;			.NumTopGridSelSec - Number of top Grid selectors to keep for secondary genome population
;			.TrainExpressions	- Training expressions (dependent variables)
;			.TrainingBias		- minimum number of days separating tables - this value is related to the
;								training expressions. For instance, a training expression of
;								Next3MonthProfit would imply a training bias of 91 days.
;			TrainingQtrs:	1	- The number of 
;		trainedTo		- the first date that the estimator Lambda can be used in training
;							or testing. 
;		previousPeriods 	- a vector of dates associated witht the regressMemories
; 	that will be passed in the trainMachine call documented next.
;; Return:   Rf:     A new evolutionary grid machine Lambda instance.

;; *******************************************************************
  pvars:(;; Public Variables
		;; All of these will be copied by getMyPvars and
		;; set by setMyPvars to facilitate exteral storage of the data
		;; portion of an egm instance.
		AllExp				;; Concatenation of FieldExp and TrainExp
		AllStocks			;; Vector of Directories for each y. Each directory is list of stocks picked with count.
		AnalystName			;; AnalystName -- documents creator of instance
		BaseExp				;; Saved so it can be applied to cursor passed with runMachine
		BucketSize			;; The size of individual buckets in the grid
		(CoreDone false)	;; Flag indicating if the core genome population has been trained
		CoreExpCount		;; Number of field expressions included in core genome population
		Evolve				;; Flag indicating if the estimator Lambda has and should evolve a secondary population
		FieldExp			;; Saved so it can be applied to cursor passed with runMachine
		Genome				;; This short vector has the following schema:
							;; index		value description
							;;	0		index into morph vector
							;; 	1..n		indices into field expressions
		LowRanksCore			;; Vector of current lowest rank for each dependent variable from core genome population
		LowRanksSec			;; Vector of current lowest rank for each dependent variable from secondary genome population
		LowRanksIndexCore		;; Vector of index into TopGridSelCore[t] to lowest rank entry for dependent variable from core genome population
		LowRanksIndexSec		;; Vector of index into TopGridSelSec[t] to lowest rank entry for dependent varaible from secondary genome population
		MaxDepth				;; maximum number for column cross-correlation to perform
		Morph				;; Use morph vectors? true/false
		NumAllExp			;; Length of all expressions .. equal to NumCols
		NumBaseExp			;; Length of base expressions vector
		NumCols				;; The number of cols in XY
		NumFieldExp			;; Length of field expressions vector .. equal to NumX
		NumGenome			;; number of elements in Genome
		NumRows				;; The number of rows in XY
		NumTrainExp			;; Length of training expressions vector .. equal to NumY
		(NumTopGridSelCore 100)	;; Number of top grid selectors to keep for each dependent variable from core genome population
		(NumTopGridSelSec 100)		;; Number of top grid selectors to keep for each dependent variable from secondary genome population
		NumX					;; Number of independent variables
		NumY					;; Number of dependent variables
		PreviousPeriods		;; Vector of TVAL dates of previous periods to train on in trainMachine
		RankByWinPct		;; boolean value indicating the use of winPct rather than score to rank grid selectors in topGridSelectors vector
		(SecondaryDone false) ;; Flag indicating if secondary genome population has been trained
		StockIDs			;; Vector of stock IDs - matching indicies with XY.
		(Summerized false)	;; Flag variable for public child Lambda summerize.
		TableDate			;; Date associated with independent data values
		TopGridSelCore		;; Vector of Vectors of top scoring genome grid selector structures for each dependent variable from
							;; core genome population
		TopGridSelSec			;; Vector of Vectors of top scroring genome grid selector structures for each dependent variable from 
							;; secondary genome population
		TrainingBias			;; Number of days of training bias. This number is associated with the training expressions used
							;; in the table. For instance, if Next3MonthProfit is used as a training field then the training
							;; bias is 91 days. This number is added to the TableDate to get the TrainedToDate.
		TrainedToDate			;; The first date the estimator Lambda can be used. This date the TableDate plus the TrainingBias. 
		TrainExp				;; Saved so it can be applied to cursor passed with runMachine.
		XY					;; Dense sigmod training vector. Created from cursor passed on Lambda creation.


		;; Private Child Lambdas
         _inputCursor    		;; Crunch cursor input into a dense sigmoid XY training vector array - uses baseExpressions and fieldExpressions
		_setMyPvars			;; Assign values from structure argument to pvars of Lambda - used to initlize egm Lambda from previously stored Lambda data
		_init
		_prettyPrintMemory

     	;; Public Child Lambdas
        runMachine			;; Run this percentile grid machine on the XY sample vector array 
        trainMachine			;; Train this percentile grid machine on the XY training vector array.
		clearSecondary		;; Clear the training information for the secondary genome population
		getTotalY			;; Get the toal value of the specified dependent variable for the XY stored in this estimator instance
		getMyPvars			;; Extract data portion of Lambda for external storage
		selfTest				;; Perform a training and return trained instance of the Lambda
		summerize			;; Perform summary calculations on an estimator Lambdas pvars. This is where we do the calculations that 
							;; we do not want to do each time we call trainMachine. Remember that trainMachine may be call many many times
							;; with small time slices. It is better to call summerize later when we want to "use" a previously trained
							;; estimator Lambda result. Note that the flag variable Summerized is set by the summerize routine and cleared
							;; by subsequent calls to trainMachine.

         ) ; end persistant variables

  ;; *******************************************************************************
  ;; Define Private Child Lambdas 
  ;; *******************************************************************************
   ;; Create a pretty print string of the specified training memory for self-descriptive storage.
   (defun _prettyPrintMemory(buffer memory margin)
      vars:(n N key newMargin sourceTemplate
           (blanks "                                                                                                                                                              ")
           (tailSW false)
           ) ; end temporary variables
      ;; Initialize the display buffer (if necessary).
      (if (= buffer #void)
          (begin 
             (setq buffer (new Vector: byte: 20000000))
             (setq sourceTemplate (mid (browseLib.checkout Analyst: "analystWizard:pgmRegress:%MEMORY_HDR") (+ 7 (length _eol)) 100000))
             (appendWriteln buffer sourceTemplate _eol (left blanks margin))
             (setq tailSW true)
          )) ; end if
      ;; Validate the display buffer (if necessary).
      (cond 
         ;; Manage pretty printing of a Structure.
         ((= (type memory) Structure:)
          (begin
             (setq N (length memory))
             (setq newMargin (+ margin 2))
             (appendWriteln buffer "#{")
             (loop for n from 0 until N do  
                (setq key memory[n 0])
                (appendWriteln buffer key ": ")
                (_prettyPrintMemory buffer memory[n 1] (+ newMargin (length key) 2))
                (appendWriteln buffer _eol (left blanks newMargin))
                ) ; end loop
             (appendWriteln buffer "} ; end Structure")
          )) ; end Structure case
         ;; Manage pretty printing of a Dictionary.
         ((= (type memory) Dictionary:)
          (begin
             (setq N (length memory))
             (setq newMargin (+ margin 2))
             (appendWriteln buffer "#{dic|| " _eol  (left blanks newMargin))
             (loop for n from 0 until N do  
                (setq key memory[n 0])
                (appendWriteln buffer key ": ")
                (appendWriteln buffer (string memory[n 1] true))
                (appendWriteln buffer _eol (left blanks newMargin))
                ) ; end loop
             (appendWriteln buffer "} ; end Dictionary")
          )) ; end Dictionary case
         ;; Manage pretty printing of a Directory.
         ((= (type memory) Directory:)
          (begin
             (setq N (length memory))
             (setq newMargin (+ margin 2))
             (appendWriteln buffer "#{dir|| " _eol  (left blanks newMargin))
             (loop for n from 0 until N do  
                (setq key memory[n 0])
                (appendWriteln buffer (string key true) " ")
                (appendWriteln buffer (string memory[n 1] true))
                (appendWriteln buffer _eol (left blanks newMargin))
                ) ; end loop
             (appendWriteln buffer "} ; end Directory")
          )) ; end Directory case
         ;; Manage pretty printing of an Object Vector.
         ((= (type memory) ObjVector:)
          (begin
             (setq N (length memory))
             (setq newMargin (+ margin 6))
             (appendWriteln buffer "#(obj|" _eol (left blanks newMargin))
             (loop for n from 0 until N do  
                (_prettyPrintMemory buffer memory[n] newMargin)
                (appendWriteln buffer " ;; [" n "]" _eol (left blanks newMargin))
                ) ; end loop
             (appendWriteln buffer ") ; end ObjVector")
          )) ; end ObjVector case
         ;; Manage pretty printing of a String.
         ((= (type memory) String:)
          (begin
             (appendWriteln buffer "{" (substitute (substitute (substitute (substitute memory _eol #\tab) #\tab " ") #\return " ") #\newline " ") "}")
          )) ; end ObjVector case
         ;; Manage pretty printing of default value.
         (else
             (appendWriteln buffer (string memory true))
         ) ; end other case
         ) ; end cond
      ;; Terminate the display buffer (if necessary).
      (if (= tailSW true)
          (begin 
             (setq sourceTemplate (mid (browseLib.checkout Analyst: "analystWizard:pgmRegress:%MEMORY_TAIL") 7 100000))
             (appendWriteln buffer sourceTemplate)
             (setq buffer (string buffer))
          )) ; end if
      buffer) ; end prettyPrintMemory

   ;; *******************************************************************************
   ;; Define Public Child Lambdas 
   ;; *******************************************************************************

;selfTest - full process test of the math.egm Lambda. The only dependencies on 
;DeepGreen are the FASummary tables.
;
;selfTest processes tables of data managed by dataMineLib. These tables
;are specified by the selfTest.tableList structure initialized in selfTest. Each 
;table is associated with a date. These tables are 91 and 98 days apart.
;
;These tables contain both the independent and dependent values. The independent
;values are "as of" the table's date. The dependent values vary in their date
;association based on their content. A common example would be Next3MonthProfit
;in the stock summary tables. Next3MonthProfit contains the profit for a stock
;for the three month period subsequent to the tables date. Since we are using
;the quarterly tables, no dependent expressions can have more than a 98 day bias.
;
;Estimator Lambdas contain both the dependent and independent values from the
;tables. We identify estimator Lambdas by the date associated with the 
;DEPENDENT values in the estimator Lambda. Note that some other math Lambdas,
;like the PGM, identify estimator Lambdas by the date associated with the 
;independent values used to train the estimator Lambda. Don't let this ball
;you up!
;
;EGM estimator Lambdas are created using calls like:
;	(setq estimator (math.egm cursor modelStructure trainedToDate previousPeriods))
;
;EGM estimator Lambdas are trained using calls like:
;	(estimator.trainMachine timeToTrain regressMemories)
;
;EGM estimator Lambdas are applied to a cursor by calls like:
;	(esitmator.runMachine cursor backCountPeriods)
;
;selfTest's training is controlled by the following parameters assigned in the pvars:
;		tableList		; Structure of table specification vectors of the form:
;							#{tablekey: #(XlsFilename tableName tabledate) ...}
;		(backCount 2) 	; Number of estimator Lambdas passed to egn.runMachine
;		(backPeriods 4) 	; Number of tables processed.
;		
; Pay particular attention to the processing of the tables and their associated dates
; in this routine.
;
(defun selfTest() 
	vars:( b j k p n N r R y i 
		(backCount 2) ; Number of estimator Lambdas passed to egm.runMachine
		backCountPeriods	; Vector of estimator Lambdas specified by backCount
		backPeriodEstimators
		currrentDate
		cursor 
		end
		estimator
		estimators
		first
		memory
		modelStructure
		notes
		numFound
		(numPreviousPeriods 2) ; number of estimator Lambdas passed to egm.trainMachine
		previousEstimator
		previousPeriods
		previousPeriodEstimators
		record
		regressMemories
		Rf
		start
		tableDate
		tableList
		theDate
		(timeToTrain 100000)
		trainedToDate
		)

	(setq modelStructure (new Structure:
		; BaseFilters are the base expressions
		BaseFilters: #("top (Volume*Price) 400;" ) ; BaseExp
		Cabinet:		"test"	; AnalystName
		; CutFilters are the field expressions
		CutFilters:	#( 	"((Volume/SharesOutstanding)*(1/PEGLastQtrEPSPctChg))" 
						"((Volume/SharesOutstanding)*(1/PEGPrj5YearGrowth))" 
						"RelativePriceStrength" 
						"((Volume/SharesOutstanding)*(1/PEG))"
						"(max(High52Week,Price)/min(Low52Week,Price))" 
						"((Price-min(Low52Week,Price))/(max(High52Week,Price)-min(Low52Week,Price)))" 
					)	; FieldExp
		CoreExpCount: 4
		MaxDepth: 3
		Morph: true
		NumTopGridSelCore: 50
		NumTopGridSelSec: 50
		RankByWinPct: false
		TrainExpressions:	#("Next3MonthProfit") ; TrainExp
		TrainingBias: 91
		TrainingQtrs:	1		;
	))
	;; Setup MorphVectors
	;; TBD		

;	(setq tableList #{FASummary20010302: #("SummaryTablesFA\FASummary20010302.xls" FASummary20010302 #Mar,2,2001 ) FASummary20010601: #("SummaryTablesFA\FASummary20010601.xls" FASummary20010601 #Jun,1,2001 ) FASummary20010831: #("SummaryTablesFA\FASummary20010831.xls" FASummary20010831 #Aug,31,2001 ) FASummary20011130: #("SummaryTablesFA\FASummary20011130.xls" FASummary20011130 #Nov,30,2001 ) FASummary20020301: #("SummaryTablesFA\FASummary20020301.xls" FASummary20020301 #Mar,1,2002 ) FASummary20020531: #("SummaryTablesFA\FASummary20020531.xls" FASummary20020531 #May,31,2002 ) FASummary20020830: #("SummaryTablesFA\FASummary20020830.xls" FASummary20020830 #Aug,30,2002 ) FASummary20021129: #("SummaryTablesFA\FASummary20021129.xls" FASummary20021129 #Nov,29,2002 ) FASummary20030228: #("SummaryTablesFA\FASummary20030228.xls" FASummary20030228 #Feb,28,2003 )})
	(setq tableList #{FASummary20010302: #("SummaryTablesFA\FASummary20010302.xls" FASummary20010302 #Mar,2,2001 ) FASummary20010601: #("SummaryTablesFA\FASummary20010601.xls" FASummary20010601 #Jun,1,2001 ) FASummary20010831: #("SummaryTablesFA\FASummary20010831.xls" FASummary20010831 #Aug,31,2001 ) FASummary20011130: #("SummaryTablesFA\FASummary20011130.xls" FASummary20011130 #Nov,30,2001 ) FASummary20020301: #("SummaryTablesFA\FASummary20020301.xls" FASummary20020301 #Mar,1,2002 )})
	;(setq tableList ^deepGreen._summaryQuarterlyTables) ;; Note Dependency on deepGreen!!!!
	; tableList[n 0] -- contains name of dataMineLib table
	; tableList[n][0] -- contains name of .xls file
	; tableList[n][1] -- contains name of dataMineLib table
	; tableList[n][2] -- contains date of table

	(setq N (length tableList))
	(setq end (- N 1)) ; last period to process 
	(setq estimators (new Directory:))
	; Create and perform training of each estimator Lambda
	(loop for n from 0 until end do ; don't include the last entry in tableList
		; Get tableDate from tableList
		(setq tableDate tableList[n][2]); this is the date assoicated with the independent data values!

		; Calculate the date of the next quarter table entry
		; Note that we can not just add 91 days as the spread is sometimes 98 days. This means we always have to have
		; a table like the quarterly tables, create by deepGreen for instance, to get this future date.
		(setq trainedToDate tableList[(+ n 1)][2])

		; Check that we have at least 91 days spread between tables
		(if (< (- trainedToDate tableDate) 91) (begin
			(writeln "egm.selfTest error! less than 91 days between trainedToDate and tableDate")
			(return false)
			end))

		; Create estimator Lambda 
		(if (not (isMember trainedToDate estimators)) (begin ;; create Lambda, will always happen in this simple selfTest
	 		; Generate list of previous periods to pass as an argument when creating the egmInstance for this period.
			; These previous period dates are the "trainedToDates" used to index the estimator Lambdas.
			(setq j (max 1 (- n numPreviousPeriods))) 
			(setq k 0)
			(setq previousPeriods (new Vector:))
			(loop for i from j until n do 
				(setq previousPeriods[k] tableList[i][2])
				(setq k (+ k 1))
			);j
			(setq cursor (dataMineLib.open tableList[n 0] memory:)); Open the cursor for the tableDate
			(setq estimator (math.egm cursor modelStructure trainedToDate previousPeriods)); create the esitmator Lambda
			(setq cursor (dataMineLib.close cursor))
			(setq estimators[trainedToDate] estimator); save the estimator keyed by the trainedToDate
			(writeln "created estimators[" trainedToDate"]  trainedToDate: " estimator.TrainedToDate " tableDate: " estimator.TableDate " previous periods:" estimator.PreviousPeriods );
			end))

		(setq estimator estimators[trainedToDate])

		(writeln "training estimator Lambda, trainedToDate: " estimator.TrainedToDate " previousPeriods:" estimator.PreviousPeriods)
		;generate vector of estimator Lambda memories to pass to trainMachine using
		; the previousPeriods list stored in the current estimator Lambda's pvars
		(setq previousPeriodEstimators (new Vector: object:))
		(setq previousPeriods estimator.PreviousPeriods)
		(loop for i from 0 until (length previousPeriods) do
			(setq previousPeriodEstimators[i] (estimators[previousPeriods[i]].getMyPvars))
		);i

(writeln "estimator.CoreExpCount = " estimator.CoreExpCount)
;		(estimator.trainMachine timeToTrain Jitter: previousPeriodEstimators) ; Jitter
;		(estimator.trainMachine timeToTrain All: previousPeriodEstimators) ; All
;		(estimator.trainMachine timeToTrain Core: previousPeriodEstimators) ; Core
;		(estimator.trainMachine timeToTrain Secondary: previousPeriodEstimators) ; Secondary
		(estimator.trainMachine timeToTrain Evolve: previousPeriodEstimators) ; Secondary		
;		Disable the preceeding statements and enable the following loop to test the incremental training feature
;		vars:(t)
;		(loop for t from 0 until 5 do
;			(estimator.trainMachine .20 Jitter: previousPeriodEstimators)
;		)		
	);n

	; print out  gridSelectors
	(loop for n from 0 until (length estimators) do
		(setq estimator estimators[n 1])
		(writeln "Estimator for " estimators[n 0])
;		(writeln (_prettyPrintMemory #void (estimator.getMyPvars) 0))
		(loop for  y from 0 until estimator.NumY do 
			(loop for i from 0 until estimator.NumTopGridSelCore do
				(writeln "topGridSelCore["y"]["i"] " estimator.TopGridSelCore[y][i])
			);i
		);y
		(loop for  y from 0 until estimator.NumY do 
			(loop for i from 0 until estimator.NumTopGridSelSec do
				(writeln "topGridSelSec["y"]["i"] " estimator.TopGridSelSec[y][i])
			);i
		);y
	);n

	; perform ranking for each table in tableList from start to end
	(loop for n from 1 to end do ;don't include first entry, do include last entry in tableList!
		(writeln "Calling runMachine on table " tableList[n][1])

		(setq trainedToDate tableList[n][2])

		(setq cursor (dataMineLib.open tableList[n 0] memory:)); Open the cursor
		; Get the estimator Lambda to run against the current table
		(setq estimator estimators[trainedToDate])
		; generate a directory of additional trained estimator Lambdas to run against
		; the current cursor. The rankings of each estimator Lambda is 
		; averaged for a final ranking.
		(setq first (max 1 (- n backCount)))
		(setq backPeriodEstimators (new Directory:))
		(loop for j from first until n do
			(setq backPeriodEstimators[estimators[j 0]] (estimators[j 1].getMyPvars))
		);j
		(estimator.runMachine cursor backPeriodEstimators)

		(setq numFound 0)
		(setq R (length cursor.rowVector))
		(loop for r from 0 until R do
			(setq record (refObjVector cursor.rowVector r))
			(setq notes record.SpecialSituationNotes)
			(if (<> notes #void) (begin
				(writeln r " " notes)
				(setq numFound (+ numFound 1))
				))
		);n
		(writeln "Number of stocks ranked=" numFound)
		(setq cursor (dataMineLib.close cursor))
	);n
	
  	true)

(defun getTotalY(y)
	vars:(n total yOffset)
	(if (> y NumY) (begin
		(writeln "Error: math.egm.getTotalY - argument larger than number of Y in dataset")
		(return 0)
		))
	(setq yOffset (+ NumX y))
	(loop for n from 0 until NumRows do
		(setq total (+ total XY[n][yOffset]))
	);n
	total);getAllPerformance


;; Assign seleted pvars to the structure argument s. This routine allows the "data" portion
;; of the egm instance to be extracted for external store. Note that no Lambdas
;; are included in this copy.
(defun getMyPvars(...)
	vars:(p n N s)
	(setq p (myself))
	(setq p p.Pv)
	(setq N (length p))
	(if (= (argCount) 1)
		(setq s (argFetch 0))
		(setq s (new Structure:)))
	(loop for n from 0 until N do
		(if (not (isLambda p[n]))
			(setq s[p[n 0]] (copy p[n])))
	); n
	s) ;; end of getMyPvars

;; Assign selected items from the structure s into this Lambda's pvars. This routine allows the "data"
;; portion of an egm instance to be initialized from some external store. Note that this
;; copy is governed by the existance of the target Pv element in  this Lambda's pvars. Other values
;; stored in the structure argument s will not be copied into the  this Lambda's pvars structure.
;; Note: Lambdas will never be assigned into this Lambda's pvars by this routine.
(defun _setMyPvars(s)
	vars: (i n N p)
	(setq p (myself))
	(setq p p.Pv)
	(setq N (length p))
	(loop for n from 0 until N do
		(if (not (isLambda p[n])) (begin
			(setq i (member p[n 0] s))
			(if (isNumber i)
				(setq p[n 1] (copy s[i 1])))
			))
	)
	true)

;; Form 1 init of the egm. Note that this process is only called once to establish
;; a starting internal state for the egm.
(defun _init(cursor modelStructure trainedToDate previousPeriods)
	vars:(i cursorData)

	(setq Evolve 			modelStructure.Evolve)
(writeln "modelStructure.Evolve=" modelStructure.Evolve)
	(setq TrainingQtrs 	modelStructure.TrainingQtrs)
	(setq AnalystName 	modelStructure.Cabinet)
	(setq FieldExp 		modelStructure.CutFilters)
	(setq BaseExp 		modelStructure.BaseFilters)
	(setq CoreExpCount	modelStructure.CoreExpCount)
	(setq TrainExp 		modelStructure.TrainExpressions)
	(setq MaxDepth 		(integer modelStructure.MaxDepth))
	(setq Morph			modelStructure.Morph)
	(setq NumTopGridSelCore 	(integer modelStructure.NumTopGridSelCore))
	(setq NumTopGridSelSec (integer modelStructure.NumTopGridSelSec))
	(setq TrainingBias 	(integer modelStructure.TrainingBias))
	(setq RankByWinPct	modelStructure.RankByWinPct)
	(setq TrainedToDate	trainedToDate)
	(setq PreviousPeriods previousPeriods)
	
     (if (not (isVector BaseExp)) (setq BaseExp (new Vector: 1 BaseExp)))
	(setq NumBaseExp (integer (length BaseExp)))
	(setq NumFieldExp (integer (length FieldExp)))
	(setq NumTrainExp (integer (length TrainExp)))
	(setq AllExp (append FieldExp TrainExp))
	(setq NumAllExp (integer (length AllExp)))

	(if (not (isVector PreviousPeriods)) (setq PreviousPeriods (new Vector:)))

	(setq Genome (new Vector: short: 1 0)) ; First number is morphVector index, Second number is first number of column selection genome
	(setq NumGenome (integer (length Genome)))

	(setq cursorData (_inputCursor cursor)) ; crunch the table into a dense sigmod vector of vectors
	(setq TableDate cursor.bckVector[0].CurrentDate)	

	(setq XY cursorData.xy)
	(setq StockIDs cursorData.StockIDs)

	(setq NumRows (integer (length XY)))
	(setq NumX (integer NumFieldExp))
	(setq NumY (integer NumTrainExp))
	(setq NumCols (integer (+ NumX NumY)))
	(setq TopGridSelCore (new Vector: object: NumY))
	(setq TopGridSelSec (new Vector: object: NumY))

	(loop for i from 0 until NumY do (setq TopGridSelCore[i] (new Vector: object: NumTopGridSelCore)))
	(setq LowRanksIndexCore (new Vector: short: NumY 0))
	(setq LowRanksCore (new Vector: number: NumY BIGNEGNUM))

	(loop for i from 0 until NumY do (setq TopGridSelSec[i] (new Vector: object: NumTopGridSelSec)))
	(setq LowRanksIndexSec (new Vector: short: NumY 0))
	(setq LowRanksSec (new Vector: number: NumY BIGNEGNUM))

	(setq BucketSize 5) ; Fixed value

 	true); end init

 ;; Crunch cursor input into a dense sigmoid XY training vector array.
 ;; Note: The SpecialSituationNotes field of the cursor is used as a scratch pad. 
 ;; ** inputCursor uses many parent Lambdas pvars. These must be initialized
 ;;    before calling _inputCursor! See the _init Lambda. 
 (defun _inputCursor(cursor)
    vars:(k m n numRecords xy command record minX maxX rngX stockIDs
         temp rowXY)

	 ; Run base expressions
    (loop for k from 0 until NumBaseExp do (begin 
		(cursor.run BaseExp[k])
	))
    (setq numRecords cursor.recordCount)
    (setq xy (new Vector: object: numRecords)) ;; xy is local here, not the XY in the pvars
	(setq stockIDs (new Vector: short: numRecords))

	;; Construct the filter command that will update the SpecialSituationNotes field in the table with
	;; the calculated dependent and independent values in the xy
	(setq command (append "setnr SpecialSituationNotes new('Vector','number'," NumAllExp)) 
	(loop for m from 0 until NumAllExp do (setq command (append command "," AllExp[m])))
	(setq command (append command ");"))
	;; Extract raw data from cursor into XY vector array.
	(cursor.run command) ;; Fills the special situation notes with results of all expressions

	; extract special situation notes into XY array vector and find min, max and range in independent columns
   (setq minX (new Vector: number: numRecords BIGPOSNUM))  
   (setq maxX (new Vector: number: numRecords BIGNEGNUM))  
   (setq rngX (new Vector: number: numRecords))
   (loop for n from 0 until numRecords do
       (setq record (refObjVector cursor.rowVector n)) 
    (setq stockIDs[n] record.ID)
       (setq rowXY record.SpecialSituationNotes) ; assign local for speed
       (loop for m from 0 until NumFieldExp do ; for each field expression column, don't examine train expression columns
          (if (< (setq temp (refNumVector rowXY m)) (refNumVector minX m)) (setNumVector minX m temp))
          (if (> temp (refNumVector maxX m)) (setNumVector maxX m temp))
          (setNumVector rngX m (- (refNumVector maxX m) (refNumVector minX m)))
          ) ; end MX loop 
       (setObjVector xy n rowXY)
       (setq record.SpecialSituationNotes #void)
       );n
    ; make a pass through XY making values sigmod for field expression columns (ie: independent variables)
	(loop for n from 0 until numRecords do
       (setq rowXY (refObjVector xy n)) ; assign local for speed
       (loop for m from 0 until NumFieldExp do
          (if (> (setq temp (refNumVector rngX m)) 0)
              (setNumVector rowXY m (/ (- (refNumVector rowXY m) (refNumVector minX m)) (refNumVector rngX m)))
              (setNumVector rowXY m 0.0)
              ) ; end if
          ); m
       );n
    (new Structure: xy: xy StockIDs: stockIDs)) ; end inputCursor

;; Clear the secondary genome population training results and
;; reset the SecondaryDone flag. Note that myTrainingMemory is passed
;; so that the latest field expressions are updated and that we 
;; recrunch the table data into the xy matrix.
(defun clearSecondary(cursor myTrainingMemory)
	vars:(i 
		cursorData
		modelStructure
		)
	(setq SecondaryDone false)
	(setq modelStructure myTrainingMemory.Models[0])
	(setq FieldExp 		modelStructure.CutFilters)
	(setq CoreExpCount	modelStructure.CoreExpCount)
	(setq NumTopGridSelSec (integer modelStructure.NumTopGridSelSec))

	(loop for i from 0 until NumY do (setq TopGridSelSec[i] (new Vector: object: NumTopGridSelSec)))
	(setq LowRanksIndexSec (new Vector: short: NumY 0))
	(setq LowRanksSec (new Vector: number: NumY BIGNEGNUM))	

	(setq NumBaseExp (integer (length BaseExp)))
	(setq NumFieldExp (integer (length FieldExp)))
	(setq NumTrainExp (integer (length TrainExp)))
	(setq AllExp (append FieldExp TrainExp))
	(setq NumAllExp (integer (length AllExp)))

;(writeln TrainExp)
	(setq cursorData (_inputCursor cursor)) ; crunch the table into a dense sigmod vector of vectors
										; This could be made more efficient by crunching only the columns
										; affected by the secondary genome population. 

	(setq TableDate cursor.bckVector[0].CurrentDate)	

	(setq XY cursorData.xy)
;(writeln "XY[0]" XY[0])
	(setq StockIDs cursorData.StockIDs)

	(setq NumRows (integer (length XY)))
	(setq NumX (integer NumFieldExp))
	(setq NumCols (integer (+ NumX NumY)))
	(setq Genome (new Vector: short: 1 0)) ; First number is morphVector index, Second number is first number of column selection genome
	(setq NumGenome (integer (length Genome)))


true)

; Run the machine on cursor
;	Args:
;		cusor			- table containing data to be ranked by the estimator Lambda. 
;							This is not the same as the estimator Lambda data. Instead,
;							it is a table of data having a table date greater than or
;							the same as the date of the estimator Lambda. Remember that
;							the estimator Lambda is identified by the date associated with
;							the dependent values the estimator Lambda trained on.
;		backCountPeriods - directory of zero or more previously trained estimator Lambda memories
; see the docmentation in the egm.cpp file for the runMachine C function
;
(defun runMachine(cursor ...)
	vars:(n N me estVector)
     ;; Collect arguments.
	(if (= (argCount) 2) (setq estVector (argFetch 1)) (setq estVector (new Directory:)))
	(setq N (length estVector))
	(display "runMachine estVector=")
	(loop for n from 0 until N do
		(display  estVector[n].TableDate " ")
	)
	(display _eol)
	(writeln "math.egm.runMachine tableDate:" (date cursor.bckVector[0].CurrentDate))
	(setq me (myself))
	(math_egm_RunMachine me cursor estVector)
	true) ; end runMachine


;; Train this grid machine for the time allowed by timeToRun
;	(egmInstance.trainMachine timeToTrain regressMemories)
;	Args:
;		timeToTrain		- the number of seconds the estimator should train
;		regressMemories	- a vector of zero or more previously trained estimator
;							Lambda memories. The dates of these previous periods
;							were specified in the previousPeriods argument to
;							math.egm. These dates can be retreived by the statement
;							(setq previousPeriods estimator.previousPeriods)
;
;trainMachine performs quasi-regression on the XY grid in the egmInstance and
;on the XY grids in egmInstances passed in the regressMemories vector.
; see the documentation in the egm.cpp file for the trainMachine C function.
(defun trainMachine(...)
     vars:(i n N me estMemories timeToRun procType)

	;; Get arguments
	(if (< (argCount) 2) (begin
		(writeln "Error! math.egm.trainMachine - too few arguments")
		(return false)))



	(setq timeToRun (argFetch 0))
	(setq procType (argFetch 1))
(writeln "procType = " procType)
	(cond 
		((= procType Jitter:) (setq procType 0))
		((= procType All:) (setq procType 1))
		((= procType Core:) (setq procType 2))
		((= procType Secondary:) (setq procType 3))
		((= procType Evolve:) (setq procType 4)) ;; Do all but skip Core if Core already done
	)

	; 3rd argument is optional vector of estimator Lambda memories used for time series based training
	(if (= (argCount) 3) 
		(setq estMemories (argFetch 2))
		(setq estMemories (new Vector: object: 0)))

	; Check to make sure none of the estimator Lambda memories passed have TrainedToDates less than
	; the TrainedToDate of this Lambda
	(loop for i from 0 until (length estMemories) do
		(if (not (< estMemories[i].TrainedToDate TrainedToDate)) (begin
			(writeln "egm.trainMachine Error! estimator Lambdas TrainedToDates out of range")
			(return false)
			))
	);i
	(display "math.egm.trainMachine estMemories:")
	(setq N (length estMemories))
	(loop for n from 0 until N do
		(writeln estMemories[n].TableDate)
	)
	(display _eol)
	(writeln "math.egm.trainMachine")
	(setq me (myself))
	(math_egm_TrainMachine me timeToRun procType (copy estMemories))
     true) ; end trainMachine

;; summerize
;; This Lambda will perform summary calculations on the pvars of an math.egm Lambda.
;; You can pass in the pvars structure or allow it to work on the current Lambdas
;; pvars by not passing an argument.
(defun summerize(...) 
	vars:(y i m j
		allStocks
		len
		NumTopGridSelCore
		NumTopGridSelSec
		numY
		result 
		stocks
		topGridSelCore
		topGridSelSec
		)
;(writeln "Sumerizing")

	(if (= (argCount) 1) ; argument is expected to be pvars of an egm Lambda
		(setq result (argFetch 0))
	else (begin ;; use the current Lambdas pvars
		(setq result (myself))
		(setq result result.Pv)
		))

	; Don't repeat summary if it has already been done and no calls to 
	; trainMachine have been performed since the last summarize call.
	(if (= result.Summerized true) (return result))

	; Assign local copies of pvars used in summary analysis
	(setq NumTopGridSelCore result.NumTopGridSelCore)
	(setq NumTopGridSelSec result.NumTopGridSelSec)
	(setq numY result.NumY)
	(setq topGridSelCore result.TopGridSelCore)
	(setq topGridSelSec result.TopGridSelSec)
		
	; Perform stock selection diversity analysis for each y
	(setq allStocks (new Vector: numY))
	(loop for y from 0 until numY do
		(setq allStocks[y] (new Directory:))
		(loop for i from 0 until NumTopGridSelCore do 
			;(writeln topGridSelCore[y][i])
			(setq stocks topGridSelCore[y][i].Stocks)
			(setq len (length stocks))
			(loop for j from 0 until len do
				(setq m (member stocks[j] allStocks[y]))
				(if (= m false) 
					(setq allStocks[y][stocks[j]] 1)
					(setq allStocks[y][m 1] (+ allStocks[y][m 1] 1)))
			);j 
		);i
		(loop for i from 0 until NumTopGridSelSec do 
			;(writeln topGridSelSec[y][i])
			(setq stocks topGridSelSec[y][i].Stocks)
			(setq len (length stocks))
			(loop for j from 0 until len do
				(setq m (member stocks[j] allStocks[y]))
				(if (= m false) 
					(setq allStocks[y][stocks[j]] 1)
					(setq allStocks[y][m 1] (+ allStocks[y][m 1] 1)))
			);j 
		);i
;		(writeln "For y=" y " StocksPicked=" (length allStocks[y]) " AllStocks=" allStocks[y])
	);y 
	(setq result.Summerized true)
	(setq result.AllStocks allStocks)
	result); end summerize

  	;; *******************************************************************************
  	;; Begin Main Logic 
  	;; *******************************************************************************
  	vars:(Egm memory)
  	(setq Egm (new (myself)))
	; Args
	;; Form 1: create a new egm instance with initial training information contained in modelStructure
	;; (setq estimator (math.egm cursor modelStructure trainedToDate previousPeriods))
	;; Form 2: create a new egm instance with previously saved egm state information
	;; (setq estimator (math.egm s)) ;; s is structure containing saved pvars from former instance of egm
	(cond
		((= (argCount) 1) (begin ; Form 2
			(setq memory (argFetch 0))
			(if (= memory #void) (return #void))
			(Egm._setMyPvars memory)
			))
		; Form 1 - (egm cursor modelStructure trainedToDate previousPeriods)
		((= (argCount) 4) (Egm._init (argFetch 0) (argFetch 1) (argFetch 2) (argFetch 3)))
		(else 
			(begin
			(writeln "Error: egm received wrong number of arguments")
			(return #void)
		))
	); end cond


  Egm) ; end EGMMachine













;;**EXPORTKEY**:math:evolveExpr
 
;; This Lambda explores the generation of field expressions
;; using GE with the following design criteria:
;; 1. Genomes and phenotypes are floating point vectors
;; 2. Terminator rule is an OPTIONAL part of grammar
;; 3. No Genome wrapping
;; 4. Any rule with insufficient genome terminates and returns
;;    the expression it was passed.
;; 5. The expression is passed recursively to each rule.
;;
;; 	Grammar Rules
;		(setq Rules #{
;			Expr: #(Rule:)
;			Rule: #(
;				#(Field:)
;				#(UnaryFunc: "(" Rule: ")")			
;				#(UnaryOper: "(" Rule: ")")			
;				#(BinaryFunc: "(" Rule: "," Rule: ")" )	
;				#("(" Rule:  Operator:  Rule: ")" )
;				#("(" Rule: AbenialOperator: Rule: ")" )
;				#(BinaryFunc: "(" Number: "," Rule: ")" )
;				#(BinaryFunc: "(" Rule: "," Number: ")" )
;				#("(" Number:  Operator: Rule: ")")
;				#( "(" Rule:  Operator: Number: ")" )
;				#("(" Number: AbenialOperator: Rule: ")" )
;				)
;			Field: #( "Price" "Sales" "Volume" "SharesOutstanding" "PEGLastQtrEPSPctChg" "EarningsPS" "Low52Week" "PastEPSHistory[0]" "PastEPSHistory[4]" "avg(AnalystRank)")
;			Number: #(obj| Real: Integer: "1")
;			Operator: #("-" "/")
;			AbenialOperator: #("+" "*")
;			UnaryOper: #("-")
;			UnaryFunc: #("abs")
;			BinaryFunc: #("max" "min")
;		})


(deforphan math evolveExpr(...)
	pvars:(
		;; Public pvars - The convention is that these be returned by getMyPvars
		Rules 			; Structure of grammar rules used to generate expressions from GenomeKeys
		Genomes			; Directory of genome structures keyed on GenomeKey element of genome structure
		MaxGenomeLen		; Maximum length of GenomeKey. Note: GenomeKey is a vector of reals.
		GenomeKeySeeds	; Vector of GenomeKeys used to generate part of initial population
		Population		; Vector of current genome structures sorted by Score element of genome structure
		MutationPct		; % of population to mutate with each generation
		CrossoverPct		; % of population to breed using crossover with each generation
		NewPct			; % of new, randomly generated members, introduced into population with each generation
		PopulationSize	; Number of members in the population
		ScoreLambda		; Registered Lambda used to score population. Use #void to call default score Lambda.
		ScoreLambdaArgs	; A Structure containing arguments that must be passed to score Lambda.
		Report			; A vector of report records updated by the report child Lambda
		Performance		; 
		CumGen			; Cumulative generation count across multiple runs
		(EchoReport false)

		;; Private pvars - The convention is that these should not be returned by getMyPvars
		_Genome
		_NewGenome
		_Expression
		_Gp
		
		;; Public Child Lambdas
		selfTest			; A selfTest of the GE Lambda
		score			; Cause the GE Lambda instance to run the registered score routine
		breed			; Perform the mutate and crossover functions on the current population
		report			; Report on the current population
		getMyPvars
		setMyPvars

		;; Private Child Lambdas
		_crossover		; Create two new GenomeKeys from two existing GenomeKeys
		_generate		; Generate a full genome structure from a GenomeKey
		_generate2		; The recursive logic used by _generate
		_mutate			; Create a new GenomeKey by mutating an existing GenomeKey
		_score			; The default scoring routine provided by the GE Lambda
		score			; Routine that calls the registered score routine specified in the ScoreLambda
		)

	(defun selfTest() 
		vars:(state ge)
		;; Initialize the state structure we pass when we create
		;; a new GE Lambda instance. We do it this way in the selfTest
		;; as a proxy for extracting the state of a GE Lambda instance that
		;; was saved into a repository. See the getMyPvars child Lambda to see how
		;; the necessary state information is extracted from a GE Lambda
		;; instance in preparation for its persistence in a repository. We don't 
		;; just save the whole Lambda to save memory and to allow previously 
		;; generated state information to be used by later generations of 
		;; enhanced GE Lambda instances.
		(setq state #{
			Rules: #{
				Expr: #(Rule:)
				Rule: #(
					#(Field:)
	;				#(UnaryFunc: "(" Rule: ")")			
	;				#(UnaryOper: Field: )			
					#(BinaryFunc: "(" Rule: "," Rule: ")" )
					#("(" "1#" Field: ")")	
					#("(" Rule:  Operator:  Rule: ")" )
					#("(" Rule: AbenialOperator: Rule: ")" )
	;				#(BinaryFunc: "(" Number: "," Rule: ")" )
	;				#(BinaryFunc: "(" Rule: "," Number: ")" )
	;				#("(" Number:  Operator: Rule: ")")
	;				#( "(" Rule:  Operator: Number: ")" )
	;				#("(" Number: AbenialOperator: Rule: ")" )
					)
				Number: #(obj| Real: Integer: "1")
				Operator: #("-", "#") ; # is protected divide
				AbenialOperator: #("+" "*")
				UnaryOper: #("-")
				UnaryFunc: #("abs")
				BinaryFunc: #("max" "min")
				Field:#(
				#{Expression: "AnalystRank" Type: Structure: Elements: #("compositeAnalyst" "maxStockroom" "stockWizard" "valueMeister") }
				#{Expression: "AnalystRankPast13Week" Type: Structure: Elements: #("compositeAnalyst" "maxStockroom" "stockWizard" "valueMeister") }
				#{Expression: "AnalystRankPast1Week" Type: Structure: Elements: #("compositeAnalyst" "maxStockroom" "stockWizard" "valueMeister") }
				#{Expression: "AnalystRankPast4Week" Type: Structure: Elements: #("compositeAnalyst" "maxStockroom" "stockWizard" "valueMeister") }
				#{Expression: "Beta" Type: Number: }
				#{Expression: "BookValue" Type: Number: }
				#{Expression: "Cash" Type: Number:  }
				#{Expression: "CashFlow" Type: Number:  }
				#{Expression: "DebitPctCapital" Type: Number:  }
				#{Expression: "Earnings" Type: Number:  }
				#{Expression: "EPSGrowthMomentum" Type: Number:  }
				#{Expression: "EstEarningsReturn5Year" Type: Number:  }
				#{Expression: "EstPctChgEPSFY" Type: Number:  }
				#{Expression: "EstPctChgEPSQtr1" Type: Number:  }
				#{Expression: "EstPctChgEPSQtr2" Type: Number:  }
				#{Expression: "FinancialRank" Type: Number:  }
				#{Expression: "High52Week" Type: Number:  }
				#{Expression: "LastQtrEPSPctChg" Type: Number:  }
				#{Expression: "LastQtrEPSSurprise" Type: Number:  }
				#{Expression: "Low52Week" Type: Number:  }
				#{Expression: "MarketValue" Type: Number:  }
				#{Expression: "Past12MonthEPSPctChg" Type: Number:  }
				#{Expression: "Past12MonthSalesPctChg" Type: Number:  }
				#{Expression: "Past13WeekPctChg" Type: Number:  }
				#{Expression: "Past26WeekPctChg" Type: Number:  }
				#{Expression: "Past5YearBVGrowth" Type: Number:  }
				#{Expression: "Past5YearEPSGrowth" Type: Number:  }
				#{Expression: "PastEPSHistory" Type: NumberVector: Length: 8 }
				#{Expression: "PastMarginHistory" Type: NumberVector: Length: 8 }
				#{Expression: "PastPriceHistory" Type: NumberVector: Length: 12 }
				#{Expression: "PastSalesHistory" Type: NumberVector: Length: 8 }
				#{Expression: "PrjBVGrowth" Type: Number:  }
				#{Expression: "PrjBVGrowth" Type: Number:  }
				#{Expression: "PrjCashFlowPS" Type: Number:  }
				#{Expression: "PrjDivGrowth" Type: Number:  }
				#{Expression: "PrjEPSGrowth" Type: Number:  }
				#{Expression: "PrjEPSGrowth" Type: Number:  }
				#{Expression: "ProfitPastHistory" Type: NumberVector: Length: 12 }
				#{Expression: "RelativeEPSStrength" Type: Number:  }
				#{Expression: "RelativePCFStrength" Type: Number:  }
				#{Expression: "RelativePriceStrength" Type: Number:  }
				#{Expression: "Sales" Type: Number:  }
				#{Expression: "SharesOutstanding" Type: Number:  }
				#{Expression: "superTrader.sortinoRatio(math.vectorDeltaPercents(reverse(copy(ProfitPastHistory))),0)" Type: Number:  }
				#{Expression: "superTrader.sortinoRatio(math.vectorDeltaPercents(reverse(resize(copy(PastEPSHistory),21))),0)" Type: Number:  }
				#{Expression: "superTrader.sortinoRatio(math.vectorDeltaPercents(reverse(resize(copy(PastMarginHistory),21))),0)" Type: Number:  }
				#{Expression: "superTrader.sortinoRatio(math.vectorDeltaPercents(reverse(resize(copy(PastSalesHistory),21))),0)" Type: Number:  }
				#{Expression: "TimingHistory" Type: Number:  }
				#{Expression: "TimingRank" Type: Number:  }
				#{Expression: "TimingSignal" Type: Number:  }
				#{Expression: "TotalAssets" Type: Number:  }
				#{Expression: "Value" Type: Number:  }
				#{Expression: "Volume" Type: Number:  }
				#{Expression: "WallStreetRank" Type: Number:  }
				#{Expression: "Yield" Type: Number:  }
				)
			}; Rules
			Genomes: #void			; Directory of genome structures keyed on GenomeKey element of genome structure
			MaxGenomeLen: 60			; Maximum length of GenomeKey. Note: GenomeKey is a vector of reals.
			GenomeSeedKeys: #void		; Vector of GenomeKeys used to generate part of initial population
			Population: #void			; Vector of current genome structures sorted by Score element of genome structure
			MutationPct: .20			; % of population to mutate with each generation
			CrossoverPct: .20			; % of population to breed using crossover with each generation
			NewPct: .10				; % of new, randomly generated members, introduced into population with each generation
			PopulationSize: 50		; Number of members in the population
			ScoreLambda: #void			; Lambda used to score population. Use #void to call default score Lambda.
			ScoreLambdaArgs: #void		; A Structure containing arguments that must be passed to score Lambda.
			});setq state
	
		; Create GE Lambda instance using state
		(setq ge (math.evolveExpr state))

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;; Generate an initial population and
		;; assign it into the state structure.
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		vars:(n
			eS
			genome
			genomes
			populationSize
			population
			genomeKeySeeds
			)
		(setq ge.Genomes (new Directory:))
		;; Initialize see genome keys. These will be used to create a portion of the initial population.
		(setq ge.GenomeKeySeeds #void)
		(setq ge.GenomeKeySeeds #(obj|
			;"       ( ( EarningsPS / SalesPS) * avg(PrjBVGrowth,PrjEPSGrowth))" 	
			#(num| 0 4 3       0 14 1    0 66  1 0 8)
			;"       ( ( Price - min( Low52Week,Price) ) / (  max( High52Week,Price) -  min( Low52Week,Price)))" 
			#(num| 0 3 3  0 40 0 1 1       0 24  0 40    1 3  1 0        0 21  0 40  0  1 1       0 24  0 40)
			;"       ( ( Volume / SharesOutstanding) * ( 1/ PEG))" 
			#(num| 0 4 3   0 74 1              0 67  1 2     36)  
			;"       ( ( Volume / SharesOutstanding ) * ( 1/ PEGLastQtrEPSPctChg))" 
			#(num| 0 3 3   0 74 1              0 67   1 2                     37)
			;"       ( ( Volume / SharesOutstanding ) * ( 1/ PEGPrj5YearGrowth))" 
			#(num| 0 3 3   0 74 1              0 67   1 2                   37)
			;"       ( BookValuePS / Price)" 
			#(num| 0 3        0 10 1  0 66)
			;"       ( ( CashFlowPS - EarningsPS ) / Price)" 
			#(num| 0 3 3       0 77 0       0 14   1  0 40) 
			;"       ( CashPS / Price )" 
			#(num| 0 3   0 12 1  0 40)
			;"       ( EarningsPS / Price)" 
			#(num| 0 3       0 14 1  0 40)
			;"       (EarningsPS / SalesPS)" 
			#(num| 0 3      0 14 1    0 66)
			;"       ( SalesPS / Price)" 
			#(num| 0 3    0 66 1   0 40)
			;"       ( TotalAssetsPS / Price)" 
			#(num| 0 3          0 72 1  0 40)
			;"       ( Value / MarketValue)" 
			#(num| 0 3  0 73 1        0 25)
			;"       (  max(High52Week,Price) /  min(Low52Week,Price))" 
			#(num| 0 3  1 0       0 21  0 40  1  1 1      0 24  0 40)
			))
		(setq genomeKeySeeds ge.GenomeKeySeeds)
		(setq CumGen 0)
		(setq genomes (new Directory:))
		(setq populationSize state.PopulationSize)
		(setq population (new Vector: object: populationSize))
		; Create portion of initial population from known "good" genomes
		(loop for n from 0 until (length genomeKeySeeds) do
			(setq eS (ge._generate genomeKeySeeds[n]))
			;(writeln "GenomeKeySeeds["n"]="eS)
			(setq genomes[eS.Genome] eS)
			(setq population[n] eS)
		);n

		(while (< n populationSize) do ; Create remainder of initial population using mutate
			(setq genome (ge._mutate #void)) ; returns a new expression structure
			(setq eS (ge._generate genome)); generates eS structure
			(if (not (isMember eS.Genome genomes)) (begin
				(setq genomes[eS.Genome] eS) ; genomes is a list of all NewGenomes we have tried.
				(if (<> eS.Expression "") (begin
					(setq population[n] eS); population is the current population we are testing
					(setq n (+ n 1))
					))
				))
		);while n
		(setq ge.Population (copy population))
		(setq population #void); void local copy to save memory
		(setq ge.Genomes (copy genomes))
		(setq genomes #void); void local copy to save memory
		(setq GenomeKeySeeds #void); void local to save memory
		;;;;; Done - Generate an initial population

		; Create a ScoreLambdaArgs structure and assign it to the state structure
		; ScoreLambdaArgs are passed to the Lambda used to score the population. The score Lambda is responsible for
		; implementing the fitness function. In this example, our score Lambda will be scoring the genomes against
		; FASummary tables. The members of the ScoreLambdaArgs structure contain all of the "score Lambda specific" 
		; parametric information necessary for this process. Score Lambdas receive the GE Lambda instance pvar structure
		; as an argument. The pvar structure of the GE Lambda instance will contain the ScoreLambdaArgs member and 
		; thus provide the score Lambda with the parametric information it requires. Note that the score Lambda has
		; access to all of the other GE Lambda instance pvars as well. Score Lambdas will normally use and update the GE 
		; Lambda instance Genome and Population pvar members.

		(setq ge.ScoreLambdaArgs (new Structure: 
			BaseExp: "top (Volume*Price) 400;"
			TrainExp: "Next3MonthProfit"
			TableList: #{FASummary20010302: #("SummaryTablesFA\FASummary20010302.xls" FASummary20010302 #Mar,2,2001 ) 
						FASummary20010601: #("SummaryTablesFA\FASummary20010601.xls" FASummary20010601 #Jun,1,2001 ) 
						FASummary20010831: #("SummaryTablesFA\FASummary20010831.xls" FASummary20010831 #Aug,31,2001 ) 
						FASummary20011130: #("SummaryTablesFA\FASummary20011130.xls" FASummary20011130 #Nov,30,2001 ) 
						FASummary20020301: #("SummaryTablesFA\FASummary20020301.xls" FASummary20020301 #Mar,1,2002 )}
			))

		; Register the score Lambda to be used. _score is the default score Lambda but users may write their own and register it.
		; Use _score as a guide for the development of application specific score Lambdas.
		(setq ge.ScoreLambda _score)

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;;;; Self Test Main Logic 
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		vars:(n 
			ge 					; GE Lambda instance
			numGenerations		; Number of generations to breed
			)
		(setq EchoReport true)
		(setq numGenerations 3)
		(loop for n from 0 until numGenerations do
			(setq CumGen (+ CumGen 1))
			(writeln "Generation " n)
			(if (<> n 0) (ge.breed)); Don't breed the first generation because it has no scores
			(ge.score) ; score the generation
			(ge.report CumGen)
		);n
		;;;;; Done - Self Test Main Logic

		true); selfTest

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; Score - Call the registered score Lambda - by default this is _score
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Args:
	;	none
	; Returns:
	;	value returned by score Lambda - usually a numeric value
	(defun score()
		vars:(p)
		(setq p (myself))
		(ScoreLambda p) ; by default ScoreLambda points to _score
		)

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; _score - default scoring Lambda
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Args:
	;	pvar structure of GE Lambda instance
	; Returns:
	;	value returned by score Lambda - usually a numeric value
	(defun _score(evolveExprInstance)
		vars:(n N t v p P s
			baseExp				; stateVars.BaseExp 
			command				;
			cursor				;
			genScore			;
			low					;
			lowIndex			;
			minTopValues		; A vector containing the smallest value in a topValues vector
			minTopValuesIndex	; A vector cotaining the index for the minTopValue
			numTables			;
			numTop				; The number of top values to keep for each population expression
			popExpVal			; The value returned from executing a population expression on a row
			population			;
			record				; The current record from cursor
			rowXY				;
			startIndex			; First member of population that needs to be scored. Previously scored members need not be rescored!
			stateVars
			tableList			; stateVars.TableList
			trainExp			; stateVars.TrainExp
			topValues			; A vector of vectors containing the top numTop values for each population expression
			topScores			; A vector of vectors containing the trainExpVal associated with each value in the TopValues vector
			trainExpVal			; The value returned from executing trainExp on a row
			)

		; Get local references to the parametric values required
		; by the score function. Note that different score Lambdas
		; can have very different parametric score information
		; and this information is always isolated in the ScoreLambdaArgs
		; structure. This architecture makes the score Lambda signature
		; a constant and allows polymorphic behavior throughout the rest
		; of the GE Lambda. 
		(setq stateVars evolveExprInstance.Pv)
		(setq population stateVars.Population)
		(setq tableList stateVars.ScoreLambdaArgs.TableList)
		(setq baseExp stateVars.ScoreLambdaArgs.BaseExp)
		(setq trainExp stateVars.ScoreLambdaArgs.TrainExp)
		(setq numTables (length tableList))

		;; Clear existing scores in genome
		(setq N (length population))
		(loop for n from 0 until N do
			(setq population[n].Score #void) ; set score to zero
			(setq population[n].WinPct #void)
		);n
(writeln "numTables=" numTables)
		(loop for t from 0 until numTables do
			(display ".")
			(setq cursor (dataMineLib.open tableList[t 0] memory:))
			;Find first unscored member in population
			(setq P (length population))
			(setq p 0)
;			(while (and (<> population[p].Score #void) (< p P)) (setq p (+ p 1)))
;			(setq startIndex (integer p))
;			(if (= startIndex P) (return false))

(setq startIndex 0)
			;; Reduce the table rows to the subset specified by baseExp - ie highly liquid stocks
			(cursor.run baseExp)	
			(setq numTop 10)
			;; Initialize score keeping vectors
			(setq topValues (new Vector: object: P))
			(loop for p from startIndex until P do (setq topValues[p] (new Vector: number: numTop BIGNEGNUM)))
			(setq minTopValues (new Vector: number: P BIGNEGNUM))
			(setq minTopValuesIndex (new Vector: short: P 0))
			(setq topScores (new Vector: object: P))
			(loop for p from startIndex until P do (setq topScores[p] (new Vector: number: numTop BIGNEGNUM)))
			
			;; Construct the filter command that will update the SpecialSituationNotes field in the table with
			;; the calculated dependent and independent values
			(setq command (append "setnr SpecialSituationNotes new('Vector','number'," (+ P 1))) 
			(loop for p from startIndex until P do 
					(setq command (append command "," population[p].Expression))
			);p
			(setq command (append command "," trainExp ");"))
			;; Extract raw data from cursor into XY vector array.
			(cursor.run command) ;; Fills the special situation notes with results of all expressions

			;; Make a pass through the cursor scoring the top numTop 
			;; rows for each expressing in the population.
			(setq N cursor.recordCount)
			(loop for n from 0 until N do
				(setq record (refObjVector cursor.rowVector n))
				(setq rowXY record.SpecialSituationNotes); assign local for speed
				(setq trainExpVal (refNumVector rowXY P)); the train expression follows population expressions
				(loop for p from startIndex until P do ; for each population expression
					(setq popExpVal (refNumVector rowXY p))
					; Check if popExpVal is larger than largest value currently in topValues vector
					(if (> popExpVal minTopValues[p]) (begin
						(setq topValues[p][minTopValuesIndex[p]] popExpVal)
						(setq topScores[p][minTopValuesIndex[p]] trainExpVal)
						;; Find new lowest value in topValues list
						(setq low BIGPOSNUM)
						(setq lowIndex 0)
						(loop for s from 0 until numTop do
							(setq v topValues[p][s])
							(if (< v low) (begin
								(setq low v)
								(setq lowIndex s)
								))
						);t
						(setq minTopValues[p] low)
						(setq minTopValuesIndex[p] lowIndex)
						))
				);p
			);n
			;; Add scores to population history
			(loop for p from startIndex until P do
				(setq v 0)
				(loop for s from 0 until numTop do
					(setq v (+ v topScores[p][s]))
				);t
				(setq population[p].Score (+ population[p].Score (/ v numTop)))
			;(writeln "topScores["p"]=" population[p].Score)
			);p

			;(score cursor population baseExp trainExp)
			(setq cursor (dataMineLib.close cursor))
		);t

		;; Rank population
		(setq genScore 0)
		(setq N (length population))
		(loop for n from 0 until N do
			(setq population[n].Score  (/ population[n].Score numTables))
			(setq genScore (+ genScore population[n].Score))
		);n
		; Store a sorted COPY of the population vector back into the pvars of the
		; calling GE Lambda instance.
		(setq stateVars.Population (sort population (lambda (x y) (> x.Score y.Score))))
	(return genScore))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; report - report on current population
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Args:
	;	none
	; Returns:
	; 	true
	(defun report(gen)
		vars:(n N population rec s)
		(if (= Report #void) (setq Report (new Vector: object:)))
		(setq population Population); get reference to pvar for faster access
		(setq N (length population))
		(loop for n from 0 until N do
			(setq rec population[n])
			(setq Report[(length Report)] (new Vector: 4 (integer gen) (/ (integer (* rec.Score 1000)) 1000) rec.UseCount (copy rec.Expression)))
			(if EchoReport (writeln Report[(length Report)]))
		);n
	true)


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; breed - Perform mutate and crossover functions on the current population
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; breed uses the current "state" of the GE Lambda instance carried in the
	; pvars of the Lambda's pvars.
	; Args:
	;	none
	; Returns:
	; 	true
	(defun breed()
		vars:( g n N
			eS
			eSnew
			g1
			g2
			keepPct		
			numTables
			populationSize
			replaceIndex
			)
		;; Note: Variables used/updated in parent Lambda pvars
		;; Population
		;; CrossoverPct
		;; MutationPct
		;; NewPct
		;; Genomes

		(setq populationSize (length Population))

		;; Mutate top half of good performers to replace bottom half
		(setq keepPct (- 1 MutationPct CrossoverPct NewPct))
		(setq replaceIndex (integer (* keepPct populationSize)))
	
		; Do New - this represents new competition arriving in the habitat
		(setq N (integer (* NewPct populationSize)))
		(loop for n from 0 until N do
			(setq eSnew (_generate (_mutate #void)))
			(while (or (isMember eSnew.Genome Genomes) (= eSnew.Expression "")) do
				(setq eSnew (_generate (_mutate eS.Genome)))
			);while
			(setq Genomes[eSnew.Genome] eSnew)
			(setq Population[replaceIndex] eSnew)
;(writeln "new n="replaceIndex " " eSnew)
			(setq replaceIndex (+ replaceIndex 1))
		);n

		; Do mutations
		(setq N (integer (* MutationPct populationSize)))
		(loop for n from 0 until N do
			(setq eS Population[n])
			(setq eSnew (_generate (_mutate eS.Genome)))
			(while (or (isMember eSnew.Genome Genomes) (= eSnew.Expression "")) do
				(setq eSnew (_generate (_mutate eS.Genome)))
			);while
			(setq Genomes[eSnew.Genome] eSnew)
			(setq Population[replaceIndex] eSnew)
;(writeln "mutation n="replaceIndex " " eSnew)
			(setq replaceIndex (+ replaceIndex 1))
		);n
		; Do crossovers
		(setq N (integer (* CrossoverPct populationSize)))
		(loop for n from 0 until N by 2 do
			(setq g1 (copy Population[n].Genome))
			(setq g2 (copy Population[(+ n 1)].Genome))
			(_crossover g1 g2)
			;; Generate valid crossover product using mutation if necessary
			(setq eSnew (_generate g1))
			(while (or (isMember eSnew.Genome Genomes) (= eSnew.Expression "")) do
				(setq eSnew (_generate (_mutate eS.Genome)))
			);while
			(setq eSnew.Score #void)
			(setq eSnew.WinPct #void)
			(setq Genomes[eSnew.Genome] eSnew)
			(setq Population[replaceIndex] eSnew)
;(writeln "crossover1 n=" replaceIndex " " eSnew)
			(setq replaceIndex (+ replaceIndex 1))					

			(if (= replaceIndex populationSize) (goto DONE:))
			(setq eSnew (_generate g2))
			(while (or (isMember eSnew.Genome Genomes) (= eSnew.Expression "")) do
				(setq eSnew (_generate (_mutate eS.Genome)))
			);while
			(setq eSnew.Score #void)
			(setq eSnew.WinPct #void)
			(setq Genomes[eSnew.Genome] eSnew)
			(setq Population[replaceIndex] eSnew)
;(writeln "crossover2 n=" replaceIndex " " eSnew)
			(setq replaceIndex (+ replaceIndex 1))

		);n
		DONE::
		true)

	(defun _generate(genome)
		vars:(result)
		(setq _Gp 0)				; Gp is a parent Lambda pvar
		(setq _Genome (copy genome)); _Genome is a parent Lambda pvar
		(setq _NewGenome (new Vector: number: 0))  ; NewGenome is a parent Lambda pvar
		(setq _Expression "")		; Expression is a parent Lambda pvar
		(_generate2 Expr:)
		(setq _Gp 0)
		(setq _Genome (copy _NewGenome)) 	; The new Genome contains fixups for introns etc.
		(setq _NewGenome (new Vector: number: 0))
		(setq _Expression "")
		(_generate2 Expr:) 		; _generate again to get the final expression
		(setq result (new Structure: Genome: (copy _Genome) Expression: (copy _Expression)))
		result)

	(defun _generate2(currentRule)
		vars:(n N i k j J 
			args
			argsBeg
			argBeg
			argEnd
			argNum
			dynamicRule
			element
			elementItem
			elementItemValue
			ruleTerm
			ruleTermIndex
			rule
			ruleIndex
			originalExpressionLength
			originalNewGenomeLength			
			numValue
			)

		(setq originalExpressionLength (length _Expression))
		(setq originalNewGenomeLength (length _NewGenome))

		(if (isSymbol currentRule) 
			(setq rule Rules[currentRule])
			(setq rule currentRule))

		(setq N (length rule))
		(setq n _Genome[_Gp])
		(setq ruleIndex (modi n N))
		(setq ruleTerm rule[ruleIndex])		
		(setq _NewGenome[_Gp] ruleIndex)

		(cond
			((= ruleTerm Integer:) ;; Found Integer term
				(setq _Gp (+ _Gp 1))
				(if (>= _Gp (length _Genome)) (goto BAIL:))
				(setq numValue (integer _Genome[_Gp]))
				(if (= numValue 0) (goto BAIL:))
				(setq _NewGenome[_Gp] numValue)
				(setq _Expression (append _Expression (string _NewGenome[_Gp])))
				(if (= (right _Expression 2) "/1") (goto BAIL:)) 
				(if (= (right _Expression 2) "*1") (goto BAIL:)) 
				)

			((= ruleTerm Real:) ;; Found Real number term
				(setq _Gp (+ _Gp 1))
				(if (>= _Gp (length _Genome)) (goto BAIL:))
				(setq numValue Genome[Gp])
				(if (= numValue 0.0) (goto BAIL:))
				(setq _NewGenome[_Gp] numValue)
				(setq _Expression (append _Expression (string _NewGenome[_Gp])))
				)

			((isString ruleTerm) ;; Found a literal, add it to expression
				(setq _Expression (append _Expression ruleTerm))
				(if (= (right _Expression 2) "/1") (goto BAIL:)) 
				(if (= (right _Expression 2) "*1") (goto BAIL:)) 
				(if (= (right _Expression 2) "1*") (goto BAIL:)) 
				(if (= (right _Expression 7) "abs(abs") (goto BAIL:))
				)				

			((isSymbol ruleTerm) ;; Found another rule name
				(setq _Gp (+ _Gp 1))
				(if (>= _Gp (length _Genome)) (goto BAIL:))
				(_generate2 ruleTerm)
				)

			((isStructure ruleTerm) ;; Found field schema
				(setq _Expression (append _Expression ruleTerm.Expression))
				(cond 
					((= ruleTerm.Type Structure:) ; Handle field that contains a structure
						(setq _Gp (+ _Gp 1))
						(if (>= _Gp (length _Genome)) (goto BAIL:))
						(_generate2 ruleTerm.Elements)
					)
					((= ruleTerm.Type NumberVector:) ; Handle field that contains a number vector
						;Generate vector of index strings
						(setq J ruleTerm.Length)
						(setq dynamicRule (new Vector: (- J 1)))
						(loop for j from 0 until J do
							(setq dynamicRule[j] (append "[" j "]"))
						);j

						(setq _Gp (+ _Gp 1))
						(if (>= _Gp (length _Genome)) (goto BAIL:))
						(_generate2 dynamicRule)
					)
				)
				
				);; End of field schema

			((isVector ruleTerm) ;; Found rule vector 
				;; Process each rule element of the rule vector
				(setq N (length ruleTerm))
				(setq args (new Vector: 0)); to keep track of beg & end of arguments for Abenial shuffle and equality check
				(setq argsBeg (new Vector: 0))
				(setq argNum 0)
				(loop for n from 0 until N do
					(setq ruleElement ruleTerm[n])
					(cond 
						((= ruleElement Integer:) ;; Found Integer term
							(setq _Gp (+ _Gp 1))
							(if (>= _Gp (length _Genome)) (goto BAIL:))
							(setq numValue (integer _Genome[_Gp]))
							(if (= numValue 0) (goto BAIL:))
							(setq args[argNum] numValue)
							(setq argNum (+ argNum 1))
							(setq _NewGenome[_Gp] numValue)
							(setq _Expression (append _Expression (string _NewGenome[_Gp])))
							(if (= (right _Expression 2) "/1") (goto BAIL:)) 
							(if (= (right _Expression 2) "*1") (goto BAIL:)) 
							)

						((= ruleElement Real:) ;; Found Real number term
							(setq _Gp (+ _Gp 1))
							(if (>= _Gp (length _Genome)) (goto BAIL:))
							(setq numValue _Genome[_Gp])
							(if (= numValue 0.0) (goto BAIL:))
							(setq args[argNum] numValue)
							(setq argNum (+ argNum 1))
							(setq _NewGenome[_Gp] numValue)
							(setq _Expression (append _Expression (string _NewGenome[_Gp])))
							)
			
						((isString ruleElement) ;; Found a literal, add it to expression
							(setq _Expression (append _Expression ruleElement))
							(if (= (right _Expression 2) "/1") (goto BAIL:))
							(if (= (right _Expression 2) "*1") (goto BAIL:))
							(if (= (right _Expression 2) "1*") (goto BAIL:))
							(if (= (right _Expression 7) "abs(abs") (goto BAIL:))
							)

						((isSymbol ruleElement) ;; Found another rule name
							(setq _Gp (+ _Gp 1))
							(setq argBeg _Gp)
							(setq argsBeg[argNum] argBeg)
							(if (>= _Gp (length _Genome)) (goto BAIL:))
							(if (= (_generate2 ruleElement) false) (goto BAIL:))
							(setq k 0)
							(setq args[argNum] (new Vector: number: 0))
							(setq argEnd _Gp)
							(loop for i from argBeg to argEnd do ;; collect argument section in genome
								(setq args[argNum][k] _NewGenome[i])
								(setq k (+ k 1))
							);i
							(setq argNum (+ argNum 1))
							(if (= (right _Expression 2) "/1") (goto BAIL:)) 
							(if (= (right _Expression 2) "*1") (goto BAIL:)) 
							(if (= (right _Expression 2) "1*") (goto BAIL:)) 
							)
						
					);cond
				);n
				;; Intron Checks
				(cond 
					((isMember Operator: ruleTerm) ; 
						(if (compareEQ args[0] args[2]) (begin
							(goto BAIL:) ; Sales-Sales Sales/Sales etc.
							))
					)
					((isMember BinaryFunc: ruleTerm) ;
						(if (compareEQ args[1] args[2]) (begin
							(goto BAIL:) ; max(Sales,Sales) etc.
							))
					)
					((isMember AbenialOperator: ruleTerm)
						; Check if we should do swap of arguments
						; Are both arguments vectors of the same length and of type Field?
						; Should the fields be reversed?
						(if (and (isVector args[0])
								(isVector args[2])
								(= (length args[0]) (length args[2]))
								(= Rules.Rule[args[0][0]][0] Field:)
								(= Rules.Rule[args[2][0]][0] Field:)
								(> args[0][1] args[2][1])) (begin
								(setq k argsBeg[2])
								(loop for i from 0 until (length args[0]) do
									(setq _NewGenome[k] args[0][i])
									(setq k (+ k 1))
								)
								(setq k argsBeg[0])
								(loop for i from 0 until (length args[2]) do
									(setq _NewGenome[k] args[2][i])
									(setq k (+ k 1))
								)
							))
					)
				
				);cond						

				);(isVector ruleTerm)
		);cond

		(return true)

		BAIL::
		(setq _Expression (left _Expression originalExpressionLength))
		(setq _NewGenome (resize _NewGenome originalNewGenomeLength))

		false); _generate

	;; Crossover modifies genome1 and genome2 in place.
	(defun _crossover(genome1 genome2)
		vars:(i n N r len maxLen modLen)
		(setq maxLen (min (length genome1) (length genome2)))
		(setq modLen (integer (random (/ maxLen 2))))
		(if (< modLen 1) (return false))
		(setq modStart (integer (random (- (length genome1) maxLen))))
		(setq N (+ modStart modLen))
		(loop for i from modStart until N do
			(setq n genome2[i])
			(setq genome2[i] genome1[i])
			(setq genome1[i] n)
		);i
		true);mutate

	;; Mutate returns a new genome. It does not modify the genome
	;; referenced by the argument.
	(defun _mutate(genome)
		vars:(n N r newGenome)
		(setq newGenome (copy genome))
		(if (= newGenome #void) (setq newGenome (new Vector: number:  0)))
		(setq N (length newGenome)) ; modify a value in an existing part of the genome
		(if (> N 1) (begin
			(setq n (integer (random (- N 1))))
			(setq r (random 64000))
			(setq newGenome[n] r)
			))
		(loop for n from N until MaxGenomeLen do ; fill remainder of genome with random values
			(setq r (random 64000))
			(setq newGenome[n] r)
		)
		newGenome);mutate

	;; Assign seleted pvars to the structure argument s. This routine allows the "data" portion
	;; of the GE instance to be extracted for external store. Note that no Lambdas
	;; are included in this copy. Also note that pvars with names begining with the _ character
	;; are exclueded.
	(defun getMyPvars(...)
		vars:(p n N s)
		(setq p (myself))
		(setq p p.Pv)
		(setq N (length p))
		(if (= (argCount) 1)
			(setq s (argFetch 0))
			(setq s (new Structure:)))
		(loop for n from 0 until N do
			(if (not (isLambda p[n])) (begin
				(if (<> (left (string p[n 0]) 1) "_")
					(setq s[p[n 0]] (copy p[n])))
				))
		); n
		s) ;; end of getMyPvars

	 ;; Assign selected items from the structure s into this Lambda's pvars. This routine allows the "data"
	 ;; portion of a ge instance to be initialized from some external store. Note that this
	 ;; copy is governed by the existance of the target Pv element in  this Lambda's pvars. Other values
	 ;; stored in the structure argument s will not be copied into the  this Lambda's pvars structure.
	 ;; Note: Lambdas will never be assigned into this Lambda's pvars by this routine.
	 (defun setMyPvars(s)
		vars: (i n N p)
		(setq p (myself))
		(setq p p.Pv)
		(setq N (length p))
		(loop for n from 0 until N do
			(if (not (isLambda p[n])) (begin
				(setq i (member p[n 0] s))
				(if (isNumber i)
					(setq p[n 1] (copy s[i 1])))
				))
		)
		true); end of setMyPvars

;; Main Section
	vars:( k i N g s
		count
		gS	
		genomes
		(populationSize 100)
		(maxCount 100000)
		ge
		)

	(if (= (argCount) 0) (begin
		(writeln "Error: ScreenGE5 - missing argument on create")
		(return false)
		))

	(setq ge (new (myself))); Create and return a new Lambda instance
	(ge.setMyPvars (argFetch 0))
	(return ge)
	); evolveExpr













;;**EXPORTKEY**:math:gaussianEliminate
(defriend math:gaussianEliminate(w)
;; *******************************************************************
;; name:     gaussianEliminate
;; 
;; summary:  Triangulates the M by M+1 coefficient array representing 
;;           a system of M simultaneous linear equations in M variables.
;; Parms:    w:       The M by M+1 coefficient array.
;; Return:   w:       The M by M+1 array after triangulation.
;; Note:     See Sedgewick[2] chap 37.
;; Note:     This Lambda fudges around singular points.
;; *******************************************************************
    vars:(i j k m m+1 maxRow tmp)
    ;; Begin main logic.
    (setq m+1 (length w[0]))
    (setq m (length w))
    (if (= m+1 (1+ m))
        (loop for i from 0 until m do
            (setq maxRow i)
            (loop for j from (1+ i) until m do
                (if (> (abs w[j][i]) (abs w[maxRow][i]))
                    (setq maxRow j)))
            (setq tmp w[i])
            (setq w[i] w[maxRow])
            (setq w[maxRow] tmp)
            (loop for j from (1+ i) until m do
                (loop for k from m to i by -1 do
		            ;; Fudge around singular condition iff we would be dividing by zero.
		            ;; Note: This occurs whenever the matrix is singular and a slight fudge
		            ;;       introduces very little error. 
                    (if (= w[i][i] 0.0)
                        (if (= w[j][i] 0.0)
                            (setq w[j][k] (- w[j][k] (* w[i][k] 0)))
                            (setq w[j][k] (- w[j][k] (* w[i][k] (/ (1+ w[j][i]) (1+ w[i][i]))))))
                        (setq w[j][k] (numCheck (- w[j][k] (* w[i][k] (/ w[j][i] w[i][i]))))))
                    (if (not (isNumber w[j][k]))
                        (setq w[j][k] 0)
                        ;(error
                        ;  (append "gaussianEliminate: w[j][k]=" w[j][k] 
                        ;           ",w[i][k]=" w[i][k] 
                        ;           ",w[j][i]=" w[j][i] 
                        ;           ",w[i][i]=" w[i][i] 
                        ;           ",i=" i ",j=" j ",k=" k))
                        ))))
        (error "!gaussianEliminate: wrong size array!"))
    w) ; end gaussianEliminate












































































;;**EXPORTKEY**:math:gaussianSubstitute
(defriend math:gaussianSubstitute(w)
;; *******************************************************************
;; name:     gaussianSubstitute
;; 
;; summary:  Returns the M coefficient vector from a triangulated array
;;           representing a system of M simultaneous linear equations in 
;;           M variables.
;; Parms:    w:       The M by M+1 triangulated array.
;; Return:   x:       The M coefficient vector.
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    vars:(j k m m+1 sum x)
    ;; Begin main logic.
    (setq m+1 (length w[0]))
    (setq m (length w))
    (setq x (new Vector: m 0))
    (if (= m+1 (1+ m))
        (loop for j from (-1+ m) to 0 by -1 do
            (setq sum 0)
            (loop for k from (1+ j) until m do
                (setq sum (numCheck (+ sum (* w[j][k] x[k]))))
                (if (not (isNumber sum))
                    (error
                       (append "gaussianSubstitute: sum=" sum 
                                ",w[j][k]=" w[j][k] 
                                ",x[k]=" x[k] 
                                ",j=" j ",k=" k))))
            ;; Fudge around singular condition iff we would be dividing by zero.
            ;; Note: This occurs whenever the matrix is singular and a slight fudge
            ;;       introduces very little error. 
            (if (= w[j][j] 0.0)
                (if (= w[j][m] 0.0)
                    (setq x[j] 0.0)
                    (setq x[j] (/ (1+ (- w[j][m] sum)) (1+ w[j][j]))))
                (setq x[j] (numCheck (/ (- w[j][m] sum) w[j][j]))))
            (if (not (isNumber x[j]))
                (setq x[j] 0)
                ;(error
                ;   (append "gaussianSubstitute: sum=" sum 
                ;            ",w[j][k]=" w[j][k] 
                ;            ",x[k]=" x[k] 
                ;            ",j=" j ",k=" k))
                ))
        (error "!gaussianSubstitute: wrong size array!"))
    x) ; end gaussianSubstitute







































































;;**EXPORTKEY**:math:linearRegress
(defriend math:linearRegress(XY ...)
;; *******************************************************************
;; name:     linearRegress
;; 
;; summary:  Returns a vector containing the coefficients resulting
;;           from a linear regression on two variables. If x and y 
;;           are two variables, then (regression w) is: #(a  b  error).
;;           where a + bx = y represents the least squares best fit.
;;           The term, error, is the least squares error = sqr(y - (a + bx)).
;; Parms:    XY:      The N by 2 array representing the original observations
;;                    in the form of:    x  y
;;                                       x  y
;;                                        ... 
;;                                       x  y
;;           Y        (Optional) Y number vector of dependent variables
;; Return:   C:       The coefficient vector #(num| a b error)
;; *******************************************************************
	regs:(M n N Number:InvN)
	regs:(Number:numerator Number:denominator Number:a Number:b Number:err)
	regs:(Number:xmean Number:xsum Number:x)
	regs:(Number:ymean Number:ysum Number:y)
	regs:(Number:xxdot Number:yydot Number:xydot)
	regs:(NumPointer:pX NumPointer:pY)
	vars:(NumVector:C NumVector:X NumVector:Y)
	(setq C (|Gv:new| Vector: Number: 3))
	(setq N (length XY))
    ;; Note: If X and Y arguments are passed, both must be NumVectors!
    (if (= (argCount) 2) (begin (setq X XY) (setq Y (argFetch 1)) (goto Regress:)))
    ;; Note: If an XY argument is passed, it must be a Vector of NumVectors!
	(setq M (length XY[0]))
	(if (<> M 2) (error "linearRegress: XY array must be M by 2"))
	(setq X (new Vector: Number: N))
	(setq Y (new Vector: Number: N))
    (loop for n from 0 until N do
       (setq X[n] XY[n][0])
       (setq Y[n] XY[n][1])    
       ) ; end loop
    ;; Note: We now have X and Y NumVectors to work with.
    Regress::
    (setq N (length X))
    (setq InvN (/ 1.0 (number N)))
    (setq pX X)
    (setq pY Y)
    (loop for n from 0 until N do
       (setq x pX[n])
       (setq y pY[n])
       (+= xsum x)
       (+= ysum y)
       (+= xxdot (* x x))
       (+= xydot (* x y))
       (+= yydot (* y y))
       ) ; end main loop
    (setq xmean (/ xsum N)) 
    (setq ymean (/ ysum N)) 
   	(setq numerator (- xydot (* ysum xmean)))
    (setq denominator (- xxdot (/ (* xsum xsum) N)))
   	(if (= denominator 0.0) (setq b 0.0) (setq b (/ numerator denominator)))
   	(setq a (- ymean (* b xmean)))
    (setq err (+ yydot (* -2.0 b xydot) (* -2.0 a ysum) (* b b xxdot) (* 2.0 a b xsum) (* a a N) ))
   	(setq C[0] a)
   	(setq C[1] b)
    (setq C[2] (* InvN err))
	C) ; linearRegress












;;**EXPORTKEY**:math:makeGaussianArray
(defriend math:makeGaussianArray(w)
;; *******************************************************************
;; name:     makeGaussianArray
;; 
;; summary:  Returns the M by M+1 system of linear equations representing
;;           the coefficient derivative equations for the least squares
;;           error fit.
;; Parms:    w:       The N by M+1 array representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;; Return:   x:       The M by M+1 array containing the dot products of the column 
;;                    vectors of the original observation array w,
;;                    where: x[i,j] = vectorDotProduct(wcol[i],wcol[j]);
;;
;; Note1:    See Sedgewick[2] chap 38.
;; Note2:    The Gaussian array represents the coefficients of the derrivatives
;;           of the least squares conversion of the original observation
;;           array in preparation for least squares regression.
;; *******************************************************************
    vars:(i j k m M M+1 n N x sum)
    ;; Begin main logic.
    (setq w (convertToArray w))
    (setq x (new Vector: (sub1 (length w[0]))))
    (setq N (length w))
    (setq M+1 (length w[0]))
    (setq M (sub1 M+1))
    (loop for i from 0 until M do
       (setq x[i] (new Vector: number: M+1))
       (loop for j from 0 until M+1 do
           (begin
              (setq sum 0)
              (loop for k from 0 until N do
                  (+= sum (numCheck (* (numCheck w[k][i]) (numCheck w[k][j]))))
                  (if (not (isNumber sum))
                      (setq sum 0)
                      ))
              (setq x[i][j] sum))))
    x) ; end makeGaussianArray












































































;;**EXPORTKEY**:math:makeGaussianMatrix
(defriend math:makeGaussianMatrix(A)
;; *******************************************************************
;; name:     makeGaussianMatrix
;; 
;; summary:  Returns the M by M+1 system of linear equations representing
;;           the coefficient derivative equations for the least squares
;;           error fit.
;; Parms:    A:       The N by M+1 matrix representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;; Return:   G:       The M by M+1 matrix containing the dot products of the column 
;;                    vectors of the original observation matrix A,
;;                    where: G[i,j] = vectorDotProduct(colA[i],colA[j]);
;;
;; Note1:    See Sedgewick[2] chap 38.
;; Note2:    The Gaussian matrix represents the coefficients of the derrivatives
;;           of the least squares conversion of the original observation
;;           array in preparation for least squares regression.
;; *******************************************************************
    vars:(i j k m M M+1 n N G sum rankA)
    ;; Begin main logic.
    (setq A (convertToMatrix A))
    ;; Use the built-in makeGaussianMatrix function (if possible).
    (if (isFunction |Gv:makeGaussianMatrix|) (return (|Gv:makeGaussianMatrix| A)))
    ;; Compute the Gaussian matrix.
    (setq rankA (rank A))
    (setq N rankA[0])
    (setq M+1 rankA[1])
    (setq M (sub1 M+1))
    (setq G (new Matrix: number: 2 M M+1))
    (loop for i from 0 until M do
       (loop for j from 0 until M+1 do
           (begin
              (setq sum 0)
              (loop for k from 0 until N do
                  (+= sum (numCheck (* (numCheck A[k i]) (numCheck A[k j]))))
                  (if (not (isNumber sum))
                      (setq sum 0)
                      ))
              (setq G[i j] sum))))
    G) ; end makeGaussianMatrix












































































;;**EXPORTKEY**:math:makeGramArray
(defriend math:makeGramArray(w ...)
;; *******************************************************************
;; name:     makeGramArray
;; 
;; summary:  Returns the N by N array containing the dot products 
;;           of the input vectors, known as the Gramm array.
;;
;; Parms:    w:         The N by M+1 array representing the original observations
;;                      in the form of:    x x ... x y
;;                                         x x ... x y
;;                                             ... 
;;                                         x x ... x y
;;           includeY   (Optional)True iff the y entries are to be included (default is true).
;; Return:   g:         The N by N array containing the dot products of the row 
;;                      vectors of the original observation array w,
;;                      where: x[i,j] = vectorDotProduct(wrow[i],wrow[j]);
;;
;; Note:     See Cristianini, "Support Vector Machines", page 169.
;; *******************************************************************
    vars:(i j k m M n N g sum includeY)
    ;; Begin main logic.
    (setq w (convertToArray w))
    (if (>= (argCount) 2) (setq includeY (argFetch 1)) (setq includeY true))
    (setq N (length w))
    (setq M (if (= includeY true) (length w[0]) (subi (length w[0]) 1))) 
    (setq g (new Vector: N))
    (loop for i from 0 until N do
       (setq g[i] (new Vector: number: N))
       ) ; end i loop
    (loop for i from 0 until N do
       (loop for j from i until N do
          (setq sum 0)
          (loop for k from 0 until M do
              (+= sum (numCheck (* (numCheck w[i][k]) (numCheck w[j][k]))))
              (if (not (isNumber sum))
                  (setq sum 0)
                  ) ; end if
              ) ; end k loop
          (setq g[i][j] sum)
          (setq g[j][i] sum)
          ) ; end j loop
       ) ; end i loop
    g) ; end makeGramArray












































































;;**EXPORTKEY**:math:makeGramMatrix
(defriend math:makeGramMatrix(A ...)
;; *******************************************************************
;; name:     makeGramMatrix
;; 
;; summary:  Returns the N by N matrix containing the dot products 
;;           of the input vectors, known as the Gramm array.
;;
;; Parms:    A:         The N by M+1 matrix representing the original observations
;;                      in the form of:    x x ... x y
;;                                         x x ... x y
;;                                             ... 
;;                                         x x ... x y
;;           includeY   (Optional)True iff the y entries are to be included (default is true).
;; Return:   G:         The N by N matrix containing the dot products of the row 
;;                      vectors of the original observation matrix A,
;;                      where: x[i,j] = vectorDotProduct(rowA[i],rowA[j]);
;;
;; Note:     See Cristianini, "Support Vector Machines", page 169.
;; *******************************************************************
    vars:(i j k m M n N G sum includeY rankA)
    ;; Begin main logic.
    (if (>= (argCount) 2) (setq includeY (argFetch 1)) (setq includeY true))
    (setq A (convertToMatrix A))
    ;; Use the built-in makeGramMatrix function (if possible).
    (if (= includeY true) (return (^makeGramMatrix A)))
    ;; Use the NAG library (if available).
    (if (and (= nagLibrary true) (= includeY true)) (return (^matrixMultiply aABpbC: TransposeB: A A)))
    ;; Make the Gram matrix for the input matrix.
    (setq rankA (rank A))
    (setq N rankA[0])
    (setq M (if (= includeY true) rankA[1] (subi rankA[1] 1))) 
    (setq G (new Matrix: number: 2 N N))
    (loop for i from 0 until N do
       (loop for j from i until N do
          (setq sum 0)
          (loop for k from 0 until M do
              (+= sum (numCheck (* (numCheck A[i k]) (numCheck A[j k]))))
              (if (not (isNumber sum))
                  (setq sum 0)
                  ) ; end if
              ) ; end k loop
          (setq G[i j] sum)
          (setq G[j i] sum)
          ) ; end j loop
       ) ; end i loop
    G) ; end makeGramMatrix





















;;**EXPORTKEY**:math:matrixColDotProducts
(defriend math:matrixColDotProducts(A)
;; *******************************************************************
;; name:     matrixColDotProducts
;; 
;; summary:  Returns the M by M matrix containing the dot products 
;;           of all possible pairs of columns in the input matrix,
;;           which is equivalent to:
;;  
;;                            transpose(A)*A.
;;
;; Parms:    A:         The N by M matrix, N rows, M columns,
;;                      in the form of:    x x ... x
;;                                         x x ... x
;;                                             ... 
;;                                         x x ... x
;; Return:   G:         The M by M matrix containing the dot products of the column 
;;                      vectors of the original observation matrix A,
;;                      where: x[i,j] = vectorDotProduct(colA[i],colA[j]);
;;
;; Note:     See Cristianini, "Support Vector Machines", page 169.
;; *******************************************************************
    regs:(i j k m M n N)
    regs:(NumPointer:Ap NumPointer:Gp)
    regs:(Number:aki Number:akj Number:sum Number:prod)
    vars:(G rankA)
    ;; Begin main logic.
    (if (not (isMatrix A)) (setq A (convertToMatrix A)))
    ;; Make the product matrix from the input matrix.
    (setq rankA (rank A))
    (setq N rankA[0])
    (setq M rankA[1]) 
    (setq G (new Matrix: number: 2 M M))
    (setq Ap A)
    (setq Gp G)
    (loop for i from 0 until M do
       (loop for j from i until M do
          (setq sum 0.0)
          (loop for k from 0 until N do
              (setq aki Ap[(+ (* M k) i)])
              (setq akj Ap[(+ (* M k) j)])
              (setq prod (* aki akj)) 
              (setq prod (numCheck prod)) 
              (+= sum prod)
              (setq sum (numCheck sum))
              ) ; end k loop
          (setq Gp[(+ (* M i) j)] sum)
          (setq Gp[(+ (* M j) i)] sum)
          ) ; end j loop
       ) ; end i loop
    G) ; end matrixColDotProducts





















;;**EXPORTKEY**:math:matrixCrossCorrelation
(defriend math:matrixCrossCorrelation(X)
;; *******************************************************************
;; name:     matrixCrossCorrelation
;; 
;; summary:  Returns The N by (M*M) cross correlation matrix containing
;;           all possible cross correlations of the original input
;;           features. 
;;
;; Parms:    X:         The N by M matrix representing the original observations
;;                      in the form of:    x x ... x x
;;                                         x x ... x x
;;                                             ... 
;;                                         x x ... x x
;; Return:   C:         The N by (M*M) matrix containing all possible cross
;;                      correlations of the original observations in the
;;                      form of X[n i]*X[n j] for all i,j in M.
;;
;; Note:     See Cristianini, "Support Vector Machines", page 169.
;; *******************************************************************
    regs:(i j m M MM n N)
    regs:(Number:xni Number:xnj Number:prod)
    regs:(NumPointer:Xp NumPointer:Cp)
    vars:(C rankX)
    ;; Begin main logic.
    (setq X (convertToMatrix X))
    ;; Make the cross correlation matrix for the input matrix.
    (setq rankX (rank X))
    (setq N rankX[0])
    (setq M rankX[1]) 
    (setq MM (* M M))
    (setq C (new Matrix: Number: 2 N MM))
    (setq Xp X)
    (setq Cp C)
    (loop for n from 0 until N do
       (setq m 0)
       (loop for i from 0 until M do
          (loop for j from 0 until M do
             (setq xni Xp[(+ (* M n) i)])
             (setq xnj Xp[(+ (* M n) j)])
             (setq prod (* xni xnj))
             (setq prod (numCheck prod))
             (setq Cp[(+ (* n MM) m)] prod)
             (++ m)
             ) ; end i loop
          ) ; end j loop
       ) ; end n loop
    C) ; end matrixCrossCorrelation















;;**EXPORTKEY**:math:matrixDiagonal
(defriend math:matrixDiagonal(NumVector:V)
;; *******************************************************************
;; name:     matrixDiagonal
;; 
;; summary:  Returns the N by N diagonal matrix with the specified 
;;           numbers only along the diagonal and zero's everywhere else.
;;
;; Parms:    diagonal:  The number of rows and columns in the identity matrix
;;
;; Return:   D:         The N by N diagonal matrix, 
;;                      in the form of:    v 0 ... 0 0
;;                                         0 v ... 0 0
;;                                             ... 
;;                                         0 0 ... v 0
;;                                         0 0 ... 0 v
;;
;; *******************************************************************
    regs:(n N)
    regs:(NumPointer:Dp NumPointer:Vp)
    regs:(Number:Diag)
    vars:(I)
    ;; Begin main logic.
    (setq N (length V))
    (setq D (new Matrix: Number: 2 N N))
    (setq Dp D)
    (setq Vp V)
    (loop for n from 0 until N do (setq Diag Vp[n]) (setq Dp[(+ (* N n) n)] Diag))
    D) ; end matrixDiagonal












































































;;**EXPORTKEY**:math:matrixDualGramRegress
(defriend math:matrixDualGramRegress(X Y)
;; *******************************************************************
;; summary:  Returns the dense N coefficient vector giving the coefficients
;;           with the best least squares fit of the variables, in dual Gram
;;           matrix form. 
;;
;; Parms:    X:       The N by M array representing the original observations
;;                    in the form of:    x x ... x
;;                                       x x ... 
;;                                           ... 
;;                                       x x ... x
;;           Y:       A separate vector containing the dependent values.
;; Return:   C:       The N+1th coefficient vector (with N+1th = stdError, avgError, minError, maxError, avgY).
;; Note:     See Cristianini, "Support Vector Machines" chap 2.
;; *******************************************************************
    vars:(Xt C Y avgY G U n m M N ey absEy stdErr avgErr minErr maxErr rankX)
    ;; Perform a least squares regression on all the factors.
    (if (or (= X #void) (<= (length X) 0)) (return (new Vector: number:)))
    (setq avgY (avg Y))
    (setq X (convertToMatrix X))
    (setq G (|Gv:makeGramMatrix| X Y))
    (setq U (matrixGaussianEliminate G))
    (setq C (matrixGaussianSubstitute U))
    ;; Compute the least squares regression error.
    (setq minErr BIGPOSNUM)
    (setq maxErr 0)
    (setq N (length C))
    (loop for n from 0 until N do
       (setq ey 0)
       (loop for m from 0 until N do
          (setq ey (+ ey (* C[m] G[n m])))
          ) ; end M loop
       (setq ey (- Y[n] ey))
       (setq absEy (abs ey))
       (setq avgErr (+ avgErr absEy))
       (setq minErr (min absEy minErr))
       (setq maxErr (max absEy maxErr))
       (setq stdErr (+ stdErr (* ey ey)))
       ) ; end N loop
    (setq avgErr (/ avgErr N))
    (setq stdErr (/ stdErr N))
    ;; Return the coefficient vector with the error in the M+1st slot.
    (setq C[(length C)] stdErr)
    (setq C[(length C)] avgErr)
    (setq C[(length C)] minErr)
    (setq C[(length C)] maxErr)
    (setq C[(length C)] avgY)
    C) ; end matrixDualGramRegress















;;**EXPORTKEY**:math:matrixDualGramRegressC
(defriend math:matrixDualGramRegressC(X Y)
;; *******************************************************************
;; summary:  Returns the dense N coefficient vector giving the coefficients
;;           with the best least squares fit of the variables, in dual Gram
;;           matrix form, with constant term inserted before the first column. 
;;
;; Parms:    X:       The N by M+1 array representing the original observations
;;                    in the form of:    x x ... x
;;                                       x x ... 
;;                                           ... 
;;                                       x x ... x
;;           Y:       A separate vector containing the dependent values.
;; Return:   C:       The N+1th coefficient vector (with N+1th = stdError, avgError, minError, maxError, avgY).
;; Note:     See Cristianini, "Support Vector Machines" chap 2.
;; *******************************************************************
    vars:(C)
    ;; Perform a least squares regression on all the factors.
    (if (or (= X #void) (<= (length X) 0)) (return (new Vector: number:)))
    (setq X (convertToMatrixC X))
    ;; Perform a least squares regression on all the factors.
    (setq C (matrixDualGramRegress X Y))
    C) ; end matrixMultipleRegressC





















;;**EXPORTKEY**:math:matrixFillColumn
(defriend math:matrixFillColumn(NumMatrix:A Integer:m x)
;; *******************************************************************
;; name:     matrixFillColumn
;; 
;; summary:  Destructively fills the mth column vector with the fill value.
;;           The fill value may be a scalar or a vector.
;;
;; Parms:    A:       An N by M matrix.
;;           m:       The mth column vector to fill.
;;           x:       The value to fill (either a NumVvector or a Number).
;;
;; Return:   A:       The filled input matrix.
;; *******************************************************************
    regs:(m M n N Number:c)
    regs:(NumPointer:Ap NumPointer:Cp)
    vars:(rankA NumVector:C)
    (setq A (convertToMatrix A))
    (setq rankA (rank A))
    (setq N rankA[0])
    (setq M rankA[1])
    (setq Ap A)
    ;; Check the arguments for validity.
    (if (or (< m 0) (>= m M)) (error "math.matrixFillColumn: invalid fill column index"))
    (if (and (not (isNumber x)) (<> (type x) NumVector:)) (error "math.matrixFillColumn: fill constant must be either a Number or a NumVector"))
    ;; Fill the column with the proper values.
    (cond
       ;; Fill the column with a number.
       ((isNumber x)
        (begin
          (setq c x)
          (loop for n from 0 until N do (setq Ap[(+ (* n M) m)] c))
        )) ; end fill value is a Number
       ;; Fill the column with a vector.
       (else
        (begin
          (setq C x) 
          (setq Cp C)
          (if (<> N (length C)) (error "math.matrixFillColumn: fill vector must be same length as matrix"))
          (loop for n from 0 until N do (setq Ap[(+ (* n M) m)] Cp[n]))
        )) ; end fill value is a Number Vector
       ) ; end cond
    A) ; end matrixFillColumn



















































;;**EXPORTKEY**:math:matrixFillRow
(defriend math:matrixFillRow(NumMatrix:A Integer:n x)
;; *******************************************************************
;; name:     matrixFillRow
;; 
;; summary:  Destructively fills the nth row with the fill value.
;;           The fill value may be a scalar or a vector.
;;
;; Parms:    A:       An N by M matrix.
;;           n:       The mth row to fill.
;;           x:       The value to fill (either a NumVector or a Number).
;;
;; Return:   A:       The filled input matrix.
;; *******************************************************************
    regs:(m M n N Number:c)
    regs:(NumPointer:Ap NumPointer:Cp)
    vars:(rankA NumVector:C)
    (setq A (convertToMatrix A))
    (setq rankA (rank A))
    (setq N rankA[0])
    (setq M rankA[1])
    (setq Ap A)
    ;; Check the arguments for validity.
    (if (or (< n 0) (>= n N)) (error "math.matrixFillRow: invalid fill row index"))
    (if (and (not (isNumber x)) (<> (type x) NumVector:)) (error "math.matrixFillRow: fill constant must be either a Number or a NumVector"))
    ;; Fill the rows with the proper values.
    (cond
       ;; Fill the row with a number.
       ((isNumber x)
        (begin
          (setq c x)
          (loop for m from 0 until M do (setq Ap[(+ (* n M) m)] c))
        )) ; end fill value is a Number
       ;; Fill the row with a vector.
       (else
        (begin
          (setq C x) 
          (setq Cp C)
          (if (<> M (length C)) (error "math.matrixFillRow: fill vector must be same width as matrix"))
          (loop for m from 0 until M do (setq Ap[(+ (* n M) m)] Cp[m]))
        )) ; end fill value is a Number Vector
       ) ; end cond
    A) ; end matrixFillRow



















































;;**EXPORTKEY**:math:matrixGaussian
(defriend math:matrixGaussian(A)
;; *******************************************************************
;; name:     matrixGaussian
;; 
;; summary:  Returns the M by M+1 matrix representing the coefficient 
;;           derivative equations for the best least squares
error fit 
;;           for the system of linear equations.
;;
;; Parms:    A:       The N by M+1 matrix representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;
;; Return:   G:       The M by M+1 matrix containing the dot products of the column 
;;                    vectors of the original observation matrix A,
;;                    where: G[i,j] = vectorDotProduct(colA[i],colA[j]);
;;
;; Note1:    See Sedgewick[2] chap 38.
;; Note2:    The Gaussian matrix represents the coefficients of the derrivatives
;;           of the least squares conversion of the original observation
;;           array in preparation for least squares regression.
;; *******************************************************************
    vars:(i j k m M M1 n N G sum rankA)
    ;; Begin main logic.
    (setq A (convertToMatrix A))
    ;; Use the built-in makeGaussianMatrix function (if possible).
    (if (isFunction |Gv:makeGaussianMatrix|) (return (|Gv:makeGaussianMatrix| A)))
    ;; Compute the Gaussian matrix.
    (setq rankA (rank A))
    (setq N rankA[0])
    (setq M1 rankA[1])
    (setq M (- M1 1))
    (setq G (new Matrix: number: 2 M M1))
    (loop for i from 0 until M do
       (loop for j from 0 until M1 do
           (begin
              (setq sum 0.0)
              (loop for k from 0 until N do
                  (+= sum (numCheck (* (numCheck A[k i]) (numCheck A[k j]))))
                  (if (not (isNumber sum))
                      (setq sum 0.0)
                      ))
              (setq G[i j] sum))))
    G) ; end matrixGaussian












































































;;**EXPORTKEY**:math:matrixGaussianEliminate
(defriend math:matrixGaussianEliminate(A ...)
;; *******************************************************************
;; name:     matrixGaussianEliminate
;; 
;; summary:  Triangulates the M by M+1 coefficient matrix representing 
;;           a system of M simultaneous linear equations in M variables.
;; Parms:    A:       The M by M+1 coefficient matrix.
;; Return:   A:       The M by M+1 matrix after triangulation.
;; Note1:    See Sedgewick[2] chap 37, uses row swapping.
;; Note2:    This Lambda implements an approximal form of gaussian 
;;           elimination by fudging around singular conditions where
;;           dividing by zero would be a problem.
;; *******************************************************************
    vars:(i j k M M+1 rankA maxRow tmp destroySW)
    ;; Begin main logic.
    (setq destroySW (if (= (argCount) 2) (argFetch 1) false))
    (setq A (convertToMatrix A))
    ;; Use the built-in makeGaussianMatrix function (if possible).
    (if (isFunction |Gv:matrixGaussianEliminate|) (return (|Gv:matrixGaussianEliminate| A destroySW)))
    ;; Triangulate the M by M+1 coefficient matrix.
    (setq rankA (rank A))
    (setq M+1 rankA[1])
    (setq M rankA[0])
    (if (and (= (length rankA) 2) (= M+1 (add1 M)))
        (loop for i from 0 until M do
            (setq maxRow i)
            (loop for j from (add1 i) until M do
                (if (> (abs A[j i]) (abs A[maxRow i])) (setq maxRow j))
                ) ; end j loop
            (loop for j from 0 until M+1 do
                (setq tmp A[i j])
                (setq A[i j] A[maxRow j])
                (setq A[maxRow j] tmp)
                ) ; end j loop
            (loop for j from (1+ i) until M do
                (loop for k from M to i by -1 do
		            ;; Fudge around singular condition iff we would be dividing by zero.
		            ;; Note: This occurs whenever the matrix is singular and a slight fudge
		            ;;       introduces very little error. 
                    (if (= A[i i] 0.0)
                        (if (= A[j i] 0)
                            (setq A[j k] (numCheck (- A[j k] A[i k])))
                            (setq A[j k] (numCheck (- A[j k] (* A[i k] (/ (1+ A[j i]) (1+ A[i i]))))))
                            ) ; end if
                        (setq A[j k] (numCheck (- A[j k] (* A[i k] (/ A[j i] A[i i])))))
                        ) ; end if
                    (if (not (isNumber A[j k]))
                        (setq A[j k] 0)
                        )
                    ) ; end k loop
                 ) ; end j loop
            ) ; end i loop
        (error "!matrixGaussianEliminate: wrong size matrix!")
        ) ; end if
    A) ; end matrixGaussianEliminate












































































;;**EXPORTKEY**:math:matrixGaussianSolve
(defriend math:matrixGaussianSolve(A Y)
;; *******************************************************************
;; name:     matrixGaussianSolve
;; 
;; summary:  Solves the M by M+1 system of M simultaneous linear 
;;           equations in M variables, and returns the M vector 
;;           containing the coefficients for each of the M variables. 
;;
;; Parms:    A:       The M by M matrix of independent values.
;;           Y:       The M vector of dependet values.
;; Return:   C:       The M vector of linear coefficients solving the system.
;; Note:     See Sedgewick[2] chap 37, uses pivoting.
;; *******************************************************************
    vars:(C U)
    ;; Begin main logic.
    (setq A (convertToMatrix A Y))
    (setq U (matrixGaussianEliminate A))
    (setq C (matrixGaussianSubstitute U))
    C) ; end matrixGaussianSolve















;;**EXPORTKEY**:math:matrixGaussianSubstitute
(defriend math:matrixGaussianSubstitute(A)
;; *******************************************************************
;; name:     matrixGaussianSubstitute
;; 
;; summary:  Returns the M coefficient vector from a triangulated array
;;           representing a system of M simultaneous linear equations in 
;;           M variables.
;; Parms:    A:       The M by M+1 triangulated array.
;; Return:   C:       The M coefficient number vector.
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    vars:(j k M M+1 sum C rankA)
    ;; Begin main logic.
    (setq A (convertToMatrix A))
    ;; Use the built-in matrixGaussianSubstitute function (if possible).
    (if (isFunction |Gv:matrixGaussianSubstitute|) (return (|Gv:matrixGaussianSubstitute| A)))
    ;; Perform the Gaussian substitution.
    (setq rankA (rank A))
    (setq M+1 rankA[1])
    (setq M rankA[0])
    (setq C (new Vector: number: M 0))
    (if (and (= (length rankA) 2) (= M+1 (add1 M)))
        (loop for j from (subi M 1) to 0 by -1 do
            (setq sum 0)
            (loop for k from (add1 j) until M do
                (setq sum (numCheck (+ sum (* A[j k] C[k]))))
                (if (not (isNumber sum))
                    (error
                       (append "matrixGaussianSubstitute: sum=" sum 
                                ",A[j k]=" A[j k] 
                                ",C[k]=" C[k] 
                                ",j=" j ",k=" k))
                    ) ; end if
                ) ; end k loop
            ;; Fudge around singular condition iff we would be dividing by zero.
            ;; Note: This occurs whenever the matrix is singular and a slight fudge
            ;;       introduces very little error. 
            (if (= A[j j] 0.0)
                (if (= A[j M] 0)
                    (setq C[j] 0)
                    (setq C[j] (/ (1+ (- A[j M] sum)) (add1 A[j j])))
                    ) ; end if
                (setq C[j] (numCheck (/ (- A[j M] sum) A[j j])))
                ) ; end if
            (if (not (isNumber C[j]))
                (setq C[j] 0)
                ) ; end if
            ) ; end j loop
        (error "!matrixGaussianSubstitute: wrong size array!")
        ) ; end if
    C) ; end matrixGaussianSubstitute







































































;;**EXPORTKEY**:math:matrixIdentity
(defriend math:matrixIdentity(Integer:Size ...)
;; *******************************************************************
;; name:     matrixIdentity
;; 
;; summary:  Returns the N by N identity matrix with 1's only 
;;           along the diagonal and zero's everywhere else.
;;
;; Parms:    Size:      The number of rows and columns in the identity matrix
;;           Diag:      (Optional)Alternative diagonal constant
;; Return:   I:         The N by N identity matrix, 
;;                      in the form of:    1 0 ... 0 0
;;                                         0 1 ... 0 0
;;                                             ... 
;;                                         0 0 ... 1 0
;;                                         0 0 ... 0 1
;;
;; *******************************************************************
    regs:(n N)
    regs:(NumPointer:Ip)
    regs:((Number:Diag 1.0))
    vars:(I)
    ;; Begin main logic.
    (setq N Size)
    (if (= (argCount) 2) (setq Diag (number (argFetch 1))))
    (setq I (new Matrix: Number: 2 N N))
    (setq Ip I)
    (loop for n from 0 until N do (setq Ip[(+ (* N n) n)] Diag))
    I) ; end matrixIdentity












































































;;**EXPORTKEY**:math:matrixInvert
(defriend math:matrixInvert(A)
;; *******************************************************************
;; name:     matrixInvert
;; 
;; summary:  Returns the inversion of the N by N input matrix using 
;;           gaussian LU decomposition, without pivoting, to reduce round 
;;           off errors.
;;
;; Parms:    A:       The M by M matrix.
;;
;; Return:   AI:      The M by M matrix after inversion.
;;
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    regs:(i j k M N )
    regs:(NumPointer:Wp NumPointer:Up)
    regs:(Number:uji Number:uii Number:ujk Number:uik Number:wji (Number:fudge 1.0))
    vars:(rankU NumVector:AI NumVector:L NumVector:U NumVector:UI NumVector:W)
    ;; Begin main logic.
    (setq U (copyToMatrix A))
    (setq rankU (rank U))
    (setq N rankU[0])
    (setq M rankU[1])
    (setq L (matrixIdentity M))
    (if (or (<> (length rankU) 2) (<> N M)) (error "math.matrixInvert: expecting a square matrix"))
    ;; Compute lower, L, and upper, U, components of A.
    ;; Note: The lower component is a lower triangular matrix.
    ;;       The upper component is an upper triangular matrix.
    (setq Up U)
    (loop for i from 0 until M do
        (setq W (matrixIdentity M))
        (setq Wp W)
        (loop for j from (+ 1 i) until M do
            (setq uji Up[(+ (* M j) i)])
            (setq uii Up[(+ (* M i) i)])
            (if (= uii 0.0)
                (if (= uji 0.0)
                    (setq wji -1.0)
                    (setq wji (- 0.0 (/ uji (+ fudge uii))))                   
                    ) ; end if
                (setq wji (- 0.0 (/ uji uii)))
                ) ; end if
            (setq wji (numCheck wji))
            (setq Wp[(+ (* M j) i)] wji)
            (loop for k from (- M 1) to i by -1 do
	            ;; Fudge around singular condition iff we would be dividing by zero.
	            ;; Note: This occurs whenever the matrix is singular, then we use a slight fudge,
	            ;;       which introduces very little error, but allows an approximate inverse. 
                (setq uji Up[(+ (* M j) i)])
                (setq ujk Up[(+ (* M j) k)])
                (setq uii Up[(+ (* M i) i)])
                (setq uik Up[(+ (* M i) k)])
                (if (= uii 0.0)
                    (if (= uji 0)
                        (setq ujk (- ujk (* uik 0.0)))
                        (setq ujk (- ujk (* uik (/ 1.0 (+ fudge uii)))))
                        ) ; end if
                    (setq ujk (numCheck (- ujk (* uik (/ uji uii)))))
                    ) ; end if
                (if (not (isNumber ujk)) (setq ujk 0.0))
                (setq Up[(+ (* M j) k)] ujk)
                ) ; end k loop
             ) ; end j loop
        (setq L (matrixMultiply W L))        
        ) ; end i loop
    ;; Compute the inverse of the upper triangular matrix, U.
    (setq UI (matrixInvertUpperTriangular U))
    ;; Compute the inverse of the input matrix A.
    (setq AI (matrixMultiply UI L))
    AI) ; end matrixInvert











































































;;**EXPORTKEY**:math:matrixInvertUpperTriangular
(defriend math:matrixInvertUpperTriangular(U)
;; *******************************************************************
;; name:     matrixInvertUpperTriangular
;; 
;; summary:  Returns the inversion of the M by M upper triangular input 
;;           matrix, using gaussian substitution.
;;
;; Parms:    U:       The M by M matrix (upper triangular).
;; Return:   UI:      The M by M matrix after inversion (also upper triangular).
;;
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    regs:(i j k M M1 N)
    regs:(NumPointer:Wp NumPointer:Up NumPointer:UIp NumPointer:Cp)
    regs:(Number:cij Number:w)
    vars:(NumMatrix:AI NumMatrix:L NumMatrix:UI NumVector:W NumMatrix:C)
    ;; Begin main logic.
    (setq U (convertToMatrix U))
    (setq rankU (rank U))
    (setq N rankU[0])
    (setq M rankU[1])
    (setq M1 (+ M 1))
    (if (or (<> (length rankU) 2) (<> N M)) (error "math.matrixInvertUpperTriangular: expecting a square matrix"))
    (setq UI (new Matrix: Number: 2 M M))
    ;; Create the bare coefficients matrix.
    ;; Note: We use gaussian substitution to solve the inverse
    ;;       matrix as a series of simultaneous linear equations.
    (setq C (new Matrix: Number: 2 M M1))
    (setq Cp C)
    (setq Up U)
    (setq UIp UI)
    (loop for i from 0 until M do
        (loop for j from 0 until M do
            (setq cij Up[(+ (* (- M j 1) M) (- M i 1))])
            (setq Cp[(+ (* i M1) j)] cij)
            ) ; end j loop
        ) ; end i loop
    ;; Add the dependent variable for the first row of the inversion.
	(loop for j from M until 0 by -1 do
	    (loop for i from 0 until M do
	        (if (= i (- j 1)) (setq Cp[(+ (* i M1) M)] 1.0) (setq C[(+ (* i M1) M)] 0.0))
	        ) ; end i loop
	    (setq W (matrixGaussianSubstitute C))
        (setq Wp W)
	    (loop for k from 0 until M do
	       (setq w Wp[(- M k 1)])
	       (setq UIp[(+ (* (- M j) M) k)] w)
	       ) ; end k loop
	    ) ; end j loop
    UI) ; end matrixInvertUpperTriangular

























;;**EXPORTKEY**:math:matrixLower
(defriend math:matrixLower(A)
;; *******************************************************************
;; name:     matrixLower
;; 
;; summary:  Returns the lower triangular component matrix of the M by M 
;;           input matrix using gaussian LU decomposition, without pivoting, 
;;           to reduce round off errors.
;;
;; Parms:    A:       The M by M matrix.
;; Return:   L:       The M by M lower triangular matrix component.
;;
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    regs:(i j k M N maxRow (Number:fudge 1.0))
    regs:(NumPointer:Up NumPointer:WIp)
    regs:(Number:uii Number:uji Number:uik Number:ujk Number:wji)
    vars:(rankU NumMatrix:L NumMatrix:U NumMatrix:WI)
    ;; Begin main logic.
    (setq U (copyToMatrix A))
    (setq rankU (rank U))
    (setq N rankU[0])
    (setq M rankU[1])
    (if (or (<> (length rankU) 2) (<> N M)) (error "math.matrixLower: expecting a square matrix"))
    (setq L (matrixIdentity M))
    ;; Begin creation of lower triangular matrix component.
    (setq Up U)
    (loop for i from 0 until M do
        (setq WI (matrixIdentity M))
        (setq WIp WI)
        (loop for j from (add1 i) until M do
            (setq uii Up[(+ (* i N) i)])
            (setq uji Up[(+ (* j N) i)])
	        (if (= uii 0.0)
	            (if (= uji 0)
	                (setq wji -1.0)
	                (setq wji (/ (+ fudge uji) (+ fudge uii)))
	                ) ; end if
	            (setq wji (- (/ uji uii)))
	            ) ; end if
            (setq wji (numCheck wji))
            (setq WIp[(+ (* j N) i)] wji)	
            (loop for k from (- M 1) to i by -1 do
	            ;; Fudge around singular condition iff we would be dividing by zero.
	            ;; Note: This occurs whenever the matrix is singular, then we use a slight fudge,
	            ;;       which introduces very little error, but allows an approximate inverse. 
                (setq uii Up[(+ (* i N) i)])
                (setq uji Up[(+ (* j N) i)])
                (setq uik Up[(+ (* i N) k)])
                (setq ujk Up[(+ (* j N) k)])
                (if (= uii 0.0)
                    (if (= uji 0)
                        (setq ujk (- ujk (* uik 0.0)))
                        (setq ujk (- ujk (* uik (/ (+ fudge uji) (+ fudge uii)))))
                        ) ; end if
                    (setq ujk (numCheck (- ujk (* uik (/ uji uii)))))
                    ) ; end if
                (if (not (isNumber ujk)) (setq ujk 0.0))
                (setq Up[(+ (* j N) k)] ujk)
                ) ; end k loop
             ) ; end j loop
        (setq L (matrixMultiply WI L))
        ) ; end i loop
    L) ; end matrixLower











































































;;**EXPORTKEY**:math:matrixMultipleRegress
(defriend math:matrixMultipleRegress(X ...)
;; *******************************************************************
;; summary:  Returns the dense M+1 coefficient vector giving the coefficients
;;           with the best least squares fit of the variables.
;;
;; Parms:    X:       The N by M+1 matrix representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;           Y:       (Optional)A separate vector contains the dependent values.
;;
;; Return:   C:       The M+1 coefficient vector (with M+1th = stdError, avgError, minError, maxError, avgY).
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    regs:(m n M N)
    vars:(Xt C Y avgY ey absEy stdErr avgErr minErr maxErr rankX)
    ;; Perform a least squares regression on all the factors.
    (if (or (= X #void) (<= (length X) 0)) (return (new Vector:)))
    (if (= (argCount) 2)
        (setq X (convertToMatrix X (setq Y (argFetch 1))))
        (setq X (convertToMatrix X))
        ) ; end if 
    (setq Xt (|Gv:makeGaussianMatrix| X))
    (setq Xt (|Gv:matrixGaussianEliminate| Xt true))
    (setq C (|Gv:matrixGaussianSubstitute| Xt))
    ;; Compute the least squares regression error.
    (setq minErr BIGPOSNUM)
    (setq maxErr 0)
    (setq N (setq rankX (rank X))[0])
    (setq M (subi rankX[1] 1))
    (loop for n from 0 until N do
       (setq ey 0)
       (loop for m from 0 until M do
          (setq ey (+ ey (* C[m] X[n m])))
          ) ; end M loop
       (setq avgY (+ avgY (/ X[n M] N)))
       (setq ey (- X[n M] ey))
       (setq absEy (|Gv:abs| ey))
       (setq avgErr (+ avgErr (/ absEy N)))
       (setq minErr (|Gv:min| absEy minErr))
       (setq maxErr (|Gv:max| absEy maxErr))
       (setq stdErr (+ stdErr (/ (* ey ey) N)))
       ) ; end N loop
    ;; Return the coefficient vector with the std error in the Mth slot.
    (setq C[(length C)] stdErr)
    (setq C[(length C)] avgErr)
    (setq C[(length C)] minErr)
    (setq C[(length C)] maxErr)
    (setq C[(length C)] avgY)
    C) ; end matrixMultipleRegress















;;**EXPORTKEY**:math:matrixMultipleRegressC
(defriend math:matrixMultipleRegressC(X ...)
;; *******************************************************************
;; summary:  Returns the dense M coefficient vector giving the coefficients
;;           with the best least squares fit of the variables. A constant
;;           term is added to the input matrix.
;;
;; Parms:    X:       The N by M+1 array representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;           Y:       (Optional)A separate vector contains the dependent values.
;;
;; Return:   C:       The M+1 coefficient vector (with N+1th = stdError, avgError, minError, maxError, avgY).
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    vars:(Xt C Y i j M N err ey rankX)
    ;; Load and convert the arguments to the proper format.
    (if (or (= X #void) (<= (length X) 0)) (return (new Vector:)))
    (if (= (argCount) 2) 
        (setq X (convertToMatrixC X (setq Y (argFetch 1))))
        (setq X (convertToMatrixC X))
        ) ; end if
    ;; Perform a least squares regression on all the factors.
    (setq C (matrixMultipleRegress X))
    C) ; end matrixMultipleRegressC





















;;**EXPORTKEY**:math:matrixMultipleRegression
(defriend math:matrixMultipleRegression(X ...)
;; *******************************************************************
;; summary:  Returns the dense M+1 coefficient vector giving the coefficients
;;           with the best least squares fit of the variables.
;;
;; Parms:    X:       The N by M+1 matrix representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;           Y:       (Optional)A separate vector contains the dependent values.
;;
;; Return:   C:       The M coefficient vector.
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    vars:(Xt C Y)
    ;; Perform a least squares regression on all the factors.
    (if (or (= X #void) (<= (length X) 0)) (return (new Vector:)))
    (if (= (argCount) 2)
        (setq X (convertToMatrix X (setq Y (argFetch 1))))
        (setq X (convertToMatrix X))
        ) ; end if 
    (setq Xt (|Gv:makeGaussianMatrix| X))
    (setq Xt (|Gv:matrixGaussianEliminate| Xt true))
    (setq C (|Gv:matrixGaussianSubstitute| Xt))
    ;; Return the coefficient vector of the multiple regression.
    C) ; end matrixMultipleRegression















;;**EXPORTKEY**:math:matrixMultipleRegressionC
(defriend math:matrixMultipleRegressionC(X ...)
;; *******************************************************************
;; summary:  Returns the dense M coefficient vector giving the coefficients
;;           with the best least squares fit of the variables. A constant
;;           term is added to the input matrix.
;;
;; Parms:    X:       The N by M+1 array representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;           Y:       (Optional)A separate vector contains the dependent values.
;;
;; Return:   C:       The M+1 coefficient vector (with an axis constant inserted in the 0th position).
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    vars:(Xt C Y i j M N err ey rankX)
    ;; Load and convert the arguments to the proper format.
    (if (or (= X #void) (<= (length X) 0)) (return (new Vector:)))
    (if (= (argCount) 2) 
        (setq X (convertToMatrixC X (setq Y (argFetch 1))))
        (setq X (convertToMatrixC X))
        ) ; end if
    ;; Perform a least squares regression on all the factors.
    (setq C (matrixMultipleRegression X))
    C) ; end matrixMultipleRegressionC





















;;**EXPORTKEY**:math:matrixMultiply
(defriend math:matrixMultiply(A B)
;; *******************************************************************
;; name:     matrixMultiply
;; 
;; summary:  Returns The N by M matrix product from multiplying A times B, 
;;           where A is maximally an N by K matrix, and B is maximally a
;;           K by M matrix.
;; 
;;           Minimally the A component may be a scalar, a vector, a row vector, 
;;           a column vector, a vector array, or a matrix. In each case,
;;           the appropriately shaped product result will be returned.
;;
;; Parms:    A:       The N by K matrix representing the left matrix
;;           B:       The K by M matrix representing the right matrix
;; Return:   AB:      The N by M matrix containing the dot products of the row 
;;                    vectors of the left matrix A, with the column vectors
;;                    of the right matrix B,
;;                    where: AB[m,n] = vectorDotProduct(rowA[m],colB[n]);
;;
;; Note1:    See Sedgewick[2] chap 38.
;; *******************************************************************
    regs:(k K m M n N)
    regs:(Number:sum Number:prod)
    regs:(NumPointer:ABp NumPointer:Ap NumPointer:Bp)
    regs:(Number:An Number:Bn)
    regs:(Number:ab Number:a Number:b)
    vars:(rankA rankB NumMatrix:AB)
    ;; Perform conversion to a proper matrix (if necessary). 
    (setq A (convertToMatrix A))
    (setq B (convertToMatrix B))
    ;; Perform matrix multiply for each type of matrix.
    (cond
       ;; Manage the case for A is a scalar and B is a scalar
	   ;; Parms:    A:       A scalar
	   ;;           B:       A scalar
	   ;; Return:   AB:      The scalar product of A and B
       ((and (isNumber A) (isNumber B)) 
        (begin
	      (setq AB (* A B))
	    )) ; end case for A is a scalar and B is a scalar

       ;; Manage the case for A is a scalar and B is a matrix
	   ;; Parms:    A:       A scalar
	   ;;           B:       A matrix
	   ;; Return:   AB:      The scalar product of A and B
       ((and (isNumber A) (isMatrix B)) 
        (begin
          (setq N (length B))
          (setq AB (copy B))
          (setq ABp AB)
          (setq Bp B)
          (setq An A)
          (loop for n from 0 until N do
	        (setq ABp[n] (* An Bp[n]))
            ) ; end loop
	    )) ; end case for A is a scalar and B is a matrix

       ;; Manage the case for B is a matrix and B is a scalar
	   ;; Parms:    A:       A matrix
	   ;;           B:       A scalar
	   ;; Return:   AB:      The scalar product of A and B
       ((and (isMatrix A) (isNumber B)) 
        (begin
          (setq N (length A))
          (setq AB (copy A))
          (setq ABp AB)
          (setq Ap A)
          (setq Bn B)
          (loop for n from 0 until N do
	        (setq ABp[n] (* Ap[n] Bn))
            ) ; end loop
	    )) ; end case for A is a matrix and B is a scalar

       ;; Manage the case for A is a matrix and B is a matrix
	   ;; Parms:    A:       The M by K matrix representing the left matrix
	   ;;           B:       The K by N matrix representing the right matrix
	   ;; Return:   AB:      The M by N matrix containing the dot products of the row 
	   ;;                    vectors of the left matrix A, with the column vectors
       ;;                    of the right matrix B,
       ;;                    where: AB[m,n] = vectorDotProduct(rowA[m],colB[n]);
       ((and (isMatrix A) (isMatrix B) (= (length (setq rankA (rank A))) 2) (= (length (setq rankB (rank B))) 2)) 
        (begin
	      (if (<> rankB[0] (setq K rankA[1])) (error "math.matrixMultiply: number of cols in A must equal number of rows in B"))
	      (setq N rankA[0])
	      (setq M rankB[1])
	      (setq AB (new Matrix: number: 2 N M))
          (setq Ap A)
          (setq Bp B)
          (setq ABp AB)
	      (loop for n from 0 until N do
	         (loop for m from 0 until M do
	            (setq sum 0.0)
	            (loop for k from 0 until K do
	               (setq a Ap[(+ (* n K) k)])
	               (setq b Bp[(+ (* k M) m)])
                   (setq prod (* a b))
                   (setq prod (numCheck prod))
	               (+= sum prod)
	               (if (not (isNumber sum)) (setq sum 0.0))
	               ) ; end K loop
	            (setq ABp[(+ (* n M) m)] sum)
	            ) ; end N loop
	         ) ; end M loop
	    )) ; end case for A is a matrix and B is a matrix

       ;; Manage all other cases as errors
       (else (error "math.matrixMultiply: illegal type combination of A or B")) 

       ) ; end matrix type cond
    ;; Convert result to a scalar (if appropriate)
    (if (= (length AB) 1) (setq AB AB[0]))
    AB) ; end matrixMultiply












































































;;**EXPORTKEY**:math:matrixNormalize
(defriend math:matrixNormalize(A)
;; *******************************************************************
;; summary:  Normalizes the column values in a matrix. Each matrix
;;           cell value is normalized by finding the high and low
;;           range for the value's column, then value is converted 
;;           into a fraction (0 ... 1) of it's column's high-low range.
;; 
;;           Minimally the A component may be a scalar, a vector, a row vector, 
;;           a column vector, a vector array, or a matrix. In each case,
;;           the appropriately shaped product result will be returned.
;;
;; Parms:    A:       The N by M matrix to be normalized
;; Return:   AN:      The N by M matrix with normalized columns
;;
;; Note1:    See Sedgewick[2] chap 38.
;; *******************************************************************
    regs:(m M n N)
    regs:(Number:a Number:high Number:low Number:range)
    regs:(NumPointer:Ap NumPointer:ANp)
    vars:(NumMatrix:AN rankA)
    ;; If A is a scalar, then it is already normalized.
    (if (isNumber A) (return A))
    (setq A (convertToMatrix A))
    ;; If A is a matrix, then normalize its columns.
    (setq rankA (rank A))
    (setq N rankA[0])
    (setq M rankA[1])
    (setq AN (new Matrix: Number: 2 N M))
    (setq Ap A)
    (setq ANp AN)
    (loop for m from 0 until M do
        (setq high BIGNEGNUM)
        (setq low BIGPOSNUM)
        (loop for n from 0 until N do
            (setq a Ap[(+ (* n M) m)])
            (if (< high a) (setq high a))
            (if (> low a) (setq low a))
            ) ; end n loop
        (setq range (- high low))
        (if (= range 0.0)
            (loop for n from 0 until N do
                (setq ANp[(+ (* n M) m)] 1.0)
                ) ; end n loop
            (loop for n from 0 until N do
                (setq ANp[(+ (* n M) m)] (/ (- Ap[(+ (* n M) m)] low) range))
                ) ; end n loop
            ) ; end if
        ) ; end m loop
    AN) ; end matrixNormalize








































































;;**EXPORTKEY**:math:matrixRowDotProducts
(defriend math:matrixRowDotProducts(A)
;; *******************************************************************
;; name:     matrixRowDotProducts
;; 
;; summary:  Returns the N by N matrix containing the dot products 
;;           of all possible pairs of rows in the input matrix,
;;           which is equivalent to:
;;  
;;                            A*transpose(A).
;;
;; Parms:    A:         The N by M matrix, N rows, M columns,
;;                      in the form of:    x x ... x
;;                                         x x ... x
;;                                             ... 
;;                                         x x ... x
;; Return:   G:         The N by N matrix containing the dot products of the row 
;;                      vectors of the original observation matrix A,
;;                      where: x[i,j] = vectorDotProduct(rowA[i],rowA[j]);
;;
;; Note:     See Cristianini, "Support Vector Machines", page 169.
;; *******************************************************************
    regs:(i j k m M n N)
    regs:(NumPointer:Ap NumPointer:Gp)
    regs:(Number:aik Number:ajk Number:sum Number:prod)
    vars:(G rankA)
    ;; Begin main logic.
    (if (not (isMatrix A)) (setq A (convertToMatrix A)))
    ;; Make the product matrix from the input matrix.
    (setq rankA (rank A))
    (setq N rankA[0])
    (setq M rankA[1]) 
    (setq G (new Matrix: number: 2 N N))
    (setq Ap A)
    (setq Gp G)
    (loop for i from 0 until N do
       (loop for j from i until N do
          (setq sum 0.0)
          (loop for k from 0 until M do
              (setq aik Ap[(+ (* M i) k)])
              (setq ajk Ap[(+ (* M j) k)])
              (setq prod (* aik ajk)) 
              (setq prod (numCheck prod)) 
              (+= sum prod)
              (setq sum (numCheck sum))
              ) ; end k loop
          (setq Gp[(+ (* N i) j)] sum)
          (setq Gp[(+ (* N j) i)] sum)
          ) ; end j loop
       ) ; end i loop
    G) ; end matrixRowDotProducts





















;;**EXPORTKEY**:math:matrixTranspose
(defriend math:matrixTranspose(A)
;; *******************************************************************
;; name:     matrixTranspose
;; 
;; summary:  Returns The N by M matrix transposition of the M by N input matrix.
;; 
;;           Minimally the A component may be a scalar, a vector, a row vector, 
;;           a column vector, a vector array, or a matrix. In each case,
;;           the appropriately shaped product result will be returned.
;;
;; Parms:    A:       The M by N matrix to be transposed
;; Return:   T:       The N by M matrix transposition of the input
;;
;; Note1:    See Sedgewick[2] chap 38.
;; *******************************************************************
    regs:(m M n N)
    regs:(Number:t NumPointer:Ap NumPointer:Tp)
    vars:(NumMatrix:T rankA)
    ;; Perform conversion to a proper matrix (if necessary). 
    (setq A (convertToMatrix A))
    ;; Perform matrix transposition for each type of input.
    (cond
       ;; Manage the case for A is a scalar and B is a scalar
	   ;; Parms:    A:       A scalar
	   ;; Return:   T:       The scalar itself
       ((isNumber A) 
        (begin
	      (return A)
	    )) ; end case for A is a scalar

       ;; Manage the case for A is a matrix
	   ;; Parms:    A:       The M by N matrix to be transposed
	   ;; Return:   T:       The N by M matrix transposition of the input
       ((isMatrix A) 
        (begin
	      (setq N (setq rankA (rank A))[0])
	      (setq M rankA[1])
	      (setq T (new Matrix: Number: 2 M N))
          (setq Ap A)
          (setq Tp T)
	      (loop for m from 0 until M do
	         (loop for n from 0 until N do
	            (setq Tp[(+ (* N m) n)] Ap[(+ (* M n) m)])
	            ) ; end M loop
	         ) ; end N loop
	    )) ; end case for A is a matrix

       ;; Manage all other cases as errors
       (else (error "math.matrixTranspose: illegal type for A")) 

       ) ; end matrix type cond
    T) ; end matrixTranspose






;;**EXPORTKEY**:math:matrixTriangulate
(defriend math:matrixTriangulate(A)
;; *******************************************************************
;; name:     matrixTriangulate
;; 
;; summary:  Triangulates the M by M matrix using gaussian elimination 
;;           to reduce round off error.
;;
;; Parms:    A:       The M by M matrix.
;; Return:   A:       The M by M matrix after triangulation.
;;
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    regs:(i j k M N (Number:fudge 1.0))
    regs:(Number:aii Number:aji Number:aik Number:ajk NumPointer:Ap)
    vars:(rankA)
    ;; Begin main logic.
    (setq A (convertToMatrix A))
    (setq Ap A)
    (setq rankA (rank A))
    (setq M rankA[1])
    (setq N rankA[0])
    (if (and (= (length rankA) 2) (= N M))
        (loop for i from 0 until M do
            (loop for j from (add1 i) until M do
                (loop for k from (- M 1) to i by -1 do
		            ;; Fudge around singular condition iff we would be dividing by zero.
		            ;; Note: This occurs whenever the matrix is singular and a slight fudge
		            ;;       introduces very little error.
                    (setq aii Ap[(+ (* M i) i)])
                    (setq aji A[(+ (* M j) i)])
                    (setq aik A[(+ (* M i) k)])
                    (setq ajk A[(+ (* M j) k)])
                    (if (= aii 0.0)
                        (if (= aji 0)
                            (setq ajk (- ajk (* aik 0.0)))
                            (setq ajk (- ajk (* aik (/ (+ fudge aji) (+ fudge aii)))))
                            ) ; end if
                        (setq ajk (- ajk (* aik (/ aji aii))))
                        ) ; end if
                    (if (not (isNumber ajk)) (setq ajk 0.0))
                    (setq Ap[(+ (* M j) k)] ajk)
                    ) ; end k loop
                 ) ; end j loop
            ) ; end i loop
        (error "math.matrixTriangulate: expecting square matrix")
        ) ; end if
    A) ; end matrixTriangulate












































































;;**EXPORTKEY**:math:matrixUpper
(defriend math:matrixUpper(A)
;; *******************************************************************
;; name:     matrixUpper
;; 
;; summary:  Returns the upper triangular component matrix of the N by N 
;;           input matrix using gaussian LU decomposition, without pivoting, 
;;           to reduce round off errors.
;;
;; Parms:    A:       The M by M matrix.
;; Return:   U:       The M by M upper triangular matrix component.
;;
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    regs:(i j k M N (Number:fudge 1.0))
    regs:(NumPointer:Up NumPointer:WIp)
    regs:(Number:uii Number:uji Number:ujk Number:uik Number:wji)
    vars:(rankU NumMatrix:L NumMatrix:U NumMatrix:WI)
    ;; Begin main logic.
    (setq U (copyToMatrix A))
    (setq rankU (rank U))
    (setq N rankU[1])
    (setq M rankU[0])
    (setq L (matrixIdentity M))
    (setq Up U)
    (if (and (= (length rankU) 2) (= N M))
        (loop for i from 0 until M do
            (setq WI (matrixIdentity M))
            (setq WIp WI)
            (loop for j from (+ 1 i) until M do
                (setq uii Up[(+ (* i M) i)])
                (setq uji Up[(+ (* j M) i)])
                (if (= uii 0.0)
                    (if (= uji 0.0)
                        (setq wji -1.0)
                        (setq wji (- uji (/ (+ fudge uji) (+ fudge uii))))
                        ) ; end if
                    (setq wji (- (/ uji uii)))
                    ) ; end if
                (setq wji (numCheck wji))
                (setq WIp[(+ (* j M) i)] wji)	
                (loop for k from (subi M 1) to i by -1 do
		            ;; Fudge around singular condition iff we would be dividing by zero.
		            ;; Note: This occurs whenever the matrix is singular and a slight fudge
		            ;;       introduces very little error. 
                    (setq uii Up[(+ (* i M) i)])
                    (setq uji Up[(+ (* j M) i)])
                    (setq uik Up[(+ (* i M) k)])
                    (setq ujk Up[(+ (* j M) k)])
                    (if (= uii 0.0)
                        (if (= uji 0.0)
                            (setq ujk (- ujk (* uik 0.0)))
                            (setq ujk (- ujk (* uik (/ (+ fudge uji) (+ fudge uii)))))
                            ) ; end if
                        (setq ujk (- ujk (* uik (/ uji uii))))
                        ) ; end if
                    (if (not (isNumber ujk)) (setq ujk 0.0))
                    (setq Up[(+ (* j M) k)] ujk)
                    ) ; end k loop
                 ) ; end j loop
            (setq L (matrixMultiply WI L))
            ) ; end i loop
        (error "math.matrixUpper: expecting square matrix")
        ) ; end if
    U) ; end matrixUpper











































































;;**EXPORTKEY**:math:matrixWidrowHoffRegression
(defriend math:matrixWidrowHoffRegression(X Y Gmax err ...)
;; *******************************************************************
;; summary:  Returns the dense M coefficient vector giving the coefficients
;;           with the best least squares fit of the variables with a constant
;;           term inserted in the model. The method used is the Widrow-Hoff
;;           iterative approximation.
;;
;; Parms:    X:        The N by M matrix representing the original observations
;;                     of the independent variables.
;;           Y:        The N vector representing the original observations
;;                     of the dependent variable.
;;           Gmax:     The maximum number of optimization trials (generations) to attempt before
;;                     returning the best set of coefficients available at that time.
;;           err:      A minimum error value which would terminate further optimization 
;;                     trials and return the best set of coefficients at that time. This
;;                     minimum error is expressed as the absolute value of the average error.
;;           RfSW:     (Optional)If present and true, return a linear regression Lambda, Rf,
;;                     with coefficient vector, Rf.C, and Rf.Error set to the proper values.
;;           printSW:  (Optional)If present and true, display each regression interation on the console
;;
;; Return:   C:        The M+2 coefficient vector (with M+1th = error), AND the 0th term being an inserted constant.
;; Note:     See Cristianini, "Support Vector Machines", page 23.
;; *******************************************************************
    vars:(C B yMean m M n N oldC oldB oldError currentError ey dotProduct (learningRate .025) generationCount Rf RfSW printSW)
    ;; Convert the input into the proper form.
    (setq X (convertToMatrix X))
    (if (or (not (isMatrix X)) (not (isVector Y)) (<> (length (setq rankX (rank X))) 2) (<= (setq M rankX[1]) 0) (<> (setq N rankX[0]) (length Y))) (error "math.matrixWidrowHoffRegression: invalid input arguments"))
    (if (>= (argCount) 5) (setq RfSW (argFetch 4)) (setq RfSW false))
    (if (>= (argCount) 6) (setq printSW (argFetch 5)) (setq printSW false))
    ;; Adjust the X and Y matrices for overflow and underflow errors.
    (loop for n from 0 until N do
       (loop for m from 0 until M do
          (setq X[n m] (numCheck X[n m]))
          ) ; end M loop 
       (setq Y[n] (numCheck Y[n]))
       ) ; end N loop
    ;; Initialize the coefficient vector and the constant.
    ;; Note: Make initial guesses using the average of the column vectors.
    (setq B 0)
    (setq C (new Vector: number: M))
    (setq yMean (abs (setq B (avg Y))))
    (loop for n from 0 until N do
       (loop for m from 0 until M do
          (setq C[m] (+ C[m] (/ X[n m] N)))
          ) ; end M loop
       ) ; end N loop
    (loop for m from 0 until M do
          (setq C[m] (/ B C[m] M))
       ) ; end M loop
    (setq generationCount 0)
    (setq currentError BIGPOSNUM)
    (setq learningRate (/ learningRate N))
    (if (= printSW true) (writeln _eol "G=[" (integer generationCount) ",E=[" currentError "],L=[" learningRate "],B=[" B "],C=(" (mid (string C true) 6 10000000) ")"))
    ;; Perform a single iterative pass through the observation matrix, X.
    NextGeneration::
    (++ generationCount)
    (setq oldB B)
    (setq oldC (copy C))
    (setq oldError currentError)
    (setq currentError 0)
    (loop for n from 0 until N do 
       ;; Compute the dot product of the coefficient vector, C, and the observation vector rowX[n].
       (setq dotProduct 0.0)
       (loop for m from 0 until M do
          (setq dotProduct (+ dotProduct (* C[m] X[n m])))
          ) ; end m loop
       ;; Compute the estimate error and the sum of the squared estimate errors.
       (setq ey (- (numCheck (+ B dotProduct)) Y[n]))
       (setq currentError (+ currentError (/ (abs ey) N)))
       ;; Adjust the coefficient vector, C, and the constant, B, using the learning rate.
       (loop for m from 0 until M do
          (setq C[m] (- C[m] (numCheck (* learningRate ey X[n m]))))
          ) ; end m loop
       (setq B (- B (* learningRate ey)))
       ) ; end n loop
    (if (= printSW true) (writeln "G=[" (integer generationCount) ",E=[" currentError "],L=[" learningRate "],B=[" B "],C=(" (mid (string C true) 6 100000) ")"))
    ;; Adjust the learning rate (if necessary).
    ;; Note: Constant adjustment of the learning rate is an extremely important
    ;;       heuristic in order to obtain fast convergence for this algorithm.  
    (cond
      ((> (abs currentError) BIGPOSNUM) 
       (begin 
          (setq learningRate (* learningRate .1)) 
	      (setq B oldB)
	      (setq C oldC)
	      (setq currentError oldError)
       )) ; end case
      ((= oldError currentError) (setq learningRate (* learningRate 1.1)))
      ((< oldError currentError)
       (begin 
          (setq learningRate (* learningRate .1)) 
		  (setq B oldB)
		  (setq C oldC)
		  (setq currentError oldError)
       )) ; end case
      ) ; end cond
    ;; Try another generation of training (if necessary). 
    (if (and (< generationCount Gmax) (> currentError err)) (goto NextGeneration:))
    ;; If requested, return a linear regression Lambda, Rf, with
    ;; coefficient vector, Rf.C, and Rf.Error set accordingly.
    (if (= RfSW true)
        (begin
            (setq Rf (math.numericRegress.makeLinear M))
		    (setq Rf.C (insert C 0 B))
		    (setq Rf.Error currentError)
		    (setq Rf.G generationCount)
		    (setq Rf.P 1)
		    (return Rf)
		)); end if
    ;; Return the coefficient vector with the error in the M+1st slot,
    ;; and the constant, B, in the zeroth slot.
    (setq C (insert C 0 B))
    (setq C[(addi M 1)] currentError)
    C) ; end matrixWidrowHoffRegression

























;;**EXPORTKEY**:math:multipleRegress
(defriend math:multipleRegress(w)
;; *******************************************************************
;; summary:  Returns the sparse M coefficient vector for the factors 
;;           with the best least squares fits of the variables.
;; Parms:    w:       The N by M+1 array representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;; Return:   v:       The M+2 coefficient vector (with M+1th = error), 
;;                    AND the 0th term being an inserted constant.
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    vars:(wt v ve vc vx vy vr vs i I m M n N)
    ;; Extract the least squares error from multiple linear regressions
    (if (or (= w #void) (<= (length w) 0)) (return (new Vector:)))
    (setq w (convertToArray w))
    (setq N (length w))
    (setq M (subi (length w[0]) 1))
    (setq v (new Vector: M))
    (setq vc (new Vector: (addi M 2)))
    (setq vx (new Vector: M))
    (setq vr (new Vector: M))
    (setq vy (convertToColumnVector w M))
    (loop for m from 0 until M do
        (setq vx[m] (convertToColumnVector w m))
        (setq vr[m] (regress vx[m] vy))
        ;; Save the error factor from the single linear regression operation.
        ;; Note: We will use them to determine which columns are best fits.
        (setq v[m] vr[m][2])
        ) ; end loop
    ;; Sort the regression errors to find the smallest errors.
    ;; Note: A sorted vector of integers is returned.
    (setq ve (sort (copy v) < true))
    ;; Save the first linear regression model (it may be the best).
    (setq vc[0] vr[ve[0]][0]) ;; Save the best constant coefficient.
    (setq vc[(add1 ve[0])] vr[ve[0]][1]) ;; Save the best linear coefficient.
    (setq vc[(addi M 1)] vr[ve[0]][2]) ;; Save the current error factor.
    (if (= M 1) (return vc))
    (if (= vr[ve[0]][2] 0) (return vc))
    ;; Create an initial N by M+1 array for use in multivariate regression.
    (setq wt (new Vector: N))
    (loop for n from 0 until N do
       (setq wt[n] (copy w[n]))
       (insert wt[n] 0 1)   ;; Allow for a constant.
       ) ; end loop
    ;; Perform a complete multivariate regression.
    (setq vs (multivariableRegress wt))
    (if (>= vs[(subi (length vs) 1)] vc[(subi (length vc) 1)]) (goto Last:))
    (setq vc vs)
    ;; Create a series multivariate regressions until the error starts to climb.
    (loop for i from (subi (length ve) 1) until 0 by -1 do
       ;; Eliminate the next worst column from the next regression.
       (arrayFillColumn wt ve[i] 0)
       ;; Perform a regression on the remaining columns.
       (setq vs (multivariableRegress wt))
       (if (>= vs[(subi (length vs) 1)] vc[(subi (length vc) 1)]) (goto Last:))
       (setq vc vs)
       ) ; end loop
    ;; Return the regression coefficient vector.
    Last::
    vc) ; end multipleRegress







































































;;**EXPORTKEY**:math:multivariableRegress
(defriend math:multivariableRegress(w)
;; *******************************************************************
;; summary:  Returns the dense M coefficient vector giving the coefficients
;;           with the best least squares fit of the variables.
;; Parms:    w:       The N by M+1 array representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;; Return:   v:       The M+1 coefficient vector (with M+1th = error).
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    vars:(wt x i j M N err ey)
    ;; Perform a least squares regression on all the factors.
    (if (or (= w #void) (<= (length w) 0)) (return (new Vector:)))
    (setq w (convertToArray w))
    (setq wt (makeGaussianArray w))
    (setq wt (gaussianEliminate wt))
    (setq x (gaussianSubstitute wt))
    ;; Compute the least squares regression error.
    (setq N (length w))
    (setq M (subi (length w[0]) 1))
    (loop for i from 0 until N do
       (setq ey 0)
       (loop for j from 0 until M do
          (setq ey (+ ey (* x[j] w[i][j])))
          ) ; end M loop
       (setq ey (- w[i][M] ey))
       (setq err (+ err (* ey ey)))
       ) ; end N loop
    (setq err (/ err N))
    ;; Return the coefficient vector with the error in the M+1st slot.
    (setq x[M] err)
    x) ; end multivariableRegress








































;;**EXPORTKEY**:math.multivariableRegressC
(defriend math:multivariableRegressC(X)
;; *******************************************************************
;; summary:  Returns the dense M coefficient vector giving the coefficients
;;           with the best least squares fit of the variables with a constant
;;           term inserted in the model.
;; Parms:    X:       The N by M+1 array representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;; Return:   v:       The M+2 coefficient vector (with M+1th = error), 
;;                    AND the 0th term being an inserted constant.
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    vars:(wt v tmp i j M N err ey)
    ;; Perform a least squares regression on all the factors.
    (if (or (= X #void) (<= (length X) 0)) (return (new Vector:)))
    (setq X (convertToArray X))
    ;; Insert a constant 1.0 term into the first column so multivariate regression will return an intercept coefficient.
    (setq wt (new Vector: Object: (setq N (length X))))
    (loop for n from 0 until N do
       (setq wt[n] (copy X[n]))
       (insert wt[n] 0 1.0)   ;; Insert the constant 1.0
       ) ; end loop
    ;; Perform the multivariate regression.
    (setq tmp (makeGaussianArray wt))
    (setq tmp (gaussianEliminate tmp))
    (setq v (gaussianSubstitute tmp))
    ;; Compute the least squares regression error.
    (setq N (length wt))
    (setq M (subi (length wt[0]) 1))
    (loop for i from 0 until N do
       (setq ey 0)
       (loop for j from 0 until M do
          (setq ey (+ ey (* v[j] wt[i][j])))
          ) ; end M loop
       (setq ey (- wt[i][M] ey))
       (setq err (+ err (* ey ey)))
       ) ; end N loop
    (setq err (/ err N))
    ;; Return the coefficient vector with the error in the M+1st slot.
    (setq v[M] err)
    v) ; end multivariableRegressC








































;;**EXPORTKEY**:math:multivariableRegressIC
(defriend math:multivariableRegressIC(w)
;; *******************************************************************
;; summary:  Returns the dense M coefficient vector giving the coefficients
;;           with the best least squares fit of the variables with a constant
;;           term inserted in the model. The method used is evolutionary
;;           approximation.
;; Parms:    w:       The N by M+1 array representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;; Return:   v:       The M+2 coefficient vector (with M+1th = error), 
;;                    AND the 0th term being an inserted constant.
;; Note:     See evolutionary programming.
;; *******************************************************************
    vars:(wt x i j M N err ey Rf)
    ;; Perform a least squares regression on all the factors.
    (if (or (= w #void) (<= (length w) 0)) (return (new Vector:)))
    (setq w (convertToArray w))
    ;; Insert a constant term into the input array for use in multivariate regression.
    (setq M (subi (length w[0]) 1))
    (setq wt (new Vector: (setq N (length w))))
    (loop for n from 0 until N do
       (setq wt[n] (copy w[n]))
       (insert wt[n] 0 1)   ;; Allow for a constant.
       ) ; end loop
    (setq w wt)
    ;; Create a linear estimator function (evolutionary)
    (setq Rf (numericRegress.makeLinear M))
    (setq Rf.Strategy #void)
    (numericRegress.setRandom false 8191)
    ;; Perform the multivariate regression.
    (setq Rf (numericRegress w Rf 500 .000001))
    (setq x (objectToVector Rf.C))
    ;; Return the coefficient vector with the error in the M+1st slot.
    (setq x[(addi M 1)] Rf.Error)
    x) ; end multivariableRegressIC















;;**EXPORTKEY**:math:normalizeArray
(defriend math:normalizeArray(mat ...)
;; *******************************************************************
;; summary:  Normalizes the column values in a array. Each array
;;           cell value is normalized by finding the high and low
;;           range for the value's column, then value is converted 
;;           into a fraction (0 ... 1) of it's column's high-low range.
;; Args:     mat:     A array with values to be normalized. The array
;;                    is stored in [row][column] order.
;;           depSW    (Optional) true if last column is to be unchanged.
;; Return:   nmat:    The normalized array (all values are fractions).
;; *******************************************************************
    vars:(nmat i j Y colCount rowCount high low range (depSW false))
    (if (= (argCount) 2) (setq depSW (argFetch 1)))
    (setq mat (convertToArray mat))
    (setq rowCount (length mat))
    (setq colCount (length mat[0]))
    (setq Y (sub1 colCount))
    (setq nmat (new Vector: rowCount))
    (loop for i from 0 until rowCount do (setq nmat[i] (new Vector: number: colCount)))
    (loop for i from 0 until colCount do
        (setq high -999999999999999)
        (setq low 999999999999999)
        (loop for j from 0 until rowCount do
            (if (not (isNumber mat[j][i])) (setq nmat[j][i] 0))
            (if (< high mat[j][i]) (setq high mat[j][i]))
            (if (> low mat[j][i]) (setq low mat[j][i]))
            ) ; end j loop
        (setq range (- high low))
        (if (= range 0)
            (loop for j from 0 until rowCount do (setq nmat[j][i] 1))
            (loop for j from 0 until rowCount do (setq nmat[j][i] (/ (- mat[j][i] low) range)))
            ) ; end if
        ) ; end i loop
    (if depSW (loop for i from 0 until rowCount do (setq nmat[i][Y] mat[i][Y])))
    nmat) ; end normalizeArray








































































;;**EXPORTKEY**:math:numericRegress
(defriend math:numericRegress(X Y Rf Gmax err)
;; *******************************************************************
;; summary:  Returns the input estimator Lambda with numeric coefficients 
;;           optimized against the specified objective function. The 
;;           regression may be linear or nonlinear and will minimize any 
;;           objective function. Neither the estimator Lambda nor the 
;;           objective function are required to be continuous.
;;
;;			 The problem sizes addressed by this Lambda are classified as:
;;			 	o Small Problems (nominally 4 columns by 500 rows of data).
;;			 	o Midsize Problems (nominally 20 columns by 5000 rows of data).
;;			 	o Large scale Problems (nominally 100 columns by 25000 rows of data).
;;
;; 			 This Lambda uses many user selectable search strategies for 
;;           estimator Lambda optimization. Each technology has its own 
;;			 strengths and weaknesses. Some are linear only. Some cannot be
;;           used on large scale problems. As new technologies become available,
;;           in the scientific community, they are made available here. The
;;           currently supported numeric regression strategies are as 
;;           follows.
;;
;;           Multivariate Linear Regression: This is the technology developed
;;           by Gauss several centuries ago. It is deterministic and absolutely 
;;           accurate, but limited to linear regression only. This strategy is
;;           extremely fast and applicable for all small, midsize, and large
;;			 scale regression problems.
;;
;;           Support Vector Machine Regression: This is technology developed
;;           by Vapnik and Platt in this century. It is non-deterministic,
;;			 incremental, but fairly accurate and capable of handling both
;;           linear and non-linear regression tasks. This strategy is
;;           reasonably fast and applicable for all small, midsize, and large
;;			 scale regression problems.
;;
;;           Neural Net Regression: This is technology developed by Hecht-
;;			 Nielson in the previous century. It is non-deterministic,
;;			 incremental, somewhat accurate, and capable of handling both
;;           linear and non-linear regression tasks. This strategy is
;;           somewhat fast but applicable for only small scale regression 
;;           problems. Although it can handle more than 500 rows of data,
;;			 it has trouble handling more than 4 or so continuous column inputs.
;;
;;           Genetic Evolutionary Regression: This technology was developed
;;			 by Holland and Koza in the past century. It is non-deterministic,
;;			 incremental, fairly inaccurate but capable of handling both
;;           linear and non-linear regression tasks. This strategy is
;;           fairly slow and applicable for only small scale regression 
;;           problems. It has trouble handling more than 500 or so rows.
;;           There are three flavors of this technology. Evolve, uses a
;;			 real number genome. EvolveBinary uses a binary genome, and
;;           evolveBinaryMemo uses a binary genome with memoizing.
;;			 
;;           Induced Regression: This is ad hoc technology defined by the
;;			 user. The regression technology to be used must be entirely
;;           contained in the user supplied estimator Lambda. As examples
;;			 of such ad hoc, user defined technology, we supply estimator
;;           regression Lambdas to implement nearest neighbor cluster 
;;           regression, and euclidean induction regression. The speed 
;; 			 and problem size applicability of these strategies is entirely
;;           dependent upon the estimator Lambda supplied by the user.
;;			 
;;
;; Parms:    X:       (Option 1): The N by M+1 array of the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;                    Note: Where each of the row vectors must be Number vectors.
;;                    (Option 2): The N by M+1 number matrix of the observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;                    (Option 3): The N by M array of the independent variables
;;                    in the form of:    x x ... x
;;                                       x x ... x
;;                                           ... 
;;                                       x x ... x
;;                    Note: Where each of the row vectors must be Number vectors.
;;           Y:       (Option A) #void (in which case the X argument must be
;;					  either Option 1 or Option 2, and never Option 3.
;;           	      (Option B) The N number vector representing the dependent  
;;                    variables  in the form of:   	y
;;                                       			y
;;                                          	  ..... 
;;                                       			y
;;                    Note: If Y is NOT #void then the first argument, X, must
;; 					  be Option 3, and never Options 1 or 2.
;;     
;;                    Note:  Each of the available regression strategies will 
;;                           accept X and Y variables in any of the legal options
;;							 combinations. In all cases, data conversion will be
;;							 done for the user. To avoid expensive automatic
;;							 data conversions, use the following X Y input
;;							 options for each strategy as follows:
;;
;;					         	Multivariate Linear Regression: X(Option 2) Y(Option A)
;;					         	Support Vector Machine Regression: X(Option 3) Y(Option B)
;;					         	Neural Net Regression: X(Option 1) Y(Option A)
;;					         	Genetic Evolutionary Regression: X(Option 1) Y(Option A)
;;					         	Induced Regression: ...user defined...
;; 
;;           Rf:      The estimator Lambda which maps a number vector, of length M,
;;                    into a single real number, Rf: Vm --> R, whose coefficients
;;                    are to be optimized. The estimator Lambda is in the form of
;;                    a function, (Rf mVector) ==> aNumber, and the coefficients
;;                    to be optimized are stored as a number vector Rf.C 
;;                    in the persistant variables of the Rf Lambda.
;;                    Note: The estimator Lambda, Rf, must be an Lambda containing
;;                          the following persistant (pvars) variables:
;;                          C             A number vector containing the coefficients to optimized
;;                          Error         The final score from the objective function 
;;                          G             The final generation count when optimization halted
;;                          Objective     The objective function which maps an N by M+1 observation 
;;                                        array, X, using it's parent estimator Lambda, Rf, into 
;;                                        a single positive real error value. The objective function 
;;                                        is in the form of a child Lambda, (Rf.Objective X) ==> anErrorNumber.
;;                                        The numericRegress Lambda attempts to optimize the estimator
;;                                        Lambda coefficients, Rf.C, such that the objective 
;;                                        function's resulting error value is minimized. 
;;                          P             The final population count when optimization halted 
;;                          Strategy      The optimization strategy to use:
;;                                        #void				Genetic Evolutionary Algorithms (real number genome)  
;;                                        evolve: 			Genetic Evolutionary Algorithms (real number genome)  
;;                                        evolveBinary: 	Genetic Evolutionary Algorithms (binary genome)  
;;                                        evolveBinaryMemo: Genetic Evolutionary Algorithms (binary genome) with memos  
;;                                        induce:	    	Induced Regression (user supplied technology)  
;;                                        linear:	    	Multivariate Linear Regression  
;;                                        neural:	    	Neural Net Regression  
;;                                        svm:	    		Support Vector Machine Regression  
;;                    Note:  We supply a number of child Lambdas with expertise in constructing
;;                           estimator Lambdas for use with the various regression strategies. 
;;                           Reviewing the following child Lambdas and their documentation will
;;							 provide assistance in using each of the available regression 
;;							 techniques:
;;
;;					         	Multivariate Linear Regression: 
;;									makeLinear
;;					         	Support Vector Machine Regression:
;;									makeSVM
;;					         	Neural Net Regression:
;;									makeNeural, makeSplitNeural
;;					         	Genetic Evolutionary Regression: 
;;									makeLinearEvolve, makeLogit
;;								    makeBExponential, makeExponential
;;								    makeSinusoidal, makeTailExponential
;;					         	Induced Regression: 
;;									makeCluster, makeEuclidean, makePolynomial 
;;									makeNeighbor, makeLinearNetwork
;;
;;           Gmax:    The maximum number of optimization trials (generations) to attempt before
;;                    returning the best set of coefficients available at that time.
;;           err:     A minimum error value which would terminate further optimization 
;;                    trials and return the best set of coefficients at that time.
;;
;; Return:   Rf:      The estimator Lambda with coefficients Rf.C optimized.
;; 
;; Note:     The selfTest child Lambda, located near the end of this source file,
;;			 contains a number of examples demonstrating the use of these regression
;;			 techniques to solve test regression problems. Also included are 
;;			 examples of using the child Lambdas to construct estimator Lambdas 
;;			  for use with each regression strategy.
;; *******************************************************************
    pvars:(;; Public variables
           C                       ;; The best coefficient vector at the current time.
           Cn                      ;; The size of the coefficient vector. 
           Cn1                     ;; The size of the coefficient vector minus one. 
           cvSurvivors             ;; The survivor's from the current generation of coefficient vectors.
           cvError                 ;; The previous generation's best error from the each population of coefficient vectors.
           cvPopulation            ;; The current generation of coefficient vectors.
           cvStop                  ;; The stop training switch for each population of coefficient vectors.
           Error                   ;; The best error at the current time.
           forceNewPopulation      ;; The training switch to force a new population of coefficient vectors.
           generationCount         ;; Current index of optimization trial attempts.
           M                       ;; The number of input columns in the observation array.
           Mm1                     ;; The number of input columns in the observation array minus one.
           (maxPopulationAge 50)   ;; The maximum age of any distinct population for any optimization trail.
           (maxPopulations 3)      ;; The maximum number of distinct populations for any optimization trail.
           (maxSurvivors 5)        ;; The maximum survivors from a population before starting the next optimization trail.
           (minPopulation 25)      ;; The minimum population, of cvPopulation, before attempting an optimization trail.
           Mp1                     ;; The number of input and dependent columns in the observation array.
           N                       ;; The number of rows in the observation array.
           Net                     ;; The best neural net at the current time.
           populationAge           ;; The age of each of the current populations for this optimization trail.
           populationCount         ;; The maximum number of distinct populations for this optimization trail.
           (populationStart 0)     ;; The minimum number of distinct populations for any optimization trail.
           (randomSW true)         ;; The pseudo random setting (True = random, False = srandom).
           ;; Public child methods
           makeBExponential        ;; (experimental) Return a binary exponential regression estimator Lambda for use with numericRegress.
           makeCluster             ;; Return a nearest neighbor cluster estimator Lambda for use with numericRegress.
           makeEuclidean           ;; Return a Euclidean induction regression estimator Lambda for use with numericRegress.
           makeExponential         ;; Return an exponential regression estimator Lambda for use with numericRegress.
           makeLinear              ;; Return a linear regression estimator Lambda for use with numericRegress (using Gaussian elimination).
           makeLinearEvolve        ;; Return a linear regression estimator Lambda for use with numericRegress (using evolutionary programming).
           makeLinearNetwork       ;; Return a linear regression network estimator Lambda for use with numericRegress.
           makeLogit               ;; Return a logit regression estimator Lambda for use with numericRegress.
           makeNeighbor            ;; Return a nearest neighbor regression estimator Lambda for use with numericRegress.
           makeNeural              ;; Return a neural net regression estimator Lambda for use with numericRegress.
           makePolynomial          ;; Return a polynomial regression estimator Lambda for use with numericRegress.
           makeSinusoidal          ;; Return a sinusoidal estimator Lambda for use with numericRegress.
           makeSplitNeural         ;; Return a neural net regression estimator Lambda for use with numericRegress (split training).
           makeSVM		           ;; Return a support vector machine regression estimator Lambda for use with numericRegress.
           makeTailExponential     ;; Return an exponential tail estimator Lambda for use with numericRegress.
           setRandom               ;; Set this regression routine to use random/srandom function.
           ;; Private variables
           _random                 ;; The current random function. 
           ;; Private child methods
           binaryEvolution         ;; Optimize the binary estimator coefficients using genetic algorithms.
           binaryMemoEvolution     ;; Optimize the binary estimator coefficients using genetic algorithms with memos.
           induceRegression        ;; Optimize the estimator coefficients using induced regression.
           linearRegression        ;; Optimize the estimator coefficients using multivariate regression.
           neuralRegression        ;; Optimize the estimator coefficients using neural net back propagation training.
           numberEvolution         ;; Optimize the number estimator coefficients using genetic algorithms.
           supportRegression       ;; Optimize the estimator coefficients using support vector machine regression.
           ;; Private maintenance child methods
           selfTest                ;; The self test method for this Lambda. 
           testWorkBench           ;; The test work bench where we experiment with new regression technologies. 
           ) ;; end of persistent variables
    vars:(i naCount (ErrTollerance .1))
    ;; *******************************************************************
    ;; Define Private Child Lambdas
    ;; *******************************************************************
    ;; Optimize the estimator coefficients using induced regression.
    ;; Note: The user's objective function is responsible for all the work.
    (defun induceRegression(X Y Rf)
       ;; Allow the objective function to induce 
       ;; learning into the estimator Lambda.
       (setq Rf.Error (Rf.Objective X Y))
       Rf) ; end induceRegression
    ;; Multivariate Linear Regression: optimize the estimator 
    ;; coefficients for the specified estimator Lambda using
    ;; the training data supplied.
    ;; Perferred Data Format:  X ==> N x M+1 number matrix, Y ==> #void.  
    (defun linearRegression(X Y Rf)
       vars:(cv M)
       ;; Create a new linear regression estimator Lambda.
       ;; Note: We always return an estimator Lambda valid
       ;;       for multivariate linear regression.
       (setq Rf (makeLinear Rf.variableCount))
       ;; Compute the optimized coefficient vector.
       ;; Note: The error is in the M+2 vector position.
       (setq X (math.convertToMatrix X Y))
       (setq cv (matrixMultipleRegress X))
       ;; Return the optimized linear estimator Lambda.
       (setq M (length cv))
       (setq Rf.Error cv[(- M 5)])
       (setq Rf.AvgErr cv[(- M 4)])
       (setq Rf.MinErr cv[(- M 3)])
       (setq Rf.MaxErr cv[(- M 2)])
       (setq Rf.AvgY cv[(- M 1)])
       (setq Rf.C (resize cv (subi M 5)))
       (setq Rf.P 1)
       (setq Rf.G 1)
       Rf) ; end linearRegression
    ;; Neural Net Regression: optimize the estimator 
    ;; coefficients for the specified estimator Lambda using
    ;; the training data supplied.
    ;; Perferred Data Format:  X ==> N x M+1 vector array, Y ==> #void.  
    (defun neuralRegression(X Y Rf Gmax errLIMIT)
       vars:(naCount m M n N oldErr err delta)
       ;; Make a new neural net regression estimator Lambda.
       ;; Note: We always start with fresh neural net weights.
       (setq X (math.convertToArray X Y))
	   (if (= randomSW true) (setq _random ^random) (setq _random ^srandom))
       (setq Rf.Net (makeNeuralNet Rf.variableCount Rf.hiddenCount 1 Rf.Filter))
	   (if (= randomSW true) (clear:Rf.Net) (clear:Rf.Net srandom.seed))
       (setq Rf.C (getWeights:Rf.Net))
       ;; Compute the optimized coefficient vector using back propagation.
       ;; Note: The error is in the M+2 vector position.
       (setq populationCount 0)
       (setq generationCount 0)
       (setq Error "NA")
       (setq C Rf.C)
       (setq Net Rf.Net)
       (setq Rf.Error "NA")
	   NewPopulation::
	   (if (< Rf.Error Error)
	       (begin
	          (setq Error Rf.Error)
	          (setq Net Rf.Net)
	          (setq C (getWeights:Rf.Net))
              (setq Rf.Net (makeNeuralNet Rf.variableCount Rf.hiddenCount 1 Rf.Filter))
	       )) ; end if
	   (if (= randomSW true) (clear:Rf.Net) (clear:Rf.Net srandom.seed))
	   (setq populationCount (addi populationCount 1))
       (setq oldErr "NA")
	   NewGeneration::
	   (setq generationCount (addi generationCount 1))
       ;; Expose the NeuralNet to the whole training set.
       ;; Note: The Objective function is responsible for this task.
       (setq Rf.Error (Rf.Objective X))
       ;; After training, check the error to determine the next learning step.
	   (if (or (<= Rf.Error errLIMIT) (>= generationCount Gmax)) (goto StopTraining:))
	   (if (> Rf.Error oldErr) (goto NewPopulation:))
	   (setq oldErr Rf.Error)
	   (goto NewGeneration:)
       ;; Return the optimized neural net estimator Lambda.
	   StopTraining::
	   (if (< Rf.Error Error)
	       (begin
	          (setq Error Rf.Error)
	          (setq C (getWeights:Rf.Net))
	          (setq Net Rf.Net)
	       )) ; end if
       (setq Rf.G generationCount)
       (setq Rf.P populationCount)
       (setq Rf.Error Error)
       (setq Rf.C C)
       (setq Rf.Net (clear:Net C))
       Rf) ; end neuralRegression
    ;; Support Vector Machine Regression: optimize the estimator 
    ;; coefficients for the specified estimator Lambda using
    ;; the training data supplied.
    ;; Perferred Data Format:  X ==> N x M vector array, Y ==> N number vector.  
    (defun supportRegression(X Y Rf maxGen maxErr)
       vars:(s m n N)
       ;; Create a new support vector machine estimator Lambda.
       ;; Note: We always return an estimator Lambda valid
       ;;       for support vector machine regression.
       (setq Rf (makeSVM Rf.variableCount Rf.kernel Rf.ErrTollerance Rf.MaxSVSize))
       ;; Compute the optimized support vector machine result structure.
       (setq s (svmRegression X Y Rf.kernel Rf.ErrTollerance maxErr maxGen Rf.MaxSVSize false))
       ;; Return the optimized linear estimator Lambda.
       (setq Rf.Error s.Error)
       (setq Rf.AvgErr s.Error)
       (setq Rf.MinErr #void)
       (setq Rf.MaxErr #void)
       (setq Rf.AvgY (avg Y))
       (setq Rf.Xs X)
       (setq Rf.C s.Weights)
       ;; Drop all but the support vectors for the trained model.
       ;; Note: Eliminate all entries with weights of zero.
       (setq Rf.C (new Vector: number:))
       (setq Rf.Xs (new Vector: object:))
       (setq m 0)
       (setq N (length X))
       (loop for n from 0 until N do 
          (if (<> s.Weights[n] 0)
              (begin
                 (setq Rf.C[m] s.Weights[n])
                 (setq Rf.Xs[m] X[n])
                 (setq m (addi m 1))
              )) ; end if
          ) ; end loop
       (setq Rf.P 1)
       (setq Rf.G s.Generations)
       Rf) ; end supportRegression
    ;; *******************************************************************
    ;; Define Public Child Lambdas
    ;; *******************************************************************
    ;; Set this regression routine to use random/srandom function.
    (defun setRandom(swt ...) 
       (setq randomSW swt)
       (if (and (= swt false) (= (argCount) 2)) (setq ^srandom.seed (argFetch 1)))
       swt) ; end setRandom
    ;; *******************************************************************
    ;; Define Private Maintenance Child Lambdas
    ;; *******************************************************************
    ;; Perform a self test of this general regression Lambda.
    (defun selfTest()
        pvars:(Rf Rfs Rf1 Rf2 Rf3 Rf4)
        vars:(m M n N in i I 
              VN temp crunch sum 
              startTime endTime
              minV maxV rngV
              c C cc x X ey y Y g G Ss E Kn) 
              
        (writeln _eol "*************************math.numericRegress.selfTest*********************")
        (gc)(setq startTime (getTickCount 0))
        (goto StartSelfTest:)

        ;; *********************************************************************************************
        ;; Begin Play Area
        ;; *********************************************************************************************

        ;; *********************************************************************************************
        (setq M 8)
        (setq N 2500)
        (setq G 1)
        (setq I 0)
        (setq E .05)
        (setq Kn ^vectorSigmoidInnerProduct)
        (writeln _eol "Start Support Vector Machine Regression of a " N " row " M " column " G " generation sigmoid polynomial with random noise.")
        ;; Start Support Vector Machine Regression of a mid scale linear polynomial with random noise..
        ;; Model: y = C[0]*X[0] + C[1]*X[1] + ... + C[M-1]*X[M-1] 
        ;; Hint: The Support Vector Machine Regression technology does NOT return
        ;;       a threshold constant. To get the threshold constant we supply
        ;;       a column of one's. The coefficient for the column of one's is
        ;;       the proper threshold coefficient for this model.
        (setq X (new Vector: Object: N))
        (setq Y (new Vector: Number: N))
        (setq C (new Vector: Number: M))
        (loop for m from 0 until M do
           (setq C[m] (- (^random 10) 5))
           ) ; end C loop
        (loop for n from 0 until N do
           (setq X[n] (new Vector: number: M))
           (setq X[n][0] 1)
           (setq y (* C[0] X[n][0]))
           (loop for m from 1 until M do 
          	  (setq X[n][m] (^random .01))
              (setq y (+ y (* C[m] X[n][m])))
              ) ; end M loop
		   (setq y (+ (* y .8) (* y (^random .004))))
           (setq Y[n] (/ (exp y) (+ 1.0 (exp y))))           
           ) ; end N loop
        ;; Create a support vector machine estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        support vector machine regression model, 
        ;;        which has been modified to prevent overflow.
        ;; Note2: The objective is an error tollerant average
        ;;        absolute error function (expressed as a percent
        ;;        of target value).        
        (setq Rf (makeSVM M Kn .0000000000001 100))
        (setRandom false 8191)
        (setq Rf (numericRegress X Y Rf G E))
        (loop for i from 0 until I do
           (if (<= Rf.Error E) (goto EndTraining:))
           (setq Rf1 (makeSVM M Kn .0000000000001 G))
           (setq Rf1 (numericRegress X Y Rf1 G E))
           (if (< Rf1.Error Rf.Error) (setq Rf Rf1))
           ) ; end loop
        ;; Display self test results for this test run.
        EndTraining::
        (writeln "Test Results:")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy)
        (loop for i from 0 until 10 do
           (setq y Y[i])
           (setq ey (Rf X[i]))
           (writeln "[" i "] y=" y ", ey=" ey)
           ) ; end loop
        (goto SelfTestLast:) ;; MFK ...Testing...

        ;; *********************************************************************************************
        (setq M 8)
        (setq N 250)
        (setq G 400)
        (writeln _eol "Start Neural Net Regression of a " N " row " M " column " G " generation cube polynomial with random noise.")
        ;; Start Neural Net Regression of a mid scale linear polynomial with random noise..
        ;; Model: y = C[0]*X[0] + C[1]*X[1] + ... + C[M-1]*X[M-1] 
        ;; Hint: The Support Vector Machine Regression technology does NOT return
        ;;       a threshold constant. To get the threshold constant we supply
        ;;       a column of one's. The coefficient for the column of one's is
        ;;       the proper threshold coefficient for this model.
        (setq X (new Vector: Object: N))
        (setq Y (new Vector: Number: N))
        (setq C (new Vector: Number: M))
        (loop for m from 0 until M do
           (setq C[m] (- (^random 10) 5))
           ) ; end C loop
        (loop for n from 0 until N do
           (setq X[n] (new Vector: number: M))
           (setq X[n][0] 1)
           (setq y (* C[0] X[n][0]))
           (loop for m from 1 until M do 
          	  (setq X[n][m] (^random .01))
              (setq y (+ y (* C[m] X[n][m])))
              ) ; end M loop
		   (setq y (+ (* y .8) (* y (^random .004))))
           (setq Y[n] (/ (exp y) (+ 1.0 (exp y))))           
           ) ; end N loop
        ;; Create a exponential neural net estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        continuous neural net regression model, 
        ;;        which has been modified to prevent 
        ;;        overflow.
        ;; Note2: A standard least squares objective
        ;;        function is used.
        (setq Rf (makeNeural 8 sigmoid:))
        ;; Regress the neural net.
        (setRandom false 8191)
        (setq Rf (numericRegress X Y Rf G .000001))
        ;; Display self test results for this test run.
        (writeln "Test Results:")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy
                 ", F=" Rf.Filter)
        (loop for i from 0 until (length X) by 10 do
           (setq y X[i][(sub1 (length X[i]))])
           (setq ey (Rf X[i]))
           (writeln "[" i "] y=" y ", ey=" ey)
           ) ; end loop
        (goto SelfTestLast:) ;; MFK ...Testing...

        ;; *********************************************************************************************
        ;; End Play Area
        ;; *********************************************************************************************

        ;; ============================================================
        ;; Multivariate Linear Regression
        ;; ============================================================
        StartSelfTest::
  
        ;; *********************************************************************************************
        (writeln _eol "Start Multivariate Linear Regression of a small scale sigmoid linear polynomial.")
        ;; Start Multivariate Linear Regression of a small scale sigmoid linear polynomial.
        ;; Model: y = C[0]*X[0] + C[1]*X[1] + ... + C[M-1]*X[M-1] 
        ;; Hint: The Multivariate Linear Regression technology does NOT return
        ;;       a threshold constant. To get the threshold constant we supply
        ;;       a column of one's. The coefficient for the column of one's is
        ;;       the proper threshold coefficient for this model.
        (setq M 4)
        (setq N 500)
        (setq X (new Matrix: number: 2 N (addi M 1)))
        (setq Y #void)
        (setq C (new Vector: number: M))
        (loop for m from 0 until M do
           (setq C[m] (^random .00001))
           ) ; end C loop
        (loop for n from 0 until N do
           (setq X[n 0] 1)
           (setq y (* C[0] X[n 0]))
           (loop for m from 1 until M do 
          	  (setq X[n m] (^random .00001))
              (setq y (+ y (* C[m] X[n m])))
              ) ; end M loop
		   (setq X[n m] y)           
           ) ; end N loop
        ;; Create a linear estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        linear regression model, which has
        ;;        been modified to prevent overflow.
        ;; Note2: A standard least squares objective
        ;;        function is used.
        (setq Rf (makeLinear M))
        (setRandom false 8191)
        (setq Rf (numericRegress X Y Rf 1 .0001))
        ;; Display self test results for this test run.
        (writeln "Test Results:")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy)
        (writeln "........C={" C "}")
        (writeln ".....Rf.C=[" Rf.C "]")
        (loop for i from 0 until 10 do
           (setq y X[i M])
           (setq ey (Rf (matrixRowToVector X i M)))
           (writeln "[" i "] y=" y ", ey=" ey)
           ) ; end loop
  
        ;; *********************************************************************************************
        (writeln _eol "Start Multivariate Linear Regression of a midscale linear polynomial.")
        ;; Start Multivariate Linear Regression of a midscale linear polynomial.
        ;; Model: y = C[0]*X[0] + C[1]*X[1] + ... + C[M-1]*X[M-1] 
        ;; Hint: The Multivariate Linear Regression technology does NOT return
        ;;       a threshold constant. To get the threshold constant we supply
        ;;       a column of one's. The coefficient for the column of one's is
        ;;       the proper threshold coefficient for this model.
        (setq M 20)
        (setq N 5000)
        (setq X (new Matrix: number: 2 N (addi M 1)))
        (setq Y #void)
        (setq C (new Vector: number: M))
        (loop for m from 0 until M do
           (setq C[m] (- (^random 10) 5))
           ) ; end C loop
        (loop for n from 0 until N do
           (setq X[n 0] 1)
           (setq y (* C[0] X[n 0]))
           (loop for m from 1 until M do 
          	  (setq X[n m] (- (^random 10) 5))
              (setq y (+ y (* C[m] X[n m])))
              ) ; end M loop
		   (setq X[n m] y)           
           ) ; end N loop
        ;; Create a linear estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        linear regression model, which has
        ;;        been modified to prevent overflow.
        ;; Note2: A standard least squares objective
        ;;        function is used.
        (setq Rf (makeLinear M))
        (setRandom false 8191)
        (setq Rf (numericRegress X Y Rf 1 .0001))
        ;; Display self test results for this test run.
        (writeln "Test Results:")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy)
        (writeln "........C={" C "}")
        (writeln ".....Rf.C=[" Rf.C "]")
        (loop for i from 0 until 10 do
           (setq y X[i M])
           (setq ey (Rf (matrixRowToVector X i M)))
           (writeln "[" i "] y=" y ", ey=" ey)
           ) ; end loop


        ;; *********************************************************************************************
        (writeln _eol "Start Multivariate Linear Regression of a midscale linear polynomial with random noise.")
        ;; Start Multivariate Linear Regression of a midscale linear polynomial with random noise.
        ;; Model: y = (C[0]*X[0] + C[1]*X[1] + ... + C[M-1]*X[M-1]) +- 20% at random 
        ;; Hint: The Multivariate Linear Regression technology does NOT return
        ;;       a threshold constant. To get the threshold constant we supply
        ;;       a column of one's. The coefficient for the column of one's is
        ;;       the proper threshold coefficient for this model.
        (setq M 50)
        (setq N 50000)
        (setq X (new Matrix: number: 2 N (addi M 1)))
        (setq Y #void)
        (setq C (new Vector: number: M))
        (loop for m from 0 until M do
           (setq C[m] (- (^random 10) 5))
           ) ; end C loop
        (loop for n from 0 until N do
           (setq X[n 0] 1)
           (setq y (* C[0] X[n 0]))
           (loop for m from 1 until M do 
          	  (setq X[n m] (- (^random 10) 5))
              (setq y (+ y (* C[m] X[n m])))
              ) ; end M loop
		   (setq X[n m] (+ (* y .8) (* y (^random .4))))           
           ) ; end N loop
        ;; Create a linear estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        linear regression model, which has
        ;;        been modified to prevent overflow.
        ;; Note2: A standard least squares objective
        ;;        function is used.
        (setq Rf (makeLinear M))
        (setRandom false 8191)
        (setq Rf (numericRegress X Y Rf 1 .0001))
        ;; Display self test results for this test run.
        (writeln "Test Results:")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy)
        (writeln "........C={" C "}")
        (writeln ".....Rf.C=[" Rf.C "]")
        (loop for i from 0 until 10 do
           (setq y X[i M])
           (setq ey (Rf (matrixRowToVector X i M)))
           (writeln "[" i "] y=" y ", ey=" ey)
           ) ; end loop

        ;; ============================================================
        ;; Support Vector Machine Regression
        ;; ============================================================
          
        ;; *********************************************************************************************
        (writeln _eol "Start Support Vector Machine Regression of a small scale sigmoid linear polynomial.")
        ;; Start Support Vector Machine Regression of a small scale sigmoid linear polynomial.
        ;; Model: y = C[0]*X[0] + C[1]*X[1] + ... + C[M-1]*X[M-1] 
        ;; Hint: The Support Vector Machine Regression technology does NOT return
        ;;       a threshold constant. To get the threshold constant we supply
        ;;       a column of one's. The coefficient for the column of one's is
        ;;       the proper threshold coefficient for this model.
        (setq M 4)
        (setq N 500)
        (setq X (new Vector: object: N))
        (setq Y (new Vector: number: N))
        (setq C (new Vector: number: M))
        (loop for m from 0 until M do
           (setq C[m] (^random .00001))
           ) ; end C loop
        (loop for n from 0 until N do
           (setq X[n] (new Vector: number: M))
           (setq X[n][0] 1)
           (setq y (* C[0] X[n][0]))
           (loop for m from 1 until M do 
          	  (setq X[n][m] (^random .00001))
              (setq y (+ y (* C[m] X[n][m])))
              ) ; end M loop
		   (setq Y[n] y)           
           ) ; end N loop
        ;; Create a support vector machine estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        support vector machine regression model, 
        ;;        which has been modified to prevent overflow.
        ;; Note2: The objective is an error tollerant average
        ;;        absolute error function (expressed as a percent
        ;;        of target value).
        (setq Rf (makeSVM M ^vectorInnerProduct .1 50))
        (setRandom false 8191)
        (setq Rf (numericRegress X Y Rf 2 .05))
        ;; Display self test results for this test run.
        (writeln "Test Results:")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy)
        (loop for i from 0 until 10 do
           (setq y Y[i])
           (setq ey (Rf X[i]))
           (writeln "[" i "] y=" y ", ey=" ey)
           ) ; end loop
  
        ;; *********************************************************************************************
        (writeln _eol "Start Support Vector Machine Regression of a mid scale linear polynomial with random noise.")
        ;; Start Support Vector Machine Regression of a mid scale linear polynomial with random noise..
        ;; Model: y = C[0]*X[0] + C[1]*X[1] + ... + C[M-1]*X[M-1] 
        ;; Hint: The Support Vector Machine Regression technology does NOT return
        ;;       a threshold constant. To get the threshold constant we supply
        ;;       a column of one's. The coefficient for the column of one's is
        ;;       the proper threshold coefficient for this model.
        (setq M 20)
        (setq N 5000)
        (setq X (new Vector: object: N))
        (setq Y (new Vector: number: N))
        (setq C (new Vector: number: M))
        (loop for m from 0 until M do
           (setq C[m] (- (^random 10) 5))
           ) ; end C loop
        (loop for n from 0 until N do
           (setq X[n] (new Vector: number: M))
           (setq X[n][0] 1)
           (setq y (* C[0] X[n][0]))
           (loop for m from 1 until M do 
          	  (setq X[n][m] (- (^random 10) 5))
              (setq y (+ y (* C[m] X[n][m])))
              ) ; end M loop
		   (setq Y[n] (+ (* y .8) (* y (^random .4))))           
           ) ; end N loop
        ;; Create a support vector machine estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        support vector machine regression model, 
        ;;        which has been modified to prevent overflow.
        ;; Note2: The objective is an error tollerant average
        ;;        absolute error function (expressed as a percent
        ;;        of target value).
        (setq Rf (makeSVM M ^vectorInnerProduct .1 50))
        (setRandom false 8191)
        (setq Rf (numericRegress X Y Rf 2 .05))
        ;; Display self test results for this test run.
        (writeln "Test Results:")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy)
        (loop for i from 0 until 10 do
           (setq y Y[i])
           (setq ey (Rf X[i]))
           (writeln "[" i "] y=" y ", ey=" ey)
           ) ; end loop
  
        ;; *********************************************************************************************
        (writeln _eol "Start Support Vector Machine Regression of a mid scale sigmoid quadratic polynomial with random noise.")
        ;; Start Support Vector Machine Regression of a mid scale sigmoid quadratic polynomial with random noise.
        ;; Model: y = C[0]*X[0]*X[0] + C[1]*X[1]*X[1] + ... + C[M-1]*X[M-1]*X[M-1] 
        ;; Hint: The Support Vector Machine Regression technology does NOT return
        ;;       a threshold constant. To get the threshold constant we supply
        ;;       a column of one's. The coefficient for the column of one's is
        ;;       the proper threshold coefficient for this model.
        (setq M 20)
        (setq N 5000)
        (setq X (new Vector: object: N))
        (setq Y (new Vector: number: N))
        (setq C (new Vector: number: M))
        (loop for m from 0 until M do
           (setq C[m] (^random .0009999))
           ) ; end C loop
        (loop for n from 0 until N do
           (setq X[n] (new Vector: number: M))
           (setq X[n][0] 1)
           (setq y (* C[0] X[n][0] X[n][0]))
           (loop for m from 1 until M do 
          	  (setq X[n][m] (^random .0009999))
              (setq y (+ y (* C[m] X[n][m] X[n][m])))
              ) ; end M loop
		   (setq Y[n] (+ (* y .8) (* y (^random .4))))           
           ) ; end N loop
        ;; Create a support vector machine estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        support vector machine regression model, 
        ;;        which has been modified to prevent overflow.
        ;; Note2: The objective is an error tollerant average
        ;;        absolute error function (expressed as a percent
        ;;        of target value).
        (setq Rf (makeSVM M ^vectorSquareInnerProduct .1 400))
        (setRandom false 8191)
        (setq Rf (numericRegress X Y Rf 2 .05))
        ;; Display self test results for this test run.
        (writeln "Test Results:")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy)
        (loop for i from 0 until 10 do
           (setq y Y[i])
           (setq ey (Rf X[i]))
           (writeln "[" i "] y=" y ", ey=" ey)
           ) ; end loop
  

        ;; *********************************************************************************************
        (writeln _eol "Start Support Vector Machine Regression of a small scale crunched sigmoid quadratic polynomial with random noise.")
        ;; Start Support Vector Machine Regression of a small scale quadratic polynomial with random noise.
        ;; Model: y = C[0]*X[0]*X[0] + C[1]*X[1]*X[1] + ... + C[M-1]*X[M-1]*X[M-1] 
        ;; Hint: The Support Vector Machine Regression technology does NOT return
        ;;       a threshold constant. To get the threshold constant we supply
        ;;       a column of one's. The coefficient for the column of one's is
        ;;       the proper threshold coefficient for this model.
        (setq M 4)
        (setq N 500)
        (setq X (new Vector: object: N))
        (setq Y (new Vector: number: N))
        (setq C (new Vector: number: M))
        (loop for m from 0 until M do
           (setq C[m] (- (^random 10) 5))
           ) ; end C loop
        (loop for n from 0 until N do
           (setq X[n] (new Vector: number: M))
           (setq X[n][0] 1)
           (setq y (* C[0] X[n][0] X[n][0]))
           (loop for m from 1 until M do 
          	  (setq X[n][m] (- (^random 10) 5))
              (setq y (+ y (* C[m] X[n][m] X[n][m])))
              ) ; end M loop
		   (setq Y[n] (+ (* y .8) (* y (^random .4))))           
           ) ; end N loop
        ;; Determine the dynamic range all independent and dependent data into the sigmoid domain/range.
        ;; Crunching all independent and dependent data into the sigmoid domain/range.
        (setq minV BIGPOSNUM)
        (setq maxV BIGNEGNUM)
        (loop for n from 0 until N do
           (loop for m from 0 until M do
              (if (< X[n][m] minV) (setq minV X[n][m]))
              (if (> X[n][m] maxV) (setq maxV X[n][m]))
              ) ; end M loop
           (if (< Y[n] minV) (setq minV Y[n]))
           (if (> Y[n] maxV) (setq maxV Y[n]))
           ) ; end N loop
        (setq rngV (- maxV minV))
        ;; Crunching all independent and dependent data into the sigmoid domain/range.
        (loop for n from 0 until N do
           (loop for m from 0 until M do
              (setq X[n][m] (/ (- X[n][m] minV) rngV 4))
              (if (or (< X[n][m] 0) (> X[n][m] 1)) (error "numericRegress.selfTest: data not crunched properly")) 
              ) ; end M loop
           (setq Y[n] (/ (- Y[n] minV) rngV))
           (if (or (< Y[n] 0) (> Y[n] 1)) (error "numericRegress.selfTest: data not crunched properly")) 
           ) ; end N loop
        (setq rngV (- maxV minV))
        ;; Create a support vector machine estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        support vector machine regression model, 
        ;;        which has been modified to prevent overflow.
        ;; Note2: The objective is an error tollerant average
        ;;        absolute error function (expressed as a percent
        ;;        of target value).
        (setq Rf (makeSVM M ^vectorSquareInnerProduct .1 50))
        (setRandom false 8191)
        (setq Rf (numericRegress X Y Rf 2 .05))
        ;; Display self test results for this test run.
        (writeln "Test Results:")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy
                 ", MinV=" minV
                 ", MaxV=" maxV
                 ", RngV=" rngV)
        (loop for i from 0 until 10 do
           (setq y Y[i])
           (setq ey (Rf X[i]))
           (writeln "[" i "] y=" y ", ey=" ey)
           ) ; end loop
  

        ;; *********************************************************************************************
        (writeln _eol "Start Support Vector Machine Regression of a small scale quadratic polynomial.")
        (writeln      "Note: demonstrates how decreasing Gaussian Sample Size destroys accuracy.")
        ;; Start Support Vector Machine Regression of a small scale quadratic polynomial.
        ;; Note: demonstrates how decreasing Gaussian Sample Size destroys accuracy.
        ;; Model: y = C[0]*X[0]*X[0] + C[1]*X[1]*X[1] + ... + C[M-1]*X[M-1]*X[M-1] 
		;; Note1: Support Vector Machine regression can be highly accurate
		;;        when solving non-linear regression problems. However, the
		;;        accuracy varies from excellent to poor depending upon
		;;        the ratio of: the chosen Gaussian Sample Size (maxSVSize);
		;;        the number of regression variables (M); and the THEORETICAL
		;;        number of variables created by the kernel function to
		;;        make the non-linear problem linearly solvable. A simplified
		;;        example would be as follows.
		;;
		;;		  Solving a quadratic regression problem with variableCount == 3,
		;;        y = sum{m from 0 until variableCount}(Cm*Xm*Xm), and 
		;;        a kernel function of vectorSquareInnerProduct, is very
		;;        accurate with a Gaussian Sample Size (maxSVSize) of 10.
		;;        However, if the variableCount is increased to 10, then the
		;;        accuracy completely breaks down and is not restored until
		;;        the Gaussian Sample Size is increased to around 100. An
		;;        explanation is as follows.
		;;
		;;		  In order to make the quadratic regression linearly tractable,
		;;        the vectorSquareInnerProduct performs an on-the-fly squaring
		;;        of each training point vector. Thus, with a training point
		;;        vector of size three, the vectorSquareInnerProduct creates
		;;        the following on-the-fly THEORETICAL new training point:
		;;		  kernel(X1,X2,X3) => (X1,X2,X3,X1*X1,X2*X2,X3*X3,X1*X2,X1*X3,X2*X3).
		;;        Clearly the problem is now linearly solvable because the squared
		;;        variables are now terms in the THEORETICAL on-the-fly linear regression
		;;        created by the kernel. NOTICE however, that the THEORETICAL linear
		;;        regression, created on-the-fly by the kernel function, has nine variables
		;;        not three variables as in the original quadratic problem. Unless the
		;;        number of training points is greater than nine, and the Gaussian 
		;;        sample size is greater than nine, the on-the-fly linear regression
		;;        will not have enough data to get accurate results. This test case
		;;        demonstrates the fall in accuracy as Gaussian Sample Size is decreased. 
        (setq M 10)
        (setq N 500)
        (setq G 100)
        (setq X (new Vector: object: N))
        (setq Y (new Vector: number: N))
        (setq C (new Vector: number: M))
        (loop for m from 0 until M do
           (setq C[m] (- (^random 10) 5))
           ) ; end C loop
        (loop for n from 0 until N do
           (setq X[n] (new Vector: number: M))
           (setq X[n][0] 1)
           (setq y (* C[0] X[n][0] X[n][0]))
           (loop for m from 1 until M do 
          	  (setq X[n][m] (- (^random 10) 5))
              (setq y (+ y (* C[m] X[n][m] X[n][m])))
              ) ; end M loop
		   (setq Y[n] y)           
           ) ; end N loop
                      
        (writeln "...(Accuracy is excellent with Gaussian sample size at full strength)")
        ;; Create a support vector machine estimator Lambda.
        ;; Note1: Use the vectorSquareInnerProduct kernel,
        ;;        which converts the quatratic regression
        ;;        problem into a linear regression problem
        ;;        by effectively creating extra variables
        ;;        on-the-fly.
        (setq Rf (makeSVM M ^vectorSquareInnerProduct .1 G))
        (setq Rf (numericRegress X Y Rf 2 .05))
        ;; Display self test results for this test run.
        (writeln "Test Results: (exact fit)")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy)
        (loop for i from 0 until 10 do
           (setq y Y[i])
           (setq ey (Rf X[i]))
           (setq py (if (<> y 0) (abs (/ (- ey y) y)) (abs (- ey y))))
           (writeln "[" i "] y=" y ", ey=" ey ", %e=" py)
           ) ; end loop
                                   
        (writeln "...Using the same data, reduce the Gaussian sample size to one half.")
        (writeln ".....(Accuracy is reduced)")
        ;; Using the same data, reduce the Gaussian sample size to one half.
        (setq Rf (makeSVM M ^vectorSquareInnerProduct .1 (divi G 2)))
        (setq Rf (numericRegress X Y Rf 2 .05))
        ;; Display self test results for this test run.
        (writeln "Test Results: (one half sample size)")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy)
        (loop for i from 0 until 10 do
           (setq y Y[i])
           (setq ey (Rf X[i]))
           (setq py (if (<> y 0) (abs (/ (- ey y) y)) (abs (- ey y))))
           (writeln "[" i "] y=" y ", ey=" ey ", %e=" py)
           ) ; end loop
                                   
        (writeln "...Using the same data, reduce the Gaussian sample size to one quarter.")
        (writeln ".....(Accuracy is seriously reduced)")
        ;; Using the same data, reduce the Gaussian sample size to one quarter.
        (setq Rf (makeSVM M ^vectorSquareInnerProduct .1 (divi G 4)))
        (setq Rf (numericRegress X Y Rf 2 .05))
        ;; Display self test results for this test run.
        (writeln "Test Results: (one quarter sample size)")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy)
        (loop for i from 0 until 10 do
           (setq y Y[i])
           (setq ey (Rf X[i]))
           (setq py (if (<> y 0) (abs (/ (- ey y) y)) (abs (- ey y))))
           (writeln "[" i "] y=" y ", ey=" ey ", %e=" py)
           ) ; end loop
                                   
        (writeln "...Using the same data add some random noise, but restore the sample size to full strength.")
        (writeln ".....(Accuracy is good, even with difficult data, with Gaussian sample size at full strength)")
        ;; Using the same data add some random noise.
        (loop for n from 0 until N do
		   (setq Y[n] (+ (* Y[n] .8) (* Y[n] (^random .4))))           
           ) ; end N loop
        (setq Rf (makeSVM M ^vectorSquareInnerProduct .1 G))
        (setq Rf (numericRegress X Y Rf 2 .05))
        ;; Display self test results for this test run.
        (writeln "Test Results: (added random noise but full sample size)")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy)
        (loop for i from 0 until 10 do
           (setq y Y[i])
           (setq ey (Rf X[i]))
           (setq py (if (<> y 0) (abs (/ (- ey y) y)) (abs (- ey y))))
           (writeln "[" i "] y=" y ", ey=" ey ", %e=" py)
           ) ; end loop
                                                                      

        ;; ============================================================
        ;; Neural Net Regression
        ;; ============================================================
          
        ;; *********************************************************************************************
        (writeln _eol "Start Neural Net Regression of a small scale sigmoid exponential model.")
        ;; Start Neural Net Regression of a small scale sigmoid exponential model.
        ;; Model: y = -1 + expt(2x1,1) + expt(4x2,1.5) 
        ;; Hint: This test case gets much more accurate
        ;;       than the genetic algorithm, but it does  
        ;;		 so by overfitting in the center leaving
        ;;       the extrema much less acuurate. In general
        ;;       the neural net fails to understand the model.
        (setq VN 500)
        (setq Y #void)
        (setq X (new Vector: object: VN))
        (loop for i from 0 until VN do
            (setq X[i] (new Vector: number: 2 (/ i VN) (/ (- VN i) VN)))
            (setq X[i][2] (+ -1 (expt (* 2 X[i][0]) 1) (expt (* 4 X[i][1]) 1.5)))
            ) ; end loop
        ;; Create a exponential neural net estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        continuous neural net regression model, 
        ;;        which has been modified to prevent 
        ;;        overflow.
        ;; Note2: A standard least squares objective
        ;;        function is used.
        (setq Rf (makeNeural 2 continuous:))
        ;; Regress the neural net.
        (setRandom false 8191)
        (setq Rf (numericRegress X Y Rf 400 .000001))
        ;; Display self test results for this test run.
        (writeln "Test Results:")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy
                 ", F=" Rf.Filter)
        (loop for i from 0 until (length X) by 10 do
           (setq y X[i][(sub1 (length X[i]))])
           (setq ey (Rf X[i]))
           (writeln "[" i "] y=" y ", ey=" ey)
           ) ; end loop
        

        ;; *********************************************************************************************
        (writeln _eol "Start Neural Net Regression of a small scale sigmoid logit model.")
        ;; Start Neural Net Regression of a small scale sigmoid logit model.
        ;; Model: y = (exp(-1 + 2x1 + 4x2) / (1 + exp(-1 + 2x1 + 4x2))) 
        ;; Hint: This test case gets fairly accurate and is an
        ;;       example of the efficient use of neural nets.
        (setq VN 500)
        (setq Y #void)
        (setq X (new Vector: VN))
        (loop for i from 0 until VN do
            (setq X[i] (new Vector: number: 2 (/ i VN) (/ (- VN i) VN)))
            (setq temp (exp (+ -1 (* 2 X[i][0]) (* 4 X[i][1])))) 
            (setq X[i][2] (/ temp (+ 1 temp)))
            ) ; end loop
        ;; Create a sigmoid neural net estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        neural net regression model, which has
        ;;        been modified to prevent overflow.
        ;; Note2: A standard least squares objective
        ;;        function is used.
        (setq Rf (makeNeural 2 sigmoid:))
        (setq Rf.hiddenCount 1)
        (setRandom false 8191)
        (setq Rf (numericRegress X Y Rf 1000 0))
        ;; Display self test results for this test run.
        (writeln "Test Results:")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", N=" Rf.Net
                 ", S=" Rf.Strategy
                 ", F=" Rf.Filter)
        (loop for i from 0 until (length X) by 10 do
           (setq y X[i][(sub1 (length X[i]))])
           (setq ey (Rf X[i]))
           (writeln "[" i "] y=" y ", ey=" ey)
           ) ; end loop
 

        ;; ********************************************************************************************************************
        (writeln _eol "Start Neural Net Regression of a small scale sigmoid exponential model with special objective function.")
        ;; Start Neural Net Regression of a small scale sigmoid exponential model with special objective function.
        ;; Model: y = -1 + expt(2x1,1) + expt(4x2,1.5) 
        ;; Hint: This test case gets much more accurate
        ;;       by replacing the standard least squares error  
        ;;		 objective function with an objective function
        ;;       which greatly punishes false positives. This user
		;;		 objective function, instead of back propagation, trains
        ;;       the net to return .95 iff target is >= 3.0. In general,
        ;;       a great deal can be accomplished with non-standard
        ;;       objective functions.
        (setq VN 500)
        (setq Y #void)
        (setq X (new Vector: VN))
        (loop for i from 0 until VN do
            (setq X[i] (new Vector: number: 2 (/ i VN) (/ (- VN i) VN)))
            (setq X[i][2] (+ -1 (expt (* 2 X[i][0]) 1) (expt (* 4 X[i][1]) 1.5)))
            ) ; end loop
        ;; Create a neural net estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        neural net regression model, which has
        ;;        been modified to prevent overflow.
        ;; Note2: Use user errors instead of back propagation, to train
        ;;        the net to return 95 iff target is >= 3.0
        (setq Rf (eval {
          (lambda(v)
             pvars:(C                   ; Coefficient vector
                    Error           	; Final objective error after training
                    Filter				; My neural net output filter
                    G                 	; Final generation count after training
                    hiddenCount			; My neural net hidden variables
                    Net					; My neural net object
                    Objective       	; Objective function to use in training
                    P                	; Final population count after training
                    Rf              	; My estimator Lambda
                    (Strategy neural:)	; My optimization strategy
                    variableCount		; Estimator Lambda input variables
                    ) ; end of persistant variables
             vars:(i j result M ey y)
             ;; Define a standard least square objective function
             ;; Note: The neural net objective function must also
             ;;       expose the neural net to the whole training
             ;;       set while scoring. 
             (defun Objective(X)
                vars:(i result M N err delta y ey)
                (setq result 0)
                (setq err (new Vector: number: 1 0))
                (setq M (- (length X[0]) 1))
                (setq N (length X))
                (loop for i from 0 until N do 
		           (propagateForward:Rf.Net X[i])
		           (setq ey Rf.Net.output.outputs[0])
		           (setq y X[i][M])
		           (setq err[0] 
		              (cond
		                 ((and (>= ey .95) (>= y 3.0)) 0)
		                 ((and (>= ey .95) (< y 3.0)) (- N))
		                 ((and (< ey .95)  (>= y 3.0)) (/ N 10))
		                 ((and (< ey .95)  (< y 3.0)) 0)		           
		               )) ; end cond
		           (if (<> err[0] 0) (propagateUserError:Rf.Net err))
		           (setq result (+ result (abs err[0])))
                   ) ; end training loop
                (/ result N)) ; end define objective function
             ;; Begin estimator Lambda main logic 
             (propagateForward:Net v)
             (setq result Net.output.outputs[0]) 
             ;; Forceably prevent overflow
             (setq result (max (min result BIGPOSNUM) BIGNEGNUM))
             result) }
             )) ; end define estimator Lambda
        ;; Set the coefficient vector to the number of variables plus the constant coefficient.
        (setq Rf.Filter sigmoid:)
        (setq Rf.variableCount 2)
        (setq Rf.hiddenCount 5)
        (setq Rf.C (new Vector: number: 1 0))
        (setq Rf.Net (makeNeuralNet Rf.variableCount Rf.hiddenCount 1 Rf.Filter))
        (setq Rf.C (getWeights:Rf.Net))
        (setq Rf.Rf Rf)
        ;; Regress the neural net.
        (setRandom false 8191)
        (setq Rf (numericRegress X Y Rf 400 .000001))
        ;; Display self test results for this test run.
        (writeln "Test Results:")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", N=" Rf.Net
                 ", S=" Rf.Strategy
                 ", F=" Rf.Filter)
        (loop for i from 0 until (length X) by 10 do
           (setq y X[i][(sub1 (length X[i]))])
           (setq ey (Rf X[i]))
           (writeln "[" i "] y=" y ", ey=" ey)
           ) ; end loop

        ;; ============================================================
        ;; Genetic Evolutionary Regression
        ;; ============================================================
          
        ;; ********************************************************************************************************************
        (writeln _eol "Start Genetic Evolutionary Regression of a small scale co-evolutionary polynomial model.")
        ;; Start Genetic Evolutionary Regression of a small scale co-evolutionary polynomial model.
        ;; Model: y = -1 + (2*expt(x[0],2.5)) + (4*expt(x[1],1.5)) 
        ;; Hint: This test case gets much more accurate,
        ;;       By simply crunching the input into the 
        ;;		 sigmoid domain and adding more data points.
        (setq VN 500)
        (setq Y #void)
        (setq X (new Vector: VN))
        (setRandom false 8191)
        (loop for i from 0 until VN do
            (setq X[i] (new Vector: number: 2 (srandom 1.0) (srandom 1.0)))
            (setq X[i][2] (+ -1 (* 2 (expt X[i][0] 2.5)) (* 4 (expt X[i][1] 1.5))))
            ) ; end loop
        ;; Create a co-evolutionary exponential estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        exponential regression model, which has
        ;;        been modified to prevent overflow.
        ;; Note2: A standard least squares objective
        ;;        function is used.
        (setq Rf (symbolicRegress.makeCoPolynomial 2))
        (setq Rf (numericRegress X Y Rf 2.0500 0))
        ;; Display self test results for this test run.
        (writeln "Test Results: (seed=" srandom.seed ")")
        (writeln "Actual: y = -1 + (2*expt(x[0],2.5) + (4*expt(x[1],1.5))")
        (writeln (Rf.show))

        ;; ********************************************************************************************************************
        (writeln _eol "Start Genetic Evolutionary Regression of a small scale sinusoidal model.")
        ;; Start Genetic Evolutionary Regression of a small scale sinusoidal model.
        ;; Model: y = x2(.1 + .2sin(.5x1)) 
        ;; Hint: This test case gets much more accurate,
        ;;       By simply crunching the input into the 
        ;;		 sigmoid domain and adding more data points.
        (setq VN 500)
        (setq Y #void)
        (setq X (new Vector: VN))
        (setRandom false 8191)
        (loop for i from 0 until VN do
            (setq X[i] (new Vector: number: 2 (srandom 1.0) (srandom 1.0)))
            (setq X[i][2] (* X[i][1] (+ .1 (* .2 (sin (* .5 X[i][0]))))))
            ) ; end loop
        ;; Create a sinusoidal estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        sinusoidal regression model, which has
        ;;        been modified to prevent overflow.
        ;; Note2: A standard least squares objective
        ;;        function is used.
        (setq Rf (makeSinusoidal))
        (setq Rf (numericRegress X Y Rf 100 0))
        ;; Display self test results for this test run.
        (writeln "Test Results: (seed=" srandom.seed ")")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy
                 ", C[0]:{.1}=" Rf.C[0]
                 ", C[1]:{.2}=" Rf.C[1]
                 ", C[2]:{.5}=" Rf.C[2])
        (loop for i from 0 until 10 do
          (setq y X[i][(sub1 (length X[i]))])
          (setq ey (Rf X[i]))
          (writeln "[" i "] y=" y ", ey=" ey)
          ) ; end loop


        ;; ********************************************************************************************************************
        (writeln _eol "Start Genetic Evolutionary Regression of a small scale exponential model.")
        ;; Start Genetic Evolutionary Regression of a small scale exponential model.
        ;; Model: y = -1 + expt(2x1,1) + expt(4x2,1.5) 
        ;; Hint: This test case gets much more accurate,
        ;;       By simply crunching the input into the 
        ;;		 sigmoid domain and adding more data points.
        (setq VN 500)
        (setq Y #void)
        (setq X (new Vector: VN))
        (loop for i from 0 until VN do
            (setq X[i] (new Vector: number: 2 (/ i VN) (/ (- VN i) VN)))
            (setq X[i][2] (+ -1 (expt (* 2 X[i][0]) 1) (expt (* 4 X[i][1]) 1.5)))
            ) ; end loop
        ;; Create a simple exponential estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        exponential regression model, which has
        ;;        been modified to prevent overflow.
        ;; Note2: A standard least squares objective
        ;;        function is used.
        (setq Rf (makeExponential 2))
        (setRandom false 8191)
        (setq Rf (numericRegress X Y Rf 500 0))
        ;; Display self test results for this test run.
        (writeln "Test Results: (seed=" srandom.seed ")")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy
                 ", C[0]:{-1}=" Rf.C[0]
                 ", C[1]:{2.0}=" Rf.C[1]
                 ", C[2]:{1.0}=" Rf.C[2]
                 ", C[3]:{4.0}=" Rf.C[3]
                 ", C[4]:{1.5}=" Rf.C[4])
        (loop for i from 0 until 10 do
          (setq y X[i][(sub1 (length X[i]))])
          (setq ey (Rf X[i]))
          (writeln "[" i "] y=" y ", ey=" ey)
          ) ; end loop
        

        ;; ********************************************************************************************************************
        (writeln _eol "Start Genetic Evolutionary Regression of a small scale exponential model with longer training period.")
        ;; Start Genetic Evolutionary Regression of a small scale exponential model with longer training period.
        ;; Model: y = -1 + expt(2x1,1) + expt(4x2,1.5) 
        ;; Hint: This test case gets much more accurate,
        ;;       By simply crunching the input into the 
        ;;		 sigmoid domain and adding more data points.
        (setq VN 500)
        (setq Y #void)
        (setq X (new Vector: VN))
        (setRandom false 8191)
        (loop for i from 0 until VN do
            (setq X[i] (new Vector: number: 2 (srandom 1.0) (srandom 1.0)))
            (setq X[i][2] (+ -1 (expt (* 2 X[i][0]) 1) (expt (* 4 X[i][1]) 1.5)))
            ) ; end loop
        ;; Create a simple exponential estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        exponential regression model, which has
        ;;        been modified to prevent overflow.
        ;; Note2: A standard least squares objective
        ;;        function is used.
        (setq Rf (makeExponential 2))
        (setq Rf (numericRegress X Y Rf 1500 0))
        ;; Display self test results for this test run.
        (writeln "Test Results: (seed=" srandom.seed ")")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy
                 ", C[0]:{-1}=" Rf.C[0]
                 ", C[1]:{2.0}=" Rf.C[1]
                 ", C[2]:{1.0}=" Rf.C[2]
                 ", C[3]:{4.0}=" Rf.C[3]
                 ", C[4]:{1.5}=" Rf.C[4])
        (loop for i from 0 until 10 do
          (setq y X[i][(sub1 (length X[i]))])
          (setq ey (Rf X[i]))
          (writeln "[" i "] y=" y ", ey=" ey)
          ) ; end loop


        ;; ********************************************************************************************************************
        (writeln _eol "Start Genetic Evolutionary Regression of a small scale exponential model using a binary genome.")
        ;; Start Genetic Evolutionary Regression of a small scale exponential model using a binary genome.
        ;; (Experimental)
        ;; Model: y = -1 + expt(2x1,1) + expt(4x2,3) 
        ;; Hint: This test case gets much more accurate,
        ;;       By simply crunching the input into the 
        ;;		 sigmoid domain and adding more data points.
        (setq VN 500)
        (setq Y #void)
        (setq X (new Vector: VN))
        (setRandom false 8191)
        (loop for i from 0 until VN do
            (setq X[i] (new Vector: number: 2 (srandom 1.0) (srandom 1.0)))
            (setq X[i][2] (+ -1 (expt (* 2 X[i][0]) 1) (expt (* 4 X[i][1]) 1.5)))
            ) ; end loop
        ;; Create a binary exponential estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        exponential regression model, which has
        ;;        been modified to prevent overflow.
        ;; Note2: A standard least squares objective
        ;;        function is used.
        (setq Rf (makeBExponential 2))
        (setq Rf (numericRegress X Y Rf 10 0))
        ;; Display self test results for this test run.
        (writeln "Test Results: (seed=" srandom.seed ")")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" (integer Rf.P)
                 ", G=" (integer Rf.G)
                 ", F=" (integer Rf.Fe)
                 ", S=" Rf.Strategy
                 ", C[0]:{-1}=" Rf.RC[0]
                 ", C[1]:{2.0}=" Rf.RC[1]
                 ", C[2]:{1.0}=" Rf.RC[2]
                 ", C[3]:{4.0}=" Rf.RC[3]
                 ", C[4]:{1.5}=" Rf.RC[4]
                 ", CBits=" Rf.C)
        (loop for i from 0 until 10 do
          (setq y X[i][(sub1 (length X[i]))])
          (setq ey (Rf X[i]))
          (writeln "[" i "] y=" y ", ey=" ey)
          ) ; end loop 


        ;; ********************************************************************************************************************
        (writeln _eol "Start Genetic Evolutionary Regression of a small scale sigmoid logit model.")
        ;; Start Genetic Evolutionary Regression of a small scale sigmoid logit model.
        ;; Model: y = (exp(-1 + 2x1 + 4x2) / (1 + exp(-1 + 2x1 + 4x2))) 
        ;; Hint: This test case gets fairly accurate and is an
        ;;       example of the efficient use of genetic algorithms.
        (setq VN 500)
        (setq Y #void)
        (setq X (new Vector: VN))
        (loop for i from 0 until VN do
            (setq X[i] (new Vector: number: 2 (/ i VN) (/ (- VN i) VN)))
            (setq temp (exp (+ -1 (* 2 X[i][0]) (* 4 X[i][1])))) 
            (setq X[i][2] (/ temp (+ 1 temp)))
            ) ; end loop
        ;; Create a sigmoid logit estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        logit regression model, which has
        ;;        been modified to prevent overflow.
        ;; Note2: A standard least squares objective
        ;;        function is used.
        (setq Rf (makeLogit 2))
        (setRandom false 8191)
        (setq Rf (numericRegress X Y Rf 1000 0))
        ;; Display self test results for this test run.
        (writeln "Test Results:")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy
                 ", C[0]:{-1}=" Rf.C[0]
                 ", C[1]:{2}=" Rf.C[1]
                 ", C[2]:{4}=" Rf.C[2])
        (loop for i from 0 until 10 do
           (setq y X[i][(sub1 (length X[i]))])
           (setq ey (Rf X[i]))
           (writeln "[" i "] y=" y ", ey=" ey)
           ) ; end loop
             

        ;; ============================================================
        ;; Induced Regression
        ;; ============================================================


        ;; *********************************************************************************************
        (writeln _eol "Start Induced Regression of a mid scale quadratic polynomial with random noise.")
        ;; Start Induced Regression of a small scale quadratic polynomial with random noise.
        ;; Model: y = C[0]*X[0]*X[0] + C[1]*X[1]*X[1] + ... + C[M-1]*X[M-1]*X[M-1] 
        (setq M 20)
        (setq N 5000)
        (setq X (new Matrix: number: 2 N (addi M 1)))
        (setq Y #void)
        (setq C (new Vector: number: M))
        (loop for m from 0 to M do
           (setq C[m] (- (^random 10) 5))
           ) ; end C loop
        (loop for n from 0 until N do
           (setq y 0)
           (loop for m from 0 until M do 
          	  (setq X[n m] (- (^random 10) 5))
              (setq y (+ y (* C[m] X[n m] X[n m])))
              ) ; end M loop
		   (setq X[n M] (+ (* y .8) (* y (^random .4))))           
           ) ; end N loop
        ;; Create an induced general polynomial estimator Lambda.
        ;; Note1: The estimator Lambda is a general 
        ;;        polynomial regression model, which has
        ;;        been modified to prevent overflow.
        ;; Note2: A error tollerant objective function is used.
        (setq Rf (numericRegress.makePolynomial M 3 continuous: .1))
        (setq Rf (numericRegress X Y Rf 1 0))
        ;; Display self test results for this test run.
        (writeln "Test Results:")
        (writeln (Rf.show))
        (loop for n from 0 until 10 do
           (setq y X[n M])
           (setq ey (Rf (math.matrixRowToVector X n M)))
           (writeln "[" n "] y=[" y "], ey=[" ey "], err=[" (- y ey) "]")
           ) ; end loop
        
        ;; ********************************************************************************************************************
        (writeln _eol "Start Induced Regression of a small scale exponential model using a continuous euclidean estimator.")
        ;; Start Genetic Evolutionary Regression of a small scale exponential model using a continuous euclidean estimator.
        ;; Model: y = -1 + expt(2x1,1) + expt(4x2,3) 
        ;; Hint: This test case gets much more accurate,
        ;;       By simply crunching the input into the 
        ;;		 sigmoid domain and adding more data points.
        (setq VN 500)
        (setq Y #void)
        (setq X (new Vector: VN))
        (setRandom false 8191)
        (loop for i from 0 until VN do
            (setq X[i] (new Vector: number: 2 (srandom 1.0) (srandom 1.0)))
            (setq X[i][2] (+ -1 (expt (* 2 X[i][0]) 1) (expt (* 4 X[i][1]) 1.5)))
            ) ; end loop
        ;; Create a euclidean estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        euclidean regression model, which has
        ;;        been modified to prevent overflow.
        ;; Note2: A standard least squares objective
        ;;        function is used.
        (setq Rf (makeEuclidean 2 continuous: #void true))
        (setq Rf (numericRegress X Y Rf 500 0))
        ;; Display self test results for this test run.
        (writeln "Test Results: (seed=" srandom.seed ")")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy)
        (loop for i from 0 until 10 do
          (setq y X[i][(sub1 (length X[i]))])
          (setq ey (Rf X[i]))
          (writeln "[" i "] y=" y ", ey=" ey)
          ) ; end loop
        
        ;; ********************************************************************************************************************
        (writeln _eol "Start Induced Regression of a small scale exponential model using a decile crunching euclidean estimator.")
        ;; Start Genetic Evolutionary Regression of a small scale exponential model using a decile crunching euclidean estimator.
        ;; Model: y = -1 + expt(2x1,1) + expt(4x2,3) 
        ;; Hint: This test case gets much more accurate,
        ;;       By simply crunching the input into the 
        ;;		 sigmoid domain and adding more data points.
        (setq VN 500)
        (setq Y #void)
        (setq X (new Vector: VN))
        (setRandom false 8191)
        (loop for i from 0 until VN do
            (setq X[i] (new Vector: number: 2 (srandom 1.0) (srandom 1.0)))
            (setq X[i][2] (+ -1 (expt (* 2 X[i][0]) 1) (expt (* 4 X[i][1]) 1.5)))
            ) ; end loop
        ;; Create a euclidean estimator Lambda.
        ;; Note1: The estimator Lambda is a standard 
        ;;        euclidean regression model, which has
        ;;        been modified to prevent overflow.
        ;; Note2: A standard least squares objective
        ;;        function is used.
        ;; Note3: A standard decile input crunching
        ;;        function is used.
        (setq Rf (makeEuclidean 2 continuous: decile: true))
        (setq Rf (numericRegress X Y Rf 500 0))
        ;; Display self test results for this test run.
        (writeln "Test Results: (seed=" srandom.seed ")")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy)
        (loop for i from 0 until 10 do
          (setq y X[i][(sub1 (length X[i]))])
          (setq ey (Rf X[i]))
          (writeln "[" i "] y=" y ", ey=" ey)
          ) ; end loop
        
       ;; End self test and display time required.
       SelfTestLast::
       (setq endTime (getTickCount startTime))
       (writeln "Timing = " endTime " seconds.")
       true) ; end selfTest
    ;; Perform a self test of this general regression Lambda.
    (defun testWorkBench()
        pvars:(Rf Sf)
        vars:( i I k K m M n N in 
              VN temp crunch sum 
              startTime endTime
              minV maxV rngV 
              err avgErr avgY minErr maxErr
              c C x X KX KY ey py y Y g G Ss) 
              
        (writeln _eol "*************************math.numericRegress.testWorkBench*********************")
        (gc)(setq startTime (getTickCount 0))
        
        ;; ============================================================
        ;; Support Vector Regression
        ;; ============================================================
        
        ;; *********************************************************************************************
        (writeln _eol "Start Support Vector Machine Regression of a large scale square polynomial.")
        (writeln      "Note: demonstrates how decreasing Gaussian Sample Size destroys accuracy.")
        ;; Start Support Vector Machine Regression of a small scale cubic polynomial.
        ;; Note: demonstrates how decreasing Gaussian Sample Size destroys accuracy.
        ;; Model: y = C[0]*X[0]*X[0] + C[1]*X[1]*X[1] + ... + C[M-1]*X[M-1]*X[M-1] 
		;; Note1: Support Vector Machine regression can be highly accurate
		;;        when solving non-linear regression problems. However, the
		;;        accuracy varies from excellent to poor depending upon
		;;        the ratio of: the chosen Gaussian Sample Size (maxSVSize);
		;;        the number of regression variables (M); and the THEORETICAL
		;;        number of variables created by the kernel function to
		;;        make the non-linear problem linearly solvable. A simplified
		;;        example would be as follows.
		;;
		;;		  Solving a quadratic regression problem with variableCount == 3,
		;;        y = sum{m from 0 until variableCount}(Cm*Xm*Xm), and 
		;;        a kernel function of vectorSquareInnerProduct, is very
		;;        accurate with a Gaussian Sample Size (maxSVSize) of 10.
		;;        However, if the variableCount is increased to 10, then the
		;;        accuracy completely breaks down and is not restored until
		;;        the Gaussian Sample Size is increased to around 100. An
		;;        explanation is as follows.
		;;
		;;		  In order to make the quadratic regression linearly tractable,
		;;        the vectorSquareInnerProduct performs an on-the-fly squaring
		;;        of each training point vector. Thus, with a training point
		;;        vector of size three, the vectorSquareInnerProduct creates
		;;        the following on-the-fly THEORETICAL new training point:
		;;		  kernel(X1,X2,X3) => (X1,X2,X3,X1*X1,X2*X2,X3*X3,X1*X2,X1*X3,X2*X3).
		;;        Clearly the problem is now linearly solvable because the squared
		;;        variables are now terms in the THEORETICAL on-the-fly linear regression
		;;        created by the kernel. NOTICE however, that the THEORETICAL linear
		;;        regression, created on-the-fly by the kernel function, has nine variables
		;;        not three variables as in the original quadratic problem. Unless the
		;;        number of training points is greater than nine, and the Gaussian 
		;;        sample size is greater than nine, the on-the-fly linear regression
		;;        will not have enough data to get accurate results. This test case
		;;        demonstrates the fall in accuracy as Gaussian Sample Size is decreased. 
        (setq M 20)
        (setq N 50000)
        (setq Ss (expt M 2.0))
        (setq X (new Vector: object: N))
        (setq Y (new Vector: number: N))
        (setq C (new Vector: number: M))
        (loop for m from 0 until M do
           (setq C[m] (- (|Gv:random| 10) 5))
           ) ; end C loop
        (loop for n from 0 until N do
           (setq X[n] (new Vector: number: M))
           (setq X[n][0] 1)
           (setq y (* C[0] X[n][0] X[n][0]))
           (loop for m from 1 until M do 
          	  (setq X[n][m] (- (|Gv:random| 10) 5))
              (setq y (+ y (* C[m] X[n][m] X[n][m] X[n][m])))
              ) ; end M loop
		   (setq Y[n] y)           
           ) ; end N loop
                      
        (writeln "...(Accuracy is excellent with Gaussian sample size at full strength)")
        ;; Create a support vector machine estimator Lambda.
        ;; Note1: Use the vectorSquareInnerProduct kernel,
        ;;        which converts the quatratic regression
        ;;        problem into a linear regression problem
        ;;        by effectively creating extra variables
        ;;        on-the-fly.
        (setq Rf (makeSVM M |Gv:vectorSquareInnerProduct| .1 Ss))
        (setq Rf (numericRegress X Y Rf 3 .05))
        ;; Display self test results for this test run.
        (writeln "Test Results: (exact fit)")
        (writeln "==>"
                 "  E=" Rf.Error 
                 ", P=" Rf.P
                 ", G=" Rf.G
                 ", S=" Rf.Strategy)
        (loop for i from 0 until 10 do
           (setq y Y[i])
           (setq ey (Rf X[i]))
           (setq py (if (<> y 0) (abs (/ (- ey y) y)) (abs (- ey y))))
           (writeln "[" i "] y=" y ", ey=" ey ", %e=" py)
           ) ; end loop
                                   
                                   
               
       ;; End self test and display time required.
       LastTest::
       (setq endTime (getTickCount startTime))
       (writeln "Timing = " endTime " seconds.")
       true) ; end testWorkBench
    ;; *******************************************************************
    ;; Main Logic Section
    ;; *******************************************************************
    ;; Compute the number of independent variables.
    ;; Note: We must discover what input options have
    ;;       been used with arguments X and Y.
    (cond
      ;; Is X an N x M+1 vector array?
      ((and (isVector X) (= Y #void)) (setq naCount (sub1 (length X[0]))))
      ;; Is X an N x M vector array?
      ((and (isVector X) (isVector Y)) (setq naCount (length X[0])))
      ;; Is X an N x M+1 number matrix?
      ((and (isMatrix X) (= Y #void)) (setq naCount (sub1 (rank X)[1])))
      ;; Is X an N x M number matrix?
      ((and (isMatrix X) (isVector Y)) (setq naCount (rank X)[1]))
      (else (error "math.numericRegress: expecting either a vector array or matrix as first argument"))
      ) ; end cond
    ;; Initialize Rf estimator Lambda (if necessary).
    ;; Note: We supply some standard estimator Lambdas for use by name
    ;;       as shown in the code below. Each estimator Lambda, shown
    ;;       below, uses one of the supported regression strategies.  
    (cond
      ((or (= Rf #void) (= Rf linear:)) (setq Rf (makeLinear naCount)))
      ((and (isLambda Rf) (= Rf.Strategy #void)) (setq Rf.Strategy evolve:))
      ((= Rf bExponential:) (setq Rf (makeBExponential naCount)))
      ((= Rf cluster:) (setq Rf (makeCluster naCount continuous:)))
      ((= Rf euclidean:) (setq Rf (makeEuclidean naCount continuous:)))
      ((= Rf exponential:) (setq Rf (makeExponential naCount)))
      ((= Rf polynomial:) (setq Rf (makePolynomial naCount continuous: 1)))
      ((= Rf linearEvolve:) (setq Rf (makeLinearEvolve naCount)))
      ((= Rf svm:) (setq Rf (makeSVM naCount ^vectorInnerProduct ErrTollerance 100)))
      ((= Rf linearNetwork:) (setq Rf (makeLinearNetwork naCount continuous:)))
      ((= Rf logit:) (setq Rf (makeLogit naCount)))
      ((= Rf neighbor:) (setq Rf (makeNeighbor naCount continuous:)))
      ((= Rf neural:) (setq Rf (makeNeural naCount continuous:)))
      ((= Rf sinusoidal:) (setq Rf (makeSinusoidal)))
      ((= Rf splitNeural:) (setq Rf (makeSplitNeural naCount continuous:)))
      ((= Rf tailExponential:) (setq Rf (makeTailExponential naCount .10)))
      ) ; end cond
    ;; Perform the requested regression strategy specified in Rf.Strategy variable.
    (cond 
	  ;; Multivariate Linear Regression  
      ((= Rf.Strategy linear:) (return (linearRegression X Y Rf)))
	  ;; Support Vector Machine Regression  
      ((= Rf.Strategy svm:) (return (supportRegression X Y Rf Gmax err)))
	  ;; Neural Net Regression  
      ((= Rf.Strategy neural:) (return (neuralRegression X Y Rf Gmax err)))
	  ;; Genetic Evolutionary Algorithms (real number genome)  
      ((= Rf.Strategy evolve:) (return (numberEvolution X Y Rf Gmax err)))
	  ;; Genetic Evolutionary Algorithms (binary genome)  
      ((= Rf.Strategy evolveBinary:) (return (binaryEvolution X Y Rf Gmax err)))
	  ;; Genetic Evolutionary Algorithms (binary genome) with memos  
      ((= Rf.Strategy evolveBinaryMemo:) (return (binaryMemoEvolution X Y Rf Gmax err)))
	  ;; Induced Regression (user supplied technology)  
      ((= Rf.Strategy induce:) (return (induceRegression X Y Rf)))
      ;; All other strategies default to: Genetic Evolutionary Algorithms (real number genome)  
      (else (setq Rf (numberEvolution X Y Rf Gmax err)))
      ) ; end cond
    Rf) ; end numericRegress












;;**EXPORTKEY**:math:numericRegress:binaryEvolution
(defriend math.numericRegress binaryEvolution(X Y Rf Gmax err)
;; ******************************************************************************************
;; summary:  Genetic Evolutionary Regression: optimize the estimator 
;; 			 coefficients for the specified estimator Lambda using
;; 			 the training data supplied, and a binary genome.
;;
;;           Returns the input estimator function with coefficients optimized 
;;           against the specified objective function. The regression may
;;           be linear of nonlinear and will minimize any objective function.
;;           Neither the estimator function nor the objective function 
;;           are required to be continuous.
;;
;; 			 Notes:
;;				o Perferred Data Format:  X ==> N x M+1 vector array, Y ==> #void.
;; 			 	o This Lambda assumes that the coefficient vector is an number vector. 
;;
;; Parms:    X:       The N by M+1 vector array representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;                    Note: Where each of the row vectors must be Number vectors.
;;           Y:       (Optional)The N number vector containing the dependent variables
;;                    in the form of:    	  y
;;                                            y
;;                                           ... 
;;                                            y
;;                    Note: If Y is NOT #void, the X will not contain the y terms.
;;           Rf:      The estimator function which maps a number vector, of length M,
;;                    into a single real number, Rf: Vm --> R, whose coefficients
;;                    are to be optimized. The estimator function is in the form of
;;                    an Lambda, (Rf mVector) ==> aNumber, and the coefficients
;;                    to be optimized are stored as a number vector Rf.C 
;;                    in the persistant variables of the Rf Lambda.
;;                    Note: The estimator function, Rf, must be an Lambda containing
;;                          the following persistant (pvars) variables:
;;                          C             A bit vector containing the binary coefficients to optimize
;;                          Error         The final score from the objective function 
;;                          G             The final generation count when optimization halted
;;                          Objective     The objective function which maps an N by M+1 observation 
;;                                        array, X, using it's parent estimator function, Rf, into 
;;                                        a single positive real error value. The objective function 
;;                                        is in the form of a child Lambda, (Rf.Objective X) ==> anErrorNumber.
;;                                        The numericRegress Lambda attempts to optimize the estimator
;;                                        function coefficients, Rf.C, such that the objective 
;;                                        function's resulting error value is minimized. 
;;                          P             The final population count when optimization halted 
;;                          Strategy      The optimization strategy to use: 
;;											evolveBinary: 	Use genetic algorithm (binary genome)  
;;           Gmax:    The maximum number of optimization trials (generations) to attempt before
;;                    returning the best set of coefficients available at that time.
;;           err:     A minimum error value which would terminate further optimization 
;;                    trials and return the best set of coefficients at that time.
;;
;; Return:   Rf:      The estimator function with coefficients Rf.C optimized.
;; 
;; Note:     See Genetic Algorithms by John Holland.
;; ******************************************************************************************
    pvars:(;; Private child methods
           initCoefficients        ;; Initialize cvPopulation with a random coefficient vector population.
           migrateCoefficients     ;; Migrate the fittest coefficient vectors between distinct populations.
           mutateCoefficients      ;; Mutate the fittest coefficient vectors in the general population.
           recombineCoefficients   ;; Recombine the fittest coefficient vectors in the general population.
           scorePopulation         ;; Score the entire cvPopulation coefficient vector population.
           ;; Private Variables
           (learningRate .25)      ;; The learning rate during each optimization trail.
           (migrationRate .05)     ;; The migration rate between population before starting the next optimization trail.
           (maxPopulationAge 100)  ;; The maximum age of any distinct population for any optimization trail.
           (maxPopulations 3)      ;; The maximum number of distinct populations for any optimization trail.
           (maxSurvivors 20)       ;; The maximum survivors from a population before starting the next optimization trail.
           (minPopulation 50)      ;; The minimum population, of cvPopulation, before attempting an optimization trail.
           ) ;; end of persistent variables
    vars:(i naCount)
    ;; *******************************************************************
    ;; Define Private Child Lambdas
    ;; *******************************************************************
    ;; Initialize cvPopulation with a random coefficient vector population.
    (defun initCoefficients(X Rf)
       vars:(i cv currentPop iC)
       ;; Initialize all system parameters.
       (if (= randomSW true) (setq _random ^random) (setq _random ^srandom))
       ;; The population count increases as the generation count increases.
       (setq populationCount (integer (min (++ populationCount) maxPopulations)))
       ;; Initialize coefficients for each distinct population.
       (setq iC Rf.C)
       (setq cvPopulation (new Vector:))
       (setq cvError (new Vector:))
       (setq cvStop (new Vector:))
       (setq populationAge 0)
       (loop for currentPop from 0 until populationCount do
          (setq cvPopulation[currentPop] (new Structure:))
          (setq cvError[currentPop] "NA")
          (setq cvStop[currentPop] false)
          ;; Create a set of random coefficient vectors.
          (while (< (length cvPopulation[currentPop]) minPopulation) do
             (setq cv (new Vector: bit: Cn))
             (loop for i from 0 until Cn do
                (setq cv[i] (if (>= (_random 1.0) .5) 1 0))
                ) ; end loop
             (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) cv "NA")
             ) ; end while loop
          ) ; end currentPop loop
       (if (isVector iC) (insert cvPopulation[0] (length cvPopulation[0]) iC "NA"))
       true) ; end initCoefficients
    ;; Score the entire cvPopulation coefficient vector population.
    (defun migrateCoefficients()
       vars:(i j currentPop CBaby CScore)
       ;; Migrate coefficients between each distinct population.
       (if (<= migrationRate 0) (return true))
       (loop for currentPop from 0 until populationCount do
          (if (= cvStop[currentPop] true) (goto SkipThisPopulation:))
          (loop for j from 0 until populationCount do
             (if (and (<> j currentPop) (< (_random 1.0) migrationRate))
                 (begin
		            ;; Migrate a coefficient vectors between populations
		            (setq CBaby (copy cvPopulation[j][0 0]))
		            (setq CScore cvPopulation[j][0 1])
                    (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) CBaby CScore)
                    )) ; end if
             ) ; end j loop
          SkipThisPopulation::
          ) ; end currentPop loop
       true) ; end scorePopulation
    ;; Mutate the coefficients of each of the survivors.
    (defun mutateCoefficients()
       vars:(i j jm start end temp C0 C1 C2 C3 C4 C5 C6 C7 C8 currentPop delta err r (MRate 10))
       ;; Mutate coefficients for each distinct population.
       (loop for currentPop from 0 until populationCount do
          (if (= cvStop[currentPop] true) (goto SkipThisPopulation:))
          ;; Mutate best survivor coefficient vector one chromosome at a time.
          (loop for j from 0 until Cn do
	         ;; Mutate the best coefficient vector one chromosome at a time.
	         (setq C0 (copy cvSurvivors[currentPop][0 0]))
	         (setq C0[j] (if (= C0[j] 1) 0 1))
             (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) C0 "NA")
             ) ; end mutation loop
          ;; Mutate a survivor coefficient vector one chromosome segment at a time.
          (loop for r from 0 until MRate do
	         ;; Mutate each coefficient vector by genome segment.
	         (setq i (integer (_random (* .99999 (length cvSurvivors[currentPop])))))
	         (setq C0 (copy cvSurvivors[currentPop][0 0]))
	         (setq C1 (copy cvSurvivors[currentPop][i 0]))
	         (setq start (integer (max (_random Cn) Cn1)))
	         (setq end (integer (max (_random Cn) Cn1)))
	         (loop for jm from start to end do
	            (setq j (modi jm Cn))
	            (setq C0[j] (if (= C0[j] 1) 0 1))
	            (setq C1[j] (if (= C1[j] 1) 0 1))
	            ) ; end jm loop
             (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) C0 "NA")
             (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) C1 "NA")
             ) ; end mutation loop
          ;; Mutate a survivor coefficient vector all coefficients at once
          ;; Note: This heurism mutates the best genome, all coefficients, in both directions.
          (loop for r from 0 until MRate do
	         ;; Mutate each coefficient vector one chromosome at a time.
	         (setq i (integer (_random (* .99999 (length cvSurvivors[currentPop])))))
	         (setq C0 (copy cvSurvivors[currentPop][0 0]))
	         (setq C1 (copy cvSurvivors[currentPop][i 0]))
	         (setq j (integer (max (_random Cn) Cn1)))
	         (setq C0[j] (if (= C0[j] 1) 0 1))
	         (setq C1[j] (if (= C1[j] 1) 0 1))
             (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) C0 "NA")
             (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) C1 "NA")
             ) ; end mutation loop
          SkipThisPopulation::
          ) ; end currentPop loop
       true) ; end mutateCoefficients
    ;; Recombine the coefficients of each of the survivors.
    (defun recombineCoefficients()
       vars:(mon pop i j jm start end temp CMom CPop currentPop delta r (CRate 3))
       ;; Recombine coefficients from pairs of coefficient vectors in each distinct population.
       (loop for currentPop from 0 until populationCount do
          (if (= cvStop[currentPop] true) (goto SkipThisPopulation:))
	      ;; Recombine (crossover) two random coefficient vectors in the survivor population
          (loop for r from 0 until CRate do
	          (setq mom (setq pop 0))          
	          (while (= mom pop) do  
	             (setq mom (integer (_random (* .99999 (length cvSurvivors[currentPop])))))
	             (setq pop (integer (_random (* .99999 (length cvSurvivors[currentPop])))))
	             ) ; end while
	          (setq CMom (copy cvSurvivors[currentPop][mom 0]))
	          (setq CPop (copy cvSurvivors[currentPop][pop 0]))
	          (setq start (integer (max (_random Cn) Cn1)))
	          (setq end (integer (max (_random Cn) Cn1)))
	          (loop for jm from start to end do
	             (setq j (modi jm Cn))
	             (setq CMom[j] cvSurvivors[currentPop][pop 0][j])
	             (setq CPop[j] cvSurvivors[currentPop][mom 0][j])
	             ) ; end jm loop
              (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) CMom "NA")
              (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) CPop "NA")
              ); end crossover loop
          SkipThisPopulation::
          ) ; end currentPop loop
       true) ; end recombineCoefficients
    ;; Score the entire cvPopulation coefficient vector population.
    (defun scorePopulation(Rf X)
       vars:(i currentPop score)
       ;; Score the coefficients for each distinct population.
       (setq forceNewPopulation true)
       (setq cvSurvivors (new Vector:))
       (loop for currentPop from 0 until populationCount do
          (if (= cvStop[currentPop] true) (goto SkipThisPopulation:))
          ;; Score all coefficient vectors in the current population
          (loop for i from 0 until (length cvPopulation[currentPop]) do
             (if (= cvPopulation[currentPop][i 1] "NA")
                 (begin
                    (setq Rf.C cvPopulation[currentPop][i 0])
                    (setq score (Rf.Objective X))
                    ;; Assign score to population only if it is not a floating point error of some kind.
                    (if (= (+ score 1) score)
                        ;; Remove the coefficient vector if the score is a floating point error of some kind.
                        (setq cvPopulation[currentPop][i 1] "NA")
                        ;; Set the coefficient vector's score in the population otherwise.
                        (setq cvPopulation[currentPop][i 1] score)
                        ) ; end if
                 )) ; end if
             ) ; end loop
          ;; Reduce the current population to only the most fit survivors
          (sort cvPopulation[currentPop] < byValue:)
          (while (< cvPopulation[currentPop][0 1] 0) do (delete cvPopulation[currentPop] 0)) 
          (resize cvPopulation[currentPop] maxSurvivors)
          (setq cvSurvivors[currentPop] (copy cvPopulation[currentPop]))
          (if (> cvError[currentPop] cvSurvivors[currentPop][0 1])
              then
              (begin
                 (setq cvError[currentPop] cvSurvivors[currentPop][0 1]) 
                 (setq forceNewPopulation false)
                 (if (> Error cvSurvivors[currentPop][0 1])
                     (begin
                        (setq Error cvSurvivors[currentPop][0 1]) 
                        (setq C cvSurvivors[currentPop][0 0])
                        ;(writeln "Selecting Best of Breed G=[" generationCount "] P=[" currentPop "] E=[" Error "]")
                     )) ; end if
                 ) ; end then
              else
              (setq cvStop[currentPop] true)
              ) ; end if
          SkipThisPopulation::
          ) ; end currentPop loop
       true) ; end scorePopulation
    ;; *******************************************************************
    ;; Begin Genetic Algorithm Logic Section
    ;; *******************************************************************
    (setq X (math.convertToArray X Y))
    (setq Rf (copy Rf))
    (setq C #void)
    (setq Error "NA")
    (setq Cn (length Rf.C))
    (setq Cn1 (subi Cn 1))
    (setq N (length X))
    (setq Mp1 (length X[0]))
    (setq M (subi Mp1 1))
    (setq Mm1 (subi M1 1))
    (setq naCount 0)
    (setq generationCount 0)
    (setq populationCount populationStart)
    NewPopulation::
    (initCoefficients X Rf)
    (scorePopulation Rf X)
    (setq Rf.C C)
    (setq Rf.Error Error)
    NewGeneration:: 
    (if (= Error "NA")
        (begin
           (++ naCount) 
           (if (<= naCount 10) 
               (goto NewPopulation:)
               (goto StopTraining:)
               ) ; end inner if
        )) ; end "NA" if
    (setq naCount 0)
    ;(if (= forceNewPopulation true) (goto NewPopulation:))
    (setq generationCount (addi generationCount 1))
    (setq populationAge (addi populationAge 1))
    (mutateCoefficients)
    (recombineCoefficients)
    (scorePopulation Rf X)
    (migrateCoefficients)
    (setq Rf.C C)
    (setq Rf.Error Error)
    (if (or (<= Error err) (>= generationCount Gmax)) (goto StopTraining:))
    (if (>= populationAge (+ maxPopulationAge (divi generationCount maxPopulationAge))) (goto NewPopulation:))
    (goto NewGeneration:)
    ;; Return the optimized estimator function Lambda.
    StopTraining::
    (setq Rf.C C)
    (setq Rf.Error Error)
    (setq Rf.G generationCount)
    (setq Rf.P populationCount)
    Rf) ; end binaryEvolution


































;;**EXPORTKEY**:math:numericRegress:binaryMemoEvolution
(defriend math.numericRegress binaryMemoEvolution(X Y Rf MaxGen MinErr)
;;	***************************************************************************************
;; summary:  Genetic Evolutionary Regression: optimize the estimator 
;; 			 coefficients for the specified estimator Lambda using
;; 			 the training data supplied, and a binary genome with memoizing.
;;
;;           Returns the input estimator function with coefficients optimized 
;;           against the specified objective function. The regression may
;;           be linear of nonlinear and will minimize any objective function.
;;           Neither the estimator function nor the objective function 
;;           are required to be continuous.
;;
;; 			 Notes:
;;				o Perferred Data Format:  X ==> N x M+1 vector array, Y ==> #void.
;; 			 	o This Lambda assumes that the coefficient vector is an number vector. 
;;
;; Parms:    X:       The N by M+1 vector array representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;                    Note: Where each of the row vectors must be Number vectors.
;;           Y:       (Optional)The N number vector containing the dependent variables
;;                    in the form of:    	  y
;;                                            y
;;                                           ... 
;;                                            y
;;                    Note: If Y is NOT #void, the X will not contain the y terms.
;;           Rf:      The estimator function which maps a number vector, of length M,
;;                    into a single real number, Rf: Vm --> R, whose coefficients
;;                    are to be optimized. The estimator function is in the form of
;;                    an Lambda, (Rf mVector) ==> aNumber, and the coefficients
;;                    to be optimized are stored as a number vector Rf.C 
;;                    in the persistant variables of the Rf Lambda.
;;                    Note: The estimator function, Rf, must be an Lambda containing
;;                          the following persistant (pvars) variables:
;;                          C             A bit vector containing the binary coefficients to optimize
;;                          Error         The final score from the objective function 
;;                          G             The final generation count when optimization halted
;;                          Objective     The objective function which maps an N by M+1 observation 
;;                                        array, X, using it's parent estimator function, Rf, into 
;;                                        a single positive real error value. The objective function 
;;                                        is in the form of a child Lambda, (Rf.Objective X) ==> anErrorNumber.
;;                                        The numericRegress Lambda attempts to optimize the estimator
;;                                        function coefficients, Rf.C, such that the objective 
;;                                        function's resulting error value is minimized. 
;;                          P             The final population count when optimization halted 
;;                          Strategy      The optimization strategy to use: 
;;											evolveBinary: 	Use genetic algorithm (binary genome)  
;;           Gmax:    The maximum number of optimization trials (generations) to attempt before
;;                    returning the best set of coefficients available at that time.
;;           err:     A minimum error value which would terminate further optimization 
;;                    trials and return the best set of coefficients at that time.
;;
;; Return:   Rf:      The estimator function with coefficients Rf.C optimized.
;; 
;; Note:     See Genetic Algorithms by John Holland.
;;	***************************************************************************************
pvars:(; Public variables
	; Private variables
	mPopulation			; Vector with members and fitness (error) for each member
	mSurvivors			; New population generated by recombination and mutation 
	; Tracking statistics
	mAlleleFreq			; Frequency of a specific allele
	mClone				; Percentage of parents cloned
	mCross				; Percentage of parents recombined
	mMaxErr				; Maximum error per generation
	mMinErr				; Minimum error per generation
	mMutation			; Percentage mutated
	mTotal				; Total number of parents over all generations
		
	; Public child Methods
	; Private child methods
	mRandom				; The current random function.
	InitMembers			; Initialize Population with a random genomes (C).
	MutateMembers		; Mutate the fittest members in the general population.
	RecombineMembers	; Recombine the fittest members in the general population.
	SelectMembers		; Determine the fitness of all of the members of the population.
	SpinWheel			; Select member at random with probability proportional to fitness.
	PrintBinaryVector	; Print out a vector in one row.
)
vars:( ; Private variables
	Gen					; Current generation index
	m					; Index into a member of mPopulation, mSurvivors
	(PopSize 26)		; The number of members in a population (make it an even num) (~26)
	(PSex 0.7)			; Probability of recombining as opposed to cloning (~0.7)
	(PMutate 0.01)		; Probability of mutation in genome, C (~0.001)
	Rf					; Copy of the estimator function
	Temp				; Holds a structure reference
	 ; Private child methods
)

;;	***************************************************************************************
;;	InitMembers - Initialize the current population with randomly selected genomes
;;	mPopulation is an array of structures.  The key of each element is the genome, C.
;;	The value of each element is the Error calculated in SelectMembers.  Here the value
;;	is initialized to "NA".
;;	InitMembers puts the initial value of Rf.C in the first slot of mPopulation.  It then
;;	puts randomly selected bit-vectors into the remaining PopSize-1 slots.  The 
;;	Modifies:	Fills the mPopulation array
;;	aC			An initial guess for the value of C passed from Rf.C. Must not be empty.
;;	aPopLen		Number of elements in mPopulation  
;;	Returns:	Nothing
;;	***************************************************************************************
(defun InitMembers(aC aPopLen)
	vars:(C CLen i m)
	; Initialize parameters.
	(setq CLen (length aC))
	; Put the initial member from Rf into the population
	(setq C mPopulation[0 0])
	(loop for i from 0 until CLen do
		(setq C[i] aC[i])
	)		
	; Create a set of PopLen members at random
	(loop for m from 1 until aPopLen do
		(setq C mPopulation[m 0])
		(loop for i from 0 until CLen do
			(if (>= (mRandom 1.0) 0.5)
				(setq C[i] 1)
				(setq C[i] 0)
			)
		)		
	)
) ; end InitMembers

;;	***************************************************************************************
;;	MutateMembers - Make a few random changes to the new population
;;	MutateMembers changes one random allele (a bit) in the genome C with probability
;;  PMutate (~.001) in each member of the survivor population.
;;	Modifies:	Changes a few bits in the keys (bit-vectors) of the mSurvivor members
;;	aPMutate	Probability that one allele in one member is modified.
;;	Returns:	Nothing
;	***************************************************************************************
(defun MutateMembers(aPMutate)
	vars:(m b C CLen PopLen)
	
	(setq PopLen (length mSurvivors))
	(setq CLen (length mSurvivors[0 0]))
	(loop for m from 0 until PopLen do
		(if (<= (mRandom 1.0) aPMutate) (begin 		
			(setq C mSurvivors[m 0])
			; Select one bit for change
			(setq b (integer (mRandom (* 0.99999 CLen))))
			(setq C[b] (bitwiseXor 1 C[b]))
			(writeln "mSurvivors[" m " 0][" b "]=" mSurvivors[m 0][b])          ;;; TEST
			(++ mMutation)
		))
	)
) ; end MutateMembers
 
;;	***************************************************************************************
;;	RecombineMembers - Recombine or clone genomes in the Survivor population
;;	This is a sexually explicit routine.  Sensitive viewers may wish to skip this routine.
;;	RecombineMembers clones or recombines the most fit pairs of members from mPopulation
;;	These children are used to fill the mSurvivor structure array. (v.f., InitMembers)
;;	The most fit members are selected for reproduction using "roulete-wheel sampling"
;;	Modifies:	Fills the mSurvivor structure array with PopLen new members 
;;	aPSex		Probability of recombining versus cloning (~.7)  
;;	Returns:	Nothing
;;	***************************************************************************************
(defun RecombineMembers(aPSex)
	vars:(b C CLen CMom CPop Err i Locus m Mom Pop Prob PopLen Tot)
	
	; Initialize
	(setq PopLen (length mPopulation))
	(setq CLen (length mPopulation[1 0]))
	
	; Track the frequency of each allele
	(loop for m from 0 until PopLen do
		(loop for i from 0 until CLen do
			(if (> mPopulation[m 0][i] 0) (setq mAlleleFreq[i] (add1 mAlleleFreq[i])))
		)
	)
	; Compute probability proportional to inverse error for each member, m, of the population
	(setq Prob (new Vector: number: PopLen))
	(setq Tot 0.0)
	(loop for m from 0 until PopLen do
		(if (<= (setq Err mPopulation[m 1]) 0) (goto Quit:))
		(setq Prob[m] (/ 1.0 Err))
		(setq Tot (+ Tot Prob[m]))	
	)
	(loop for m from 0 until PopLen do (setq Prob[m] (/ Prob[m] Tot)))

	; Recombine or clone each pair of parents, Mom, Pop
	; Add the 2 children to the mSurvivors population.
	(resize mSurvivors 0)
	(loop for m from 0 until PopLen do
		(setq mTotal (addi mTotal 2))	
		; Select two different parents based upon fitness from mPopulation
		(setq Pop (SpinWheel Prob))
		(loop for i from 0 until PopLen do
			(setq Mom (SpinWheel Prob))
			(if (<> Mom Pop) (goto Break:))
		)
		Break::

		; If clone, just copy parents; else, crossover the tails of the
		; parents' genomes.
		(if (or (> (mRandom 1.0) aPSex) (= Mom Pop)) (begin ; Clone
			; (writeln "Clone m=" m ",Pop=" Pop ",Mom=" Mom)                      ;;; TEST
			; Copy both parents into the survivor population.
			(insert mSurvivors  m mPopulation[Pop 0] mPopulation[Pop 1])
			(setq m (add1 m))
			(insert mSurvivors  m mPopulation[Mom 0] mPopulation[Mom 1])
			(setq mClone (addi mClone 2))
		) (begin  ; else, Recombine
			; (writeln "Recombine m=" m ",Pop=" Pop ",Mom=" Mom)                  ;;; TEST
			(insert mSurvivors  m mPopulation[Pop 0] "NA")		
			(setq CPop mSurvivors[m 0])
			(setq m (add1 m))
			(insert mSurvivors  m mPopulation[Mom 0] "NA")
			(setq CMom mSurvivors[m 0])
			(setq mCross (addi mCross 2))

			; Select the locus of division at random and cross tails
		    (setq Locus (integer (mRandom (* CLen .9999))))
		    (loop for i from Locus until CLen do
		    	(setq b CPop[i])
		    	(setq CPop[i] CMom[i])
				(setq CMom[i] b)		    
			)
		))
	)
Quit::
)	 ; End RecombineMembers

;;	***************************************************************************************
;;	SelectMembers - Submit current population to unnatural selection
;;	SelectMembers determines the fitness of each member of the population.  This info is
;;	used later by RecombineMembers to determine who is most likely to mate.
;;	Modifies:	Fills the values in the mPopulation structure array with error estimate
;;  Warning:	This routine may take a long time if the Objective function is slow.
;;	Note:		Fitness is defined as 1/Err.  Err cannot be zero because GA quits if 0.
;;	aRf			Estimator function provided to evolveParameter.  Rf contains objective fcn
;;	aW			Parameter passed to Objective function. Not used by this GA.
;;  aGen		Generation number (starting from 0)
;;	Returns:	Nothing
;;	***************************************************************************************
(defun SelectMembers(aRf aW aGen)
	vars:(Err m PopLen MaxEr MinEr)
	; Initialize
	(setq PopLen (length mPopulation))
	(setq MaxEr 0)
	(setq MinEr 1.0e128)
		
	; Determine the fitness of each member in the current population.
	(loop for m from 0 until PopLen do
		(if (= mPopulation[m 1] "NA") (begin
			(setq aRf.C mPopulation[m 0])
			;; (setq Err (aRf.Objective aW))				; Err is the inverse of fitness 
			(setq Err (add1 m)) (setq aRf.Error (add1 m))                       ;;; TEST
			(if (= (+ Err 1) Err) (setq Err 1.0e128))	; Set large value if not a number
			(setq mPopulation[m 1] Err)
			(if (< Err MinEr) (setq MinEr Err))
			(if (> Err MaxEr) (setq MaxEr Err))
		))
	)
	; Sort the current population by increasing error (decreasing fitness)
	(sort mPopulation < byValue:)
	(display " C[0]=") (PrintBinaryVector mPopulation[0 0])                      ;;; TEST
	(writeln "  Err=" (text mPopulation[0 1] "##.##") ", Gen=" aGen)
	(setq mMaxErr[aGen] MaxEr)
	(setq mMinErr[aGen] MinEr)
) ; end SelectMembers

;;	***************************************************************************************
;;	SpinWheel - Select a member based upon "roulette-wheel sampling"
;;	P is set to a random number between 0 and 1.  The probability of each member of
;;	mPopulation is added to the running total until the total exceeds P.  The last
;;	member is selected.  A member with a large probability is more likely to be selected
;;	than one with a small probability.
;;	Note:		The index into aProb is the same as the index into mPopulation
;;	aProb		A probability proportional to fitness for each member of mPopulation
;;	Returns:	The index of the selected member
;;	***************************************************************************************
(defun SpinWheel(aProb)
	vars: (m P PopLen Tot)

	; Prob is an array of PopLen numbers. The sum of the elements of Prob is one.
	; Each prob in the array is proportional to the fitness of the member, m
	(setq Tot 0.0)
	(setq PopLen (length aProb))
	(setq P (mRandom 1.0))					; P is a probability
	(loop for m from 0 until PopLen do
		(+= Tot aProb[m]) 
		(if (<= P Tot) (goto Done:))
	)
Done::
	(return m)
)

(defun PrintBinaryVector(v)
	vars: (m VLen)
	(setq VLen (length v))
	(loop for m from 0 until VLen do
		(display " " (text v[m] "#"))
	)
)

;	***************************************************************************************
;	Main Logic Section
;	***************************************************************************************
; Check inputs
(setq X (math.convertToArray X Y))
; MinErr must be non-negative
(if (< MinErr 0) (begin (writeln "EvolveParameter: MinErr must be non-negative") (return #void)))

; Initialize variables
(setq mRandom ^random)
(setq CLen (length Rf.C))
(setq mPopulation (new Structure: ))				; No size allowed for structures
(setq mSurvivors  (new Structure: ))
(loop for m from 0 until PopSize do
 	(insert mPopulation m (new Vector: bit: CLen) "NA") 
)
(setq mAlleleFreq (new Vector: number: CLen))	; Frequency of a specific allele
(setq mClone 0)									; Overall percentage of parents cloned
(setq mCross 0)									; Overall percentage of parents recombined
(setq mMaxErr (new Vector: number: PopSize))	; Maximum error per generation
(setq mMinErr (new Vector: number: PopSize))	; Minimum error per generation
(setq mMutation 0)								; Overall percentage mutated
(setq mTotal 0)									; Total number of parents over all generations

(InitMembers Rf.C PopSize)
(loop for Gen from 0 to MaxGen do
	(SelectMembers Rf X Gen)					; Determine fitness of members
	(if (or (<= mPopulation[0 1] MinErr) (>= Gen MaxGen)) (goto QuitEvolving:))
	(RecombineMembers PSex)						; Create new survivor population
	(MutateMembers PMutate)						; Randomly mutate a few survivors
	
	; Replace the current population with the new population
	(setq Temp mPopulation)
	(setq mPopulation mSurvivors)
	(setq mSurvivors Temp)
)
QuitEvolving::

; Compute statistics
(display "AlleleFreq=")
(loop for m from 0 until CLen do
	(display " " (text (/ mAlleleFreq[m] mTotal) "##.##"))
)
(writeln)

(display "Clone=" (text (/ mClone mTotal) "##.##"))
(display ", Cross=" (text (/ mCross mTotal) "##.##"))
(writeln ", Mutation=" (text (/ mMutation mTotal) "#.###"))

(display "MaxErr=")
(loop for m from 0 until MaxGen do
	(display " " (text mMaxErr[m] "##.##"))
)
(writeln)
(display "MinErr=")
(loop for m from 0 until MaxGen do
	(display " " (text mMinErr[m] "##.##"))
)
(writeln)
; Return the optimized estimator function Lambda.
(setq Rf.C mPopulation[0 0])
(setq Rf.Error mPopulation[0 1])
(setq Rf.G Gen)
(setq Rf.P PopSize)
(return Rf)
) ; end binaryMemoEvolution



































;;**EXPORTKEY**:math:numericRegress:makeBExponential
(defchild math.numericRegress makeBExponential(variableCount)
;; *******************************************************************
;; summary:  Genetic Evolutionary Regression: (binary genome)
;; 
;;           Return an exponential regression estimator Lambda for
;;           use with numericRegress. The estimator Lambda implements 
;;           an exponential polynomial regression model, which has
;;           been modified to prevent overflow.
;;
;;			 Notes:
;;				o A standard least squares objective function is used.
;;				o Estimator training data must be a single N x M+1 vector array.
;;				o Estimator input must be a single number vector.
;;			    
;;			 Model:
;; 				y = C[0] + C[1]*expt(X[0],C[2]) + ... +  C[2M]*expt(X[M-1],C[2M+1])
;; 
;; Parms:    variableCount:  The number of independent variables in the regression.
;;
;; Return:   Estimator       Always returns an estimator Lambda.
;; *******************************************************************
   vars:(X Rf in i)
   ;; Create an exponential estimator Lambda.
   ;; Note1: The estimator Lambda is an exponential 
   ;;        polynomial regression model, which has
   ;;        been modified to prevent overflow.
   ;; Note2: A standard least squares objective
   ;;        function is used.
   (setq Rf (eval {
     (lambda(v)
        pvars:(C                    ; Coefficient vector (bit)
               RC                   ; Coefficient vector (number)
               Error           		; Final objective error after training
               AvgErr           	; Average objective error after training
               MinErr           	; Minimum objective error after training
               MaxErr           	; Maximum objective error after training
               AvgY              	; Average of dependent variable during training
               Fe                   ; Final function executions after training
               G               		; Final generation count after training
               Objective       		; Objective function to use in training
               P               		; Final population count after training
               Rf              		; My estimator Lambda
               (Strategy evolveBinary) ; My optimization strategy
               variableCount        ; Count of independent variables
               ) ; end of persistant variables
        vars:(i cc ce result M ey y)
        ;; Define a standard least square objective function
        (defun Objective(X)
           vars:(i ey y M N stdErr avgErr minErr maxErr avgY)
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (if (= RC #void) (setq RC (new Vector: number: (divi variableCount 48))))
           (setq RC (bitToNumberVector C RC))
           (++ Fe)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq Rf.Error stdErr)
           (setq Rf.AvgErr avgErr)
           (setq Rf.MinErr minErr)
           (setq Rf.MaxErr maxErr)
           (setq Rf.AvgY avgY)
           Rf.Error) ; end define objective function
        ;; Define a standard estimator Lambda display function
        (defun show()
           vars:(result ca n N)
           ;; Append the training statistics to the model display
           (setq result (append "model bExponential: G=" Rf.G ",P=" Rf.P ",StdErr=" Rf.Error ",AvgErr=" Rf.AvgErr ",MinErr=" Rf.MinErr ",MaxErr=" Rf.MaxErr ",AvgY=" Rf.AvgY))
           result) ; end define a standard estimator Lambda display function
        ;; Test the estimator Lambda
        (defun test(X)
           vars:(nn NN m M i I j J k K CC M N ey y stdErr avgErr minErr maxErr avgY)
           ;; Compute the error statistics.
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq result (append "model bExponential: StdErr=" stdErr ",AvgErr=" avgErr ",MinErr=" minErr ",MaxErr=" maxErr ",AvgY=" avgY))
           result) ; end test function
        ;; Begin estimator Lambda main logic 
        (if (= RC #void) 
            (begin 
               (setq RC (new Vector: number: (divi variableCount 48)))
               (setq RC (bitToNumberVector C RC))
               )) ; end if 
        (setq M (divi (length RC) 2))
        (setq cc 1)
        (setq ce 2)
        (setq result RC[0])
        (loop for i from 0 until M do 
           (setq result (+ result (expt (* RC[cc] v[i]) RC[ce])))
           (setq cc (addi cc 2))
           (setq ce (addi ce 2))
           ) ; end loop
        ;; Forceably prevent overflow
        (setq result (numCheck result))
        result) }
        )) ; end define estimator Lambda
   ;; Set the coefficient vector to the number of variables times 2 plus the constant coefficient.
   (setq Rf.variableCount variableCount)
   (setq Rf.C (new Vector: bit: (integer (* (+ (* variableCount 2) 1) 48))))
   (setq Rf.Rf Rf)
   Rf) ; end makeBExponential




















;;**EXPORTKEY**:math:numericRegress:makeCluster
(defchild math.numericRegress makeCluster(variableCount filter)
;; *****************************************************************************************
;; summary:  Induced Regression:
;;           
;;			Return a nearest neighbor cluster induction estimator Lambda
;;           for use with numericRegress.  This estimator Lambda memorizes each unique 
;;		     independent vector, X, observed during the training period along with the 
;;           average dependent variable, y, associated with the memorized observation, X. 
;;           Additionally, all the observations are segmented into nearest neighbor 
;;           euclidean clusters along each independent axis. For each cluster along 
;;           each independent axis, the optimal multivariable regression coefficients 
;;           are memorized.
;;
;;           During estimation, if the input X has been observed during training, then
;;           the best estimate for y is the average y value seen for X during training.
;;           If the input X has NOT been observed during training, then the best estimate
;;           for y is the average y value, returned from the memorized optimal multivariable
;;           regression coefficients, for each of the euclidean nearest neighbor clusters 
;;           of X as on each independent axis as segmented during training. 
;;
;;			 Notes:
;;				o A standard least squares objective function is used.
;;				o Estimator training data must be a single N x M+1 vector array.
;;				o Estimator input must be a single number vector.
;;			    
;; Parms:    variableCount:  The number of independent variables in the regression.
;;           filter:         The type of final output filter. Must be one of
;;                           (binary, bipolar, continuous, or sigmoid).
;;
;; Return:   Estimator       Always returns a Nearest Neighbor Euclidean cluster regression 
;; ******************************************************************************************
   vars:(n N)
   ;; Create a nearest neighbor cluster induction estimator Lambda.
   ;; Note1: The estimator Lambda uses the Euclidean 
   ;;        distance between observations to induce a
   ;;        regression model.
   ;; Note2: A standard least squares error score is computed.
   (setq Rf (eval {
     (lambda(v)
        pvars:(;; Public variables
               C                    ; Coefficient vector
               check           		; Check the result for over or under flow
               Clusters	            ; My training cluster array
               Error           		; Final objective error after training
               AvgErr           	; Average objective error after training
               MinErr           	; Minimum objective error after training
               MaxErr           	; Maximum objective error after training
               AvgY              	; Average of dependent variable during training
               Filter        	    ; My output filter (binary, bipolar, continuous, or sigmoid)
               (G 0)             	; Final generation count after training
               maxErr             	; Maximum error for use in discouraging training
               (P 0)             	; Final population count after training
               Rf              		; My estimator Lambda
               (Strategy induce:)	; My optimization strategy
               variableCount		; Estimator Lambda input variables
               ;; Public child Lambdas
               estimateFromCluster 	; Estimate the dependent variable using the specified cluster
               distance        		; Euclidean distance function
               leastSquaresError  	; Return the least squares error for a cluster
               memorizeCluster  	; Create a cluster directory on the specified target columns
               Objective       		; Objective function to use in training
               ) ; end of persistant variables
        vars:(n NN ey ev errTotal)
        ;; **************************************
        ;; Define estimator Lambda child Lambdas
        ;; **************************************
        ;; Euclidean distance function
        ;; Note: If the target contains a vector
        ;;       of indices, then we are computing
        ;;       the partial distance from a cluster
        ;;       mean vector VMean to an input vector, v.
        (defun distance(VMean v target)
           vars:(dd dy m M t T)
           (if (= (length target) 0)
               then
               ;; Compute complete euclidean distance
               (begin
                  (setq M variableCount) 
                  (setq dd 0)
                  (loop for m from 0 until M do
                     (setq dy (- VMean[m] v[m])) 
                     (setq dd (+ dd (* dy dy)))
                     ) ; end loop
               ) ; end then
               else
               ;; Compute partial euclidean cluster distance
               (begin 
                  (setq T (length target)) 
                  (setq dd 0)
                  (loop for t from 0 until T do
                    (setq m target[t])
                    (setq dy (- VMean[t] v[m])) 
                    (setq dd (+ dd (* dy dy)))
                    ) ; end loop
               )) ; end else                     
           (sqrt dd)) ; end distance function  
        ;; Estimate the dependent variable using the specified segment
        (defun estimateFromCluster(cluster v)
           vars:(nn NN ey dy minDY hits)
	       ;; Order the observed dependent values by their simple distance metric.
	       (setq minDY BIGPOSNUM)
	       (setq ey (setq hits 0))
	       (setq NN (length cluster.Net))
	       (loop for nn from 0 until NN do
	          (setq dy (distance cluster.Net[nn 0] v cluster.Cols))
	          (cond
	             ;; We have a cluster exactly as close to the input vector. 
	             ((= dy minDY) 
	              (begin 
	                 (setq minDY dy)
	                 (setq ey (+ ey cluster.Net[nn 1]))
	                 (++ hits)
	              ))
	             ;; We have a cluster closer to the input vector. 
	             ((< dy minDY) 
	              (begin 
	                 (setq minDY dy)
	                 (setq ey cluster.Net[nn 1])
	                 (setq hits 1)
	              ))
                 ) ; end cond
	          ) ; end loop
           (if (> hits 0) (setq ey (/ ey hits)))
	       ;; Convert the dependent value using the requested output filter.
	       (if (= Filter sigmoid:) (setq ey (/ (- (min (max ey cluster.Min) cluster.Max) cluster.Min) (- cluster.Max cluster.Min))))
	       (if (= Filter bipolar:) (setq ey (if (>= ey cluster.Avg) 1 -1)))
	       (if (= Filter binary:) (setq ey (if (>= ey cluster.Avg) 1 0)))
           ey)  ; end estimateFromCluster function
        ;; Compute the least squares error for the specified cluster.
        ;; Note: This is the least squares error if the specified cluster
        ;;       was used as the sole source estimator Lambda.
        (defun leastSquaresError(X cluster)
           vars:(M n N  ey y stdErr avgErr minErr maxErr avgY)
           (setq stdErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for n from 0 until N do (setq stdErr (+ stdErr (* (setq ey (- (estimateFromCluster cluster X[n]) (setq y X[n][M]))) ey))))
           (/ stdErr N)
           (setq cluster.Error stdErr)           
           cluster) ; end define leastSquaresError function
	    ;; Create a cluster directory on the specified 
	    ;; target columns.
        ;; Note: If the target contains a vector
        ;;       of indices, then we are computing
        ;;       the partial distance from a cluster.
	    (defun memorizeCluster(X target)
           vars:(result wy vy y m M n N nn NN t T 
                 scoreVector clusterVector colVector
                 Net minY maxY avgY
                 ) ; end instance variables
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (setq T (length target))
           (if (= T 0)
               then
               (begin
                  (setq colVector (new Vector: M))
                  (loop for t from 0 until M do (setq colVector[t] t))                
                  (setq T M)
               ) ; end then
               else
               (setq colVector target)
               ) ; end if
	       ;; Train the relative values net.
	       (setq Net (new Directory:))
	       ;; Order the observed dependent values by their simple distance metric,
	       ;; using only the independent variables whose columns are specified in
	       ;; the target column vector.
	       (setq minY BIGPOSNUM)
	       (setq maxY BIGNEGNUM)
	       (setq avgY 0)
	       (loop for n from 0 until N do
	          (setq y X[n][M])
	          (setq minY (min y minY))
	          (setq maxY (max y maxY))
	          (setq avgY (+ avgY y))
	          ;; Isolate the cluster vector from the observation vectors.
	          (setq clusterVector (new Vector: T))
	          (loop for t from 0 until T do (setq clusterVector[t] X[n][colVector[t]]))
	          ;; Use the cluster vectors to order the observations.
	          (setq scoreVector Net[clusterVector])
	          (if (= scoreVector #void) (setq scoreVector (new Vector: 0)))
	          (setq scoreVector[(length scoreVector)] y)
	          (setq Net[clusterVector] scoreVector)
	          ) ; end train relative values net
	       (setq avgY (/ avgY N))
	       ;; Average the observed dependent values.
	       (setq NN (length Net))
	       (loop for nn from 0 until NN do 
	          (setq vy Net[nn 0])
	          (setq y (avg Net[nn 1]))
	          (setq Net[vy] y)
	          ) ; end train relative values net
	       ;; Create the result structure.
	       (setq result (new Structure: Net: Net Cols: target Error: maxErr Avg: avgY Min: minY Max: maxY))
	       (leastSquaresError X result)) ; end memorizeCluster function
        ;; Train the nearest neighbor estimator Lambda
        ;; by memorizing the clustered observations.
        (defun Objective(X Y)
           vars:(nn NN m M i I j J k K CC M N ey y stdErr avgErr minErr maxErr avgY)
       	   (setq X (math.convertToArray X Y))
           (setq maxErr math.BIGPOSNUM)
           (setq Clusters (new Vector:))
           (setq variableCount (- (length X[0]) 1))
           ;; Memorize the observations as single column clusters.
           (loop for m from 0 until variableCount (setq Clusters[(length Clusters)] (memorizeCluster X (new Vector: 1 m))))
	       (sort Clusters (lambda( x y) (<= x.Error y.Error)))
           ;; Memorize the observations as double column clusters.
	       (setq I (integer (min 3 (/ variableCount 2))))
	       (loop for i from 0 until I do
	          (loop for j from (addi i 1) to I do
	             (setq Clusters[(length Clusters)] (memorizeCluster X (new Vector: 2 Clusters[i].Cols[0] Clusters[j].Cols[0])))
	             ) ; end j loop
	          ) ; end i loop
           ;; Memorize the observations as triple column clusters.
	       (setq I (integer (min 4 (/ variableCount 2))))
	       (loop for i from 0 to (subi I 2) do
	          (loop for j from (addi i 1) to I do
	             (loop for k from (addi j 1) to I do
	                (setq Clusters[(length Clusters)] (memorizeCluster X (new Vector: 3 Clusters[i].Cols[0] Clusters[j].Cols[0] Clusters[k].Cols[0])))
	                ) ; end k loop
	             ) ; end j loop
	          ) ; end i loop
           ;; Memorize the observations as general clusters.
           (setq Clusters[(length Clusters)] (memorizeCluster X #void))
           ;; Compute the error statistics.
           (setq stdErr 0)
           (setq minErr math.BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq Rf.Error stdErr)
           (setq Rf.AvgErr avgErr)
           (setq Rf.MinErr minErr)
           (setq Rf.MaxErr maxErr)
           (setq Rf.AvgY avgY)
	       ;; Return an error of zero.
           0) ; end objective function
        ;; Define a standard estimator Lambda display function
        (defun show()
           vars:(result ca n N)
           ;; Append the training statistics to the model display
           (setq result (append "model cluster: G=" Rf.G ",P=" Rf.P ",StdErr=" Rf.Error ",AvgErr=" Rf.AvgErr ",MinErr=" Rf.MinErr ",MaxErr=" Rf.MaxErr ",AvgY=" Rf.AvgY))
           result) ; end define a standard estimator Lambda display function
        ;; Test the estimator Lambda
        (defun test(X)
           vars:(nn NN m M i I j J k K CC M N ey y stdErr avgErr minErr maxErr avgY)
           ;; Compute the error statistics.
           (setq stdErr 0)
           (setq minErr math.BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq result (append "model cluster: StdErr=" stdErr ",AvgErr=" avgErr ",MinErr=" minErr ",MaxErr=" maxErr ",AvgY=" avgY))
           result) ; end test function
        ;; ***********************************
        ;; Begin estimator Lambda main logic
        ;; ***********************************
	    (setq NN (length Clusters))
	    (setq ey (new Vector: NN))
	    (setq ev (new Vector: NN))
	    (loop for nn from 0 until NN do
	       (setq ey[nn] (estimateFromCluster Clusters[nn] v))
	       (setq ev[nn] Clusters[nn].Error)
	       ) ; end loop
        ;; Convert the error vector to relevence percentages.
        (setq errTotal (+ (sum ev) .01))
	    (setq ev (math.vectorSub errTotal ev))
        (setq errTotal (sum ev))
	    (setq ev (math.vectorDivide ev errTotal))
	    (setq ey (math.vectorDotProduct ey ev))
	    ey) }
        )) ; end define estimator Lambda
   ;; Set the coefficient vector to the user suggested clusters.
   (setq Rf.Filter filter)
   (setq Rf.variableCount variableCount)
   (setq Rf.C #void)
   (setq Rf.Rf Rf)
   Rf) ; end makeCluster















;;**EXPORTKEY**:math:numericRegress:makeEuclidean
(defchild math.numericRegress makeEuclidean(variableCount filter ...)
;; *****************************************************************************************
;; summary:  Induced Regression:
;;            
;;           Return a Euclidean induction regression estimator Lambda for use with
;;           numericRegress. This estimator Lambda memorizes each unique independent 
;;           vector, X, observed during the training period along with the average 
;;           dependent variable, y, associated with the memorized observation, X.
;;
;;           During estimation, if the input X has been observed during training, then
;;           the best estimate for y is the average y value seen for X during training.
;;           If the input X has NOT been observed during training, then the best estimate
;;           for y is the average y value for the euclidean nearest neighbors of X as 
;;           memorized during training. The optimal neighborhood size is determined 
;;           empirically during training.
;;
;;			 Notes:
;;				o A standard least squares objective function is used.
;;				o Estimator training data must be a single N x M+1 vector array.
;;				o Estimator input must be a single number vector.
;;			    
;; Parms:    variableCount:  The number of independent variables in the regression.
;;           filter:         The type of final output filter. Must be one of
;;                            (binary, nbinary, bipolar, continuous, or sigmoid).
;;           crunch: 		 Input vector crunch function. Defaults to copy function 
;;                            if missing or #void. Can be used to crunch input elements 
;;                            into deciles, percentiles, etc. After learning, during estimation 
;;                            phase, new observations are compared with the memorized crunched 
;;                            training samples stored during learning.
;;                              o Does not effect the dependent variable. 
;;								o Only used during training. 
;;                            	o Must produce a copy of the original input vector.
;;                            	o Must NEVER mutate the original input vector.
;;                           May use the following symbols for specially prepared sigmoid
;;                            input crunching functions.
;;                              o percentile:
;;                              o decile:
;;                              o quintile:
;;                              o quartile:
;;                              o trinary:
;;                              o binary:
;;                           May use a number argument for specially prepared sigmoid
;;                            input crunching functions of the specified arity.
;;           errorOn:        True iff least squared errors are to be returned 
;;                            from the Objective function
;; Return:   Estimator       Always returns a Nearest Neighbor Euclidean cluster regression 
;; ******************************************************************************************
   vars:(X Rf in i crunch)
   ;; Create a Euclidean induction estimator Lambda.
   ;; Note1: The estimator Lambda uses the Euclidean 
   ;;        distance between observations to induce a
   ;;        regression model.
   ;; Note2: A standard least squares error score is computed.
   (setq Rf (eval {
     (lambda(v)
        ;; Persistant variables and child methods
        pvars:(C                    ; Coefficient vector
               Error           		; Final objective error after training
               AvgErr           	; Average objective error after training
               MinErr           	; Minimum objective error after training
               MaxErr           	; Maximum objective error after training
               AvgY              	; Average of dependent variable during training
               Crunch        	    ; My input crunch function (defaults to copy function if not supplied by user)
               Filter        	    ; My output filter (binary, bipolar, continuous, or sigmoid)
               (G 0)             	; Final generation count after training
               avgY           		; The average dependent value seen in training
               maxY           		; The minimum dependent value seen in training
               minY         		; The maximum dependent value seen in training
               myErrorSW       	    ; My return least squares error switch
               myTrainingSW       	; My training in progress switch
               Net           		; The induction net to use in training
               Objective       		; Objective function to use in training
               (P 0)             	; Final population count after training
               Rf              		; My estimator Lambda
               (Strategy induce:)	; My optimization strategy
               variableCount		; Estimator Lambda input variables
               ) ; end of persistant variables
        ;; Temporary variables
        vars:( i j M n N nn NN 
               ey rank wy dy rangeY
               neighbor1stDy        ; The distance to the 1st nearest neighbor
               neighbor2ndDy        ; The distance to the 2nd nearest neighbor
               neighbor3rdDy        ; The distance to the 3rd nearest neighbor
               neighbor1stSum       ; The total y values seen at the 1st nearest neighbor
               neighbor2ndSum       ; The total y values seen at the 2nd nearest neighbor
               neighbor3rdSum       ; The total y values seen at the 3rd nearest neighbor
               neighbor1stCnt       ; The count of y values seen at the 1st nearest neighbor
               neighbor2ndCnt       ; The count of y values seen at the 2nd nearest neighbor
               neighbor3rdCnt       ; The count of y values seen at the 3rd nearest neighbor
               ) ; end of temporary variables
        ;; Private distance function
        (defun distance(v1 v2)
           vars:(dd dy m M)
           (setq M variableCount) 
           (setq dd 0)
           (loop for m from 0 until M do
             (setq dy (- v1[m] v2[m])) 
             (setq dd (+ dd (* dy dy)))
             ) ; end loop
           (sqrt dd)) ; end distance
        ;; Define a standard least square objective function
        (defun Objective(X Y)
           vars:(i j err wy y M N NN scoreVector inputVector 
                 M N ey stdErr avgErr minErr maxErr avgY)
       	   (setq X (math.convertToArray X Y))
           (setq variableCount (setq M (- (length X[0]) 1)))
           (setq M (- (length X[0]) 1))
           (setq N (length X))
	       ;; Train the relative values net.
	       (setq Net (new Directory:))
	       (setq minY BIGPOSNUM)
	       (setq maxY BIGNEGNUM)
	       (setq avgY 0)
	       ;; Order the observed dependent values by their simple distance metric.
	       (loop for i from 0 until N do
	          (setq y X[i][M])
	          (setq minY (min y minY))
	          (setq maxY (max y maxY))
	          (setq avgY (+ avgY y))
	          ;; Use the observation vectors to order the observations.
	          (setq inputVector (Crunch X[i]))
	          (setq scoreVector Net[X[i]])
	          (if (= scoreVector #void) (setq scoreVector (new Vector: 0)))
	          (setq scoreVector[(length scoreVector)] y)
	          (setq Net[inputVector] scoreVector)
	          ) ; end train relative values net
	       (if (<> N 0) (setq avgY (/ avgY N)))
	       ;; Average the observed dependent values.
	       (setq NN (length Net))
	       (loop for i from 0 until NN do 
	          (setq wy Net[i 0])
	          (setq Net[wy] (avg Net[i 1]))
	          ) ; end train relative values net
	       ;; Return a least squares error coefficient if
	       ;; requested by the persistant error switch.
	       ;; Note: Normally we return a least squares
	       ;;       error of zero because we have memorized
	       ;;       each of the training observations verbatim.
	       ;;       If a least squares error value is requested,
	       ;;       we approximate one by averaging the scores
	       ;;       around the 3 nearest neighbors of each
	       ;;       of the observations seen during training.  
           (if (<> myErrorSW true) (return 0))
           (setq myTrainingSW true)
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq Rf.Error stdErr)
           (setq Rf.AvgErr avgErr)
           (setq Rf.MinErr minErr)
           (setq Rf.MaxErr maxErr)
           (setq Rf.AvgY avgY)
           (setq myTrainingSW false)
           Rf.Error) ; end define objective function
        ;; Define a standard esitmator Lambda display function
        (defun show()
           vars:(result ca n N)
           ;; Append the training statistics to the model display
           (setq result (append "model euclidean: G=" Rf.G ",P=" Rf.P ",StdErr=" Rf.Error ",AvgErr=" Rf.AvgErr ",MinErr=" Rf.MinErr ",MaxErr=" Rf.MaxErr ",AvgY=" Rf.AvgY))
           result) ; end define a standard estimator Lambda display function
        ;; Test the estimator Lambda
        (defun test(X)
           vars:(nn NN m M i I j J k K CC M N ey y stdErr avgErr minErr maxErr avgY)
           ;; Compute the error statistics.
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq result (append "model euclidean: StdErr=" stdErr ",AvgErr=" avgErr ",MinErr=" minErr ",MaxErr=" maxErr ",AvgY=" avgY))
           result) ; end test function
        ;; +++++++++++++++++++++++++++++++++++
        ;; Begin estimator Lambda main logic
        ;; +++++++++++++++++++++++++++++++++++
        (setq M variableCount)
	    (setq NN (length Net))
	    ;; Find the nearest neighbors using the distance metric.
	    ;; The nearest neighbor is the one with the smallest distance.
        ;; Note: Determining the best neighborhood size for estimation:
        ;;       For exact matches we use a neighborhood of 1.
        ;;       For inexact matches we use the 3 nearest neighbors.
        (setq neighbor1stDy "NA")
        (setq neighbor2ndDy "NB")
        (setq neighbor3rdDy "NC")
        (setq neighbor1stSum avgY)
        (setq neighbor2ndSum avgY)
        (setq neighbor3rdSum avgY)
        (setq neighbor1stCnt 1)
        (setq neighbor2ndCnt 1)
        (setq neighbor3rdCnt 1)
        (loop for nn from 0 until NN do
           (setq dy (distance v Net[nn 0]))
           (cond
             ;; Distance matches the best seen so far.
             ((= dy neighbor1stDy)
              (begin
                 (setq neighbor1stSum (+ neighbor1stSum Net[nn 1]))
                 (setq neighbor1stCnt (+ neighbor1stCnt 1))
                 ))
             ;; Distance matches the second best seen so far.
             ((= dy neighbor2ndtDy)
              (begin
                 (setq neighbor2ndSum (+ neighbor2ndSum Net[nn 1]))
                 (setq neighbor2ndCnt (+ neighbor2ndCnt 1))
                 ))
             ;; Distance matches the third best seen so far.
             ((= dy neighbor3rdtDy)
              (begin
                 (setq neighbor3rdSum (+ neighbor3rdSum Net[nn 1]))
                 (setq neighbor3rdCnt (+ neighbor3rdCnt 1))
                 ))
             ;; Distance beats the best seen so far.
             ((< dy neighbor1stDy)
              (begin
                 (setq neighbor3rdDy neighbor2ndDy)
                 (setq neighbor3rdSum neighbor2ndSum)
                 (setq neighbor3rdCnt neighbor2ndCnt)
                 (setq neighbor2ndDy neighbor1stDy)
                 (setq neighbor2ndSum neighbor1stSum)
                 (setq neighbor2ndCnt neighbor1stCnt)
                 (setq neighbor1stDy dy)
                 (setq neighbor1stSum Net[nn 1])
                 (setq neighbor1stCnt 1)
                 ))
             ;; Distance beats the second best seen so far.
             ((< dy neighbor2ndDy)
              (begin
                 (setq neighbor3rdDy neighbor2ndDy)
                 (setq neighbor3rdSum neighbor2ndSum)
                 (setq neighbor3rdCnt neighbor2ndCnt)
                 (setq neighbor2ndDy dy)
                 (setq neighbor2ndSum Net[nn 1])
                 (setq neighbor2ndCnt 1)
                 ))
             ;; Distance beats the third best seen so far.
             ((< dy neighbor3rdDy)
              (begin
                 (setq neighbor3rdDy dy)
                 (setq neighbor3rdSum Net[nn 1])
                 (setq neighbor3rdCnt 1)
                 ))
             ) ; end cond
           ) ; end loop
	    ;; Compute the estimated value, ey, for the unknown dependent variable, y.
	    ;; Note: If the input vector has been observed during training,
	    ;;       then use the average value of all dependent variables
	    ;;       observed during training as the estimated value for y.
	    ;;       If the input vector has not been observed during training,
	    ;;       then use the average dependent value in the specified 
	    ;;       neighborhood as the estimated value for y.
	    (if (or (<> Crunch copy) (and (<> myTrainingSW true) (= neighbor1stDy 0)))
	        ;; For exact matches, we use a neighborhood of 1.
	        then
	        (setq ey (/ neighbor1stSum neighbor1stCnt))
	        else
	        ;; For inexact matches, we use a neighborhood of 3.
	        (setq ey (avg (/ neighbor1stSum neighbor1stCnt) (/ neighbor2ndSum neighbor2ndCnt) (/ neighbor3rdSum neighbor3rdCnt)))
			) ; end if
        ;; Convert the dependent value using the requested output filter.
        (setq rangeY (- maxY minY))
        (if (= Filter sigmoid:) (if (= rangeY 0) (setq ey .5) (setq ey (/ (- ey minY) rangeY))))
        (if (= Filter bipolar:) (setq ey (if (>= ey avgY) 1 -1)))
        (if (= Filter binary:) (setq ey (if (>= ey avgY) 1 0)))
        (if (= Filter nbinary:) (setq ey (if (<= ey avgY) 1 0)))
        ;; Return the estimated dependent value.
        ey) }
        )) ; end define estimator Lambda
   ;; ***************************************************************
   ;; Start SETUP logic section.
   ;; ***************************************************************
   ;; A standard input crunching function is created for possible later use.
   ;; Note: Only crunches sigmoid input data properly.
   (setq crunch (lambda(x)
                    pvars:(factor) 
                    vars:(V n N) 
                    (setq V (copy x))
                    (setq N (length V))
                    (loop for n from 0 until N do
                       (setq V[n] (/ (integer (* V[n] factor)) factor))
                       ) ; end loop
                    V)) ; end crunch 
   ;; Examine the input arguments
   (setq Rf.Crunch copy)
   (if (and (>= (argCount) 3) (isLambda (argFetch 2))) 		(setq Rf.Crunch (argFetch 2)))
   (if (and (>= (argCount) 3) (isNumber (argFetch 2))) 		(begin (setq Rf.Crunch crunch) (setq Rf.Crunch.factor (argFetch 2))))
   (if (and (>= (argCount) 3) (= (argFetch 2) percentile:)) (begin (setq Rf.Crunch crunch) (setq Rf.Crunch.factor 100)))
   (if (and (>= (argCount) 3) (= (argFetch 2) decile:)) 	(begin (setq Rf.Crunch crunch) (setq Rf.Crunch.factor 10)))
   (if (and (>= (argCount) 3) (= (argFetch 2) quintile:)) 	(begin (setq Rf.Crunch crunch) (setq Rf.Crunch.factor 5)))
   (if (and (>= (argCount) 3) (= (argFetch 2) quartile:)) 	(begin (setq Rf.Crunch crunch) (setq Rf.Crunch.factor 4)))
   (if (and (>= (argCount) 3) (= (argFetch 2) trinary:)) 	(begin (setq Rf.Crunch crunch) (setq Rf.Crunch.factor 3)))
   (if (and (>= (argCount) 3) (= (argFetch 2) binary:)) 	(begin (setq Rf.Crunch crunch) (setq Rf.Crunch.factor 2)))
   (if (and (= (argCount) 4) (= (argFetch 3) true)) 		(setq Rf.myErrorSW true) (setq Rf.myErrorSW false))
   ;; Set up the Euclidean estimator Lambda.
   (setq Rf.Filter filter)
   (setq Rf.variableCount variableCount)
   (setq Rf.C (new Vector: number: 1 2))
   (setq Rf.Rf Rf)
   Rf) ; end makeEuclidean

































;;**EXPORTKEY**:math:numericRegress:makeExponential
(defchild math.numericRegress makeExponential(variableCount)
;; *******************************************************************
;; summary:  Genetic Evolutionary Regression: (real number genome)
;; 
;;           Return an exponential regression estimator Lambda for
;;           use with numericRegress. The estimator Lambda implements 
;;           an exponential polynomial regression model, which has
;;           been modified to prevent overflow.
;;
;;			 Notes:
;;				o A standard least squares objective function is used.
;;				o Estimator training data must be a single N x M+1 vector array.
;;				o Estimator input must be a single number vector.
;;			    
;;			 Model:
;; 				y = C[0] + C[1]*expt(X[0],C[2]) + ... +  C[2M]*expt(X[M-1],C[2M+1])
;; 
;; Parms:    variableCount:  The number of independent variables in the regression.
;;
;; Return:   Estimator       Always returns an estimator Lambda.
;; *******************************************************************
   vars:(X Rf in i)
   ;; Create an exponential estimator Lambda.
   ;; Note1: The estimator Lambda is an exponential 
   ;;        polynomial regression model, which has
   ;;        been modified to prevent overflow.
   ;; Note2: A standard least squares objective
   ;;        function is used.
   (setq Rf (eval {
     (lambda(v)
        pvars:(C                    ; Coefficient vector
               Error           		; Final objective error after training
               AvgErr           	; Average objective error after training
               MinErr           	; Minimum objective error after training
               MaxErr           	; Maximum objective error after training
               AvgY              	; Average of dependent variable during training
               Fe                   ; Final function executions after training
               G               		; Final generation count after training
               Objective       		; Objective function to use in training
               P               		; Final population count after training
               Rf              		; My estimator Lambda
               (Strategy evolve) 	; My optimization strategy
               ) ; end of persistant variables
        vars:(i cc ce result M ey y)
        ;; Define a standard least square objective function 
        ;; Note: The observations must be a vector array.
        (defun Objective(X)
           vars:(i M N ey y stdErr avgErr minErr maxErr avgY)
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (+= Fe N)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq Rf.Error stdErr)
           (setq Rf.AvgErr avgErr)
           (setq Rf.MinErr minErr)
           (setq Rf.MaxErr maxErr)
           (setq Rf.AvgY avgY)
           Rf.Error) ; end define objective function
        ;; Define a standard estimator Lambda display function
        (defun show()
           vars:(result cc ce  n N)
           (setq result (append "model: y = " C[0]))
           (setq N (divi (length C) 2))
	       (setq cc 1)
	       (setq ce 2)
           (loop for n from 0 until N do
              ;; + (Cc[n]*expt(x[n],Ce[n])
              (if (isNegative C[cc])
                  (setq result (append result " - (" (abs C[cc]) "*" "expt(x[" n "]," C[ce] "))"))
                  (setq result (append result " + (" C[cc] "*" "expt(x[" n "]," C[ce] "))"))
                  ) ; end sign if
              (setq cc (addi cc 2))
              (setq ce (addi ce 2))
              ) ; end loop
           ;; Append the training statistics to the model display
           (setq result (append result _eol "G=" Rf.G ",P=" Rf.P ",StdErr=" Rf.Error ",AvgErr=" Rf.AvgErr ",MinErr=" Rf.MinErr ",MaxErr=" Rf.MaxErr ",AvgY=" Rf.AvgY))
           result) ; end define a estimator Lambda display function
        ;; Test the estimator Lambda
        (defun test(X)
           vars:(nn NN m M i I j J k K CC M N ey y stdErr avgErr minErr maxErr avgY)
           ;; Compute the error statistics.
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq result (append "model exponential: StdErr=" stdErr ",AvgErr=" avgErr ",MinErr=" minErr ",MaxErr=" maxErr ",AvgY=" avgY))
           result) ; end test function
        ;; Begin estimator Lambda main logic
        (setq M (divi (length C) 2))
        (setq cc 1)
        (setq ce 2)
        (setq result C[0])
        (loop for i from 0 until M do 
           (setq result (+ result (expt (* C[cc] v[i]) C[ce])))
           (setq cc (addi cc 2))
           (setq ce (addi ce 2))
           ) ; end loop
        ;; Forceably prevent overflow
        (setq result (numCheck result))
        result) }
        )) ; end define estimator Lambda
   ;; Set the coefficient vector to the number of variables times 2 plus the constant coefficient.
   (setq Rf.C (new Vector: number: (integer (+ (* variableCount 2) 1)) 0))
   (setq Rf.Rf Rf)
   Rf) ; end makeExponential







































;;**EXPORTKEY**:math:numericRegress:makeLinear
(defchild math.numericRegress makeLinear(variableCount)
;; *******************************************************************
;; summary:  Multivariable Linear Regression:
;; 
;;           Return a linear regression estimator Lambda for 
;;           use with numericRegress. The estimator Lambda implements 
;;           a linear polynomial regression model, which has
;;           been modified to prevent overflow.
;;
;;			 Notes:
;;				o A standard least squares objective function is used.
;;				o Estimator training data must be a single N x M+1 number matrix.
;;				o Estimator input must be a single number vector.
;;			    
;;			 Model: 
;;				y = C[0]*X[0] + ... +  C[M-1]*X[M-1]
;;
;;			 Note: This model does NOT support a threshold constant at C[0].
;;                 The model assumes each coefficient C[m] is multiplied
;;                 by its paired variable X[m]. 
;;
;;			 Hint: A threshold constant at C[0] can be achieved by setting
;;				   X[0] equal to one for all rows in the training data. The
;;                 resulting C[0] value will equal the threshold constant.
;; 
;; Parms:    variableCount:  The number of independent variables in the regression.
;;
;; Return:   Estimator       Always returns an estimator Lambda.
;; *******************************************************************
   vars:(X Rf in i)
   ;; Create a linear estimator Lambda.
   ;; Note1: The estimator Lambda is a standard 
   ;;        linear regression model, which has
   ;;        been modified to prevent overflow.
   ;; Note2: A standard least squares objective
   ;;        function is used.
   (setq Rf (eval {
     (lambda(v)
        pvars:(C                    ; Coefficient vector
               Error           		; Final objective error after training
               AvgErr           	; Average objective error after training
               MinErr           	; Minimum objective error after training
               MaxErr           	; Maximum objective error after training
               AvgY              	; Average of dependent variable during training
               (G 0)             	; Final generation count after training
               Objective       		; Objective function to use in training
               (P 0)             	; Final population count after training
               Rf              		; My estimator Lambda
               (Strategy linear)	; My optimization strategy
               variableCount		; Estimator Lambda input variables
               ) ; end of persistant variables
        vars:(i j result M ey y)
        ;; Define a standard least square objective function
        ;; Note: X must be an N x M+1 number matrix.
        (defun Objective(X)
           vars:(i ey y M N stdErr avgErr minErr maxErr avgY)
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq N (rank X)[0])
           (setq M (- (rank X)[1] 1))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf (matrixRowToVector X i M)) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq Rf.Error stdErr)
           (setq Rf.AvgErr avgErr)
           (setq Rf.MinErr minErr)
           (setq Rf.MaxErr maxErr)
           (setq Rf.AvgY avgY)
           Rf.Error) ; end define objective function
        ;; Define a standard estimator Lambda display function
        (defun show()
           vars:(result n N)
           (setq result (append "model: y = 0"))
           (setq N (length C))
           (loop for n from 0 until N do
              (if (isNegative C[n])
                  (setq result (append result " - (" (abs C[n]) "*" "x[" n "])"))
                  (setq result (append result " + (" C[n] "*" "x[" n "])"))
                  ) ; end sign if
              ) ; end loop
           ;; Append the training statistics to the model display
           (setq result (append result _eol "G=" Rf.G ",P=" Rf.P ",ETol=" Rf.ErrTollerance ",StdErr=" Rf.Error ",AvgErr=" Rf.AvgErr ",MinErr=" Rf.MinErr ",MaxErr=" Rf.MaxErr ",AvgY=" Rf.AvgY))
           result) ; end define a standard esitmator Lambda display function
        ;; Test the estimator Lambda
        ;; Note: X must be an N x M+1 number matrix.
        (defun test(X)
           vars:(nn NN m M i I j J k K CC M N ey y stdErr avgErr minErr maxErr avgY)
           ;; Compute the error statistics.
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq N (rank X)[0])
           (setq M (- (rank X)[1] 1))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf (matrixRowToVector X i M)) (setq y X[i M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq result (append "model linear: StdErr=" stdErr ",AvgErr=" avgErr ",MinErr=" minErr ",MaxErr=" maxErr ",AvgY=" avgY))
           result) ; end test function
        ;; Begin estimator Lambda main logic
        (setq M (length C))
        (setq result 0)
        (loop for i from 0 until M do (setq result (+ result (* C[i] v[i]))))
        ;; Forceably prevent overflow
        (setq result (numCheck result))
        result) }
        )) ; end define estimator Lambda
   ;; Set the coefficient vector to the number of variables plus the constant coefficient.
   (setq Rf.variableCount variableCount)
   (setq Rf.C (new Vector: number: (+ variableCount 1) 0))
   (setq Rf.Rf Rf)
   Rf) ; end makeLinear



















;;**EXPORTKEY**:math:numericRegress:makeLinearEvolve
(defchild math.numericRegress makeLinearEvolve(variableCount)
;; *******************************************************************
;; summary:  Genetic Evolutionary Regression: (real number genome)
;; 
;;           Return a linear regression estimator Lambda for
;;           use with numericRegress. The estimator Lambda implements 
;;           a linear polynomial regression model, which has
;;           been modified to prevent overflow.
;;
;;			 Notes:
;;				o A standard least squares objective function is used.
;;				o Estimator training data must be a single N x M+1 vector array.
;;				o Estimator input must be a single number vector.
;;			    
;;			 Model:
;; 				y = C[0] + C[1]*X[0] + ... +  C[M]*X[M-1]
;; 
;; Parms:    variableCount:  The number of independent variables in the regression.
;;
;; Return:   Estimator       Always returns an estimator Lambda.
;; *******************************************************************
   vars:(X Rf in i)
   ;; Create a linear estimator Lambda.
   ;; Note1: The estimator Lambda is a standard 
   ;;        linear regression model, which has
   ;;        been modified to prevent overflow.
   ;; Note2: A standard least squares objective
   ;;        function is used.
   (setq Rf (eval {
     (lambda(v)
        pvars:(C                    ; Coefficient vector
               Error           		; Final objective error after training
               AvgErr           	; Average objective error after training
               MinErr           	; Minimum objective error after training
               MaxErr           	; Maximum objective error after training
               AvgY              	; Average of dependent variable during training
               (G 0)             	; Final generation count after training
               Objective       		; Objective function to use in training
               (P 0)             	; Final population count after training
               Rf              		; My estimator Lambda
               (Strategy evolve)	; My optimization strategy
               variableCount		; Estimator Lambda input variables
               ) ; end of persistant variables
        vars:(i j result M ey y)
        ;; Define a standard least square objective function 
        ;; Note: The observations must be a vector array.
        (defun Objective(X)
           vars:(i ey y M N stdErr avgErr minErr maxErr avgY)
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq Rf.Error stdErr)
           (setq Rf.AvgErr avgErr)
           (setq Rf.MinErr minErr)
           (setq Rf.MaxErr maxErr)
           (setq Rf.AvgY avgY)
           Rf.Error) ; end define objective function
        ;; Define a standard estimator Lambda display function
        (defun show()
           vars:(result ca n N)
           (setq result (append "model: y = " C[0]))
           (setq N (subi (length C) 1))
	       (setq ca 1)
           (loop for n from 0 until N do
              ;; + (C[n]*x[n])
              (if (isNegative C[ca])
                  (setq result (append result " - (" (abs C[ca]) "*" "x[" n "])"))
                  (setq result (append result " + (" C[ca] "*" "x[" n "])"))
                  ) ; end sign if
              (setq ca (addi ca 1))
              ) ; end loop
           ;; Append the training statistics to the model display
           (setq result (append result _eol "G=" Rf.G ",P=" Rf.P ",StdErr=" Rf.Error ",AvgErr=" Rf.AvgErr ",MinErr=" Rf.MinErr ",MaxErr=" Rf.MaxErr ",AvgY=" Rf.AvgY))
           result) ; end define a standard estimator Lambda display function
        ;; Test the estimator Lambda
        (defun test(X)
           vars:(nn NN m M i I j J k K CC M N ey y stdErr avgErr minErr maxErr avgY)
           ;; Compute the error statistics.
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq result (append "model linearEvolve: StdErr=" stdErr ",AvgErr=" avgErr ",MinErr=" minErr ",MaxErr=" maxErr ",AvgY=" avgY))
           result) ; end test function
        ;; Begin estimator Lambda main logic
        (setq M (subi (length C) 1))
        (setq result C[0])
        (loop for i from 0 until M do (setq result (+ result (* C[(+ i 1)] v[i]))))
        ;; Forceably prevent overflow
        (setq result (numCheck result))
        result) }
        )) ; end define estimator Lambda
   ;; Set the coefficient vector to the number of variables plus the constant coefficient.
   (setq Rf.variableCount variableCount)
   (setq Rf.C (new Vector: number: (+ variableCount 1) 0))
   (setq Rf.Rf Rf)
   Rf) ; end makeLinearEvolve




















;;**EXPORTKEY**:math:numericRegress:makeLinearNetwork
(defchild math.numericRegress makeLinearNetwork(variableCount filter)
;; *****************************************************************************************
;; summary:  Induced Regression:
;;            
;; 			 Return a linear network regression estimator Lambda for use with 
;;           numericRegress. This estimator Lambda memorizes each unique independent 
;;			 vector, X, observed during the training period along with the average 
;;           dependent variable, y, associated with the memorized observation, X. 
;;           Additionally, all the observations are segmented into nearest neighbor 
;;           euclidean clusters along each independent axis. For each cluster along 
;;           each independent axis, the optimal multivariable regression coefficients 
;;           are memorized.
;;
;;           During estimation, if the input X has been observed during training, then
;;           the best estimate for y is the average y value seen for X during training.
;;           If the input X has NOT been observed during training, then the best estimate
;;           for y is the average y value, returned from the memorized optimal multivariable
;;           regression coefficients, for each of the euclidean nearest neighbor clusters 
;;           of X as on each independent axis as segmented during training. 
;;
;;			 Notes:
;;				o A standard least squares objective function is used.
;;				o Estimator training data must be a single N x M+1 vector array.
;;				o Estimator input must be a single number vector.
;;			    
;; Parms:    variableCount:  The number of independent variables in the regression.
;;           filter:         The type of final output filter. Must be one of
;;                           (binary, nbinary, bipolar, continuous, or sigmoid).
;;
;; Return:   Estimator       Always returns a Nearest Neighbor Euclidean cluster regression 
;; ******************************************************************************************
   vars:(X Rf in i)
   ;; Create a Linear Network Regression induction estimator Lambda.
   ;; Note1: The estimator Lambda uses the Euclidean 
   ;;        distance between observations to induce a
   ;;        regression model.
   ;; Note2: A standard least squares error score is computed.
   (setq Rf (eval {
     (lambda(v)
        pvars:(C                    ; Coefficient vector
               check           		; Check the result for over or under flow.
               Clusters	            ; My training cluster array
               distance        		; Private distance function.
               Error           		; Final objective error after training
               AvgErr           	; Average objective error after training
               MinErr           	; Minimum objective error after training
               MaxErr           	; Maximum objective error after training
               AvgY              	; Average of dependent variable during training
               estimateSegment 	    ; Estimate the dependent variable using the specified segment
               Filter        	    ; My output filter (binary, bipolar, continuous, or sigmoid)
               (G 0)             	; Final generation count after training
               avgY           		; The average dependent value seen in training
               maxY           		; The minimum dependent value seen in training
               minY         		; The maximum dependent value seen in training
               Net           		; The induction net to use in training
               dNet           		; The distance net to use in prediction
               Objective       		; Objective function to use in training
               oldC       		    ; The previous neighborhood used in the prior prediction
               oldV       		    ; The previous input vector used in the prior prediction
               (P 0)             	; Final population count after training
               regressSegment 		; Create a regression vector on the specified Segment
               Rf              		; My estimator Lambda
               segment 		        ; Segment the input array on the specified column
               (Strategy induce)	; My optimization strategy
               variableCount		; Estimator Lambda input variables
               ) ; end of persistant variables
        vars:(i j m M n N NN ey err errTotal rank wy dy neighborhood rangeY)
        ;; **************************************
        ;; Define estimator Lambda child Lambdas
        ;; **************************************
        ;; Private distance function
        (defun distance(v1 v2)
           vars:(dd dy m M)
           (setq M variableCount) 
           (setq dd 0)
           (loop for m from 0 until M do
             (setq dy (- v1[m] v2[m])) 
             (setq dd (+ dd (* dy dy)))
             ) ; end loop
           (sqrt dd)) ; end distance function  
        ;; Estimate the dependent variable using the specified segment
        (defun estimateSegment(Seg target v ey ee)
           vars:(m M n N result CC C E)
           (setq C Seg[target].C)
           (setq CC Seg[target].CC)
           (setq E Seg[target].E)
           (loop for n from 0 until (length CC) do (if (<= v[target] CC[n]) (goto NextStep:)))
           NextStep::
           (setq M (length C[n]))
           (setq result C[n][0])
           (loop for m from 1 until M do (setq result (+ result (* v[(sub1 m)] C[n][m]))))
           (setq ee[target] E[n])
           (setq ey[target] result)
           result)  ; end estimateSegment Lambda
        ;; Define a standard least square objective function
        (defun Objective(X Y)
           vars:(i j result wy y m M n N NN scoreVector bestC bestError 
                 errorVector maxNeighborhood ey stdErr avgErr minErr maxErr avgY)
       	   (setq X (math.convertToArray X Y))
           (setq dNet (setq oldC (setq oldV #void)))
           (setq result 0)
           (setq variableCount (setq M (- (length X[0]) 1)))
           (setq N (length X))
	       ;; Train the relative values net.
	       (setq Net (new Directory:))
	       (setq minY BIGPOSNUM)
	       (setq maxY BIGNEGNUM)
	       (setq avgY 0)
	       ;; Order the observed dependent values by their simple distance metric.
	       (loop for i from 0 until N do
	          (setq y X[i][M])
	          (setq minY (min y minY))
	          (setq maxY (max y maxY))
	          (setq avgY (+ avgY y))
	          ;; Use the observation vectors to order the observations.
	          (setq scoreVector Net[X[i]])
	          (if (= scoreVector #void) (setq scoreVector (new Vector: 0)))
	          (setq scoreVector[(length scoreVector)] y)
	          (setq Net[X[i]] scoreVector)
	          ) ; end train relative values net
	       (setq avgY (/ avgY N))
	       ;; Average the observed dependent values.
	       (setq NN (length Net))
	       (loop for i from 0 until NN do 
	          (setq wy Net[i 0])
	          (setq Net[wy] (avg Net[i 1]))
	          ) ; end train relative values net
	       ;; Create a segmentation for each independent variable
	       ;; in the observation array seen during training.
	       (setq Clusters (new Vector: M))
           (loop for m from 0 until M do
               (setq Clusters[m] (segment X m))
               (setq Clusters[m] (regressSegment Clusters[m] m))
               ) ; end segment loop
           ;; Compute standard error statistics
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq Rf.Error stdErr)
           (setq Rf.AvgErr avgErr)
           (setq Rf.MinErr minErr)
           (setq Rf.MaxErr maxErr)
           (setq Rf.AvgY avgY)
           ;; Always return an error of zero
           0) ; end objective function
	    ;; Create a regression vector on the specified Segment.
	    (defun regressSegment(WS target)
	       vars:(S s ws n N m M CC C E vc result)
		   ;; Segment the input array.
		   (setq S (length WS))
		   (setq CC (new Vector: S))
		   (setq E (new Vector: (add1 S)))
		   (setq C (new Vector: (add1 S)))
		   ;; Compute the coefficient vectors from multiple linear regressions.
		   (loop for s from 0 until S do
		      (setq CC[s] WS[s][(sub1 (length WS[s]))][target])
		      (setq vc (math.multivariableRegressC WS[s]))
		      (setq M (subi (length vc) 1))
		      (setq E[s] vc[M])
		      (setq C[s] (resize vc M))
		      ) ; end loop
		   (setq CC[S] CC[(sub1 S)])
		   (setq E[S] E[(sub1 S)])
		   (setq result (new Structure: C: C CC: CC E: E))
	       result) ; end regressSegment function
	    ;; Segment the input array on the specified column.
	    (defun segment(X target)
	       vars:(WS S s ws n N m M)
	       ;; Determine the length and width of the observation array.
	       (setq N (length X))
	       (setq M (integer (- (length X[0]) 1)))
	       ;; Heuristically determine the number of segments.
	       (setq S (integer (min 10 (max 1 (/ N (* 1.5 M))))))
	       ;; Sort the training samples on the specified variable.
	       (sort X (let ((target target)) (lambda(_x _y) (<= _x[target] _y[target]))))
	       ;; Heuristically determine the number of segments.
	       (setq M (integer (/ N S)))
	       (setq WS (new Vector: S))
	       (loop for n from 0 until N do
	          (setq m (divi n M))
	          (if (= WS[m] #void) (setq WS[m] (new Vector:)))
	          (setq WS[m][(length WS[m])] X[n])
	          ) ; end loop		    
	       WS) ; end segment function
        ;; Define a standard estimnator Lambda display function
        (defun show()
           vars:(result ca n N)
           ;; Append the training statistics to the model display
           (setq result (append "model linearNetwork: G=" Rf.G ",P=" Rf.P ",StdErr=" Rf.Error ",AvgErr=" Rf.AvgErr ",MinErr=" Rf.MinErr ",MaxErr=" Rf.MaxErr ",AvgY=" Rf.AvgY))
           result) ; end define a standard estimator Lambda display function
        ;; Test the estimator Lambda
        (defun test(X)
           vars:(nn NN m M i I j J k K CC M N ey y stdErr avgErr minErr maxErr avgY)
           ;; Compute the error statistics.
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq result (append "model linearNetwork: StdErr=" stdErr ",AvgErr=" avgErr ",MinErr=" minErr ",MaxErr=" maxErr ",AvgY=" avgY))
           result) ; end test function
        ;; ***********************************
        ;; Begin estimator Lambda main logic
        ;; ***********************************
        (setq M variableCount)
	    (setq NN (length Net))
	    ;; Find the nearest neighbors using the distance metric.
	    ;; Note: The nearest neighbor is the one which sorts to 
	    ;;       the front of the dNet Dictionary.
	    (if (<> v oldV)
	        (begin
	           (setq dNet (new Directory:))
		       (loop for i from 0 until NN do
		          (setq dy (distance v Net[i 0]))
		          (setq dNet[dy] Net[i 1])
		       ))) ; end find the nearest neighbor
	    ;; Compute the estimated value, ey, for the unknown dependent variable, y.
	    ;; Note: If the input vector has been observed during training,
	    ;;       then use the average value of all dependent variables
	    ;;       observed during training as the estimated value for y.
	    ;;       If the input vector has not been observed during training,
	    ;;       then use the average dependent value in the specified 
	    ;;       neighborhood as the estimated value for y.
	    (if (= dNet[0 0] 0)
	        then 
	        (setq ey dNet[0 1])
	        else
	        (begin
	           ;; Fill the estimate and error vectors.
			   (setq ey (new Vector: M))
			   (setq err (new Vector: M))
			   (loop for m from 0 until M do
			      (estimateSegment Clusters m v ey err)
			      ) ; end find the nearest neighbor
	           ;; Convert the error vector to relevence percentages.
	           (setq errTotal (sum err))
			   (setq err (math.vectorSub errTotal err))
	           (setq errTotal (sum err))
			   (setq err (math.vectorDivide err errTotal))
			   (setq ey (math.vectorDotProduct ey err))
			)) ; end if
        ;; Convert the dependent value using the requested output filter.
        (setq rangeY (- maxY minY))
        (if (= Filter sigmoid:) (if (= rangeY 0) (setq ey .5) (setq ey (/ (- ey minY) rangeY))))
        (if (= Filter bipolar:) (setq ey (if (>= ey avgY) 1 -1)))
        (if (= Filter binary:) (setq ey (if (>= ey avgY) 1 0)))
        (if (= Filter nbinary:) (setq ey (if (<= ey avgY) 1 0)))
        ;; Return the estimated dependent value.
        (setq oldC C[0])
        (setq oldV v)
        ey) }
        )) ; end define estimator Lambda
   ;; Set the coefficient vector to the optimal distance coefficient.
   (setq Rf.Filter filter)
   (setq Rf.variableCount variableCount)
   (setq Rf.C (new Vector: number: 1 3))
   (setq Rf.Rf Rf)
   Rf) ; end makeLinearNetwork






































;;**EXPORTKEY**:math:numericRegress:makeLogit
(defchild math.numericRegress makeLogit(variableCount)
;; *******************************************************************
;; summary:  Genetic Evolutionary Regression: (real number genome)
;; 
;;           Return a logit regression estimator Lambda for use
;;           with numericRegress. The estimator Lambda implements 
;;           a logit polynomial regression model, which has been
;;           modified to prevent overflow.
;;
;;			 Notes:
;;				o A standard least squares objective function is used.
;;				o Estimator training data must be a single N x M+1 vector array.
;;				o Estimator input must be a single number vector.
;;			    
;;			 Model:
;; 				d = C[0] + abs(C[1])*X[0] + ... +  abs(C[M])*X[M]
;;				y = exp(d) / (1 + exp(d))
;; 
;; Parms:    variableCount:  The number of independent variables in the regression.
;;
;; Return:   Estimator       Always returns an estimator Lambda.
;; *******************************************************************
   vars:(X Rf in i)
   ;; Create a sigmoid logit estimator Lambda.
   ;; Note1: The estimator Lambda is a standard 
   ;;        logit regression model, which has
   ;;        been modified to prevent overflow.
   ;; Note2: A standard least squares objective
   ;;        function is used.
   (setq Rf (eval {
     (lambda(v)
        pvars:(C                    ; Coefficient vector
               Error           		; Final objective error after training
               AvgErr           	; Average objective error after training
               MinErr           	; Minimum objective error after training
               MaxErr           	; Maximum objective error after training
               AvgY              	; Average of dependent variable during training
               G               		; Final generation count after training
               Objective       		; Objective function to use in training
               P               		; Final population count after training
               Rf              		; My estimator Lambda
               (Strategy evolve) 	; My optimization strategy
               ) ; end of persistant variables
        vars:(i j result M ey y)
        ;; Define a standard least square objective function 
        ;; Note: The observations must be a vector array.
        (defun Objective(X)
           vars:(i M N y ey stdErr avgErr minErr maxErr avgY)
           ;; Compute standard error statistics
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq Rf.Error stdErr)
           (setq Rf.AvgErr avgErr)
           (setq Rf.MinErr minErr)
           (setq Rf.MaxErr maxErr)
           (setq Rf.AvgY avgY)
           Rf.Error) ; end define objective function
        ;; Define a standard estimator Lambda display function
        (defun show()
           vars:(result ca n N)
           (setq result (append "model: y = logit(" C[0]))
           (setq N (subi (length C) 1))
	       (setq ca 1)
           (loop for n from 0 until N do
              (if (isNegative C[ca])
                  (setq result (append result " - (" (abs C[ca]) "*" "x[" n "])"))
                  (setq result (append result " + (" C[ca] "*" "x[" n "])"))
                  ) ; end sign if
              (setq ca (addi ca 1))
              ) ; end loop
           ;; Append the training statistics to the model display
           (setq result (append result ")" _eol "G=" Rf.G ",P=" Rf.P ",StdErr=" Rf.Error ",AvgErr=" Rf.AvgErr ",MinErr=" Rf.MinErr ",MaxErr=" Rf.MaxErr ",AvgY=" Rf.AvgY))
           result) ; end define a standard estimator Lambda display function
        ;; Test the estimator Lambda
        (defun test(X)
           vars:(nn NN m M i I j J k K CC M N ey y stdErr avgErr minErr maxErr avgY)
           ;; Compute the error statistics.
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq result (append "model logit: StdErr=" stdErr ",AvgErr=" avgErr ",MinErr=" minErr ",MaxErr=" maxErr ",AvgY=" avgY))
           result) ; end test function
        ;; Begin estimator Lambda main logic
        (setq M (subi (length C) 1))
        (setq result C[0])
        (loop for i from 0 until M do (setq result (+ result (* C[(+ i 1)] v[i]))))
        ;; Forceably prevent overflow
        (setq result (numCheck result))
        ;; Create sigmoid logit result
        (setq result (/ (exp result) (add1 (exp result)))) 
        result) }
        )) ; end define estimator Lambda
   ;; Set the coefficient vector to the number of variables plus the constant coefficient.
   (setq Rf.C (new Vector: number: (+ variableCount 1) 0))
   (setq Rf.Rf Rf)
   Rf) ; end makeLogit







































;;**EXPORTKEY**:math:numericRegress:makeNeighbor
(defchild math.numericRegress makeNeighbor(variableCount filter)
;; *****************************************************************************************
;; summary:  Induced Regression:
;;            
;;           Return a nearest neighbor induction regression estimator Lambda
;;           for use with numericRegress. This estimator Lambda memorizes each unique 
;;           independent vector, X, observed during the training period along with the 
;;           average dependent variable, y, associated with the memorized observation, X. 
;;           Additionally, all the observations are segmented into nearest neighbor euclidean 
;;           clusters along each independent axis. For each cluster along each independent 
;;           axis, the optimal multivariable regression coefficients are memorized.
;;
;;           During estimation, if the input X has been observed during training, then
;;           the best estimate for y is the average y value seen for X during training.
;;           If the input X has NOT been observed during training, then the best estimate
;;           for y is the average y value, returned from the memorized optimal multivariable
;;           regression coefficients, for each of the euclidean nearest neighbor clusters 
;;           of X as on each independent axis as segmented during training. 
;;
;;			 Notes:
;;				o A standard least squares objective function is used.
;;				o Estimator training data must be a single N x M+1 vector array.
;;				o Estimator input must be a single number vector.
;;			    
;; Parms:    variableCount:  The number of independent variables in the regression.
;;           filter:         The type of final output filter. Must be one of
;;                           (binary, nbinary, bipolar, continuous, or sigmoid).
;;
;; Return:   Estimator       Always returns a Nearest Neighbor Euclidean cluster estimator Lambda 
;; ******************************************************************************************
   vars:(X Rf in i)
   ;; Create a nearest neighbor induction Regression induction estimator Lambda.
   ;; Note1: The estimator Lambda uses the Euclidean 
   ;;        distance between observations to induce a
   ;;        regression model.
   ;; Note2: A standard least squares error score is computed.
   (setq Rf (eval {
     (lambda(v)
        pvars:(C                    ; Coefficient vector
               check           		; Check the result for over or under flow.
               Clusters	            ; My training cluster array
               distance        		; Private distance function.
               Error           		; Final objective error after training
               AvgErr           	; Average objective error after training
               MinErr           	; Minimum objective error after training
               MaxErr           	; Maximum objective error after training
               AvgY              	; Average of dependent variable during training
               estimateSegment 	    ; Estimate the dependent variable using the specified segment
               Filter        	    ; My output filter (binary, bipolar, continuous, or sigmoid)
               (G 0)             	; Final generation count after training
               avgY           		; The average dependent value seen in training
               maxY           		; The minimum dependent value seen in training
               minY         		; The maximum dependent value seen in training
               Net           		; The induction net to use in training
               dNet           		; The distance net to use in prediction
               Objective       		; Objective function to use in training
               oldC       		    ; The previous neighborhood used in the prior prediction
               oldV       		    ; The previous input vector used in the prior prediction
               (P 0)             	; Final population count after training
               memorizeSegment 		; Create a statistical vector on the specified Segment
               Rf              		; My estimator Lambda
               segment 		        ; Segment the input array on the specified column
               (Strategy induce)	; My optimization strategy
               variableCount		; Estimator Lambda input variables
               ) ; end of persistant variables
        vars:(i j m M n N NN ey err errTotal rank wy dy neighborhood rangeY)
        ;; **************************************
        ;; Define estimator Lambda child Lambdas
        ;; **************************************
        ;; Private distance function
        (defun distance(v1 v2)
           vars:(dd dy m M)
           (setq M variableCount) 
           (setq dd 0)
           (loop for m from 0 until M do
             (setq dy (- v1[m] v2[m])) 
             (setq dd (+ dd (* dy dy)))
             ) ; end loop
           (sqrt dd)) ; end distance function  
        ;; Estimate the dependent variable using the specified segment
        (defun estimateSegment(Seg target v ey ee)
           vars:(m M n N result CC C E)
           (setq C Seg[target].C)
           (setq CC Seg[target].CC)
           (setq E Seg[target].E)
           (loop for n from 0 until (length CC) do (if (<= v[target] CC[n]) (goto NextStep:)))
           NextStep::
           (setq ee[target] E[n])
           (setq ey[target] C[n])
           result)  ; end estimateSegment function
        ;; Define a standard least square objective function
        (defun Objective(X Y)
           vars:(i j result wy y m M n N NN scoreVector bestC bestError 
                 errorVector maxNeighborhood ey stdErr avgErr minErr maxErr avgY)
       	   (setq X (math.convertToArray X Y))
           (setq dNet (setq oldC (setq oldV #void)))
           (setq result 0)
           (setq variableCount (setq M (- (length X[0]) 1)))
           (setq N (length X))
	       ;; Train the relative values net.
	       (setq Net (new Directory:))
	       (setq minY BIGPOSNUM)
	       (setq maxY BIGNEGNUM)
	       (setq avgY 0)
	       ;; Order the observed dependent values by their simple distance metric.
	       (loop for i from 0 until N do
	          (setq y X[i][M])
	          (setq minY (min y minY))
	          (setq maxY (max y maxY))
	          (setq avgY (+ avgY y))
	          ;; Use the observation vectors to order the observations.
	          (setq scoreVector Net[X[i]])
	          (if (= scoreVector #void) (setq scoreVector (new Vector: 0)))
	          (setq scoreVector[(length scoreVector)] y)
	          (setq Net[X[i]] scoreVector)
	          ) ; end train relative values net
	       (setq avgY (/ avgY N))
	       ;; Average the observed dependent values.
	       (setq NN (length Net))
	       (loop for i from 0 until NN do 
	          (setq wy Net[i 0])
	          (setq Net[wy] (avg Net[i 1]))
	          ) ; end train relative values net
	       ;; Create a segmentation for each independent variable
	       ;; in the observation array seen during training.
	       (setq Clusters (new Vector: M))
           (loop for m from 0 until M do
               (setq Clusters[m] (segment X m))
               (setq Clusters[m] (memorizeSegment Clusters[m] m))
               ) ; end segment loop
           ;; Compute standard error statistics
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq Rf.Error stdErr)
           (setq Rf.AvgErr avgErr)
           (setq Rf.MinErr minErr)
           (setq Rf.MaxErr maxErr)
           (setq Rf.AvgY avgY)
           ;; Always return an error of zero.
           0) ; end objective function
	    ;; Create a statistical vector on the specified Segment.
	    (defun memorizeSegment(WS target)
	       vars:(S s ws n N m M CC C E vc result)
		   ;; Segment the input array.
		   (setq S (length WS))
		   (setq CC (new Vector: S))
		   (setq E (new Vector: (add1 S)))
		   (setq C (new Vector: (add1 S)))
		   ;; Compute the statistical vectors from average and standard
		   ;; deviation of the dependent variable across the segment.
		   (loop for s from 0 until S do
		      (setq M (sub1 (length WS[s][0])))
		      (setq CC[s] WS[s][(sub1 (length WS[s]))][target])
		      (setq vc (math.convertToColumnVector WS[s] M))
		      (setq M (subi (length vc) 1))
		      (if (> M 1) (setq E[s] (stdev vc)) (setq E[s] 0))
		      (setq C[s] (avg vc))
		      ) ; end loop
		   (setq CC[S] CC[(sub1 S)])
		   (setq E[S] E[(sub1 S)])
		   (setq result (new Structure: C: C CC: CC E: E))
	       result) ; end memorizeSegment function
	    ;; Segment the input array on the specified column.
	    (defun segment(X target)
	       vars:(WS S s ws n N m M)
	       ;; Determine the length and width of the observation array.
	       (setq N (length X))
	       (setq M (integer (- (length X[0]) 1)))
	       ;; Heuristically determine the number of segments.
	       (setq S (integer (min 10 (max 1 (/ N 5)))))
	       ;; Sort the training samples on the specified variable.
	       (sort X (let ((target target)) (lambda(_x _y) (<= _x[target] _y[target]))))
	       ;; Heuristically determine the number of segments.
	       (setq M (integer (/ N S)))
	       (setq WS (new Vector: S))
	       (loop for n from 0 until N do
	          (setq m (divi n M))
	          (if (= WS[m] #void) (setq WS[m] (new Vector:)))
	          (setq WS[m][(length WS[m])] X[n])
	          ) ; end loop		    
	       WS) ; end segment function
        ;; Define a standard estimator Lambda display function
        (defun show()
           vars:(result ca n N)
           ;; Append the training statistics to the model display
           (setq result (append "model neighbor: G=" Rf.G ",P=" Rf.P ",StdErr=" Rf.Error ",AvgErr=" Rf.AvgErr ",MinErr=" Rf.MinErr ",MaxErr=" Rf.MaxErr ",AvgY=" Rf.AvgY))
           result) ; end define a standard estimator Lambda display function
        ;; Test the estimator Lambda
        (defun test(X)
           vars:(nn NN m M i I j J k K CC M N ey y stdErr avgErr minErr maxErr avgY)
           ;; Compute the error statistics.
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq result (append "model neighbor: StdErr=" stdErr ",AvgErr=" avgErr ",MinErr=" minErr ",MaxErr=" maxErr ",AvgY=" avgY))
           result) ; end test function
        ;; ***********************************
        ;; Begin estimator Lambda main logic
        ;; ***********************************
        (setq M variableCount)
	    (setq NN (length Net))
	    ;; Find the nearest neighbors using the distance metric.
	    ;; Note: The nearest neighbor is the one which sorts to 
	    ;;       the front of the dNet Dictionary.
	    (if (<> v oldV)
	        (begin
	           (setq dNet (new Directory:))
		       (loop for i from 0 until NN do
		          (setq dy (distance v Net[i 0]))
		          (setq dNet[dy] Net[i 1])
		       ))) ; end find the nearest neighbor
	    ;; Compute the estimated value, ey, for the unknown dependent variable, y.
	    ;; Note: If the input vector has been observed during training,
	    ;;       then use the average value of all dependent variables
	    ;;       observed during training as the estimated value for y.
	    ;;       If the input vector has not been observed during training,
	    ;;       then use the average dependent value in the specified 
	    ;;       neighborhood as the estimated value for y.
	    (if (= dNet[0 0] 0)
	        then 
	        (setq ey dNet[0 1])
	        else
	        (begin
	           ;; Average the y values seen for the nearest neighbors.
			   (setq neighborhood (integer (min (length dNet) C[0])))
			   (setq dy 0)
			   (loop for i from 0 until neighborhood  do
			      (setq dy (+ dy dNet[i 1]))
			      ) ; end find the nearest neighbor
			   (if (<> neighborhood 0) (setq dy (/ dy neighborhood)) (setq dy avgY))
	           ;; Fill the estimate and error vectors.
			   (setq ey (new Vector: M))
			   (setq err (new Vector: M))
			   (loop for m from 0 until M do
			      (estimateSegment Clusters m v ey err)
			      ) ; end find the nearest neighbor
	           ;; Convert the error vector to relevence percentages.
	           (setq errTotal (sum err))
			   (setq err (math.vectorSub errTotal err))
	           (setq errTotal (sum err))
			   (setq err (math.vectorDivide err errTotal))
			   (setq ey (math.vectorDotProduct ey err))
			   (setq ey (avg ey dy))
			)) ; end if
        ;; Convert the dependent value using the requested output filter.
        (setq rangeY (- maxY minY))
        (if (= Filter sigmoid:) (if (= rangeY 0) (setq ey .5) (setq ey (/ (- ey minY) rangeY))))
        (if (= Filter bipolar:) (setq ey (if (>= ey avgY) 1 -1)))
        (if (= Filter binary:) (setq ey (if (>= ey avgY) 1 0)))
        (if (= Filter nbinary:) (setq ey (if (<= ey avgY) 1 0)))
        ;; Return the estimated dependent value.
        (setq oldC C[0])
        (setq oldV v)
        ey) }
        )) ; end define estimator Lambda
   ;; Set the coefficient vector to the optimal distance coefficient.
   (setq Rf.Filter filter)
   (setq Rf.variableCount variableCount)
   (setq Rf.C (new Vector: number: 1 3))
   (setq Rf.Rf Rf)
   Rf) ; end makeNeighbor




































;;**EXPORTKEY**:math:numericRegress:makeNeural
(defchild math.numericRegress makeNeural(variableCount filter ...)
;; *******************************************************************
;; summary:  Neural Net Regression:
;;            
;;           Return a neural net regression estimator Lambda for 
;;           use with numericRegress. 
;;
;;			 Notes:
;;				o A standard least squares objective function is used.
;;				o Estimator training data must be a single N x M+1 vector array.
;;				o Estimator input must be a single number vector.
;;			    
;; Parms:    variableCount:  The number of independent variables in the regression.
;;           filter:         The type of neural net output filter. Must be one of
;;                           (binary, bipolar, continuous, or sigmoid).
;;           hidden:         (Optional) The number of hidden layer neurons.
;;
;; Return:   Estimator       Always returns a neural net estimator Lambda.
;; *******************************************************************
   vars:(X Rf in i hidden)
   ;; Create a neural net estimator Lambda.
   ;; Note1: The estimator Lambda is a standard 
   ;;        neural net regression model, which has
   ;;        been modified to prevent overflow.
   ;; Note2: A standard least squares objective
   ;;        function is used.
   (setq Rf (eval {
     (lambda(v)
        pvars:(;; Public variables.
               C                    ; Coefficient vector
               Error           		; Final objective error after training
               AvgErr           	; Average objective error after training
               MinErr           	; Minimum objective error after training
               MaxErr           	; Maximum objective error after training
               AvgY              	; Average of dependent variable during training
               Filter				; My neural net output filter
               G                 	; Final generation count after training
               hiddenCount		    ; My neural net hidden variables
               Net					; My neural net object
               P                	; Final population count after training
               Rf              		; My estimator Lambda
               (Strategy neural)	; My optimization strategy
               variableCount		; Estimator Lambda input variables
               ;; Public child Lambdas.
               Check           		; Check the result for over or under flow
               Objective       		; Objective function to use in training
               ) ; end of persistant variables
        vars:(i j result M ey y)
        ;; Define a standard least square objective function
        ;; Note: The neural net objective function must also
        ;;       expose the neural net to the whole training
        ;;       set while scoring. 
        (defun Objective(X)
           vars:(i result M N y ey stdErr avgErr minErr maxErr avgY)
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
	          (propagateForward:Rf.Net X[i])
	          (propagateBackward:Rf.Net X[i])
              (setq stdErr (+ stdErr (/ (* (setq ey (- Rf.Net.output.outputs[0] (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end training loop
           (setq Rf.Error stdErr)
           (setq Rf.AvgErr avgErr)
           (setq Rf.MinErr minErr)
           (setq Rf.MaxErr maxErr)
           (setq Rf.AvgY avgY)
           Rf.Error) ; end define objective function
        ;; Define a standard estimator Lambda display function
        (defun show()
           vars:(result ca n N)
           (setq result (append "model coefficients = " (string C true)))
           ;; Append the training statistics to the model display
           (setq result (append result _eol "G=" Rf.G ",P=" Rf.P ",StdErr=" Rf.Error ",AvgErr=" Rf.AvgErr ",MinErr=" Rf.MinErr ",MaxErr=" Rf.MaxErr ",AvgY=" Rf.AvgY))
           result) ; end define a standard estimator Lambda display function
        ;; Test the estimator Lambda
        (defun test(X)
           vars:(nn NN m M i I j J k K CC M N ey y stdErr avgErr minErr maxErr avgY)
           ;; Compute the error statistics.
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq result (append "model neural: StdErr=" stdErr ",AvgErr=" avgErr ",MinErr=" minErr ",MaxErr=" maxErr ",AvgY=" avgY))
           result) ; end test function
        ;; ***********************************
        ;; Begin estimator Lambda main logic
        ;; ***********************************
        (propagateForward:Net v)
        (setq result Net.output.outputs[0])
        ;; Forceably prevent overflow
        (setq result (numCheck result))
        result) }
        )) ; end define estimator Lambda
   ;; Set the coefficient vector to the number of variables plus the constant coefficient.
   (if (= (argCount) 3) (setq hidden (argFetch 2)) (setq hidden (+ (* variableCount 2) 1)))
   (setq Rf.Filter filter)
   (setq Rf.variableCount variableCount)
   (setq Rf.hiddenCount hidden)
   (setq Rf.C (new Vector: number: 1 0))
   (setq Rf.Net (makeNeuralNet Rf.variableCount  Rf.hiddenCount 1 Rf.Filter))
   (clear:Rf.Net)
   (setq Rf.C (getWeights:Rf.Net))
   (setq Rf.Rf Rf)
   Rf) ; end makeNeural




































;;**EXPORTKEY**:math:numericRegress:makePolynomial
(defchild math.numericRegress makePolynomial(variableCount power filter ErrTollerance)
;; *********************************************************************
;; summary:  Induced Regression: (uses ad hoc Gaussian methods)
;; 
;;           Return a non-linear polynomial regression estimator Lambda
;;           for use with numericRegress. The estimator Lambda implements 
;;           a general non-linear polynomial regression model, which has
;;           been modified to prevent overflow.
;;
;;           This ad hoc regression technique attempts to reduce a
;;           non-linear problem to a linear polynomial regression
;;           where Gaussian methods can be applied.
;;
;;			 First, a general polynomial is computed, then it is raised
;;           to a power, and a filter is applied to the output. To find
;;           the polynomial regression coefficients, the filter and power
;;           are reversed in the dependent variable, Y. Now we have a linear
;;           relationship between X and Y and we can use Gaussian techniques.
;;           
;;           Warning: In order to be reversable, the power must be a positive,
;;           		  odd, integer. Since we need all the positive integers
;;                    to create the family of Nth order polynomials, from which
;;                    all other functions can be simulated, this method tends
;;                    to over fit all problems where an even integer would 
;;                    suffice to fit the data. The sigmoid output filter has
;;                    been selected so that it can be easily reversed. 
;;
;;			 Notes:
;;				o The objective function computes the error tollerant average
;;                absolute error (expressed as a percent of each dependent variable).
;;				o Estimator training data must be a single N x M+1 number matrix.
;;				o Estimator input must be a single number vector.
;;			    
;;			 Model:
;; 				p = expt( C[0]*X[0] + ... +  C[M-1]*X[M-1] , power )
;;              	(filter == continuous):   	y = p
;;              	(filter == sigmoid):   		y = exp(p) / (1 + exp(p))
;; 
;; Parms:    variableCount:  The number of independent variables in the regression.
;;           power:			 The power to which the polynomial is to be raised (a positive, odd integer).                           
;;           filter:         The output filter for the polynomial (continuous, or sigmoid).
;; 			 ErrTollerance:  The error tollerance limit to use in the regression.
;;
;; Return:   Estimator       Always returns an estimator Lambda.
;; *********************************************************************
   vars:(X Rf in i)
   ;; Create an polynomial estimator Lambda.
   ;; Note1: The estimator Lambda is a general 
   ;;        polynomial regression model, which has
   ;;        been modified to prevent overflow.
   ;; Note2: A standard least squares objective
   ;;        function is used.
   (setq Rf (eval {
     (lambda(v)
        pvars:(C                    ; Coefficient vector
               Error           		; Final objective error after training
               AvgErr           	; Average objective error after training
			   ErrTollerance   	    ; Error tollerance limit to use in the regression.
               MinErr           	; Minimum objective error after training
               MaxErr           	; Maximum objective error after training
               AvgY              	; Average of dependent variable during training
               Filter               ; Output filter for the polynomial
               Power                ; Power to which the polynomial is to be raised
               Fe                   ; Final function executions after training
               G               		; Final generation count after training
               Objective       		; Objective function to use in training
               P               		; Final population count after training
               Rf              		; My estimator Lambda
               (Strategy induce) 	; My optimization strategy
               ) ; end of persistant variables
        vars:(i cc ce result m M ey y p ep)
        ;; Define the objective function which computes the error tollerant 
		;; average absolute error (expressed as a percent of each dependent variable).
        ;; Note: X must be an N x M+1 number matrix, and Y must be void.
        (defun Objective(X Y)
           vars:(cv ey y m M n N ey y stdErr avgErr avgErrPct minErr maxErr avgY)
       	   (setq X (math.convertToMatrix X Y))
           (setq N (rank X)[0])
           (setq M (- (rank X)[1] 1))
       	   ;; Reverse the output filter and power operations on Y.
       	   ;; Note: This creates a linear relationship between X and Y.
       	   (setq Y (new Vector: number: N))
       	   (loop for n from 0 until N do
       	      (setq Y[n] (setq y X[n M]))
       	      ;; MFK ...Filter reversal not implemented yet...
       	      ;; Reverse Power operation.
       	      (setq X[n M] (if (< y 0) (- (expt (abs y) (/ Power))) (expt y (/ Power))))
       	      ) ; end Y loop 
	       ;; Compute the optimized coefficient vector.
	       ;; Note: The error is in the M+2 vector position.
	       (setq cv (math.matrixMultipleRegress X))
	       (setq Rf.C (resize cv M))
	       (setq Rf.P 1)
	       (setq Rf.G 1)
       	   ;; Restore the output column in the input matrix.
       	   ;; Note: This makes this non-destructive on X.
       	   (loop for n from 0 until N do
       	      (setq X[n M] Y[n])
       	      ) ; end Y loop 
           ;; Compute the average absolute error as a percentage of each Y.
           (setq avgErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (loop for n from 0 until N do 
              (setq ey (abs (- (Rf (math.matrixRowToVector X n M)) (setq y X[n M]))))
              (if (<> y 0) (setq ey (abs (/ ey y))))
              (setq ey (max 0 (- ey ErrTollerance)))
              (setq avgErr (+ avgErr (/ ey N)))
              (setq avgY (+ avgY (/ y N)))
              (setq minErr (min minErr ey))
              (setq maxErr (max maxErr ey))
              ) ; end loop
           (setq Rf.Error avgErr)
           (setq Rf.AvgErr avgErr)
           (setq Rf.MinErr minErr)
           (setq Rf.MaxErr maxErr)
           (setq Rf.AvgY avgY)
           Rf.Error) ; end define objective function
        ;; Define a standard estimator Lambda display function
        (defun show()
           vars:(result cc n N)
           (cond
             ((= Filter sigmoid:) (setq result "model: y = exp(p) / (1 + exp(p)), where p = "))
             (else (setq result "model: y = "))           
             ) ; end cond
           (setq N (length C))
           (loop for n from 0 until N do
              ;; + (Cc[n]*x[n])
              (if (isNegative C[n])
                  (setq result (append result " - (" (abs C[n]) "*" "x[" n "])"))
                  (setq result (append result " + (" C[n] "*" "x[" n "])"))
                  ) ; end sign if
              ) ; end loop
           (if (<> Power 1) (setq result (append result " : raised to the power of " Power)))   
           ;; Append the training statistics to the model display
           (setq result (append result _eol "G=" Rf.G ",P=" Rf.P ",Error=" Rf.Error ",ErrTollerance=" Rf.ErrTollerance ",MinErr=" Rf.MinErr ",MaxErr=" Rf.MaxErr ",AvgY=" Rf.AvgY))
           result) ; end define a estimator Lambda display function
        ;; Test the estimator Lambda
        ;; Note: X must be an N x M+1 number matrix.
        (defun test(X)
           vars:(nn NN m M i I j J k K CC M N ey y stdErr avgErr minErr maxErr avgY)
           ;; Compute the average absolute error as a percentage of each Y.
           (setq N (rank X)[0])
           (setq M (- (rank X)[1] 1))
           (setq avgErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (loop for n from 0 until N do 
              (setq ey (abs (- (Rf (math.matrixRowToVector X n M)) (setq y X[n M]))))
              (if (<> y 0) (setq ey (abs (/ ey y))))
              (setq ey (max 0 (- ey ErrTollerance)))
              (setq avgErr (+ avgErr (/ ey N)))
              (setq avgY (+ avgY (/ y N)))
              (setq minErr (min minErr ey))
              (setq maxErr (max maxErr ey))
              ) ; end loop
           (setq result (append "model polynomial: Error=" avgErr ",ErrTollerance=" ErrTollerance ",MinErr=" minErr ",MaxErr=" maxErr ",AvgY=" avgY))
           result) ; end test function
        ;; ********************************
        ;; Begin estimator Lambda main logic
        ;; ********************************
        ;; Compute the polynomial
        (setq M (length C))
        (setq p 0)
        (loop for m from 0 until M do 
           (setq p (+ p (* C[m] v[m])))
           ) ; end loop
        ;; Raise the polynomial to the requested power
        (setq ey p)
        (loop for n from 1 until Power do (setq ey (* ey p)))
        ;; Set the output, y, to the filtered polynomial
        (cond
          ;; Sigmoid filter
          ((= Filter sigmoid:) (setq y (/ (setq ep (exp ey)) (+ 1.0 ep))))
          ;; All others are treated as continuous filters
          (else (setq y ey))           
          ) ; end cond
        ;; Forceably prevent overflow
        (setq y (numCheck y))
        y) }
        )) ; end define estimator Lambda
   ;; Validate the power argument.
   (if (not (and (isInteger power) (isPositive power) (isOdd power))) (error "math.makePolynomial: power argument must be a positive, odd, integer"))
   ;; Validate the filter argument.
   (if (and (<> filter continuous:) (<> filter sigmoid:)) (error "math.makePolynomial: filter argument must be continuous or sigmoid"))
   ;; Set the coefficient vector to the number of variables plus the constant coefficient.
   (setq Rf.C (new Vector: number: (integer variableCount) 0))
   (setq Rf.Rf Rf)
   (setq Rf.Filter filter)
   (setq Rf.Power power)
   (setq Rf.ErrTollerance ErrTollerance)
   Rf) ; end makePolynomial



















;;**EXPORTKEY**:math:numericRegress:makeSVM
(defchild math.numericRegress makeSVM(variableCount kernel ErrTollerance maxSVSize)
;; *******************************************************************************
;; summary:  Support Vector Machine Regression:
;; 
;;           Return a support vector machine regression estimator Lambda 
;;           for use with numericRegress. The estimator Lambda implements 
;;           a support vector machine polynomial regression model, which has
;;           been modified to prevent overflow. The Support Vector Machine
;;           creates a weighted polynomial dual model based on the Gram
;;			 matrix of the observations.
;;
;;			 Notes:
;;				o The objective function computes the error tollerant average
;;                absolute error (expressed as a percent of each dependent variable).
;;				o Estimator training data must be an N x M vector array, X,
;;				  and an N number vector, Y.
;;				o Estimator input must be a single number vector.
;;			    
;;			 Model: 
;;				y = C[0]*kernel(X[0],Xs) + ... +  C[M-1]*kernel(X[M-1],Xs)
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
;;                  will not have enough data to get accurate results. (The Lambda
;;                  math.numericRegress.selfTest contains a sample test case which
;;                  demonstrates the fall in accuracy as Gaussian Sample Size is decreased). 
;; 
;;			 Note2: This model does NOT support a threshold constant at C[0].
;;                  The model assumes each coefficient C[m] is multiplied
;;                  by its paired dual model variable: kernel(X[0],Xs). 
;;
;;			 Hint:  A threshold constant at C[0] can be achieved by setting
;;				    X[0] equal to one for all rows in the training data.
;; 
;; Parms:    variableCount:  The number of independent variables in the regression.
;; 			 kernel:  		 The kernel function to use in the regression.
;; 			 ErrTollerance:  The error tollerance limit to use in the regression.
;;           maxSVSize  	 The Gaussian sample size (maximum number of support vectors to use during Gaussian initialization -- defaults to 100).
;;
;; Return:   Estimator       Always returns an estimator Lambda.
;; *******************************************************************************
   vars:(X Rf in i)
   ;; Create a support vector machine estimator Lambda.
   ;; Note1: The estimator Lambda uses a standard 
   ;;        support vector machine regression model, 
   ;;        which has been modified to prevent overflow.
   ;; Note2: The objective function computes the
   ;;        error tollerant average absolute error 
   ;;        (expressed as a percent of each dependent variable).
   (setq Rf (eval {
     (lambda(v)
        pvars:(C                    ; Coefficient weight vector for the SVM dual model
               Date           		; The date of the last training
               Error           		; Final objective error after training
               ErrTollerance 		; Error tollerance limit during training
               kernel		 		; Kernel function to use during training
               AvgErr           	; Average objective error after training
               MinErr           	; Minimum objective error after training
               MaxErr           	; Maximum objective error after training
               MaxSVSize           	; Maximum number of support vectors to use in Gaussian initialization.
               AvgY              	; Average of dependent variable during training
               (G 0)             	; Final generation count after training
               Objective       		; Objective function to use in training
               (P 0)             	; Final population count after training
               Rf              		; My estimator Lambda
               (Strategy svm)		; My optimization strategy
               variableCount		; Estimator Lambda input variables
               Xs		   			; Support Vectors after regression training
               ) ; end of persistant variables
        vars:(i j m M result ey y)
        ;; Define the objective function which computes the error tollerant 
		;; average absolute error (expressed as a percent of each dependent variable).
        ;; Note: X must be an N x M vector array, and Y must be an N number vector.
        (defun Objective(X Y)
           vars:(n N m M ey y avgErr minErr maxErr avgY)
           (setq avgErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq N (length X))
           (setq M (length X[0]))
           ;; Compute the average absolute error as a percentage of each Y.
           (loop for n from 0 until N do 
              (setq ey (abs (- (Rf X[n]) (setq y Y[n]))))
              (if (<> Y[n] 0) (setq ey (abs (/ ey Y[n]))))
              (setq ey (max 0 (- ey ErrTollerance)))
              (setq avgErr (+ avgErr (/ ey N)))
              (setq avgY (+ avgY (/ y N)))
              (setq minErr (min minErr ey))
              (setq maxErr (max maxErr ey))
              ) ; end loop
           (setq Rf.Error avgErr)
           (setq Rf.AvgErr avgErr)
           (setq Rf.MinErr minErr)
           (setq Rf.MaxErr maxErr)
           (setq Rf.AvgY avgY)
           Rf.Error) ; end define objective function
        ;; Define a standard estimator Lambda display function
        (defun show()
           vars:(result n N)
           (setq result (append "model: y = 0"))
           (setq N (length C))
           (loop for n from 0 until N do
              (if (isNegative C[n])
                  (setq result (append result " - (" (abs C[n]) "*" "kernel(x,Xs[" n "])"))
                  (setq result (append result " + (" C[n] "*kernel(x,Xs[" n "])"))
                  ) ; end sign if
              ) ; end loop
           ;; Append the training statistics to the model display
           (setq result (append result _eol "G=" Rf.G ",P=" Rf.P ",ETol=" Rf.ErrTollerance ",StdErr=" Rf.Error ",AvgErr=" Rf.AvgErr ",MinErr=" Rf.MinErr ",MaxErr=" Rf.MaxErr ",AvgY=" Rf.AvgY))
           result) ; end define a standard esitmator Lambda display function
        ;; Test the estimator Lambda
        ;; Note: X must be an N x M vector array, and Y must be an N number vector.
        (defun test(X Y)
           vars:(n N m M ey y avgErr minErr maxErr avgY)
           (setq avgErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq N (length X))
           (setq M (length X[0]))
           ;; Compute the average absolute error as a percentage of each Y.
           (loop for n from 0 until N do 
              (setq ey (abs (- (Rf X[n]) (setq y Y[n]))))
              (if (<> Y[n] 0) (setq ey (abs (/ ey Y[n]))))
              (setq ey (max 0 (- ey ErrTollerance)))
              (setq avgErr (+ avgErr (/ ey N)))
              (setq avgY (+ avgY (/ y N)))
              (setq minErr (min minErr ey))
              (setq maxErr (max maxErr ey))
              ) ; end loop
           (setq result (append "model svm: Error=" avgErr ",MinErr=" minErr ",MaxErr=" maxErr ",AvgY=" avgY))
           result) ; end test function
        ;; Begin estimator Lambda main logic
        (setq M (length C))
        (setq result 0)
        (loop for m from 0 until M do (setq result (+ result (* C[m] (kernel v Xs[m])))))
        ;; Forceably prevent overflow
        (setq result (numCheck result))
        result) }
        )) ; end define estimator Lambda
   ;; Set the coefficient vector to the number of variables plus the constant coefficient.
   (setq Rf.variableCount variableCount)
   (setq Rf.C (new Vector: number: variableCount))
   (setq Rf.ErrTollerance ErrTollerance)
   (setq Rf.kernel kernel)
   (setq Rf.MaxSVSize maxSVSize)
   (setq Rf.Rf Rf)
   Rf) ; end makeSVM




















;;**EXPORTKEY**:math:numericRegress:makeSinusoidal
(defchild math.numericRegress makeSinusoidal()
;; *******************************************************************
;; summary:  Genetic Evolutionary Regression: (real number genome)
;; 
;;           Return a sinusoidal regression estimator Lambda for 
;;           use with numericRegress. The estimator Lambda implements 
;;           a sinusoidal polynomial regression model, which has
;;           been modified to prevent overflow.
;;
;;			 Notes:
;;				o A standard least squares objective function is used.
;;				o Estimator training data must be a single N x M+1 vector array.
;;				o Estimator input must be a single number vector.
;;			    
;;			 Model:
;;              y = C[0]*X[1] + (C[1]*X[1])*sin(C[2]*X[0])
;; 
;; Parms:    variableCount:  The number of independent variables in the regression.
;;
;; Return:   Estimator       Always returns an estimator Lambda.
;; *******************************************************************
   vars:(X Rf in i (constantCount 3))
   ;; Create a sinusoidal estimator Lambda.
   ;; Note1: The estimator Lambda is a sinusoidal 
   ;;        polynomial regression model, which has
   ;;        been modified to prevent overflow.
   ;; Note2: A standard least squares objective
   ;;        function is used.
   (setq Rf (eval {
     (lambda(v)
        pvars:(C                    ; Coefficient vector
               Error           		; Final objective error after training
               AvgErr           	; Average objective error after training
               MinErr           	; Minimum objective error after training
               MaxErr           	; Maximum objective error after training
               AvgY              	; Average of dependent variable during training
               G               		; Final generation count after training
               Objective       		; Objective function to use in training
               P               		; Final population count after training
               Rf              		; My estimator Lambda
               (Strategy evolve) 	; My optimization strategy
               ) ; end of persistant variables
        vars:(i cc ce result M ey y)
        ;; Define a standard least square objective function 
        ;; Note: The observations must be a vector array.
        (defun Objective(X)
           vars:(i M N y ey stdErr avgErr minErr maxErr avgY)
           ;; Compute standard error statistics
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq Rf.Error stdErr)
           (setq Rf.AvgErr avgErr)
           (setq Rf.MinErr minErr)
           (setq Rf.MaxErr maxErr)
           (setq Rf.AvgY avgY)
           Rf.Error) ; end define objective function
        ;; Define a standard estimator Lambda display function
        (defun show()
           vars:(result ca n N)
           ;; Append the training statistics to the model display
           (setq result (append "model sinusoidal: G=" Rf.G ",P=" Rf.P ",StdErr=" Rf.Error ",AvgErr=" Rf.AvgErr ",MinErr=" Rf.MinErr ",MaxErr=" Rf.MaxErr ",AvgY=" Rf.AvgY))
           result) ; end define a standard estimator Lambda display function
        ;; Test the estimator Lambda
        (defun test(X)
           vars:(nn NN m M i I j J k K CC M N ey y stdErr avgErr minErr maxErr avgY)
           ;; Compute the error statistics.
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq result (append "model sinusoidal: StdErr=" stdErr ",AvgErr=" avgErr ",MinErr=" minErr ",MaxErr=" maxErr ",AvgY=" avgY))
           result) ; end test function
        ;; Begin estimator Lambda main logic
        (setq M (divi (length C) 2))
        (setq cc 1)
        (setq ce 2)
        (setq result (+ (* C[0] v[1]) (* C[1] v[1] (sin (* C[2] v[0])))))
        ;; Forceably prevent overflow
        (setq result (numCheck result))
        result) }
        )) ; end define estimator Lambda
   ;; Set the coefficient vector to the number of constant coefficients.
   (setq Rf.C (new Vector: number: constantCount 0))
   (setq Rf.Rf Rf)
   Rf) ; end makeSinusoidal







































;;**EXPORTKEY**:math:numericRegress:makeSplitNeural
(defchild math.numericRegress makeSplitNeural(variableCount filter ...)
;; *******************************************************************
;; summary:  Neural Net Regression:
;;            
;; 	 		 Return a neural net regression estimator Lambda for 
;;           use with numericRegress. A split training methodology
;;           splits the training set, during regression, into sample A(80%),
;;           and sample B(20%). All neural backpropagation training is
;;           done on sample A; while, all error statistics are run against
;;           sample B.
;;
;;           Note: 	This is an implementation of "out of sample training", and
;;            		helps prevent over fitting (which can plague neural nets).
;;
;;			 Note:
;;				o A standard least squares objective function is used.
;;				o Estimator training data must be a single N x M+1 vector array.
;;				o Estimator input must be a single number vector.
;;			    
;; Parms:    variableCount:  The number of independent variables in the regression.
;;           filter:         The type of neural net output filter. Must be one of
;;                           (binary, bipolar, continuous, or sigmoid).
;;           hidden:         (Optional) The number of hidden layer neurons.
;;
;; Return:   Esitmator       Always returns a neural net estimator Lambda.
;; *******************************************************************
   vars:(X Rf in i hidden)
   ;; Create a neural net estimator Lambda.
   ;; Note1: The estimator Lambda is a standard 
   ;;        neural net regression model, which has
   ;;        been modified to prevent overflow.
   ;; Note2: A standard least squares objective
   ;;        function is used.
   (setq Rf (eval {
     (lambda(v)
        pvars:(;; Public variables.
               C                    ; Coefficient vector
               Error           		; Final objective error after training
               AvgErr           	; Average objective error after training
               MinErr           	; Minimum objective error after training
               MaxErr           	; Maximum objective error after training
               AvgY              	; Average of dependent variable during training
               Filter				; My neural net output filter
               G                 	; Final generation count after training
               hiddenCount		    ; My neural net hidden variables
               Net					; My neural net object
               P                	; Final population count after training
               Rf              		; My estimator Lambda
               (Strategy neural)	; My optimization strategy
               variableCount		; Estimator Lambda input variables
               ;; Public child Lambdas.
               Check           		; Check the result for over or under flow
               Objective       		; Objective function to use in training
               ) ; end of persistant variables
        vars:(i j result M ey y)
        ;; Define a standard least square objective function
        ;; Note: The neural net objective function must also
        ;;       expose the neural net to the whole training
        ;;       set while scoring. 
        (defun Objective(X)
           vars:(i n result M N y ey stdErr avgErr minErr maxErr avgY)
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq n 1)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
		   ;; A split training methodology splits the training set, during regression, 
		   ;; into sample A(80%), and sample B(20%). All neural backpropagation training is
           ;; done on sample A; while, all error statistics are run against sample B.
           (loop for i from 0 until N do
	          (propagateForward:Rf.Net X[i])
              (if (<> (modi i 5) 4)
                  ;; We train the neuralNet on test data in sample A.
                  (begin 
	                 (propagateBackward:Rf.Net X[i])
	              ) ; end sample A training
                  ;; We compute the neuralNet error on test data in sample B.
                  (begin 
                     (setq stdErr (+ stdErr (* (setq ey (- Rf.Net.output.outputs[0] (setq y X[i][M]))) ey)))
		             (setq avgY (+ avgY (setq y X[i][M])))
		             (setq avgErr (+ avgErr (abs ey)))
		             (setq minErr (min minErr (abs ey)))
		             (setq maxErr (max maxErr (abs ey)))
	                 (++ n)
	              )) ; end sample B error computation
              ) ; end training loop
           (setq Rf.Error (/ stdErr n))
           (setq Rf.AvgErr (/ avgErr n))
           (setq Rf.MinErr minErr)
           (setq Rf.MaxErr maxErr)
           (setq Rf.AvgY (/ avgY n))
           Rf.Error) ; end define objective function
        ;; Define a standard estimator Lambda display function
        (defun show()
           vars:(result ca n N)
           ;; Append the training statistics to the model display
           (setq result (append "model splitNeural: G=" Rf.G ",P=" Rf.P ",StdErr=" Rf.Error ",AvgErr=" Rf.AvgErr ",MinErr=" Rf.MinErr ",MaxErr=" Rf.MaxErr ",AvgY=" Rf.AvgY))
           result) ; end define a standard estimator Lambda display function
        ;; Test the estimator Lambda
        (defun test(X)
           vars:(nn NN m M i I j J k K CC M N ey y stdErr avgErr minErr maxErr avgY)
           ;; Compute the error statistics.
           (setq stdErr 0)
           (setq minErr BIGPOSNUM)
           (setq maxErr 0)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (loop for i from 0 until N do 
              (setq stdErr (+ stdErr (/ (* (setq ey (- (Rf X[i]) (setq y X[i][M]))) ey) N)))
              (setq avgY (+ avgY (/ y N)))
              (setq avgErr (+ avgErr (/ (abs ey) N)))
              (setq minErr (min minErr (abs ey)))
              (setq maxErr (max maxErr (abs ey)))
              ) ; end loop
           (setq result (append "model splitNeural: StdErr=" stdErr ",AvgErr=" avgErr ",MinErr=" minErr ",MaxErr=" maxErr ",AvgY=" avgY))
           result) ; end test function
        ;; ***********************************
        ;; Begin estimator Lambda main logic
        ;; ***********************************
        (propagateForward:Net v)
        (setq result Net.output.outputs[0])
        ;; Forceably prevent overflow
        (setq result (numCheck result))
        result) }
        )) ; end define estimator Lambda
   ;; Set the coefficient vector to the number of variables plus the constant coefficient.
   (if (= (argCount) 3) (setq hidden (argFetch 2)) (setq hidden (+ (* variableCount 2) 1)))
   (setq Rf.Filter filter)
   (setq Rf.variableCount variableCount)
   (setq Rf.hiddenCount hidden)
   (setq Rf.C (new Vector: number: 1 0))
   (setq Rf.Net (makeNeuralNet Rf.variableCount  Rf.hiddenCount 1 Rf.Filter))
   (setq Rf.C (getWeights:Rf.Net))
   (setq Rf.Rf Rf)
   Rf) ; end makeSplitNeural




































;;**EXPORTKEY**:math:numericRegress:makeTailExponential
(defchild math.numericRegress makeTailExponential(variableCount tailPct)
;; *******************************************************************
;; summary:  Genetic Evolutionary Regression: (real number genome)
;; 
;; 			 Return an exponential tail estimator Lambda for 
;;           use with numericRegress.  The estimator Lambda implements 
;;           an exponential polynomial regression model, which has
;;           been modified to prevent overflow.
;;
;; 			 Note: This objective function trys to maximize its best top N% and 
;;       		   to minimize its bottom N%. The product, not the sum, of the
;;       		   actual scores is used to force more wins/losses into each tail
;;       		   during the training process.
;;
;;			 Notes:
;;				o A standard least squares objective function is used.
;;				o Estimator training data must be a single N x M+1 vector array.
;;				o Estimator input must be a single number vector.
;;			    
;;			 Model:
;; 				y = C[0] + C[1]*expt(X[0],C[2]) + ... +  C[2M]*expt(X[M-1],C[2M+1])
;; 
;; Parms:    variableCount:  The number of independent variables in the regression.
;;           tailPct:        The percent of independent variables, top and bottom tail,
;;                           to be optimized.
;;
;; Return:   Estimator       Always returns an exponential estimator Lambda.
;; *******************************************************************
   vars:(X Rf in i)
   ;; Create an exponential estimator Lambda.
   ;; Note1: The estimator Lambda is an exponential 
   ;;        polynomial regression model, which has
   ;;        been modified to prevent overflow.
   ;; Note2: A standard least squares objective
   ;;        function is used.
   (setq Rf (eval {
     (lambda(v)
        pvars:(C                    ; Coefficient vector
               Error           		; Final objective error after training
               AvgErr           	; Average objective error after training
               MinErr           	; Minimum objective error after training
               MaxErr           	; Maximum objective error after training
               AvgY              	; Average of dependent variable during training
               G               		; Final generation count after training
               MaxEY             	; Final average score for the top est. tail after training
               MaxY             	; Final average score for the top tail after training
               MinEY             	; Final average score for the top est. tail after training
               MinY             	; Final average score for the top tail after training
               Objective       		; Objective function to use in training
               P               		; Final population count after training
               Rf              		; My estimator Lambda
               (Strategy evolve) 	; Percent of independent variables in the either tail.
               TailPct 			    ; tailPct
               ) ; end of persistant variables
        vars:(i cc ce result M ey y)
        ;; Define a tail maximize/minimize objective function.
        ;; Note: This objective function trys to maximize its best top 10% and 
        ;;       to minimize its bottom 10%. The product, not the sum, of the
        ;;       actual scores is used to force more wins/losses into each tail
        ;;       during the training process.
        (defun Objective(X)
           vars:(n result err ey y K M N estY realY maxY maxEY minY minEY dB dT)
           (setq result 1)
           (setq M (- (length X[0]) 1))
           (setq N (length X))
           (setq K (integer (* N Rf.TailPct)))
           ;; Compute the average value of the top/bottom tails of the dependent variable.
           (setq realY (new Vector: number: N))
           (loop for n from 0 until N do (setq realY[n] X[n][M]))
           (setq estY (sort realY < true))
		   (setq maxY 0)
           (loop for n from (subi N K) until N do (setq maxY (+ maxY X[realY[n]][M])))
		   (setq maxY (/ maxY K))           
		   (setq minY 0)
           (loop for n from 0 until K do (setq minY (+ minY X[realY[n]][M])))
		   (setq minY (/ minY K))           
           ;; Compute the average value of the top/bottom tails of the est. dependent variable.
           (setq estY (new Vector: number: N))
           (loop for n from 0 until N do (setq estY[n] (Rf X[n])))
           (setq estY (sort estY < true))
		   (setq maxEY 0)
           (loop for n from (subi N K) until N do (setq maxEY (+ maxEY X[estY[n]][M])))
		   (setq maxEY (/ maxEY K))           
		   (setq minEY 0)
           (loop for n from 0 until K do (setq minEY (+ minEY X[estY[n]][M])))
		   (setq minEY (/ minEY K))           
           ;; Compute the average error value for the top/bottom tails between the est. & actual dependent variable.
           (setq Rf.MinEY minEY)
           (setq Rf.MinY minEY)
           (setq Rf.MaxEY maxEY)
           (setq Rf.MaxY maxY)
           (setq dB (* (- minEY minY) (- minEY minY)))
           (setq dT (* (- maxEY maxY) (- maxEY maxY)))
           (setq result (sqrt (+ dB dT)))
           result) ; end define objective function
        ;; Define a standard estimator Lambda display function
        (defun show()
           vars:(result cc ce  n N)
           (setq result (append "model: y = " C[0]))
           (setq N (divi (length C) 2))
	       (setq cc 1)
	       (setq ce 2)
           (loop for n from 0 until N do
              ;; + (Cc[n]*expt(x[n],Ce[n])
              (if (isNegative C[cc])
                  (setq result (append result " - (" (abs C[cc]) "*" "expt(x[" n "]," C[ce] "))"))
                  (setq result (append result " + (" C[cc] "*" "expt(x[" n "]," C[ce] "))"))
                  ) ; end sign if
              (setq cc (addi cc 2))
              (setq ce (addi ce 2))
              ) ; end loop
           ;; Append the training statistics to the model display
           (setq result (append result _eol "G=" Rf.G ",P=" Rf.P ",StdErr=" Rf.Error ",AvgErr=" Rf.AvgErr ",MinErr=" Rf.MinErr ",MaxErr=" Rf.MaxErr ",AvgY=" Rf.AvgY))
           result) ; end define a estimator Lambda display function
        ;; Begin estimator Lambda main logic
        (setq M (divi (length C) 2))
        (setq cc 1)
        (setq ce 2)
        (setq result C[0])
        (loop for i from 0 until M do 
           (setq result (+ result (expt (* C[cc] v[i]) C[ce])))
           (setq cc (addi cc 2))
           (setq ce (addi ce 2))
           ) ; end loop
        ;; Forceably prevent overflow
        (setq result (numCheck result))
        result) }
        )) ; end define estimator Lambda
   ;; Set the coefficient vector to the number of variables times 2 plus the constant coefficient.
   (setq Rf.C (new Vector: number: (integer (+ (* variableCount 2) 1)) 0))
   (setq Rf.TailPct tailPct)
   (setq Rf.Rf Rf)
   Rf) ; end makeTailExponential







































;;**EXPORTKEY**:math:numericRegress:numberEvolution
(defriend math.numericRegress numberEvolution(X Y Rf Gmax err)
;; ******************************************************************************************
;; summary:  Genetic Evolutionary Regression: optimize the estimator 
;; 			 coefficients for the specified estimator Lambda using
;; 			 the training data supplied, and a real number genome.
;;
;;           Returns the input estimator function with coefficients optimized 
;;           against the specified objective function. The regression may
;;           be linear of nonlinear and will minimize any objective function.
;;           Neither the estimator function nor the objective function 
;;           are required to be continuous.
;;
;; 			 Notes:
;;				o Perferred Data Format:  X ==> N x M+1 vector array, Y ==> #void.
;; 			 	o This Lambda assumes that the coefficient vector is an number vector. 
;;
;; Parms:    X:       The N by M+1 vector array representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;                    Note: Where each of the row vectors must be Number vectors.
;;           Y:       (Optional)The N number vector containing the dependent variables
;;                    in the form of:    	  y
;;                                            y
;;                                           ... 
;;                                            y
;;                    Note: If Y is NOT #void, the X will not contain the y terms.
;;           Rf:      The estimator function which maps a number vector, of length M,
;;                    into a single real number, Rf: Vm --> R, whose coefficients
;;                    are to be optimized. The estimator function is in the form of
;;                    an Lambda, (Rf mVector) ==> aNumber, and the coefficients
;;                    to be optimized are stored as a number vector Rf.C 
;;                    in the persistant variables of the Rf Lambda.
;;                    Note: The estimator function, Rf, must be an Lambda containing
;;                          the following persistant (pvars) variables:
;;                          C             An number vector containing the number coefficients to optimize
;;                          Error         The final score from the objective function 
;;                          G             The final generation count when optimization halted
;;                          Objective     The objective function which maps an N by M+1 observation 
;;                                        matrix, X, using it's parent estimator function, Rf, into 
;;                                        a single positive real error value. The objective function 
;;                                        is in the form of a child Lambda, (Rf.Objective X) ==> anErrorNumber.
;;                                        The numericRegress Lambda attempts to optimize the estimator
;;                                        function coefficients, Rf.C, such that the objective 
;;                                        function's resulting error value is minimized. 
;;                          P             The final population count when optimization halted 
;;                          Strategy      The optimization strategy to use:
;;                                        #void				Use genetic algorithm  
;;                                        evolve: 			Use genetic algorithm (real number genome)  
;;                                        evolveBinary: 	Use genetic algorithm (binary genome)  
;;                                        evolveInteger: 	Use genetic algorithm (integer genome)  
;;                                        linear:	    	Use multivariate linear regression  
;;                                        neural:	    	Use neural net regression  
;;           Gmax:    The maximum number of optimization trials (generations) to attempt before
;;                    returning the best set of coefficients available at that time.
;;           err:     A minimum error value which would terminate further optimization 
;;                    trials and return the best set of coefficients at that time.
;;
;; Return:   Rf:      The estimator function with coefficients Rf.C optimized.
;; 
;; Note:     See Genetic Algorithms by John Holland.
;; ******************************************************************************************
    pvars:(;; Private child methods
           initCoefficients        ;; Initialize cvPopulation with a random coefficient vector population.
           migrateCoefficients     ;; Migrate the fittest coefficient vectors between distinct populations.
           mutateCoefficients      ;; Mutate the fittest coefficient vectors in the general population.
           recombineCoefficients   ;; Recombine the fittest coefficient vectors in the general population.
           scorePopulation         ;; Score the entire cvPopulation coefficient vector population.
           ;; Private Variables
           (learningRate .25)      ;; The learning rate during each optimization trail.
           (migrationRate .05)     ;; The migration rate between population before starting the next optimization trail.
           (maxPopulationAge 10)   ;; The maximum age of any distinct population for any optimization trail.
           (maxPopulations 3)      ;; The maximum number of distinct populations for any optimization trail.
           (maxSurvivors 20)       ;; The maximum survivors from a population before starting the next optimization trail.
           (minPopulation 50)      ;; The minimum population, of cvPopulation, before attempting an optimization trail.
           ) ;; end of persistent variables
    vars:(i naCount)
    ;; *******************************************************************
    ;; Define Private Child Lambdas
    ;; *******************************************************************
    ;; Initialize cvPopulation with a random coefficient vector population.
    (defun initCoefficients(X Rf)
       vars:(i cv currentPop iC)
       ;; Initialize all system parameters.
       (if (= randomSW true) (setq _random ^random) (setq _random ^srandom))
       ;; The population count increases as the generation count increases.
       (setq populationCount (integer (min (++ populationCount) maxPopulations)))
       ;; Initialize coefficients for each distinct population.
       (setq iC Rf.C)
       (setq cvPopulation (new Vector:))
       (setq cvError (new Vector:))
       (setq cvStop (new Vector:))
       (setq populationAge 0)
       (loop for currentPop from 0 until populationCount do
          (setq cvPopulation[currentPop] (new Structure:))
          (setq cvError[currentPop] "NA")
          (setq cvStop[currentPop] false)
          ;; Create a set of random coefficient vectors.
          (while (< (length cvPopulation[currentPop]) minPopulation) do
             (setq cv (new Vector: number: Cn))
             (loop for i from 0 until Cn do
                (setq cv[i] (- (_random 2.0) 1.0))
                (if (and (> (_random 1.0) .5) (<> cv[i] 0)) (setq cv[i] (/ 1.0 cv[i])))
                ) ; end loop
             (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) cv "NA")
             ) ; end while loop
          ) ; end currentPop loop
       (if (isVector iC) (insert cvPopulation[0] (length cvPopulation[0]) iC "NA"))
       true) ; end initCoefficients
    ;; Score the entire cvPopulation coefficient vector population.
    (defun migrateCoefficients()
       vars:(i j currentPop CBaby CScore)
       ;; Migrate coefficients between each distinct population.
       (if (<= migrationRate 0) (return true))
       (loop for currentPop from 0 until populationCount do
          (if (= cvStop[currentPop] true) (goto SkipThisPopulation:))
          (loop for j from 0 until populationCount do
             (if (and (<> j currentPop) (< (_random 1.0) migrationRate))
                 (begin
		            ;; Migrate a coefficient vectors between populations
		            (setq CBaby (copy cvPopulation[j][0 0]))
		            (setq CScore cvPopulation[j][0 1])
                    (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) CBaby CScore)
                    )) ; end if
             ) ; end j loop
          SkipThisPopulation::
          ) ; end currentPop loop
       true) ; end scorePopulation
    ;; Mutate the coefficients of each of the survivors.
    (defun mutateCoefficients()
       vars:(i j jm start end temp C0 C1 C2 C3 C4 C5 C6 C7 C8 currentPop delta err r (MRate 1))
       ;; Mutate coefficients for each distinct population.
       (loop for currentPop from 0 until populationCount do
          (if (= cvStop[currentPop] true) (goto SkipThisPopulation:))
          ;; Mutate a survivor coefficient vector one chromosome segment at a time.
          (loop for r from 0 until MRate do
	         (setq i (integer (_random (* .99999 (length cvSurvivors[currentPop])))))
	         ;; Mutate each coefficient vector higher and lower by section.
	         (if (= (setq err cvSurvivors[currentPop][i 1]) "NA") (setq err (_random 1.0)))
	         (setq C0 (copy cvSurvivors[currentPop][i 0]))
	         (setq C1 (copy cvSurvivors[currentPop][i 0]))
	         (setq C2 (copy cvSurvivors[currentPop][i 0]))
	         (setq C3 (copy cvSurvivors[currentPop][i 0]))
	         (setq start (integer (max (_random Cn) Cn1)))
	         (setq end (integer (max (_random Cn) Cn1)))
	         (loop for jm from start to end do
	            (setq j (modi jm Cn))
	            (setq C0[j] (+ C0[j] (* err learningRate C0[j])))
	            (setq C1[j] (- C1[j] (* err learningRate C1[j])))
	            (setq C2[j] (* C2[j] .5))
	            (setq C3[j] (* C3[j] 2))
	            ) ; end jm loop
             (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) C0 "NA")
             (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) C1 "NA")
             (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) C2 "NA")
             (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) C3 "NA")
             ) ; end mutation loop
          ;; Retrieve the top survivor coefficient vector error score
	      (if (= (setq err cvSurvivors[currentPop][0 1]) "NA") (setq err (_random 1.0)))
          ;; Mutate (hill creeping) a survivor coefficient vector one coefficient at a time
          ;; Note: This heurism increases linearly with the number of coefficients.
          (loop for r from 0 until MRate do
	         ;; Mutate each coefficient vector higher and lower one coefficient at a time.
	         (loop for j from 0 until Cn do
	            (setq C1 (copy cvSurvivors[currentPop][0 0]))
	            (setq C2 (copy cvSurvivors[currentPop][0 0]))
	            (setq C3 (copy cvSurvivors[currentPop][0 0]))
	            (setq delta (- (_random 1.0) .5))
	            (setq C1[j] (+ C1[j] (* C1[j] delta)))
	            (setq C2[j] (- C2[j] (* err learningRate C2[j])))
	            (setq C3[j] (+ C3[j] (* err learningRate C3[j])))
                (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) C1 "NA")
                (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) C2 "NA")
                (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) C3 "NA")
	            ) ; end j loop
             ) ; end mutation loop
          ;; Mutate (hill climbing) a survivor coefficient vector all coefficients at once
          ;; Note: This heurism moves the genome, all coefficients, in both directions.
          (loop for r from 0 until MRate do
	         ;; Mutate each coefficient vector higher and lower all coefficients at once.
	         (setq C1 (copy cvSurvivors[currentPop][0 0]))
	         (setq C2 (copy cvSurvivors[currentPop][0 0]))
	         (loop for j from 0 until Cn do
	            (setq C1[j] (- C1[j] (* err learningRate C1[j])))
	            (setq C2[j] (+ C2[j] (* err learningRate C2[j])))
	            ) ; end j loop
             (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) C1 "NA")
             (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) C2 "NA")
             ) ; end mutation loop
          SkipThisPopulation::
          ) ; end currentPop loop
       true) ; end mutateCoefficients
    ;; Recombine the coefficients of each of the survivors.
    (defun recombineCoefficients()
       vars:(mon pop i j jm start end temp CMom CPop currentPop delta r (HRate 1) (CRate 3))
       ;; Recombine coefficients from pairs of coefficient vectors in each distinct population.
       (loop for currentPop from 0 until populationCount do
          (if (= cvStop[currentPop] true) (goto SkipThisPopulation:))
          ;; Recombine (hill climb average) the best two coefficient vectors in the survivor population
          ;; Note: This heurism increases linearly with the number of coefficients.
          (loop for r from 0 until HRate do
	          (loop for j from 0 until Cn do
	             (setq CMom (copy cvSurvivors[currentPop][0 0]))
	             (setq CPop (copy cvSurvivors[currentPop][1 0]))
	             (setq delta (* (_random 1.0) (- CMom[j] CPop[j])))
	             (if (<> delta 0)
	                 (begin 
	                    (setq CMom[j] (+ CMom[j] delta))    
	                    (setq CPop[j] (- CPop[j] delta))    
                        (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) CMom "NA")
                        (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) CPop "NA")
                     )) ; end if
	             ) ; end j loop
              ); end hill climbing loop
	     ;; Recombine (crossover) two random coefficient vectors in the survivor population
         (loop for r from 0 until CRate do
	          (setq mom (setq pop 0))          
	          (while (= mom pop) do  
	             (setq mom (integer (_random (* .99999 (length cvSurvivors[currentPop])))))
	             (setq pop (integer (_random (* .99999 (length cvSurvivors[currentPop])))))
	             ) ; end while
	          (setq CMom (copy cvSurvivors[currentPop][mom 0]))
	          (setq CPop (copy cvSurvivors[currentPop][pop 0]))
	          (setq start (integer (max (_random Cn) Cn1)))
	          (setq end (integer (max (_random Cn) Cn1)))
	          (loop for jm from start to end do
	             (setq j (modi jm Cn))
	             (setq CMom[j] cvSurvivors[currentPop][pop 0][j])
	             (setq CPop[j] cvSurvivors[currentPop][mom 0][j])
	             ) ; end jm loop
              (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) CMom "NA")
              (insert cvPopulation[currentPop] (length cvPopulation[currentPop]) CPop "NA")
              ); end crossover loop
          SkipThisPopulation::
          ) ; end currentPop loop
       true) ; end recombineCoefficients
    ;; Score the entire cvPopulation coefficient vector population.
    (defun scorePopulation(Rf X)
       vars:(i currentPop score)
       ;; Score the coefficients for each distinct population.
       (setq forceNewPopulation true)
       (setq cvSurvivors (new Vector:))
       (loop for currentPop from 0 until populationCount do
          (if (= cvStop[currentPop] true) (goto SkipThisPopulation:))
          ;; Score all coefficient vectors in the current population
          (loop for i from 0 until (length cvPopulation[currentPop]) do
             (if (= cvPopulation[currentPop][i 1] "NA")
                 (begin
                    (setq Rf.C cvPopulation[currentPop][i 0])
                    (setq score (Rf.Objective X))
                    ;; Assign score to population only if it is not a floating point error of some kind.
                    (if (= (+ score 1) score)
                        ;; Remove the coefficient vector if the score is a floating point error of some kind.
                        ;(begin (delete cvPopulation[currentPop] cvPopulation[currentPop][i 0]) (setq i (subi i 1)))
                        (setq cvPopulation[currentPop][i 1] "NA")
                        ;; Set the coefficient vector's score in the population otherwise.
                        (setq cvPopulation[currentPop][i 1] score)
                        ) ; end if
                 )) ; end if
             ) ; end loop
          ;; Reduce the current population to only the most fit survivors
          (sort cvPopulation[currentPop] < byValue:)
          (while (< cvPopulation[currentPop][0 1] 0) do (delete cvPopulation[currentPop] 0)) 
          (resize cvPopulation[currentPop] maxSurvivors)
          (setq cvSurvivors[currentPop] (copy cvPopulation[currentPop]))
          (if (> cvError[currentPop] cvSurvivors[currentPop][0 1])
              then
              (begin
                 (setq cvError[currentPop] cvSurvivors[currentPop][0 1]) 
                 (setq forceNewPopulation false)
                 (if (> Error cvSurvivors[currentPop][0 1])
                     (begin
                        (setq Error cvSurvivors[currentPop][0 1]) 
                        (setq C cvSurvivors[currentPop][0 0])
                        ;(writeln "Selecting Best of Breed G=[" generationCount "] P=[" currentPop "] E=[" Error "]")
                     )) ; end if
                 ) ; end then
              else
              (setq cvStop[currentPop] true)
              ) ; end if
          SkipThisPopulation::
          ) ; end currentPop loop
       true) ; end scorePopulation
    ;; *******************************************************************
    ;; Begin Genetic Algorithm Logic Section
    ;; *******************************************************************
    (setq X (math.convertToArray X Y))
    (setq Rf (copy Rf))
    (setq C Rf.C)
    (setq Error "NA")
    (setq Cn (length Rf.C))
    (setq Cn1 (subi Cn 1))
    (setq N (length X))
    (setq Mp1 (length X[0]))
    (setq M (subi Mp1 1))
    (setq Mm1 (subi M1 1))
    (setq naCount 0)
    (setq generationCount 0)
    (setq populationCount populationStart)
    NewPopulation::
    (initCoefficients X Rf)
    (scorePopulation Rf X)
    (setq Rf.C C)
    (setq Rf.Error Error)
    NewGeneration::
    (if (= Error "NA")
        (begin
           (++ naCount) 
           (if (<= naCount 10) 
               (goto NewPopulation:)
               (goto StopTraining:)
               ) ; end inner if
        )) ; end "NA" if
    (setq naCount 0)
    (if (= forceNewPopulation true) (goto NewPopulation:))
    (setq generationCount (addi generationCount 1))
    (setq populationAge (addi populationAge 1))
    (mutateCoefficients)
    (recombineCoefficients)
    (scorePopulation Rf X)
    (migrateCoefficients)
    (setq Rf.C C)
    (setq Rf.Error Error)
    (if (or (<= Error err) (>= generationCount Gmax)) (goto StopTraining:))
    (if (>= populationAge (+ maxPopulationAge (divi generationCount maxPopulationAge))) (goto NewPopulation:))
    (goto NewGeneration:)
    ;; Return the optimized estimator function Lambda.
    StopTraining::
    (setq Rf.C C)
    (setq Rf.Error Error)
    (setq Rf.G generationCount)
    (setq Rf.P populationCount)
    Rf) ; end numberEvolution





















;;**EXPORTKEY**:math:percentileGridMachine
(deforphan math:percentileGridMachine(...)
;; *******************************************************************
;; summary:  Returns a new percentile grid machine Lambda ready for training.
;;
;;           Percentile grid machines are quasi regression engines which
;;           learn and make regression estimates on XY vector arrays such as:
;;
;;               XY:  An N by M+1 array representing the original observations
;;                    in the form of:    x x x ...  y
;;                                       x x x ...  y
;;                                           ... 
;;                                       x x x ...  y
;;
;;           The percentile grid machine constructs a percentile grid, of the
;;           XY training data. The grid contains Bayesian equal probabalistic
;;           percentile views of all possible N-way cross correlations of the 
;;           independent columns in XY. Each row in the percentile grid contains
;;           exactly 100 entries representing the average Y values for each
;;           percentile of the specified N-way cross correlation of independent
;;           variables.
;;
;;           The percentile grid machine is designed to provide either 1-way,
;;           2-way, 3-way, or 4-way column cross correlations. The PGM automatically
;;           selects the largest number of columns to cross correlate which it can
;;           fit into available resources. The percentile grid size is determined
;;           by the number of independent columns, MX, and the number of columns
;;           chosen for cross correlation, C. The number of rows in the percentile
;;           grid is always equal to expt(MX,C). 
;;            
;;           Percentile grid machines operate on entire vector arrays 
;;           at once. For this reason percentile grid machines are not
;;           strictly regression engines, and therefore are not included
;;           in the math.numericRegress Lambda's tool set.
;;
;;
;; Parms:    MGrid	 The number of cols in each row of the percentile training grid (default is 100 for percentile subdivisions).
;;
;; Return:   Rf:     A new percentile grid machine Lambda ready for training.
;; *******************************************************************
  pvars:(;; Public Variables
         C                      ;; The number of columns to cross correlate for each percentile grid row (must be either 1, 2 ,3, or 4).
         Grid					;; The percentile training grid for this machine.
         MX                     ;; The number of independent variables.
         MXY					;; The number of independent and dependent variables.
         MGrid      			;; The number of cols in each row of the percentile training grid for this machine (default is 100 for percentile subdivisions).
         NGrid                  ;; The number of rows in the percentile training grid for this machine, always equal to expt(MX,C).
         NX                     ;; The number of training examples.
         (Strategy PGM)         ;; The regression strategy used by this Lambda.
         ;; Public Child Lambdas
         inputArray     		;; Crunch input vector array into a dense sigmoid XY training vector array.
         inputCursor    		;; Crunch cursor input into a dense sigmoid XY training vector array.
         resetMachine			;; Reset this percentile grid machine (clears all existing training memory).
         runMachine			    ;; Run this percentile grid machine on the XY sample vector array.
         trainMachine			;; Train this percentile grid machine on the XY training vector array.
         ) ; end persistant variables
  vars:(Pgm)
  ;; *******************************************************************************
  ;; Define Public Child Lambdas 
  ;; *******************************************************************************
  ;; Crunch input vector array into a dense sigmoid XY training vector array. 
  ;; Parms:    XY:      The N by M+1 array representing the original observations
  ;;                    in the form of:    x x x ...  y
  ;;                                       x x x ...  y
  ;;                                           ... 
  ;;                                       x x x ...  y
  ;;           Y:       (Optional) The N vector containing the dependent values.
  (defun inputArray(X ...)
     vars:(m M n N XY minX maxX rngX Y rowXY temp)
     ;; Convert the arguments into an XY vector array.
     (if (= (argCount) 2) (setq Y (argFetch 1)))
     (setq XY (math.convertToArray X Y))
     (setq MXY (length XY[0]))
     (setq MX (sub1 MXY))
     (setq NX (length XY))
     (setq minX (new Vector: number: MX BIGPOSNUM))  
     (setq maxX (new Vector: number: MX BIGNEGNUM))  
     (setq rngX (new Vector: number: MX))
     ;; Compute the independent column min and max values.
     (loop for n from 0 until NX do
        (setq rowXY (refObjVector XY n))
        (loop for m from 0 until MX do
           (if (< (setq temp (refNumVector rowXY m)) (refNumVector minX m)) (setNumVector minX m temp))
           (if (> temp (refNumVector maxX m)) (setNumVector maxX m temp))
           (setNumVector rngX m (- (refNumVector maxX m) (refNumVector minX m)))
           ) ; end MX loop 
        ) ; end NX loop
    ;; Crunch the raw XY data into dense sigmoid form.
    (loop for n from 0 until NX do
        (setq rowXY (refObjVector XY n))
        (loop for m from 0 until MX do
           (if (> (setq temp (refNumVector rngX m)) 0)
               (setNumVector rowXY m (/ (- (refNumVector rowXY m) (refNumVector minX m)) (refNumVector rngX m)))
               (setNumVector rowXY m 0.0)
               ) ; end if
           ) ; end MX loop 
        ) ; end NX loop
     XY) ; end inputArray
  ;; Crunch cursor input into a dense sigmoid XY training vector array.
  ;; Note: The SpecialSituationNotes field of the cursor is used as a scratch pad. 
  (defun inputCursor(cursor baseExpressions expressionVector)
     vars:(k K m M n N XY command record)
     ;; Collect the field expressions arguments.
     (setq MXY (length expressionVector))
     (setq MX (sub1 MXY))
     (if (not (isVector baseExpressions)) (setq baseExpressions (new Vector: 1 baseExpressions)))
     (setq K (length baseExpressions)) 
     (loop for k from 0 until K do (cursor.run baseExpressions[k]))
     (setq NX cursor.recordCount)
     (setq XY (new Vector: object: NX))  
     (setq command (append "setnr SpecialSituationNotes new('Vector','number'," MXY)) 
     (loop for m from 0 until MXY do (setq command (append command "," expressionVector[m])))
     (setq command (append command ");"))
     ;; Extract raw data from cursor into XY vector array.
     ;; Note: also compute column min and max values.
     (cursor.run command)
     (loop for n from 0 until NX do
        (setq record (refObjVector cursor.rowVector n)) 
        (setObjVector XY n record.SpecialSituationNotes)
        (setq record.SpecialSituationNotes #void)
        ) ; end NX loop
     XY) ; end inputCursor
  ;; Reset this percentile grid machine (clears all existing training memory).
  (defun resetMachine()
     (setq C #void)
     (setq Grid #void)
     (setq MX #void)
     (setq MXY #void)
     (setq NGrid 0)
     (setq NX #void)
     (setq Strategy PGM:)
     true) ; end resetMachine
  ;; Run this percentile grid machine on the XY sample vector array.
  ;; Note1: May be run on a vector array or a table cursor, as follows.
  ;;        (Option 1)  (runMachine XY {gridVector})
  ;;        (Option 2)  (runMachine X Y {gridVector})
  ;;        (Option 3)  (runMachine cursor baseExpressions expressionVector {gridVector})
  ;; Note2: If a table cursor is passed as an argument, the SpecialSituationNotes field 
  ;;        of the table cursor will contain the new dependent variable, Yest, estimates.
  ;; Note3: An optional gridVector argument provides an object vector of percentile grids
  ;;        each of which will have an equal vote on the estimator values.
  (defun runMachine(X ...)
     vars:(m m1 m2 m3 m4 M n N g G XY
           Xscore Yest W index sortNdx 
           record rowTemp gridVector 
           Y cursor baseExpressions expressionVector
           ) ; end temporary variables
     ;; Collect the arguments.
     (cond
      ;; Are we training on a table cursor?
      ((isLambda X) 
       (begin
          (setq cursor X)
          (setq baseExpressions (argFetch 1))
          (setq expressionVector (argFetch 2))
          (if (= (argCount) 4) (setq gridVector (argFetch 3)) (setq gridVector (new Vector: object: 1 Grid)))
          (setq XY (inputCursor cursor baseExpressions expressionVector))
          (setq XY (inputArray XY))
       )) ; end table cursor case
      ;; Are we training on an XY vector array?
      (else 
       (begin
          (if (= (argCount) 2) (setq Y (argFetch 1)))
          (if (= (argCount) 3) (setq gridVector (argFetch 2)) (setq gridVector (new Vector: object: 1 Grid)))
          (setq XY (inputArray X Y))
       )) ; end vector array case
      ) ; end arguments cond
     ;; Use the percentile grid, constructed during training,
     ;; to produce an estimate for the dependent variable, Yest.
     ;; Note: The percentile grid is yet another vector array
     ;;       whose number vector rows contain exactly 100 
     ;;       elements which represent the percentile views
     ;;       of the data.
     (setq G (length gridVector))
     (setq Yest (new Vector: number: NX))
     (cond
      ;; Manage case where percentile grid is one column deep.
      ((= C 1)
       (begin
         (setq W (/ NX MGrid))
         (loop for m from 0 until MX do
           (setq Xscore (new Vector: number: NX))
           (loop for n from 0 until NX do (setNumVector Xscore n (refNumVector (refObjVector XY n) m)))
           (setq Xscore (sort Xscore < true))
           (loop for n from 0 until NX do
              (setq index (integer (* MGrid (/ n NX))))
              (setq sortNdx (refIntVector Xscore n)) 
              (loop for g from 0 until G do (setNumVector Yest sortNdx (+ (refNumVector Yest sortNdx) (/ (refNumVector (refObjVector (refObjVector gridVector g) m) index) NGrid G))))
              ) ; end NX loop
           ) ; end MX loop
       )) ; end one column case
      ;; Manage case where percentile grid is two columns deep.
      ((= C 2)
       (begin
         (setq W (/ NX MGrid))
         (setq m 0)
         (loop for m1 from 0 until MX do
            (loop for m2 from 0 until MX do
	           (setq Xscore (new Vector: number: NX))
	           (loop for n from 0 until NX do (setNumVector Xscore n (+ (integer (* 10 (refNumVector (setq rowTemp (refObjVector XY n)) m1))) (refNumVector rowTemp m2))))
	           (setq Xscore (sort Xscore < true))
	           (loop for n from 0 until NX do
	              (setq index (integer (* MGrid (/ n NX))))
	              (setq sortNdx (refIntVector Xscore n)) 
                  (loop for g from 0 until G do (setNumVector Yest sortNdx (+ (refNumVector Yest sortNdx) (/ (refNumVector (refObjVector (refObjVector gridVector g) m) index) NGrid G))))
	              ) ; end NX loop
	           (setq m (addi m 1))
               ) ; end m2 loop
           ) ; end m1 loop
       )) ; end two columns case
      ;; Manage case where percentile grid is three columns deep.
      ((= C 3)
       (begin
         (setq W (/ NX MGrid))
         (setq m 0)
         (loop for m1 from 0 until MX do
            (loop for m2 from 0 until MX do
               (loop for m3 from 0 until MX do
		           (setq Xscore (new Vector: number: NX))
		           (loop for n from 0 until NX do (setNumVector Xscore n (+ (* 10 (integer (* 5 XY[n][m1]))) (integer (* 5 XY[n][m2])) XY[n][m3])))
		           (setq Xscore (sort Xscore < true))
		           (loop for n from 0 until NX do
		              (setq index (integer (* MGrid (/ n NX))))
		              (setq sortNdx (refIntVector Xscore n)) 
		              (loop for g from 0 until G do (setNumVector Yest sortNdx (+ (refNumVector Yest sortNdx) (/ (refNumVector (refObjVector (refObjVector gridVector g) m) index) NGrid G))))
		              ) ; end NX loop
		           (setq m (addi m 1))
                   ) ; end m3 loop
               ) ; end m2 loop
           ) ; end m1 loop
       )) ; end three columns case
      ;; Manage case where percentile grid is four columns deep.
      ((= C 4)
       (begin
         (setq W (/ NX MGrid))
         (setq m 0)
         (loop for m1 from 0 until MX do
            (loop for m2 from 0 until MX do
               (loop for m3 from 0 until MX do
                   (loop for m4 from 0 until MX do
			           (if (= Grid[m] #void) (setq Grid[m] (new Vector: number: MGrid)))
			           (setq Xscore (new Vector: number: NX))
			           (loop for n from 0 until NX do (setNumVector Xscore n (+ (* 100 (integer (* 3 XY[n][m1]))) (* 10 (integer (* 3 XY[n][m2]))) (integer (* 3 XY[n][m3])) XY[n][m4])))
			           (setq Xscore (sort Xscore < true))
			           (loop for n from 0 until NX do
			              (setq index (integer (* MGrid (/ n NX))))
			              (setq sortNdx (refIntVector Xscore n)) 
			              (loop for g from 0 until G do (setNumVector Yest sortNdx (+ (refNumVector Yest sortNdx) (/ (refNumVector (refObjVector (refObjVector gridVector g) m) index) NGrid G))))
			              ) ; end NX loop
			           (setq m (addi m 1))
                       ) ; end m4 loop
                   ) ; end m3 loop
               ) ; end m2 loop
           ) ; end m1 loop
       )) ; end four columns case
      ) ; end cond
     ;; Are we running the machine on a table cursor?
     ;; Note: Score the cursor in the SpecialSituationNotes field.
     (if (isLambda cursor)
         (begin
		     (loop for n from 0 until NX do
		       (setq record (refObjVector cursor.rowVector n))
		       (setq record.SpecialSituationNotes (refNumVector Yest n))
		       ) ; end cursor loop 
         )) ; end table cursor if
     Yest) ; end runMachine
  ;; Train this percentile grid machine on an XY training vector array.
  ;; Note1: May be cummulatively trained on multiple training data arrays 
  ;;        as long as the percentile grid machine is not reset between trainings.
  ;; Note2: May be trained on a vector array or a table cursor, as follows.
  ;;        (Option 1)  (trainMachine XY)
  ;;        (Option 2)  (trainMachine X Y)
  ;;        (Option 3)  (trainMachine cursor baseExpressions expressionVector)
  ;; Note3: If a table cursor is passed as an argument, the SpecialSituationNotes field 
  ;;        of the table cursor is used as a scratch pad during the input processing. 
  (defun trainMachine(X ...)
     vars:(m m1 m2 m3 m4 M n N XY
           Xscore W index Y rowXY rowGrid rowTemp
           cursor baseExpressions expressionVector
           ) ; end temporary variables
     ;; Collect the arguments.
     (cond
      ;; Are we training on a table cursor?
      ((isLambda X) 
       (begin
          (setq cursor X)
          (setq baseExpressions (argFetch 1))
          (setq expressionVector (argFetch 2))
          (setq XY (inputCursor cursor baseExpressions expressionVector))
          (setq XY (inputArray XY))
       )) ; end table cursor case
      ;; Are we training on an XY vector array?
      (else 
       (begin
          (if (= (argCount) 2) (setq Y (argFetch 1)))
          (setq XY (inputArray X Y))
       )) ; end vector array case
      ) ; end arguments cond
     ;; Build the percentile grid for this training data.
     ;; Note1: The percentile grid is yet another vector array
     ;;        whose number vector rows contain exactly 100 
     ;;        elements which represent the percentile views
     ;;        of the data.
     ;; Note2: Although the number of percentile grid columns
     ;;        defaults to 100, the value of the MGrid variable
     ;;        may be altered to reflect a non-percentile size.
     ;; Note3: The heurism used to set the value of C, executed
     ;;        below, attempts to keep the maximum number of
     ;;        percentile grid rows at or below 4096.
     (setq C (cond ((<= MX 1) 1) ((<= MX 2) 2) ((<= MX 3) 3) ((<= MX 8) 4) ((<= MX 16) 3) ((<= MX 64) 2) (else 1)))
     (setq NGrid (integer (expt MX C)))
     (cond
      ;; Manage case where percentile is one column deep.
      ((= C 1)
       (begin
         (setq W (/ NX MGrid))
         (if (= Grid #void) (setq Grid (new Vector: object: NGrid)))
         (loop for m from 0 until MX do
           (if (= Grid[m] #void) (setq Grid[m] (new Vector: number: MGrid)))
           (setq Xscore (new Vector: number: NX))
           (loop for n from 0 until NX do (setNumVector Xscore n (refNumVector (refObjVector XY n) m)))
           (setq Xscore (sort Xscore < true))
           (setq rowGrid (refObjVector Grid m)) 
           (loop for n from 0 until NX do
              (setq index (integer (* MGrid (/ n NX))))
              (setNumVector rowGrid index (+ (refNumVector rowGrid index) (/ (refNumVector (refObjVector XY (refIntVector Xscore n)) MX) W)))
              ) ; end NX loop
           ) ; end MX loop
       )) ; end one column case
      ;; Manage case where percentile is two columns deep.
      ((= C 2)
       (begin
         (setq W (/ NX MGrid))
         (if (= Grid #void) (setq Grid (new Vector: object: NGrid)))
         (setq m 0)
         (loop for m1 from 0 until MX do
            (loop for m2 from 0 until MX do
	           (if (= Grid[m] #void) (setq Grid[m] (new Vector: number: MGrid)))
	           (setq Xscore (new Vector: number: NX))
	           (loop for n from 0 until NX do (setNumVector Xscore n (+ (integer (* 10 (refNumVector (setq rowTemp (refObjVector XY n)) m1))) (refNumVector rowTemp m2))))
	           (setq Xscore (sort Xscore < true))
	           (setq rowGrid (refObjVector Grid m)) 
	           (loop for n from 0 until NX do
	              (setq index (integer (* MGrid (/ n NX)))) 
	              (setNumVector rowGrid index (+ (refNumVector rowGrid index) (/ (refNumVector (refObjVector XY (refIntVector Xscore n)) MX) W)))
	              ) ; end NX loop
	           (setq m (addi m 1))
                ) ; end m2 loop
           ) ; end m1 loop
       )) ; end two columns case
      ;; Manage case where percentile is three columns deep.
      ((= C 3)
       (begin
         (setq W (/ NX MGrid))
         (if (= Grid #void) (setq Grid (new Vector: object: NGrid)))
         (setq m 0)
         (loop for m1 from 0 until MX do
            (loop for m2 from 0 until MX do
               (loop for m3 from 0 until MX do
		           (if (= Grid[m] #void) (setq Grid[m] (new Vector: number: MGrid)))
		           (setq Xscore (new Vector: number: NX))
		           (loop for n from 0 until NX do (setNumVector Xscore n (+ (* 10 (integer (* 5 (refNumVector (setq rowTemp (refObjVector XY n)) m1)))) (integer (* 5 (refNumVector rowTemp m2))) (refNumVector rowTemp m3))))
		           (setq Xscore (sort Xscore < true))
		           (setq rowGrid (refObjVector Grid m))
		           (loop for n from 0 until NX do
		              (setq index (integer (* MGrid (/ n NX)))) 
		              (setNumVector rowGrid index (+ (refNumVector rowGrid index) (/ (refNumVector (refObjVector XY (refIntVector Xscore n)) MX) W)))
		              ) ; end NX loop
		           (setq m (addi m 1))
                   ) ; end m3 loop
               ) ; end m2 loop
           ) ; end m1 loop
       )) ; end three columns case
      ;; Manage case where percentile is four columns deep.
      ((= C 4)
       (begin
         (setq W (/ NX MGrid))
         (if (= Grid #void) (setq Grid (new Vector: object: NGrid)))
         (setq m 0)
         (loop for m1 from 0 until MX do
            (loop for m2 from 0 until MX do
               (loop for m3 from 0 until MX do
                   (loop for m4 from 0 until MX do
			           (if (= Grid[m] #void) (setq Grid[m] (new Vector: number: MGrid)))
			           (setq Xscore (new Vector: number: NX))
			           (loop for n from 0 until NX do (setNumVector Xscore n (+ (* 100 (integer (* 3 (refNumVector (setq rowTemp (refObjVector XY n)) m1)))) (* 10 (integer (* 3 (refNumVector rowTemp m2)))) (integer (* 3 (refNumVector rowTemp m3))) (refNumVector rowTemp m4))))
			           (setq Xscore (sort Xscore < true))
			           (setq rowGrid (refObjVector Grid m))
			           (loop for n from 0 until NX do
			              (setq index (integer (* MGrid (/ n NX)))) 
			              (setNumVector rowGrid index (+ (refNumVector rowGrid index) (/ (refNumVector (refObjVector XY (refIntVector Xscore n)) MX) W)))
			              ) ; end NX loop
			           (setq m (addi m 1))
                       ) ; end m4 loop
                   ) ; end m3 loop
               ) ; end m2 loop
           ) ; end m1 loop
       )) ; end four columns case
      ) ; end cond
     Grid) ; end trainMachine
  ;; *******************************************************************************
  ;; Begin Main Logic 
  ;; *******************************************************************************
  (setq Pgm (new (myself)))
  (Pgm.resetMachine)
  (if (>= (argCount) 1) (setq Pgm.MGrid (argFetch 0)) (setq Pgm.MGrid 100))
  Pgm) ; end percentileGridMachine












;;**EXPORTKEY**:math:projectRegress
(defriend math:projectRegress(cursor strategyList)
;; *******************************************************************
;; summary:  Correlates the strategies in a dataMineLib miner project.
;;           Returns the sparse M coefficient vector for the strategies 
;;           with the best least squares fit of the variables.
;;           The strategy list is used to construct an N by M+1 array
;;           where the M+1 column represents the dependent variable
;;           (the last strategy named in the field list).
;;           
;; Example:  w:         The N by M+1 array representing the field list
;;                      in the form of:    x x ... x y
;;                                         x x ... x y
;;                                             ... 
;;                                         x x ... x y
;;
;; Parms:    cursor:       A memory cursor from which the N by M+1 array will be extracted.
;;           strategyList: The field list with which the N by M+1 array will be extracted.
;;                         (Note: the strategy list must contain strategies from the Name column).
;; Return:   vc:           The M+1 coefficient vector (0 = constant term, and e = M+1 term).
;; Note:     The cursor IS restored before the regression array is extracted.
;; 
;; Globals:  dataMineLib
;; *******************************************************************
    vars:(vc w m M n N start end record strategySearch)
    ;; Extract the least squares error from multiple linear regressions
    (setq start (add1 (member Cnt: cursor.colVector)))
    (setq end (length cursor.colVector))
    (setq N (- end start))
    (setq M+1 (length strategyList))
    (setq M (subi M+1 1))
    ;; Extract the N by M+1 array for use in multiple regression.
    (setq w (new Vector: N))
    (loop for m from 0 until M+1 do
       (cursor.restore)
       (setq strategySearch (append {all Name == '} strategyList[m] {'}))
       (cursor.run strategySearch)
       (if (<> cursor.recordCount 1) (error (append "projectRegress received an unknown strategy [" strategyList[m] "]")))
       (setq record cursor.rowVector[0])
       (loop for n from 0 until N do
          (if (= w[n] #void) (setq w[n] (new Vector: M+1)))
          (setq w[n][m] record[(+ start n)])
          ) ; end N loop
       ) ; end M loop
    ;; Perform a multiple regression on the extracted array.
    (setq vc (multipleRegress w))
    ;; Return the regression coefficient vector.
    vc) ; end projectRegress







































































;;**EXPORTKEY**:math:projectToArray
(defriend math:projectToArray(cursor strategyList)
;; *******************************************************************
;; summary:  Returns a regression array from the cursor and project
;;           strategies specified.
;;
;;           The strategy list is used to construct an N by M+1 array
;;           where the M+1 column represents the dependent variable
;;           (the last strategy named in the field list).
;;           
;; Example:  w:         The N by M+1 array representing the field list
;;                      in the form of:    x x ... x y
;;                                         x x ... x y
;;                                             ... 
;;                                         x x ... x y
;;
;; Parms:    cursor:       A memory cursor from which the N by M+1 array will be extracted.
;;           strategyList: The field list with which the N by M+1 array will be extracted.
;;                         (Note: the strategy list must contain strategies from the Name column).
;; Return:   w:            The regression array with the dependent variable as the last column.
;; Note:     The cursor IS restored before the regression array is extracted.
;; 
;; Globals:  dataMineLib
;; *******************************************************************
    vars:(vc w m M n N start end record strategySearch)
    ;; Extract the least squares error from multiple linear regressions
    (setq start (add1 (member Cnt: cursor.colVector)))
    (setq end (length cursor.colVector))
    (setq N (- end start))
    (setq M+1 (length strategyList))
    (setq M (subi M+1 1))
    ;; Extract the N by M+1 array for use in multiple regression.
    (setq w (new Vector: N))
    (loop for m from 0 until M+1 do
       (cursor.restore)
       (setq strategySearch (append {all Name == '} strategyList[m] {'}))
       (cursor.run strategySearch)
       (if (<> cursor.recordCount 1) (error (append "projectToArray received an unknown strategy [" strategyList[m] "]")))
       (setq record cursor.rowVector[0])
       (loop for n from 0 until N do
          (if (= w[n] #void) (setq w[n] (new Vector: M+1)))
          (setq w[n][m] record[(+ start n)])
          ) ; end N loop
       ) ; end M loop
    ;; Return the regression coefficient vector.
    w) ; end projectToArray







































































;;**EXPORTKEY**:math:regress
(defriend math:regress(x y)
;; *******************************************************************
;; name:     regress
;; 
;; summary:  Returns a vector containing the coefficients resulting
;;           from a linear regression on two vectors of equal length. 
;;           If x and y are vectors of equal length,
;;           then (regression x y) is: #(a  b  e).
;;           where a + bx = y represents the least squares best fit
;;           extracted from a comparison of the two vectors. The term
;;           e is the error ä sqr(y - (a + bx)).
;; Parms:    x:       A vector of length N.
;;           y:       A vector of length N.
;; Return:   v:       The vector #(a b e)
;; *******************************************************************
    vars:(m n v vt xmean ymean numerator denominator)
    (setq v (new Vector: 3))
    (setq n (length x))
    (setq m (length y))
    (if (<> n m)
        (error 
           (append  "regress: vectors not the same length"
                    " (length x)=" n
                    " (length y)=" m
                    " x=" x 
                    " y=" y)))
    (begin
       (setq xmean (avg x))
       (setq ymean (avg y))
       (setq numerator (vectorDotProduct (setq vt (vectorSub x xmean)) y)) 
       (setq denominator (vectorDotProduct vt vt))
       (if (= denominator 0)
           (setq v[1] 0)
           (setq v[1] (/ numerator denominator)))
       (setq v[0] (- ymean (* v[1] xmean)))
       (setq vt (vectorSub (vectorAdd (vectorProduct x v[1]) v[0]) y)) 
       (setq v[2] (vectorDotProduct vt vt)))
    v) ; regress












































































;;**EXPORTKEY**:math:regressNet
(defriend math:regressNet(w)
;; *******************************************************************
;; summary:  Returns an Lambda using the coefficients from several 
;;           segmented multiple regressions.
;; Parms:    w:       The N by M+1 array representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;; Return:   Lambda:   An Lambda (Lambda(X) ==> y) expecting an X input vector, returning y.
;;           Note:    The input vector, X, is a row of the w array, and
;;                    the return value, y, is the Lambda's best prediction for y.
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    vars:(result E vc wt v vx vy vr vs i I m M n N WS s S C CC target)
    ;; *******************************************************************
    ;; Define Child Lambdas
    ;; *******************************************************************
    ;; Segment the input array on the specified column.
    (defun segment(w target)
       vars:(C S s ws n N m M)
       ;; Don't segment if there are insufficient traning samples.
       (if (<= (length w) 30) (return (new Vector: 1 w)))
       (if (<= (length w) (* 2 (length w[0]))) (return (new Vector: 1 w)))
       ;; Sort the training samples on the specified variable.
       (sort w (let ((target target)) (lambda(_x _y) (<= _x[target] _y[target]))))
       ;; Heuristically determine the number of segments.
       (setq S (integer (min 10 (/ (length w) 30))))
       (setq C (new Vector: S))
       ;; Heuristically determine the number of segments.
       (setq N (length w))
       (setq M (integer (/ N S)))
       (loop for n from 0 until N do
          (setq m (divi n M))
          (if (= C[m] #void) (setq C[m] (new Vector:)))
          (setq C[m][(length C[m])] w[n])
          ) ; end loop
       C) ; end segment
    ;; *******************************************************************
    ;; Main Logic Section
    ;; *******************************************************************
    ;; Create the initial result Lambda and check the input array.
    (if (or (= w #void) (<= (length w) 0)) (return (new Vector:)))
    (setq result { 
                  (lambda(X) 
                     pvars:((CC false) (C false) (target 0) (E 0))
                     vars:(m M n N result)
                     (loop for n from 0 until (length CC) do (if (<= X[target] CC[n]) (goto NextStep:)))
                        NextStep::
                        (setq M (length C[n]))
                        (setq result C[n][0])
                        (loop for m from 1 until M do (setq result (+ result (* X[(sub1 m)] C[n][m]))))
                        result)
                  })
    ;; Segment the input array.
    (setq target 0)
    (setq WS (segment w target))
    (setq S (length WS))
    (setq CC (new Vector: S))
    (setq E (new Vector: (add1 S)))
    (setq C (new Vector: (add1 S)))
    ;; Compute the coefficient vectors from multiple linear regressions.
    (loop for s from 0 until S do
       (setq CC[s] WS[s][(sub1 (length WS[s]))][target])
       (setq vc (multivariableRegressC WS[s]))
       (setq M (subi (length vc) 1))
       (setq E[s] vc[M])
       (setq C[s] (resize vc M))
       ) ; 
    (setq CC[S] CC[(sub1 S)])
    (setq E[S] E[(sub1 S)])
    ;; Compile and return the final trained result Lambda.
    Last::
    (setq result (eval result))
    (setq result.target target)
    (setq result.C C)
    (setq result.CC CC)
    (setq result.E E)
    result) ; end regressNet





































;;**EXPORTKEY**:math:regressNetBinaryRegress
(defriend math:regressNetBinaryRegress(w T S Z)
;; *******************************************************************
;; summary:  Returns an Lambda using the coefficients from several 
;;           segmented multiple regressions.
;; Parms:    w:       The N by M+1 array representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;           T:       The column index of the target variable to segment at the top level.
;;           S:       The maximum number of splits to segment the target variable.
;;           Z:       The minimum number of elements in each segment of the target variable
;; Return:   Lambda:   An Lambda (Lambda(X) ==> y) expecting an X input vector, returning y.
;;           Note:    The input vector, X, is a row of the w array, and
;;                    the return value, y, is the Lambda's best prediction for y.
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    vars:(result Root)
    pvars:( ;; Public child methods
           Segment   ;; Segment the observations along one column into two equal halves
           Split     ;; Split the observations at one node into two child nodes
           ;; Public variables 
           Nodes     ;; Vector of nodes for binary tree
           s         ;; Number of splits so far
           Trace     ;; Set verbose mode if true
          )
    ;; *******************************************************************
    ;; Define Child Lambdas
    ;; *******************************************************************
    
    ; Segment the observations along one column into two equal halves.
    ; Returns boundary value between half the observations
    (defun Segment(w T Z)
       vars:(b h m M2 n N N2 v ws x)
       
       ; Don't segment if there are insufficient training samples.
       (setq N (length w))         ; N = Number of observations
       (setq N2 (divi (add1 N) 2)) ; N2 = half of N
       (setq M2 (* 2 (sub1 (length w[0]))))   ; M twice the number of parameters
       (if (< N Z)  (begin (display "Segment: N is less than " Z  " samples." _eol)  (return 0.0)))
       (if (< N M2) (begin (display "Segment: N is less than " M2 " rows." _eol)  (return 0.0)))
       ; Sort the training samples on the specified variable.
       (sort w (let ((T T)) (lambda(_x _y) (<= _x[T] _y[T]))))

       ; Divide the observations into two matrices, ws[0], ws[1]
       (setq ws (new Vector: 2))
       (setq ws[0] (new Vector: 1))        ; Lower half of the observations
       (setq ws[1] (new Vector: 1))        ; Upper half of the observations
       (setq h w[N2][T])            ; h = column value at halfway point
       ; Set boundary value when past halfway and boundary value has changed
       ; Note: if all of the values are the same, they all go into lower half
       (loop for n from N2 until N do
           (setq v w[n])
           (setq x v[T])         ; x = next column value
           (if  (> x h)
               (begin
                   (setq b (/ (+ v[T] h) 2))
                   (goto Quit:)
               )
           )
       )
       ; No split found, just use 0
       (setq b 0.0)
       
       Quit::
       (return b)
    )
    ;; Split - Splits the observations at one node into two child nodes
    ;; w - observations for this node
    ;; i - Index into Nodes vector
    ;; T - Select a specific column (iff positive)
    ;; Z - Minimum number of observations per segment
    ;; Returns - set of nodes in Nodes
    (defun Split(w i T S Z)
        vars:(b did1 e emin left m M n N right vl vu wl wu)

        (setq N (length w))              ; Number of observations in w
        (setq M (sub1 (length w[0])))    ; Number of parameters in w
        (setq wl (new Vector: 1))        ; Holds observations for lower half of w
        (setq wu (new Vector: 1))        ; Holds observations for upper half of w
        (setq emin 1.0E+40)              ; Minimum LS error over all columns
        (setq did1 false)                ; Split at least one column flag
        (if (<= M 0) (return #void))

        ; Split along each column
        (loop for m from 0 until M do
            (if (>= T 0) (setq m T))     ; Force top-level column
            (setq b (Segment w m Z))
            ; Skip if Segment fails
            (if (<= b 0) (begin (goto Skip:)))

            ; Fill wl with observations at or below b, wu with observations above b
            (resize wl 0)
            (resize wu 0)
            (loop for n from 0 until N do
                (if (<= w[n][m] b)
                    (setq wl[(length wl)] w[n])
                    (setq wu[(length wu)] w[n])
                )
            )
            (if (or (< (length wl) Z) (< (length wu) Z)) (begin (goto Skip:)))

            ; Do a regression on each half.
            (setq vl (multivariableRegressC wl))
            (setq vu (multivariableRegressC wu))
            (setq e (+ vl[(1+ M)] vu[(1+ M)]))   ; Total error for both segments
            (if (< e emin) (begin
                    (setq emin e)
                    (setq did1 true)
                    (setq Nodes[i].column m)
                    (setq Nodes[i].boundary b)
                    (setq Nodes[i].lower vl)
                    (setq Nodes[i].upper vu)
            ))
        Skip::
            (if (>= T 0) (goto Break:))          ; Just one iteration if top level
        )
        Break::
        ; If not able to split this node, it is a leaf node. Compute best coeffs for this leaf
        (if (or (not did1) (>= s S)) (begin
            (setq vl (multivariableRegressC w))
            (setq Nodes[i].lower vl)
            (setq Nodes[i].column -1)
            (return #void)
        ))
        ; Now create nodes for each child and split them
        (setq left (length Nodes))
        (setq n (new Node:))
        (setq n.left 0)
        (setq n.right 0)
        (setq Nodes[left] n)
        (setq Nodes[i].left left)

        (setq right (length Nodes))
        (setq m (new Node:))
        (setq m.left 0)
        (setq m.right 0)
        (setq Nodes[right] m)
        (setq Nodes[i].right right)

        ; Split w on the best column
        (setq b Nodes[i].boundary)
        (setq m Nodes[i].column)
        (if Trace (display "Splitting node=" i ", column=" m ", boundary=" b _eol))
        (if (<> ^ColUsed #void) (++ ^ColUsed[m]))
        (++ s)
        (resize wl 0)
        (resize wu 0)
        (loop for n from 0 until N do
            (if (<= w[n][m] b)
                (setq wl[(length wl)] w[n])
                (setq wu[(length wu)] w[n])
            )
        )
        (Split wl left  -1 S Z)
        (Split wu right -1 S Z)
        (return #void)
    )
    ;; *****************************************************************************
    ;; Main Logic Section
    ;; Binary Regression Tree Version
    ;;     Divide observations at top node into 2 halves along one column.  Repeat
    ;;     for all of the other columns.  Pick the column with the least total error.
    ;;     Repeat the above process for each node until less than Z samples per node.
    ;;     Save split column number and boundary at each intermediate node.  Save
    ;;     coefficients at each leaf node.
    ;;     Build a prediction Lambda that determines leaf node for a sample input and
    ;;     then estimates Next3MonthProfit based upon coeffs and observation values.
    ;; *****************************************************************************
    ;; Create the initial result Lambda and check the input array.
    (if (or (= w #void) (<= (length w) 0)) (return (new Vector: 1)))
    (setq Trace true)                       ; Print diagnostics
    (setq result
       (append "(lambda(X) "
         " vars:(m M result vc) "
         " pvars:((Nodes false) (Error 0)) "
         ;; Chase - Get the coefficients from the binary tree for a value X
         ;; X - Observation values
         ;; n - Starting node number
         " (defun Chase(X n) "
         "    vars:(b column node vc) "
         "    (setq b Nodes[n].boundary) "
         "    (setq column Nodes[n].column) "
              ; If a leaf node, return coeffs.
         "    (if (< column 0) (begin "
         "        (return Nodes[n].lower) "
         "    )) "
             ; else, continue down the tree
         "    (if (<= X[column] b) "
         "        (setq node Nodes[n].left) "
         "        (setq node Nodes[n].right) "
         "    ) "
         "    (if (> node 0) "
         "        (return (Chase X node)) "
         "        (return #void) "
         "    ) "
         " ) "
         " (setq vc (Chase X 0)) "
         " (setq M (length X)) "          ; Number of parameters in an observation
         " (setq Error vc[(1+ M)]) "
         " (setq result vc[0]) "          ; Add constant to result 
         " (loop for m from 0 until M do (+= result (* X[m] vc[(1+ m)]))) "
         " result)"
       )
    )
    ; Define a vector containing a binary tree of nodes
    (defstruct Node: column: boundary: left: right: upper: lower:)

    (setq Nodes (new Vector: 1))
    ; Hand craft the first node and split it
    (setq Root (new Node:))
    (setq Root.left 0)
    (setq Root.right 0)
    (setq Nodes[0] Root)
    (setq s 0)                          ; No splits so far
    (Split w 0 T S Z)
     ; Compile and set the final trained result Lambda.
    (setq result (eval result))
    (setq result.Nodes Nodes)
    (return result)
 )  ; end regressNetBinaryRegress



















































;;**EXPORTKEY**:math:regressNetCart2
(defriend math:regressNetCart2(w T S Z)
;; *******************************************************************
;; summary:  Returns an Lambda using the best estimate from several 
;;           segmented classifications.
;; Parms:    w:       The N by M+1 array representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;           T:       The column index of the target variable of the first split
;;                    If <0, then first split is based upon lowest impurity.
;;           S:       The maximum number of splits to segment the tree.
;;           Z:       The minimum number of elements in each segment
;; Return:   Lambda:   An Lambda (Lambda(X) ==> y) expecting an X input vector, returning  y.
;;           Note:    The input vector, X, is a row of the w array, and
;;                    the return value, y, is the Lambda's best prediction for y.
;; *******************************************************************
    vars:(i result Root M vy yS)
    pvars:( ;; Public child methods
            Split    ;; Splits the observations at one node into two child nodes
            ;; Public variables
            Nodes    ;;  Vector of nodes created for this test.
            s        ;;  Number of splits so far
            Trace    ;;  True for verbose output, else false
            T1       ;;  First column used in split.
            S1       ;;  Maximum number of splits allowed
            Z1       ;;  Minimum number of elements in each segment
          )
    ;; *******************************************************************
    ;; Define Child Lambdas
    ;; *******************************************************************

    ;; Split - Splits the observations at one node into two child nodes
    ;; w - observations for this node
    ;; n - Index into Nodes vector
    ;; Returns - set of nodes in Nodes
    (defun Split(w n yS)
        vars:(b delta dmax iN iL iR i I j J k left NL NL1 NL2 NR NR1 NR2 NT
              node pL pR right v vx wl wr)

        ; Initialize some variables
        (setq I (length w))              ; Number of observations in w
        (setq J (sub1 (length w[0])))    ; Number of parameters in w
        (setq wl (new Vector:))          ; Holds observations for left half of w
        (setq wr (new Vector:))          ; Holds observations for right half of w
        (setq dmax 0.0)                  ; Max change in impurity over all splits
        (if (or (<= I 0) (<= J 0)) (return #void))     ; Shouldn't happen

        ; Determine impurity for this node
        (setq NL1 0)                     ; Number observations in class 1
        (setq NL2 0)                     ; Number observations in class 2
        (loop for i from 0 until I do
            (if (<= w[i][J] yS)
                (++ NL1)                 ; Use NL1 for N1, NL2 for N2
                (++ NL2)
            )
        )
        (setq NT (+ NL1 NL2))            ; Total num of observations
        (if (<= NT 0) (begin (writeln "Split: NT=0!") (goto Skip:))) ; Shouldn't happen
        (setq iN (* (/ NL1 NT) (/ NL2 NT))) ; Node impurity = p(1|n)p(2|n)
        ;; (if Trace (writeln "p(1|n)=" (/ NL1 NT) ", p(2|n)=" (/ NL2 NT)))
        ; No split if impurity is zero or NT < 2*Z1
        (if (or (<= iN 0.0) (< NT (* Z1 2))) (goto Leaf:))
 
        ; Split along each column
        (loop for j from 0 until J do
            (if (and (= n 0) (>= T1 0)) (setq j T1))  ; Force column at top level
            ; Sort results on this column
            (setq vx (convertToColumnVector w j))
            (sort vx <)
            (setq b -1.0E99)            ; Last boundary value

            ; For each unique value of x
            (loop for k from 0 to I do
                (if (<= vx[k] b) (goto Skip:)) ; Skip duplicate boundary values
                (setq b vx[k])           ; Set the next boundary value
                (setq NL 0)              ; Num of observations below b
                (setq NL1 0)             ; Num of class1 observations below b
                (setq NL2 0)             ; Num of class2 observations below b
                (setq NR 0)              ; Num of observations above b (right child)
                (setq NR1 0)             ; Num of class1 observations above b
                (setq NR2 0)             ; Num of class2 observations above b 
                ; Divide all observations at b into 2 piles
                (loop for i from 0 until I do
                    (setq v w[i])        ; v = ith observation vector
                    ; Divide observations at boundary
                    (if (<= v[j] b) (begin
                        (if (<= v[J] yS)
                            (++ NL1)     ; Result is in class 1
                            (++ NL2)     ; Result is in class 2
                        )
                    )
                    ; else, put observation in right node
                    (begin
                        (if (<= v[J] yS)
                            (++ NR1)     ; Result is in class 1
                            (++ NR2)     ; Result is in class 2
                        )
                    ))
                )   ; Next observation
                (setq NL (+ NL1 NL2))    ; Num of observations below b
                (setq NR (+ NR1 NR2))    ; Num of observations above b

                ; Both children must have at least Z1 observations
                (if (or (< NL Z1) (< NR Z1)) (goto Skip:))

                ; Compute impurity for this node using gini index
                (setq pL (/ NL NT))      ; Proportion of nodes below b
                (setq pR (/ NR NT))      ; Proportion of nodes above b

                ; Compute impurity for left and right children
                (setq iL (* (/ NL1 NL) (/ NL2 NL)))  ; Impurity of left node
                (setq iR (* (/ NR1 NR) (/ NR2 NR)))  ; Impurity of right node

                ; Compute change in impurity for this division
                (setq delta (- iN (+ (* pL iL) (* pR iR)))) ; Change in impurity

                ; If this delta is largest, save this break point
                (if (and (> delta dmax) (>= NL Z1) (>= NR Z1)) (begin
                    (setq dmax delta)
                    (setq Nodes[n].column j)
                    (setq Nodes[n].boundary b)
                    ;; (if Trace (writeln "Split: Column=" j ", Boundary=" b ",Delta=" (text delta "##.##")) 
                ))
Skip::
            )   ; Next boundary
            (if (and (= n 0) (>= T1 0)) (goto Break:))    ; Just do one column at node 0
        )   ; Next column
Break::
        ; Must be a leaf node if no split was made
        (if (or (< Nodes[n].column 0) (>= s S1)) (goto Leaf:))

        ; Create 2 new child nodes
        ; Now create nodes for each child and split them
        (setq left (length Nodes))    ; Left node index into Nodes
        (setq node (new Node:))       ; Left node structure
        (setq node.left 0)
        (setq node.right 0)
        (setq node.column -1)
        (setq Nodes[left] node)
        (setq Nodes[n].left left)

        (setq right (length Nodes))  ; Right node index
        (setq node (new Node:))
        (setq node.left 0)
        (setq node.right 0)
        (setq node.column -1)
        (setq Nodes[right] node)
        (setq Nodes[n].right right)

        ; Split w on the best column, boundary
        (setq b Nodes[n].boundary)
        (setq j Nodes[n].column)
        ;; (if Trace (writeln "Splitting node=" n ", column=" j ", boundary=" b))
        (++ s)
        (if (<> ^ColUsed #void) (++ ^ColUsed[j]))
        (resize wl 0)
        (resize wr 0)
        (loop for i from 0 until I do
            (setq v w[i])
            (if (<= v[j] b)
                (setq wl[(length wl)] v)
                (setq wr[(length wr)] v)
            )
        )
        (Split wl left yS)
        (Split wr right yS)
        (return #void)
Leaf::
        ;; Set result to average value of the Ys at this node
        (setq b 0)
        (loop for i from 0 until I do
             (+= b w[i][J])
        )
        (setq b (/ b I))
        ;(setq Nodes[n].result b)
        ; Determine predominant class of this node
        (setq NL1 0)
        (setq NL2 0)
        (loop for i from 0 until I do
            (if (<= w[i][J] yS)
                (++ NL1)
                (++ NL2)
            )
        )
        ; Compute impurity for this node
        (setq NT (+ NL1 NL2))                              ; Total num of observations
        (setq Nodes[n].impurity (* (/ NL1 NT) (/ NL2 NT))) ; Node impurity = p(1|n)p(2|n)

        (if (>= NL1 NL2)
            (begin (setq i 0) (setq Nodes[n].result 0.0))
            (begin (setq i 1) (setq Nodes[n].result 0.9))
        )
        (if Trace (writeln "Split: Leaf node " n ", result=" (text Nodes[n].result "##.##")
                   ", class=" (add1 i) ", AVG=" (text b "##.##") ", NL1=" NL1 ", NL2=" NL2))
        ; Note misclassifications
        (if (<> ^Classif #void) (begin
            (+= ^Classif[0][i] NL1)
            (+= ^Classif[1][i] NL2)
        ))
        (return #void)
    )


    ;; *****************************************************************************
    ;; Main Logic Section
    ;; CART - Classification and Regression Trees
    ;;   Divide observations at root node into 2 parts at each distinct value along
    ;;   one column.  Repeat for all of the other columns.  Pick the column and the
    ;;   boundry that best splits the high yield results from the other results.  
    ;;   Repeat the above process for each node until less than MIN samples per node.
    ;;   Save split column number and boundary at each intermediate node.
    ;;   Build a prediction Lambda that determines leaf node for a sample input and
    ;;   then estimates Next3MonthProfit based upon coeffs and observation values.
    ;; *****************************************************************************
    ;; Create the initial result Lambda and check the input array.
    (if (or (= w #void) (<= (length w) 0)) (return (new Vector: 1)))
    (setq result
       (append "(lambda(X) "
         " vars:(r) "
         " pvars:(Chase (Error 0)) "
           ;; Chase - Get the result from the binary tree for a value X
           ;; X - Observation values
           ;; n - Starting node number
         " (defun Chase(X n) "
         "     vars:(b column node vc) "
         "     pvars: (Nodes) "
         "     (setq b Nodes[n].boundary) "
         "     (setq column Nodes[n].column) "
               ; If a leaf node, return result.
         "     (if (< column 0) (begin "
         "         (setq Error Nodes[n].impurity) "
         "         (return Nodes[n].result) "
         "     )) "
               ; else, continue down the tree
         "     (if (<= X[column] b) "
         "         (setq node Nodes[n].left) "
         "         (setq node Nodes[n].right) "
         "     ) "
         "     (if (> node 0) "
         "         (return (Chase X node)) "
         "         (return #void) "
         "     ) "
         " ) "
         ; Chase returns the value from the leaf node
         " (setq r (Chase X 0)) "
         " r)"
       )
    )
    ; Initialize
    (setq Trace false)              ; Set to true to show details
    (setq s 0)
    (setq T1 T)
    (setq S1 S)
    (setq Z1 Z)                    ; Minimum observations in a node

    ; Find the boundry between the upper 95% of the y's, yS
    (setq M (sub1 (length w[0])))
    (setq vy (convertToColumnVector w M))
    (sort vy >)
    (setq i (divi (length w) 20))      ; Upper 5%
    (setq yS vy[i])
    (if Trace (writeln _eol "yS=" (text yS "##.##")))

    ; Define a vector containing a binary tree of nodes
    (defstruct Node: column: boundary: left: right: result: impurity:)
    (setq Nodes (new Vector: 1))
    ; Hand craft the first node and split it
    (setq Root (new Node:))
    (setq Root.left 0)
    (setq Root.right 0)
    (setq Root.column -1)
    (setq Nodes[0] Root)
    (Split w 0 yS)
     ; Compile and set the final trained result Lambda.
    (setq result (eval result))
    (setq result.Nodes Nodes)
    (return result)
 )  ; end regressNetCart2














































;;**EXPORTKEY**:math:regressNetCart3
(defriend math:regressNetCart3(w T S Z)
;; *******************************************************************
;; summary:  Returns an Lambda using Classification Tree with 3 classes - no pruning
;; Parms:    w:       The N by M+1 array representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;           T:       The column index of the target variable of the first split
;;           S:       The maximum number of splits to segment the tree.
;;           Z:       The minimum number of elements in each segment
;; Return:   Lambda:   An Lambda (Lambda(X) ==> y) expecting an X input vector, returning  y.
;;           Note:    The input vector, X, is a row of the w array, and
;;                    the return value, y, is the Lambda's best prediction for y.
;; Note:     Classification and Regression Trees by Breiman et al, CRC Press 1998
;; *******************************************************************
    vars:(i result Root M vy)
    pvars:( ;; Public Methods:
            Split     ; Split the observations at one node into two child nodes
            ;; Public variables:
            Nodes     ; Vector of node structures for binary tree
            s         ; Number of splits for this test
            Trace     ; Verbose output iff true
            T1        ;  First column used in split.
            S1        ;  Maximum number of splits allowed
            Z1        ;  Minimum number of elements in each segment
            yL        ;  Upper boundary of y values for class 2
            yH        ;  Lower boundary of y values for class 3
          )

    ;; *******************************************************************
    ;; Define Child Lambdas
    ;; *******************************************************************
    ;; Split - Split the observations at one node into two child nodes
    ;; w - observations for this node
    ;; n - Index into Nodes vector
    ;; Returns - set of nodes in Nodes
    (defun Split(w n)
        vars:(b b1 delta delta1 dmax iN iL iR i I j j1 J k left NL NL1 NL2 NL3
              NR NR1 NR2 NR3 NT node p1 p2 p3 pL pR right v vx wl wr y)

        ; Initialize some variables
        (setq I (length w))              ; Number of observations in w
        (setq J (sub1 (length w[0])))    ; Number of parameters in w
        (setq wl (new Vector:))          ; Holds observations for left half of w
        (setq wr (new Vector:))          ; Holds observations for right half of w
        (setq dmax 0.0)                  ; Max change in impurity over all splits
        (setq j1 -1)                     ; No split made
        (if (or (<= I 0) (<= J 0)) (return #void))     ; Shouldn't happen

 
        ; Determine impurity for this node
        (setq NL1 0)                     ; Number observations in class 1
        (setq NL2 0)                     ; Number observations in class 2
        (setq NL3 0)                     ; Number observations in class 2
        (loop for i from 0 until I do
            (setq y w[i][J])                ; v = ith observation vector
            (if (> y yH)
                (++ NL2)     ; Result is in class 2
                (if (< y yL)
                    (++ NL3)         ; Use NLx for Nx
                    (++ NL1)
                )
            )
        )
        (setq NT (+ NL1 NL2 NL3))        ; Total num of observations

        (if (<= NT 0) (begin (writeln "Split: NT=0!") (goto Skip:))) ; Shouldn't happen
        (setq p1 (/ NL1 NT))
        (setq p2 (/ NL2 NT))
        (setq p3 (/ NL3 NT))
        (setq iN (- 1 (+ (* p1 p1) (* p2 p2) (* p3 p3))))  ; Node impurity = 1 - S{ pi **2}

        ;; (if Trace (writeln "iN=" iN ", p(1|n)=" p1 ", p(2|n)=" p2 ", p(3|n)=" p3 ))
        ; No split if impurity is zero or NT < Z1
        (if (or (<= iN 0.0) (< NT Z1)) (begin  (goto Leaf:)))
 
        ; Split along each column
        (loop for j from 0 until J do
            (if (and (= n 0) (>= T1 0)) (setq j T1))  ; Force column at top level

            ; Sort results on this column
            (setq vx (convertToColumnVector w j))
            (sort vx <)
            (setq b -1.0E99)            ; Last boundary value

            ; For each unique value of x
            (loop for k from 0 until I do
                (if (<= vx[k] b) (goto Skip:)) ; Skip duplicate boundary values
                (setq b vx[k])           ; Set the next boundary value
                (setq NL 0)              ; Num of observations below b
                (setq NL1 0)             ; Num of class1 observations below b
                (setq NL2 0)             ; Num of class2 observations below b
                (setq NL3 0)             ; Num of class3 observations below b
                (setq NR 0)              ; Num of observations above b (right child)
                (setq NR1 0)             ; Num of class1 observations above b
                (setq NR2 0)             ; Num of class2 observations above b
                (setq NR3 0)             ; Num of class2 observations below b
 
                ; Divide all observations at b into 3 piles
                (loop for i from 0 until I do
                    (setq v w[i])        ; v = ith observation vector
                    (setq y v[J])
                    ; Divide observations at boundary
                    (if (<= v[j] b) (begin
                        (if (> y yH)
                            (++ NL2)     ; Result is in class 2
                            (if (< y yL)
                                (++ NL3)     ; Result is in class 3
                                (++ NL1)     ; Result is in class 1
                            )
                        )
                    )
                    ; else, put observation in right node
                    (begin
                        (if (> y yH)
                            (++ NR2)     ; Result is in class 2
                            (if (< y yL)
                                (++ NR3)     ; Result is in class 3
                                (++ NR1)     ; Result is in class 1
                            )
                        )
                    ))
                )   ; Next observation
                (setq NL (addi NL1 NL2 NL3))    ; Num of observations below b
                (setq NR (addi NR1 NR2 NR3))    ; Num of observations above b

                ; Both children must have at least Z1 observations
                (if (or (< NL Z1) (< NR Z1)) (goto Skip:))

                ; Compute impurity for this node using gini index
                (setq pL (/ NL NT))      ; Proportion of nodes below b
                (setq pR (/ NR NT))      ; Proportion of nodes above b

                ; Compute impurity for left and right children
                (setq p1 (/ NL1 NL))
                (setq p2 (/ NL2 NL))
                (setq p3 (/ NL3 NL))
                (setq iL (- 1 (+ (* p1 p1) (* p2 p2) (* p3 p3))))  ; Impurity of left node
                (setq p1 (/ NR1 NR))
                (setq p2 (/ NR2 NR))
                (setq p3 (/ NR3 NR))
                (setq iR (- 1 (+ (* p1 p1) (* p2 p2) (* p3 p3))))  ; Impurity of right node

                ; Compute change in impurity for this division
                (setq delta (- iN (+ (* pL iL) (* pR iR)))) ; Change in impurity

                ; If this delta is largest, save this break point
                (if (and (> delta dmax) (>= NL Z1) (>= NR Z1)) (begin
                    (setq dmax delta)
                    (setq j1 j)
                    (setq b1 b)
                    ;; (if Trace (writeln "Split: Column=" j ", Boundary=" b ",Delta=" (text delta "##.##")) 
                ))
Skip::
            )   ; Next boundary
            (if (and (= n 0) (>= T1 0)) (goto Break:))    ; Just do one column at node 0

        )   ; Next column
Break::
        ; Quit if no split was made
        (if (or (< j1 0) (>= s S1)) (goto Leaf:))
        (setq Nodes[n].column j1)
        (setq Nodes[n].boundary b1)

        ; Create 2 new child nodes
        ; Now create nodes for each child and split them
        (setq left (length Nodes))    ; Left node index into Nodes
        (setq node (new Node:))       ; Left node structure
        (setq node.left 0)
        (setq node.right 0)
        (setq node.column -1)
        (setq Nodes[left] node)
        (setq Nodes[n].left left)

        (setq right (length Nodes))  ; Right node index
        (setq node (new Node:))
        (setq node.left 0)
        (setq node.right 0)
        (setq node.column -1)
        (setq Nodes[right] node)
        (setq Nodes[n].right right)

        ; Split w on the best column, boundary
        (++ s)
        (if Trace (writeln "Splitting node=" n ", column=" j1 ", boundary=" b1 ", delta=" (text dmax "##.##") ",s=" s))
        (if (<> ^ColUsed #void) (++ ^ColUsed[j1]))   ; Track the number of times that this col is used.
        (resize wl 0)
        (resize wr 0)
        (loop for i from 0 until I do
            (setq v w[i])
            (if (<= v[j1] b1)
                (setq wl[(length wl)] v)
                (setq wr[(length wr)] v)
            )
        )
        (Split wl left)
        (Split wr right)
        (return #void)
Leaf::
        ; Set result to average value of the Ys at this node
        (setq b 0)
        (loop for i from 0 until I do
            (+= b w[i][J])
        )
        (setq b (/ b I))
        ;; (setq Nodes[n].result b)
        ; Determine predominant class of this node
        (setq NL1 0)
        (setq NL2 0)
        (setq NL3 0)
        (loop for i from 0 until I do
           (setq y w[i][J])
            (if (> y yH)
                (++ NL2)             ; Result is in class 2
                (if (< y yL)
                    (++ NL3)         ; Use NLx for Nx
                    (++ NL1)
                )
            )
        )
        (setq NT (+ NL1 NL2 NL3))
        (setq p1 (/ NL1 NT))
        (setq p2 (/ NL2 NT))
        (setq p3 (/ NL3 NT))
        (setq iN (- 1 (+ (* p1 p1) (* p2 p2) (* p3 p3))))  ; Node impurity
        (setq Nodes[n].impurity iN)

        ; Determine the class for this leaf node
        (if (> NL2 NL1)
            (if (>= NL3 NL2)
                (begin (setq i 2) (setq Nodes[n].result -0.9))
                (begin (setq i 1) (setq Nodes[n].result  0.9))
            )
            (if (> NL3 NL1)
                (begin (setq i 2) (setq Nodes[n].result -0.9))
                (begin (setq i 0) (setq Nodes[n].result  0.0))

            )
        )
        ; Track the classifications for this leaf
        (if (<> ^Classif #void) (begin
                (+= ^Classif[0][i] NL1)
                (+= ^Classif[1][i] NL2)
                (+= ^Classif[2][i] NL3)
        ))
        (if Trace (writeln "Leaf node " n ", result=" (text Nodes[n].result "##.##")
        ", class=" (add1 i) ", AVG=" (text b "##.##") ", TOTAL=" I ", NL1=" NL1 ", NL2=" NL2 ", NL3=" NL3))
        (return #void)
    )

    ;; *****************************************************************************
    ;; Main Logic Section
    ;; CART3 - Classification and Regression Trees
    ;;   Divide observations at top node into 3 parts at each distinct value along
    ;;   one column.  Repeat for all of the other columns.  Pick the column and the
    ;;   boundry that best splits the high yield results from the low yield results.  
    ;;   Repeat the above process for each node until less than MIN samples per node.
    ;;   Save split column number and boundary at each intermediate node.
    ;;   Build a prediction Lambda that determines leaf node for a sample input and
    ;;   then estimates Next3MonthProfit based upon coeffs and observation values.
    ;; *****************************************************************************
    ;; Create the initial result Lambda and check the input array.
    (if (or (= w #void) (<= (length w) 0)) (return (new Vector: 1)))
    (setq result
       (append "(lambda(X) "
         "vars:(r) "
         "pvars:(Chase Nodes (Error 0)) "
          ;; Chase - Get the result from the binary tree for a value X
          ;; X - Observation values
          ;; n - Starting node number
         "(defun Chase(X n) "
         "    vars:(b column node vc) "
         "    (setq b Nodes[n].boundary) "
         "    (setq column Nodes[n].column) "
              ; If a leaf node, return result.
         "    (if (< column 0) (begin "
         "       (setq Error Nodes[n].impurity) "
         ;; "       (writeln \"Node=\" n \",r=\" Nodes[n].result \"Error=\" Error) "
         "       (return Nodes[n].result) "
         "    )) "
              ; else, continue down the tree
         "    (if (<= X[column] b) "
         "        (begin (setq node Nodes[n].left)  ) "     ;; (display \"Setting left node=\" node)) "
         "        (begin (setq node Nodes[n].right) ) "     ;; (display \"Setting right node=\" node)) "
         "    ) "
         "    (if (> node 0) "
         "        (return (Chase X node)) "
         "        (begin (return #void) (display \"Bad node=\" node) (return #void)) "
         "    ) "
         ") "
          ; Chase returns the value from the leaf node
         "(setq r (Chase X 0)) "
         "r)"
       )
    )
    ; Initialize
    (setq Trace false)             ; Set to true to show details
    (setq s 0)                     ; No splits yet
    (setq T1 T)
    (setq S1 S)
    (setq Z1 Z)                    ; Minimum observations in a node

    ; Find the boundry between the upper 95% of the y's and the lower 5% of the y's.
    (setq M (sub1 (length w[0])))
    (setq vy (convertToColumnVector w M))
    (sort vy >)
    (setq i (divi (length w) 20))         ; Upper/lower 5%
    (setq yH vy[i])
    (sort vy <)
    (setq yL vy[i])

    ;; Initialize some instrumentation variables
    (if Trace (writeln "T=" T1 ",S=" S1 ",Z=" Z1 ",Boundary=" i ",yL=" (text yL "##.##") ",yH=" (text yH "##.##")))

    ; Define a vector containing a binary tree of structures
    (defstruct Node: column: boundary: left: right: result: impurity:)
    (setq Nodes (new Vector: 1))
    ; Hand craft the first node and split it
    (setq Root (new Node:))
    (setq Root.left 0)
    (setq Root.right 0)
    (setq Root.column -1)
    (setq Nodes[0] Root)
    (Split w 0)

    ; Compile and set the final trained result Lambda.
    (setq result (eval result))
    (setq result.Nodes Nodes)
    (return result)
 )  ; end regressNetCart3













































;;**EXPORTKEY**:math:regressNetMultiSegment
(defriend math:regressNetMultiSegment(w T S Z)
;; *******************************************************************
;; summary:  Returns an Lambda using the coefficients from several 
;;           segmented multiple regressions.
;; Parms:    w:       The N by M+1 array representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;           T:       The column index of the target variable of the first split
;;                    (currently ignored)
;;           S:       The maximum number of splits to segment the target variable.
;;           Z:       The minimum number of elements in each segment of the target variable
;;  Since MultiSegment ignores T, it will not work with regressTree.
;; Return:   Lambda:   An Lambda (Lambda(X) ==> y) expecting an X input vector, returning y.
;;           Note:    The input vector, X, is a row of the w array, and
;;                    the return value, y, is the Lambda's best prediction for y.
;; Note:     See Sedgewick[2] chap 37.
;; ***********************************************************************
    pvars:( Trace )   ; Verbose mode
    vars:(result E vc wt v vx vy vr vs i j I m M M1 n N WS s C CC T)
    ;; *******************************************************************
    ;; Define Child Lambdas
    ;; *******************************************************************
    ;; Segment the input array on the specified column.
    (defun segment(w T S Z)
       vars:(C S s ws n N m M)

       ;; Don't segment if there are insufficient training samples.
       (if (<= (length w) Z) (return (new Vector: 1 w)))
       (if (<= (length w) (* 2 (length w[0]))) (return (new Vector: 1 w)))
       ;; Sort the training samples on the specified variable.
       (sort w (let ((T T)) (lambda(_x _y) (<= _x[T] _y[T]))))
       ;; Heuristically determine the number of segments.
       (setq S (integer (min S (/ (length w) Z))))  ; S=Max number of segments
       (setq C (new Vector: S))         ; C=vector holding observations for each segment
       (setq N (length w))        ; N = Number of observations
       (setq M (integer (/ N S))) ; M = Observations / segment
       (loop for n from 0 until N do  ; Loop thru all observations
          (setq m (min (sub1 S) (divi n M)))     ; m = segment # for this observation
          (if (= C[m] #void) (setq C[m] (new Vector:)))
          (setq C[m][(length C[m])] w[n])        ; C[m][end]=observation n
       )
    C)

    ;; *****************************************************************************
    ;; Main Logic Section
    ;; MultiSegment Version
    ;;     Divide along each axis into S segments each with max(Z, 2 * M) elements
    ;;     Save all S * M sets of coeffs, avg squared error, dividing points
    ;;     In result, pick the column whose segment has the least squared error.
    ;;     Use coeffs for this segment to predict the next 3 month profit
    ;; *****************************************************************************
    ;; Create the initial result Lambda and check the input array.
    (if (or (= w #void) (<= (length w) 0)) (return (new Vector:)))
    (setq result
       (append "(lambda(X) pvars:((CC false) (C false) (E 0) (Error 0)) "
         " vars:(emin j J m M s S SL res) "
         " (setq emin 1.0E+40) "    ; emin=lowest avg squared error of all columns
         " (setq J -1) "            ; Column with lowest error
         " (setq S -1) "            ; Segment with lowest error
         " (setq M (length X)) "    ; Number of parameters (columns)
         " (loop for j from 0 until M do "
         "    (setq SL (length CC[j])) " ; Number of segments for this column
         ;    Find segment that the observation falls into for this column
         "    (loop for s from 0 until SL do "
         "        (if (<= X[j] CC[j][s]) (goto NextStep:)))"
         "    NextStep:: "
         "    (if (>= s SL) (setq s (sub1 SL))) "
         ;    Record the column & segment with minimum error
         "    (if (< E[j][s] emin) "
         "       (begin (setq emin E[j][s]) (setq J j) (setq S s))))"
         ;; " (display \"E=\" emin \", C[\" J \"][\" S \"][0]=\" C[J][S][0] \", X[0]=\" X[0] _eol) "
         " (setq res C[J][S][0]) "
         " (loop for m from 1 until M do (setq res (+ res (* X[(sub1 m)] C[J][S][m])))) "
         " (setq Error emin) "
         " res)"
       )
    )
    ;  Initialize
    (setq Trace true)                 ; Set to true for verbose mode
    (setq M (sub1 (length w[0])))      ; Number of independent variables
    (setq CC (new Vector: M))          ; CC=upper boundary of column-segment
    (setq E (new Vector: M))           ; E=Least-squared error for column-segment
    (setq C (new Vector: M))           ; C=Regression coeffs for column-segment

    ;; Segment the input array along each axis
    (loop for j from 0 until M do      ; Loop thru each column
        (setq WS (segment w j S Z))    ; WS=observations for each segment of column j
        (setq S (length WS))           ; S=number of segments for this column
        (if Trace (display "Column " j " divided into " S " segments." _eol))
        (setq CC[j] (new Vector: S))
        (setq E[j] (new Vector: (add1 S)))
        (setq C[j] (new Vector: (add1 S)))
        ; Compute the coefficient vectors for each segment of each column
        (loop for s from 0 until S do
           (setq CC[j][s] WS[s][(sub1 (length WS[s]))][j])
           (setq vc (multivariableRegressC WS[s]))
           (setq M1 (subi (length vc) 1))
           (setq E[j][s] vc[M])
           (setq C[j][s] (resize vc M))
        )
        (setq CC[j][S] CC[j][(sub1 S)])
        (setq E[j][S] E[j][(sub1 S)])
    )
    ;; Compile and set the final trained result Lambda.
    (setq result (eval result))
    (setq result.C C)
    (setq result.CC CC)
    (setq result.E E)
    (return result)
 )  ; end regressNetMultiSegment














































;;**EXPORTKEY**:math:regressNetTwoSegment
(defriend math:regressNetTwoSegment(w T S Z)
;; *******************************************************************
;; summary:  Returns an Lambda using the coefficients from several 
;;           segmented multiple regressions.
;; Parms:    w:       The N by M+1 array representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;           T:       The column index of the target variable to segment.
;;           S:       The maximum number of splits to segment the target variable.
;;           Z:       The minimum number of elements in each segment of the target variable
;; Return:   Lambda:   An Lambda (Lambda(X) ==> y) expecting an X input vector, returning y.
;;           Note:    The input vector, X, is a row of the w array, and
;;                    the return value, y, is the Lambda's best prediction for y.
;; Note:     See Sedgewick[2] chap 37.
;; This version generates linear coeffs. for 2 or more segments along one axis.
;; It selects the axis with the least best linear fit.
;; *******************************************************************
    pvars:(Trace               ; Verbose output iff true
          )
    vars:(result E vc wt v vx vy vr vs i I m M n N WS s C CC)
    ;; *******************************************************************
    ;; Define Child Lambdas
    ;; *******************************************************************
    ;; Segment the input array on the specified column.
    (defun segment(w T S Z)
       vars:(C s ws n N m M)
       ;; Don't segment if there are insufficient training samples.
       (if (<= (length w) Z) (return (new Vector: 1 w)))
       (if (<= (length w) (* 2 (length w[0]))) (return (new Vector: 1 w)))
       ;; Sort the training samples on the specified variable.
       (sort w (let ((T T)) (lambda(_x _y) (<= _x[T] _y[T]))))
       ;; Heuristically determine the number of segments.
       (setq S (integer (min S (/ (length w) Z))))
       (setq C (new Vector: S))
       (setq N (length w))
       (setq M (integer (/ N S)))
       (loop for n from 0 until N do
          (setq m (min (sub1 S) (divi n M)))
          (if (= C[m] #void) (setq C[m] (new Vector:)))
          (setq C[m][(length C[m])] w[n])
          ) ; end loop
       C) ; end segment

    ; Select the column to be used for segmentation
    ; Only used if T is < 0.  The column is specified by an input parameter
    (defun selectTarget(w)
        vars: (emin j M t vr vx vy)
        (setq M (sub1 (length w[0])))
        (setq vy (convertToColumnVector w M))
        (setq emin 1.0E+40)           ; error of best column
        (setq t -1)                           ; t is column with best fit

        ; Select the column with the best linear fit
        (loop for j from 0 until M do
            (setq vx (convertToColumnVector w j))
            (setq vr (regress vx vy))
            ;; (if Trace (display "j=" j ", vr=" vr _eol))
            (if (< vr[2] emin)
                (begin
                    (setq t j)
                    (setq emin vr[2])
                )
            ) 
        )
        (return t)
    )

    ;; *******************************************************************
    ;; Main Logic Section
    ;; *******************************************************************
    ;; Create the initial result Lambda and check the input array.
    (if (or (= w #void) (<= (length w) 0)) (return (new Vector:)))
    (setq Trace false)              ; Set to true to show details
    (setq result (append "(lambda(X) pvars:((CC false) (C false) (T 0) (E 0) (Error 0)) "
          " vars:(m M n N result) "
          " (loop for n from 0 until (length CC) do (if (<= X[T] CC[n]) (goto NextStep:))) "
          " NextStep:: "
          " (setq M (length C[n])) "
          " (setq result C[n][0]) "
          " (loop for m from 1 until M do (setq result (+ result (* X[(sub1 m)] C[n][m])))) "
          " (setq Error E[n]) "
          " result)"
    )    )
    ; Use T if it is from 0 until M; else, pick the best one.
    (if (< T 0) (setq T (selectTarget w)))
    (if (<> ^ColUsed #void) (++ ColUsed[T]))
    ;; Segment the input array.
    (setq WS (segment w T S Z))
    (setq S (integer (min S (length WS))))
    (if Trace (display "Setting T=" T ", S=" S _eol))
    (setq CC (new Vector: S))
    (setq E (new Vector: (add1 S)))
    (setq C (new Vector: (add1 S)))

    ;; Compute the coefficient vectors from multiple linear regressions.
    (loop for s from 0 until S do
       (setq CC[s] WS[s][(sub1 (length WS[s]))][T])
       (setq vc (multivariableRegressC WS[s]))
       (setq M (subi (length vc) 1))
       (setq E[s] vc[M])
       (setq C[s] (resize vc M))
    )
    (setq C[S] C[(sub1 S)])
    (setq E[S] E[(sub1 S)])

    ;; Compile and set the final trained result Lambda.
    (setq result (eval result))
    (setq result.T T)
    (setq result.C C)
    (setq result.CC CC)
    (setq result.E E)
    ;; Return the regression Lambda.
    result) ; end regressTree
















































;;**EXPORTKEY**:math:regressTree
(defriend math:regressTree(w T S Z)
;; *******************************************************************
;; summary:  Returns an Lambda using the coefficients from several 
;;           segmented multiple regressions.
;; Parms:    w:       The N by M+1 array representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;           T:       The column index of the target variable to segment.
;;           S:       The maximum number of splits to segment the target variable.
;;           Z:       The minimum number of elements in each segment of the target variable.
;; Return:   Lambda:   An Lambda (Lambda(X) ==> y) expecting an X input vector, returning y.
;;           Note:    The input vector, X, is a row of the w array, and
;;                    the return value, y, is the Lambda's best prediction for y.
;; Note:     See Sedgewick[2] chap 37.
;; *******************************************************************
    vars:(result E vc wt v vx vy vr vs i I m M n N WS s C CC)
    ;; *******************************************************************
    ;; Define Child Lambdas
    ;; *******************************************************************
    ;; Segment the input array on the specified column.
    (defun segment(w T S Z)
       vars:(C s ws n N m M)
       ;; Don't segment if there are insufficient traning samples.
       (if (<= (length w) Z) (return (new Vector: 1 w)))
       (if (<= (length w) (* 2 (length w[0]))) (return (new Vector: 1 w)))
       ;; Sort the training samples on the specified variable.
       (sort w (let ((T T)) (lambda(_x _y) (<= _x[T] _y[T]))))
       ;; Heuristically determine the number of segments.
       (setq S (integer (min S (/ (length w) Z))))
       (setq C (new Vector: S))
       ;; Heuristically determine the number of segments.
       (setq N (length w))
       (setq M (integer (/ N S)))
       (loop for n from 0 until N do
          (setq m (integer (min (sub1 S) (divi n M))))
          (if (= C[m] #void) (setq C[m] (new Vector:)))
          (setq C[m][(length C[m])] w[n])
          ) ; end loop
       C) ; end segment
    ;; *******************************************************************
    ;; Main Logic Section
    ;; *******************************************************************
    ;; Create the initial result Lambda and check the input array.
    (if (or (= w #void) (<= (length w) 0)) (return (new Vector:)))
    (setq result (append "(lambda(X) pvars:((CC false) (C false) (T 0) (E 0) (Error 0)) "
                                   " vars:(m M n N result) "
                                   " (loop for n from 0 until (length CC) do (if (<= X[T] CC[n]) (goto NextStep:))) "
                                   " NextStep:: "
                                   " (setq M (length C[n])) "
                                   " (setq result C[n][0]) "
                                   " (loop for m from 1 until M do (setq result (+ result (* X[(sub1 m)] C[n][m])))) "
                                   " (setq Error E[n]) "
                                   " result)"
                         ))
    ;; Segment the input array.
    (setq WS (segment w T S Z))
    (setq S (integer (min S (length WS))))
    (setq CC (new Vector: S))
    (setq E (new Vector: (add1 S)))
    (setq C (new Vector: (add1 S)))
    ;; Compute the coefficient vectors from multiple linear regressions.
    (loop for s from 0 until S do
       (setq CC[s] WS[s][(sub1 (length WS[s]))][T])
       (setq vc (multivariableRegressC WS[s]))
       (setq M (subi (length vc) 1))
       (setq E[s] vc[M])
       (setq C[s] (resize vc M))
       ) ; 
    (setq C[S] C[(sub1 S)])
    (setq E[S] E[(sub1 S)])
    ;; Compile and set the final trained result Lambda.
    (setq result (eval result))
    (setq result.T T)
    (setq result.C C)
    (setq result.CC CC)
    (setq result.E E)
    ;; Return the regression Lambda.
    result) ; end regressTree

















































;;**EXPORTKEY**:math:sigmoidizeArray
(defriend math:sigmoidizeArray(mat ...)
;; *******************************************************************
;; summary:  Converts the column values in an array to sigmoid values. 
;;           Each array cell value is normalized by finding the positive
;;           logit value x/(1+x). Positive values of x are scaled into [.5 - 1.0]. 
;;           Negative values of x are scaled into [0 - .5].
;; Args:     mat:     A array with values to be sigmoidized. The array
;;                    is stored in [row][column] order.
;;           depSW    (Optional) true if last column is to be unchanged.
;; Return:   nmat:    The sigmoidized array (all values are positive fractions).
;; *******************************************************************
    vars:(nmat i j x logit Y colCount rowCount high low range (depSW false))
    (if (= (argCount) 2) (setq depSW (argFetch 1)))
    (setq mat (convertToArray mat))
    (setq rowCount (length mat))
    (setq colCount (length mat[0]))
    (setq Y (sub1 colCount))
    (setq nmat (new Vector: rowCount))
    (loop for i from 0 until rowCount do (setq nmat[i] (new Vector: number: colCount)))
    (loop for i from 0 until colCount do
        (loop for j from 0 until rowCount do
           (setq x mat[i][j])
           (setq logit (/ x 2 (add1 x))) 
           (if (isPositive x) 
               (setq logit (+ .5 logit))
               (setq logit (- .5 logit))
               ) ; end if
           (setq nmat[j][i] logit)
           ) ; end j loop
        ) ; end i loop
    (if depSW (loop for i from 0 until rowCount do (setq nmat[i][Y] mat[i][Y])))
    nmat) ; end sigmoidizeArray








































































;;**EXPORTKEY**:math:smoRegress
(defriend math:smoRegress(x y kernelID ...)
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
           (myCLearningSW false)		;; False iff we are to use the builtin SVM C Function for our learning. 
           (Integer:mySVSize 100)		;; Size of the seed support vector group during initialization. 
           Integer:M					;; The number of elements in each training point (independent variables). 
           Integer:N					;; The number of target points (independent variables). 
           Integer:N1					;; The first training point to improve (selected by heurism). 
           Integer:N2					;; The second training point to improve (selected by heurism). 
           Py                      		;; The cached absolute delta support vector output while training the SMO regression model (in percent of target). 
           NumVector:W                  ;; The best guess weight coefficient N vector for the SMO regression model.
           X                       		;; The N X M vector array of training points (independent variables). 
           NumVector:Y                  ;; The N vector of target points (dependent variables). 
           ;; Public child methods
           clear			       		;; Clear the current support vector machine.
           computeError	           		;; Return the regression error for the the SMO coefficients.
           kernel   		       		;; Return the svm kernel output for the specified two input points.
           svmLambda			       		;; Return an Lambda ready to compute the svm output for specified input points.
           svmOutput		       		;; Return the svm output for the specified input point.
           svmTraining		       		;; Train the svm machine on the specified inputs and model.
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
    ;; Return the regression error for the currently trained SMO model.
    ;; Note1: The regression error is computed as a percent of the target.
    ;; Note2: Any regression error within the tollerance limit, ETollerance, is treated as NO error.
    (defun computeError(recalcSW)
       regs:(m n RN)
       regs:(Number:etol Number:pct Number:ey Number:dy Number:Yn)
       regs:((Number:err 0.0) (Number:RZero 0.0))
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
          (-= pct etol)
          (setq pct (|Gv:abs| pct))
	      (+= err pct)
	      ) ; end error loop
       (vmregRunInHardware stop:)
	   (/= err RN)
       err) ; end computeError
    ; Return an Lambda ready to compute the svm output for specified input points.
    (defun svmLambda()
       regs:(n m)
       vars:(Lambda)
       (setq Lambda (eval "(lambda(NumVector:x) pvars:(Strategy W X N M kernel ETollerance Error) regs:(Integer:n Integer:m Number:ey Number:ky) (setq ey 0.0) (loop for n from 0 until N do (if (<> W[n] 0.0) (setq ky (* W[n] (kernel x X[n]))) (setq ky 0.0)) (setq ey (+ ey ky)) ) ey)"))
       (setq Lambda.Strategy svm:)
       (setq Lambda.W (new Vector: Number:))
       (setq Lambda.X (new Vector: Object:))
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
        (if (= tollerance 0.0) (setq tollerance 1.0e-300))
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
	      ((= kernelID #void) (setq kernel |Gv:vectorInnerProduct|))
	      ((= kernelID "binary") (setq kernel |Gv:vectorBinaryInnerProduct|))
	      ((= kernelID "bipolar") (setq kernel |Gv:vectorBipolarInnerProduct|))
	      ((= kernelID "cube") (setq kernel |Gv:vectorCubeInnerProduct|))
	      ((= kernelID "exp") (setq kernel |Gv:vectorExpInnerProduct|))
	      ((= kernelID "cosine") (setq kernel |Gv:vectorCosineInnerProduct|))
	      ((= kernelID "linear") (setq kernel |Gv:vectorInnerProduct|))
	      ((= kernelID "log") (setq kernel |Gv:vectorLogInnerProduct|))
	      ((= kernelID "quart") (setq kernel |Gv:vectorQuartInnerProduct|))
	      ((= kernelID "quint") (setq kernel |Gv:vectorQuintInnerProduct|))
	      ((= kernelID "sine") (setq kernel |Gv:vectorSineInnerProduct|))
	      ((= kernelID "sigmoid") (setq kernel |Gv:vectorSigmoidInnerProduct|))
	      ((= kernelID "square") (setq kernel |Gv:vectorSquareInnerProduct|))
	      ((= kernelID "tan") (setq kernel |Gv:vectorTanInnerProduct|))
	      ((= kernelID "tanh") (setq kernel |Gv:vectorTanhInnerProduct|))
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
	    ;; Use the built-in SMO regression Function for training.
  	    (setq result (|Gv:svmRegression| X Y kernel ETollerance maxErr GenerationMax mySVSize myVerboseSW))
  	    (setq Error result.Error)
  	    (setq Generations result.Generations)
  	    (setq W result.Weights)
        (setq Ey result.Ey)
        (setq Py result.Py)
	    ;; Generate the final absolute error score after training.
	    HaltTraining::
	    (if myVerboseSW (writeln "smoRegress: Final Model Generations = [" Generations  "], ETollerance = [" ETollerance  "], Error = [" Error "]"))       
        result) ; end svmTraining
    ;; ****************************************
    ;; Define Private Maintenance Child Lambdas.
    ;; ****************************************
    ;; The self test method for this Lambda.
    (defun selfTest(testName columns rows maxGenerations)
       vars:(m n g G y ey C c X Y Yv avgY avgTopEy topEyCnt
             kernelID err Net pct 
             startTime endTime startTimeT endTimeT
             (checkResults true)
             (tol 0.0) (errStop 0.0) (Cs 1.0)
             ) ; end temporary variables
       (clear)
       (setq startTimeT (getTickCount 0))
       (setq srandom.seed 8192.0)
       (setq myVerboseSW false)
       ;; Select the requested test case
       (cond
         ;; testName Case linear 
         ((= testName linear:)
          (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: We support a bias by having X[0] == 1 for all N.
		       ;; Note2: This algorithm seems to work well when N is at least 25 times M.
		       (setq kernelID ^vectorInnerProduct)
		       (setq c Cs)
		       (setq M columns)
		       (setq N rows)
		       (setq X (new Vector: Object: N))
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
		       (smoRegress X Y kernelID tol errStop maxGenerations)
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
          )) ; end testName Case linear
         ;; testName Case linearSigmoid 
         ((= testName linearSigmoid:)
          (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: The inputs, X, are restricted to the sigmoid domain.
		       ;; Note2: We support a bias by having X[0] == 1 for all N.
		       ;; Note3: This algorithm seems to work well when N is at least 25 times M.
		       (setq kernelID ^vectorInnerProduct)
		       (setq c Cs)
		       (setq M columns)
		       (setq N rows)
		       (setq X (new Vector: Object: N))
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
		       (smoRegress X Y kernelID tol errStop maxGenerations)
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
          )) ; end testName Case linear
         ;; testName Case srandom 
         ((= testName srandom:)
          (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: We support a bias by having X[0] == 1 for all N.
		       ;; Note2: This algorithm seems to work well when N is at least 25 times M.
		       (setq kernelID ^vectorInnerProduct)
		       (setq c Cs)
		       (setq M columns)
		       (setq N rows)
		       (setq X (new Vector: Object: N))
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
		       (smoRegress X Y kernelID tol errStop maxGenerations)
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
          )) ; end testName Case srandom
         ;; testName Case mixedRandom 
         ((= testName mixedRandom:)
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
		       (setq M columns)
		       (setq N rows)
		       (setq X (new Vector: Object: N))
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
		       (smoRegress X Y kernelID tol errStop maxGenerations)
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
          )) ; end testName Case mixedRandom
         ;; testName Case randomSigmoid 
         ((= testName randomSigmoid:)
          (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: The inputs, X, are restricted to the sigmoid domain.
		       ;; Note2: We support a bias by having X[0] == 1 for all N.
		       ;; Note3: This algorithm seems to work well when N is at least 25 times M.
		       (setq kernelID ^vectorInnerProduct)
		       (setq c Cs)
		       (setq M columns)
		       (setq N rows)
		       (setq X (new Vector: Object: N))
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
		       (smoRegress X Y kernelID tol errStop maxGenerations)
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
          )) ; end testName Case linear
         ;; testName Case square 
         ((= testName square:)
          (begin
		       ;; Create a test polynomial regression where y = -11.2 + C[0]*X[0] - C[1]*(X[1]**2) + C[2]*X[2] - C[3]*(X[3]**2) ...
		       ;; Note: We support a bias by having X[0] == 1 for all N.
		       (setq kernelID ^vectorSquareInnerProduct)
		       (setq c Cs)
		       (setq M columns)
		       (setq N rows)
		       (setq X (new Vector: Object: N))
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
		       (smoRegress X Y kernelID tol errStop maxGenerations)
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
          )) ; end testName Case square
         ;; testName Case linearSquare 
         ((= testName linearSquare:)
          (begin
		       ;; Create a test polynomial regression where y = C[0]*X[0] + C[1]*X[1] - C[2]*X[2] + C[3]*X[3] - C[4]*X[4] ...
		       ;; Note1: We support a bias by having X[0] == 1 for all N.
		       (setq kernelID ^vectorSquareInnerProduct)
		       (setq c Cs)
		       (setq M columns)
		       (setq N rows)
		       (setq X (new Vector: Object: N))
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
		       (smoRegress X Y kernelID tol errStop maxGenerations)
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
          )) ; end testName Case square
         ;; testName Case tan 
         ((= testName tan:)
          (begin
		       ;; Create a test polynomial regression where y = -11.2 + C[0]*X[0] - C[1]*(X[1]**2) + C[2]*X[2] - C[3]*(X[3]**2) ...
		       ;; Note: We support a bias by having X[0] == 1 for all N.
		       (setq kernelID ^vectorTanInnerProduct)
		       (setq c Cs)
		       (setq M columns)
		       (setq N rows)
		       (setq X (new Vector: Object: N))
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
		       (smoRegress X Y kernelID tol errStop maxGenerations)
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
          )) ; end testName Case tan
         ;; testName Case log 
         ((= testName log:)
          (begin
		       ;; Create a test polynomial regression where y = -11.2 + C[0]*X[0] - C[1]*(X[1]**2) + C[2]*X[2] - C[3]*(X[3]**2) ...
		       ;; Note: We support a bias by having X[0] == 1 for all N.
		       (setq kernelID ^vectorLogInnerProduct)
		       (setq c Cs)
		       (setq M columns)
		       (setq N rows)
		       (setq X (new Vector: Object: N))
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
		       (smoRegress X Y kernelID tol errStop maxGenerations)
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
          )) ; end testName Case log
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








;;**EXPORTKEY**:math:symbolicMath
(defriend math:symbolicMath()
;; *******************************************************************
;; summary:  Manages symbolic math problems. This Lambda contains
;;           a number of expert system rules about algebra which
;;           are used to evaluate and simplify symbolic algebra
;;           problems.
;; Args:     none:    
;; Return:   true         Always returns true at startup time.
;; *******************************************************************
    vars:(name s i n commands keys)
    pvars:(;; Persistent Variables
           myVariables             ;; The memory for symbolic math variables.
           inKB                    ;; The knowledgebase of input transformation rules.
           mathKB                  ;; The knowledgebase of symbolic math rules.
           myExplanation           ;; The explanation of symbolic math rules.
           ;; Child Lambdas
           errorHandler            ;; Called with user input from HTML form.
           evaluate                ;; Simplify user input formula and return a String.
           prettyPrint             ;; Pretty print the output formula.
           rulesLib              ;; Rule based expert system for symbolic math.
           showVariables           ;; Show the variables currently stored in symbolic math.
           ) ;; end of pvars
    ;; *******************
    ;; Define child Lambdas
    ;; *******************
    ;; Manages any unforseen errors which occur.
    (defun errorHandler(errMsg) (append "I got an internal error: " errMsg))
    ;; Simplify user input formula and return a String.
    (defun evaluate(inFormula)
        vars:(stemp mathFormula outFormula outString)
        (onError errorHandler)
        ;; If the formula is empty, return a query.
        (if (= inFormula "") (return "what?"))
        ;; This is a request to evaluate an algebraic formula?
        (if (isString inFormula) (setq inFormula (lisp inFormula arithmetic:)))
        (if (not (isPair inFormula)) (setq inFormula (list inFormula)))
        (if (= inKB #void) (math.symbolicMath))
        (setq mathFormula (inKB.apply inFormula))
        (setq myExplanation (append "<HR><H4><U>My reasoning at each evaluation step:</U></H4><BR><P> " (substitute (substitute inKB.explanation _eol "</P><P>") "Rule [" "Rule [c") "</P><P>"))
        (setq outFormula (mathKB.apply mathFormula))
        (setq myExplanation (append (substitute myExplanation "Final result" "Rule [c] replacing") (substitute mathKB.explanation _eol "</P><P>") "</P>"))
        (setq outString (prettyPrint outFormula false))
        outString) ;; end evaluate
    ;; Pretty print the output formula.
    (defun prettyPrint(outFormula encloseSW)
        vars:(n N
              top a1 a2
              (outString "") 
              (op #{+ + - - * * / /})
              (fn #{acos acos asin asin atan atan cos cos cosh cosh deg deg exp exp log log log10 log10 log2 log2 rad rad sin sin sinh sinh sqrt sqrt tan tan})
              (bn #{expt expt logbase logbase})
              ) ; end temporary variables
        (cond
          ;; Display all atoms as is.
          ((isAtom outFormula) (setq outString (string outFormula true)))
          ;; Display all atoms as is.
          (else
            (cond
              ;; Is this a biniary function call?
              ((isMember (setq top outFormula[0]) bn) (setq outString (append bn[top] "(" (prettyPrint outFormula[1] true) ","  (prettyPrint outFormula[2] true) ")")))
              ;; Is this a uniary function call?
              ((isMember (setq top outFormula[0]) fn)
               (cond
                ;; Is the argument a singleton? 
                ((isAtom (setq a1 outFormula[1])) (setq outString (append fn[top] "(" (prettyPrint outFormula[1] true) ")")))
                ;; Is the argument a singleton? 
                ((isPair (setq a1 outFormula[1])) (setq outString (append fn[top] (prettyPrint outFormula[1] true))))                
                )) ; end uniary case
              ;; Is this a binary operator?
              ((isMember (setq top outFormula[0]) op)
               (if (= encloseSW true) 
                   (setq outString (append "(" (prettyPrint outFormula[1] true) " " op[top] " " (prettyPrint outFormula[2] true) ")"))
                   (setq outString (append (prettyPrint outFormula[1] true) " " op[top] " " (prettyPrint outFormula[2] true)))
                   ))
              ;; Is this a list of values?
              (else
               (begin
                (setq N (length outFormula))
                (loop for n from 0 until N do
                   (setq outString (append outString " " (prettyPrint outFormula[n] encloseSW) " " ))
                   ) ; end loop
                (setq outString (trim outString))
                )) ; end inner else case
              ) ; end inner cond 
           ) ; end pair case
          ) ; end outter cond
        outString) ;; end prettyPrint
    ;; Show the variables currently stored in symbolic math.
    (defun showVariables()
        vars:(n N answerString)
        (setq answerString _eol)
        (setq N (length myVariables))
        (loop for n from 0 until N do
          ;; Display all atoms as is.
          ;(setq answerString (append answerString myVariables[n 0] " = " (string myVariables[n 1] true) _eol))
          (setq answerString (append answerString myVariables[n 0] " = " (prettyPrint myVariables[n 1] false) _eol))
          ) ; end loop
        answerString) ;; end showVariables
    ;; **********************************
    ;; Initialize this Lambda at start up.
    ;; **********************************
    (onError errorHandler)
    ;;************************************************************************
    ;;  Create a new copy of the rules Lambda and create the rule database
    ;;  to be used for this symbolic math demonstration application.
    ;;************************************************************************
    (setq myVariables (new Dictionary:))
    (setq inKB (new rulesLib))
    (setq inKB.singlePass false)
    (setq inKB.verbose false)
    (setq mathKB (new rulesLib))
    (setq mathKB.singlePass false)
    ;; ****************************************************
    ;; INPUT RULE DECLARATIONS
    ;; ****************************************************
    ;; Declare user defined function for use in input rules
    (inKB.assert  $BINARY:(lambda(x) vars:((d #{expt expt logbase logbase})) (if (isMember x d) d[x])))
    (inKB.assert  $EXP:(lambda(x) vars:((op #{+ + - - * * / /})
                                        (fn #{acos acos asin asin atan atan cos cos cosh cosh deg deg exp exp log log log10 log10 log2 log2 rad rad sin sin sinh sinh sqrt sqrt tan tan}))
                                 (if (or (isPair x) 
                                         (isNumber x)
                                         (and (isSymbol x)
                                              (not (isMember x op))
                                              (not (isMember x fn))
                                              (isCharAlphabetic x)))
                                          x)))
    (inKB.assert  $FACT:(lambda(a x op y b) 
                             vars:(p) 
                             (setq p (list op x y)) 
                             (if (<> b #void) 
                                 (setq p (append (list p) b))) 
                             (if (<> a #void) 
                                 (setq p (append a p))) 
                             p))
    (inKB.assert  $UFUN:(lambda(a fn x b) 
                             vars:(p) 
                             (setq p (append (list fn) x)) 
                             (if (<> a #void) 
                                 (setq p (append a (list (list fn x))))) 
                             (if (<> b #void) 
                                 (setq p (append (list p) b))) 
                             p))
    (inKB.assert  $NUM:(lambda(x) (if (isNumber x) x)))
    (inKB.assert  $OP:(lambda(x) vars:((d #{+ + - - * * / /})) (if (isMember x d) d[x])))
    (inKB.assert  $POP:(lambda(a x op1 y op2 z b) 
                             vars:(p) 
                             (setq p (list op2 (list op1 x y) z)) 
                             (if (<> b #void) 
                                 (setq p (append (list p) b))) 
                             (if (<> a #void) 
                                 (setq p (append a p))) p))
    (inKB.assert  $SYM:(lambda(s) (if (and (isSymbol s) (isCharAlphabetic s[0])) s)))
    (inKB.assert  $TERM:(lambda(x) vars:((op #{+ + - - * * / /})
                                         (fn #{acos acos asin asin atan atan cos cos cosh cosh deg deg exp exp log log log10 log10 log2 log2 rad rad sin sin sinh sinh sqrt sqrt tan tan}))
                                 (if (or (isPair x) 
                                         (and (isSymbol x)
                                              (not (isMember x op))
                                              (not (isMember x fn))
                                              (isCharAlphabetic x)))
                                          x)))
    (inKB.assert  $UNARY:(lambda(x) vars:((d #{acos acos asin asin atan atan cos cos cosh cosh deg deg exp exp log log log10 log10 log2 log2 rad rad sin sin sinh sinh sqrt sqrt tan tan})) (if (isMember x d) d[x])))
    ;; Serial expression to subexpression pairs conversion rules
    (inKB.assert  '(<$M=$SYM> = <$FN=$UNARY> <$X=$EXP> $b*) '(<$> $M = (($FN $X)) $b))
    (inKB.assert  '(<$M=$SYM> = <$OP1=$OP> <$X=$EXP> <$Y=$EXP> $b*) '(<$> $M = (($OP1 $X $Y)) $b))
    (inKB.assert  '($a* <$OP1=$OP> <$FN=$UNARY> <$X=$EXP> $b*) '(<$> $a $OP1 (($FN $X)) $b))
    (inKB.assert  '($a* <$FN=$UNARY> <$X=$EXP> <$OP1=$OP> $b*) '(<$> $a (($FN $X)) $OP1 $b))
    (inKB.assert  '($a* <$OP1=$OP> <$FN=$BINARY> ($X $Y) $b*) '(<$> $a $OP1 (($FN $X $Y)) $b))
    (inKB.assert  '($a* <$FN=$BINARY> ($X $Y) <$OP1=$OP> $b*) '(<$> $a (($FN $X $Y)) $OP1 $b))
    (inKB.assert  '($a* <$FN=$BINARY> ($X $Y) $b*) '(<$> $a (($FN $X $Y)) $b))
    (inKB.assert  '(<$FN=$BINARY> ($X $Y)) '($FN $X $Y))
    (inKB.assert  '($a* <$X=$EXP> <$OP1=$OP> <$Y=$EXP> <$OP2=$OP> <$Z=$EXP> $b*) '(<$POP> $a $X $OP1 $Y $OP2 $Z $b))
    (inKB.assert  '($a* <$X=$EXP> <$OP1=$OP> <$Y=$EXP> $b*) '(<$> $a ($OP1 $X $Y) $b))
    ;; Factor recognition rules
    ;; ****************************************************
    ;; MATH RULE DECLARATIONS
    ;; ****************************************************
    ;; Declare user defined function for use in rules
    (mathKB.assert  $VAR:(lambda(s) (if (and (isSymbol s) (isCharAlphabetic s[0]) (isMember s myVariables)) myVariables[s])))
    (mathKB.assert  $SYM:(lambda(s) (if (and (isSymbol s) (isCharAlphabetic s[0])) s)))
    (mathKB.assert  $SET:(lambda(s v) (setq myVariables[s] (list v)) (list v)))
    (mathKB.assert  $SUB:(lambda(a v b) 
                           vars:(p) 
                           (setq p v) 
                           (if (<> b #void) 
                               (setq p (append (list p) b))) 
                           (if (<> a #void) 
                               (setq p (append a p))) p))
    (mathKB.assert  $AOP:(lambda(x) vars:((d #{+ + - - })) (if (isMember x d) d[x])))
    (mathKB.assert  $EXP:(lambda(x) vars:((op #{+ + - - * * / /})
                                          (fn #{acos acos asin asin atan atan cos cos cosh cosh deg deg exp exp log log log10 log10 log2 log2 rad rad sin sin sinh sinh sqrt sqrt tan tan}))
                                   (if (or (isPair x) 
                                           (isNumber x)
                                           (and (isSymbol x)
                                                (not (isMember x op))
                                                (not (isMember x fn))
                                                (isCharAlphabetic x)))
                                            x)))
    (mathKB.assert  $PFN:(lambda(a x fn1 y fn2 z b) 
                             vars:(p) 
                             (setq p (list (list x fn1 y) fn2 z)) 
                             (if (<> b #void) 
                                 (setq p (append (list p) b))) 
                             (if (<> a #void) 
                                 (setq p (append a p))) p))
    (mathKB.assert  $FOLD:(lambda(op x y) vars:(f) (setq f (getGlobalValue (symbol op))) (f x y)))
    (mathKB.assert  $UFOLD:(lambda(fn x) vars:(f) (setq f (getGlobalValue (symbol (downcase (string fn))))) (f x)))
    (mathKB.assert  $NUM:(lambda(x) (if (isNumber x) x)))
    (mathKB.assert  $OP:(lambda(x) vars:((d #{+ + - - * * / /})) (if (isMember x d) d[x])))
    (mathKB.assert  $UNARY:(lambda(x) vars:((d #{acos acos asin asin atan atan cos cos cosh cosh deg deg exp exp log log log10 log10 log2 log2 rad rad sin sin sinh sinh sqrt sqrt tan tan})) (if (isMember x d) d[x])))
    (mathKB.assert  $TERM:(lambda(x) vars:((d #{acos acos asin asin atan atan cos cos cosh cosh deg deg exp exp log log log10 log10 log2 log2 rad rad sin sin sinh sinh sqrt sqrt tan tan}))
                                 (if (or (isPair x) 
                                         (and (isSymbol x)
                                              (not (isMember x d))
                                              (isCharAlphabetic x)))
                                          x)))
    (mathKB.assert  $FACT:(lambda(a x op y b) 
                             vars:(p) 
                             (setq p (list x op y)) 
                             (if (<> b #void) 
                                 (setq p (append (list p) b))) 
                             (if (<> a #void) 
                                 (setq p (append a p))) p))
    ;; Symbolic variable assignment and substitution rules
    (mathKB.assert  '(<$S=$SYM> = $V*) '(<$SET> $S $V))
    (mathKB.assert  '($X* <$V=$VAR> $Y*) '(<$SUB> $X $V $Y))
    ;; Constant folding rules
    (mathKB.assert  '(<$F=$OP> <$X=$NUM> <$Z=$NUM>) '(<$FOLD> $F $X $Z))
    (mathKB.assert  '(<$F=$BINARY> <$X=$NUM> <$Z=$NUM>) '(<$FOLD> $F $X $Z))
    (mathKB.assert  '(<$F=$UNARY> <$X=$NUM>) '(<$UFOLD> $F $X))
    ;; Algebraic expression reduction rules
    (mathKB.assert  '(+ $X $X) '(* 2 $X))
    (mathKB.assert  '(+ $X 0) '$X)
    (mathKB.assert  '(+ 0 $X) '$X)
    (mathKB.assert  '(- $X 0) '$X)
    (mathKB.assert  '(- $X $X) 0)
    (mathKB.assert  '(+ (- $X $Y) $Y) '$X)
    (mathKB.assert  '(- (+ $X $Y) $Y) '$X)
    (mathKB.assert  '(- (+ $X $Y) $X) '$Y)
    (mathKB.assert  '(+ (- $X $Y) $X) '(- (* 2 $X) $Y))
    (mathKB.assert  '(/ $X $X) 1)
    (mathKB.assert  '(/ $X 1) '$X)
    (mathKB.assert  '(/ 0 $X) 0)
    (mathKB.assert  '(/ $X $X) 1)
    (mathKB.assert  '(* $X 0) 0)
    (mathKB.assert  '(* 0 $X) 0)
    (mathKB.assert  '(* $X 1) '$X)
    (mathKB.assert  '(* 1 $X) '$X)
    (mathKB.assert  '(/ (* $Y $X) $X) '$Y)
    (mathKB.assert  '(/ (* $Y $X) $Y) '$X)
    (mathKB.assert  '(* (/ $Y $X) $X) '$Y)
    (mathKB.assert  '(/ (* $Y $X) (* $Z $X)) '(* (/ $Y $Z) $X))
    ;; Excess parentheses reduction rules
    (mathKB.assert  '($X) '$X)
    (mathKB.assert  '(($X*)) '$X)
    ;; Algebraic expression reordering rules
    (mathKB.assert  '(<$OP=$AOP> <$X=$NUM> <$Y=$TERM>) '($OP $Y $X))
    (mathKB.assert  '(* <$X=$TERM> <$Y=$NUM>) '(* $Y $X))
    ;; Algebraic expression reduction rules
    (mathKB.assert  '(+ (+ <$X=$TERM> <$Y=$NUM>) <$Z=$NUM>) '(+ $X (<$FOLD> + $Y $Z)))
    (mathKB.assert  '(+ (+ <$X=$NUM> <$Y=$TERM>) <$Z=$NUM>) '(+ (<$FOLD> + $X $Z) $Y))
    (mathKB.assert  '(- (+ <$X=$TERM> <$Y=$NUM>) <$Z=$NUM>) '(+ $X (<$FOLD> - $Y $Z)))
    (mathKB.assert  '(- (+ <$X=$NUM> <$Y=$TERM>) <$Z=$NUM>) '(+ (<$FOLD> - $X $Z) $Y))
    (mathKB.assert  '(* (+ <$X=$TERM> <$Y=$NUM>) <$Z=$NUM>) '(+ ($X * $Z) (<$FOLD> * $Y $Z)))
    (mathKB.assert  '(* (+ <$X=$NUM> <$Y=$TERM>) <$Z=$NUM>) '(+ (<$FOLD> - $X $Z) (* $Y $Z)))
    (mathKB.assert  '(+ (- <$X=$TERM> <$Y=$NUM>) <$Z=$NUM>) '(+ $X (<$FOLD> - $Z $Y)))
    (mathKB.assert  '(+ (- <$X=$NUM> <$Y=$TERM>) <$Z=$NUM>) '(- (<$FOLD> + $X $Z) $Y))
    (mathKB.assert  '(- (- <$X=$TERM> <$Y=$NUM>) <$Z=$NUM>) '(- $X (<$FOLD> + $Y $Z)))
    (mathKB.assert  '(- (- <$X=$NUM> <$Y=$TERM>) <$Z=$NUM>) '(- (<$FOLD> - $X $Z) $Y))
    (mathKB.assert  '(* (- <$X=$TERM> <$Y=$NUM>) <$Z=$NUM>) '(- (* $X $Z) (<$FOLD> * $Y $Z)))
    (mathKB.assert  '(* (- <$X=$NUM> <$Y=$TERM>) <$Z=$NUM>) '(- (<$FOLD> * $X $Z) (* $Y $Z)))
    (mathKB.assert  '(* (* <$X=$TERM> <$Y=$NUM>) <$Z=$NUM>) '(* $X (<$FOLD> * $Y $Z)))
    (mathKB.assert  '(* (* <$X=$NUM> <$Y=$TERM>) <$Z=$NUM>) '(* (<$FOLD> * $X $Z) $Y))
    (mathKB.assert  '(/ (* <$X=$TERM> <$Y=$NUM>) <$Z=$NUM>) '(* $X (<$FOLD> / $Y $Z)))
    (mathKB.assert  '(/ (* <$X=$NUM> <$Y=$TERM>) <$Z=$NUM>) '(* (<$FOLD> / $X $Z) $Y))
    (mathKB.assert  '(* (/ <$X=$TERM> <$Y=$NUM>) <$Z=$NUM>) '(* $X (<$FOLD> / $Z $Y)))
    (mathKB.assert  '(* (/ <$X=$NUM> <$Y=$TERM>) <$Z=$NUM>) '(/ (<$FOLD> * $X $Z) $Y))
    (mathKB.assert  '(/ (/ <$X=$TERM> <$Y=$NUM>) <$Z=$NUM>) '(/ $X (<$FOLD> * $Y $Z)))
    (mathKB.assert  '(/ (/ <$X=$NUM> <$Y=$TERM>) <$Z=$NUM>) '(/ (<$FOLD> / $X $Z) $Y))
    ;; Trigonometric expression reduction rules
    (mathKB.assert  '(LOG (EXP $X)) '$X)
    (mathKB.assert  '(EXP (LOG $X)) '$X)
    (mathKB.assert  '(LOGBASE (EXPT $X $Y) $X) '$Y)
    (mathKB.assert  '(LOGBASE $X $X) '$X)
    (mathKB.assert  '(EXPT $X 1) '$X)
    (mathKB.assert  '(EXPT $X 0) 1)
    ;; Extended Algebraic expression reduction rules
    (mathKB.assert  '(<$Y2=$AOP> (* <$N1=$NUM> (<$Y1=$AOP> <$X1=$TERM> <$N2=$NUM>)) <$N3=$NUM>) '($Y1 (* $N1 $X1) ($Y2 (<$FOLD> * $N1 $N2) $N3)))
    ;; Factor recognition rules
    true) ;; end symbolicMath




























;;**EXPORTKEY**:math:symbolicMath:rulesLib
(deforphan math.symbolicMath rulesLib()
;; ********************************************************************
;; summary:  Create, maintain, and apply a Dictionary of IF -> THEN list
;;           substitution rules for transforming a list and all its sub
;;           lists. Anywhere a sub list matches one of the IF rules, the
;;           THEN rule is substituted in place of the original sub list.
;;           This Lambda also supports single or multiple pass substitution
;;           rule application.
;; Parms:    none
;; return:   true
;; ********************************************************************
   pvars:(rulesDic                 ;; Dictionary of morph substitution rules
          singlePass               ;; Switch for single pass morph substitution
          explanation              ;; Contains the explanation after apply
          verbose                  ;; Switch for displaying each explanation step on the console.
          changeCount              ;; Number of rule based substitutions
          (morphFail |*failure*|:) ;; Morph rule failure RHS value
          (maxPasses 200)          ;; Maximum number of passes before issuing error (singlePass = false)
          passCount                ;; Current number of passes already executed by apply
          ;; Methods list 
          appendList               ;; Child to append to items together to always make a list.
          apply                    ;; Child to apply all rules in the rules dictionary to morph a list
          assert                   ;; Child to add a single new rule to the rules dictionary 
          bind                     ;; Child to bind dummy variables to values in a sub list 
          doClear                  ;; Child to clear the rules dictionary 
          isMatch                  ;; Child to match a regular expression with a candidate expression 
          len                      ;; Child to return the number of rules in the rules Dictionary 
          listRules                ;; Child for morph to call during rule application
          new                      ;; Child for initializing the newly created list rules Lambda
          ref1                     ;; Child to return the THEN form corresponding to an IF form 
          rulesDictionary          ;; Friend to manage a dictionary of rules
          set1                     ;; Child to add a single new rule to the rules dictionary 
          setFailure               ;; Child to set the rule failure RHS value
          setMaxPasses             ;; Child to set the maximum apply passes limit 
          setSinglePass            ;; Child to set the single pass switch 
          setVerbose               ;; Child to set the verbose switch 
          unassert                 ;; Child to delete a single rule from the rules dictionary 
          ) ;; end of persistent variables
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> new #void) (return true))
   ;; Initialize the inline child Lambdas.
   ;; Append the two arguments into a list.
   (defun appendList(one two ...)
       vars:(result argc i)
       (cond 
           ((and (isPair one) (isPair two)) (setq result (append one two)))
           ((= one #void) (setq result two))
           ((= two #void) (setq result one))
           ((isPair one) (setq result (append one (list two))))
           (else (setq result (list one two)))
           ) ; end cond
       (setq argc (argCount))
       (loop for i from 2 until argc do
           (setq result (appendList result (argFetch i)))
           ) ;; end loop
       result) ;; end appendList
   (defun apply(theList)
       vars:(x)
       (setq x theList)
       (setq passCount 0)
       (setq explanation "")
       Retry::
       (if (> passCount maxPasses) (error "listRule_Pass" "Exceeded maximum number of apply rules."))
       (setq changeCount 0)
       (setq x (morph (list x) listRules morphFail))
       (if (isPair x) (setq x (car x)))
       (if (and (> changeCount 0) (= singlePass false)(isPair x)) (goto Retry:))
       (setq explanation (append explanation "Final result: " (string theList true) " ==> "  (string x true)))
       (if (= verbose true)
           (writeln "Final result: " (string theList true) " ==> "  (string x true)))
       x) ;; end of apply
   (defun assert(ifForm thenForm) (setq rulesDic[ifForm] thenForm))
   (defun bind(mask dic)
   ;; ********************************************************************
   ;; summary:  Bind the specified mask expression (mask) with a Dictionary
   ;;           (dic) containing wild card variables ($Xn) which are bound 
   ;;           to values.
   ;; Parms:    mask:    The mask expression with dummy variables.
   ;;           dic:     The Dictionary of bound variables.
   ;; return:   mask:    The new expression with bound dummy variables replaced.
   ;; ********************************************************************
      vars:(f i n s result tail)
      ;; If the mask is a quoted pair, then remove the extra quotes with eval.
      (if (= (type mask) QuotedPair:) (setq mask (eval mask)))
      ;; Bind the mask with the Dictionary values.
      (cond ;; If the mask is a List whose first element is an appendList
            ;; token value (ie <$>), then recursively bind each 
            ;; remaining element of the List, and return the result of 
            ;; applying appendList to the final bound List.
            ((and (isPair mask) (= mask[0] "<$>"))
             (begin
                (setq n (length mask))
                (loop for i from 1 until n do (setq mask[i] (bind mask[i] dic)))
                (setq tail (cdr mask))
                (if (<= (length tail) 1)
                    (setq result tail) 
                    (setq result (^apply appendList (cdr mask))))))                 
            ;; If the mask is a List whose first element is a lambda wild
            ;; card variable (ie <$x>), then recursively bind each 
            ;; remaining element of the List, and return the result of 
            ;; applying the wild card lambda value to the final bound List.
            ((and (isPair mask) 
                  (isSymbol mask[0]) 
                  (= (left mask[0] 2) "<$")
                  (= (right mask[0] 1) ">"))
             (begin
                (setq n (length mask))
                (loop for i from 1 until n do (setq mask[i] (bind mask[i] dic)))
                (setq n (length mask[0]))
                (setq s (symbol (mid mask[0] 1 (subi n 2))))
                (setq f rulesDic[s])
                (setq result (^apply f (cdr mask))))) 
            ;; If the mask is a List, then recursively bind each 
            ;; element of the List, and return the final bound List.
            ((isPair mask)
             (setq n (length mask))
             (loop for i from 0 until n do (setq mask[i] (bind mask[i] dic)))
             (setq result mask)) 
            ;; If the mask is a wild card symbol, then return the
            ;; value of the wildcard symbol in the bindings Dictionary.
            ((and (isSymbol mask) (= mask[0] #\$)) 
             (setq result dic[mask]))
            ;; If the mask is anything else, then return the mask as is.
            (else (setq result mask))
            ) ;; end cond
      result) ;; end of bind
   (defun doClear() (new))
   (defun isMatch(reg exp dic)
   ;; ********************************************************************
   ;; summary:  Match the specified regular expression (reg) with a candidate
   ;;           expression (exp). The regular expression contains dummy
   ;;           variables ($Xn) which are bound to values in the candidate
   ;;           expression using the Dictionary (dic). The act of matching
   ;;           an expression produces a Dictionary with the correct bindings.
   ;; Parms:    reg:     The regular expression with unbound variables ($Xn).
   ;;           exp:     The natural expression to match with the regular.
   ;;           dic:     The Dictionary of bound variables.
   ;; return:   bool:    True if match, or false if no match.
   ;; ********************************************************************
      vars:(i n s pos leftName rightName
            regLeftHand regRightHand wildcd
            expLeftHand expRightHand tmpDic
            recognizeProc recognizeResult nreg 
            ) ;; end of temporary variables
      ;; Any error results in a no match!
      ;(onError (lambda(err) false))
      ;; Attempt to match the regular expression with the input expression.
      (cond ;; If both expressions are Pairs and the first term in the regular
            ;; expression is a named wild card ending with an asterisk $name*,
            ;; then the wild card is matched to the left hand remainder of the
            ;; input expression, and we try to match the right hand portions of the
            ;; regular expression with any right hand portion of the input expression. 
            ((and (isPair reg) (isPair exp) 
                  (isSymbol reg[0]) 
                  (= (left reg[0] 1) "$") 
                  (= (right reg[0] 1) "*")) 
             (begin
                ;; Isolate the wild card name.
                (setq wildcd (mid reg[0] 0 (sub1 (length reg[0]))))
                (setq regRightHand (cdr reg))
                (setq expRightHand exp)
                (setq expLeftHand #void)
                ;; Match immediately, if the wild card terminates the regular expression.
                (if (= regRightHand #void)
                    (begin
                       (setq dic[wildcd] exp)  
                       (return true)
                       )) ;; end if
                ;; Try to match with any right hand portion of the input expression.
                (while (isPair expRightHand)
                    (setq tmpDic (copy dic))
                    (if (isMatch regRightHand expRightHand tmpDic)
                        (if (and (<> tmpDic[wildcd] #void) (compareNE tmpDic[wildcd] expLeftHand))
                            (return false)
                            (begin (setq tmpDic[wildcd] expLeftHand) (objectToDictionary dic tmpDic) (return true))
                            ) ;; end then
                        (begin
                           (setq nreg (car expRightHand))
                           (setq expRightHand (cdr expRightHand))
                           (if (isPair expLeftHand) 
                               (setq expLeftHand (append expLeftHand (list nreg)))
                               (if (= expLeftHand #void) 
                                   (setq expLeftHand (list nreg))
                                   (setq expLeftHand (append expLeftHand (list nreg))))
                               )) ;; end if
                        ) ;; end if
                    ) ;; end while
                false))
            ;; If both expressions are Pairs and the second term in the regular
            ;; expression is a named wild card ending with an asterisk $name*,
            ;; And the input expression has length one, 
            ;; then the wild card is matched to the left hand remainder of the
            ;; input expression, and we try to match the right hand portions of the
            ;; regular expression with any right hand portion of the input expression. 
            ((and (isPair reg) (isPair exp) 
                  (= (length reg) 2) 
                  (isSymbol reg[1]) 
                  (= (left reg[1] 1) "$") 
                  (= (right reg[1] 1) "*") 
                  (= (length exp) 1)) 
             (begin
                (setq tmpDic (copy dic))
                (setq wildcd (mid reg[1] 0 (sub1 (length reg[1]))))
                (if (<> tmpDic[wildcd] #void) (return false))
                (if (isMatch (car reg) (car exp) tmpDic)
                    (begin
                       (objectToDictionary dic tmpDic)
                       (return true))
                    (return false))
                    )) ;; end empty wild card case
            ;; If both expressions are Pairs, try to match their cars and cdrs.
            ((and (isPair reg) (isPair exp))
             (begin
                (setq tmpDic (copy dic))
                (if (and (isMatch (car reg) (car exp) tmpDic) (isMatch (cdr reg) (cdr exp) tmpDic))
                    (begin
                       (objectToDictionary dic tmpDic)
                       (return true))
                    (return false))
                    )) ;; end both pairs case
            ;; If the regular expression is a wild card rule <$name=$rule>,
            ;; and the rule (retrieved from the rules dictionary) returns true,
            ;; then match and bind the wild card except in the case of a
            ;; previous binding with the same wild card which is not equal 
            ;; to this attempted binding.
            ((and (isSymbol reg) (= (left reg 2) "<$"))
             (begin
                (setq nreg (mid reg 1 (subi (length reg) 2)))  
                (cond
                   ((isBoolean (setq pos (find "=$" (string nreg)))) false)
                   ((not (isLambda (setq recognizeProc rulesDic[(setq rightName (mid nreg (addi pos 1) 10000))]))) false)
                   ((= (setq recognizeResult (recognizeProc exp)) false) false)
                   ((= dic[(setq leftName (left nreg pos))] #void) (setq dic[leftName] recognizeResult) true)
                   ((isEqual dic[leftName] recognizeResult) true)
                   (else false))))
            ;; If the regular expression is a wild card symbol (begins with $),
            ;; then match and bind the wild card except in the case of a
            ;; previous binding with the same wild card which is not equal 
            ;; to this attempted binding.
            ((and (isSymbol reg) (= (left reg 1) "$"))
             (cond ((= dic[reg] #void) (setq dic[reg] exp) true)
                   ((isEqual dic[reg] exp) true)
                   (else false)))
            ;; If both expressions are equal, then this is a match!
            ((= reg exp) 
             true)
            ;; Everything else is a no match!
            (else false)
            ) ;; end cond
      ) ;; end of isMatch
   (defun len() (length rulesDic))
   (defun listRules(sexp)
   ;; ********************************************************************
   ;; summary:  If the head of the specified sub list (sexp) matches a rule
   ;;           (ifForm) in the rules Dictionary, then apply the substitution 
   ;;           rule (thenForm) to the sub list passed by the morph procedure.
   ;; ********************************************************************
      vars:(i n dic cpy ret)
      ;(onError (lambda(err) err))      
      (setq n (length rulesDic))
      (loop for i from 0 until n do
          (setq dic (makeDictionary))
          (if (isMatch rulesDic[i 0] sexp dic)
              (begin
                 (++ changeCount)
                 (setq cpy (copy rulesDic[i 1]))
                 (setq ret (bind cpy dic))
                 (setq explanation (append explanation 
                                           "Rule [" i "] replacing: " 
                                           (string sexp true) " ==> "  
                                           (string ret true) _eol))
                 (if (= verbose true) 
                     (writeln "Rule [" i "] replacing: " (string sexp true) " ==> "  (string ret true)))
                 (return ret)
              )) ;; end if
          ) ;; end of loop
      morphFail) ;; end of listRules
   (defun new()
      (setq rulesDic (^new rulesDictionary))
      (setq singlePass true)
      (setq verbose false)) ;; end of new
   (defun ref1(ifForm) 
        (if (isSymbol ifForm) (return (myself)[Pv:][ifForm])) ;; Supports Lambda polymorphism.
        (if (isNumber ifForm)
            (return (spair rulesDic[ifForm 0] rulesDic[ifForm 1]))
            rulesDic[ifForm])) ;; end of ref1
   (defun set1(ifForm thenForm) 
        (if (= ifForm singlePass:) (return (setq singlePass thenForm))) ;; Supports Lambda polymorphism.
        (if (= ifForm verbose:) (return (setq verbose thenForm))) ;; Supports Lambda polymorphism.
        (assert ifForm thenForm)) ;; end of set1
   (defun setFailure(RHSvalue) (setq morphFail RHSvalue))
   (defun setMaxPasses(limit) (setq maxPasses limit))
   (defun setSinglePass(swt) (setq singlePass swt))
   (defun setVerbose(swt) (setq verbose swt))
   (defun unassert(ifForm) (setq rulesDic[ifForm] #void))
   ;; Initialize the Lambda and assign a new rules Dictionary.
   (new)) ;; end of rulesLib


;;  ***NOTES***:
;; 
;; This Lambda provides a rule based capability for transforming
;; an old list into a new list.
;;
;; This Lambda accepts and saves a set of transformation rules in
;; its internal rules dictionary. Each rule is entered, by the
;; programmer, in two parts -- an IF form followed by a THEN form.
;; An example might be:
;; 
;;     (rulesLib.assert '(x + y) '(addi x y))
;;
;; We may then apply the above rules against a list as follows: 
;;
;;     (rulesLib.apply '(x + y))   ==>  '(addi x y)
;;
;; This Lambda supports wild card variables to make rule definitions
;; more flexible for the programmer. An example of wild card rule
;; variables is as follows:
;;
;;     (rulesLib.assert '($X + $Y) '(addi $X $Y))
;;
;; We may then apply the above rules against a list as follows: 
;;
;;     (rulesLib.apply '(m + 10))   ==>  '(addi m 10)
;;
;; The rules and wild card variables operate on sub lists as well
;; as the whole list as follows: 
;;
;;     (rulesLib.apply '((m + 10) + 20))   ==>  '(addi (addi m 10) 20)
;;
;; This Lambda supports named lambda rule definitions which allow
;; more flexible actions to be taken by the programmer during the
;; recognition phase and also during the production phase. Some 
;; examples of named lambda rule definitions is as follows:
;;
;;     (rulesLib.assert $FOLD:(lambda(op x y) vars:(f) (setq f (getGlobalValue (symbol op))) (f x y)))
;;     (rulesLib.assert $NUM:(lambda(x) (if (isNumber x) x)))
;;     (rulesLib.assert $OP:(lambda(x) vars:((d #{+ addi - subi * muli / divi})) (if (isMember x d) d[x])))
;;     (rulesLib.assert '(<$X=$NUM> <$Y=$OP> <$Z=$NUM>) '(<$FOLD> $Y $X $Z))
;;     (rulesLib.assert '($X <$Y=$OP> $Z) '($Y $X $Z))
;;
;; We may then apply the above rules against a list as follows: 
;;
;;     (rulesLib.apply '(m + 10))   ==>  '(addi m 10)
;;     (rulesLib.apply '(5 + 10))   ==>  15
;;
;; Double quoting a right hand production rule causes the append
;; function to be applied to the result as follows:
;;
;;     (rulesLib.assert '($X + $Y) ''(+ $X $Y))
;;     (rulesLib.apply '((5 6 7) + (10 20)))   ==>  '(+ 5 6 7 10 20)
;;
;; This Lambda supports multiple rule definitions up to the limits
;; of available memory. Rules asserted first have precedence over 
;; rules asserted later as follows:
;;
;;     (rulesLib.assert $OP:(lambda(x) vars:((d #{+ addi - subi * muli / divi})) (if (isMemEqv x d) d[x])))
;;     (rulesLib.assert '($X <$Y=$OP> $Z) '($Y $X $Z))
;;     (rulesLib.assert '(addi $X $X) '(muli $X 2))
;;     (rulesLib.assert '(addi $X 0) '$X)
;;
;; We may then apply the multiple rules against a list as follows: 
;;
;;     (rulesLib.apply '(m + m))   ==>  '(addi m m)
;;
;; This Lambda supports multiple passes, during rule application,
;; as follows:
;;
;;     (setq rulesLib.singlePass false)
;;     (rulesLib.apply '(m + m))   ==>  '(muli m 2)
;;     (rulesLib.apply '(m + 0))   ==>  m
;;
;; This Lambda supports asterisk wild card rules anywhere within lists,
;; as follows:
;;
;;     (rulesLib.assert '(min ($X*)) '(<$FN> min $X))
;;     (rulesLib.assert '($X* min)   '(<$FN> min $X))
;;     (rulesLib.assert $FN:(lambda(fn x) (append (list fn) x)))
;;     (rulesLib.apply  '(min (2 3)))  ==>  '(min 2 3)
;;     (rulesLib.apply  '((2 3) min))  ==>  '(min 2 3)
;;
;;





































































;;**EXPORTKEY**:math:symbolicMath:rulesLib:rulesDictionary
(defriend math.symbolicMath.rulesLib rulesDictionary()
;; ********************************************************************
;; summary:  Create, maintain, and apply a Dictionary of IF -> THEN
;;           substitution rules and wild card lambda rules. The wild
;;           card lambda rules are kept separate from the production
;;           rules. The Lambda is designed to behave similar to a 
;;           Dictionary.
;; Parms:    none  
;; return:   true
;; ********************************************************************
   pvars:(leftRULES                ;; Vector of left hand production rules
          rightRULES               ;; Vector of right hand production rules
          lambdaRULES              ;; Dictionary of lambda rules
          ;; Methods list 
          len                      ;; Method to return the number of rules in the rules Dictionary 
          new                      ;; Method to create a new rules Dictionary 
          ref1                     ;; Method to return the THEN form corresponding to an IF form 
          ref2                     ;; Method to return the rules contained in the rules dictionary 
          refCount                 ;; Method to return the number of production rules 
          set1                     ;; Method to add a single new rule to the rules dictionary 
         ) ;; end of persistent variables
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> new #void) (return true))
   ;; Initialize the inline child Lambdas.
   (defun len() (length leftRULES))
   (defun new()
      (setq leftRULES (^new Vector: 0))
      (setq rightRULES (^new Vector: 0))
      (setq lambdaRULES (^new Directory:))
      true) ;; end of new
   (defun ref1(key)
      vars:(i n)
      ;; Are we retrieving wild card lambda rule?
      (if (and (or (isString key) (isSymbol key)) (= key[0] #\$))
          (return lambdaRULES[key]))
      ;; We retrieving right hand production rules.
      (setq n (length leftRULES))
      (loop for i from 0 until n do
          (if (isEqual key leftRULES[i])
              (return rightRULES[i]))
          ) ;; end loop
      #void) ;; end of ref1
   (defun ref2(index1 index2)
      vars:(n)
      (setq n (length leftRULES))
      ;; Are we retrieving a wild card lambda rule?
      (if (>= index1 n)
          (return lambdaRULES[(subi index1 n) index2]))
      ;; Are we retrieving left hand production rules.
      (if (= index2 0)
          (return leftRULES[index1]))
      ;; Are we retrieving right hand production rules.
      (if (= index2 1)
          (return rightRULES[index1]))
      (error "arglist")) ;; end of ref2
   (defun refCount() (length leftRULES))
   (defun set1(key newValue)
      vars:(i n)
      ;; Are we asserting a wild card lambda rule?
      (if (and (isSymbol key) (= key[0] #\$) (<> key $ALL$:))
          (begin
             (if (<> newValue #void)  
                 (return (setq lambdaRULES[key] newValue))) 
             ;; We are deleting a wild card lambda rule.
             (setq i (member key lambdaRULES))
             (if (isNumber i)
                 (vectorDelete lambdaRULES i))
             (return #void))) ;; end deleting lambda rule
      ;; We are asserting a production rule.
      (setq n (length leftRULES))
      (setq i (member key leftRULES))
      (if (isNumber i)
          (if (= newValue #void)
              (begin
                 (vectorDelete leftRULES i)
                 (vectorDelete rightRULES i)
                 (return #void)) ;; end delete production rule
              (begin
                 (setq rightRULES[i] newValue)
                 (return newValue))) ;; end replace production rule
          ) ;; end if
      ;; Do not assert new rules with #void right hand productions.
      (if (= newValue #void) (error "rightHand" (append "rulesDictionary found right hand #void: " key " ==> " newValue)))
      ;; Assert new production rule
      (setq leftRULES[n] key)
      (setq rightRULES[n] newValue)
      newValue) ;; end of set1
   ;; Initialize the Lambda and clear the rules Dictionary.
   (new)) ;; end of rulesDictionary
;;
;; *NOTES*:
;;
;; Rules must be in one of the following forms:
;;
;;      IFFORM                 THENFORM                  DESCRIPTION
;;
;;      $symbol                ..an Lambda..              Lambda wild card rule
;;      $ALL$                  ..anything..              Final production rule
;;      ..other..              ..anything..              Production rule


















































;;**EXPORTKEY**:math:symbolicRegress
(defriend math:symbolicRegress(w Gmax err)
;; *******************************************************************
;; summary:  Returns the input estimator function with numeric coefficients 
;;           optimized against the specified objective function. The 
;;           regression is nonlinear and will minimize any objective function.
;;           Neither the estimator function nor the objective function 
;;           are required to be continuous.
;;
;; 			 This Lambda uses three search technologies for estimator function
;;           optimization: multivariate regression, neural net regression,
;;           and genetic algorithm regression. Genetic algorithms learn
;;           faster when the input data has been crunched into the sigmoid
;;           or the closed bipolar domain. The neural net regression requires 
;;           that the input data be crunched into the sigmoid domain. 
;;
;;           Multivariate regression is deterministic and absolutely accurate,
;;           but limited in scope. Both genetic algorithms and neural nets are
;;           nondeterministic and only appoximately accurate, but are general 
;;			 in scope. Please see the _selfTest child Lambda for an introduction
;;           to the use and accuracy of these various regression techniques in
;;           a wide range of sample problem domains.
;;
;; Parms:    w:       The N by M+1 matrix representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;                    Note: Where each of the row vectors must be Number vectors.
;;           Gmax:    The maximum number of optimization trials (generations) to attempt before
;;                    returning the best set of coefficients available at that time.
;;           err:     A minimum error value which would terminate further optimization 
;;                    trials and return the best set of coefficients at that time.
;;
;; Return:   Rf:      The estimator function with coefficients Rf.C optimized.
;;                    This estimator function maps a number vector, of length M,
;;                    into a single real number, Rf: Vm --> R, whose coefficients
;;                    are optimized. The estimator function is in the form of
;;                    an Lambda, (Rf mVector) ==> aNumber, and the coefficients
;;                    to be optimized are stored as a number vector Rf.C 
;;                    in the persistant variables of the Rf Lambda.
;;                    Note: The estimator function, Rf, must be an Lambda containing
;;                          the following persistant (pvars) variables:
;;                          C             A number vector containing the coefficients to optimize
;;                          Error         The final score from the objective function 
;;                          G             The final generation count when optimization halted
;;                          Objective     The objective function which maps an N by M+1 observation 
;;                                        matrix, w, using it's parent estimator function, Rf, into 
;;                                        a single positive real error value. The objective function 
;;                                        is in the form of a child Lambda, (Rf.Objective w) ==> anErrorNumber.
;;                                        The symbolicRegress Lambda attempts to optimize the estimator
;;                                        function coefficients, Rf.C, such that the objective 
;;                                        function's resulting error value is minimized. 
;;                          P             The final population count when optimization halted 
;;                          Strategy      The optimization strategy to use:
;;                                        #void				Use genetic algorithm  
;;                                        evolve: 			Use genetic algorithm (real number genome)  
;;                                        evolveBinary: 	Use genetic algorithm (binary genome)  
;;                                        evolveBinaryMemo: Use genetic algorithm (binary genome) with memos  
;;                                        evolveInteger: 	Use genetic algorithm (integer genome)  
;;                                        linear:	    	Use multivariate linear regression  
;;                                        neural:	    	Use neural net regression  
;; 
;; Note:     See Genetic Algorithms by John Holland.
;; *******************************************************************
    pvars:(;; Public variables
           C                       ;; The best coefficient vector at the current time.
           Cn                      ;; The size of the coefficient vector. 
           Cn1                     ;; The size of the coefficient vector minus one. 
           cvSurvivors             ;; The survivor's from the current generation of coefficient vectors.
           cvError                 ;; The previous generation's best error from the each population of coefficient vectors.
           cvPopulation            ;; The current generation of coefficient vectors.
           cvStop                  ;; The stop training switch for each population of coefficient vectors.
           Error                   ;; The best error at the current time.
           forceNewPopulation      ;; The training switch to force a new population of coefficient vectors.
           generationCount         ;; Current index of optimization trial attempts.
           M                       ;; The number of input columns in the observation matrix.
           Mm1                     ;; The number of input columns in the observation matrix minus one.
           (maxPopulationAge 50)   ;; The maximum age of any distinct population for any optimization trail.
           (maxPopulations 3)      ;; The maximum number of distinct populations for any optimization trail.
           (maxSurvivors 5)        ;; The maximum survivors from a population before starting the next optimization trail.
           (minPopulation 25)      ;; The minimum population, of cvPopulation, before attempting an optimization trail.
           Mp1                     ;; The number of input and dependent columns in the observation matrix.
           myBestGuesses           ;; The vector of best regression Lambdas developed so far.
           N                       ;; The number of rows in the observation matrix.
           Net                     ;; The best neural net at the current time.
           populationAge           ;; The age of each of the current populations for this optimization trail.
           populationCount         ;; The maximum number of distinct populations for this optimization trail.
           (populationStart 0)     ;; The minimum number of distinct populations for any optimization trail.
           (randomSW true)         ;; The pseudo random setting (True = random, False = srandom).
           ;; Public child methods
           makeCoPolynomial        ;; Return a co-evolutionary polynomial estimator for use with symbolicRegress.
           setRandom               ;; Set this regression routine to use random/srandom function.
           ;; Private variables
           _random                 ;; The current random function. 
           ;; Private child methods
           coEvolution             ;; Optimize the co-evolution estimator using genetic programming.
           ;; Private maintenance child methods
           selfTest                ;; The self test method for this Lambda. 
           ) ;; end of persistent variables
    vars:(Rf i I m M n N variableCount coGmax)
    ;; *******************************************************************
    ;; Define Private Child Lambdas
    ;; *******************************************************************
    ;; *******************************************************************
    ;; Define Public Child Lambdas
    ;; *******************************************************************
    ;; Set this regression routine to use random/srandom function.
    (defun setRandom(swt ...) 
       (setq randomSW swt)
       (if (and (= swt false) (= (argCount) 2)) (setq ^srandom.seed (argFetch 1)))
       swt) ; end setRandom
    ;; *******************************************************************
    ;; Define Private Maintenance Child Lambdas
    ;; *******************************************************************
    ;; Perform a self test of this general regression Lambda.
    (defun selfTest()
       pvars:(Rf)
       vars:(w in i VN temp startTime endTime crunch y ey) 
              
       (writeln _eol "*************************math.symbolicRegress.selfTest*********************")
       (gc)(setq startTime (getTickCount 0))
          
       ;; Sample regression of a co-evolutionary polynomial model.
       ;; Model: y = -1 + (2*expt(sinh(x[0]),2.5)) + (4*expt(tan(x[1]),1.5)) 
       ;; Hint: This test case gets much more accurate,
       ;;       By simply crunching the input into the 
       ;;		 sigmoid domain and adding more data points.
       (setq VN 100)
       (setq w (new Vector: VN))
       (setRandom false 8191)
       (loop for i from 0 until VN do
          (setq w[i] (new Vector: number: 2 (srandom 1.0) (srandom 1.0)))
          (setq w[i][2] (+ -1 (* 2 (expt (sinh w[i][0]) 2.5)) (* 4 (expt (tan w[i][1]) 1.5))))
          ) ; end loop
       ;; Create a co-evolutionary exponential estimator function.
       ;; Note1: The estimator function is a standard 
       ;;        exponential regression model, which has
       ;;        been modified to prevent overflow.
       ;; Note2: A standard least squares objective
       ;;        function is used.
       (setq Rf (symbolicRegress w 100 0))
       ;; Display self test results for this test run.
       (writeln _eol "CoEvo Polynomial Regression Test Results: (seed=" srandom.seed ")")
       (writeln "Actual: y = -1 + (2*expt(sinh(x[0]),2.5) + (4*expt(tan(x[1]),1.5))")
       (writeln (Rf.show))

       ;; End self test and display time required.
       SelfTestLast::
       (setq endTime (getTickCount startTime))
       (writeln "Timing = " endTime " seconds.")
       true) ; end selfTest
    ;; *******************************************************************
    ;; Main Logic Section
    ;; *******************************************************************
    ;; Check the number of independent variables.
    (setq variableCount (sub1 (length w[0])))
    (if (<= variableCount 1) (error "symbolicRegress: expecting at least one independent variable"))
    (setq myBestGuesses (new Vector: object: 0))
    ;; Try a standard multivariable linear regression model.
    (setq Rf (math.numericRegress.makeLinear variableCount))
    (setq myBestGuesses[(length myBestGuesses)] (setq Rf (math.numericRegress w Rf Gmax err)))
    (setq Rf.G 1)
    (setq Rf.P 1)
    (if (<= Gmax 1) (goto Last:))
    ;; Try a standard exponential polynomial regression model.
    (setq Rf (math.numericRegress.makeExponential variableCount))
    (setq myBestGuesses[(length myBestGuesses)] (setq Rf (math.numericRegress w Rf Gmax err)))
    (setq Rf.G 1)
    (setq Rf.P 1)
    (if (<= Gmax 50) (goto Last:))
    ;; Try co-evolution of a standard polynomial.
    (setq coGmax (+ (max 1 (divi Gmax 100)) (/ Gmax 10000))) 
    (setq Rf (makeCoPolynomial variableCount))
    (setq myBestGuesses[(length myBestGuesses)] (setq Rf (coEvolution w Rf coGmax err)))
    ;; Return our best guess as the answer.
    Last::
    (sort myBestGuesses (lambda(x y) (< x.Error y.Error)))
    (setq Rf myBestGuesses[0])
    Rf) ; end symbolicRegress




























;;**EXPORTKEY**:math:symbolicRegress:coEvolution
(defriend math.symbolicRegress coEvolution(w Rf Gmax err)
;; ******************************************************************************************
;; summary:  This Lambda optimizes an estimator function with both an integer
;;           genome vector and a number coefficient vector. Although the
;;           estimator function is a black-box, it is assumed that the genome
;;           vector alters the grammatical form of the estimator function in
;;           some way; while, the coefficient vector controls the numeric
;;           grammar terminals in the usual manner.
;;
;;           This Lambda returns the input estimator function with the genome and all 
;;           coefficients optimized against the specified objective function.
;;           The regression is nonlinear and will minimize any objective function.
;;           Neither the estimator function nor the objective function 
;;           are required to be continuous.
;;
;; 			 This Lambda assumes that the coefficient vector, C, is an number vector,
;;           that the genome vector, Ge, is an integer vector, and that the
;;           genome count vector, Gc, is an integer vector. 
;;
;; Parms:    w:       The N by M+1 matrix representing the original observations
;;                    in the form of:    x x ... x y
;;                                       x x ... x y
;;                                           ... 
;;                                       x x ... x y
;;                    Note: Where each of the row vectors must be Number vectors.
;;           Rf:      The estimator function which maps a number vector, of length M,
;;                    into a single real number, Rf: Vm --> R, whose coefficients
;;                    are to be optimized. The estimator function is in the form of
;;                    an Lambda, (Rf mVector) ==> aNumber, and the coefficients
;;                    to be optimized are stored as a number vector Rf.C 
;;                    in the persistant variables of the Rf Lambda.
;;                    Note: The estimator function, Rf, must be an Lambda containing
;;                          the following persistant (pvars) variables:
;;                          C             An number vector containing the number coefficients to optimize
;;                          Error         The final score from the objective function 
;;                          Fe            Total function executions after training
;;                          G             The final generation count when optimization halted
;;                          Gc            CoEvolution genome item count vector
;;                          Ge            CoEvolution genome vector
;;                          Objective     The objective function which maps an N by M+1 observation 
;;                                        matrix, w, using it's parent estimator function, Rf, into 
;;                                        a single positive real error value. The objective function 
;;                                        is in the form of a child Lambda, (Rf.Objective w) ==> anErrorNumber.
;;                                        The symbolicRegress Lambda attempts to optimize the estimator
;;                                        function coefficients, Rf.C, such that the objective 
;;                                        function's resulting error value is minimized. 
;;                          P             The final population count when optimization halted 
;;                          Strategy      The optimization strategy to use:
;;                                        coEvolve: 		Use genetic programming co-evolution (real number genome)  
;;           Gmax:    The maximum number of optimization trials (generations) to attempt before
;;                    returning the best set of coefficients available at that time.
;;                    Note: Gmax is a fraction such as 10.1234, with the integer part indicating
;;                          the number of genome co-evolution generations, and the fractional part
;;                          indicating the number of numeric regression generations (times 10000) to use
;;                          during numeric regression training of each phenotype. For example, 21.4321 is
;;                          21 co-evolution generations and 4321 generations for each numeric regression.
;;           err:     A minimum error value which would terminate further optimization 
;;                    trials and return the best set of coefficients at that time.
;;
;; Return:   Rf:      The estimator function with genome Rf.Ge and coefficients Rf.C optimized.
;; 
;; Note:     See Genetic Programming by John Koza.
;; ******************************************************************************************
    pvars:(;; Private child methods
           coEvolveGenome          ;; Increment the co-evolution genome to the next logical genome.
           initCoefficients        ;; Initialize coPopulation with a random coefficient vector population.
           migrateCoefficients     ;; Migrate candidates between coPopulations.
           optimizeCandidate       ;; Optimize the current phenotype candidate using numeric regression.
           optimizeCoefficients    ;; Optimize the current phenotype coefficients using numeric regression.
           ;; Public Variables
           coBestGe                ;; The current best-of-breed co-evolution genome.
           coBestC                 ;; The current best-of-breed optimized coefficient vector.
           coBestError             ;; The current best-of-breed co-evolution genome & optimized coefficient vectors error.
           coGenerationCount       ;; Current count of co-evolution generations.
           coEvolutionIndices      ;; The current generation of co-evolution genome indicies vectors.
           coPopulation            ;; The current generation of co-evolution genome & optimized coefficient vectors.
           coStop                  ;; The current generation of co-evolution genome stop switches.
           ;; Private Variables
           (coEvolveGenerations 10);; The count of co-evolution generations to use during training.
           coGenomeCount           ;; The count of co-evolution genomes.
           currentPop              ;; The index of the current co-evolution population.
           forceNewPopulation      ;; The training switch to force a new population of coefficient vectors.
           maxCoGenomes            ;; The maximum count of co-evolution genomes.
           (numericGenerations 100);; The count of numeric regression generations to use for the final optimization.
           (testSW true )          ;; The test switch (true == display intermiediate results).
           (trainNumGenerations 20);; The count of numeric regression generations to use during training.
           ) ;; end of persistent variables
    vars:(i naCount)
    ;; *******************************************************************
    ;; Define Private Child Lambdas
    ;; *******************************************************************
    ;; Increment the co-evolution genome to the next logical genome.
    (defun coEvolveGenome(Rf w err)
       vars:(m M n N popIdx
             candidateGenome
             majorIndex minorIndex            
             ) ; end temporary variables 
       ;; Increment the genome in each of the co populations.
       (setq forceNewPopulation true)
       (setq N currentPop)
       (loop for popIdx from 0 to N do
	       ;; Check the population stop switch.
           (if (= coStop[popIdx] true) (goto Last:))
	       ;; Increment the major and minor co-evolution genome indices.
	       ;; Note: We stop incrementing, and request a new population,
	       ;;       only when we have exhausted all possibilities in
	       ;;       the local population search area.
	       IncrementIndices::
	       (setq candidateGenome (copy coPopulation[popIdx][0 0]))
	       (setq majorIndex coEvolutionIndices[popIdx][0])
	       (setq minorIndex coEvolutionIndices[popIdx][1])
	       (if (>= coGenomeCount maxCoGenomes)
	           (begin
	              (setq coStop[popIdx] true)
	              (goto Last:)
	           )) ; end if
	       (++ minorIndex)
	       (if (>= minorIndex Rf.Gc[majorIndex])
	           (begin
	              (setq minorIndex 0)
	              (++ majorIndex)
			      (if (>= majorIndex (length Rf.Gc))
			          (begin
			             (setq majorIndex 0)
			             (++ majorIndex)			             
				         (setq coEvolutionIndices[popIdx][0] majorIndex)
				         (setq coEvolutionIndices[popIdx][1] minorIndex)
			             (setq coStop[popIdx] true)
			             (goto Last:)
			          )) ; end major index overflow
#if 1
			      ;; Start a new population with the second best-of-breed candidate.
			      ;; Note: This inserts modified backtracking into the search strategy.
			      ;(setq coPopulation[popIdx][0].Error "NA")
			      ;(setq coPopulation[popIdx][1].Error "NA")
			      ;(optimizeCandidate popIdx (* trainNumGenerations 5) Rf w err)
			      (setq coEvolutionIndices[popIdx][0] majorIndex)
			      (setq coEvolutionIndices[popIdx][1] minorIndex)
			      (migrateCoefficients Rf (copy coPopulation[popIdx][0 0]) majorIndex -1) 
			      (migrateCoefficients Rf (copy coPopulation[popIdx][1 0]) majorIndex -1) 
	              (setq coStop[popIdx] true)
			      (goto Last:)
#endif
	           )) ; end increment indices
	       (setq coEvolutionIndices[popIdx][0] majorIndex)
	       (setq coEvolutionIndices[popIdx][1] minorIndex)
	       ;; Construct the next available candidate genome.
	       ;; Note: We check each population to make sure that this is not
	       ;;       a duplicate genome which we've already checked.
	       (setq candidateGenome[majorIndex] minorIndex)
	       (loop for n from 0 to currentPop do
	          (if (isMember candidateGenome coPopulation[n]) (goto IncrementIndices:))
	          ) ; end currentPop loop
	       (insert coPopulation[popIdx] 0 candidateGenome (new Structure: C: #void Error: "NA"))
	       (setq forceNewPopulation false)
	       (++ coGenomeCount)
           Last::
           ) ; end coPopulation loop
       true) ; end coEvolveGenome
    ;; Initialize coPopulation with a new random co-evolution genome vector.
    ;; Note: Optional arguments let us start a new coPopulation from a
    ;;       specific position in the search process which supports the
    ;;       equivalent of partial backtracking searches.
    (defun initCoefficients(Rf ...)
       vars:(nGe NGe n N
             candidateGenome 
             (candidateMajorIndex 0) 
             (candidateMinorIndex -1)
             ) ; end temporary variables
       ;; Initialize all system parameters.
       (if (= randomSW true) (setq _random ^random) (setq _random ^srandom))
       (if (>= coGenomeCount maxCoGenomes)
           (begin
              (setq coStop[currentPop] true)
              (return true)
           )) ; end if 
       (if (>= (argCount) 2) (setq candidateGenome (copy (argFetch 1))))
       (if (>= (argCount) 3) (setq candidateMajorIndex (argFetch 2)))
       (if (>= (argCount) 4) (setq candidateMinorIndex (argFetch 3)))
       ;; The population count increases as the generation count increases.
       (setq populationCount (setq currentPop (length coPopulation)))
       ;; Initialize a co-evolution genome for the next distinct population.
       (setq NGe (length Rf.Ge))
       (if (= coPopulation #void) (setq coPopulation (new Vector:)))
       (if (= coEvolutionIndices #void) (setq coEvolutionIndices (new Vector:)))
       (if (= coStop #void) (setq coStop (new Vector:)))
       (setq coPopulation[currentPop] (new Structure:))
       (setq coEvolutionIndices[currentPop] (new Vector: integer: 2 candidateMajorIndex candidateMinorIndex))
       (setq coStop[currentPop] false)
       (setq forceNewPopulation false)
       ;; Create an initial random co-evolution genome.
       TryANewSeedGenome::
       (if (= candidateGenome #void) (setq candidateGenome (new Vector: integer: NGe)))
       (loop for nGe from candidateMajorIndex until NGe do
          (setq candidateGenome[nGe] (_random Rf.Gc[nGe]))
          ) ; end loop
       ;; We check each population to make sure that this is not
       ;; a duplicate genome which we've already checked.
       (loop for n from 0 to currentPop do
          (if (isMember candidateGenome coPopulation[n]) (goto TryANewSeedGenome:))
          ) ; end currentPop loop
       (insert coPopulation[currentPop] 0 candidateGenome (new Structure: C: #void Error: "NA"))
       (setq populationCount (length coPopulation))
       (++ coGenomeCount)
       true) ; end initCoefficients
    ;; Migrate candidates between coPopulations.
    ;; Note: Optional arguments let us start a new coPopulation from a
    ;;       specific position in the search process which supports the
    ;;       equivalent of partial backtracking searches.
    (defun migrateCoefficients(Rf candidateGenome candidateMajorIndex candidateMinorIndex)
       vars:(nGe NGe n N
             ) ; end temporary variables
       ;; Initialize all system parameters.
       (if (>= coGenomeCount maxCoGenomes)
           (begin
              (setq coStop[currentPop] true)
              (return true)
           )) ; end if 
       ;; The population count increases as the generation count increases.
       (setq populationCount (setq currentPop (length coPopulation)))
       ;; Migrate a co-evolution genome into the next distinct population.
       (setq NGe (length Rf.Ge))
       (if (= coPopulation #void) (setq coPopulation (new Vector:)))
       (if (= coEvolutionIndices #void) (setq coEvolutionIndices (new Vector:)))
       (if (= coStop #void) (setq coStop (new Vector:)))
       (setq coPopulation[currentPop] (new Structure:))
       (setq coEvolutionIndices[currentPop] (new Vector: integer: 2 candidateMajorIndex candidateMinorIndex))
       (setq coStop[currentPop] false)
       (setq forceNewPopulation false)
       ;; Create an initial random co-evolution genome.
       (insert coPopulation[currentPop] 0 candidateGenome (new Structure: C: #void Error: "NA"))
       (setq populationCount (length coPopulation))
       true) ; end migrateCoefficients
    ;; Optimize the current phenotype candidate using numeric regression.
    (defun optimizeCandidate(popIdx gCount Rf w err)
       vars:(n N)
       vars:(popIdx n N)
       ;; Optimize the coefficients for the next population candidate.
       ;; Note: We scann through all populations looking for candidates.
	   (if (>= coGenomeCount maxCoGenomes) (goto Last:))
       (if (= coStop[popIdx] true) (goto Last:))
       (setq N (length coPopulation[popIdx]))
       (loop for n from 0 until N do
	       (if (<> coPopulation[popIdx][n].Error "NA") (goto Skip:))
	       (setq Rf.Ge (copy coPopulation[popIdx][n 0]))
	       (if testSW (display "[" popIdx "," coGenerationCount "] Ge= " Rf.Ge)) 
	       (setq Rf (math.numericRegress w Rf gCount err))
	       (if testSW (writeln " G=" Rf.G " Err=" Rf.Error " {" coBestGe "," coBestError "}")) 
	       (setq coPopulation[popIdx][n].Error Rf.Error)
	       (setq coPopulation[popIdx][n].C Rf.C)
	       Skip::
           ) ; end optimize loop
       ;; Sort the population candiates ascending by error.
       (sort coPopulation[popIdx] (lambda(x y) (< x.Error y.Error)) byValue:)
       ;; Replace the best-of-breed if appropriate.
       (if (< coPopulation[popIdx][0].Error coBestError)
           (begin
              (setq coBestError coPopulation[popIdx][0].Error)
              (setq coBestC (copy coPopulation[popIdx][0].C))
              (setq coBestGe (copy coPopulation[popIdx][0 0]))
           )) ; end if
       Last::
       true) ; end optimizeCandidate
    ;; Optimize the current phenotype coefficients using numeric regression.
    (defun optimizeCoefficients(Rf w err)
       vars:(popIdx n N)
       ;; Optimize the coefficients for the next population candidate.
       ;; Note: We scann through all populations looking for candidates.
	   (if (>= coGenomeCount maxCoGenomes) (goto Last:))
       (loop for popIdx from 0 to currentPop do
           (optimizeCandidate popIdx trainNumGenerations Rf w err)
           ) ; end cpPopulation loop
       Last::
       true) ; end optimizeCoefficients
    ;; *******************************************************************
    ;; Begin Genetic Algorithm Logic Section
    ;; *******************************************************************
    (setq Rf (copy Rf))
    (setq coBestGe #void)
    (setq coBestC #void)
    (setq coBestError "NA")
    (setq coPopulation #void)
    (setq coEvolutionIndices #void)
    (setq coStop #void)
    (setq coGenerationCount 0)
    (setq populationCount populationStart)
    (setq numericGenerations (integer (* (fraction Gmax) 10000)))
    (setq coEvolveGenerations (integer Gmax))
    (setq coGenomeCount 0)
    (setq maxCoGenomes 1)
    (loop for n from 0 until (length Rf.Gc) do (setq maxCoGenomes (* maxCoGenomes Rf.Gc[n])))
    NewPopulation::
    (if (>= coGenerationCount coEvolveGenerations) (goto StopTraining:))
    (setq coGenerationCount (addi coGenerationCount 1))
    (initCoefficients Rf)
    NewGeneration::
    (if (>= coGenomeCount maxCoGenomes) (goto StopTraining:))
    (optimizeCoefficients Rf w err)
    (coEvolveGenome Rf w err)
    (if (<= coBestError err) (goto StopTraining:))
    (if (= forceNewPopulation true) (goto NewPopulation:))
    (goto NewGeneration:)
    ;; Return the optimized estimator function Lambda.
    StopTraining::
    (setq Rf.C coBestC)
    (setq Rf.Ge coBestGe)
    (setq Rf.Error coBestError)
    (if testSW (display "[F] Ge= " Rf.Ge)) 
    (setq Rf (math.numericRegress w Rf numericGenerations err))
    (if testSW (writeln " G=" Rf.G " Err=" Rf.Error)) 
    (setq Rf.G coGenerationCount)
    (setq Rf.P populationCount)
    Rf) ; end coEvolution


































;;**EXPORTKEY**:math:symbolicRegress:makeCoPolynomial
(defchild math.symbolicRegress makeCoPolynomial(variableCount)
;; *******************************************************************
;; summary:  Return a co-evolutionary polynomial estimator for use 
;;           with symbolicRegress.
;;
;;           The grammar is a simple polynomial grammar for N variables as follows:
;;           POLY: CON + TERM(1) ... + TERM(N)
;;           TERM: (CON*VAR)
;;           TERM: (CON*UFN(VAR))
;;           TERM: (CON*BFN(VAR,CON))
;;           TERM: (CON*BFN(UFN(VAR),CON))
;;           UFN:  abs ; ceiling ; floor ; fact ; round ; acos ; cos ; cosh ; asin
;;           UFN:  sin ; sinh ; sign ; atan ; tan ; tanh ; deg ; rad ; exp ; log 
;;           UFN:  log2 ; log10 ; integer ; fraction ; sqrt
;;           BFN:  + ; - ; * ; / ; mod ; expt ; gcd ; lcm ; max ; min ; logbase 
;;
;;           The grammar above generates a flat polynomial genome for 2 variables as follows:
;;           [0]   C[0] + (C[1]*BFN(UFN(x[0]),C[2])) + (C[3]*BFN(UFN(x[1]),C[4]))
;;
;; Parms:    variableCount:  The number of independent variables in the regression.
;;
;; Return:   Estimator       Always returns an co-evolved estimator function.
;; *******************************************************************
   pvars:(;; Public variables
          BFN             ;; Vector of valid binary functions
          BFNs            ;; Vector of valid binary function names
          UFN             ;; Vector of valid unary functions
          UFNs            ;; Vector of valid unary function names
          ;; Public methods
          bnoop           ;; Binary no-operation function
          noop            ;; Unary no-operation function
          ) ; end persistant variables
   vars:(w Rf in 
         i I n N
         ) ; end temporary variables
   ;; ****************************************************************
   ;; Define child Lambdas
   ;; **************************************************************** 
   ;; Unary no-operation function
   (defun noop(v) v)
   ;; Binary no-operation function
   (defun bnoop(v c) v)
   ;; ****************************************************************
   ;; Define persistant variables
   ;; ****************************************************************
   (setq BFN (new Vector: 8  mod expt gcd lcm max min logbase bnoop))
   (setq BFNs (new Vector: 8  "mod" "expt" "gcd" "lcm" "max" "min" "logbase" "bnoop"))
   (setq UFN (new Vector: 25  abs ceiling floor fact round acos cos cosh asin sin sinh sign atan tan tanh deg rad exp log log2 log10 integer fraction sqrt noop))
   (setq UFNs (new Vector: 25  "abs" "ceiling" "floor" "fact" "round" "acos" "cos" "cosh" "asin" "sin" "sinh" "sign" "atan" "tan" "tanh" "deg" "rad" "exp" "log" "log2" "log10" "integer" "fraction" "sqrt" "noop"))
   ;; ****************************************************************
   ;; Define the flat co-phenotype of (variableCount) variables
   ;; ****************************************************************
   ;; Create an general polynomial estimator function.
   ;; Note1: The estimator function is a general 
   ;;        polynomial regression model, which has
   ;;        been modified to prevent overflow.
   ;; Note2: A standard least squares objective
   ;;        function is used.
   (setq Rf (eval {
     (lambda(v)
        pvars:(C                    ; Coefficient vector
               Error           		; Final objective error after training
               Fe                   ; Final function executions after training
               Fn                   ; CoEvolution genome function vector of vectors
               Fs                   ; CoEvolution genome function names vector of vectors
               G               		; Final generation count after training
               Ge                   ; CoEvolution genome vector
               Gc                   ; CoEvolution genome item count vector
               Objective       		; Objective function to use in training
               P               		; Final population count after training
               Rf              		; My estimator function
               (Strategy evolve)	; My optimization strategy
               ) ; end of persistant variables
        vars:(n N ca cb fa fb result ey y)
        ;; Trap any errors and return 0 as the guestimate.
        (onError (lambda(err) BIGPOSNUM))
        ;; Define a standard polynomial display function
        (defun show()
           vars:(result ca cb fa fb n N)
           (setq result (append "model: y = " C[0]))
           (setq N (divi (length C) 2))
	       (setq ca 1)
	       (setq cb 2)
	       (setq fa 0)
	       (setq fb 1)
           (loop for n from 0 until N do
              (cond
                 ;; + (C1*x[n])
                 ((and (= Fs[fa][Ge[fa]] "bnoop") (= Fs[fb][Ge[fb]] "noop"))
                  (if (isNegative C[ca])
                      (setq result (append result " - (" (abs C[ca]) "*" "x[" n "])"))
                      (setq result (append result " + (" C[ca] "*" "x[" n "])"))
                      ) ; end sign if
                  ) ; end case 
                 ;; + (C1*UFN(x[n]))
                 ((and (= Fs[fa][Ge[fa]] "bnoop") (<> Fs[fb][Ge[fb]] "noop"))
                  (if (isNegative C[ca])
                      (setq result (append result " - (" (abs C[ca]) "*" Fs[fb][Ge[fb]] "(x[" n "]))"))
                      (setq result (append result " + (" C[ca] "*" Fs[fb][Ge[fb]] "(x[" n "]))"))
                      ) ; end sign if
                  ) ; end case 
                 ;; + (C1*BFN(x[n],C2))
                 ((and (<> Fs[fa][Ge[fa]] "bnoop") (= Fs[fb][Ge[fb]] "noop"))
                  (if (isNegative C[ca])
                      (setq result (append result " - (" (abs C[ca]) "*" Fs[fa][Ge[fa]] "(x[" n "]," C[cb] "))"))
                      (setq result (append result " + (" C[ca] "*" Fs[fa][Ge[fa]] "(x[" n "]," C[cb] "))"))
                      ) ; end sign if
                  ) ; end case 
                 ;; + (C1*BFN(UFN(x[n]),C2))
                 (else
                  (if (isNegative C[ca])
                      (setq result (append result " - (" (abs C[ca]) "*" Fs[fa][Ge[fa]] "(" Fs[fb][Ge[fb]] "(x[" n "])," C[cb] "))"))
                      (setq result (append result " + (" C[ca] "*" Fs[fa][Ge[fa]] "(" Fs[fb][Ge[fb]] "(x[" n "])," C[cb] "))"))
                      ) ; end sign if
                  ) ; end case               
                 ) ; end cond 
              (setq ca (addi ca 2))
              (setq ce (addi cb 2))
              (setq fa (addi fa 2))
              (setq fb (addi fb 2))
              ) ; end loop
           ;; Append the training statistics to the model display
           (setq result (append result _eol "G=" Rf.G ",P=" Rf.P ",Fe=" Rf.Fe ",Error=" Rf.Error))
           result) ; end define a standard polynomial display function
        ;; Define a standard least square objective function
        (defun Objective(w)
           vars:(i result ey y M N)
           (setq result 0)
           (setq M (- (length w[0]) 1))
           (setq N (length w))
           (++ Fe)
           (loop for n from 0 until N do (setq result (+ result (* (setq ey (- (Rf w[n]) w[n][M])) ey))))
           (/ result N)) ; end define objective function
        ;; Begin estimator function main logic
        (setq N (divi (length C) 2))
        (setq ca 1)
        (setq cb 2)
        (setq fa 0)
        (setq fb 1)
        (setq result C[0])
        (loop for n from 0 until N do
           (setq result (+ result (* C[ca] (Fn[fa][Ge[fa]] (Fn[fb][Ge[fb]] v[n]) C[cb]))))
           (setq ca (addi ca 2))
           (setq ce (addi cb 2))
           (setq fa (addi fa 2))
           (setq fb (addi fb 2))
           ) ; end loop
        ;; Forceably prevent overflow
        (setq result (numCheck result))
        result) }
        )) ; end define estimator function
   ;; Set the coefficient vector to the number of variables times 2 plus the constant coefficient.
   (setq Rf.C (new Vector: number: (integer (+ (* variableCount 2) 1)) 0))
   (setq Rf.Ge (new Vector: integer: (* variableCount 2) 0))
   (setq Rf.Fn (new Vector: object: (* variableCount 2) BFN UFN))
   (setq Rf.Fs (new Vector: object: (* variableCount 2) BFNs UFNs))
   (setq Rf.Gc (new Vector: integer: (* variableCount 2) (length BFN) (length UFN)))
   (setq Rf.Rf Rf)
   Rf) ; end makeCoPolynomial







































;;**EXPORTKEY**:math:timeRegress
(defriend math:timeRegress(w)
;; *******************************************************************
;; name:     timeRegress
;; 
;; summary:  Returns a vector containing the coefficients resulting
;;           from a linear time regression on the specified dependent
;;           variables. If y is the vector
dependent variabe to regress,
;;           then we create x which is a vector of increasing integers,
;;           then (regression y) is: #(a  b  error).
;;           where a + bx = y represents the least squares best fit.
;;           The term, error, is the least squares error = sqr(y - (a + bx)).
;; Parms:    w:       The N vector representing the dependent observations.
;; Return:   v:       The coefficient vector #(a b error)
;; *******************************************************************
	vars:(m n N M v vt xmean ymean numerator denominator x y)
	(setq v (new Vector: 3))
	(setq N (length w))
	(setq x (new Vector: number: N))
	(setq y (new Vector: number: N))
    (loop for n from 0 until N do
       (setq x[n] (+ n 1))
       (setq y[n] w[n])    
       ) ; end loop
   	(setq xmean (avg x))
   	(setq ymean (avg y))
   	(setq numerator (vectorDotProduct (setq vt (vectorSub x xmean)) y)) 
   	(setq denominator (vectorDotProduct vt vt))
   	(if (= denominator 0)
       	(setq v[1] 0)
       	(setq v[1] (/ numerator denominator)))
   	(setq v[0] (- ymean (* v[1] xmean)))
   	(setq vt (vectorSub (vectorAdd (vectorProduct x v[1]) v[0]) y)) 
   	(setq v[2] (vectorDotProduct vt vt))
	v) ; timeRegress












;;**EXPORTKEY**:math:vectorAdd
(defriend math:vectorAdd(x y)
;; *******************************************************************
;; summary:  Returns the sum of two vectors or of a vector and a constant. 
;;           If x and y are vectors of equal length,
;;           then x+y is: #(x[0]+y[0]  x[1]+y[1] .... x[N]+y[N]).
;;           If x is a vector and y is a constant,
;;           then x+y is: #(x[0]+y  x[1]+y .... x[N]+y).
;; Parms:    x:       A vector of length N.
;;           y:       A vector of length N or a constant.
;; Return:   v:       The vector sum of x+y
;; *******************************************************************
    vars:(i m n v)
    (cond
       ;; x is a singleton, y is a singleton
       ((and (isNumber x) (isNumber y))
        (setq v (+ x y))
        )
       ;; x is a vector, y is a singleton
       ((isNumber y)
        (begin
           (setq n (length x))
           (setq v (new Vector: Number: n))
           (loop for i from 0 until n do (setq v[i] (+ x[i] y)))
        ))
       ;; x is a singleton, y is a vector
       ((isNumber x)
        (begin
           (setq n (length y))
           (setq v (new Vector: Number: n))
           (loop for i from 0 until n do (setq v[i] (+ x y[i])))
        ))
       ;; x is a vector, y is a vector
       (else
        (begin
           (setq n (length x))
           (setq v (new Vector: Number: n))
           (setq m (length y))
           (if (<> n m)
               (error 
                  (append "vectorAdd: vectors not the same length"
                          " (length x)=" n
                          " (length y)=" m
                          " x=" x
                          " y=" y)))
           (loop for i from 0 until n do (setq v[i] (+ x[i] y[i])))
        ))
        ) ; end cond
    v) ; end vectorAdd












































































;;**EXPORTKEY**:math:vectorDeltaPercents
(defriend math:vectorDeltaPercents(x)
;; *******************************************************************
;; summary:  Convert an N vector into an N-1 vector of deltas 
;;            (expressed as percent differences). 
;;           If x is: x[0] x[1] .... x[N], 
;;           then result is: (x[1]/x[0])-1 (x[2]/x[1])-1 .... (x[N]/x[N-1])-1.
;; Note:     Where ever x[n] == 0, then x[n+1]/x[n] == x[n+1].
;; Parms:    x:       A vector of length N.
;; Return:   v:       n N-1 vector of deltas (expressed as percent differences).
;; *******************************************************************
    vars:(n N m M v)
    (setq N (sub1 (length x)))
    (if (<= N 0) (return #void))
    (setq v (new Vector: N))
    (loop for n from 0 until N do
       (if (= x[n] 0)
           (setq v[n] x[(add1 n)])
           (setq v[n] (sub1 (/ x[(add1 n)] x[n])))
           ) ; end if
       ) ; end loop
    v) ; end vectorDeltaPercents












































































;;**EXPORTKEY**:math:vectorDeltas
(defriend math:vectorDeltas(x)
;; *******************************************************************
;; summary:  Convert an N vector into an N-1 vector of deltas. 
;;           If x is: x[0] x[1] .... x[N], 
;;           then result is: (x[1]-x[0]) (x[2]-x[1]) .... (x[N]-x[N-1]).
;; Parms:    x:       A vector of length N.
;; Return:   v:       n N-1 vector of deltas.
;; *******************************************************************
    vars:(n N m M v)
    (setq N (sub1 (length x)))
    (if (<= N 0) (return #void))
    (setq v (new Vector: N))
    (loop for n from 0 until N do
       (setq v[n] (- x[(add1 n)] x[n]))
       ) ; end loop
    v) ; end vectorDeltas












































































;;**EXPORTKEY**:math:vectorDivide
(defriend math:vectorDivide(x y)
;; *******************************************************************
;; summary:  Returns the quotient of two vectors or of a vector and a constant. 
;;           If x and y are vectors of equal length,
;;           then x??y is: #(x[0]/y[0]  x[1]/y[1] .... x[N]/y[N]).
;;           If x is a vector and y is a constant,
;;           then x??y is: #(x[0]/y  x[1]/y .... x[N]/y).
;; Parms:    x:       A vector of length N.
;;           y:       A vector of length N or a constant.
;; Return:   v:       The vector product of x??y
;; *******************************************************************
    vars:(i m n v)
    (cond
       ;; x is a singleton, y is a singleton
       ((and (isNumber x) (isNumber y))
        (setq v (/ x y))
        )
       ;; x is a vector, y is a singleton
       ((isNumber y)
        (begin
           (setq n (length x))
           (setq v (new Vector: Number: n))
           (loop for i from 0 until n do (setq v[i] (/ x[i] y)))
        ))
       ;; x is a singleton, y is a vector
       ((isNumber x)
        (begin
           (setq n (length y))
           (setq v (new Vector: Number: n))
           (loop for i from 0 until n do (setq v[i] (/ x y[i])))
        ))
       ;; x is a vector, y is a vector
       (else
        (begin
           (setq n (length x))
           (setq v (new Vector: Number: n))
           (setq m (length y))
           (if (<> n m)
               (error 
                  (append "vectorDivide: vectors not the same length"
                          " (length x)=" n
                          " (length y)=" m
                          " x=" x
                          " y=" y)))
           (loop for i from 0 until n do (setq v[i] (/ x[i] y[i])))
        ))
        ) ; end cond
    v) ; end vectorDivide





































;;**EXPORTKEY**:math:vectorDotProduct
(defriend math:vectorDotProduct(x y)
;; *******************************************************************
;; summary:  Returns the dot product of two vectors. If x and y are 
;;           vectors of equal length, 
;;           then xùy is: x[0]y[0] + x[1]y[1] + .... x[N]y[N].
;; Parms:    x:       A vector of length N.
;;           y:       A vector of length N.
;; Return:   v:       The product of xùy
;; *******************************************************************
    vars:(i m n sum)
    (setq n (length x))
    (setq m (length y))
    (if (<> n m)
        (error 
            (append "vectorDotProduct: vectors not the same length"
                    " (length x)=" n
                    " (length y)=" m
                    " x=" x
                    " y=" y)))
    (loop for i from 0 until n do
        (setq sum (+ sum (* x[i] y[i]))))
    sum) ; end vectorDotProduct












































































;;**EXPORTKEY**:math:vectorProduct
(defriend math:vectorProduct(x y)
;; *******************************************************************
;; summary:  Returns the product of two vectors or of a vector and a constant. 
;;           If x and y are vectors of equal length,
;;           then x??y is: #(x[0]y[0]  x[1]y[1] .... x[N]y[N]).
;;           If x is a vector and y is a constant,
;;           then x??y is: #(x[0]y  x[1]y .... x[N]y).
;; Parms:    x:       A vector of length N.
;;           y:       A vector of length N or a constant.
;; Return:   v:       The vector product of x??y
;; *******************************************************************
    vars:(i m n v)
    (cond
       ;; x is a singleton, y is a singleton
       ((and (isNumber x) (isNumber y))
        (setq v (* x y))
        )
       ;; x is a vector, y is a singleton
       ((isNumber y)
        (begin
           (setq n (length x))
           (setq v (new Vector: Number: n))
           (loop for i from 0 until n do (setq v[i] (* x[i] y)))
        ))
       ;; x is a singleton, y is a vector
       ((isNumber x)
        (begin
           (setq n (length y))
           (setq v (new Vector: Number: n))
           (loop for i from 0 until n do (setq v[i] (* x y[i])))
        ))
       ;; x is a vector, y is a vector
       (else
        (begin
           (setq n (length x))
           (setq v (new Vector: Number: n))
           (setq m (length y))
           (if (<> n m)
               (error 
                  (append "vectorProduct: vectors not the same length"
                          " (length x)=" n
                          " (length y)=" m
                          " x=" x
                          " y=" y)))
           (loop for i from 0 until n do (setq v[i] (* x[i] y[i])))
        ))
        ) ; end cond
    v) ; end vectorProduct





































;;**EXPORTKEY**:math:vectorSub
(defriend math:vectorSub(x y)
;; *******************************************************************
;; summary:  Returns the difference of two vectors or of a vector and
;;           a constant. 
;;           If x and y are vectors of equal length,
;;           then x-y is: #(x[0]-y[0]  x[1]-y[1] .... x[N]-y[N]).
;;           If x is a vector and y is a constant,
;;           then x+y is: #(x[0]-y  x[1]-y .... x[N]-y).
;; Parms:    x:       A vector of length N.
;;           y:       A vector of length N or a constant.
;; Return:   v:       The vector difference of x+y
;; *******************************************************************
    vars:(i m n v)
    (cond
       ;; x is a singleton, y is a singleton
       ((and (isNumber x) (isNumber y))
        (setq v (- x y))
        )
       ;; x is a vector, y is a singleton
       ((isNumber y)
        (begin
           (setq n (length x))
           (setq v (new Vector: Number: n))
           (loop for i from 0 until n do (setq v[i] (- x[i] y)))
        ))
       ;; x is a singleton, y is a vector
       ((isNumber x)
        (begin
           (setq n (length y))
           (setq v (new Vector: Number: n))
           (loop for i from 0 until n do (setq v[i] (- x y[i])))
        ))
       ;; x is a vector, y is a vector
       (else
        (begin
           (setq n (length x))
           (setq v (new Vector: Number: n))
           (setq m (length y))
           (if (<> n m)
               (error 
                  (append "vectorSub: vectors not the same length"
                          " (length x)=" n
                          " (length y)=" m
                          " x=" x
                          " y=" y)))
           (loop for i from 0 until n do (setq v[i] (- x[i] y[i])))
        ))
        ) ; end cond
    v) ; end vectorSub










