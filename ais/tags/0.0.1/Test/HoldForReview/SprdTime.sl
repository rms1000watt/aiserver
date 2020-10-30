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
;;  
;;  Title:    Spreadsheet Timing study
;;
;;  Author:   Michael F. Korns
;;
;;  Project:  Spreadsheet timinging study
;;            The speed and efficiency of Spreadsheet cell access
;;            and math, is acceptance tested in this test script.
;;
;;  Notes:    Must have loaded %StripCascade before testing. 
;;            Must have loaded %optimizedCascade before testing. 
;;            Must have loaded %cascade before testing. 
;;            No other dependencies.  
;;

;;  The VmScript diagnostic report function for this test script
(defun diagnostic(msg) (error msg))
(writeln "Spreadsheet Timing Test started")

;;  Load the required spreadsheet objects.
(define TotalCascade ":Spreadsheets:TotalCascade")
(define Cascade ":Spreadsheets:Cascade")
(define StripCascade ":Spreadsheets:StripCascade")
(define optimizedCascade ":Spreadsheets:optimizedCascade")
(if (= CascadeSS #void)
    (begin
       (open-window TotalCascade 0 1)
       (open-window Cascade 0 1)
       (open-window StripCascade 0 1)
       (open-window optimizedCascade 0 1)))
(define TotalCascadeSS @|:Spreadsheets:TotalCascade|)
(define CascadeSS @|:Spreadsheets:Cascade|)
(define StripCascadeSS @|:Spreadsheets:StripCascade|)
(define optimizedCascadeSS @|:Spreadsheets:optimizedCascade|)

;;  Define a simple VmScript timing test procedure.
;;  Note:  This procedure accepts two arguments
;;         proc:      the procedure to be tested
;;         count:     the number of iterations to test
(define (timingTest proc count) vars:(startTime endTime)
    (set! startTime (get-tick-count 0))
    (proc count) 
    (set! endTime (get-tick-count startTime))
    (writeln "Elapsed time is " endTime " Seconds" ) endTime)

;;  Define a simple VmScript ratio timing test procedure.
;;  Note:  This procedure accepts two arguments
;;         proc1:     the first procedure to be tested
;;         proc2:     the second procedure to be tested
;;         count:     the number of iterations to test
(define (timingRatio proc1 proc2 count) vars:(startTime endTime)
    (set! startTime (get-tick-count 0))
    (proc1 count) 
    (set! endTime (get-tick-count startTime))
    (set! startTime (get-tick-count 0))
    (proc2 count) 
    (set! endTime2 (get-tick-count startTime))
    (/ endTime endTime2))

;;  Setup Cascade Spreadsheet tests.
;;  Splice the Cascade Spreadsheet with compiled (but not flattened) formulas.
;;  Note: splices VmScript equivalents into the LRTE versions.
(defun recalcCascade(n) (recalc CascadeSS n))
(defun lazyCascade(n) vars:(i) 
    (loop for i from 1 to n do
        (setq CascadeSS[0 0] 1)))

;;  Setup optimizedCascade Spreadsheet tests.
;;  Applying the global optimization technique of "common subexpression Reuse",
;;  we can factor the Cascade Spreadsheet into the optimizedCascade Spreadsheet.
;;  The linearized version of the optimizedCascade Spreadsheet is as follows:
;;     #A10 = (sum #A1:A9)                          repeats 1 time
;;     #B10 = (sum #A10 #B1:B9)                     repeats 15 times in: #B10:P10       
;;     #A11 = (+ #A10 #A10)                         repeats 90 times in: #A11:A100       
;;     #B11 = (- (+ #A11 #B10 #B10) #$A10)          repeats 1350 times in: #B11:P100       
;;  Note: Simulates a VmScript linear equivalent of the optimizedCascade LRTE version.
(defun recalcOptimizedCascade(n) (recalc optimizedCascadeSS n))

;;  Setup StripCascade Spreadsheet tests.
;;  Splice the StripCascade Spreadsheet with compiled (but not flattened) formulas.
;;  Note: splices VmScript equivalents into the LRTE versions.
(defun recalcStripCascade(n) (recalc StripCascadeSS n))
(defun partialStripCascade(n) vars:(i) 
    (loop for i from 1 to n do
        (setq StripCascadeSS[0 0] 1)))
(defun lazyStripCascade(n) vars:(i) 
    (loop for i from 1 to n do
        (setq StripCascadeSS[0 1] 2)))

;;  Setup TotalCascade Spreadsheet tests.
(defun recalcTotalCascade(n) (recalc TotalCascadeSS n))
(defun partialTotalCascade(n) vars:(i) 
    (loop for i from 1 to n do
        (setq TotalCascadeSS[0 0] 2)))

;;  Perform timing tests
(setq TotalCascadeSS.lazy-recalc false)
(setq CascadeSS.lazy-recalc false)
(setq StripCascadeSS.lazy-recalc false)
(setq optimizedCascadeSS.lazy-recalc false)

(writeln "Starting StripCascade recalc")
(timingTest recalcStripCascade 1)
(writeln "Starting TotalCascade recalc")
(timingTest recalcTotalCascade 1)
(writeln "Starting Cascade recalc")
(timingTest recalcCascade 1)

;;  Close all open windows and destroy all Spreadsheet references

(close-window  CascadeSS.window true)
(close-window  StripCascadeSS.window true)
(close-window  optimizedCascadeSS.window true)
(close-window  TotalCascadeSS.window true)

(setq TotalCascadeSS #void)
(setq CascadeSS #void)
(setq StripCascadeSS #void)
(setq optimizedCascadeSS #void)

;;  Notify user of completion
(writeln "Spreadsheet Timing Test completed")

