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
;;  Title:    SmartLisp Neural Net Test
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the Neural Net
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_Neural.sl")

;;************************************************************************
;; Tests for training on linear problem.
;;************************************************************************

(writeln   "*****************************************")
(writeln   "starting training on linear relationship.")

(loop for n from 0 until 10 do
   (setq aNN (makeNeuralNet 1 1 continuous:))
   (clear:aNN)
   (setq oldErr 10000)
   (setq in (makeVector number:  10 .01 .02 .03 .04 .05  .06  .07  .08  .09 .1))
   (setq x (makeVector number: 1 1))
   (setq y (makeVector number: 1 2))
   (loop for i from 0 to 10000 do
      (setq err 0)
      (loop for j from 0 until 10 do
         (setq x[0] in[j])
         (setq y[0] (+ .381 (* .297 in[j])))

         (propagateForward:aNN x)
         (propagateBackward:aNN y)

         (setq err (+ err aNN.error))
         ) ; end j loop

      (if (or (and (<= err .001) (> err oldErr)) (= err 0.0))
          (begin
             (writeln "Net trained well on linear problem in [" n "] simulations after [" i "] epochs.")
             (goto EndLinear:)
             )) ; end if
      (setq oldErr err)
      ) ;; end i loop
  (if (< err .001)
      (begin
         (writeln "Net trained well on linear problem in [" n "] simulations after [" i "] epochs.")
         (goto EndLinear:)
         )) ; end if
  );; end n loop

(writeln "Net failed to converge on linear problem in [" n "] simulations of [" i "] epochs.")
EndLinear::
(loop for j from 0 until 10 do
   (setq x[0] in[j])
   (setq y[0] (+ .381 (* .297 in[j])))
   (propagateForward:aNN x)
   (writeln ".381 + (.297 * " in[j] ") = " y[0]
            " not " aNN.output.outputs[0] 
            " so the error is " (- y[0] aNN.output.outputs[0]))
   ) ; end display loop
(writeln "Sum of squares of error = " (text err "##.########"))
(writeln "*****************************************")

;;************************************************************************
;; Tests for training on mapping problem.
;;************************************************************************

(writeln "*****************************************")
(writeln "starting training on mapping problem.")
(loop for n from 0 until 100 do
   (setq aNN (makeNeuralNet 1 1 continuous:))
   (clear:aNN)
   (setq oldErr 10000)
   (setq in (makeVector number:  10 .1  .2 .3 .4 .5 .6 .7 .8 .9 .1))
   (setq out (makeVector number: 10 .9  .8 .7 .6 .5 .4 .3 .2 .1 .9))
   (setq x (makeVector number: 1 0))
   (setq y (makeVector number: 1 0))
   (loop for i from 0 to 10000 do
      (setq err 0)
      (loop for j from 0 until 10 do
         (setq x[0] in[j])
         (setq y[0] out[j])

         (propagateForward:aNN x)
         (propagateBackward:aNN y)

         (setq err (+ err aNN.error))
         ) ; end j loop

      (if (or (and (<= err .001) (> err oldErr)) (= err 0.0))
          (begin
             (writeln "Net trained well on mapping problem in [" n "] simulations after [" i "] epochs.")
             (goto EndMapping:)
             )) ; end if
      (setq oldErr err)
      ) ;; end i loop
   (if (< err .001)
       (begin
          (writeln "Net trained well on mapping problem in [" n "] simulations after [" i "] epochs.")
          (goto EndMapping:)
          )) ; end if
   ) ;; end n loop

(writeln "Net failed to converge on mapping problem in [" n "] simulations of [" i "] epochs.")
EndMapping::
(loop for j from 0 until 10 do
   (setq x[0] in[j])
   (setq y[0] out[j])
   (propagateForward:aNN x)
   (writeln "mapping(" in[j] ") = " y[0]
            " not " aNN.output.outputs[0] 
            " so the error is " (- y[0] aNN.output.outputs[0]))
   ) ; end display loop
(writeln "Sum of squares of error = " (text err "##.########"))
(writeln "*****************************************")

;;************************************************************************
;; Tests for training on squares problem.
;;************************************************************************

(writeln "*****************************************")
(writeln "starting training on squares problem.")
(loop for n from 0 until 100 do
   (setq aNN (makeNeuralNet 1 1 continuous:))
   (clear:aNN)
   (setq oldErr 10000)
   (setq in (makeVector number:  10 .1  .2  .3  .4  .5  .6  .7  .8  .9  .1))
   (setq x (makeVector number: 1 0))
   (setq y (makeVector number: 1 0))
   (loop for i from 0 to 10000 do
      (setq err 0)
      (loop for j from 0 until 10 do
         (setq x[0] in[j])
         (setq y[0] (* in[j] in[j]))

         (propagateForward:aNN x)
         (propagateBackward:aNN y)

         (setq err (+ err aNN.error))
         ) ; end j loop

      (if (or (and (<= err .001) (> err oldErr)) (= err 0.0))
          (begin
             (writeln "Net trained well on squares problem in [" n "] simulations after [" i "] epochs.")
             (goto EndSquares:)
             )) ; end if
      (setq oldErr err)
      ) ;; end i loop
   (if (< err .001)
       (begin
          (writeln "Net trained well on squares problem in [" n "] simulations after [" i "] epochs.")
          (goto EndSquares:)
          )) ; end if
   ) ;; end n loop

(writeln "Net failed to converge on squares problem in [" n "] simulations of [" i "] epochs.")
EndSquares::
(loop for j from 0 until 10 do
   (setq x[0] in[j])
   (setq y[0] (* in[j] in[j]))
   (propagateForward:aNN x)
   (writeln "square(" in[j] ") = " y[0]
            " not " aNN.output.outputs[0] 
            " so the error is " (- y[0] aNN.output.outputs[0]))
   ) ; end display loop
(writeln "Sum of squares of error = " (text err "##.########"))
(writeln "*****************************************")

;;************************************************************************
;; Tests for training on sine problem.
;;************************************************************************

(writeln "*****************************************")
(writeln "starting training on sine problem.")
(loop for n from 0 until 100 do
   (setq aNN (makeNeuralNet 1 1 continuous:))
   (clear:aNN)
   (setq oldErr 10000)
   (setq in (makeVector number:  10 .1  .2  .3  .4  .5  .6  .7  .8  .9  .1))
   (setq x (makeVector number: 1 0))
   (setq y (makeVector number: 1 0))
   (loop for i from 0 to 10000 do
      (setq err 0)
      (loop for j from 0 until 10 do
         (setq x[0] in[j])
         (setq y[0] (sin in[j]))

         (propagateForward:aNN x)
         (propagateBackward:aNN y)

         (setq err (+ err aNN.error))
         ) ; end j loop

      (if (or (and (<= err .001) (> err oldErr)) (= err 0.0))
          (begin
             (writeln "Net trained well on sine problem in [" n "] simulations after [" i "] epochs.")
             (goto EndSin:)
             )) ; end if
      (setq oldErr err)
      ) ;; end i loop
   (if (< err .001)
       (begin
          (writeln "Net trained well on sine problem in [" n "] simulations after [" i "] epochs.")
          (goto EndSin:)
          )) ; end if
   ) ;; end n loop

(writeln "Net failed to converge on sine problem in [" n "] simulations of [" i "] epochs.")
EndSin::
(loop for j from 0 until 10 do
   (setq x[0] in[j])
   (setq y[0] (sin in[j]))
   (propagateForward:aNN x)
   (writeln "sine(" in[j] ") = " y[0]
            " not " aNN.output.outputs[0] 
            " so the error is " (- y[0] aNN.output.outputs[0]))
   ) ; end display loop
(writeln "Sum of squares of error = " (text err "##.########"))
(writeln "*****************************************")

;;************************************************************************
;; Tests for training on xor problem.
;;************************************************************************

(writeln "*****************************************")
(writeln "starting training on xor problem.")
(loop for n from 0 until 100 do
   (setq aNN (makeNeuralNet 2 1 binary:))
   (clear:aNN)
   (setq oldErr 10000)
   (setq in1 (makeVector integer:  4 0 0 1 1))
   (setq in2 (makeVector integer:  4 0 1 0 1))
   (setq x (makeVector number: 2 0 0))
   (setq y (makeVector number: 1 0))
   (loop for i from 0 to 10000 do
      (setq err 0)
      (loop for j from 0 until 4 do
         (setq x[0] (integer in1[j]))
         (setq x[1] (integer in2[j]))
         (setq y[0] (bitwiseXor in1[j] in2[j]))

         (propagateForward:aNN x)
         (propagateBackward:aNN y)

         (setq err (+ err aNN.error))
         ) ; end j loop

      (if (or (and (<= err .001) (> err oldErr)) (= err 0.0))
          (begin
             (writeln "Net trained well on xor problem in [" n "] simulations after [" i "] epochs.")
             (goto EndXor:)
             )) ; end if
      (setq oldErr err)
      ) ;; end i loop
   (if (< err .001)
       (begin
          (writeln "Net trained well on xor problem in [" n "] simulations after [" i "] epochs.")
          (goto EndXor:)
          )) ; end if
   ) ;; end n loop

(writeln "Net failed to converge on xor problem in [" n "] simulations of [" i "] epochs.")
EndXor::
(loop for j from 0 until 4 do
   (setq x[0] (integer in1[j]))
   (setq x[1] (integer in2[j]))
   (setq y[0] (bitwiseXor in1[j] in2[j]))
   (propagateForward:aNN x)
   (writeln "xor(" in1[j] "," in2[j] ") = " y[0]
            " not " aNN.output.outputs[0] 
            " so the error is " (- y[0] aNN.output.outputs[0]))
   ) ; end display loop
(writeln "Sum of squares of error = " (text err "##.########"))
(writeln "*****************************************")


(testEnd "Test_Neural.sl")


















