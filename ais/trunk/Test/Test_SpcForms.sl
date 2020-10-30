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
;;  Title:    AIS Lisp Special Forms Test
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the Special Forms
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_SpcForms.sl")

(defun lw () 
 (l (ref (get-window scriptName) 'window)))

(defun mlw () 
 (ml (ref (get-window scriptName) 'window)))

(defun et (evalTxt result) 
 (equal (eval evalTxt) result ))

;;************************************************************************
;; Tests for the and special form
;;************************************************************************

(define and1 "(and  (=  12  12)  (>  2  1))")
(testit and1 true)

(define and1 "(and  (=  24  24)  (<  1  1))")
(testit and1 false)

(define and1 "(and  1  2  3  4  5)")
(testit and1 false)

(define and1 "(and)")
(testit and1 true)

(define and1 "(and  true  true)")
(testit and1 true)

(define and1 "(and  false  true)")
(testit and1 false)

(define and1 "(and  true  false)")
(testit and1 false)

(define and1 "(and  false  false)")
(testit and1 false)

(define and1 "(not (and))")
(testit and1 false)

(define and1 "(not (and  true  true))")
(testit and1 false)

(define and1 "(not (and  false  true))")
(testit and1 true)

(define and1 "(not (and  true  false))")
(testit and1 true)

(define and1 "(not (and  false  false))")
(testit and1 true)

(define and1 "(not (and))")
(testit and1 false)

(define and1 "(not (and  (not (and  false  false))  true))")
(testit and1 false)

(define and1 "(not (and  (not (and))  true))")
(testit and1 true)

(define and1 "(not (and  (not (and  true  false))  false))")
(testit and1 true)

(define and1 "(not (and  false  false))")
(testit and1 true)

;;************************************************************************
;; Tests for argCount special form
;;************************************************************************

(defun argcount1 (a b c) (argCount))
(testit  "(argcount1 a b c)" 3)

;;************************************************************************
;; Tests for argFetch special form
;;************************************************************************

(defun argfetch1 (...) (argFetch 1))
(testit  "(argfetch1 1 2 3)" 2)

;;************************************************************************
;; Tests for args: special form
;;************************************************************************

(define args1 (compile (morph (lisp "args:(x y) (+ x y)"))))
(testit  "(args1 1 2)" 3)

;;************************************************************************
;; Tests for the begin special form
;;************************************************************************

(define begin1 "(begin  (+  1  2)  (+  3  4))")
(testit begin1 7)

(define begin2 "(begin)")
(testit begin2 #void)

;;************************************************************************
;; Tests for the case special form
;;************************************************************************

(define case1 "(case  (*  2  3) ((2  3  5  7)  'PRIME)  ((1  4  6  8  9)  'COMPOSITE))")
(testit case1 'COMPOSITE)

(define case2 "(case  (*  2  3) ((2  3  5  7)  'PRIME)  (else  'COMPOSITE))")
(testit case2 'COMPOSITE)

(define case3 "(case  (*  4  3) ((2  3  5  7)  'PRIME)  ((1  4  6  8  9)  'COMPOSITE))")
(testit case3 false)

(define case4 "(case  (*  2  3) ((2  3  5  7)  (- 1 2) (+ 4 5))  ((1 4 6 8 9)  (+ 1 2) (* 4 5)))")
(testit case4 20)

;;************************************************************************
;; Tests for the cond special form
;;************************************************************************

(define cond1 "(cond ((>  3  2)  'GREATER) ((<  3  2)  'LESS-THAN))")
(testit cond1 'GREATER)

(define cond2 "(cond (false  (+ 6 7)) (else  (* 3 4)))")
(testit cond2 12)

(define cond3 "(cond (true  (+ 6 7) (- 6 5)) (else  (* 3 4)))")
(testit cond3 1)

;;************************************************************************
;; Tests for the define special form
;;
;;   (define  name)
;;   (define  name  exp)
;;   (define  (name  arg...)  vars:  (var...)  pvars:  (var...)  exp...)
;;   (define  macro: (name  arg...)  vars:  (var...)  pvars:  (var...)  exp...)
;;************************************************************************

(define define1 "(define foo 100)")
(testit define1 100)

(define define2 (define (foo ) (+ 1 1)))
(testit define2 2)

(define define3 (define (foo x) (+ 1 1 x)))
(testit "(define3 1)" 3)

(define define4 (define (foo x) vars:((y 1)) (+ 1 1 x y)))
(testit "(define4 1)" 4)

(define define5 (define (foo x) vars:((y 1))  pvars:((z 1)) (+ 1 1 x y z)))
(testit "(define5 1)" 5)

(define macro: (foo x y z) (+ x y z))
(testit "(foo 1 2 3)" 6)

(define define7 "(define foo)")
(testit define7 #void)

;;************************************************************************
;; Tests for the goto special form
;;************************************************************************

goto skipme:
(testError scriptName " *FAILURE* " "goto skipme:")
(error "specialforms")
skipme::

(goto skipme1:)
(testError scriptName " *FAILURE* " "(goto skipme1:)")
(error "specialforms")
skipme1::

(define (goto1) goto skipme: (error "specialforms") skipme:: true)
(testit goto1 true)

(define (goto2) (goto skipme:) (error "specialforms") skipme:: true)
(testit goto2 true)

;;************************************************************************
;; Tests for the gotoEQ special form
;;************************************************************************

(gotoLE 1 2 skipcc:)
(testError scriptName " *FAILURE* " "(gotoLE 1 2 skipcc:)")
(error "specialforms")
skipcc::

(gotoLT 1 2 skipcc1:)
(testError scriptName " *FAILURE* " "(gotoLT 1 2 skipcc1:)")
(error "specialforms")
skipcc1::

(gotoEQ 2 2 skipcc2:)
(testError scriptName " *FAILURE* " "(gotoEQ 2 2 skipcc2:)")
(error "specialforms")
skipcc2::

(gotoNE 1 2 skipcc3:)
(testError scriptName " *FAILURE* " "(gotoNE 1 2 skipcc3:)")
(error "specialforms")
skipcc3::

(gotoGT 3 2 skipcc4:)
(testError scriptName " *FAILURE* " "(gotoGT 3 2 skipcc4:)")
(error "specialforms")
skipcc4::

(gotoGE 3 2 skipcc5:)
(testError scriptName " *FAILURE* " "(gotoGE 3 2 skipcc5:)")
(error "specialforms")
skipcc5::

;;************************************************************************
;; Tests for the if special form
;;************************************************************************

(define if1 "(if true then 1 else 2)")
(testit if1 1)

(define if2 "(if true 1  2)")
(testit if2 1)

(define if3 "(if false 1)")
(testit if3 false)

;;************************************************************************
;; Tests for the lambda special form
;;************************************************************************

(define lambda_1 (lambda () (+ 1 1)))
(testit "(lambda_1)" 2)

(define lambda_2 (lambda (x) (+ 1 x)))
(testit "(lambda_2 1)" 2)

(define lambda_3 (lambda (x) vars:((y 1)) (+ y x)))
(testit "(lambda_3 1)" 2)

(define lambda_4 (lambda (x) vars:((y 1)) pvars:((z 1)) (+ y x z)))
(testit "(lambda_4 1)" 3)

;;************************************************************************
;; Tests for the let special form
;;************************************************************************

(define let1 "(let  ((X  2)  (Y  3))  (*  X  Y))")
(testit let1 6)

(define let2 "(let ((X 5))  (lambda (Y) (+  X  Y))) X")
(testit let2 5)

;;************************************************************************
;; Tests for the loop special form
;;************************************************************************

(define loop1  "(loop for x from 0 to 10 by 1 do 0 )")
(testit loop1 11)

(define loop1  "(loop for x from 0 until 10 do)")
(testit loop1 10)

(define loop2  "(loop for x from 10 until 0 by -1 do 0)")
(testit loop2 0)

(define loop3  "(loop for x from 10 to 0 by -1 do 0)")
(testit loop3 -1)

;;************************************************************************
;; Tests for the myself special form
;;************************************************************************

(defun foo() (myself))
(if (<> (foo) foo) (error "specialforms"))

;;************************************************************************
;; Tests for the not special form
;;************************************************************************

(define not1 "(not  false)")
(testit not1 true)

(define not2 "(not  (+ 2 2))")
(testit not2 true)

(define not3 "(not  (= 2 2))")
(testit not3 false)

;;************************************************************************
;; Tests for the or special form
;;************************************************************************

(define or1 "(or  (=  12  12)  (>  2  1))")
(testit or1 true)

(define or2 "(or (=  24  24)  (<  1  1))")
(testit or2 true)

(define or3 "(or  1  2  3  4  5)")
(testit or3 false)

(define or3 "(or)")
(testit or3 false)

(define or1 "(or  true  true)")
(testit or1 true)

(define or2 "(or true  false)")
(testit or2 true)

(define or3 "(or  false true)")
(testit or3 true)

(define or3 "(or  false false)")
(testit or3 false)

(define or3 "(or)")
(testit or3 false)

(define or1 "(not (or  true  true))")
(testit or1 false)

(define or2 "(not (or true  false))")
(testit or2 false)

(define or3 "(not (or  false true))")
(testit or3 false)

(define or3 "(not (or  false false))")
(testit or3 true)

(define or3 "(not (or))")
(testit or3 true)

(define or1 "(not (or  true  (not (or))))")
(testit or1 false)

(define or2 "(not (or (not (or  false false))  false))")
(testit or2 false)

(define or3 "(not (or  false true))")
(testit or3 false)

(define or3 "(not (or  (not (or  false true)) false))")
(testit or3 true)

;;************************************************************************
;; Tests for the and or combination special forms
;;************************************************************************

(define and1 "(and  (or  false true)  (not (or  (not (or  false true)) false)))")
(testit and1 true)

(define and1 "(and  false  (or true  false))")
(testit and1 false)

(define and1 "(and  true  false)")
(testit and1 false)

(define and1 "(and  false  false)")
(testit and1 false)

(define and1 "(not (and))")
(testit and1 false)

(define and1 "(not (and  true  true))")
(testit and1 false)

(define and1 "(not (and  false  true))")
(testit and1 true)

(define and1 "(not (or  (and  (or  false true)  (not (or  (not (or  false true)) false)))  (not (and  true  true))))")
(testit and1 false)

(define and1 "(not (and  false  false))")
(testit and1 true)

;;************************************************************************
;; Tests for the pvars special form
;;************************************************************************

(define pvars1 "(defun foo(x)  pvars:((y 4)) (+ x y)) (foo 6)")
(testit pvars1 10)

;;************************************************************************
;; Tests for the quote special form
;;************************************************************************

(define quote1 "(quote (A  (+  1  2)  (x  y)))")
(testit quote1 '(A  (+  1  2)  (x  y)))

(define quote2 "(quote (A))")
(testit quote2 '(A))

(define quote3 "(quote ())")
(testit quote3 #void)

(define quote4 "'()")
(testit quote4 #void)

(define quote5 "(quote 1)")
(testit quote5 1)

(define quote6 "(quote \"Hello\")")
(testit quote6 "Hello")

(define quote7 "(quote X)")
(testit quote7 "X")

(define quote8 "(quote true)")
(testit quote8 true)

(define quote9 "(quote false)")
(testit quote9 false)


;;************************************************************************
;; Tests for the return special form
;;************************************************************************

(define return1 "(begin (return 2) 3)")
(testit return1 2)

;;************************************************************************
;; Tests for the send special form
;;************************************************************************

(addMethod  Integer:  square: (lambda(n)  (*  n  n)))

(testit "(send  square:  0)" 0)

(testit "(send  square:  4)" 16)

;;************************************************************************
;; Tests for the setq special form
;;************************************************************************

(define setq1 "(setq  (ref  #(1  2  3)  2)  (*  3  4))")
(testit setq1 '#(1  2  12) )

(define setq2 "(setq  (ref  (makeString \"Hello\")  0)  #\\H)")
(testit setq2 "Hello" )

(define setq3 "(setq  (ref  #(1  2  3)  1)  4)")
(testit setq3  '#(1  4  3))

;;************************************************************************
;; Tests for the vars special form
;;************************************************************************

(define vars1 "(defun foo(x)  vars:((y 4)) (+ x y)) (foo 6)")
(testit vars1 10)

;;************************************************************************
;; Tests for the while special form
;;************************************************************************

(setq x 0)
(define while1 "(setq x 0) (while (< x 10) (++ x))")
(testit while1 false)

;;************************************************************************
;; Tests for the compound boolean special forms
;;************************************************************************

(define bool1 "(or (and))")
(testit bool1 true )

(define bool1 "(and (or))")
(testit bool1 false )

(define bool1 "(not (or (and)))")
(testit bool1 false )

(define bool1 "(not (and (or)))")
(testit bool1 true )

(define bool1 "(or (and true true true))")
(testit bool1 true )

(define bool1 "(and (or true false false))")
(testit bool1 true )

(define bool1 "(not (or (and true true true)))")
(testit bool1 false )

(define bool1 "(not (and (or false false true)))")
(testit bool1 false )


(testEnd "Test_SpcForms.sl")

