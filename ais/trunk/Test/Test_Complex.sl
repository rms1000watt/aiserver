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
;;  Title:    Complex Numbers Autotest
;;
;;  Author:   Michael F. Korns
;;
;;  Project:  AIS Regression Test Suite 
;;
;;  Notes:    A subset of the vmcompiler is acceptance tested in this test script.
;;            Transcendental functions are not tested at this time.
;;
;;  Files:    RegTest.sl
;;

;#memory=100
;#memoryObjectHeaders=30

(runScript "RegTest.sl")

(setq scriptName "Complex Numbers Test")
(testStart "Test_Complex.sl")
(setq scriptCount 0)
 
Retry::

;; *******************************************************************
;; name:     Complex Number Tests
;; *******************************************************************
     
(defun testMe(c1)
    vars:(x)  
    (setq x (* (abs c1) (exp (* #ci (argument c1))))) (if (<> (string x) (string c1)) (goto Bad:))
    (setq x (* (abs c1) (+ (cos (argument c1)) (* #ci (sin (argument c1)))))) (if (<> (string x) (string c1)) (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe #c4i)"  true)

(defun testMe(title)
    vars:((Integer:n1 1) (Number:x1 1.0))  
    vars:(x (c1 #c2.0+2.0i) (c2 #c4.0+0.0i) (c3 #c0.0+4.0i) (c4 #c4.0-0.5i))  
    (setq x (* (abs c1) (exp (* #ci (argument c1))))) (if (<> (string x) (string c1)) (goto Bad:))
    (setq x (* (abs c1) (+ (cos (argument c1)) (* #ci (sin (argument c1)))))) (if (<> (string x) (string c1)) (goto Bad:))
    (setq x (* (abs c2) (exp (* #ci (argument c2))))) (if (<> (string x) (string c2)) (goto Bad:))
    (setq x (* (abs c2) (+ (cos (argument c2)) (* #ci (sin (argument c2)))))) (if (<> (string x) (string c2)) (goto Bad:))
    (setq x (* (abs c3) (exp (* #ci (argument c3))))) (if (<> (string x) (string c3)) (goto Bad:))
    (setq x (* (abs c3) (+ (cos (argument c3)) (* #ci (sin (argument c3)))))) (if (<> (string x) (string c3)) (goto Bad:))
    (setq x (* (abs c4) (exp (* #ci (argument c4))))) (if (<> (string x) (string c4)) (goto Bad:))
    (setq x (* (abs c4) (+ (cos (argument c4)) (* #ci (sin (argument c4)))))) (if (<> (string x) (string c4)) (goto Bad:))
    (setq x (sqrt -1)) (if (<> (string x) "#c1.0i") (goto Bad:))
    (setq x (sqrt #c4.0+0i)) (if (<> (string x) "#c2.0") (goto Bad:))
    (setq x (sqrt (* c1 c1))) (if (<> (string x) (string c1)) (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex Identities})"  true)

(defun testMe(title)
    vars:((Integer:n1 1) (Number:x1 1.0))  
    vars:(op x (c1 #c23.0+2.0i) (c2 #c23.0+2.0i))  
    (setq op +)
    (setq x (+ c1 c2)) (if (<> x #c46.0+4.0i) (goto Bad:))
    (setq x (+ n1 c2)) (if (<> x #c24.0+2.0i) (goto Bad:))
    (setq x (+ c1 n1)) (if (<> x #c24.0+2.0i) (goto Bad:))
    (setq x (+ x1 c2)) (if (<> x #c24.0+2.0i) (goto Bad:))
    (setq x (+ c1 x1)) (if (<> x #c24.0+2.0i) (goto Bad:))
    (setq x (op c1 c2)) (if (<> x #c46.0+4.0i) (goto Bad:))
    (setq x (op n1 c2)) (if (<> x #c24.0+2.0i) (goto Bad:))
    (setq x (op c1 n1)) (if (<> x #c24.0+2.0i) (goto Bad:))
    (setq x (op x1 c2)) (if (<> x #c24.0+2.0i) (goto Bad:))
    (setq x (op c1 x1)) (if (<> x #c24.0+2.0i) (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex Addition})"  true)

(defun testMe(title)
    vars:((Integer:n1 1) (Integer:n2 2) (Number:x1 1.0) (Number:x2 2.0))  
    vars:(op x (c1 #c1.0+1.0i) (c2 #c2.0+2.0i))  
    (setq op -)
    (setq x (- c1 c2)) (if (<> x #c-1.0-1.0i) (goto Bad:))
    (setq x (- n1 c2)) (if (<> x #c-1.0-2.0i) (goto Bad:))
    (setq x (- c1 n2)) (if (<> x #c-1.0+1.0i) (goto Bad:))
    (setq x (- x1 c2)) (if (<> x #c-1.0-2.0i) (goto Bad:))
    (setq x (- c1 x2)) (if (<> x #c-1.0+1.0i) (goto Bad:))
    (setq x (op c1 c2)) (if (<> x #c-1.0-1.0i) (goto Bad:))
    (setq x (op n1 c2)) (if (<> x #c-1.0-2.0i) (goto Bad:))
    (setq x (op c1 n2)) (if (<> x #c-1.0+1.0i) (goto Bad:))
    (setq x (op x1 c2)) (if (<> x #c-1.0-2.0i) (goto Bad:))
    (setq x (op c1 x2)) (if (<> x #c-1.0+1.0i) (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex Subtraction})"  true)

(defun testMe(title)
    vars:((Integer:n1 1) (Integer:n2 2) (Number:x1 1.0) (Number:x2 2.0))  
    vars:(op x (c1 #c1.0+1.0i) (c2 #c2.0+2.0i))  
    (setq op *)
    (setq x (* c1 c2)) (if (<> x #c4.0i) (goto Bad:))
    (setq x (* n1 c2)) (if (<> x #c2.0+2.0i) (goto Bad:))
    (setq x (* c1 n1)) (if (<> x #c1.0+1.0i) (goto Bad:))
    (setq x (* x1 c2)) (if (<> x #c2.0+2.0i) (goto Bad:))
    (setq x (* c1 x1)) (if (<> x #c1.0+1.0i) (goto Bad:))
    (setq x (op c1 c2)) (if (<> x #c4.0i) (goto Bad:))
    (setq x (op n1 c2)) (if (<> x #c2.0+2.0i) (goto Bad:))
    (setq x (op c1 n1)) (if (<> x #c1.0+1.0i) (goto Bad:))
    (setq x (op x1 c2)) (if (<> x #c2.0+2.0i) (goto Bad:))
    (setq x (op c1 x1)) (if (<> x #c1.0+1.0i) (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex Multiplication})"  true)

(defun testMe(title)
    vars:((Integer:n1 4) (Integer:n2 2) (Number:x1 4.0) (Number:x2 2.0))  
    vars:(op x (c1 #c4.0+4.0i) (c2 #c2.0+2.0i))  
    (setq op /)
    (setq x (/ c1 c2)) (if (<> x #c2.0) (goto Bad:))
    (setq x (/ n1 c2)) (if (<> x #c1.0-1.0i) (goto Bad:))
    (setq x (/ c1 n2)) (if (<> x #c2.0+2.0i) (goto Bad:))
    (setq x (/ x1 c2)) (if (<> x #c1.0-1.0i) (goto Bad:))
    (setq x (/ c1 x2)) (if (<> x #c2.0+2.0i) (goto Bad:))
    (setq x (op c1 c2)) (if (<> x #c2.0) (goto Bad:))
    (setq x (op n1 c2)) (if (<> x #c1.0-1.0i) (goto Bad:))
    (setq x (op c1 n2)) (if (<> x #c2.0+2.0i) (goto Bad:))
    (setq x (op x1 c2)) (if (<> x #c1.0-1.0i) (goto Bad:))
    (setq x (op c1 x2)) (if (<> x #c2.0+2.0i) (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex Division})"  true)

(defun testMe(title)
    vars:(x (c2 #c4.0-3.0i))  
    (setq x (abs c2)) (if (<> x 5.0) (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex abs})"  true)

(defun testMe(title)
    vars:(x (c1 #c4.0-3.0i))  
    (setq x (argument c1)) (if (<> (string x) "-0.6435011087933") (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex argument})"  true)

(defun testMe(title)
    vars:(x c1)
    (setq c1 (new Complex: 2 3))  
    (setq x (conjugate c1)) (if (<> x #c2.0-3.0i) (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex conjugate})"  true)

(defun testMe(title)
    vars:(x c1)
    (setq c1 (new Complex: 2.1 3))  
    (setq x (string (fraction c1))) (if (<> x "0.1") (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex fraction})"  true)

(defun testMe(title)
    vars:(x c1)
    (setq c1 (new Complex: -2.1 3))  
    (setq x (sign c1)) (if (<> x -1) (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex sign})"  true)

(defun testMe(title)
    vars:(x c1)
    (setq c1 (new Complex: -2.1 3))  
    (setq x (string (cos c1))) (if (<> x "#c-5.082619940996+8.647523471257i") (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex cos})"  true)

(defun testMe(title)
    vars:(x c1)
    (setq c1 (new Complex: -2.1 3))  
    (setq x (string (sin c1))) (if (<> x "#c-8.69050013501-5.057485133471i") (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex sin})"  true)

(defun testMe(title)
    vars:(x c1)
    (setq c1 (new Complex: -2.1 3))  
    (setq x (string (tan c1))) (if (<> x "#c0.004331341269009+1.002424058439i") (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex tan})"  true)

(defun testMe(title)
    vars:(x c1)
    (setq c1 (new Complex: -2.1 3))  
    (setq x (string (cosh c1))) (if (<> x "#c-4.102838942269-0.5675644558689i") (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex cosh})"  true)

(defun testMe(title)
    vars:(x c1)
    (setq c1 (new Complex: -2.1 3))  
    (setq x (string (sinh c1))) (if (<> x "#c3.981607997138+0.5848455080109i") (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex sinh})"  true)

(defun testMe(title)
    vars:(x c1)
    (setq c1 (new Complex: -2.1 3))  
    (setq x (string (tanh c1))) (if (<> x "#c-0.9715784852398-0.008143652278835i") (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex tanh})"  true)

(defun testMe(title)
    vars:(x y)
    (setq y #c4)(setq x (string (sqrt y))) (if (<> x "#c2.0") (goto Bad:))
    (setq y #c-4)(setq x (string (sqrt y))) (if (<> x "#c2.0i") (goto Bad:))
    (setq y #c4)(setq x (string (* (sqrt y) (sqrt y)))) (if (<> x "#c4.0") (goto Bad:))
    (setq y #c-4)(setq x (string (* (sqrt y) (sqrt y)))) (if (<> x "#c-4.0") (goto Bad:))
    (setq y #c4+4i)(setq x (string (* (sqrt y) (sqrt y)))) (if (<> x "#c4.0+4.0i") (goto Bad:))
    (setq y #c4-4i)(setq x (string (* (sqrt y) (sqrt y)))) (if (<> x "#c4.0-4.0i") (goto Bad:))
    (setq y #c-4-4i)(setq x (string (* (sqrt y) (sqrt y)))) (if (<> x "#c-4.0-4.0i") (goto Bad:))
    (setq y #c-4+4i)(setq x (string (* (sqrt y) (sqrt y)))) (if (<> x "#c-4.0+4.0i") (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex sqrt})"  true)

(defun testMe(title)
    vars:(x c1 c2)
    (setq c1 (new Complex: 4 4))  
    (setq c2 (new Complex: 0 .5))  
    (setq x (string (expt (expt c1 c2) (/ c2)))) (if (<> x "#c4.0+4.0i") (goto Bad:))
    (setq c1 (new Complex: 4 -4))  
    (setq c2 (new Complex: 0 .5))  
    (setq x (string (expt (expt c1 c2) (/ c2)))) (if (<> x "#c4.0-4.0i") (goto Bad:))
    (setq c1 (new Complex: -4 4))  
    (setq c2 (new Complex: 0 .5))  
    (setq x (string (expt (expt c1 c2) (/ c2)))) (if (<> x "#c-4.0+4.0i") (goto Bad:))
    (setq c1 (new Complex: -4 -4))  
    (setq c2 (new Complex: 0 .5))  
    (setq x (string (expt (expt c1 c2) (/ c2)))) (if (<> x "#c-4.0-4.0i") (goto Bad:))
    (setq c1 (new Complex: 4 4))  
    (setq c2 (new Complex: 0 .25))  
    (setq x (string (expt (expt c1 c2) (/ c2)))) (if (<> x "#c4.0+4.0i") (goto Bad:))
    (setq c1 (new Complex: 4 -4))  
    (setq c2 (new Complex: 0 .25))  
    (setq x (string (expt (expt c1 c2) (/ c2)))) (if (<> x "#c4.0-4.0i") (goto Bad:))
    (setq c1 (new Complex: -4 4))  
    (setq c2 (new Complex: 0 .25))  
    (setq x (string (expt (expt c1 c2) (/ c2)))) (if (<> x "#c-4.0+4.0i") (goto Bad:))
    (setq c1 (new Complex: -4 -4))  
    (setq c2 (new Complex: 0 .25))  
    (setq x (string (expt (expt c1 c2) (/ c2)))) (if (<> x "#c-4.0-4.0i") (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex expt})"  true)

(defun testMe(title)
    vars:(x c1)
    (setq c1 (new Complex: 4 4))  
    (setq x (string (exp (log c1)))) (if (<> x "#c4.0+4.0i") (goto Bad:))
    (setq c1 (new Complex: 4 -4))  
    (setq x (string (exp (log c1)))) (if (<> x "#c4.0-4.0i") (goto Bad:))
    (setq c1 (new Complex: -4 4))  
    (setq x (string (exp (log c1)))) (if (<> x "#c-4.0+4.0i") (goto Bad:))
    (setq c1 (new Complex: -4 -4))  
    (setq x (string (exp (log c1)))) (if (<> x "#c-4.0-4.0i") (goto Bad:))
    (setq c1 (new Complex: 16 4))  
    (setq x (string (exp (log c1)))) (if (<> x "#c16.0+4.0i") (goto Bad:))
    (setq c1 (new Complex: 16 -4))  
    (setq x (string (exp (log c1)))) (if (<> x "#c16.0-4.0i") (goto Bad:))
    (setq c1 (new Complex: -16 4))  
    (setq x (string (exp (log c1)))) (if (<> x "#c-16.0+4.0i") (goto Bad:))
    (setq c1 (new Complex: -16 -4))  
    (setq x (string (exp (log c1)))) (if (<> x "#c-16.0-4.0i") (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex log})"  true)

(defun testMe(title)
    vars:(x c1)
    (setq c1 (new Complex: 4 4))  
    (setq x (string (log (exp c1)))) (if (<> x "#c4.0+4.0i") (goto Bad:))
    (setq c1 (new Complex: 16 4))  
    (setq x (string (log (exp c1)))) (if (<> x "#c16.0+4.0i") (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex exp})"  true)

(defun testMe(title)
    vars:(x c1 c2)
    (setq c1 (new Complex: 4 0))  
    (setq c2 (new Complex: 2 0))  
    (setq x (string (logbase c1 c2))) (if (<> x "#c2.0") (goto Bad:))
    (setq c1 (new Complex: 16 0))  
    (setq c2 (new Complex: 4 0))  
    (setq x (string (logbase c1 c2))) (if (<> x "#c2.0") (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex logbase})"  true)

(defun testMe(title)
    vars:(x c1 c2)
    (setq c1 (new Complex: 100 0))  
    (setq x (string (log10 c1))) (if (<> x "#c2.0") (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex log10})"  true)

(defun testMe(title)
    vars:(x c1 c2)
    (setq c1 (new Complex: (/ (pi) 2) (/ (pi) 2)))  
    (setq x (string (acos (cos c1)))) (if (<> x "#c1.570796326795+1.570796326795i") (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex acos})"  true)

(defun testMe(title)
    vars:(x c1 c2)
    (setq c1 (new Complex: (/ (pi) 2) (/ (pi) 2)))  
    (setq x (string (asin (sin c1)))) (if (<> x "#c1.570796326795-1.570796326795i") (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex asin})"  true)

(defun testMe(title)
    vars:(x c1 c2)
    (setq c1 (new Complex: (/ (pi) 2) (/ (pi) 2)))  
    (setq x (string (atan (tan c1)))) (if (<> x "#c-1.570796326795+1.570796326795i") (goto Bad:))
    (return true)
    Bad::
    false)
(testit "(testMe {Complex atan})"  true)

(testEnd "Test_Complex.sl")

