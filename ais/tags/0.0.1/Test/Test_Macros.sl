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
;;  Title:    AIS Lisp Builtin Macros Test
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the Builtin Macros
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_Macros.sl")

;;************************************************************************
;; Tests for the *= special form
;;************************************************************************

(setq f 12)
(testit "(*= f 2 )" 24)

;;************************************************************************
;; Tests for the ++ special form
;;************************************************************************

(setq f 12)
(testit "(++ f )" 13)

;;************************************************************************
;; Tests for the += special form
;;************************************************************************

(setq f 12)
(testit "(+= f 1)" 13)

;;************************************************************************
;; Tests for the -- special form
;;************************************************************************

(setq f 12)
(testit "(-- f )" 11)

;;************************************************************************
;; Tests for the -= special form
;;************************************************************************

(setq f 12)
(testit "(-= f 1 )" 11)

;;************************************************************************
;; Tests for the /= special form
;;************************************************************************

(setq f 12)
(testit "(/= f 2 )" 6)

;;************************************************************************
;; Tests for the defmacro special form
;;************************************************************************

(defmacro doit(x y)  (list '+ x y))
(testit "(doit 4 5)" 9)

;;************************************************************************
;; Tests for the addMethod special form
;;************************************************************************

(addMethod Integer:square: (lambda(self)  (* self self)))
(testit "(send square: 4)" 16)
(testit "(square:4)" 16)

;;************************************************************************
;; Tests for the defstruct special form
;;************************************************************************

(defstruct Employee Name: Salary:)
(testit "(ref (new Employee:) Name:)" #void)

(addMethod Employee:raise: (lambda(self amount) (setq self.Salary (+ self.Salary amount)) self))

(defstruct Manager include: Employee: Dept:)
(testit "(ref (new Manager:) Dept:)" #void)

(setq x (new Employee: Name: "John Doe" Salary: 100000))
(testit "x.Salary" 100000)
(testit "x.Name" "John Doe")
(testit "(ref (raise:x 20000) Salary:)" 120000)
(testit "x.Salary" 120000)

(setq y (new Manager: Name: "John Doe" Salary: 100000))
(testit "y.Salary" 100000)
(testit "y.Name" "John Doe")
(testit "(ref (raise:y 20000) Salary:)" 120000)
(testit "y.Salary" 120000)

;;************************************************************************
;; Tests for the defun special form
;;************************************************************************

(defun foo(x) (+ x x))
(testit "(foo 5)" 10)

(defun foo(x) vars:((y 2)) (+ x y))
(testit "(foo 5)" 7)

(defun foo(x) pvars:((y 2)) (+ x y))
(testit "(foo 5)" 7)

(defun foo(x) vars:((z 3)) pvars:((y 2)) (+ x y z))
(testit "(foo 5)" 10)

(defclone foo:moo(x) vars:((z 1)) pvars:((y 5)) (+ x y z))
(testit "(moo 5)" 11)

;;************************************************************************
;; Tests for the defclass special form
;;************************************************************************

(defclass foo(self) faces:((Mike true)) 
   svars:((Number:xs 1.0) (Number:ys 2.0))
   pvars:(setq using raise (base #{x:1.0 y:2.0}))
   pvars:((Number:xp 1.0) (Number:yp 2.0))
   cvars:(_ptr) 
   (+= xp yp) 
   (+= xs ys) 
   xs)

(defmethod foo:raise(self) 
   xs)

(defmacro foo:using(ptr self) 
   (setq _ptr ptr) 
   (macroReplace _ptr self '(vmregObjPointer %2 %1)))

(defmacro foo:setq(a b)
   vars:(self)
   (setq self foo.Sv)
   (cond
    ((and (isMember a self) (isMember b self)) (parse (macroReplace (offset self a) (offset self b) _ptr {(setq |regoffset:%3[:%1:]| |regoffset:%3[:%2:]|)})))
    ((isMember a self) (parse (macroReplace (offset self a) b _ptr {(setq |regoffset:%3[:%1:]| %2)})))
    ((isMember b self) (parse (macroReplace a (offset self b) _ptr {(setq %1 |regoffset:%3[:%2:]|)})))
    (else (parse (macroReplace a b {(setq %1 %2)})))
    ))

(defclass foo:moo() faces:((Mike true))
   svars:((Number:ws 10.0))
   pvars:(Hello)
   true)

(setq self (new foo))
(testit "(foo self)" 3.0)

(defun moo(self Number:a)
   regs:(WordPointer:pr Number:nx)
   vars:(temp)
   (foo@using pr self)
   (foo@setq nx xs)
   (+= nx a) 
   nx)

(setq self (new foo))
(testit "(moo self 3.0)" 4.0)

(defun moo(self Number:a)
   regs:(WordPointer:pr Number:nx)
   vars:((temp #{decl| (Number:xs 1.0) (Number:ys 2.0)}))
   (setq pr self)
   (+= pr[temp(xs)] pr[temp(ys)])
   (setq nx pr[temp(xs)])
   (+= nx a) 
   nx)

(setq self (new foo))
(testit "(moo self 3.0)" 6.0)

(testEnd "Test_Macros.sl")
