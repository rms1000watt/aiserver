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
;;  Title:    Lisp Procedures Test
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the Lisp Procedures
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_LispProc.sl")

;;************************************************************************
;; Tests for the addMethod Lisp Procedure
;;************************************************************************

(addMethod  Integer:  square:  (lambda  (n)  (*  n  n)))
(testit "(send  square:  2)" 4)
(testit "(square:  2)" 4)

;;************************************************************************
;; Tests for the append Lisp Procedure
;;************************************************************************

(testit "(append  '(A  B)  '(C  D))" '(A  B  C  D))

(testit "(stringAppend  \"Hello\"  \" there\")" "Hello there")

(testit "(append  #(1  2  3)  #(4  5  6))" #(1  2  3  4  5  6))

(testit "(append  #{A: 1  B: 2}  #{C: 5  D: 6})" #{A: 1  B: 2  C: 5  D: 6})

;;************************************************************************
;; Tests for the apply Lisp Procedure
;;************************************************************************

(define  X  '(2  5))
(testit "(apply  +  X)" 7)

(testit "(apply  +  #(1  2  3  4))" 10)

(testit "(apply  +  #{A: 1  B: 2  C: 3  D: 4})" 10)

;;************************************************************************
;; Tests for the assoc Lisp Procedure
;;************************************************************************

(testit "(assoc  'B  '((A . 1)  ((B  . 3)  . 2)  (B  . 4)))" '(B  . 4))

(testit "(assoc  'A  '((A . 1)  (B . 2)  (A . 3)))" '(A . 1))

(testit "(associate  'A  '((A . 1)  (B . 2)  (A . 3)))" '(A . 1))

;;************************************************************************
;; Tests for the isAtom Lisp Procedure
;;************************************************************************

(testit "(isAtom  -1)" true)

(testit "(isAtom  '(1  .  2))" false)

(testit "(isAtom  '(1  2))" false)

(testit "(isAtom  (cdr  '(1 . 2)))" true)

;;************************************************************************
;; Tests for the binaryInsert Lisp Procedure
;;************************************************************************

(testit "(binaryInsert #(1 3 5) 4)" 2)

(testit "(binaryInsert #(1 3 5) 1)" 0)

(testit "(binaryInsert #(1 3 5) 6)" 3)

(testit "(binaryInsert #{a: 4  c: 3} b:)" 1)

;;************************************************************************
;; Tests for the binarySearch Lisp Procedure
;;************************************************************************

(testit "(binarySearch #(1 3 5) 4)" false)

(testit "(binarySearch #(1 3 5) 1)" 0)

(testit "(binarySearch #{a: 4  c: 3} c:)" 1)

;;************************************************************************
;; Tests for the isBitVector Lisp Procedure
;;************************************************************************

(testit "(isBitVector (makeVector bit: 2))" true)

(testit "(isBitVector (makeVector 2))" false)

;;************************************************************************
;; Tests for the boolean Lisp Procedure
;;************************************************************************

(testit "(boolean  97)" true)

(testit "(isBoolean  false)" true)

;;************************************************************************
;; Tests for the isBoolean Lisp Procedure
;;************************************************************************

(testit "(isBoolean  97)" false)

(testit "(isBoolean  false)" true)

;;************************************************************************
;; Tests for the isBound Lisp Procedure
;;************************************************************************

(testit "(isBound  #{A: 23  B: 34  C:  45}  'B)" 1)

(testit "(isBound  #{A: 23  B: 34  C:  45}  'D)" false)

(testit "(isBound  #{A: 23  B: 34  C:  45}  'A)" 0)

(testit "(isBound  (new Dictionary: A: 23)  'A)" 0)

(testit "(isBound  (new Dictionary: A: 23)  'B)" false)

(testit "(isBound  (new Directory: 1 23)  1)" 0)

(testit "(isBound  (new Directory: 1 23)  2)" false)

;;************************************************************************
;; Tests for the isByteVector Lisp Procedure
;;************************************************************************

(testit "(isByteVector (makeVector byte: 2))" true)

(testit "(isByteVector (makeVector 2))" false)

;;************************************************************************
;; Tests for the c...r Lisp Procedure
;;************************************************************************

(testit "(cadr  '(A  B  C))" 'B)

(testit "(caddar  '((1  2  3)  (4  5  6)  (7  8  9)))" 3)

;;************************************************************************
;; Tests for the call_cc Lisp Procedure
;;************************************************************************

(testit "(call_cc 
             (lambda  (exit) pvars:(callme)
                 (setq callme exit) 
                 (mapc (lambda  (x) (if  (isNegative  x) (callme  x))) 
                       #(54  0  37  -3  245  19)) 
                 true))" -3)

;;************************************************************************
;; Tests for the character Lisp Procedure
;;************************************************************************

(testit "(character  8)" #\backspace)

(testit "(character  13)" #\return)

;;************************************************************************
;; Tests for the isChar Lisp Procedure
;;************************************************************************

(testit "(isChar  #\\a)" true)

(testit "(isChar  22)" false)

;;************************************************************************
;; Tests for the isCharAlphabetic Lisp Procedure
;;************************************************************************

(testit "(isCharAlphabetic  #\\3)" false)

(testit "(isCharAlphabetic  #\\a)" true)

;;************************************************************************
;; Tests for the isCharLowercase Lisp Procedure
;;************************************************************************

(testit "(isCharLowercase  #\\a)" true)

(testit "(isCharLowercase  #\\A)" false)

;;************************************************************************
;; Tests for the isCharLowercase Lisp Procedure
;;************************************************************************

(testit "(isCharUppercase  #\\B)" true)

(testit "(isCharUppercase  #\\b)" false)

;;************************************************************************
;; Tests for the isCharNumeric Lisp Procedure
;;************************************************************************

(testit "(isCharNumeric  #\\3)" true)

(testit "(isCharNumeric  #\\a)" false)

;;************************************************************************
;; Tests for the isCharWhitespace Lisp Procedure
;;************************************************************************

(testit "(isCharWhitespace  #\\newline)" true)

(testit "(isCharWhitespace  #\\*)" false)

;;************************************************************************
;; Tests for the compare Lisp Procedure
;;************************************************************************

(testit "(compare  1 1)" 0)

(testit "(compare \"Hello\"  Hello:)" 0)

(testit "(compare  #{a:1  b: 2  c: 3} #{a:1  b: 2  c: 3})" 0)

(testit "(compare (makeDictionary a:1 c: 3)  (makeDictionary a:1  c: 3))" 0)

(testit "(compare (new Directory: a:1 c: 3)  (new Directory: a:1  c: 3))" 0)

(testit "(compare  #(1  2  3) #(1  2  3))" 0)

(testit "(compare  #(1  2  3) #(1  4  3))" -1)

(testit "(compareEQ  #(1  2  3) #(1  2  3))" true)

(testit "(compareLE  #(1  2  3) #(1  4  3))" true)

(testit "(compareGT  #(1  2  3) #(1  4  3))" false)

;;************************************************************************
;; Tests for the cons Lisp Procedure
;;************************************************************************

(testit "(cons  'A  'B)" '(A . B))

(testit "(cons  'A  '())" '(A))

(testit "(cons  \"A\"  '('B  'C))" '("A"  'B  'C))

(testit "(cons  '('A  'B)  'C)" '(('A  'B)  . C))

;;************************************************************************
;; Tests for the date Lisp Procedure
;;************************************************************************

(testit "(date  \"#Jan,1,1993\")" #Jan,1,1993)

(testit "(date  -1461)" #Jan,1,4BC)

;;************************************************************************
;; Tests for the isDate Lisp Procedure
;;************************************************************************

(testit "(isDate  #Jan,1,1993)" true)

(testit "(isDate  -1461)" false)

;;************************************************************************
;; Tests for the defineStructure Lisp Procedure
;;************************************************************************

(testit "(defineStructure employee:  'name  'job  'salary)" 'employee)

(addMethod  employee:  talk: (lambda (me) "I am an employee."))

(testit "(type (new employee:))" Structure:)

(testit "(ref (new employee: salary: 45.89) salary:)" 45.89)

(testit "(send talk: (new employee:))" "I am an employee.")

(testit "(defineStructure manager: include: employee: department:)" 'manager)

(addMethod  manager:  talk: (lambda(me)  "I am a manager."))

(testit "(ref (new manager: salary: 45.89) salary:)" 45.89)

(testit "(send talk: (new manager:))" "I am a manager.")

;;************************************************************************
;; Tests for the delete Lisp Procedure
;;************************************************************************

(testit "(remove 2  \'(1 2 3))" '(1 3))

(testit "(remove 2  #(1 2 3))" #(1 3))

(testit "(remove b:  #{a:1  b: 2  c: 3})" #{a: 1  c: 3})

(testit "(remove b:  (makeDictionary a:1  b: 2  c: 3))" (makeDictionary a:1  c: 3))

(testit "(remove b:  (new Directory: a:1  b: 2  c: 3))" (new Directory: a:1  c: 3))

(testit "(delq 2  \'(1 2 3))" '(1 3))

(testit "(delq 2  #(1 2 3))" #(1 3))

(testit "(delq b:  #{a:1  b: 2  c: 3})" #{a: 1  c: 3})

;;************************************************************************
;; Tests for the isDictionary Lisp Procedure
;;************************************************************************

(testit "(isDictionary  #{a:1  b: 2  c: 3})" false)

(testit "(isDictionary  (makeDictionary a:1  b: 2  c: 3))" true)

;;************************************************************************
;; Tests for the isStructure Lisp Procedure
;;************************************************************************

(testit "(isStructure  #{a:1  b: 2  c: 3})" true)

(testit "(isStructure  (makeDictionary a:1  b: 2  c: 3))" false)

;;************************************************************************
;; Tests for the isEqual Lisp Procedure
;;************************************************************************

(testit "(isEqual  1 1)" true)

(testit "(equal \"Hello\"  Hello:)" true)

(testit "(isEqual  #{a:1  b: 2  c: 3} #{a:1  b: 2  c: 3})" true)

(testit "(equal (makeDictionary a:1 c: 3)  (makeDictionary a:1  c: 3))" true)

(testit "(equal (new Directory: a:1 c: 3)  (new Directory: a:1  c: 3))" true)

(testit "(isEqual  #(1  2  3) #(1  2  3))" true)

(testit "(isEqual  #(1  2  3) #(1  4  3))" false)

;;************************************************************************
;; Tests for the error & errorTrap Lisp Procedure
;;************************************************************************

;;(testit "(errorTrap \'(error \"bug\") \'(+ 1 2))" 3)

;;************************************************************************
;; Tests for the eval Lisp Procedure
;;************************************************************************

(testit "(eval \'(+ 1 2))" 3)

(testit "(eval (compile \'(+ 1 2)))" 3)

(testit "(eval \"(+ 1 2)\")" 3)

;;************************************************************************
;; Tests for the fieldsOf Lisp Procedure
;;************************************************************************

(testit "(fieldsOf employee:)"  #{'name  #void 'job #void  'salary #void})

;;************************************************************************
;; Tests for the isFloatVector Lisp Procedure
;;************************************************************************

(testit "(isFloatVector (makeVector float: 0))"  true)

(testit "(isFloatVector (makeVector 0))"  false)

;;************************************************************************
;; Tests for the gc Lisp Procedure
;;************************************************************************

(testit "(gc)"  #void)

;;************************************************************************
;; Tests for the getProp, putprop & proplist Lisp Procedures
;;************************************************************************

(testit "(putprop  BLUE-WHALE:  OCEAN:  MAMMAL:)"  OCEAN:)

(testit "(getProp  BLUE-WHALE:  MAMMAL:)"  OCEAN:)

(testit "(proplist  BLUE-WHALE:)"  '(MAMMAL  OCEAN))

;;************************************************************************
;; Tests for the integer Lisp Procedures
;;************************************************************************

(testit "(integer  #\\return)"  13)

(testit "(integer  \"23\")"  23)

;;************************************************************************
;; Tests for the isInteger Lisp Procedures
;;************************************************************************

(testit "(isInteger  #\\return)"  false)

(testit "(isInteger  23)"  true)

(testit "(isInteger  (+ 2.1 2))"  false)

(testit "(isInteger  (addi 2 2))"  true)

;;************************************************************************
;; Tests for the isIntegerVector Lisp Procedure
;;************************************************************************

(testit "(isIntegerVector (makeVector integer: 0))"  true)

(testit "(isIntegerVector (makeVector 0))"  false)

;;************************************************************************
;; Tests for the isFunction Lisp Procedure
;;************************************************************************

(testit "(isFunction +)"  true)

(testit "(isFunction (lambda () (+ 1 1)))"  true)

(testit "(isFunction send)"  false)

;;************************************************************************
;; Tests for the isType Lisp Procedure
;;************************************************************************

(testit "(isType Function: +)"  true)

(testit "(isType Lambda: (lambda () (+ 1 1)))"  true)

(testit "(isType Structure: (new employee:))"  true)

(testit "(isType Integer: 22.3)"  false)

;;************************************************************************
;; Tests for the last Lisp Procedure
;;************************************************************************

(testit "(last \'(1 2 3 4))"  '(4))

(testit "(last-pair \'(1 2 3 4))"  '(4))

;;************************************************************************
;; Tests for the length Lisp Procedure
;;************************************************************************

(testit "(length \'(1 2 3 4))"  4)

(testit "(length #(1 2 3 4))"  4)

(testit "(length #{a: 1 b: 2 c: 3})"  3)

(testit "(length (makeDictionary a: 1 b: 2 c: 3))"  3)

(testit "(length (new Directory: a: 1 b: 2 c: 3))"  3)

(testit "(length |1 2 3 4|:)"  7)

;;************************************************************************
;; Tests for the list Lisp Procedure
;;************************************************************************

( testit "(list  1  2  3  4)" '(1 2 3 4))

( testit "(list  1  . 2)" '(1 . 2))

( testit "(list)" #void)

;;************************************************************************
;; Tests for the isMacro Lisp Procedure
;;************************************************************************

(defmacro  foo(x)  '(x x x))
( testit "(isMacro  foo)" true )

;;************************************************************************
;; Tests for the makeDictionary Lisp Procedure
;;************************************************************************

( testit "(ref (define  dic  (makeDictionary  X:  22  Y:  34)) Y:)" 34)

( testit "dic.X" 22 )

;;************************************************************************
;; Tests for the new Directory: Lisp Procedure
;;************************************************************************

( testit "(ref (define  dic  (new Directory:  X:  22  Y:  34)) Y:)" 34)

( testit "dic.X" 22 )

;;************************************************************************
;; Tests for the makeStructure Lisp Procedure
;;************************************************************************

( testit "(define  env  (makeStructure  x:  22  y:  34))" '#{x:  22  y:  34} )

( testit "env.x" 22 )

;;************************************************************************
;; Tests for the makeObject Lisp Procedure
;;************************************************************************

( testit "(makeObject 0)" #void)

;;************************************************************************
;; Tests for the makeString Lisp Procedure
;;************************************************************************

( testit "(makeString  'MySymbol)" "MySymbol" )

;;************************************************************************
;; Tests for the makeSymbol Lisp Procedure
;;************************************************************************

( testit "(makeSymbol  \"MySymbol\" )" 'MySymbol )

;;************************************************************************
;; Tests for the makeSymbol Lisp Procedure
;;************************************************************************

( testit "(makeQuotedSymbol  \"MySymbol\" )" ''MySymbol )

;;************************************************************************
;; Tests for the makeVector Lisp Procedure
;;************************************************************************

( testit "(makeVector  3  1  .  8) " #(1  1  1  .  8) )

( testit "(makeVector  normal: 3  1  .  8) " #(1  1  1  .  8) )

;;************************************************************************
;; Tests for the map Lisp Procedure
;;************************************************************************

( testit "(map  isEven  \'(1  2  3  4))" '(false  true  false  true))

( testit "(map  floor  #(1.2  2.44))" #(1 2))

( testit "(map  add1  #{A: 1  B: 2  C: 3  D: 4})" #{A: 2  B: 3  C: 4  D: 5})

( testit "(ref (map add1 (makeDictionary A: 1  B: 2  C: 3  D: 4)) C:)" 4)

( testit "(ref (map add1 (new Directory: A: 1  B: 2  C: 3  D: 4)) C:)" 4)

;;************************************************************************
;; Tests for the mapc Lisp Procedure
;;************************************************************************

( testit "(mapc  isEven  \'(1  2  3  4))" true)

( testit "(mapc  floor  #(1.2  2.44))" 2)

( testit "(mapc  floor  (makeVector 2 1.2  2.44))" 2)

( testit "(mapc  add1  #{A: 1  B: 2  C: 3  D: 4})" 5)

;;************************************************************************
;; Tests for the member Lisp Procedure
;;************************************************************************

( testit "(member  \'B  \'(A  B  C))" 1)

;( testit "(memq  2  (makeVector integer: 3 1 2 3))" 1)
( testit "(member  2  (makeVector integer: 3 1 2 3))" 1)

;( testit "(memv  2  (makeVector integer: 3 1 2 3))" 1)
( testit "(member  2  (makeVector integer: 3 1 2 3))" 1)

;( testit "(memv  \'B  \'(A  B  C))" 1)
( testit "(member  \'B  \'(A  B  C))" 1)

( testit "(member  2  #(1 2 3)) "  1)

( testit "(member  2  (makeVector number: 3 1 2 3))" 1)

( testit "(member  #\\b  (makeVector byte: 3 #\\a #\\b #\\c))" 1)

;( testit "(memq  B:  #{A: 1  B: 2  C: 3})" 1)
( testit "(member  B:  #{A: 1  B: 2  C: 3})" 1)

;( testit "(ref (makeDictionary A: 1  B: 2  C: 3) (memv  B:  (makeDictionary A: 1  B: 2  C: 3)))" 2)
( testit "(ref (makeDictionary A: 1  B: 2  C: 3) (member  B:  (makeDictionary A: 1  B: 2  C: 3)))" 2)

;( testit "(ref (new Directory: A: 1  1 2  C: 3) (member  B:  (new Directory: A: 1  B: 2  C: 3)))" 2)
( testit "(ref (new Directory: A: 1  1 2  C: 3) (member  B:  (new Directory: A: 1  B: 2  C: 3)))" 2)

;;************************************************************************
;; Tests for the isMemEqv Lisp Procedure
;;************************************************************************

( testit "(isMemEqv  \'B  \'(A  B  C))" true)

( testit "(isMemEqv  2  (makeVector integer: 3 1 2 3))" true)

( testit "(isMemEqv  2  (makeVector integer: 3 1 2 3))" true)

( testit "(isMemEqv  B:  \'(A  B  C))" true)

( testit "(isMemEqv  2  #(1 2 3)) "  true)

( testit "(isMemEqv  2  (makeVector number: 3 1 2 3))" true)

( testit "(isMemEqv  #\\b  (makeVector byte: 3 #\\a #\\b #\\c))" true)

( testit "(isMemEqv  B:  #{A: 1  B: 2  C: 3})" true)

( testit "(isMemEqv  B:  (makeDictionary A: 1  B: 2  C: 3))" true)

( testit "(isMemEqv  B:  (new Directory: A: 1  B: 2  C: 3))" true)

;;************************************************************************
;; Tests for the methodsOf Lisp Procedure
;;************************************************************************

(addMethod  Integer:  square:  (lambda  (n)  (*  n  n)))

( testit "(isMemEqv  square: (methodsOf Integer:))" true)

;;************************************************************************
;; Tests for the money Lisp Procedure
;;************************************************************************

( testit "(money  \"$-23.45\")" $-23.45)

( testit "(money  34.56)" $34.56)

;;************************************************************************
;; Tests for the isMoney Lisp Procedure
;;************************************************************************

( testit "(isMoney  \"$-23.45\")" false)

( testit "(isMoney  $34.56)" true)

;;************************************************************************
;; Tests for the new Lisp Procedure
;;************************************************************************

( testit "(ref (new  employee:  salary:  2201.34) salary:)" 2201.34)

;;************************************************************************
;; Tests for the isNull Lisp Procedure
;;************************************************************************

( testit "(isNull  nil) " true )

( testit "(isNull  '()) " true )

( testit "(isNull #void) " true )

;;************************************************************************
;; Tests for the number Lisp Procedure
;;************************************************************************

( testit "(charToNumber  #\\a) " 97)

( testit "(stringToNumber  \"34\") " 34)

( testit "(number  |34|:) " 34)

;;************************************************************************
;; Tests for the isNumber Lisp Procedure
;;************************************************************************

( testit "(isNumber  (+ 2 2)) " true )

( testit "(isNumber  \"34\") " false )

( testit "(isNumber  3) " true )

;;************************************************************************
;; Tests for the isNumberVector Lisp Procedure
;;************************************************************************

( testit "(isNumberVector  (makeVector number: 2 1 2)) " true )

;;************************************************************************
;; Tests for the isObject Lisp Procedure
;;************************************************************************

( testit "(isObject  (makeVector number: 2 1 2)) " true )

( testit "(isObject  22) " false )

;;************************************************************************
;; Tests for the objectToDictionary Lisp Procedure
;;************************************************************************

( testit "(objectToDictionary  #(A: 1  B: 5.1))" (makeDictionary A: 1  B: 5.1))

( testit "(objectToDictionary  '(A: 1  B: 5.1))" (makeDictionary A: 1  B: 5.1))

( testit "(objectToDictionary  #{A: 1  B: 5.1})" (makeDictionary A: 1  B: 5.1))

;;************************************************************************
;; Tests for the objectToDirectory Lisp Procedure
;;************************************************************************

( testit "(objectToDirectory  #(A: 1  B: 5.1))" (new Directory: A: 1  B: 5.1))

( testit "(objectToDirectory  '(A: 1  B: 5.1))" (new Directory: A: 1  B: 5.1))

( testit "(objectToDirectory  #{A: 1  B: 5.1})" (new Directory: A: 1  B: 5.1))

;;************************************************************************
;; Tests for the objectToStructure Lisp Procedure
;;************************************************************************

( testit "(objectToStructure  #(A: 1  B: 5.1))" #{A: 1  B: 5.1})

( testit "(objectToStructure  '(A: 1  B: 5.1))" #{A: 1  B: 5.1})

( testit "(ref (objectToStructure  (makeDictionary A: 1  B: 5.1)) B:)" 5.1)

;;************************************************************************
;; Tests for the objectToList Lisp Procedure
;;************************************************************************

( testit "(objectToList  #{A: (+  3  4)  B: 5.1}) " '(A (+ 3 4) B 5.1) )

;;************************************************************************
;; Tests for the objectToVector Lisp Procedure
;;************************************************************************

( testit "(objectToVector  #{A: (+  3  4)  B: 5.1}) " '#(A (+ 3 4) B 5.1) )

;;************************************************************************
;; Tests for the isObjectVector Lisp Procedure
;;************************************************************************

( testit "(isObjectVector  (makeVector object: 2 a: b:))" true )

;;************************************************************************
;; Tests for the pair Lisp Procedure
;;************************************************************************

( testit "(pair 1 2)" '(1 . 2) )

;;************************************************************************
;; Tests for the isPair Lisp Procedure
;;************************************************************************

( testit "(isPair  '(A  .  B))" true )

( testit "(isPair  #(1  2  3  4  5))" false )

( testit "(isPair  '(A  .  B))" true )

( testit "(isPair  #(1  2  3  4  5))" false )

;;************************************************************************
;; Tests for the isLambda Lisp Procedure
;;************************************************************************

( testit "(isLambda  1)" false )

( testit "(isLambda  (lambda (X)  (+  X  X)))" true)

( testit "(isLambda  car)" false)

;;************************************************************************
;; Tests for the resize Lisp Procedure
;;************************************************************************

( testit "(length  (resize #(1 2 3 4)  5))"  5)

( testit "(length  (resize #{a: 2 b: 4}  5))"  5)

( testit "(resize  #{A:  1  B:  2}  3)" #{'A  1  'B  2  '()  #void} )

;;************************************************************************
;; Tests for the reverse Lisp Procedure
;;************************************************************************

( testit "(reverse  '(1  2  3  4))" '(4  3  2  1) )

( testit "(reverse  #(1  2  3  4))" #(4  3  2  1) )

( testit "(resize  #(A  A  A  A  A)  6)" '#(A A A A A #void) )

( testit "(reverse  '(1  2  3  4))" '(4  3  2  1) )

( testit "(reverse  '((1  2)  3  4))" '(4  3  (1  2)) )

;;************************************************************************
;; Tests for the setLastCdr Lisp Procedure
;;************************************************************************

( testit "(setLastCdr  #(1  2  .  3)  22)" #(1 2 . 22) )

;;************************************************************************
;; Tests for the setCar Lisp Procedure
;;************************************************************************

(define  Y  '(3  4))
( testit "(setCar  Y  (*  3  4))" '(12  4) )

;;************************************************************************
;; Tests for the setCdr Lisp Procedure
;;************************************************************************

(define  Y  '(3  4))
( testit "(setCdr Y  (*  3  4))" '(3 . 12) )

;;************************************************************************
;; Tests for the sort Lisp Procedure
;;************************************************************************

(define  Y  '(3  2  1  4))
( testit "(sort  Y  <)" '(1  2  3  4) )

(define  Y  #(3  2  1  4))
( testit "(sort  Y  <)" #(1  2  3  4) )

(define  Y  #{b: 3  c: 2  d: 1  a: 4})
( testit "(sort  Y  <)"  #{a: 4 b: 3  c: 2  d: 1})

(setq Y (makeVector bit: 2 1 0))              ; define a bit vector
(setq X (makeVector integer: 2 1 0 ))
(testit "(sort  Y  < true)"  X)


(setq Y (makeVector integer: 3  2 1 0))          ; define a small vector
(setq X (makeVector integer: 3 2 1 0 ))
(testit "(sort Y < true)" X )

(setq Y (makeVector byte: 3 50 49 48))          ; define a byte vector
(setq X (makeVector integer: 3 2 1 0 ))
(testit "(sort Y < true)" X )

(setq Y (makeVector float: 3 7.9 7.8 7.7))      ; define a float vector
(setq X (makeVector integer: 3 2 1 0 ))
(testit "(sort Y < true)" X )

(setq Y (makeVector integer: 3 10 9 8 ))         ; define an integer vector
(setq X (makeVector integer: 3 2 1 0 ))
(testit "(sort Y < true)" X )

(setq Y (makeVector number: 3 10 9 8 ))          ; define a number vector
(setq X (makeVector integer: 3 2 1 0 ))
(testit "(sort Y < true)" X )

(setq Y (makeVector object: 3  ))               ; define an object vector
(setq Y[0] #(1 2 3))  
(setq Y[1] #(2 3 4)) 
(setq Y[2] #(1 5 6))       
(setq X (makeVector integer: 3 1 2 0 ))
(testit "(sort Y compareGT true)" X )

(setq Y '(3 2 1 ))                                 ; define an SPAIR 
(setq a (sort Y < true))  
(setq X (makeVector integer: 3 2 1 0 ))                        
(testit "(sort Y < true)" X )

; define a structure (sort by key and return an integer vector)
(setq Y (makeStructure a:50 b:49 c:48))          
(setq X (makeVector integer: 3 2 1 0 ))
(testit "(sort Y > true)" X )

; define a structure (sort by value and return an integer vector)
(setq Y (makeStructure a:50 b:49 c:48))         
(setq X (makeVector integer: 3 0 1 2 ))
(testit "(sort Y > byValue: true)" X )

; define a structure (sort by key and return the structure)
(setq Y (makeStructure a:50 b:49 c:48)) 
(setq X (makeStructure c:48 b:49 a:50)) 
(testit "(sort Y > )" X )

; define a structure (sort by Value and return the structure)
(setq Y (makeStructure a:50 b:49 c:48)) 
(setq X (makeStructure c:48 b:49 a:50)) 
(testit "(sort Y < byValue:)" X )



;;************************************************************************
;; Tests for the string and isString Lisp Procedure
;;************************************************************************

( testit "(string  97)" "97")

( testit "(isString \"\")" true)

( testit "(isText \"mymy\")" true)

(define  Y  "34.5")
( testit "(stringToNumber  Y)"  34.5)

;;************************************************************************
;; Tests for the isSymbol Lisp Procedure
;;************************************************************************

(define  Y  "Y")
( testit "(isSymbol  Y)"  false    )

( testit "(isSymbol  'Y)"  true    )

;;************************************************************************
;; Tests for the type Lisp Procedure
;;************************************************************************

( testit "(type  -4.2)"  Number:)

( testit "(type  \"text\")"  Text:)

( testit "(type  #(1 2 3))"  Vector:)

;;************************************************************************
;; Tests for the uniqueInsert Lisp Procedure
;;************************************************************************

(testit "(uniqueInsert #(1 3 5) 4)" 3)

(testit "(uniqueInsert #(1 3 5) 1)" 0)

(testit "(uniqueInsert #(1 3 5) 6)" 3)

(testit "(uniqueInsert #{a: 4  c: 3} b:)" 2)

;;************************************************************************
;; Tests for the delete Lisp Procedure
;;************************************************************************

( testit "(delete  #(4.2  6  5.1)  1)"  #(4.2  5.1))

( testit "(delete  #{a: 4.2  b: 6  c: 5.1}  1)"  #{a: 4.2  c: 5.1})

;;************************************************************************
;; Tests for the vectorFill Lisp Procedure
;;************************************************************************

( testit "(vectorFill  #(4.2  6  5.1)  0)"  #(0  0  0)    )

;;************************************************************************
;; Tests for the insert Lisp Procedure
;;************************************************************************

( testit "(insert  #(4.2  5.1)  1  6)"  #(4.2  6  5.1))

( testit "(insert  #{a: 4.2  c: 5.1}  1  b: 6  )"  #{a: 4.2  b: 6  c: 5.1})

;;************************************************************************
;; Tests for the isVector Lisp Procedure
;;************************************************************************

( testit "(isVector  #(1  2))" true )

;;************************************************************************
;;  Define the test for Lambda cloning.
;;************************************************************************
(defun bar(x)
    pvars:(y)
    (defun new() (setq y (* y y)))
    (defun sum(x) (+ x y))
    (setq y x))

(bar 10)
(setq moo (new bar))
(testit "(bar.sum 10)" 20)
(testit "(moo.sum 10)" 110)

;;************************************************************************
;;  Define the test for Lambda polymorphism.
;;************************************************************************
(defun foo(newVec)
    pvars:(vec)
    (defun ref1(ix1) vec[ix1])
    (defun set1(ix1 newValue) (setq vec[ix1] newValue))
    (defun len() (length vec))
    (defun length(v) (^length v))
    (setq vec newVec)
    ) ;; end foo

(foo #(10 11 12 13))
(testit "foo[1]" 11)
(setq foo[2] 22)
(testit "foo[2]" 22)
(testit "(foo.Pv.length #(1 2 3))" 3)

;;************************************************************************
;;  Define the test for onError special form.
;;************************************************************************
(defun foo()
    (defun errHandler(err) (append "Foo got this error: " err))
    (onError errHandler)
    (error "badFoo")
    ) ;; end foo

(testit "(foo)" "Foo got this error: !badFoo!")

(testEnd "Test_LispProc.sl")

