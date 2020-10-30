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
;;  Title:    Simulation Translator TestBench for ansi C
;;
;;  Author:   Michael F. Korns
;;
;;  Project:  SmartBase Simulator Project
;;
;;  Notes:    A translator for Simulation to ansi integer C
;;            is prototyped in this script.
;;
;;  Files:    No dependencies. 
;;

;; ********************************************************************
;; name:     C conversion rules Dictionary for initial expressions.
;; ********************************************************************
(define *ir* (makeDictionary))
(setq *ir*.setq 
      (lambda (x) (append "INT           " x[1] " = " x[2] ";")))
(setq *ir*.define *ir*.setq)
(setq *ir*.and 
      (lambda (x) (chain-op (cdr x) " && " "(" ")")))
(setq *ir*.or 
      (lambda (x) (chain-op (cdr x) " || " "(" ")")))
(setq *ir*.+ 
      (lambda (x) (chain-op (cdr x) " + " "(" ")")))
(setq *ir*.- 
      (lambda (x) (chain-op (cdr x) " - " "(" ")")))
(setq *ir*./ 
      (lambda (x) (chain-op (cdr x) " / " "(" ")")))
(setq *ir*.* 
      (lambda (x) (chain-op (cdr x) " * " "(" ")")))
(setq *ir*.^ 
      (lambda (x) (chain-op (cdr x) " ^ " "(" ")")))
(setq *ir*.= 
      (lambda (x) (chain-op (cdr x) " == " "(" ")")))
(setq *ir*.<> 
      (lambda (x) (chain-op (cdr x) " != " "(" ")")))
(setq *ir*.<= 
      (lambda (x) (chain-op (cdr x) " <= " "(" ")")))
(setq *ir*.>= 
      (lambda (x) (chain-op (cdr x) " >= " "(" ")")))
(setq *ir*.< 
      (lambda (x) (chain-op (cdr x) " < " "(" ")")))
(setq *ir*.> 
      (lambda (x) (chain-op (cdr x) " > " "(" ")")))
(setq *ir*.bitwiseShiftRight 
      (lambda (x) (chain-op (cdr x) " >> " "(" ")")))
(setq *ir*.bitwiseShiftLeft 
      (lambda (x) (chain-op (cdr x) " << " "(" ")")))
(setq *ir*.bitwiseAnd 
      (lambda (x) (chain-op (cdr x) " & " "(" ")")))
(setq *ir*.bitwiseNand 
      (lambda (x) (append "~(" (chain-op (cdr x) " & " "(" ")") ")")))
(setq *ir*.boolean-nand 
      (lambda (x) (append "!(" (chain-op (cdr x) " & " "(" ")") ")")))
(setq *ir*.bitwiseOr 
      (lambda (x) (chain-op (cdr x) " | " "(" ")")))
(setq *ir*.bitwiseNor 
      (lambda (x) (append "~(" (chain-op (cdr x) " | " "(" ")") ")")))
(setq *ir*.boolean-nor 
      (lambda (x) (append "!(" (chain-op (cdr x) " | " "(" ")") ")")))
(setq *ir*.bitwiseXor 
      (lambda (x) (chain-op (cdr x) " ^ " "(" ")")))
(setq *ir*.bitwiseNxor 
      (lambda (x) (append "~(" (chain-op (cdr x) " ^ " "(" ")") ")")))
(setq *ir*.boolean-nxor 
      (lambda (x) (append "!(" (chain-op (cdr x) " ^ " "(" ")") ")")))
(setq *ir*.bitwiseNot 
      (lambda (x) (append "~" x[1])))
(setq *ir*.boolean-not 
      (lambda (x) (append "!" x[1])))
(setq *ir*.add1 
      (lambda (x) (append "++" x[1])))
(setq *ir*.sub1 
      (lambda (x) (append "--" x[1])))
(setq *ir*.writeln 
      (lambda (x) (chain-function writeln: (cdr x) )))
(setq *ir*.cond 
      (lambda (x) 
          vars:(i n s)
          (setq n (length x))
          (setq s (append "if " x[1][0] (chain-op (cdr x[1]) " " " {" "}"))) 
          (loop for i from 2 until n do
             (setq s (append s _eol *idnt* "else if " x[i][0] (chain-op (cdr x[i]) " " " {" "}"))) ) 
          s))
(setq *ir*.if 
      (lambda (x) 
          vars:(i n s)
          (setq n (length x))
          (setq s (append "if " x[1] (chain-op (cdr x[2]) " " " {" "}"))) 
          (loop for i from 3 until n do
             (setq s (append s _eol *idnt* "else " x[i] (chain-op (cdr x[i]) " " " {" "}"))) ) 
          s))

;; ********************************************************************
;; name:     C conversion rules Dictionary for loop expressions.
;; ********************************************************************
(define *lr* (copy *ir*))
(setq *lr*.setq 
      (lambda (x) (append "" x[1] " = " x[2] ";")))
(setq *lr*.define *lr*.setq)

;; ********************************************************************
;; name:     chain-op
;; 
;; summary:  Translate the specified declarative simulation file into
;;           an ansi C procedural source file suitable for fast
;;           execution of discrete integer simulations.
;; Parms:    x:       The list of arguments to be chained.
;;           op:      The binary operator to use in the chain.
;;           lf:      The left enclosure of the chain.
;;           rg:      The right enclosure of the chain.
;; ********************************************************************
(defun chain-op(x op lf rg)
   vars:(i n s)
   (setq n (length x))
   (setq s (append lf x[0])) 
   (loop for i from 1 until n do
      (setq s (append s op x[i]))) 
   (setq s (append s rg)))

;; ********************************************************************
;; name:     chain-function
;; 
;; summary:  Translate the specified declarative simulation procedure
;;           reference into an ansi C function call suitable for fast
;;           execution of discrete integer simulations.
;; Parms:    op:      The procedure operator to use in the chain.
;;           x:       The list of arguments to be chained.
;; ********************************************************************
(defun chain-function(op x)
   vars:(i n s)
   (setq n (length x))
   (setq s (append "FSmartbase_Eval(TGVALUE(\"" op "\")," n)) 
   (loop for i from 0 until n do
      (if (and (isString x[i]) (<> x[i][0] #\())
          (setq s (append s ",TSTRING(\"" x[i] "\")"))
          (setq s (append s ",TINT(" x[i] ")"))))
   (setq s (append s ");")))

;; ********************************************************************
;; name:     translate-simulation-to-ic
;; 
;; summary:  Translate the specified declarative simulation file into
;;           an ansi C procedural source file suitable for for fast
;;           execution of discrete integer simulations.
;; Parms:    fileIN:  The declarative source file to be converted.
;;           fileOUT: The procedureal source file after conversion.
;; Global:   *dic*:   The expression Dictionary.
;;           *exp*:   The substitution expression Dictionary.
;;           *vec*:   The expression correct execution order vector.
;;           *init*:  The initial expression vector in correct execution order.
;;           *loop*:  The main loop expression vector in correct execution order.
;;           *last*:  The final expression vector in correct execution order.
;;           *ir*:    The C conversion rules Dictionary for initial expressions.
;;           *lr*:    The C conversion rules Dictionary for loop expressions.
;;           *idnt*:  The indentation for the current C source output lines.
;; ********************************************************************
(defun translate-simulation-to-ic(fileIN ...)
   vars:(i k m n proc sout srec row col 
         fileID filename name)
   (if (= (argCount) 2)
       (begin
          (setq filename (argFetch 1))
          (runScript fileIN))
       (setq filename fileIN))
   (simulate -1)
   ;; Create the expression dictionary.
   (setq *dic* (makeDictionary))
   (setq *exp* (makeDictionary))
   (setq *vec* (makeVector object: 0))
   (setq *init* (makeVector object: 0))
   (setq *loop* (makeVector object: 0))
   (setq *last* (makeVector object: 0))
   (setq *idnt* "   ")
   ;; Open the output file.
   (setq fileID (fileOpen filename 1 0))
   ;; Write out the ansi C file header code.
   (fwriteln fileID "#include \"fsmtbase.h\"" _eol)
   (fwriteln fileID "void main_init(void);")
   (fwriteln fileID "TVAL main_procedure(INT argc,TVAL argv[]);" _eol)
   (fwriteln fileID "void main_init()")
   (fwriteln fileID "{")
   (fwriteln fileID "FSmartbase_RegisterCProcedure((LpCHAR)\"main\",(LpFUNC)&main_procedure);")
   (fwriteln fileID "}" _eol _eol)
   (fwriteln fileID "TVAL main_procedure(INT argc,TVAL argv[])")
   (fwriteln fileID "{")
   (fwriteln fileID "register INT  _finish;")
   (fwriteln fileID "register INT  _clock = 0;" _eol)
   ;; Write out the _constraints variable initializations.
   (setq n (length _constraints))
   (loop for i from 0 until n do
        (setq sout (append "INT           " _constraints[i 0] " = " _initial ";"))
        (fwriteln fileID (substitute (substitute sout "&" "s_") "%" "w_")))
   ;; Write out the _states variable initializations.
   (setq n (length _states))
   (loop for i from 0 until n do
        (setq sout (append "INT           " _states[i 0] " = 0;"))
        (fwriteln fileID (substitute (substitute sout "&" "s_") "%" "w_")))
   ;; Dictionary the _constraintMatrix formulas in ascending dependency order.
   (setq proc 0)
   (setq i 0)
   (while (<> proc #void) do
        (setq proc (getFormula _constraintMatrix cache: i))
        (++ i)
        (if (<> proc #void)
            (begin
                (setq col proc[0])
                (setq row proc[1])
                (setq m (+ (* row 16000) col))
                (setq proc (getFormula _constraintMatrix col row))
                (setq sout (append "(setq " _constraints[m 0] 
                                   " " (decompile proc) ")" _eol))
                (setq name (makeSymbol (append "w_" (mid _constraints[m 0] 1 200))))
                (setq sout (substitute (substitute sout "&" "s_") "%" "w_"))
                (setq *vec*[(length *vec*)] name)
                (setq *dic*[name] (morph (lex sout)))
                (setq *exp*[name] (caddar (morph (lex sout)))))))
   ;; Dictionary the _stateMatrix formulas in ascending dependency order.
   (setq n (length _constraints))
   (setq proc 0)
   (setq i 0)
   (while (<> proc #void) do
        (setq proc (getFormula _stateMatrix cache: i))
        (++ i)
        (if (<> proc #void)
            (begin
                (setq col proc[0])
                (setq row proc[1])
                (setq m (+ (* row 16000) col))
                (setq proc (getFormula _stateMatrix col row))
                (setq sout (append "(setq " _states[m 0] 
                                   " " (decompile proc) ")" _eol))
                (setq name (makeSymbol (append "s_" (mid _states[m 0] 1 200))))
                (setq sout (substitute (substitute sout "&" "s_") "%" "w_"))
                (setq *vec*[(length *vec*)] name)
                (setq *dic*[name] (morph (lex sout)))
                (setq *exp*[name] (caddar (morph (lex sout)))))))
   ;; Perform all optimizations and transformations on the expression dictionary.
   (apply-rules)
   ;; Write out the optimized initial formulas in the correct execution order.
   (loop for i from 0 until (length *init*) do
        (fwriteln fileID *init*[i]))
   ;; Write out the ansi C file argument verification code.
   (fwriteln fileID _eol "if ((argc != 1) || (argv[0].Tag != TYINT)) return(TERROR(\"!arglist!\"));")
   (fwriteln fileID "_finish = argv[0].u.Int;" _eol)
   ;; Write out the optimized formulas in the correct execution order.
   (fwriteln fileID "for (_clock = 0;_clock <= _finish; _clock += 1)" _eol *idnt* "{")
   (loop for i from 0 until (length *loop*) do
        (fwriteln fileID *idnt* *loop*[i]))
   (fwriteln fileID *idnt* "}" _eol) 
   ;; Write out the optimized finish formulas in the correct execution order.
   (fwriteln fileID "_clock = _finish;")
   (loop for i from 0 until (length *last*) do
        (fwriteln fileID *last*[i]))
   ;; Write out the ansi C file trailer code.
   (fwriteln fileID _eol _eol "return(argv[0]);")
   (fwriteln fileID "}" _eol _eol)
   (fileClose fileID 1))  

(setq tic translate-simulation-to-ic)

;; ********************************************************************
;; name:     apply-rules
;; 
;; summary:  Transform the expression dictionary formulas by applying
;;           transformation rules. These rules may delete items from
;;           the *vec* expression vector, may place items in the *init*
;;           or *last* expression vectors.
;; Parms:    none
;; Globals:  *dic*:   The declarative expression dictionary to be transformed.
;;           *exp*:   The substitution expression dictionary.
;;           *vec*:   The expression correct execution order vector.
;;           *init*:  The initial expression vector in correct execution order.
;;           *loop*:  The main loop expression vector in correct execution order.
;;           *last*:  The final expression vector in correct execution order.
;;           *i*:     The current index into the *vec* expression vector.
;; ********************************************************************
(defun apply-rules()
   vars:(i)
   ;; Apply local migration rules to the expression dictionary.
   (loop for i from 0 until (length *vec*) do
       (setq *i* i)
       (if (and (<> *vec*[i] #void) (migration-rules *dic*[*vec*[i]][0]))
           (setq *vec*[i] #void)))
   ;; Apply local transformation rules to the expression dictionary.
   (loop for i from 0 until (length *vec*) do
       (setq *i* i)
       (if (<> *vec*[i] #void)
           (setq *dic*[*vec*[i]] (morph *dic*[*vec*[i]] local-rules))))
   ;; Move the expression dictionary formulas to the *loop* vector.
   (loop for i from 0 until (length *vec*) do
       (if (<> *vec*[i] #void)
           (setq *loop*[(length *loop*)] *dic*[*vec*[i]][0])))
   ;; Apply C conversion rules to the *init* expression vector.
   (loop for i from 0 until (length *init*) do
       (if (<> *init*[i] #void)
           (setq *init*[i] (morph *init*[i] init-rules))))
   ;; Apply C conversion rules to the *loop* expression vector.
   (loop for i from 0 until (length *loop*) do
       (if (<> *loop*[i] #void)
           (setq *loop*[i] (morph *loop*[i] loop-rules))))
   ;; Apply C conversion rules to the *last* expression vector.
   (loop for i from 0 until (length *last*) do
       (if (<> *last*[i] #void)
           (setq *last*[i] (morph *last*[i] loop-rules))))
   true)

;; ********************************************************************
;; name:     init-rules
;; 
;; summary:  If the head of the specified sub list is in the *ir* rules
;;           Dictionary, then apply the C conversion rule to the sub list 
;;           passed by the morph procedure.
;; Parms:    sexp:    The sub list passed by morph to be converted into C.
;; Globals:  *ir*:    The C conversion rules Dictionary for initial expressions.
;; ********************************************************************
(defun init-rules(sexp)
   (if (<> *ir*[sexp[0]] #void) 
       (*ir*[sexp[0]] sexp)))

;; ********************************************************************
;; name:     loop-rules
;; 
;; summary:  If the head of the specified sub list is in the *lr* rules
;;           Dictionary, then apply the C conversion rule to the sub list 
;;           passed by the morph procedure.
;; Parms:    sexp:    The sub list passed by morph to be converted into C.
;; Globals:  *lr*:    The C conversion rules Dictionary for loop expressions.
;; ********************************************************************
(defun loop-rules(sexp)
   (if (<> *lr*[sexp[0]] #void) 
       (*lr*[sexp[0]] sexp)))

;; ********************************************************************
;; name:     local-rules
;; 
;; summary:  Transform the specified sub list, passed by morph, 
;;           by applying these local transformation rules.
;; Parms:    sexp:    The sub list passed by morph to be transformed.
;;                    declarative expression dictionary *dic*. 
;; Globals:  *dic*:   The declarative expression dictionary to be transformed.
;;           *vec*:   The expression correct execution order vector.
;;           *init*:  The initial expression vector in correct execution order.
;;           *loop*:  The main loop expression vector in correct execution order.
;;           *last*:  The final expression vector in correct execution order.
;; ********************************************************************
(defun local-rules(sexp)
   vars:(x)
   (cond 
       ((<> (setq x (local-rule1 sexp)) false) x)
       ((<> (setq x (local-rule2 sexp)) false) x)))

;; ********************************************************************
;; name:     local-rule1
;; 
;; summary:  Transform the specified sub list, passed by morph, 
;;           if it matches the following transformation pattern.
;;             (setq name (cond (test result) (test result) ...))
;;           and return the following morphed pattern.
;;             (cond (test (setq name result)) (test (setq name result)) ...)
;; Parms:    sexp:    The sub list passed by morph to be transformed.
;; ********************************************************************
(defun local-rule1(sexp)
   vars:(i n name sub tail head)
   (if (and (= (length sexp) 3) 
            (= sexp[0] setq:) 
            (isSymbol (setq name sexp[1])) 
            (<> (mid (string name) 2 7) "initial") 
            (<> (mid (string name) 2 7) "monitor") 
            (isPair (setq head sexp[2]))
            (= head[0] cond:))
       (begin
          (loop for i from 1 until (length sexp[2]) do
             (setq sub sexp[2][i])
             (setq n (sub1 (length sub)))
             (if (and (= (string sub[0]) "else") (= sub[1] name))
                 (setCdr head #void)
                 (setq sub[n] (list setq: name sub[n])))
             (setq head (cdr head)))
           sexp[2])
       false))

;; ********************************************************************
;; name:     local-rule2
;; 
;; summary:  Transform the specified sub list, passed by morph, 
;;           if it matches the following transformation patterns.
;;              (setq __monitorNN  expression)
;;              (setq __initialNN  expression)
;;           and return the following morphed pattern.
;;              expression
;; Parms:    sexp:    The sub list passed by morph to be transformed.
;; ********************************************************************
(defun local-rule2(sexp)
   vars:(i n name sub tail head)
   (if (and (= (length sexp) 3) 
            (= sexp[0] setq:) 
            (isSymbol (setq name sexp[1]))
            (or  
               (= (mid (string name) 2 7) "monitor")
               (= (mid (string name) 2 7) "initial")))
       sexp[2]))

;; ********************************************************************
;; name:     migration-rules
;; 
;; summary:  Migrate the specified expression to the *init* or *last* vectors, 
;;           by applying these local migration rules.
;; Parms:    sexp:    The expression to be migrated.
;; Globals:  *dic*:   The declarative expression dictionary to be transformed.
;;           *vec*:   The expression correct execution order vector.
;;           *init*:  The initial expression vector in correct execution order.
;;           *loop*:  The main loop expression vector in correct execution order.
;;           *last*:  The final expression vector in correct execution order.
;; ********************************************************************
(defun migration-rules(sexp)
   (cond 
       ((migration-rule1 sexp) true)))

;; ********************************************************************
;; name:     migration-rule1
;; 
;; summary:  Migrate the specified expression to the *init* or *last* vectors, 
;;           if it matches the following transformation patterns.
;;              (cond ((= _clock 0)  expression))
;;              (cond ((= _clock _finish)  expression))
;;              (if (= _clock 0)  expression)
;;              (if (= _clock _finish)  expression)
;; Parms:    sexp:    The expression to be migrated.
;; Globals:  *dic*:   The declarative expression dictionary to be transformed.
;;           *vec*:   The expression correct execution order vector.
;;           *init*:  The initial expression vector in correct execution order.
;;           *loop*:  The main loop expression vector in correct execution order.
;;           *last*:  The final expression vector in correct execution order.
;;           *i*:     The current index into the *vec* expression vector.
;; ********************************************************************
(defun migration-rule1(sexp)
   vars:(i tail)
   (if (isPair (setq sexp (local-rule2 sexp)))
       (cond ((and (= (length sexp) 2) 
                   (= sexp[0] cond:) 
                   (isPair (setq tail sexp[1]))
                   (isEqual tail[0] '(= _clock 0)))
               (loop for i from 1 until (length tail) do
                  (setq *init*[(length *init*)] tail[i]))
               true)
             ((and (= (length sexp) 2) 
                   (= sexp[0] cond:) 
                   (isPair (setq tail sexp[1]))
                   (isEqual tail[0] '(= _clock _finish)))  
              (loop for i from 1 until (length tail) do
                  (setq *last*[(length *last*)] tail[i]))
               true)
             ((and (= (length sexp) 3) 
                   (= sexp[0] if:) 
                   (isPair (setq tail sexp[1]))
                   (isEqual tail '(= _clock 0)))  
               (setq *init*[(length *init*)] sexp[2])
               true)
             ((and (= (length sexp) 3) 
                   (= sexp[0] if:) 
                   (isPair (setq tail sexp[1]))
                   (isEqual tail '(= _clock _finish)))  
               (setq *last*[(length *last*)] sexp[2])
               true))))

