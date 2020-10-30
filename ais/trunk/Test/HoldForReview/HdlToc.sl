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
;;  Title:    SmartSim Translator Prototype for ansi C
;;
;;  Author:   Michael F. Korns
;;
;;  Project:  SmartSim Project
;;
;;  Notes:    This proptotype translator script was developed at SUN Microsystems
;;            Corporation in September of 1994 testing concepts in rule based
;;            compilation of HDL simulations with more traditional event driven
;;            HDL simulations.
;;
;;            The results of these expreiments were rather spactacular. For two
;;            sample problems, supplied by the EDA engineering group at SUN,
;;            known as "Rtl.sl" and "Gate.sl", this rule based script achieved
;;            speed ups (over event driven simulation) of 22.4x for Rtl.sl and
;;            230x for Gate.sl.
;;
;;            The main translator procedure is translate-simulation-to-ic. It
;;            reads the specified HDL sample problems (Rtl.sl or Gate.sl) and
;;            produces an ansi integer C source file.
;;
;;  Files:    No dependencies. 
;;

(writeln "HDL to C Test Example Started")

;; ********************************************************************
;; name:     Simulator ansi C Translator Option Settings.
;; ********************************************************************
(define *migration* true)    ;; Code is migrated out of the main simulation loop.
(define *separation* true)   ;; Each target formula is separated into target assignment clauses and optimized.
(define *symanalysis* true)  ;; Symmantic analysis replaces recognizable forms to simpler forms.
(define *composition* false) ;; The main simulation loop is stepped by 2 using composition/reduction.
(define *memoization* true)  ;; Subexpressions are moved to the start of the main simulation loop.
(define *conversion* true)   ;; The main loop output is completely converted from SmartLisp to C.

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
(setq *ir*.modi 
      (lambda (x) (chain-op (cdr x) " % " "(" ")")))
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
(setq *ir*.binaryNand 
      (lambda (x) (append "!(" (chain-op (cdr x) " & " "(" ")") ")")))
(setq *ir*.binaryNand 
      (lambda (x) (append "!" x[1])))
(setq *ir*.bitwiseOr 
      (lambda (x) (chain-op (cdr x) " | " "(" ")")))
(setq *ir*.bitwiseNor 
      (lambda (x) (append "~(" (chain-op (cdr x) " | " "(" ")") ")")))
(setq *ir*.binaryNor 
      (lambda (x) (append "!(" (chain-op (cdr x) " | " "(" ")") ")")))
(setq *ir*.bitwiseXor 
      (lambda (x) (chain-op (cdr x) " ^ " "(" ")")))
(setq *ir*.bitwiseNxor 
      (lambda (x) (append "~(" (chain-op (cdr x) " ^ " "(" ")") ")")))
(setq *ir*.binaryNxor 
      (lambda (x) (append "!(" (chain-op (cdr x) " ^ " "(" ")") ")")))
(setq *ir*.bitwiseNot 
      (lambda (x) (append "~" x[1])))
(setq *ir*.binaryNot 
      (lambda (x) (append "!" x[1])))
(setq *ir*.add1 
      (lambda (x) (append "" x[1] "+1")))
(setq *ir*.sub1 
      (lambda (x) (append "" x[1] "-1")))
(setq *ir*.not 
      (lambda (x) (append "!" x[1])))
(setq *ir*.writeln 
      (lambda (x) (chain-function writeln: (cdr x) )))
(setq *ir*.excond 
      (lambda (x) 
          vars:(i n s)
          (setq n (length x))
          (setq s (append "if " x[1][0] (chain-op (cdr x[1]) " " " {" "}"))) 
          (loop for i from 2 until n do
             (if (isPair x[i])
                 (setq s (append s _eol *idnt* "else if " x[i][0] (chain-op (cdr x[i]) " " " {" "}")))
                 (setq s (append s _eol *idnt* "else {" x[i] "}"))))
          s))
(setq *ir*.cond 
      (lambda (x) 
          vars:(i n s)
          (setq n (length x))
          (setq s (append "(" x[1][0] " ? " (car (last x[1])) " : ")) 
          (loop for i from 2 until (sub1 n) do
             (if (isPair x[i])
                 (setq s (append s x[i][0] " ? " (car (last x[i])) " : ")) 
                 (setq s (append s x[i])))) 
          (setq s (append s (car (last x[i])) ")")) 
          s))
(setq *ir*.exif 
      (lambda (x) 
          vars:(rf s)
          (if (> (length x) 3)
              (setq s (append "if " x[1] " {" x[2] "} else {" x[3] "}"))
              (setq s (append "if " x[1] " {" x[2] "}")))
          s))
(setq *ir*.if 
      (lambda (x) 
          vars:(rf s)
          (if (> (length x) 3)
              (setq s (append " (" x[1] " ? " x[2] " : " x[3] ")"))
              (setq s (append " (" x[1] " ? " x[2] " : FALSE )")))
          s))
(setq *ir*.reduceAnd 
      (lambda (x) 
          vars:(n s)
          (setq n (sub1 (bitwiseShiftLeft 1 x[2]))) 
          (setq s (append " ((" x[1] " & " n ") == " n " ? 1 : 0)"))
          s))


;; ********************************************************************
;; name:     C conversion rules Dictionary for loop expressions.
;; ********************************************************************
(define *lr* (copy *ir*))
(setq *lr*.setq 
      (lambda (x) (append "" x[1] " = " x[2] ";")))
(setq *lr*.define *lr*.setq)

;; ********************************************************************
;; name:     Symantic Analysis rules Dictionary of regular expressions.
;; note:     Each Dictionary key is a regular expression with variables
;;           (xn) to be bound to variables in the candidate expression.
;;           When a match is found, the expression associated with the
;;           form is returned
;; ********************************************************************
(define *sr* (makeDictionary))
(setq *sr*['((= (- $X1 $X2) 1) 
      (setq $X3 (setq (ref $X3 0) (binaryNand (ref $X4 0) (ref $X4 0)))) 
             (setq $X3 (setq (ref $X3 1) (bitwiseXor (ref $X4 1) (ref $X4 0)))) 
             (setq $X3 (setq (ref $X3 2) (bitwiseXor (ref $X4 2) (reduceAnd $X4 2)))) 
             (setq $X3 (setq $X3 (setq (ref $X3 3) (bitwiseXor (ref $X4 3) (reduceAnd $X4 3))))))]
   '((= (- $X1 $X2) 1) (setq $X3 (modi (+ $X3 1) 16))))
(setq *sr*['(excond ((and (= (- _clock $X1) $X2) (>= $X3 $X4)) (setq $X5 $X6)) 
                    ((and (= (- _clock $X1) $X2) (< $X3 $X4)) (setq $X5 ($X7 $X8))))]
           '(excond ((= (- _clock $X1) $X2) (setq $X5 (if (>= $X3 $X4) $X6 ($X7 $X8)))))) 

;; ********************************************************************
;; name:     isMatch
;; 
;; summary:  Match the specified regular expression (reg) with a candidate
;;           expression (exp). The regular expression contains dummy
;;      variables (Xn) which are bound to values in the candidate
;;           expression using the Dictionary (dic). The act of matching
;;           an expression produces a Dictionary with the correct bindings.
;; Parms:    reg:     The regular expression with unbound variables (Xn).
;;           exp:     The natural expression to match with the regular.
;;           dic:     The Dictionary of bound variables.
;; return:   bool:    True if match, or false if no match.
;; ********************************************************************
(defun isMatch(reg exp dic)
   vars:(i n s)
   (cond ((and (isPair reg) (isPair exp) (<> (length reg) (length exp))) false) 
         ((and (isPair reg) (isPair exp)) (and (isMatch (car reg) (car exp) dic) (isMatch (cdr reg) (cdr exp) dic)))
         ((and (isSymbol reg) (= (left reg 1) "$"))
           (cond ((= dic[reg] #void) (setq dic[reg] exp) true)
                 ((isEqual dic[reg] exp) true)
                 (else false)))
         ((= reg exp) true)
         (else false)))

;; ********************************************************************
;; name:     bind
;; 
;; summary:  Bind the specified mask expression (mask) with a Dictionary
;;           (dic) containing dummy variables (Xn) which are bound to values.
;; Parms:    mask:    The mask expression with dummy variables.
;;           dic:     The Dictionary of bound variables.
;; return:   mask:    The new expression with bound dummy variables.
;; ********************************************************************
(defun bind(mask dic)
   vars:(i n s)
   (cond ((isPair mask)
                (setq n (length mask))
                (loop for i from 0 until n do (setq mask[i] (bind mask[i] dic)))
                mask) 
         ((and (isSymbol mask) (<> (setq s dic[mask]) #void)) s)
         (else mask)))

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
   (if (= (setq n (length x)) 0)
       x
       (begin
          (setq s (append lf x[0])) 
          (loop for i from 1 until n do
             (setq s (append s op x[i]))) 
          (setq s (append s rg)))))

;; ********************************************************************
;; name:     pop-list-string
;; 
;; summary:  Return a list of one string as the inside string.
;; Parms:    x:       The list of one string to be poped.
;; ********************************************************************
(defun pop-list-string(x)
   (if (and (isPair x) (= (length x) 1) (isString x[0])) 
       x[0] 
       x))

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
;;           *memo*:  The memoization expression Dictionary.
;;           *vec*:   The expression correct execution order vector.
;;           *init*:  The initial expression vector in correct execution order.
;;           *loop*:  The main loop expression vector in correct execution order.
;;           *last*:  The final expression vector in correct execution order.
;;           *ir*:    The C conversion rules Dictionary for initial expressions.
;;           *lr*:    The C conversion rules Dictionary for loop expressions.
;;           *sr*:    The symmantic analysis rules Dictionary of regular expressions.
;;           *idnt*:  The indentation for the current C source output lines.
;; ********************************************************************
(defun translate-simulation-to-ic(fileIN ...)
   vars:(i k m n proc sout sproc srec row col 
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
   (setq *memo* (makeDictionary))
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
                (setq sproc (decompile proc))
                (setq sout (append "(setq " _constraints[m 0] 
                                   " " sproc ")" _eol))
                (setq name (makeSymbol (append "w_" (mid _constraints[m 0] 1 200))))
                (setq sout (substitute (substitute sout "&" "s_") "%" "w_"))
                (setq sproc (substitute (substitute sproc "&" "s_") "%" "w_"))
                (setq *vec*[(length *vec*)] name)
                (setq *dic*[name] (morph (lisp sout)))
                (setq *exp*[name] (morph (lisp sproc))))))
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
                (setq sproc (decompile proc))
                (setq sout (append "(setq " _states[m 0] 
                                   " " sproc ")" _eol))
                (setq name (makeSymbol (append "s_" (mid _states[m 0] 1 200))))
                (setq sout (substitute (substitute sout "&" "s_") "%" "w_"))
                (setq sproc (substitute (substitute sproc "&" "s_") "%" "w_"))
                (setq *vec*[(length *vec*)] name)
                (setq *dic*[name] (morph (lisp sout)))
                (setq *exp*[name] (morph (lisp sproc))))))
   ;; Perform all optimizations and transformations on the expression dictionary.
   (apply-rules)
   ;; Write out the memo temporary variable declarations.
   (loop for i from 0 until (length *memo*) do
        (setq sout (append "INT           " *memo*[i 1] ";"))
        (fwriteln fileID sout))
   ;; Write out the optimized initial formulas in the correct execution order.
   (loop for i from 0 until (length *init*) do
        (fwriteln fileID *init*[i]))
   ;; Write out the ansi C file argument verification code.
   (fwriteln fileID _eol "if ((argc != 1) || (argv[0].Tag != TYINT)) return(TERROR(\"!arglist!\"));")
   (if *composition*
       (fwriteln fileID "_finish = (argv[0].u.Int | 1);" _eol)
       (fwriteln fileID "_finish = argv[0].u.Int;" _eol))
   ;; Write out the optimized formulas in the correct execution order.
   (if *composition*
       (fwriteln fileID "for (_clock = 0;_clock < _finish; _clock += 2)" _eol *idnt* "{")
       (fwriteln fileID "for (_clock = 0;_clock <= _finish; _clock += 1)" _eol *idnt* "{"))
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
;;           *memo*:  The memoization expression dictionary.
;;           *vec*:   The expression correct execution order vector.
;;           *init*:  The initial expression vector in correct execution order.
;;           *loop*:  The main loop expression vector in correct execution order.
;;           *last*:  The final expression vector in correct execution order.
;;           *i*:     The current index into the *vec* expression vector.
;; ********************************************************************
(defun apply-rules()
   vars:(i)
   ;; Apply local migration rules to the expression dictionary.
   MigrationRules::
   (if (not *migration*) (goto LocalRules:))
   (loop for i from 0 until (length *vec*) do
       (setq *i* i)
       (if (and (<> *vec*[i] #void) (migration-rules *dic*[*vec*[i]][0]))
           (setq *vec*[i] #void)))
   ;; Apply local transformation rules to the expression dictionary.
   LocalRules::
   (if (not *separation*) (goto SymanticRules:))
   (loop for i from 0 until (length *vec*) do
       (setq *i* i)
       (if (<> *vec*[i] #void)
           (setq *dic*[*vec*[i]] (morph *dic*[*vec*[i]] local-rules))))
   ;; Apply symmantic analysis rules to the expression dictionary.
   SymanticRules::
   (if (not *symanalysis*) (goto SubstitutionRules:))
   (loop for i from 0 until (length *vec*) do
       (setq *i* i)
       (if (<> *vec*[i] #void)
           (setq *dic*[*vec*[i]] (morph *dic*[*vec*[i]] symantic-rules))))
   ;; Apply expression substitution rules to the expression dictionary.
   SubstitutionRules::
   (if (not *composition*) (goto MemoRules:))
   (loop for i from 0 until (length *vec*) do
       (setq *i* i)
       (if (<> *vec*[i] #void)
           (setq *dic*[*vec*[i]] (substitution-rules *dic*[*vec*[i]]))))
   ;; Apply sub expression memoization rules to the expression dictionary.
   MemoRules::
   (if (not *memoization*) (goto MoveToLoop:))
   (loop for i from 0 until (length *vec*) do
       (setq *i* i)
       (if (<> *vec*[i] #void)
           (setq *dic*[*vec*[i]] (morph *dic*[*vec*[i]] memo-rules))))
   ;; Move the expression dictionary formulas to the *loop* vector.
   MoveToLoop::
   (loop for i from 0 until (length *vec*) do
       (if (<> *vec*[i] #void)
           (setq *loop*[(length *loop*)] *dic*[*vec*[i]][0])))
   (if (not *conversion*) (goto Last:))
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
   Last::
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
       (*ir*[sexp[0]] sexp)
       (pop-list-string sexp)))

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
       (*lr*[sexp[0]] sexp)
       (pop-list-string sexp)))

;; ********************************************************************
;; name:     symantic-rules
;; 
;; summary:  If the head of the specified sub list is in the *sr* rules
;;           Dictionary, then apply the C conversion rule to the sub list 
;;           passed by the morph procedure.
;; Parms:    sexp:    The sub list passed by morph to be converted into C.
;; Globals:  *sr*:    The symmantic analysis rules expressions.
;; ********************************************************************
(defun symantic-rules(sexp)
   vars:(i n m dic cpy ret)
   (setq ret false)
   (setq m (length *sr*))
   (loop for i from 0 until m do
       (setq dic (makeDictionary))
       (if (isMatch *sr*[i 0] sexp dic)
           (begin 
              (setq cpy (copy *sr*[i]))
              (setq ret (bind cpy dic)))))
   last:: ret)

;; ********************************************************************
;; name:     memo-rules
;; 
;; summary:  Memoize the common sub expressions which are found often,
;;           and compute them into temporary variables at the beginning
;;      of the main simulation loop (*loop*).
;; Parms:    sexp:    The expression the be transformed by memoization.
;; Globals:  *dic*:   The declarative expression dictionary to be transformed.
;;           *exp*:   The substitution expression dictionary.
;;           *vec*:   The expression correct execution order vector.
;;           *i*:     The expression correct execution order vector index.
;;           *init*:  The initial expression vector in correct execution order.
;;           *loop*:  The main loop expression vector in correct execution order.
;;           *last*:  The final expression vector in correct execution order.
;; ********************************************************************
(defun memo-rules(sexp)
   vars:(x i n name stateSW)
   pvars:((id 0))
   (setq n (length sexp))
   (setq stateSW (= "s_" (mid *vec*[*i*] 0 2)))
   (if (or (= sexp[0] <:) (= sexp[0] <=:) (= sexp[0] =:) (= sexp[0] <>:) (= sexp[0] >:) (= sexp[0] >=:))
       (loop for i from 1 until n do
          (if (and (isPair sexp[i]) (memo-check sexp[i] stateSW))
              (begin
                 (if (= (setq name *memo*[sexp[i]]) #void)
       (begin
                        (setq *memo*[sexp[i]] (setq name (makeSymbol (append "__tmp" (setq id (add1 id))))))
                        (setq *loop*[(length *loop*)] (list 'setq name sexp[i]))))
   (setq sexp[i] name)))))
   sexp)

;; ********************************************************************
;; name:     memo-check
;; 
;; summary:  Return true if we can memoize the common sub expressions which are found often.
;;           Care must be taken when allowing memoization. We cannot memoize 
;;      expressions if they may depend upon variables which are not yet set.
;; Parms:    sexp:    The expression the be checked for memoization.
;;      StateSW: True if we are memoizing a state variable.
;; Globals:  *dic*:   The declarative expression dictionary to be transformed.
;;           *exp*:   The substitution expression dictionary.
;;           *vec*:   The expression correct execution order vector.
;;           *i*:     The expression correct execution order vector index.
;;           *init*:  The initial expression vector in correct execution order.
;;           *loop*:  The main loop expression vector in correct execution order.
;;           *last*:  The final expression vector in correct execution order.
;; ********************************************************************
(defun memo-check(sexp stateSW)
   vars:(x i n name (answer true))
   pvars:((id 0))
   (setq n (length sexp))
   (loop for i from 0 until n do
      (setq x sexp[i])
      (setq answer
           (cond ((isPair x) (memo-check x stateSW))
                 ((and (isSymbol x) stateSW) (or (= x *vec*[*i*]) (and (<> "s_" (mid x 0 2)) (<> "w_" (mid x 0 2)))))
                 ((isSymbol x) (or (= x *vec*[*i*]) (<> "w_" (mid x 0 2))))
   (else true)))
      (if (not answer) (goto Last:)))
   Last::   
   answer)

;; ********************************************************************
;; name:     substitution-rules
;; 
;; summary:  Substitute the declarative expression, for each variable 
;;           for the variable itself. Also substitute the expressssion
;;           (+ _clock 1) for the variable _clock.
;; Parms:    sexp:    The expression the be transformed by substitution.
;; Globals:  *dic*:   The declarative expression dictionary to be transformed.
;;           *exp*:   The substitution expression dictionary.
;;           *vec*:   The expression correct execution order vector.
;;           *init*:  The initial expression vector in correct execution order.
;;           *loop*:  The main loop expression vector in correct execution order.
;;           *last*:  The final expression vector in correct execution order.
;; ********************************************************************
(defun substitution-rules(sexp)
   vars:(x i n)
   (setq n (length sexp))
   (cond 
       ((= sexp _clock:) '(+ _clock 1))
       ((and (isSymbol sexp) (<> *exp*[sexp] #void)) (copy *exp*[sexp]))
       ((and (isPair sexp) (> n 2) (= sexp[0] 'setq))
                     (loop for i from 2 until n do
                         (setq sexp[i] (substitution-rules sexp[i])))
                     sexp)
       ((and (isPair sexp) (= n 2) (= sexp[0] (makeSymbol "else"))) (substitution-rules sexp[1]))
       ((isPair sexp) (loop for i from 0 until n do
                         (setq sexp[i] (substitution-rules sexp[i])))
                     sexp)
       (else sexp)))

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
       ((<> (setq x (local-rule2 sexp)) false) x)
       ((<> (setq x (local-rule3 sexp)) false) x)
       ((<> (setq x (local-rule4 sexp)) false) x)))

;; ********************************************************************
;; name:     local-rule1
;; 
;; summary:  Transform the specified sub list, passed by morph, 
;;           if it matches the following transformation pattern.
;;             (setq name (cond (test result) (test result) ...))
;;           and return the following morphed pattern.
;;             (excond (test (setq name result)) (test (setq name result)) ...)
;; Parms:    sexp:    The sub list passed by morph to be transformed.
;; ********************************************************************
(defun local-rule1(sexp)
   vars:(i n name sub tail head)
   (if (and (= (length sexp) 3) 
            (= sexp[0] setq:) 
            (isSymbol (setq name sexp[1])) 
            (<> (mid name 2 7) "initial") 
            (<> (mid name 2 7) "monitor") 
            (isPair (setq head sexp[2]))
            (= head[0] cond:))
       (begin
          (setq head[0] excond:)
          (loop for i from 1 until (length sexp[2]) do
             (setq sub sexp[2][i])
             (setq n (sub1 (length sub)))
             (if (and (= sub[0] "else") (= sub[1] name))
                 (setCdr head #void)
                 (setq sub[n] (list setq: name sub[n])))
             (setq head (cdr head)))
   (morph sexp[2] local-rules))
       false))

;; ********************************************************************
;; name:     local-rule2
;; 
;; summary:  Transform the specified sub list, passed by morph, 
;;           if it matches the following transformation pattern.
;;             (setq name (if test true-exp false-exp))
;;           and return the following morphed pattern.
;;             (exif test (setq name true-exp) (setq name false-exp))
;; Parms:    sexp:    The sub list passed by morph to be transformed.
;; ********************************************************************
(defun local-rule2(sexp)
   vars:(i n name sub tail head test)
   (if (and (= (length sexp) 3) 
            (= sexp[0] setq:) 
            (isSymbol (setq name sexp[1])) 
            (<> (mid name 2 7) "initial") 
            (<> (mid name 2 7) "monitor") 
            (isPair (setq head sexp[2]))
            (= head[0] if:))
       (begin
          (setq head[0] exif:)
          (setq test sexp[2][1])
          (loop for i from 2 until (length sexp[2]) do
             (setq sexp[2][i] (list setq: name sexp[2][i])))
   (morph sexp[2] local-rules))
       false))

;; ********************************************************************
;; name:     local-rule3
;; 
;; summary:  Transform the specified sub list, passed by morph, 
;;           if it matches the following transformation patterns.
;;              (setq __monitorNN  expression)
;;              (setq __initialNN  expression)
;;           and return the following morphed pattern.
;;              expression
;; Parms:    sexp:    The sub list passed by morph to be transformed.
;; ********************************************************************
(defun local-rule3(sexp)
   vars:(i n name sub tail head)
   (if (and (= (length sexp) 3) 
            (= sexp[0] setq:) 
            (isSymbol (setq name sexp[1]))
            (or  
               (= (mid name 2 7) "monitor")
               (= (mid name 2 7) "initial")))
       sexp[2]))

;; ********************************************************************
;; name:     local-rule4
;; 
;; summary:  Transform the specified sub list, passed by morph, 
;;           if it matches the following transformation patterns.
;;              (+ 0 expression)
;;              (+ x expression)
;;           and return the following morphed pattern.
;;              expression
;; Parms:    sexp:    The sub list passed by morph to be transformed.
;; ********************************************************************
(defun local-rule4(sexp)
   vars:(i n name sub tail head)
   (setq n (length sexp))
   (cond ((and (= n 3) (= sexp[0] +:) (= sexp[1] 0)) sexp[2])
         ((and (= n 3) (= sexp[0] +:) (= sexp[2] 0)) sexp[1])
         ((and (= n 3) (= sexp[0] -:) (= sexp[2] 0)) sexp[1])
         ((and (= n 3) (= sexp[0] *:) (= sexp[1] 1)) sexp[2])
         ((and (= n 3) (= sexp[0] *:) (= sexp[2] 1)) sexp[1])
         ((and (= n 3) (= sexp[0] /:) (= sexp[2] 1)) sexp[1])))

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
   (if (isPair (setq sexp (local-rule3 sexp)))
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

;; *******************************************************************
;; name:     readTextFile
;; 
;; summary:  Perform a complete read of the specified text file.
;; Parms:    This procedure accepts one argument.
;;           name:      The name of the file to open.
;; Return:   The byte vector containing the complete text file contents.
;; *******************************************************************
(define (readTextFile name) 
   vars:(fileID self (type 0))
   (setq fileID (fileOpen name 0 type))
   (setq self (fileRead fileID))
   (fileClose fileID 1)
   self)
   
;; *******************************************************************
;; name:     writeTextFile
;; 
;; summary:  Perform a complete write of the specified text file.
;; Parms:    This procedure accepts two arguments.
;;           name:      The name of the file to create.
;;           self:      The object to be saved in the file.
;; *******************************************************************
(define (writeTextFile name self) 
   vars:(fileID (type 0))
   (setq fileID (fileOpen name 1 type))
   (fileWrite fileID self)
   (fileClose fileID 1)
   self)


;; *******************************************************************
;; note:  ...start the host independent test suite...
;; *******************************************************************

(tic "Rtl.sl" "rtl.c")
(if (compareNE (readTextFile "rtl.c") (readTextFile "rtl.cpy"))
    (error "hdltoc" "*FAILURE* for HDL to C Test Example on Rtl.sl")) 

(tic "Gate.sl" "gate.c")
(if (compareNE (readTextFile "gate.c") (readTextFile "gate.cpy"))
    (error "hdltoc" "*FAILURE* for HDL to C Test Example on Gate.sl")) 

(writeln "HDL to C Test Example Completed")
