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
;;  Title:    SmartLisp List Rules Lambda Test
;;
;;  Author:   Michael F. Korns, Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the list rule based
;;            Lambda are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_ListRule.sl")



(defun rulesDictionary()
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
      (if (and (isSymbol key) (= key[0] #\$))
          (begin
             (if (<> newValue #void)  
                 (return (setq lambdaRULES[key] newValue))) 
             ;; We are deleting a wild card lambda rule.
             (setq i (member key lambdaRULES))
             (if (isNumber i)
                 (delete lambdaRULES i))
             (return #void))) ;; end deleting lambda rule
      ;; We are asserting a production rule.
      (setq n (length leftRULES))
      (loop for i from 0 until n do
          (if (isEqual key leftRULES[i])
              (if (= newValue #void)
                  (begin
                     (delete leftRULES i)
                     (delete rightRULES i)
                     (return #void)) ;; end delete production rule
                  (begin
                     (setq rightRULES[i] newValue)
                     (return newValue))) ;; end replace production rule
              ) ;; end loop if
          ) ;; end loop
      (setq leftRULES[n] key)
      (setq rightRULES[n] newValue)
      newValue) ;; end of set1
   ;; Initialize the Lambda and clear the rules Dictionary.
   (new)) ;; end of rulesDictionary



(defun listRulesLib()
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
          verbose                  ;; Switch for verbose morph substitution
          changeCount              ;; Number of rule based substitutions.
          ;; Methods list 
          apply                    ;; Method to apply all rules in the rules dictionary to morph a list
          assert                   ;; Method to add a single new rule to the rules dictionary 
          bind                     ;; Method to bind dummy variables to values in a sub list 
          doClear                  ;; Method to clear the rules dictionary 
          isMatch                  ;; Method to match a regular expression with a candidate expression 
          len                      ;; Method to return the number of rules in the rules Dictionary 
          listRules                ;; Method for morph to call during rule application
          new                      ;; Method for initializing the newly created list rules Lambda
          ref1                     ;; Method to return the THEN form corresponding to an IF form 
          set1                     ;; Method to add a single new rule to the rules dictionary 
          setSinglePass            ;; Method to set the single pass switch 
          setVerbose               ;; Method to set the verbose switch 
          unassert                 ;; Method to delete a single rule from the rules dictionary 
          ) ;; end of persistent variables
   ;; Never initialize this Lambda more than once, because the
   ;; inline child Lambdas will overlay the cloned child Lambdas
   ;; in any clone copies of the Lambda and this causes serious
   ;; confusion when the reinitialized clone begins to affect
   ;; the persistant storage of the original Lambda.
   (if (<> new #void) (return true))
   ;; Initialize the inline child Lambdas.
   (defun apply(theList)
       vars:(x)
       (setq x theList)
       Retry::
       (setq changeCount 0)
       (setq x (morph (list x) listRules))
       (if (isPair x) (setq x (car x)))
       (if (and (> changeCount 0) (= singlePass false)(isPair x)) (goto Retry:))
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
      vars:(f i n s result (apSW false))
      ;; If the mask is a quoted pair, then set the append switch.
      (if (= (type mask) QuotedPair:)
          (begin
             (setq mask (eval mask))
             (setq apSW true)
             )) ;; end if
      ;; Bind the mask with the Dictionary values.
      (cond ;; If the mask is a List whose first element is a lambda wild
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
      ;; If the append switch is on, apply append to the result.
      (if (= apSW true) (setq result (^apply append result)))
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
                 (if (= verbose true) 
                     (writeln "[" i "] Replacing: " (string sexp true) " ==> "  (string ret true)))
                 ;(if (and (= singlePass false) (isPair ret))
                 ;    (setq ret (morph ret listRules)))
                 (return ret)
              )) ;; end if
          ) ;; end of loop
      false) ;; end of listRules
   (defun new()
      (rulesDictionary)
      (setq rulesDic (copy rulesDictionary))
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
   (defun setSinglePass(swt) (setq singlePass swt))
   (defun setVerbose(swt) (setq verbose swt))
   (defun unassert(ifForm) (setq rulesDic[ifForm] #void))
   ;; Initialize the Lambda and assign a new rules Dictionary.
   (new)) ;; end of listRulesLib


;;************************************************************************
;;  Create a new copy of the rules Lambda and create the rule database.
;;************************************************************************
(writeln   scriptName " started")
(setq gVars (new Dictionary:))
(setq ag (new listRulesLib))
;; Symbolic variable assignment and substitution rules
(ag.assert  $VAR:(lambda(s) (if (and (isSymbol s) (isCharAlphabetic s[0]) (isMember s gVars)) gVars[s])))
(ag.assert  $SYM:(lambda(s) (if (and (isSymbol s) (isCharAlphabetic s[0])) s)))
(ag.assert  $SET:(lambda(s v) (setq gVars[s] v) ok:))
(ag.assert  $SUB:(lambda(a v b) vars:(p) (setq p v) (if (<> b #void) (setq p (append (list p) b))) (if (<> a #void) (setq p (append a p))) p))
(ag.assert  '(<$S=$SYM> is $V*) '(<$SET> $S $V))
(ag.assert  '($X* <$V=$VAR> $Y*) '(<$SUB> $X $V $Y))
;; Infix to prefix notation conversion rules
(ag.assert  $IOP:(lambda(x) vars:((d #{+ addi - subi * muli / divi})) (if (isMemEqv x d) d[x])))
(ag.assert  $PFN:(lambda(a fn x y b) vars:(p) (setq p (list fn x y)) (if (<> b #void) (setq p (append (list p) b))) (if (<> a #void) (setq p (append a p))) p))
(ag.assert  '($a* $X <$FN=$IOP> $Y $b*) '(<$PFN> $a $FN $X $Y $b))
;; Constant folding rules
(ag.assert  $FOLD:(lambda(op x y) vars:(f) (setq f (getGlobalValue (symbol op))) (f x y)))
(ag.assert  $NUM:(lambda(x) (if (isNumber x) x)))
(ag.assert  $OP:(lambda(x) vars:((d #{addi addi subi subi muli muli divi divi})) (if (isMemEqv x d) d[x])))
(ag.assert  '(<$F=$OP> <$X=$NUM> <$Z=$NUM>) '(<$FOLD> $F $X $Z))
;; Algebraic expression reduction rules
(ag.assert  '(addi $X $X) '(muli $X 2))
(ag.assert  '(addi $X 0) '$X)
(ag.assert  '(addi 0 $X) '$X)
(ag.assert  '(divi $X $X) 1)
(ag.assert  '(divi 0 $X) 0)
(ag.assert  '(divi $X $X) 1)
(ag.assert  '(muli $X 0) 0)
(ag.assert  '(muli 0 $X) 0)
(ag.assert  '(muli $X 1) '$X)
(ag.assert  '(muli 1 $X) '$X)
(ag.assert  '(subi $X 0) '$X)
(ag.assert  '(subi $X $X) 0)
;; Excess parentheses reduction rules
(ag.assert  '(($X*)) '$X)

;;************************************************************************
;;  Apply the rule database single pass.
;;************************************************************************
(testit "(ag.apply '(m + n))" '(addi m n))
(testit "(ag.apply '(q / (m + n)))" '(divi q (addi m n)))
(testit "(ag.apply '(34 + 5))" '(addi 34 5))

;;************************************************************************
;;  Apply the rule database multiple pass.
;;************************************************************************
(setq ag.singlePass false)
(setq ag.verbose true)
(testit "(ag.apply '(4 - n + 3))" '(addi (subi 4 n) 3))
(testit "(ag.apply '(4 - 2 + 3))" 5)
(testit "(ag.apply '(m + 0))" 'm)
(testit "(ag.apply '(q / (m + m)))" '(divi q (muli m 2)))
(testit "(ag.apply '(q / (m + 0)))" '(divi q m))
(testit "(ag.apply '(q / (q + 0)))" 1)

;;************************************************************************
;;  Define a global symbolic variable.
;;************************************************************************
(testit "(ag.apply '(m is (x + 2)))" 'ok)
(testit "(ag.apply '(m / (x + 2)))" 1)

;;************************************************************************
;;  Terminate all this test suite
;;************************************************************************

(testEnd "Test_ListRule.sl")

















