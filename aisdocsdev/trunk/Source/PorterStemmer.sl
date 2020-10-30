;;*************************************
;;*************************************
;; Exported Lambda File Cabinet Document
;;*************************************
;;*************************************

;;**EXPORTKEY**:porterStemmer
(defun porterStemmer(inputText output)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;   Summary:  The porterStemmer is a Lisp Lambda designed to
;;           to remove suffixes from a list of words.  Such words may
;;           contain alphanumberic characters, special characters but
;;           do not contain spaces.
;;           
;;           The porterStemmer Function implements the stemming algorithm
;;           as described below through straightforward iteration of the
;;           input string containing the words to be stemmed.
;;
;;           In this implementation:          
;;           (1) All the input characters are converted into lowercase, thus 
;;               the output string is all in lowercase.
;;           (2) Each word is recognized by the presences of spaces between them.
;;           (2) Punctuation marks at the end of the words are removed.  
;;         
;;   Input:  inputText       String to be stemmed
;;   Return: stemmedText     A Vector containing the stemmed strings                                      
;;
;;   Options: (setq porterStemmer.htmlTagsOn TRUE/FALSE)
;;                           Default value: FALSE
;;                           If set to TRUE, the html tags are stemmed.                                                  
;;                           If set to FALSE, only contents between html tags are stemmed.
;;
;;                           Example: <Name> Test </Name>
;;                           If set to FALSE: Output: #("test" )
;;                           If set to TRUE:  Output: #("<Name>" "test" "</Name>" )
;;
;;             (setq porterStemmer.traceLinesOn true/false)
;;                           Default value: FALSE
;;                           If set to true, the tracelines are displayed.  
;;                           This is for debugging purposes
;;                           If set to false, the tracelines are not displayed.
;; 
;;    Overview of the Porter Stemming Algorithm  
;;   
;;    ========================
;;    Character Representation  
;;    ========================
;;
;;    A list of consonants of length greater than 0 will be denoted by C, and a 
;;    list of vowels of length greater than 0 will be denoted by V. 
;;    Therefore, words are represented by the form:
;;
;;    [C]VCVC ... [V]
;;
;;    where the square brackets denote arbitrary presence of their contents.
;;
;;    ==================
;;    Measure of a Word
;;    ==================
;;   
;;    The child Lambda sequence gets the measure of a word. The measure of the word
;;    is the number of the C-V pair in the word counted after the first occurence of V.
;;
;;    Example:
;;       Input String:  plaster
;;       Character Representation: C(pl) V(a) C(st) V(e) C(r)
;;       Measure: 2     V(a) - C(st)  -> first measure
;;                      V(e) - C(r)   -> second measure
;;
;;    ======
;;    Rules
;;    ======
;;
;;    The rules for removing a suffix will be given in the form
;;
;;    (condition) S1 -> S2
;;
;;    This means that if a word ends with the suffix S1, and the stem before S1
;;    satisfies the given condition, S1 is replaced by S2. The condition is
;;    usually given in terms of m, where m is the measure of the word.
;;
;;    Example:
;;         Input String: allowance
;;         Rule:  (m > 1) ANCE ->
;;         Measure of "allow" which is the stem before ANCE: 	2
;;         Output after removal of the suffix ANCE:	allow
;;                       
;;    The `condition' part may also contain the following:
;;
;;    *S  - the stem ends with S (and similarly for the other letters).
;;    *v* - the stem contains a vowel.
;;    *d  - the stem ends with a double consonant (e.g. -TT, -SS).
;;    *o  - the stem ends cvc, where the second c is not W, X or Y (e.g.
;;       -WIL, -HOP).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

vars: (ch cc i (ByteVector:result) (Vector:stemmedVec)  stemmedString)
 
regs:( (CharPointer:start) (CharPointer:startp) (CharPointer:mp) (CharPointer:resultp) (nill 0) (space 32) 
       (period 46)  (excpoint 33)  (question 63) (colon 58) (semicolon 59) (CharPointer:wp)
       cc (Integer:n 0) (Integer:i 0) (_a #\a) (_z #\z))
pvars: (step1ab 
        step1c 
        step2 
        step3 
        step4 
        step5
        ends
        sequence
        vowelinstem
        setto
        doublec
        cvc
        getStem
        (ByteVector:word)         ;; Current word evaluated
        (ByteVector:alpha)        ;; Vector of character representations
                                  ;; 0 for vowels and 1 for consonants  
        (Boolean:traceLinesOn false) 
        (Boolean:htmlTagsOn false)  
		modifiedHTML
        (myDontStemList #{dir|| "ais" 1 "argument" 1 "arithmetic" 1 "array" 1 "dictionary" 1 "investbyLambda" 1 "key" 1 "square" 1 "Lambda" 1
								"browseLib" 1 "recursion" 1 "query" 1
	       }) ;; myDontStemList	
        )

(setq start inputText)
(setq alpha (new Vector: Byte: 256))
(setq alpha "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001110111011111011111011111")
(setq stemmedVec (new Vector:))

(setq lengthInput (length inputText))
(setq modifiedHTML (resize (new Vector: Byte: 10) lengthInput))
(setq stemmedString (resize (new Vector: Byte: 5000) lengthInput))
(setq startp stemmedString)
(setq mp modifiedHTML)
(setq wordLen 0)
 
Fetch::
;(setq startp stemmedString) 
(setq word (resize (new String: "") 100))
;(setq word (new Vector: Byte: 100))
(setq wp word)


(setq cc start[0])

;; Promote past spaces/ garbage characters  
(while (and (<> cc 0) (<= cc 32)) (begin (++ start) (setq cc start[0])))
(if (= cc 0)   (goto End:))

;; All the characters must be in lowercase
(if (= traceLinesOn true) (writeln "porterStemmer: Fetch: cc before downcase " cc))
;(setq cc (code (downcase (setq ch (char cc)))))
(if (= traceLinesOn true) (writeln "porterStemmer: Fetch: cc: " cc))


;(setq start[0] cc)

;; Get the individual words to stem
;; Note: In this implementation we do not recognize word with numeric characters
(setq i 0)
(while (and (> (setq cc start[0]) space) (<> cc nill) (= (isNegative cc) false))
     (if (= traceLinesOn true)(writeln "porterStemmer: in while: cc " cc))
 	(if (and (< cc 91) (> cc 64)) (begin  (setq cc (+ cc 32))))
     ;; Evaluate HTML Tags
     (if  (and (isCharWhitespace word) (= cc 60))(goto HtmlTag:) )
     (if  (and (= false (isCharWhitespace word)) (= cc 60))(goto NextWord:) )
     (setq wp[0] cc)
     (setq wp[1] 0)
	(++ wp)
     ;; Promote the pointer
     ;;NextChar::
     (++ start)
) ;; end while
 

(if (= traceLinesOn true) (writeln "porterStemmer: normal word: " word))
(goto NextWord:)

;; Get contents of HTML tag
HtmlTag::
(if (= traceLinesOn true) (writeln "porterStemmer: HtmlTag "))
(setq i 0)
(setq cc start[0])
(while (and (<> (setq cc start[0]) nill) (<> cc 62))
   (setq wp[0] cc)
   (setq wp[1] 0)
	;; Setting contents for modifiedHTML
	(setq mp[0] cc)
	(setq mp[1] 0)
	(++ mp)
 	(++ wp)
   (++ start)
) ;; end while
(if (= cc 62) 
    (begin 
        (setq wp[0] cc)
        (setq wp[1] 0)
		;; Setting contents for modifiedHTML
		(setq mp[0] cc)
		(setq mp[1] 0)
		(++ mp)
		(++ wp)
        (++ start)
     ) ;; end begin
) ;; end if
(setq cc start[0])
(if (= traceLinesOn true)(writeln "porterStemmer: htmlContents: " word))
 
(if (= traceLinesOn true)(writeln "porterStemmer:start: " cc))
(if (< (setq cc start[0] ) 0) (begin  (while (and (isNegative cc)(<> cc 0)) do  (++ start) (setq cc start[0])  )))
(if (= htmlTagsOn false)
    (begin
       (if (> (setq cc start[0]) 0) 
           (begin (goto Fetch:))
           else
           (begin (goto End:))
       ) ;; end if
     ) ;; end begin
     else
    (begin
		;(resize result (length word))
        (setq result word)
        (goto SetResult:)
    ) ;; end begin
) ;; end if for htmlTagsOn

 
  
NextWord::
(if (= traceLinesOn true)(writeln "porterStemmer: NextWord: " word))
 
;; Promote past whitespace
(vmregRunInHardware start:)
(setq cc start[0])
(if (< (setq cc start[0] ) 0) (begin  (while (and (isNegative cc)(<> cc 0)) do  (++ start) (setq cc start[0])  )))


(while (and (<> cc nill) (<= cc space))  (begin (++ start) (setq cc start[0])))
(vmregRunInHardware stop:)

Stem::
(if (isCharWhitespace word) (goto Fetch:))
 
(if (isMember word myDontStemList) (begin (setq result word) (goto SetResult:)))
;(writeln "word: " word)
(if (= (isCharAlphabetic word) false) (begin (setq result word) (goto SetResult:)))
;(resize result (length word))
(setq result (step1ab word))
(setq result (step1c result))
(setq result (step2 result))
(setq result (step3 result))
(setq result (step4 result))
(setq result (step5 result))
 
SetResult::
(if (= output 0)
	(begin 
		(if (= traceLinesOn true)(writeln "porterStemmer: SetResult: result: " result))
		(setq stemmedVec n result)
		(++ n)
	) ;; end begin
) ;; end if


(if (= traceLinesOn true)(writeln "porterStemmer: SetResult: result: " result))	 
(setq m 0)
(++ n)
(setq resultp result)
(while (<> (setq cc resultp[0]) 0) do	 
	(setq mp[0] cc)		 
	(setq mp[1] 0)	
	(++ mp)	 		 
	(++ resultp)
) ;; end while
(setq mp[0] 32)
(setq mp[1] 0)
(++ mp)
 
(if (isExact (/ n 20)) (begin	(setq mp[0] 10)  (setq mp[1] 0)(++ mp) ))





(if (= output 1)
	(begin  
		(setq m 0)
		(++ n)
		(setq resultp result)
		(while (<> (setq cc resultp[0]) 0) do	 
			(setq startp[0] cc)		 
			(setq startp[1] 0)	
			(++ startp)	 		 
			(++ resultp)
		) ;; end while
		(setq startp[0] 32)
		(setq startp[1] 0)
		(++ startp)
 
		(if (isExact (/ n 20)) (begin	(setq startp[0] 10)  (setq startp[1] 0)(++ startp) ))
	 
	) ;; end begin

);; end if

;(if (= traceLinesOn true)(writeln "porterStemmer: SetResult: stemmedText: " stemmedText))
(if (= traceLinesOn true)(writeln "porterStemmer: afterStem:start[0]: " start[0]))

;(writeln "after: " stemmedString)
(if (< (setq cc start[0] ) 0) (begin  (writeln "if negative: " )(while (and (isNegative cc)(<> cc 0)) do (writeln "in negative: " ) (++ start) (setq cc start[0]) (writeln "cc negative : "  cc))))
(if (> (setq cc start[0]) 0) (goto Fetch:))


End::
 
(if (= output 0)
	(begin
		(return stemmedVec)
	) ;; end begin

) ;; end if

(if (= output 1)
	(begin
	;(setq startp[(++ wordLen)] 0)
	 
		(return stemmedString)
	) ;; end begin
) ;; end if

 
) ;; end porterStemmer





;;**EXPORTKEY**:porterStemmer:cvc
(defchild porterStemmer:cvc(last)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Summary: This Lambda returns true on the 
;;           following conditions:
;;           (1) if the characters before and after the
;;           current character evaluated are  consonants and 
;;           that the current character is a vowel.
;;           Therefore, if last is the current character:
;;                   last - 1 is  c
;;                   last is v
;;                   last + 1 is c
;;           (2) if the second c is not w,x or y.
;; 
;;           This is used when trying  to restore an e at
;;  		 the end of a short word. e.g.
;;  		 cav(e), lov(e), hop(e), crim(e), but not
;; 			 snow, box, tray.
;;
;;  Arg:     last     CharPointer pointing to the current character
;;                    in the word evaluated.
;;  Return:  result   Boolean value.  If true, the last three characters
;;                    have the form c-v-c except for w, x and y.  If otherwise,
;;                    false is returned.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

vars: (result)
regs:( c1 c2 c3 (CharPointer:nLast))

(if (= traceLinesOn true)(writeln "cvc --start--"))

(setq result false)
(setq nLast (- last 2)) 

(setq c1 nLast[0])
(++ nLast)
(setq c2 nLast[0])
(++ nLast)
(setq c3 nLast[0])
 
(if (= traceLinesOn true)(writeln "cvc: c1: " (char c1) " - c2: " (char c2) " - c3: " (char c3)))

(if (or (= c3 #\w) (= c3 #\x) (= c3 #\y)) 
   (begin
      (if (= traceLinesOn true)(writeln "cvc: result: " result))
      (if (= traceLinesOn true)(writeln "cvc --end--"))
      (return result)
    ) ;; end begin
) ;; end if
 

(if (and (= alpha[c1] 49) (= alpha[c2] 48) (= alpha[c3] 49) ) 
    (begin        
        (setq result true)
        (if (= traceLinesOn true)(writeln "cvc: result: " result))
        (if (= traceLinesOn true)(writeln "cvc --end--") )
        (return result)
     ) ;; end begin
) ;; end if

(if (= traceLinesOn true)(writeln "cvc: result: " result))
(if (= traceLinesOn true)(writeln "cvc --end--"))

result) ;; end cvc




;;**EXPORTKEY**:porterStemmer:doublec
(defchild porterStemmer:doublec(last)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Summary: This Lambda returns  the Boolean value true
;; 			if  the character and the previous character
;; 			pointed to are identical consonants
;;
;; Arg:  last   CharPointer pointing to the evluated character in 
;;              the current word.
;;
;; Return: result  Boolean value.  If true, the current word ends in
;;                 double consonants.  If not, false is returned. 
;;
;; Example:
;;   Evaluated word: controll
;;   last: last "l" in controll
;;   result: control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

vars: (result)
regs:( c2 c1)

(if (= traceLinesOn true)(writeln "doublec --start-")) 
(setq c1 last[0])
(-- last)
(setq c2 last[0])

(if (= traceLinesOn true)(writeln "doublec:  c1 c2: " (char c1) (char c2)))

(if (compareEQ c1 c2) (setq result true) else (setq result false))


(if (= traceLinesOn true)(writeln "doublec: result: " result))
(if (= traceLinesOn true)(writeln "doublec --end-"))

result) ;; end ends





























;;**EXPORTKEY**:porterStemmer:ends
(defchild porterStemmer:ends(suffix last)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Summary:  This Lambda returns the Boolean value true
;; 			 if  the suffix passed by the calling
;; 			 Lambda matches the suffix of the current 
;; 			 word evaluated
;;
;; Args: suffix   String representing the suffix to be
;;                compared with the characters in the orginal word.
;;       last     CharPointer pointing to the character
;;                in the original word where the suffix comparison
;;                will start.
;;
;; Return: result  Boolean value.  If true, the suffix passed matches the
;;                 spacified characters in the original word. If false,
;;                 the orginal word does not end with the input suffix.
;;
;; Example:
;;    Evaluated word: motoring
;;    Suffix: ING
;;    last:   points to "i" in motoring
;;    result: TRUE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

vars: (result (ByteVector:test))
regs:( (CharPointer:sLast) cc  N n m)

 
 
(setq cc last[0])
(if (= traceLinesOn true)(writeln "ends --begin--: " ))
(if (= traceLinesOn true)(writeln "ends: suffix: " suffix ))
(setq N (- (length suffix) 1))
(setq sLast (- last N))
(++ N)
;;(writeln "end N is : " N)

 
(loop for n from 0 until N do
    (setq cc sLast[n])     
    (if (= (code suffix[n]) cc)
        (begin
           ;;(writeln "here in = letter")
           (setq result true)
        ) ;; end begin
        else
        (begin
           (setq result false)
           (if (= traceLinesOn true)(writeln "ends: result: " result))
           (if (= traceLinesOn true)(writeln "ends --end--: " ))
           (return result)
        ) ;; end begin         
     ) ;; end if
     ;;(writeln "after if: " )
) ;; end loop

(if (= traceLinesOn true)(writeln "ends: result: " result))
(if (= traceLinesOn true)(writeln "ends --end--: " ))

result) ;; end ends









;;**EXPORTKEY**:porterStemmer:getStem 
(defchild porterStemmer:getStem(first suffix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Summary: This Lambda gets the pointer to the
;; 		first letter of the word evaluated and the
;; 		suffix represented as a string then returns
;;      the stem or the root word.
;;
;; Args:    first       CharPointer pointing to the 
;;                      first character of the word to be stemmed.
;;          suffix      String representing the suffix evaluated 
;;                      in the current rule.
;;
;; Return:  root        String representing the stem or the root word 
;;                      which is the original word minus the suffix.
;;
;; Example: 
;;     Original Word:     happiness
;;     Suffix:            NESS
;;     output of getStem: happi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

vars: (result (ByteVector:root))
regs:( (CharPointer:first) (CharPointer:last)cc   wordLength lengthSuffix p n N m )

(if (= traceLinesOn true)(writeln "getStem --begin--"))
(if (= traceLinesOn true)(writeln "getStem suffix: " suffix))
(if (= traceLinesOn true)(writeln "getStem word: " word))
(setq m 0)
(setq root  (new Vector: Byte: 250))
(setq lengthSuffix (length suffix))
(setq last word)

;; Set the last pointer and set the counter for the number of
;; characters in the evaluated word
(vmregRunInHardware start:)
(while (<> (setq cc last[0]) 0) do
    (++ last)
    (++ wordLength)
) ;; end while
(vmregRunInHardware stop:)
 
(if (= traceLinesOn true)(writeln "getStem wordLength: " wordLength))

(setq p 0)
(setq cc first[0])

SetFirst::
(setq n 0) 
(setq root p cc)
(setq root (++ p) 0)
(if (= traceLinesOn true)(writeln "getStem: setFirst: cc: " (char cc)))
(if (= traceLinesOn true)(writeln "getStem: setFirst: root: " root))
(++ first)
(setq cc first[0])
(setq ch (char cc))
(setq m 0)


(if (= traceLinesOn true) (writeln "getStem:  length root: " p))
(loop for n from 0 until lengthSuffix
    (if (= traceLinesOn true)(writeln "getStem: loop: suffix[n]: " suffix[n]))
    (if (= traceLinesOn true)(writeln "getStem: loop: first[m]: " first[m]))    
    (if (and (>= p (- wordLength lengthSuffix))(compareEQ suffix[n] (char first[m]))  )
        (begin
           ;;(if (compareEQ (isCharWhitespace root) false) (goto SetChar:))
          (if (= traceLinesOn true)(writeln "getStem: in loop: m: " m))
           (++ m)
          ;; (++ n)
         ) ;; end begin
         else
         (begin
             (if (= traceLinesOn true)(writeln "getStem: in loop: cc: " cc))
            (if (<> cc 0)(begin (goto SetFirst:)) else (begin (goto End:)))
         ) ;; end begin
     ) ;; end if          
) ;; end loop

End:: 
;;(setq test1 (-- m) 0)
;;(if (< k 2) (setq test1 word))
(if (= traceLinesOn true)(writeln "getStem: End: root: " root))
(if (= traceLinesOn true)(writeln "getStem --end--"))
 
root) ;; end ends










;;**EXPORTKEY**:porterStemmer:sequence
(defchild porterStemmer:sequence(test1 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Summary: The number of v-c between the first occurence of
;; 		 a vowel in the word and the last character of the word is the 
;; 		 measure of the word.
;;
;;      CV       gives 0
;;      CVCV     gives 1
;;      CVCVCV   gives 2
;;      CVCVCVC  gives 3
;;      ....
;;
;;  Input:   test1    The string to be measured.
;;  Return:  n        An integer representing the measure of the word.
;;
;;  Example:
;;       Input String:  plaster
;;       Character Representation: C(pl) V(a) C(st) V(e) C(r)
;;       Measure: 2     V(a) - C(st)  -> first measure
;;                      V(e) - C(r)   -> second measure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

vars: (n)
regs: ((CharPointer:start) cc i (Integer:n 0) )
  

(if (= traceLinesOn true)(writeln "sequence --start--"))
(if (= traceLinesOn true)(writeln "sequnce: evaluated root: " test1))
(setq start test1)
(setq cc start[0])
(if (= traceLinesOn true)(writeln "sequence: after setting start: " (char cc)))

(setq n 0)
;;(setq alpha (new Vector: Byte: 256))
;;(setq alpha "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001110111011111011111011111")
 
;; look for the first occurence of a vowel in the word
 (vmregRunInHardware start:)
 (setq cc start[0])                      
 (while (compareEQ alpha[cc] 49) (begin (++ start) (setq cc start[0]) ))
 (vmregRunInHardware stop:) 

 (if (= traceLinesOn true)(writeln "sequence: first vowel:  " (char cc)))
 (while (<> (setq cc start[0]) 0) do
       Next1::
      (if (= traceLinesOn true)(writeln "sequence: Next1: cc: " (char cc) ))
      (if (= traceLinesOn true)(writeln "sequence: Next1: alpha[cc]: " (char alpha[cc]) ))
      ;; Check if the next letter is a consonant
      (while (<> (setq cc start[0]) 0) do
          (if (= alpha[cc] 49)
              (begin
                (++ n)                  
                  (if (= traceLinesOn true)(writeln "sequence: Next1: consonant"))
                ;;(++ start)
                  (goto Next2:)
              ) ;; end begin
           ) ;;end if
           (++ start)
    ) ;; end while
     
    Next2::
    (if (= traceLinesOn true)(writeln "sequence: Next2: cc: " (char cc) ))
     ;; Check if  the next letter is a vowel
     
    ;;(++ n)
    (while (<> (setq cc start[0]) 0) do       
        (if (= alpha[cc] 48)
            (begin
               (if (= traceLinesOn true)(writeln "sequence: Next1: consonant"))               
                ;;(++ n)
                 ;;(writeln "in Next2 n is: "  n)
               ;;(writeln " alpha cc: " alpha[cc] )
                ;;(++ start)
                (goto Next1:)
            ) ;; end begin
        ) ;;end if
        (++ start)
    ) ;; end while
)
(if (= traceLinesOn true)(writeln "sequence: n: " n))
(if (= traceLinesOn true)(writeln "sequence --end--"))
(return n)
)


























;;**EXPORTKEY**:porterStemmer:setto
(defchild porterStemmer:setto(newsuffix oldsuffix last)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Summary: This Lambda drops the old suffix
;; 		replaces the suffix of the
;; 		evaluated word to a new suffix. 
;; 		The length of the new word is adjusted
;;
;; Args: newsuffix    String representing the suffix which is placed
;;                    starting from the position of the old suffix 
;;                    in the current word evaluated.
;;       oldsuffix    String representing the suffix to be replaced.
;;       last         CharPointer pointing to the character in the original
;;                    word where the new suffix is to be appended.
;;
;; Return  testing    String representing the modified word with the new suffix.
;;
;; Example:
;;   Evaluated word: formalize
;;   last: Points to "i" in formalize
;;   newsuffix: AL
;;   oldsuffix: ALIZE
;;   return: formal
;;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

vars: (result  (ByteVector:testing)  )
regs:( (CharPointer:sLast) (CharPointer:nLast) (CharPointer:start)(CharPointer:first) p cc k N M n)

(if (= traceLinesOn true)(writeln "setto --begin-- " ))
(if (= traceLinesOn true)(writeln "setto: newsuffix: " newsuffix  " from olsuffix: " oldsuffix))
(if (= traceLinesOn true)(writeln "setto: original inword: " word))

 
(setq testing (new Vector: Byte: 500))
;;(setq test #void)
(setq first word)

(setq cc last[0])
(setq N (- (length newsuffix) 1))
(setq M (- (length oldsuffix) 1))
 
(setq nLast word)
(vmregRunInHardware start:)
(while (<> (setq cc nLast[0]) 0) do
    (++ nLast)
    (++ k)  ;; length of the original word
) ;; end while
(-- nLast)
(vmregRunInHardware stop:)
 
(if (= traceLinesOn true)(writeln "setto: k: " k))
(if (= traceLinesOn true)(writeln "setto: after while: " (char nLast[0])))

;; Get the word before the suffix change
(setq p 0)

(setq temp (+ M 1))
(if (= traceLinesOn true)(writeln "temp is: " temp))
(while (<> p (- k temp)) do 
    (setq start first)
    (setq cc start[0])
    (if (= traceLinesOn true)(writeln "setto: cc is: " (char cc)))
    (setq testing p cc)      
    (setq testing (++ p) 0)
    (if (= traceLinesOn true)(writeln "test: " testing))
    (++ first))
(if (= traceLinesOn true)(writeln "setto testing after: " testing))
(if (= traceLinesOn true)(writeln "setto p: " p))
 

(if (compareEQ N M)
    (begin 
        (setq sLast (- last N))
        ;;(setq p (- p (+ N 1)))
    ) ;; end begin
    else
    (begin
        (setq sLast (- last M))
       
       ;;(setq p (- p (+ M 1)))    
    ) ;; end begin
) ;; end if

(if (= traceLinesOn true)(writeln "setto: after compareEQ: last[0]: " (char last[0])))
(if (= traceLinesOn true)(writeln "setto: after compareEQ: sLast[0]: " (char sLast[0])) )
(if (= traceLinesOn true)(writeln "setto p: " p))

;; for debugging purposes
(setq cc sLast[0])
 
 

(loop for n from 0 until (+ N 1) do
     ;; set the new value
     ;;(writeln "setto:loop: sLast[0]: " (char sLast[0]))
     (if (= traceLinesOn true)(writeln "setto:loop: newsuffix[n]: " (char newsuffix[n])))  
     (setq sLast[0] newsuffix[n])
     (setq testing p newsuffix[n])
     (setq testing (++ p) 0)
     (if (= traceLinesOn true)(writeln "setto: in loop: testing: " testing))
     (setq cc sLast[0])  
      
     (++ sLast)
) ;; end loop

(if (= traceLinesOn true)(writeln "setto: out of loop: test: " testing))

(if (= traceLinesOn true)(writeln "setto:out of loop: sLast[0]: result " (char sLast[0])))
;; set the end of word to 0
;; testing for resize function
;;(while (<> (setq cc sLast[0]) 0) do (setq sLast[0] 0) (++ sLast))
(resize word p) 
;; updating the value of test

(if (= traceLinesOn true)(writeln "setto: new test: " testing))
testing) ;; end ends

















;;**EXPORTKEY**:porterStemmer:step1ab
(defchild porterStemmer:step1ab(inword)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Summary:  step1ab() gets rid of plurals and -ed or -ing.
;;
;; Input:    inword     The string to be stemmed.
;; Return:   test       The resulting word after the rules in step1ab 
;;                      are implemented on the string to be stemmed.
;; 
;; Rules:
;;  In a set of rules written beneath each other, only one is obeyed, and this
;;  will be the one with the longest matching stem or root word for the given word.
;; 
;;  =======
;;  step1a
;;  =======
;;    SSES -> SS                         caresses  ->  caress
;;    IES  -> I                          ponies    ->  poni
;;    SS   -> SS                         caress    ->  caress
;;    S    ->                            cats      ->  cat
;;
;;  ======
;;  step1b
;;  ======
;;    (m>0) EED -> EE                    feed      ->  feed
;;                                       agreed    ->  agree
;;    (*v*) ED  ->                       plastered ->  plaster
;;                                       bled      ->  bled
;;    (*v*) ING ->                       motoring  ->  motor
;;                                       sing      ->  sing
;;
;;  If the second or third of the rules in step1b is successful, the following
;;  is done:
;;
;;    AT -> ATE                          conflat(ed)  ->  conflate
;;    BL -> BLE                          troubl(ed)   ->  trouble
;;    IZ -> IZE                          siz(ed)      ->  size
;;   (*d and not (*L or *S or *Z))
;;      -> single letter
;;                                       hopp(ing)    ->  hop
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
vars: ( result ch  suffix 
       (ByteVector:test1)       ;; root word
      )
regs:(   cc 
         k                      ;; length of word
        (CharPointer:first)     ;; first = first letter
        (CharPointer:first1) 
        (CharPointer:last)      ;; last = last letter
      )

(if (= traceLinesOn true)(writeln #\newline "step1ab --begin--"))

;; Set pointers to the evaluated word
(setq first inword)
(setq first1 inword)
(setq last inword)

(if (= traceLinesOn true)(writeln "step1ab: first character: " (char first[0])))

;; Set the last pointer and set the counter for the number of
;; characters in the evaluated word
(vmregRunInHardware start:)
(while (<> (setq cc last[0]) 0) do
    (++ last)
    (++ k)
) ;; end while
(-- last)
(vmregRunInHardware stop:)
(setq cc last[0])

(if (= traceLinesOn true)(writeln "step1ab: last character: " (char cc)))
(if (= traceLinesOn true)(writeln "step1ab: k value is: " k))
 
 
;; Suffix Rules

;; Suffix Rule: if word ends in s
(if (= cc #\s)
    (begin 
        ;; end if for sses     
        (if (ends "sses" last)(setq k (- k 2))) 
        (if (ends "ies" last)
            (begin                
                (setto "i" "ies" last)
             ) ;; end begin
        ) ;; end if 
        ;; for other words ending in s - e.g.  cats, dogs, chairs     
 
        (if (and (compareNE inword[(- k 2)] "s") (= cc #\s)) 
           (begin              
             (setq k (- k 1))
             ;; Get new word 
             (setq first1 inword)   
             (setq n 0)              
             (while (<> n k)                    
                (setq cc first1[n])                  
                (setq inword n cc)
                (++ n)
             ) ;; end while              
             (setq inword n 0)              
             ;; Reset pointer           
             (setq last inword)
             (vmregRunInHardware start:)
             (setq k 0)
             (while (<> (setq cc last[0]) 0) do                  
                 (++ last)
                 (++ k)
               ) ;; end while
             (-- last)
             (vmregRunInHardware stop:)
            ) ;; end begin
        ) ;; end if 
    ) ;; end begin
) ;; end if for s
 
(if (= traceLinesOn true)(writeln "step1ab: after SR: ends in s" ))
(if (= traceLinesOn true)(writeln "step1ab: k: " k ))
(if (= traceLinesOn true)(writeln "step1ab: last: " (char last[0]) ))

;; Initialize the variable to store the root word
(setq test1 (new Vector: Byte: 100))

;; Suffix Rule: if words end in eed, retain or remove "d"
(if (ends "eed" last)
    (begin        
         (setq test1 (getStem first "eed"))           
         (if (> (sequence test1) 0)
            (begin
                (-- k)
            ) ;; end begin
         ) ;; end if
    ) ;; end begin
) ;; end eed

 
;; Suffix Rule: if words end in ed or ing, remove the suffix
(if (ends "ed" last) (setq suffix "ed"))
(if (ends "ing" last) (setq suffix "ing"))
(setq test1 (getStem first suffix))

(if (and (or (= suffix "ed") (= suffix "ing")) (vowelinstem test1) (= false (ends "eed" last)))
    (begin 
        (if (= suffix "ed") 
            (begin 
                (if (= traceLinesOn true)(writeln "step1ab: ED" ))
                (setq k (- k 2)) 
                (setq last (- last 2))
                (setq inword (getStem first "ed"))
                ;; testing, other option is just to use getStem 
                ;; (goto Trim:)
             ) ;; end begin
         ) ;; end if

        (if (= suffix "ing") 
            (begin 
                (if (= traceLinesOn true)(writeln "step1ab: ING" ))
                (setq k (- k 3)) 
                (setq last (- last 3)) 
                (setq inword (getStem first "ing"))                 
             ) ;; end begin
        ) ;; end if
        ;;  Suffix Rule: if the above two rules are successful, do the following:
       (if (ends "at" last) 
            (begin 
               (if (= traceLinesOn true)(writeln "step1ab: AT" ))
               
               ;; Get new word 
             (setq first1 inword)   
             (setq n 0)              
             (while (<> n k)                    
                (setq cc first1[n])                  
                (setq inword n cc)
                (++ n)
             ) ;; end while
             (setq word n 0)
                              
               (setq inword (setto "ate" "at" last))
               (if (= traceLinesOn true)(writeln "step1ab: after ate: inword: " inword))
               (setq last inword)
               (vmregRunInHardware start:)
               (setq k 0)
               ;; Reset the last pointer and 
               ;; get the new length of the word
               (while (<> (setq cc last[0]) 0) do
                    (++ last)
                    (++ k)
               ) ;; end while
               (-- last)
               (vmregRunInHardware stop:)
            ) ;; end begin
        ) ;; end if
        (if (ends "bl" last)
            (begin
               (if (= traceLinesOn true)(writeln "step1ab: BL" ) )
               ;; Get new word 
             (setq first1 inword)   
             (setq n 0)              
             (while (<> n k)                    
                (setq cc first1[n])                  
                (setq inword n cc)
                (++ n)
             ) ;; end while
             (setq word n 0)
               (setq inword (setto "ble" "bl" last))
               (setq last inword)
               (vmregRunInHardware start:)
               (setq k 0)
               ;; Reset the last pointer and 
               ;; get the new length of the word
               (while (<> (setq cc last[0]) 0) do
                    (++ last)
                    (++ k)
               ) ;; end while
               (-- last)
               (vmregRunInHardware stop:)
             ) ;; end begin
        ) ;; end if
        (if (ends "iz" last) 
            (begin
               (if (= traceLinesOn true)(writeln "step1ab: IZ" ))
               ;; Get new word 
             (setq first1 inword)   
             (setq n 0)              
             (while (<> n k)                    
                (setq cc first1[n])                  
                (setq inword n cc)
                (++ n)
             ) ;; end while
             (setq word n 0)
               (setq inword (setto "ize" "iz" last))
               (setq last inword)
               (vmregRunInHardware start:)
               (setq k 0)
               ;; Reset the last pointer and 
               ;; get the new length of the word
               (while (<> (setq cc last[0]) 0) do
                    (++ last)
                    (++ k)
               ) ;; end while
               (-- last)
               (vmregRunInHardware stop:)
            ) ;; end begin
       ) ;; end if

        ;; get last value
        (if (= traceLinesOn true)(writeln "step1ab: after second stem last: " (char last[0])))

        ;; Suffix Rule: Remove the last letter if the word ends in double letters
        (if (= true (doublec last))
            (begin
                 (-- k)
                 (setq cc last[0])
                 (if (or (= cc #\s) (= cc #\z) (= cc #\l)) (++ k))
            ) ;; end begin
            else
           (begin
                 (if (= traceLinesOn true)(writeln "step1ab: else for doublec"))
                 (if (= traceLinesOn true)(writeln "step1ab: else for doublec: test1: " test1))
                 (if (= traceLinesOn true)(writeln " K is : " k))
                 (if (and (cvc last) (= (sequence test1) 1) ) 
                     (begin 
                        (if (= traceLinesOn true)(writeln "here in setting last to e"))
                        (if (= traceLinesOn true)(writeln "step1ab: last: " (char last[0])))
                         (setq inword k #\e)
                        (if (= traceLinesOn true)(writeln "step1ab: after: " inword))
                        ;;(setq inword (setto "e" "ing" last)))
                        ;;(setq inword (setto "e" "ed" last)))
                      ;; Get length of word
                      (setq last inword)
                      (vmregRunInHardware start:)
                      (while (<> (setq cc last[0]) 0) do
                          (++ last)
                          (++ k)
                       ) ;; end while
                       (-- last)
                       (vmregRunInHardware stop:)
                       (setq cc last[0])
                       ) ;; end begin
               ) ;; end if                 
           ) ;; end begin
        ) ;; end if
        
       (if (= traceLinesOn true)(writeln "step1ab: after doublec: inword is: " inword))
       (setq first1 inword)
    ) ;; end begin
) ;; end if

 
;; trim word according to length
Trim::
(if (= traceLinesOn true)(writeln "step1ab: Trim: k is: " k))
(setq n 0)
(setq test  (new Vector: Byte: 250))
(while (<> n k)   
   (setq cc first1[n])  
   (setq test n cc)
   (setq test (++ n) 0)             
) ;; end while
 
(setq word test)
(if (= traceLinesOn true)(writeln "step1ab: test: " test))
(if (= traceLinesOn true)(writeln "step1ab --end--"))
 
test) ;; end step1ab



























;;**EXPORTKEY**:porterStemmer:step1c
(defchild porterStemmer:step1c(word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Summary This  step deals with words ending in
;;		 in "y".  This may convert the letter "y"
;;       to "i" or retain it.
;;
;; Input:    word     The string to be stemmed.
;; Return:   word     The resulting word after the rules in step1c 
;;                      are implemented on the string to be stemmed.
;; 
;; Rule:
;; (*v*) Y -> I                    happy        ->  happi
;;                                 sky          ->  sky
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

vars: (result (ByteVector:test))
regs:( (CharPointer:last) (CharPointer:first) cc   n k)

(if (= traceLinesOn true)(writeln "step1c -- start-- "))

;; Update the value of the last pointer
(setq last word)
(vmregRunInHardware start:)
(while (<> (setq cc last[0]) 0) do
      (++ last)
      (++ k)
) ;; end while
(-- last)
(vmregRunInHardware stop:)
(setq cc last[0])

;; Suffix Rule: if word ends in y, chnage to "i" or retain
(if (and (vowelinstem word) (= cc #\y)) 
    (begin
        ;; point to the last letter in the word
        (setq last[0] #\i)
        ;; setting the new word
        (setq first word)
        (if (= traceLinesOn true)(writeln "step1c: after vowelinstem: k is: " k))
        (if (= traceLinesOn true)(writeln "step1c: word is: " word))
        (setq n 0)
        (setq test  (new Vector: Byte: 250))
        (while (<> n k)   
            (setq cc first[n])  
            (if (= traceLinesOn true)(writeln "in end cc: " (char cc)) )
            (setq test n cc)
            (setq test (++ n) 0)
         )
         (if (= traceLinesOn true)(writeln "step1c: test: " test))
         (if (= traceLinesOn true)(writeln "step1c --end--"))
         (return test)
     ) ;; end begin
 ) ;; end if


(if (= traceLinesOn true)(writeln "step1c: word: " word))
(if (= traceLinesOn true)(writeln "step1c --end--"))
  

word) ;; end ends


























;;**EXPORTKEY**:porterStemmer:step2 
(defchild porterStemmer:step2(word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Summary:  This step deals with words with long
;; 			suffixes.
;;
;; Input:    word       The string to be stemmed.
;; Return:   result     The resulting word after the rules in step2 
;;                      are implemented on the string to be stemmed.
;;
;; Rules:
;;  In a set of rules written beneath each other, only one is obeyed, and this
;;  will be the one with the longest matching stem or root word for the given word.
;;
;;    (m>0) ATIONAL ->  ATE           relational     ->  relate
;;    (m>0) TIONAL  ->  TION          conditional    ->  condition             
;;    (m>0) ENCI    ->  ENCE          valenci        ->  valence
;;    (m>0) ANCI    ->  ANCE          hesitanci      ->  hesitance
;;    (m>0) IZER    ->  IZE           digitizer      ->  digitize
;;    (m>0) ABLI    ->  ABLE          conformabli    ->  conformable
;;    (m>0) ALLI    ->  AL            radicalli      ->  radical
;;    (m>0) ENTLI   ->  ENT           differentli    ->  different
;;    (m>0) ELI     ->  E             vileli        - >  vile
;;    (m>0) OUSLI   ->  OUS           analogousli    ->  analogous
;;    (m>0) IZATION ->  IZE           vietnamization ->  vietnamize
;;    (m>0) ATION   ->  ATE           predication    ->  predicate
;;    (m>0) ATOR    ->  ATE           operator       ->  operate
;;    (m>0) ALISM   ->  AL            feudalism      ->  feudal
;;    (m>0) IVENESS ->  IVE           decisiveness   ->  decisive
;;    (m>0) FULNESS ->  FUL           hopefulness    ->  hopeful
;;    (m>0) OUSNESS ->  OUS           callousness    ->  callous
;;    (m>0) ALITI   ->  AL            formaliti      ->  formal
;;    (m>0) IVITI   ->  IVE           sensitiviti    ->  sensitive
;;    (m>0) BILITI  ->  BLE           sensibiliti    ->  sensible
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

vars: (result (ByteVector:result))
regs:( (CharPointer:last) (CharPointer:first) cc   n k)

(if (= traceLinesOn true)(writeln "step2 --start-- "))

(setq result word)
(setq first word)
;; Does not remove suffix from small words
;;(if (= (sequence word) 0) (goto End:))

;; Update last pointer
(setq last word)
(vmregRunInHardware start:)
(while (<> (setq cc last[0]) 0) do
    (++ last)
    (++ k)
) ;; end while
(-- last)
(vmregRunInHardware stop:)
(setq cc last[0])

(if (= traceLinesOn true)(writeln "step2: last character: " (char cc)))
(if (= traceLinesOn true)(writeln "step2: k value is: " k))


;; Program switch on the last letter of the word 

(if (= cc #\l)
    (begin
        (if (and (ends "ational" last) (> (sequence (getStem first "ational")) 0)) (begin (setq result (setto "ate" "ational" last)) (goto End:)))
        (if (and (ends "tional" last) (> (sequence (getStem first "tional")) 0)) (begin (setq result (setto "tion" "tional" last)) (goto End:))) 
    ) ;; end begin
) ;; end if

(if (= cc #\i)
    (begin
        (if (and (ends "enci" last) (> (sequence (getStem first "enci")) 0))     (begin (setq result (setto "ence" "enci" last)) (goto End:)))
        (if (and (ends "anci" last) (> (sequence (getStem first "anci")) 0))     (begin (setq result (setto "ance" "anci" last)) (goto End:)))
        (if (and (ends "abli" last) (> (sequence (getStem first "abli")) 0))     (begin (setq result (setto "able" "abli" last)) (goto End:)))
        (if (and (ends "alli" last) (> (sequence (getStem first "alli")) 0))     (begin (setq result (setto "al" "alli" last)) (goto End:)))
        (if (and (ends "entli" last) (> (sequence (getStem first "entli")) 0))   (begin (setq result (setto "ent" "entli" last)) (goto End:)))
        (if (and (ends "eli" last) (> (sequence (getStem first "eli")) 0))       (begin (setq result (setto "e" "eli" last)) (goto End:)))
        (if (and (ends "ousli" last) (> (sequence (getStem first "ousli")) 0))   (begin (setq result (setto "ous" "ousli" last)) (goto End:))) 
		(if (and (ends "aliti" last) (> (sequence (getStem first "aliti")) 0))   (begin (setq result (setto "al" "aliti" last)) (goto End:)))
		(if (and (ends "iviti" last) (> (sequence (getStem first "iviti")) 0))   (begin (setq result (setto "ive" "iviti" last)) (goto End:)))
		(if (and (ends "biliti" last) (> (sequence (getStem first "biliti")) 0)) (begin (setq result (setto "ble" "biliti" last)) (goto End:)))
    ) ;; end begin
) ;; end if

(if (= cc #\s)
    (begin
		(if (and (ends "iveness" last)(> (sequence (getStem first "iveness")) 0))  (begin (setq result (setto "ive" "iveness" last)) (goto End:)))
		(if (and (ends "fulness" last) (> (sequence (getStem first "fulness")) 0)) (begin (setq result (setto "ful" "fulness" last)) (goto End:)))
		(if (and (ends "ousness" last) (> (sequence (getStem first "ousness")) 0)) (begin (setq result (setto "ous" "ousness" last)) (goto End:)))
    ) ;; end begin
) ;; end if

(if (= cc #\n)
    (begin
		(if (and (ends "ization" last) (> (sequence (getStem first "ization")) 0)) (begin (setq result (setto "ize" "ization" last)) (goto End:)))
		(if (and (ends "ation" last) (> (sequence (getStem first "ation")) 0))     (begin (setq result (setto "ate" "ation" last)) (goto End:)))
    ) ;; end begin
) ;; end if

(if (= cc #\r)
    (begin
		(if (and (ends "izer" last) (> (sequence (getStem first "izer")) 0)) (begin (setq result (setto "ize" "izer" last)) (return result)))
		(if (and (ends "ator" last) (> (sequence (getStem first "ator")) 0)) (begin (setq result (setto "ate" "ator" last)) (goto End:)))
    ) ;; end begin
) ;; end if
 

(if (and (ends "alism" last)(> (sequence (getStem first "alism")) 0)) (begin (setq result (setto "al" "alism" last)) (return result)))


 
End::  
(if (= traceLinesOn true)(writeln "step2: result: " result))

;; Update the value of word
(setq word result)
(if (= traceLinesOn true)(writeln "step2: word is: " word))
(if (= traceLinesOn true)(writeln "step2 --end--"))
  
 

result) ;; end ends









;;**EXPORTKEY**:porterStemmer:step3 
(defchild porterStemmer:step3(inword)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Summary: This step further stems the resulting word
;;          from step2.
;;
;; Input:    inword     The string to be stemmed.
;; Return:   result     The resulting word after the rules in step3 
;;                      are implemented on the string to be stemmed.
;;
;; Rules:
;;  In a set of rules written beneath each other, only one is obeyed, and this
;;  will be the one with the longest matching stem or root word for the given word.
;;
;;    (m>0) ICATE ->  IC              triplicate     ->  triplic
;;    (m>0) ATIVE ->                  formative      ->  form
;;    (m>0) ALIZE ->  AL              formalize      ->  formal
;;    (m>0) ICITI ->  IC              electriciti    ->  electric
;;    (m>0) ICAL  ->  IC              electrical     ->  electric
;;    (m>0) FUL   ->                  hopeful        ->  hope
;;    (m>0) NESS  ->                  goodness       ->  good
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

vars: (result (ByteVector:result))
regs:( (CharPointer:last) (CharPointer:first) cc   n k)

(if (= traceLinesOn true)(writeln "step3 --start-- "))
(if (= traceLinesOn true)(writeln "step3: inword is: " inword))

(setq first inword)
(setq result inword) 
;;(if (<= (sequence word) 0) (goto End:))

;; Update the last pointer
(setq last inword)
(vmregRunInHardware start:)
(while (<> (setq cc last[0]) 0) do
    (++ last)
    (++ k)
) ;; end while
(-- last)
(vmregRunInHardware stop:)
(setq cc last[0])

(if (= traceLinesOn true)(writeln "step3: last character: " (char cc)))
(if (= traceLinesOn true)(writeln "step3: k value is: " k))


(setq word inword)
(if (= traceLinesOn true)(writeln "step3: after setting word: " word))

;; Program switch on the last letter of the word 

(if  (= cc #\e)
    (begin 
        (if (and (ends "icate" last)(> (sequence (getStem first "icate")) 0)) (begin (setq result (setto "ic" "icate" last)) (goto End:)))
        (if (and (ends "ative" last)(> (sequence (getStem first "ative")) 0)) (begin (setq result (setto "" "ative" last)) (goto End:)))
        (if (and (ends "alize" last) (> (sequence (getStem first "alize")) 0)) (begin (setq result (setto "" "alize" last)) (goto End:)))
    ) ;; end begin
) ;; end if

(if  (= cc #\l)
    (begin 
		(if (and (ends "ical" last) (> (sequence (getStem first "ical")) 0)) (begin (setq result (setto "ic" "ical" last)) (goto End:)))
		(if (and (ends "ful" last) (> (sequence (getStem first "ful")) 0)) (begin (setq result (setto "" "ful" last)) (goto End:)))
    ) ;; end begin
) ;; end if


(if (and (ends "iciti" last)(> (sequence (getStem first "iciti")) 0)) (begin (setq result (setto "ic" "iciti" last)) (return result)))
(if (and (ends "ness" last) (> (sequence (getStem first "ness")) 0)) (begin (setq result (setto "" "ness" last)) (return result)))

End::
  
(if (= traceLinesOn true)(writeln "step3: result: " result))
;; Update the value of word
(setq word result)
(if (= traceLinesOn true)(writeln "step3: word is: " word))
(if (= traceLinesOn true)(writeln "step3: inword is: " inword))
(if (= traceLinesOn true)(writeln "step3 --end--"))
  

result) ;; end ends
                          
                          

;;**EXPORTKEY**:porterStemmer:step4 
(defchild porterStemmer:step4(inword)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Summary: This step further stems the resulting word
;;          from step3.
;;
;; Input:    inword     The string to be stemmed.
;; Return:   result     The resulting word after the rules in step4
;;                      are implemented on the string to be stemmed.
;;
;; Rules:
;;  In a set of rules written beneath each other, only one is obeyed, and this
;;  will be the one with the longest matching stem or root word for the given word.
;;
;;    (m>1) AL    ->                  revival        ->  reviv
;;    (m>1) ANCE  ->                  allowance      ->  allow
;;    (m>1) ENCE  ->                  inference      ->  infer
;;    (m>1) ER    ->                  airliner       ->  airlin
;;    (m>1) IC    ->                  gyroscopic     ->  gyroscop
;;    (m>1) ABLE  ->                  adjustable     ->  adjust
;;    (m>1) IBLE  ->                  defensible     ->  defens
;;    (m>1) ANT   ->                  irritant       ->  irrit
;;    (m>1) EMENT ->                  replacement    ->  replac
;;    (m>1) MENT  ->                  adjustment     ->  adjust
;;    (m>1) ENT   ->                  dependent      ->  depend
;;    (m>1 and (*S or *T)) ION ->     adoption       ->  adopt
;;    (m>1) OU    ->                  homologou      ->  homolog
;;    (m>1) ISM   ->                  communism      ->  commun
;;    (m>1) ATE   ->                  activate       ->  activ
;;    (m>1) ITI   ->                  angulariti     ->  angular
;;    (m>1) OUS   ->                  homologous     ->  homolog
;;    (m>1) IVE   ->                  effective      ->  effect
;;    (m>1) IZE   ->                  bowdlerize     ->  bowdler
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

vars: (result (ByteVector:result) (ByteVector:root))
regs:( (CharPointer:last) (CharPointer:first) cc   n r k)

(if (= traceLinesOn true)(writeln "step4 --start-- "))
(setq first inword)
(setq result inword) 
 
;; Update the last pointer
(setq last inword)
(vmregRunInHardware start:)
(while (<> (setq cc last[0]) 0) do
    (++ last)
    (++ k)
) ;; end while
(-- last)
(vmregRunInHardware stop:)
(setq cc last[0])

(setq word inword)

(if (= traceLinesOn true)(writeln "step4: last character: " (char cc)))
(if (= traceLinesOn true)(writeln "step4: k value is: " k))

;; Program switch on the last letter of the word 

(if  (= cc #\e)
    (begin 
		(if (and (ends "ance" last)(> (sequence (getStem first "ance")) 1)) (begin (setq result (setto "" "ance" last)) (goto End:)))
		(if (and (ends "ence" last) (> (sequence (getStem first "ence")) 1)) (begin (setq result (setto "" "ence" last)) (goto End:)))
		(if (and (ends "able" last) (> (sequence (getStem first "able")) 1))(begin (setq result (setto "" "able" last)) (goto End:)))
		(if (and (ends "ible" last) (> (sequence (getStem first "ible")) 1))(begin (setq result (setto "" "ible" last)) (goto End:)))
		(if (and (ends "ate" last)(> (sequence (getStem first "ate")) 1)) (begin (setq result (setto "" "ate" last)) (goto End:)))
		(if (and (ends "ive" last) (> (sequence (getStem first "ive")) 1))(begin (setq result (setto "" "ive" last)) (goto End:)))
		(if (and (ends "ize" last) (> (sequence (getStem first "ize")) 1))(begin (setq result (setto "" "ize" last)) (goto End:)))
    ) ;; end begin
) ;; end if

(if  (= cc #\t)
    (begin 
		(if (and (ends "ant" last) (> (sequence (getStem first "ant")) 1)) (begin (setq result (setto "" "ant" last)) (goto End:)))
		(if (and (ends "ement" last) (> (sequence (getStem first "ement")) 1)) (begin (setq result (setto "" "ement" last)) (goto End:)))
		(if (and (ends "ment" last) (> (sequence (getStem first "ment")) 1))(begin (setq result (setto "" "ment" last)) (goto End:)))
		(if (and (ends "ent" last) (> (sequence (getStem first "ent")) 1))(begin (setq result (setto "" "ent" last)) (goto End:)))
    ) ;; end begin
) ;; end if

 
(if (and (ends "al" last) (> (sequence (getStem first "al")) 1))(begin (setq result (setto "" "al" last)) (return result)))
(if (and (ends "er" last) (> (sequence (getStem first "er")) 1)) (begin (setq result (setto "" "er" last)) (return result)))
(if (and (ends "ic" last) (> (sequence (getStem first "ic")) 1)) (begin (setq result (setto "" "ic" last)) (return result)))
(if (ends "ion" last)
    (begin
       (setq root (getStem first "ion"))
       (if (= traceLinesOn true)(writeln "root is: " root))
       ;; get the last letter of root
       (setq r 0)
       (while (<> root[r] 0) do (setq cc root[r]) (++ r) )        
       (if (and (or (= cc #\t) (= cc #\s)) (> (sequence root) 1))
          (begin               
             (setq result (setto "" "ion" last)) 
             (return result)
           ) ;; end begin
       ) ;; end if
     ) ;; end begin
) ;; end if
(if (and (ends "ou" last) (> (sequence (getStem first "ou")) 1))(begin (setq result (setto "" "ou" last)) (return result)))
(if (and (ends "ism" last) (> (sequence (getStem first "ism")) 1)) (begin (setq result (setto "" "ism" last)) (return result)))
(if (and (ends "iti" last) (> (sequence (getStem first "iti")) 1))(begin (setq result (setto "" "iti" last)) (return result)))
(if (and (ends "ous" last) (> (sequence (getStem first "ous")) 1))(begin (setq result (setto "" "ous" last)) (return result)))


End::
  
(if (= traceLinesOn true)(writeln "step4: result: " result))
;; Update value of word
(setq word result)
(if (= traceLinesOn true)(writeln "step4: word is: " word))
(if (= traceLinesOn true)(writeln "step4 --end--"))
  

result) ;; end ends




;;**EXPORTKEY**:porterStemmer:step5
(defchild porterStemmer:step5(inword)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Summary:  This removes the double letters 
;;           at the end of the stemmed word.
;;
;; Input:    inword     The string to be stemmed.
;; Return:   result     The resulting word after the stemming process.
;;
;; Rule:
;;     (m > 1 and *d and *L) -> single letter
;;                                    controll       ->  control
;;                                    roll           ->  roll
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

vars: (result (ByteVector:test))
regs:( (CharPointer:last) (CharPointer:first) cc   n k)

(if (= traceLinesOn true)(writeln "step5a -- start-- "))
(setq result inword)
(if (= traceLinesOn true)(writeln "step5 result is: " result))
(setq last inword)
(vmregRunInHardware start:)
(while (<> (setq cc last[0]) 0) do
      (++ last)
      (++ k)
) ;; end while
(-- last)
(vmregRunInHardware stop:)

(setq cc last[0])

(setq word inword)

(if (= traceLinesOn true)(writeln "step5: inword: " inword))
(if (= traceLinesOn true)(writeln "step5: sequence: " (sequence inword)))
;;(if (and (> (sequence inword) 1) (= cc #\e)) 
;;    (begin
;;          (if (= traceLinesOn true)(writeln "step5: in first if: last[0]: " (char last[0])))
;;          (setq last[0] 0)
;;     ) ;; end begin
;; ) ;; end if

;; Reset last pointer
(setq last inword)
(while (<> (setq cc last[0]) 0) do(++ last)) ;; end while
(-- last)

;; Remove the last letter "e" 
;;(setq cc last[0])
;;(if (and (= (sequence inword) 1) (= cc #\e)(= false (cvc last))) 
;;    (begin  
;;    (setq last[0] 0)    
        ;;(setq result (setto "" "e" last))
;;     ) ;; end begin
;; ) ;; end if

;; Reset last pointer
(setq last inword)
(while (<> (setq cc last[0]) 0) do(++ last)) ;; end while
(-- last)


(setq cc last[0])

(if (and (> (sequence inword) 1) (= cc #\l) (doublec last)) 
    (begin      
      (setq last[0] 0)
     ) ;; end begin
 ) ;; end if

(setq inword result)
(if (= traceLinesOn true)(writeln "step5: result: " result))
(if (= traceLinesOn true)(writeln "step5 --end--"))
  

result) ;; end ends



;;**EXPORTKEY**:porterStemmer:vowelinstem
(defchild porterStemmer:vowelinstem(word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Summary: This Lambda returns  the Boolean value true
;; 		if  the word contains a vowel
;;
;; Arg:    word    String representing the word evaluated
;;
;; Return: result  Boolean value.  If true, the word contains a 
;;                 vowel.  Otherwise, false is returned.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

vars: (result)
regs:( (CharPointer:start) cc  N n)

(if (= traceLinesOn true)(writeln "vowelinstem --start--"))
(if (= traceLinesOn true)(writeln "vowelinstem: evaluated word: " word) )
(setq N (length word)) 
(setq start word)
(setq result false)
(loop for n from 0 until N do
    (setq cc start[n])
    (if (< cc 32) (goto End:))
     
;;(writeln " alpha[cc] is : " (char alpha[cc]) )
    (if (and (>= cc 97) (<=  cc 122) (= alpha[cc] 48))
        (begin
           (setq result true)
           (if (= traceLinesOn true)(writeln "vowelinstem: result: " result))
           (if (= traceLinesOn true)(writeln "vowelinstem --end--"))
           (return result)
        ) ;; end begin   
     ) ;; end if
) ;; end loop

End::
(if (= traceLinesOn true)(writeln "vowelinstem: result: " result))
(if (= traceLinesOn true)(writeln "vowelinstem --end--"))
result) ;; end ends

















