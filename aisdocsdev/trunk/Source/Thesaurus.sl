;;*************************************
;;*************************************
;; Exported Lambda File Cabinet Document
;;*************************************
;;*************************************

;;**EXPORTKEY**:Thesaurus
 
(defun Thesaurus()
;; november 8 6:00
;;
;;	The purpose of Thesaurus Lambda is to retrieve and display Wordnet data migrated to text files.
;;
;; STEPS TO RUN THESAURUS AGENT
;;	Step 1. Copy the following text files to C:/RefGuide
;;			wordnet1.txt, wordnet2.txt, synsetid.txt,	word_sstypes.txt, syn_ids_nounc.txt, syn_ids_verbc.txt, 
;;  		syn_ids_advc.txt,  syn_ids_adjc.txt, syn_ids_adjsc.txt, syn_ids_nouni.txt, 
;;  		syn_ids_verbi.txt, syn_ids_advi.txt, syn_ids_adji.txt, syn_ids_adjsi.txt, synsetc.txt, synseti.txt, 
;;    		wndbwords.txt, wndbc.txt, wndbi.txt, hypernymc.txt, hypernymi.txt, holoc.txt, holoi.txt
;;
;; 	Step 2. Copy and compile the code in Thesaurus.txt
;;
;;	Step 3. At the bottommost of the program, you can copy test commands and execute them at the console 



pvars:(
	findWord					; return the Word for a given Position  [0..144307]. If the word is not found then it returns the message "NoSuchWord with that Address". 
								; Invoke determineDictType
								; Reads wordnet1.txt and wordnet2.txt
	
	findLoc						; return the Position for a given Word;  if the word is valid then return [0..144307], 
								; otherwise return -189209;  We use a first letter index to our word vectors stored 
								; in sorted order ; so we can do a binary search ranged on only those words with the requisite starting letter.
								; Invoke findInterval1 and findInterval2
								;  Reads wordnet1.txt and wordnet2.txt

	determineDictType			; given Position, determine which among wordnet1.txt or wordnet2.txt is this Position valid

	findInterval1				; return the range of Position for a given Word. There is a corresponding range for each Letter.
								; Based on wordnet1.txt
		 	
	findInterval2				; return the range of Position for a given Word. There is a corresponding range for each Letter.
		 						; Based on wordnet2.txt
		 	
	findSynsetIdLoc				; return the synset_id for a given Position. Offset is obtained by multiplying Position with 9.
	findLocSynSetId				; return the Position for a given synsetid. Performs binary search on [0 .. 115423]

	findSSTypeLoc				; for a given Position, it returns the string of polysemy counts. Offset is obtained by multiplying Position with 10.
								; Reads word_sstypes.txt

	displaySSTypePolymCount		; for a given word, it displays the ss_type and the polysemy_count
								; Invoke findSSTypeLoc

	retSSTypePolymCount			; for a given word Position, it returns the ss_type and the polysemy_count
								; Invoke findSSTypeLoc
	
	retSSTypePolymCountW		; for a given word, it returns the ss_type and the polysemy_count
								; Invoke findSSTypeLoc

	retSynIdTagCountNoun		; for a given word Position and polysemyCount, it return a list of synset_id and  a list of tag_count
								; retSynIdTagCountNoun read syn_ids_nouni.txt and syn_ids_nounc.txt
								; Invoke extractsynIds,  extractTagNum 
	retSynIdTagCountVerb		; for a given word Position and polysemyCount, it return a list of synset_id and  a list of tag_count
	retSynIdTagCountAdverb		; for a given word Position and polysemyCount, it return a list of synset_id and  a list of tag_count
	retSynIdTagCountAdjective	; for a given word Position and polysemyCount, it return a list of synset_id and  a list of tag_count
	retSynIdTagCountAdjectiveSat ; for a given word Position and polysemyCount, it return a list of synset_id and  a list of tag_count
		
	retSynIdTagCount			; for a given word Position, ss_type and polysemyCount, it return a list of synset_id and  a list of tag_count
			 					; Invoke retSynIdTagCountNoun, retSynIdTagCountVerb, retSynIdTagCountAdverb, retSynIdTagCountAdjective, retSynIdTagCountAdjectiveSat
		
	retSynProperty				; for a given synset_id, it return lexicographical_group code, sense_numbers, synonymns and word meaning for its corresponding synset
  								; Invoke findLocSynSetId, tokenizer
  								; Read synsetc.txt and synseti.txt
				
;;;;--------------------------------------------------------------------------------------------------
	morphy						;  Generate inflected forms of a given word.
								;  (Thesaurus.morphy "axes") returns:
								;		#(#(6593 "axe" 0 ) #(6593 "axe" 1 ) #(6590 "ax" 1 ) #(6590 "ax" 0 ) #(6611 "axis" 0 ) )
								;		#(6593 "axe" 0 ) means:
								;								6593 is WordNumber
								;								"axe" is stemmed word
								;								0 means "noun"
								;  (Thesaurus.morphy "java") returns:
								;		#()
								;		which means that "java" has no inflected forms. Therefore we only use its base form "java" for search.


	stemMorph					;	Returns zero or more stems of an input word.
								;   Invoked by morphy.
								;	(Thesaurus.stemMorph "axes")) returns:
								;		#(("axe" . 0) ("axe" . 1) ("ax" . 1) ("axe" . 1) ("ax" . 0) )
								;			0 means "noun"
								;			1 means "verb"
	
	
	retWndbW					;	Returns zero or more inflected forms from a set of exception lists containing
								;		 the morphological transformations for words that are not regular and 
								;		 therefore cannot be processed in an algorithmic manner.
								;	Exception lists are stored in the files: wndbwords.txt, wndbi.txt, wndbc.txt
								;   Invoked by morphy.
								;	(Thesaurus.retWndbW "axes") returns:
								;		#(#(6590 0 "ax" ) #(6611 0 "axis" ) )

	
	retWndb						; 	Similar to retWndbW.
								;	(Thesaurus.retWndb "axes") returns:
								;				#((6590 . 0) (6611 . 0) )

	
	checkWithWordOrig			; 	Adjust the result of stemMorph based on the input word 
								;   Invoked by morphy.

	
	findWndbLoc					; return the Inflected Word for a given Position  [0..4481]
	findLocWndb					; return the Position for a given Inflected Word 

;;;;---------------------------------------------------------------------------------------------------

	retPerWordProperty			; for a given word, it returns a Vector of Vector of 4-tuple (ss_type polysemy_count word_position word ) 
								; Invoke findLoc, retSSTypePolymCountW, retSSTypePolymCount, mergeSSTypes3and4, finalSort
				
	displayAllWordSSType		; it tries to mimic Results window(Set advanced search option) of the Wordnet application browser
  		  						; invoke retPerWordProperty
				

	retHyperTree				;  for a given synset_id, it returns a pair of PARENT_VECTOR and CHILD_VECTOR
								;  Read hypernymc.txt and hypernymi.txt		

	displayHyperTree			;  it tries to mimic Results\[Noun | Verb ]\Hypernym	
								;  Invoke retHyperTree
								;  Uses Stack to implement heirarchical indention
				

	retHolonymRegular			; Returns regular holonyms for a given synset_id
								; Read holoc.txt and holoi.txt

	displayRegularHolonym		; it tries to mimic Results\Noun\Holonym regular
								; Invoke retHolonymRegular

	retHolonymInherited			; Returns inherited holonyms for a given synset_id
								; Read holoc.txt and holoi.txt

	displayInheritedHolonym 	; it tries to mimic Results\Noun\Holonym inherited
								; Invoke retHolonymInherited

	retDomain					; Returns domain for a given synset_id
								; Read holoc.txt and holoi.txt

	displayDomain				; it tries to mimic Results\[Noun | Verb ]\Domain
								; Read holoc.txt and holoi.txt

	retDomainTerm				; Returns domain term for a given synset_id
								; Read holoc.txt and holoi.txt
	
	displayDomainTerm			; it tries to mimic Results\[Noun | Verb ]\Domain Terms
								; Read holoc.txt and holoi.txt
	
;; utility functions
	tokenizer
	extractTagNum
	extractsynIds
	lexGrouping
	lexGroupSSType
	displaysynProperty
	displaysynPropertyShort
	displayssType
	mergeSSTypes3and4
	finalSort
	purgeDup
	rPad
	spaceToUnderScore
	underscoreToSpace
 	sortParent
	removeAtLoc
	findChildren
	stackADT
)

(defun findWord(wordNum)
	(setq dicttype (determineDictType wordNum))
	(if (= dicttype 1)
		(begin
			(setq file1 (fileOpen "C:/RefGuide/wordnet1.txt" 0 0))
			(setq pos (* wordNum 12))
			(fileSeek file1 pos 1)
			(setq readWord (fileRead file1 12))
			(fileClose file1 1)
		)
	else
		(if (= dicttype 2)
			(begin
				(setq wordNum (- wordNum  94604 ))
				(setq file2 (fileOpen "C:/RefGuide/wordnet2.txt" 0 0))
				(setq pos (* wordNum 50))
				(fileSeek file2 pos 1)
				(setq readWord (fileRead file2 50))
				(fileClose file2 1)
			)
		else
			(setq readWord "NoSuchWord with that Address")
		)
	)
readWord
)

(defun findLoc(word)
	(setq word (downcase word))
	(if (<= (length word) 12)
		(begin
			(setq interval1 (findInterval1 word))
			(setq file1 (fileOpen "C:/RefGuide/wordnet1.txt" 0 0))
			(setq word (rPad word 12) )
			(setq left1 (car interval1))
			(setq right1 (cdr interval1))
			(setq loc -1)
			(setq dicttype -1)
			(while (and (<= left1 right1) (= loc -1))
				(begin
					(setq mid1 (divi (+ left1 right1) 2))
					(setq pos (* mid1 12))
					(fileSeek file1 pos 1)
					(setq readWord (fileRead file1 12))
					(if (> word readWord)
						(setq left1 (+ mid1 1))
					else 
						(if (< word readWord)
							(setq right1 (- mid1 1))
						else
							(begin
								(setq loc mid1)
								(setq dicttype 1)
							)
						)
					)
				)
			)
			(fileClose file1 1)
			
		)
	else
		(begin
			(setq interval2 (findInterval2 word))
			(setq file1 (fileOpen "C:/RefGuide/wordnet2.txt" 0 0))
			(setq word (rPad word 50) )
			(setq left1 (car interval2))
			(setq right1 (cdr interval2))
			(setq loc -1)
			(setq dicttype -1)
			(while (and (<= left1 right1) (= loc -1))
				(begin
					(setq mid1 (divi (+ left1 right1) 2))
					(setq pos (* mid1 50))
					(fileSeek file1 pos 1)
					(setq readWord (fileRead file1 50))
					(if (> word readWord)
						(setq left1 (+ mid1 1))
					else 
						(if (< word readWord)
							(setq right1 (- mid1 1))
						else
							(begin
								(setq loc mid1)
								(setq dicttype 2)
							)
						)
					)
				)
			)
			(fileClose file1 1)
		)
	)
(+ (* (- dicttype 1) 94604) loc)
)


(defun determineDictType(loc)
	(if (and (>= loc 0) (<= loc 94603))
		(setq ret 1)
	else
		(if (and (>= loc 94604) (<= loc 144307))
			(setq ret 2)
		else
			(setq ret -1)
		)
	)
ret
)

(defun findInterval1(word)
	(case word[0]
		(("a") (setq lowinter	298	) (setq highinter 	6679	))
		(("b") (setq lowinter	6680	) (setq highinter 	12742	))
		(("c") (setq lowinter	12743	) (setq highinter 	21458	))
		(("d") (setq lowinter	21459	) (setq highinter 	26265	))
		(("e") (setq lowinter	26266	) (setq highinter 	29551	))
		(("f") (setq lowinter	29552	) (setq highinter 	33474	))
		(("g") (setq lowinter	33475	) (setq highinter 	37541	))
		(("h") (setq lowinter	37542	) (setq highinter 	41430	))
		(("i") (setq lowinter	41431	) (setq highinter 	44309	))
		(("j") (setq lowinter	44310	) (setq highinter 	45367	))
		(("k") (setq lowinter	45368	) (setq highinter 	46527	))
		(("l") (setq lowinter	46528	) (setq highinter 	50256	))
		(("m") (setq lowinter	50257	) (setq highinter 	55471	))
		(("n") (setq lowinter	55472	) (setq highinter 	57688	))
		(("o") (setq lowinter	57689	) (setq highinter 	60034	))
		(("p") (setq lowinter	60035	) (setq highinter 	67329	))
		(("q") (setq lowinter	67330	) (setq highinter 	67699	))
		(("r") (setq lowinter	67700	) (setq highinter 	71817	))
		(("s") (setq lowinter	71818	) (setq highinter 	82278	))
		(("t") (setq lowinter	82279	) (setq highinter 	87323	))
		(("u") (setq lowinter	87324	) (setq highinter 	89735	))
		(("v") (setq lowinter	89736	) (setq highinter 	91213	))
		(("w") (setq lowinter	91214	) (setq highinter 	93846	))
		(("x") (setq lowinter	93847	) (setq highinter 	93994	))
		(("y") (setq lowinter	93995	) (setq highinter 	94321	))
		(("z") (setq lowinter	94322	) (setq highinter 	94603	))
	(else (begin (setq lowinter 0 ) (setq highinter 297)))
	)
(pair lowinter highinter)
)


(defun findInterval2(word)
	(case word[0]
		(("a") (setq lowinter	19	) (setq highinter 	3577	))
		(("b") (setq lowinter	3578	) (setq highinter 	5869	))
		(("c") (setq lowinter	5870	) (setq highinter 	11092	))
		(("d") (setq lowinter	11093	) (setq highinter 	13039	))
		(("e") (setq lowinter	13040	) (setq highinter 	14877	))
		(("f") (setq lowinter	14878	) (setq highinter 	17694	))
		(("g") (setq lowinter	17695	) (setq highinter 	22203	))
		(("h") (setq lowinter	22204	) (setq highinter 	23977	))
		(("i") (setq lowinter	23978	) (setq highinter 	25554	))
		(("j") (setq lowinter	25555	) (setq highinter 	26247	))
		(("k") (setq lowinter	26248	) (setq highinter 	26611	))
		(("l") (setq lowinter	26612	) (setq highinter 	28321	))
		(("m") (setq lowinter	28322	) (setq highinter 	30995	))
		(("n") (setq lowinter	30996	) (setq highinter 	32210	))
		(("o") (setq lowinter	32211	) (setq highinter 	33505	))
		(("p") (setq lowinter	33506	) (setq highinter 	37379	))
		(("q") (setq lowinter	37380	) (setq highinter 	37557	))
		(("r") (setq lowinter	37558	) (setq highinter 	39443	))
		(("s") (setq lowinter	39444	) (setq highinter 	44425	))
		(("t") (setq lowinter	44426	) (setq highinter 	46551	))
		(("u") (setq lowinter	46552	) (setq highinter 	47436	))
		(("v") (setq lowinter	47437	) (setq highinter 	48219	))
		(("w") (setq lowinter	48220	) (setq highinter 	49406	))
		(("x") (setq lowinter	49407	) (setq highinter 	49453	))
		(("y") (setq lowinter	49454	) (setq highinter 	49613	))
		(("z") (setq lowinter	49614	) (setq highinter 	49703	))
	(else (begin (setq lowinter 0 ) (setq highinter 18)))
	)
(pair lowinter highinter)
)



(defun findSynsetIdLoc(synsetIdNum)
	(setq fileSyn (fileOpen "C:/RefGuide/synsetid.txt" 0 0))
	(setq pos (* synsetIdNum 9))
	(fileSeek fileSyn pos 1)
	(setq readSynsetId (fileRead fileSyn 9))
	(fileClose fileSyn 1)
readSynsetId
)


(defun findLocSynSetId(word)
	(setq word ( string word))
	(setq fileSyn (fileOpen "C:/RefGuide/synsetid.txt" 0 0))
	(setq loc -1)
	(setq left1 0)
	(setq right1 115423)
	(while (and (<= left1 right1) (= loc -1))
		(begin
			(setq mid1 (divi (+ left1 right1) 2))
			(setq pos (* mid1 9))
			(fileSeek fileSyn pos 1)
			(setq readWord (fileRead fileSyn 9))
			(if (> word readWord)
				(setq left1 (+ mid1 1))
			else 
				(if (< word readWord)
					(setq right1 (- mid1 1))
				else
					(begin
						(setq loc mid1)
					)
				)
			)
		)
	)
	(fileClose fileSyn 1)
loc
)

(defun findSSTypeLoc(wordNum)
	(setq fileSyn (fileOpen "C:/RefGuide/word_sstypes.txt" 0 0))
	(setq pos (* wordNum 10))
	(fileSeek fileSyn pos 1)
	(setq readSynsetId (fileRead fileSyn 10))
	(fileClose fileSyn 1)
readSynsetId
)

(defun displaySSTypePolymCount(word)
	(setq toRet (new Vector:))
	(setq loc (findLoc word))
	(setq ss (findSSTypeLoc loc))
	(loop for k from 0 until 5 by 1 do
		(begin
			(setq startstr (* k 2))
			(setq s (substring ss  startstr (+ startstr 1)))
			(if (<> s "  ")
				(begin
					(case k
						((0) (setq ssType "noun")	)
						((1) (setq ssType "verb")	)
						((2) (setq ssType "adverb")	)
						((3) (setq ssType "adjective")	)
						((4) (setq ssType "adjective")	)
					)
					(setq toRet (append toRet (new Vector:1 (pair ssType (integer (trim s))) ) ))
				)	
			)
		)
	)
toRet
)

(defun retSSTypePolymCount(wordNum)
	(setq toRet (new Vector:))
	(setq ss (findSSTypeLoc wordNum))
	(loop for k from 0 until 5 by 1 do
		(begin
			(setq startstr (* k 2))
			(setq s (substring ss  startstr (+ startstr 1)))
			(if (<> s "  ")
				(begin
					(setq entry (new Vector: 3 k (integer (trim s)) wordNum))
					(setq toRet (append toRet (new Vector:1 entry) ))
				)	
			)
		)
	)
toRet
)


(defun retSSTypePolymCountW(word)
	(setq wordNum (findLoc word))
	(setq toRet (new Vector:))
	(setq ss (findSSTypeLoc wordNum))
	(loop for k from 0 until 5 by 1 do
		(begin
			(setq startstr (* k 2))
			(setq s (substring ss  startstr (+ startstr 1)))
			(if (<> s "  ")
				(begin
					(setq entry (new Vector: 4 k (integer (trim s)) wordNum word))
					(setq toRet (append toRet (new Vector:1 entry) ))
				)	
			)
		)
	)
toRet
)

(defun retSynIdTagCountNoun(wordNum, polysemyCount)
	(setq fileSyn_nouni (fileOpen "C:/RefGuide/syn_ids_nouni.txt" 0 0))
	(setq pos (* wordNum 12))
	(fileSeek fileSyn_nouni pos 1)
	(setq offsetLength (fileRead fileSyn_nouni 12))
	(fileClose fileSyn_nouni 1)
	(setq lengthS (integer (trim (left offsetLength 4) )))
	(setq posit (integer (trim (mid offsetLength 4 8) )))
	(setq file2_nounc (fileOpen "C:/RefGuide/syn_ids_nounc.txt" 0 0))
	(setq pos1 posit)
	(fileSeek file2_nounc posit 1)
	(setq readContent (fileRead file2_nounc lengthS))
	(fileClose file2_nounc 1)
	(setq synStr (trim readContent))
	(setq sufStr (trim (right synStr (- (length synStr) (* polysemyCount 9 )) )) )
	(setq synIds (extractsynIds synStr, polysemyCount))
	(setq tagNum (extractTagNum sufStr))
	(setq toRetNoun (pair synIds tagNum))
toRetNoun
)



(defun retSynIdTagCountVerb(wordNum, polysemyCount)
	(setq fileSyn_verbi (fileOpen "C:/RefGuide/syn_ids_verbi.txt" 0 0))
	(setq pos (* wordNum 12))
	(fileSeek fileSyn_verbi pos 1)
	(setq offsetLengthVerb (fileRead fileSyn_verbi 12))
	(fileClose fileSyn_verbi 1)
	(setq lengthSVerb (integer (trim (left offsetLengthVerb 4) )))
	(setq posit (integer (trim (mid offsetLengthVerb 4 8) )))
	(setq file2_verbc (fileOpen "C:/RefGuide/syn_ids_verbc.txt" 0 0))
	(setq pos1 posit)
	(fileSeek file2_verbc posit 1)
	(setq readContent (fileRead file2_verbc lengthSVerb))
	(fileClose file2_verbc 1)
	(setq synStr (trim readContent))
	(setq sufStr (trim (right synStr (- (length synStr) (* polysemyCount 9 )) )) )
	(setq synIds (extractsynIds synStr, polysemyCount))
	(setq tagNum (extractTagNum sufStr))
	(setq toRetVerb (pair synIds tagNum))
toRetVerb
)



(defun retSynIdTagCountAdverb(wordNum, polysemyCount)
	(setq fileSyn_advi (fileOpen "C:/RefGuide/syn_ids_advi.txt" 0 0))
	(setq pos (* wordNum 12))
	(fileSeek fileSyn_advi pos 1)
	(setq offsetLength (fileRead fileSyn_advi 12))
	(fileClose fileSyn_advi 1)
	(setq lengthS (integer (trim (left offsetLength 4) )))
	(setq posit (integer (trim (mid offsetLength 4 8) )))
	(setq file2_advc (fileOpen "C:/RefGuide/syn_ids_advc.txt" 0 0))
	(setq pos1 posit)
	(fileSeek file2_advc posit 1)
	(setq readContent (fileRead file2_advc lengthS))
	(fileClose file2_advc 1)
	(setq synStr (trim readContent))
	(setq sufStr (trim (right synStr (- (length synStr) (* polysemyCount 9 )) )) )
	(setq synIds (extractsynIds synStr, polysemyCount))
	(setq tagNum (extractTagNum sufStr))
	(setq toRetAdv (pair synIds tagNum))
toRetAdv
)


(defun retSynIdTagCountAdjective(wordNum, polysemyCount)
	(setq fileSyn_adji (fileOpen "C:/RefGuide/syn_ids_adji.txt" 0 0))
	(setq pos (* wordNum 12))
	(fileSeek fileSyn_adji pos 1)
	(setq offsetLength (fileRead fileSyn_adji 12))
	(fileClose fileSyn_adji 1)
	(setq lengthS (integer (trim (left offsetLength 4) )))
	(setq posit (integer (trim (mid offsetLength 4 8) )))
	(setq file2_adjc (fileOpen "C:/RefGuide/syn_ids_adjc.txt" 0 0))
	(setq pos1 posit)
	(fileSeek file2_adjc posit 1)
	(setq readContent (fileRead file2_adjc lengthS))
	(fileClose file2_adjc 1)
	(setq synStr (trim readContent))
	(setq sufStr (trim (right synStr (- (length synStr) (* polysemyCount 9 )) )) )
	(setq synIds (extractsynIds synStr, polysemyCount))
	(setq tagNum (extractTagNum sufStr))
	(setq toRetAdj (pair synIds tagNum))
toRetAdj
)



(defun retSynIdTagCountAdjectiveSat(wordNum, polysemyCount)
	(setq fileSyn_adjsi (fileOpen "C:/RefGuide/syn_ids_adjsi.txt" 0 0))
	(setq pos (* wordNum 12))
	(fileSeek fileSyn_adjsi pos 1)
	(setq offsetLength (fileRead fileSyn_adjsi 12))
	(fileClose fileSyn_adjsi 1)
	(setq lengthS (integer (trim (left offsetLength 4) )))
	(setq posit (integer (trim (mid offsetLength 4 8) )))
	(setq file2_adjsc (fileOpen "C:/RefGuide/syn_ids_adjsc.txt" 0 0))
	(setq pos1 posit)
	(fileSeek file2_adjsc posit 1)
	(setq readContent (fileRead file2_adjsc lengthS))
	(fileClose file2_adjsc 1)
	(setq synStr (trim readContent))
	(setq sufStr (trim (right synStr (- (length synStr) (* polysemyCount 9 )) )) )
	(setq synIds (extractsynIds synStr, polysemyCount))
	(setq tagNum (extractTagNum sufStr))
	(setq toRetAdjS (pair synIds tagNum))
toRetAdjS
)



(defun retSynIdTagCount(wordNum, ssTypeCode, polysemyCount)
	(setq toRet1 (new Vector: ))
	(case ssTypeCode
		((0) (setq toRet1 (retSynIdTagCountNoun wordNum polysemyCount )))
		((1) (setq toRet1 (retSynIdTagCountVerb wordNum polysemyCount )))
		((2) (setq toRet1 (retSynIdTagCountAdverb wordNum polysemyCount )))
		((3) (setq toRet1 (retSynIdTagCountAdjective wordNum polysemyCount )))
		((4) (setq toRet1 (retSynIdTagCountAdjectiveSat wordNum polysemyCount )))
	)
toRet1
)


(defun retSynProperty(synsetId)
	(setq wordNum (findLocSynSetId synsetId))
	(setq fileSynseti (fileOpen "C:/RefGuide/synseti.txt" 0 0))
	(setq pos (* wordNum 12))
	(fileSeek fileSynseti pos 1)
	(setq offsetLength (fileRead fileSynseti  12))
	(fileClose fileSynseti 1)
	(setq lengthS (integer (trim (left offsetLength 4) )))
	(setq posit (integer (trim (mid offsetLength 4 8) )))
	(setq fileSynsetc (fileOpen "C:/RefGuide/synsetc.txt" 0 0))
	(setq pos1 posit)
	(fileSeek fileSynsetc posit 1)
	(setq readContent (fileRead fileSynsetc lengthS))
	(fileClose fileSynsetc 1)
	(setq synStr (trim readContent))
	(setq state 0)
	(setq lexGroup "")
	(setq senseNumStr "")
	(setq synonymStr "")
	(setq meaningStr "")
	(loop for k from 0 until (length synStr) by 1 do
		(begin
			(setq sym synStr[k])
			(case state
				((0)
					(begin
						(if (<> (string sym) "@") 
							(begin
								(setq lexGroup (append lexGroup sym ))
							)
						else
							(begin
								(setq state 1)
							)
						)
					)
				)

				(	(1)
					(begin
						(if (<> (string sym) "@") 
							(begin
								(setq senseNumStr (append senseNumStr sym ))
							)
						else
							(begin
								(setq state 2)
							)
						)
					)
				)

				(	(2)
					(begin
						(if (<> (string sym) "@") 
							(begin
								(setq synonymStr (append synonymStr sym ))
							)
						else
							(begin
								(setq state 3)
							)
						)
					)
					
				)

				(	(3)
					(begin
						(if (<> (string sym) "@") 
							(begin
								(setq meaningStr (append  meaningStr sym ))
							)
						else
							(begin
								(setq state 4)
							)
						)
					)
					
				)
				
			)
		)
    )
	(setq ret (new Vector:4 lexGroup (tokenizer senseNumStr " ") (tokenizer synonymStr " ") meaningStr ))
ret
)


;;;;;;;;;;;;;;;;;;;;;;;;;; morphy functions start ;;;;;;;;;;;;;;;;;;;;;;;

(defun morphy(str)
	(setq retmangledArr (stemMorph str))
	(if (<> (length wordOrigW) 0) (begin (setq retmangledArr (checkWithWordOrig wordOrigW retmangledArr )) ) )
	(setq  retmangledArr1 (new Vector: ))
	(setq  retmangledArr1W (new Vector: ))
	(loop for kretmangledArr from 0 until (length retmangledArr) by 1 do
		(begin
			(setq newEntry (findLoc (car retmangledArr[kretmangledArr])))
			(if (> newEntry 0) 
				(begin
					(setq  retmangledArr1 (append  retmangledArr1 (new Vector:1 (pair newEntry (cdr retmangledArr[kretmangledArr]) ))))
					(setq  retmangledArr1W (append  retmangledArr1W (new Vector:1 (new Vector:3 newEntry (cdr retmangledArr[kretmangledArr]) (car retmangledArr[kretmangledArr] )))))
				)
			)
		)
	)
	(setq retmangledArr (append retmangledArr1 (retWndb str)))
	(setq retmangledArrW (append retmangledArr1W (retWndbW str)))
	(setq mangledVec (new Vector: ))
	(setq mangledVecW (new Vector: ))
	(setq nretmangledArr (new Vector: ))
	(if (<> (length retmangledArr) 0)
		(begin
			(loop for countretmangledArr from 0 until (length retmangledArr) by 1 do
				(begin
					(if (not (isMember retmangledArr[countretmangledArr] nretmangledArr))
						(begin
							(setq nretmangledArr (append nretmangledArr (new Vector:1 retmangledArr[countretmangledArr] ) ))
							(setq currW (car retmangledArr[countretmangledArr]))
							(setq countmangledVec 0)
							(setq foundmangledVec 0)
							(while (and (< countmangledVec (length mangledVec )) (= foundmangledVec 0))
								(begin
									(if (= mangledVec[countmangledVec][0] currW)
										(begin
											(setq mangledVecW[countmangledVec] (append  mangledVecW[countmangledVec] (new Vector:1 retmangledArrW[countretmangledArr][1]) ))
											(setq foundmangledVec 1)
										)
									else
										(begin (setq  countmangledVec (+ countmangledVec 1)) )
							)))
							(if (= foundmangledVec 0)
								(begin
									(setq newEntryW retmangledArrW[countretmangledArr][0])
									(setq newVW (new Vector:3 newEntryW  retmangledArrW[countretmangledArr][2] retmangledArrW[countretmangledArr][1]) )
									(setq mangledVecW (append mangledVecW (new Vector:1 newVW )))
							))
					))
			))
	))
 mangledVecW
)

(defun stemMorph(str)
(setq retmangledArr (new Vector:))
(setq state 0)
(loop for k from (- (length str) 1) until -1 by -1 do
	(begin
		(setq sym str[k])
		(case state
				((0)
					(begin
						(case (string sym)
							(("s") (begin (setq state 1) 
										(setq wordM (left str (- (length str) 1)))
										(setq word1 (pair wordM 0))
										(setq word2 (pair wordM 1))
										(setq retmangledArr (append retmangledArr (new Vector: 2 word1 word2)))))
							(("d") (begin (setq state 2)))
							(("r") (begin (setq state 3)))
							(("t") (begin (setq state 4)))
							(("g") (begin (setq state 5)))
							(("n") (begin (setq state 6)))
						(else (begin (setq state 100)))
						)
					)
				)
				((1)
					(begin
						(case (string sym)
							(("e") (begin (setq state 7)
								(setq wordM1 (left str (- (length str) 2)))
								(setq wordM2 (left str (- (length str) 1)))
								(setq word1 (pair wordM1 1))
								(setq word2 (pair wordM2 1))
								(setq retmangledArr (append retmangledArr (new Vector: 2 word1 word2)))))
							(else (begin (setq state 100)))
						)
					)
				)
				((7)
					(begin
						(case (string sym)
							(("i") (begin (setq state 100) 
								(setq wordM (append (left str (- (length str) 3)) "y"))
								(setq word1 (pair wordM 0))
								(setq word2 (pair wordM 1))
								(setq retmangledArr (append retmangledArr (new Vector: 2 word1 word2)))))
							(("s") (begin (setq state 100) 
								(setq wordM (append (left str (- (length str) 3)) "s"))
								(setq word1 (pair wordM 0))
								(setq retmangledArr (append retmangledArr (new Vector: 1 word1)))))
							(("x") (begin (setq state 100) 
								(setq wordM (append (left str (- (length str) 3)) "x"))
								(setq word1 (pair wordM 0))
								(setq retmangledArr (append retmangledArr (new Vector: 1 word1)))))
							(("z") (begin (setq state 100) 
								(setq wordM (append (left str (- (length str) 3)) "z"))
								(setq word1 (pair wordM 0))
								(setq retmangledArr (append retmangledArr (new Vector: 1 word1)))))
							(("h") (begin (setq state 12) ))
						(else (begin (setq state 100)))
						)
					)
				)
				((12)
					(begin
						(case (string sym)
							(("c") (begin (setq state 100) 
								(setq wordM (append (left str (- (length str) 4)) "ch"))
								(setq word1 (pair wordM 0))
								(setq retmangledArr (append retmangledArr (new Vector: 1 word1)))))
							(("s") (begin (setq state 100)
								(setq wordM (append (left str (- (length str) 4)) "sh"))
								(setq word1 (pair wordM 0))
								(setq retmangledArr (append retmangledArr (new Vector: 1 word1)))))
							(else (begin (setq state 100)))
						)
					)
				)
				((2)
					(begin
						(case (string sym)
							(("e") (begin (setq state 100) 
								(setq wordM1 (append (left str (- (length str) 2)) "e") )
								(setq wordM2 (left str (- (length str) 2)))
								(setq word1 (pair wordM1 1))
								(setq word2 (pair wordM2 1))
								(setq retmangledArr (append retmangledArr (new Vector: 2 word1 word2)))))
							(else (begin (setq state 100)))
						)
					)
				)
				((3)
					(begin
						(case (string sym)
							(("e") (begin (setq state 100) 
								(setq wordM1 (left str (- (length str) 2)))
								(setq wordM2 (append (left str (- (length str) 2)) "e") )
								(setq word1 (pair wordM1 3))
								(setq word2 (pair wordM1 4))
								(setq word3 (pair wordM2 3))
								(setq word4 (pair wordM2 4))
								(setq retmangledArr (append retmangledArr (new Vector:4 word1 word2 word3 word4)))))
							(else (begin (setq state 100)))
						)
					)
				)
				((4)
					(begin
						(case (string sym)
							(("s") (begin (setq state 18)))
						(else (begin (setq state 100)))
						)
					)
				)
				((18)
					(begin
						(case (string sym)
							(("e") (begin (setq state 100) 
								(setq wordM1 (left str (- (length str) 3)))
								(setq wordM2 (append (left str (- (length str) 3)) "e") )
								(setq word1 (pair wordM1 3))
								(setq word2 (pair wordM1 4))
								(setq word3 (pair wordM2 3))
								(setq word4 (pair wordM2 4))
								(setq retmangledArr (append retmangledArr (new Vector:4 word1 word2 word3 word4)))))
							(else (begin (setq state 100)))
						)
					)
				)
				((5)
					(begin
						(case (string sym)
							(("n") (begin (setq state 20)))
						(else (begin (setq state 100)))
						)
					)
				)
				((20)
					(begin
						(case (string sym)
							(("i") (begin (setq state 100) 
								(setq wordM1 (append (left str (- (length str) 3)) "e") )
								(setq wordM2 (left str (- (length str) 3)))
								(setq word1 (pair wordM1 1))
								(setq word2 (pair wordM2 1))
								(setq retmangledArr (append retmangledArr (new Vector: 2 word1 word2)))))
							(else (begin (setq state 100)))
						)
					)
				)
				((6) (begin (case (string sym) (("e") (begin (setq state 22))) (else (begin (setq state 100))) ) )
				)
				((22)
					(begin
						(case (string sym)
							(("m") (begin (setq state 100) 
								(setq wordM (append (left str (- (length str) 3)) "man"))
								(setq word1 (pair wordM 0))
								(setq retmangledArr (append retmangledArr (new Vector: 1 word1)))))
							(else (begin (setq state 100)))
						)
					)
				)
				((100) (begin (goto DONE:) ) )
			)
		)
	)
DONE::
retmangledArr 
)






(defun retWndbW(word)
	(setq wordNum (findLocWndb word))
	(setq retVec (new Vector: ))
	(if (<> wordNum -1)
	(begin
	(setq fileretWndb (fileOpen "C:/RefGuide/wndbi.txt" 0 0))
	(setq pos (* wordNum 7))
	(fileSeek fileretWndb pos 1)
	(setq offsetLength (fileRead fileretWndb 7))
	(fileClose fileretWndb 1)
	(setq lengthS (integer (trim (left offsetLength 2) )))
	(setq posit (integer (trim (mid offsetLength 2 5) )))
	(setq file2retWndb (fileOpen "C:/RefGuide/wndbc.txt" 0 0))
	(setq pos1 posit)
	(fileSeek file2retWndb posit 1)
	(setq readContent (fileRead file2retWndb lengthS))
	(fileClose file2retWndb 1)
	(setq readContent (trim readContent))
	(setq tokenC (tokenizer readContent "@"))
	(setq token1 (tokenizer tokenC[0] " "))
	(setq token2 (tokenizer tokenC[1] " "))
	(loop for kToken1 from 0 until (length token1) by 1 do
		(begin (setq retVec (append retVec (new Vector:1 (new Vector:3 (integer token1[kToken1]) (integer token2[kToken1]) (trim (findWord (integer token1[kToken1]) )))))))
	)
	))
retVec
)

(defun retWndb(word)
	(setq wordNum (findLocWndb word))
	(setq retVec (new Vector: ))
	(if (<> wordNum -1)
	(begin
	(setq fileretWndb (fileOpen "C:/RefGuide/wndbi.txt" 0 0))
	(setq pos (* wordNum 7))
	(fileSeek fileretWndb pos 1)
	(setq offsetLength (fileRead fileretWndb 7))
	(fileClose fileretWndb 1)
	(setq lengthS (integer (trim (left offsetLength 2) )))
	(setq posit (integer (trim (mid offsetLength 2 5) )))
	(setq file2retWndb (fileOpen "C:/RefGuide/wndbc.txt" 0 0))
	(setq pos1 posit)
	(fileSeek file2retWndb posit 1)
	(setq readContent (fileRead file2retWndb lengthS))
	(fileClose file2retWndb 1)
	(setq readContent (trim readContent))
	(setq tokenC (tokenizer readContent "@"))
	(setq token1 (tokenizer tokenC[0] " "))
	(setq token2 (tokenizer tokenC[1] " "))
	(loop for kToken1 from 0 until (length token1) by 1 do
		(begin (setq retVec (append retVec (new Vector:1 (pair (integer token1[kToken1]) (integer token2[kToken1]))))))
	)
	))
retVec
)

(defun checkWithWordOrig(wordOrigW  retmangledArr)
(setq  retmangledArrJ  retmangledArr)
(loop for cretmangledArr from 0 until (length retmangledArr) by 1 do
	(begin
		(setq toInsert 1)
		(if  (or (= (cdr retmangledArr[cretmangledArr]) 3) (= (cdr retmangledArr[cretmangledArr]) 4))
		(begin
			(setq kwordOrigW 0)
			(setq foundwordOrigW 0)
			(while (and (< kwordOrigW (length wordOrigW)) (= foundwordOrigW  0))
				(begin
					(if  (or (= (cdr retmangledArr[cretmangledArr]) wordOrigW[kwordOrigW][0]  ) (= (cdr retmangledArr[cretmangledArr]) (+ wordOrigW[kwordOrigW][0] 1 )))
						(begin (setq  foundwordOrigW  1))
					else
						(begin (setq  kwordOrigW (+  kwordOrigW 1)))
					)
			))
			(if (= foundwordOrigW 0) (setq toInsert 0) )	
		))
		(if (= toInsert 0) (setq retmangledArrJ (remove  retmangledArr[cretmangledArr] retmangledArrJ)))
))
(setq  retmangledArr  retmangledArrJ)
retmangledArr
)


(defun findWndbLoc (wndbwordNum)
	(setq filewndb (fileOpen "C:/RefGuide/wndbwords.txt" 0 0))
	(setq pos (* wndbwordNum 24))
	(fileSeek filewndb pos 1)
	(setq readwndb (fileRead filewndb 24))
	(fileClose filewndb 1)
readwndb
)



(defun findLocWndb(word)
	(setq word (rPad word 24) )
	(setq filewndb (fileOpen "C:/RefGuide/wndbwords.txt" 0 0))
	(setq loc -1)
	(setq left1 0)
	(setq right1 4481)
	(while (and (<= left1 right1) (= loc -1))
		(begin
			(setq mid1 (divi (+ left1 right1) 2))
			(setq pos (* mid1 24))
			(fileSeek filewndb pos 1)
			(setq readWord (fileRead filewndb 24))
			(if (> word readWord)
				(setq left1 (+ mid1 1))
			else 
				(if (< word readWord)
					(setq right1 (- mid1 1))
				else
					(begin
						(setq loc mid1)
					)
				)
			)
		)
	)
	(fileClose filewndb 1)
loc
)

;;;;;;;;;;;;;;;;;;;;;;;;;  morphy functions end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun retPerWordProperty(str)
	(setq wordOrigW (new Vector:))
	(setq locwordOrig (Thesaurus.findLoc str))
	(if (>= locwordOrig 0) (begin (setq wordOrigW  (Thesaurus.retSSTypePolymCountW str )) ))
	(setq  mangledVecW (morphy str))
	(setq retMW (new Vector: ))
	(loop for countmangledVecW from 0 until (length mangledVecW) by 1 do
		(begin
			(setq retSSPolMangledW (Thesaurus.retSSTypePolymCount mangledVecW[countmangledVecW][0]) )
			(loop for countretSSPolMangledW from 0 until (length retSSPolMangledW) by 1 do
				(begin
					(setq countVecW 2)
					(setq foundVecW 0)
					(while (and (< countVecW (length  mangledVecW[countmangledVecW] )) (= foundVecW 0))
						(begin
							(if (= mangledVecW[countmangledVecW][countVecW] retSSPolMangledW[countretSSPolMangledW][0] )
								(begin
									(setq retMW (append retMW (new Vector:1  (append retSSPolMangledW[countretSSPolMangledW]  (new Vector:1 mangledVecW[countmangledVecW][1] )))))
									(setq foundVecW 1)
								)
							else
								(begin
									(setq  countVecW (+ countVecW 1))
							))
					))
			))
	))
	(setq retMW (append retMW wordOrigW))
	(loop for i from 1 until (length retMW) by 1 do
		(setq v retMW[i] )
		(setq j i)
		(while (or (> retMW[(- j 1)][0] v[0]) (and (= retMW[(- j 1)][0] v[0]) (> retMW[(- j 1)][2] v[2]))) 
			(begin
				(setq retMW[j] retMW[(- j 1)])
				(setq j (- j 1))
			)
		(setq retMW[j] v)
		)
	)
	(setq toretMW (purgeDup retMW))
	(setq grMWEntry (new Vector:1 toretMW[0]))
	(setq prevStem toretMW[0][3] )
	(setq grMWVector (new Vector:))
	(loop for itoretMW from  1 until (length toretMW )
		(begin
			(if (= prevStem toretMW[itoretMW][3])
				(begin (setq  grMWEntry (append grMWEntry (new Vector:1 toretMW[itoretMW] )  ))	)
			else
				(begin
					(setq grMWVector (append grMWVector (new Vector:1 grMWEntry)))
					(setq grMWEntry (new Vector:1  toretMW[itoretMW] ))
				)
			)
			(setq prevStem toretMW[itoretMW][3] )
	))
	(setq grMWVector (append grMWVector (new Vector:1 grMWEntry)))
	(setq ssTypePolymVec (mergeSSTypes3and4 grMWVector ))
	(setq ssTypePolymVec (finalSort ssTypePolymVec))
ssTypePolymVec
)

(defun displayAllWordSSType(word)
	(setq  ssTypePolym1 (retPerWordProperty (spaceToUnderScore word)))
	(writeln)
	(if (= ssTypePolym1 #((#void . #(#void )) )) 
		(writeln "Wordnet has no translation for: " word)
	else
		(begin
		(loop for kssTypePolym1 from  0 until (length  ssTypePolym1)
		(begin
			(writeln)
			(writeln "The " (displayssType (car ssTypePolym1[kssTypePolym1])[0]) " " (substitute (car ssTypePolym1[kssTypePolym1])[3] "_" " " ) " has " (car ssTypePolym1[kssTypePolym1])[1] " senses")
			(writeln)
			(setq synIds1 (retSynIdTagCount (cdr ssTypePolym1[kssTypePolym1])[0][2] (cdr ssTypePolym1[kssTypePolym1])[0][0] (cdr ssTypePolym1[kssTypePolym1])[0][1]))
			(loop for cdrCount from  1 until (length (cdr ssTypePolym1[kssTypePolym1]))
				(begin
					(setq carsynid (car synIds1 ))
					(setq cdrsynid (cdr synIds1 ))
					(setq synIdsMore (retSynIdTagCount (cdr ssTypePolym1[kssTypePolym1])[cdrCount][2] (cdr ssTypePolym1[kssTypePolym1])[cdrCount][0] (cdr ssTypePolym1[kssTypePolym1])[cdrCount][1]))
					(setq carsynidsmore (car synIdsMore ))
					(setq cdrsynidsmore (cdr synIdsMore ))
					(setq synIds1 (pair (append carsynid carsynidsmore )  (append cdrsynid cdrsynidsmore ) ))
				)
			)
			(setq idList (car synIds1))
			(setq senseList (cdr synIds1))
			(loop for k2 from 0 until (length idList)
					(begin
							(setq synProperty (retSynProperty idList[k2] ))
							(display (+ k2 1) ". (" senseList[k2] ")(" idList[k2] ")" )
							(displaysynProperty synProperty)
			))
		)
		))
	)
)

(defun retHyperTree(synSetId)
	(setq hyperTree (new Vector:))  ;; vector of pairs
	(if (and (>= synSetId 100001740) (<= synSetId 400522811)) (begin
	(setq numS (Thesaurus.findLocSynSetId synSetId))
	(setq filehyperI (fileOpen "C:/RefGuide/hypernymi.txt" 0 0))
	(setq pos (* numS 12))
	(fileSeek filehyperI pos 1)
	(setq hyperIdx (fileRead filehyperI 12))
	(fileClose filehyperI 1)
	(setq lengthS (integer (trim (left hyperIdx 4) )))
	(setq posit (integer (trim (mid  hyperIdx 4 8) )))
	(setq filehyperC (fileOpen "C:/RefGuide/hypernymc.txt" 0 0))
	(setq pos1 posit)
	(fileSeek filehyperC posit 1)
	(setq readHyperContent (fileRead filehyperC lengthS))
	(fileClose filehyperC  1)
	(setq readHyperContent (trim readHyperContent))
	(if (<> readHyperContent "0")
		(begin
			(setq lengthHyperTree (- (length readHyperContent) 1))
			(setq nhyperTree (integer (/ lengthHyperTree 18)))
			(setq readHyperContent (right readHyperContent lengthHyperTree ))
			(setq parentH (new Vector:))
			(setq childH (new Vector: ))
			(loop for counthyperTree  from 0 until nhyperTree by 1 do
				(begin
					(setq parentH (append parentH (new Vector:1  (left (mid readHyperContent (* counthyperTree 18 ) 18) 9) )))
					(setq childH (append childH (new Vector:1 (right (mid readHyperContent (* counthyperTree 18 ) 18) 9) )))
				)
			)
		)
	)))
(pair parentH childH)
)


(defun displayHyperTree(word senseType)
(writeln)
(writeln "Recursively display hypernym (superordinate) tree for the search string.")
(writeln "Hypernym is the generic term used to designate a whole class of specific instances.  Y is a hypernym of X if X is a (kind of) Y.")
(writeln "Hypernym synsets are preceded by =>, and are indented from the left according to their level in the hierarchy.")
(if (or (= senseType 0) (= senseType 1))
(begin
(setq word (spaceToUnderScore word))
(setq perWordProp (Thesaurus.retPerWordProperty word))
(loop for kperWordProp from 0 until (length perWordProp) by 1 do
	(begin
		(if (= (car perWordProp[kperWordProp])[0] senseType)
			(begin
				(writeln)  ; active
				(writeln (car perWordProp[kperWordProp])[1] " senses of " (car perWordProp[kperWordProp])[3])  ;active
				(setq synIdList (Thesaurus.retSynIdTagCount (car perWordProp[kperWordProp])[2] senseType (car perWordProp[kperWordProp])[1]))
				(loop for ksynIdList from 0 until (length (car synIdList )) by 1 do
					(begin
						(writeln)  ; active
						(writeln "Sense " (+ ksynIdList 1))
						(setq root (car synIdList )[ksynIdList])
						(setq hyperTreeRec (Thesaurus.retHyperTree (integer (car synIdList)[ksynIdList])))
						(setq p (car hyperTreeRec))
						(setq c (cdr hyperTreeRec))
						(setq pcTemp (Thesaurus.sortParent p c) )
						(setq pTemp (car pcTemp))
						(setq cTemp (cdr pcTemp))
						(setq level 0)
						(setq arrowFlag 0)
						(Thesaurus.stackADT.Push root level)
						(while (<> Thesaurus.stackADT.Stack[0][0] 0)
							(begin
								(setq popped (Thesaurus.stackADT.Pop))
								(setq S (rept "   " popped[1]))
								(if (= arrowFlag 0)
									(begin
										(display (append S  "(" popped[0] ")"))  ;; active
										(setq arrowFlag 1)
									)
								else
									(begin
										(display (append S "=>" "(" popped[0] ")"))  ;; active
								))
								(setq synIdPrpty (Thesaurus.retSynProperty popped[0]))
								(Thesaurus.displaysynProperty  synIdPrpty)
								(setq sons (Thesaurus.findChildren (string popped[0]) pTemp cTemp))
								(if (<> (length sons) 0)
									(begin
										(setq level (+ popped[1] 1))
										(loop for nSons from 0 until (length sons) by 1 do
											(begin (Thesaurus.stackADT.Push sons[nSons] level))
										)
									)
								)
						))
					))
				))
		))
)
else
	(writeln "No Hypernym for Sense Type: "  senseType)
))


(defun retHolonymRegular(synId)
	(setq readContent "")
	(setq holoReg (new Vector:))
	(setq synIdNum (Thesaurus.findLocSynSetId synId))
	(if (<>  synIdNum -1)
		(begin
			(setq fileretMer (fileOpen "C:/RefGuide/holoi.txt" 0 0))
			(setq pos (* synIdNum 11))
			(fileSeek fileretMer pos 1)
			(setq offsetLength (fileRead fileretMer 11))
			(fileClose fileretMer 1)
			(setq lengthS (integer (trim (left offsetLength 4) )))
			(setq posit (integer (trim (mid offsetLength 4 7) )))
			(if (<> lengthS 0)
				(begin
					(setq file2retMer (fileOpen "C:/RefGuide/holoc.txt" 0 0))
					(setq pos1 posit)
					(fileSeek file2retMer posit 1)
					(setq readContent (fileRead file2retMer lengthS))
					(fileClose file2retMer 1)
					(setq meronymType (Thesaurus.tokenizer readContent " "))
					(loop for cmeronymType  from 0 until 3 by 1 do
						(begin
							(setq Mero (new Vector:))
							(if (> (length meronymType[cmeronymType]) 1)
								(begin
									(setq readContent1 (right meronymType[cmeronymType] (- (length meronymType[cmeronymType]) 1)))
									(setq nMero (integer (/ (length readContent1) 9)))
									(loop for countMero  from 0 until nMero by 1 do
										(begin
											(setq Mero (append Mero (new Vector:1  (mid readContent1 (* countMero 9 ) 9) )))
									))
							))
							(setq holoReg (append holoReg (new Vector:1 Mero)))
					))
			))
	))
holoReg
)



(defun displayRegularHolonym(word)
	(writeln)
	(writeln "Display all holonyms of the search string.")
	(writeln "A holonym is the name of the whole of which the 'meronym' names a part. Y is a holonym of X if X is a part of Y.")
	(writeln "A meronym is the name of a constituent part, the substance of, or a member of something.  X is a meronym of Y if X is a part of Y.")
	(writeln "Holonym synsets are preceded with either the string MEMBER OF, PART OF or SUBSTANCE OF depending on the specific type of holonym.")
	(setq word (Thesaurus.spaceToUnderScore word))
	(setq ssTypeVec1 (Thesaurus.retPerWordProperty word))
	(setq itemNounVec (new Vector: ))
	(loop for kssTypeVec1 from  0 until (length  ssTypeVec1)
		(begin
			(if (= (car ssTypeVec1[kssTypeVec1])[0] 0)
				(begin
					(setq itemNounVec (append itemNounVec (new Vector:1 (car ssTypeVec1[kssTypeVec1]))))
			))
	))
	(loop for kitemNounVec from  0 until (length itemNounVec)
		(begin
			(setq synIdListVec (Thesaurus.retSynIdTagCount  itemNounVec[kitemNounVec][2]   itemNounVec[kitemNounVec][0]   itemNounVec[kitemNounVec][1]))
			(setq retholoReg2 (new Vector: ))
			(setq cHoloReg2 0)
			(loop for ksynIdListVec from  0 until (length (car synIdListVec))
				(begin
					(setq holoReg2 (Thesaurus.retHolonymRegular (car synIdListVec)[ksynIdListVec]))
					(if (<> holoReg2 #())    
						(begin
							(setq cHoloReg2 (+ cHoloReg2 1))
							(setq synProp2 (Thesaurus.retSynProperty (car synIdListVec)[ksynIdListVec] ))
							(setq retholoReg2 (append retholoReg2 (new Vector:1 (new Vector:4 (+ ksynIdListVec 1) holoReg2 synProp2 (car synIdListVec)[ksynIdListVec]))))
					))
			))
	(if (<> retholoReg2 #())
		(begin
			(writeln)
			(writeln cHoloReg2 " of " (length (car synIdListVec)) " senses of " word )  ;; active
			(loop for kretholoReg2 from  0 until (length retholoReg2)
				(begin
					(writeln)
					(writeln "Sense "  retholoReg2[kretholoReg2][0])
					(display "("  retholoReg2[kretholoReg2][3] ")" )
					(Thesaurus.displaysynProperty retholoReg2[kretholoReg2][2])
					(loop for kholoReg2 from  0 until (length retholoReg2[kretholoReg2][1] )
						(begin
							(case  kholoReg2
								((0) (begin (setq messageOf " MEMBER OF: (") ) )
								((1) (begin	(setq messageOf " SUBSTANCE OF: (" ))	)
								((2) (begin	(setq messageOf " PART OF: (" ))	)
							)
							(loop for kholoReg22 from  0 until (length retholoReg2[kretholoReg2][1][kholoReg2] )
								(begin
									(setq holoRegsynProp2 (Thesaurus.retSynProperty retholoReg2[kretholoReg2][1][kholoReg2][kholoReg22]))
									(display messageOf  retholoReg2[kretholoReg2][1][kholoReg2][kholoReg22]  ")" )
									(Thesaurus.displaysynProperty  holoRegsynProp2)
							))
					))
			))
	))
	))
)



(defun retHolonymInherited(synId)
	(setq readContent "")
	(setq holoReg (new Vector:))
	(setq synIdNum (Thesaurus.findLocSynSetId synId))
	(if (<> synIdNum -1)
		(begin
			(setq fileretMer (fileOpen "C:/RefGuide/holoi.txt" 0 0))
			(setq pos (* synIdNum 11))
			(fileSeek fileretMer pos 1)
			(setq offsetLength (fileRead fileretMer 11))
			(fileClose fileretMer 1)
			(setq lengthS (integer (trim (left offsetLength 4) )))
			(setq posit (integer (trim (mid offsetLength 4 7) )))
			(if (<> lengthS 0)
				(begin
					(setq file2retMer (fileOpen "C:/RefGuide/holoc.txt" 0 0))
					(setq pos1 posit)
					(fileSeek file2retMer posit 1)
					(setq readContent (fileRead file2retMer lengthS))
					(fileClose file2retMer 1)
					(setq meronymType (Thesaurus.tokenizer readContent " "))
					(loop for cmeronymType  from 3 until 6 by 1 do
						(begin
							(setq Mero (new Vector:))
							(if (> (length meronymType[cmeronymType]) 1)
								(begin
									(setq readContent1 (right meronymType[cmeronymType] (- (length meronymType[cmeronymType]) 1)))
									(setq nMero (integer (/ (length readContent1) 9)))
									(loop for countMero  from 0 until nMero by 1 do
										(begin 
											(setq Mero (append Mero (new Vector:1  (mid readContent1 (* countMero 9 ) 9) )))
					
										))
							))
							(setq holoReg (append holoReg (new Vector:1 Mero)))
					))
			))
	))
holoReg
)


(defun displayInheritedHolonym(word)
	(writeln)
	(writeln "Display holonyms for search string tree.  Prints all the holonyms of the search string and all of the holonym's holonyms.")
	(writeln "A holonym is the name of the whole of which the meronym names a part. Y is a holonym of X if X is a part of Y.")
	(writeln "A meronym is the name of a constituent part, the substance of, or a member of something.  X is a meronym of Y if X is a part of Y.")
	(writeln "Holonym synsets are preceded with either the string MEMBER OF, PART OF or SUBSTANCE OF depending on the specific type of holonym.  Synsets are indented from the left according to their level in the hierarchy.")
	(setq word (Thesaurus.spaceToUnderScore word))
	(setq ssTypeVec1 (Thesaurus.retPerWordProperty word))
	(setq itemNounVec (new Vector: ))
	(loop for kssTypeVec1 from  0 until (length  ssTypeVec1)
		(begin
			(if (= (car ssTypeVec1[kssTypeVec1])[0] 0)
				(begin
					(setq itemNounVec (append itemNounVec (new Vector:1 (car ssTypeVec1[kssTypeVec1]))))
			))
	))
	(loop for kitemNounVec from  0 until (length itemNounVec)
		(begin
			(setq synIdListVec (Thesaurus.retSynIdTagCount  itemNounVec[kitemNounVec][2]   itemNounVec[kitemNounVec][0]   itemNounVec[kitemNounVec][1]))
			(setq retholoReg1 (new Vector: ))
			(setq cHoloReg1 0)
			(loop for ksynIdListVec from  0 until (length (car synIdListVec))
				(begin
					(setq holoReg1 (Thesaurus.retHolonymInherited (car synIdListVec)[ksynIdListVec]))
					(if (<> holoReg1 #())    
						(begin
							(setq cHoloReg1 (+ cHoloReg1 1))
							(setq synProp1 (Thesaurus.retSynProperty (car synIdListVec)[ksynIdListVec] ))
							(setq retholoReg1 (append retholoReg1 (new Vector:1 (new Vector:4 (+ ksynIdListVec 1) holoReg1 synProp1 (car synIdListVec)[ksynIdListVec]))))
					))
			))
	(if (<> retholoReg1 #())
		(begin
	(writeln)
	(writeln cHoloReg1 " of " (length (car synIdListVec)) " senses of " word )  ;; active

	(loop for kretholoReg1 from  0 until (length retholoReg1)
		(begin
			(writeln)
			(writeln "Sense "  retholoReg1[kretholoReg1][0])
			(loop for kholoReg1 from  0 until (length retholoReg1[kretholoReg1][1] )
				(begin
					(case  kholoReg1
						((0) (begin (setq messageOf " MEMBER OF: (") ) )
						((1) (begin	(setq messageOf " SUBSTANCE OF: (" ))	)
						((2) (begin	(setq messageOf " PART OF: (" ))	)
					)
					(if (<>  retholoReg1[kretholoReg1][1][kholoReg1] #())
					(begin
					(setq p (new Vector:))
					(setq c (new Vector:))
					(loop for kholoReg11 from  0 until (length retholoReg1[kretholoReg1][1][kholoReg1] )
						(begin
							(if (= (imod kholoReg11 2 ) 0)
								(setq p (append p (new Vector:1 retholoReg1[kretholoReg1][1][kholoReg1][kholoReg11])))
							else
								(setq c (append c (new Vector:1 retholoReg1[kretholoReg1][1][kholoReg1][kholoReg11])))
							)
								

					))
					(setq pcTemp (Thesaurus.sortParent p c) )
					(setq pTemp (car pcTemp))
					(setq cTemp (cdr pcTemp))
					(setq level 0)
					(setq arrowFlag 0)
					(setq root retholoReg1[kretholoReg1][3] )
					(Thesaurus.stackADT.Push root level)
						(while (<> Thesaurus.stackADT.Stack[0][0] 0)
							(begin
								(setq popped (Thesaurus.stackADT.Pop))
								(setq S (rept "   " popped[1]))
								(if (= arrowFlag 0)
									(begin
										(display (append S  "(" popped[0] ")"))  ;; active
										(setq arrowFlag 1)
									)
								else
									(begin
										(display (append S messageOf "(" popped[0] ")"))  ;; active
								))
								(setq synIdPrpty (Thesaurus.retSynProperty popped[0]))
								(Thesaurus.displaysynProperty  synIdPrpty)
								(setq sons (Thesaurus.findChildren (string popped[0]) pTemp cTemp))
								(if (<> (length sons) 0)
									(begin
										(setq level (+ popped[1] 1))
										(loop for nSons from 0 until (length sons) by 1 do
											(begin (Thesaurus.stackADT.Push sons[nSons] level))
										)
									)
								)
						))

					))
			))
		))
	))
))
)




(defun retDomain(synId)
	(setq readContent "")
	(setq Mero1 (new Vector:))
	(setq synIdNum (Thesaurus.findLocSynSetId synId))
	(if (<> synIdNum -1)
		(begin
		(setq fileretMer (fileOpen "C:/RefGuide/holoi.txt" 0 0))
		(setq pos (* synIdNum 11))
		(fileSeek fileretMer pos 1)
		(setq offsetLength (fileRead fileretMer 11))
		(fileClose fileretMer 1)
		(setq lengthS (integer (trim (left offsetLength 4) )))
		(setq posit (integer (trim (mid offsetLength 4 7) )))
		(if (<> lengthS 0)
			(begin
			(setq file2retMer (fileOpen "C:/RefGuide/holoc.txt" 0 0))
			(setq pos1 posit)
			(fileSeek file2retMer posit 1)
			(setq readContent (fileRead file2retMer lengthS))
			(fileClose file2retMer 1)
			(setq meronymType (Thesaurus.tokenizer readContent " "))
			(if (and (<>  meronymType[6] "d") (<> (length meronymType[6]) 0))
				(begin	
					(setq readContent1 (right meronymType[6] (- (length meronymType[6]) 1)))
					(setq nMero (integer (/ (length readContent1) 10)))
					(loop for countMero  from 0 until nMero by 1 do
						(begin
							(setq Mero1 (append Mero1 (new Vector:1 (pair (mid readContent1 (* countMero 10 ) 9) (mid readContent1 (+ (* countMero 10 ) 9) 1)))))
						))
			))
		))
	))
Mero1
)


(defun displayDomain(word ssType)
	(writeln)
	(writeln "Display domain to which this synset belongs.")
	(writeln "Each domain synset is preceeded by CATEGORY, REGION, or USAGE to distinguish topical, geographic and functional classifications, and it's part of speech.  Each word is followed by its sense number.")
	(setq word (Thesaurus.spaceToUnderScore word))
	(setq ssTypeVec1 (Thesaurus.retPerWordProperty word))
	(setq itemVec (new Vector: ))
	(loop for kssTypeVec1 from  0 until (length  ssTypeVec1)
		(begin
			(if (= (car ssTypeVec1[kssTypeVec1])[0] ssType)
				(begin
					(setq itemVec (append itemVec (new Vector:1 (car ssTypeVec1[kssTypeVec1]))))
			))
	))
	(loop for kitemVec from  0 until (length itemVec)
		(begin
			(setq synIdListVec (Thesaurus.retSynIdTagCount  itemVec[kitemVec][2]   itemVec[kitemVec][0]   itemVec[kitemVec][1]))
			(setq retholoReg2 (new Vector: ))
			(setq cHoloReg2 0)
			(loop for ksynIdListVec from  0 until (length (car synIdListVec))
				(begin
					(setq domain (Thesaurus.retDomain (car synIdListVec)[ksynIdListVec]))
					;(writeln "(car synIdListVec)[ksynIdListVec]=" (car synIdListVec)[ksynIdListVec]    )
					;(writeln "domain=" domain)
					(if (<> domain #())    
						(begin
							(setq cHoloReg2 (+ cHoloReg2 1))
							(setq synProp2 (Thesaurus.retSynProperty (car synIdListVec)[ksynIdListVec] ))
							(setq retholoReg2 (append retholoReg2 (new Vector:1 (new Vector:4 (+ ksynIdListVec 1) domain synProp2 (car synIdListVec)[ksynIdListVec]))))
					))
			))
			(if (<> retholoReg2 #())
				(begin
					(writeln)
					(writeln cHoloReg2 " of " (length (car synIdListVec)) " senses of " word )  ;; active
					(loop for kretholoReg2 from  0 until (length retholoReg2)
						(begin
							(writeln)
							(writeln "Sense "  retholoReg2[kretholoReg2][0])
							(display "("  retholoReg2[kretholoReg2][3] ")" )
							(Thesaurus.displaysynProperty retholoReg2[kretholoReg2][2])
							(loop for kholoReg2 from  0 until (length retholoReg2[kretholoReg2][1] )
								(begin
									(case  (cdr retholoReg2[kretholoReg2][1][kholoReg2])
										(("t") (begin (setq message "CATEGORY") ) )
										(("r") (begin	(setq message "REGION" )))
										(("u") (begin	(setq message "USAGE" ))))
										(setq holoRegsynProp2 (Thesaurus.retSynProperty (car retholoReg2[kretholoReg2][1][kholoReg2])))
										(display "   " message "->" (Thesaurus.lexGroupSSType (integer holoRegsynProp2[0])) "(" (car retholoReg2[kretholoReg2][1][kholoReg2]) ")" )
										(Thesaurus.displaysynPropertyShort holoRegsynProp2)
								))
					))
			))
		))
)


(defun retDomainTerm(synId)
	(setq readContent "")
	(setq Mero1 (new Vector:))
	(setq synIdNum (Thesaurus.findLocSynSetId synId))
	(if (<> synIdNum -1)
		(begin
		(setq fileretMer (fileOpen "C:/RefGuide/holoi.txt" 0 0))
		(setq pos (* synIdNum 11))
		(fileSeek fileretMer pos 1)
		(setq offsetLength (fileRead fileretMer 11))
		(fileClose fileretMer 1)
		(setq lengthS (integer (trim (left offsetLength 4) )))
		(setq posit (integer (trim (mid offsetLength 4 7) )))
		(if (<> lengthS 0)
			(begin
			(setq file2retMer (fileOpen "C:/RefGuide/holoc.txt" 0 0))
			(setq pos1 posit)
			(fileSeek file2retMer posit 1)
			(setq readContent (fileRead file2retMer lengthS))
			(fileClose file2retMer 1)
			(setq meronymType (Thesaurus.tokenizer readContent " "))
			(if (and (<>  meronymType[7] "o") (<> (length meronymType[7]) 0))
				(begin	
					(setq readContent1 (right meronymType[7] (- (length meronymType[7]) 1)))
					(setq nMero (integer (/ (length readContent1) 10)))
					(loop for countMero  from 0 until nMero by 1 do
						(begin
							(setq Mero1 (append Mero1 (new Vector:1 (pair (mid readContent1 (* countMero 10 ) 9) (mid readContent1 (+ (* countMero 10 ) 9) 1)))))
						))
			))
		))
	))
Mero1
)


(defun displayDomainTerm(word ssType)
	(writeln)
	(writeln "Display all synsets belonging to the domain.")
	(writeln "Each domain term synset is preceeded by CATEGORY TERM, REGION TERM, or USAGE TERM to distinguish topical, geographic and functional classes, and its part of speech. Each word is followed by its sense number.")
	(setq word (Thesaurus.spaceToUnderScore word))
	(setq ssTypeVec1 (Thesaurus.retPerWordProperty word))
	(setq itemVec (new Vector: ))
	(loop for kssTypeVec1 from  0 until (length  ssTypeVec1)
		(begin
			(if (= (car ssTypeVec1[kssTypeVec1])[0] ssType)
				(begin
					(setq itemVec (append itemVec (new Vector:1 (car ssTypeVec1[kssTypeVec1]))))
			))
	))
	(loop for kitemVec from  0 until (length itemVec)
		(begin
			(setq synIdListVec (Thesaurus.retSynIdTagCount  itemVec[kitemVec][2]   itemVec[kitemVec][0]   itemVec[kitemVec][1]))
			(setq retholoReg2 (new Vector: ))
			(setq cHoloReg2 0)
			(loop for ksynIdListVec from  0 until (length (car synIdListVec))
				(begin
					(setq domain (Thesaurus.retDomainTerm (car synIdListVec)[ksynIdListVec]))
					(if (<> domain #())    
						(begin
							(setq cHoloReg2 (+ cHoloReg2 1))
							(setq synProp2 (Thesaurus.retSynProperty (car synIdListVec)[ksynIdListVec] ))
							(setq retholoReg2 (append retholoReg2 (new Vector:1 (new Vector:4 (+ ksynIdListVec 1) domain synProp2 (car synIdListVec)[ksynIdListVec] ))))
						))
				))
			(if (<> retholoReg2 #())
				(begin
					(writeln)
					(writeln cHoloReg2 " of " (length (car synIdListVec)) " senses of " word )  ;; active
					(loop for kretholoReg2 from  0 until (length retholoReg2)
						(begin
							(writeln)
							(writeln "Sense "  retholoReg2[kretholoReg2][0])
							(display "("  retholoReg2[kretholoReg2][3] ")" )
							(Thesaurus.displaysynProperty retholoReg2[kretholoReg2][2])
							(loop for kholoReg2 from  0 until (length retholoReg2[kretholoReg2][1] )
								(begin
									(case  (cdr retholoReg2[kretholoReg2][1][kholoReg2])
										(("t") (begin (setq message "CATEGORY TERM") ) )
										(("r") (begin	(setq message "REGION TERM" )))
										(("u") (begin	(setq message "USAGE TERM" )))
									)
									(setq holoRegsynProp2 (Thesaurus.retSynProperty (car retholoReg2[kretholoReg2][1][kholoReg2])))
									(display "   " message "->" (Thesaurus.lexGroupSSType (integer holoRegsynProp2[0])) "(" (car retholoReg2[kretholoReg2][1][kholoReg2]) ")" )
									(Thesaurus.displaysynPropertyShort holoRegsynProp2)
							))
					))
			))
		))
)



;; utility functions
(defun tokenizer(str, delimiter)
	(setq tokenArray (new Vector: ))
	(setq token "")
	(loop for strCount from 0 until (length str) by 1 do
		(begin
			(setq sym str[strCount])
			(if (<> (string sym) delimiter) 
				(begin
					(setq token(append token sym))
				)
			else
				(begin
					(setq tokenArray (append tokenArray (new Vector:1 token )))
					(setq token "")
				)
			)
		)
	)
(setq tokenArray (append tokenArray (new Vector:1 token )))
tokenArray
)



(defun extractTagNum(sufStr)
	(setq tagNum (tokenizer sufStr " "))
tagNum
)


(defun extractsynIds(synStr, polysemyCount)
	(setq synIds (new Vector: ))
	(loop for k from 0 until polysemyCount by 1 do
		(begin
			(setq synIds (append synIds (new Vector:1 (mid synStr (* k 9) 9))))
		)
	)
synIds
)


(defun lexGrouping(str)
	(case  (string str)
		  (("00") (setq nam "adj.all"))
          (("01") (setq nam "adj.pert"))
          (("02") (setq nam "adv.all"))
          (("03") (setq nam "noun.Tops"))
          (("04") (setq nam "noun.act"))
          (("05") (setq nam "noun.animal"))
          (("06") (setq nam "noun.artifact"))
          (("07") (setq nam "noun.attribute"))
          (("08") (setq nam "noun.body"))
          (("09") (setq nam "noun.cognition"))
          (("10") (setq nam "noun.communication"))
          (("11") (setq nam "noun.event"))
          (("12") (setq nam "noun.feeling"))
          (("13") (setq nam "noun.food"))
          (("14") (setq nam "noun.group"))
          (("15") (setq nam "noun.location"))
          (("16") (setq nam "noun.motive"))
          (("17") (setq nam "noun.object"))
          (("18") (setq nam "noun.person"))
          (("19") (setq nam "noun.phenomenon"))
          (("20") (setq nam "noun.plant"))
          (("21") (setq nam "noun.possession"))
          (("22") (setq nam "noun.process"))
          (("23") (setq nam "noun.quantity"))
          (("24") (setq nam "noun.relation"))
          (("25") (setq nam "noun.shape"))
          (("26") (setq nam "noun.state"))
          (("27") (setq nam "noun.substance"))
          (("28") (setq nam "noun.time"))
          (("29") (setq nam "verb.body"))
          (("30") (setq nam "verb.change"))
          (("31") (setq nam "verb.cognition"))
          (("32") (setq nam "verb.communication"))
          (("33") (setq nam "verb.competition"))
          (("34") (setq nam "verb.consumption"))
          (("35") (setq nam "verb.contact"))
          (("36") (setq nam "verb.creation"))
          (("37") (setq nam "verb.emotion"))
          (("38") (setq nam "verb.motion"))
          (("39") (setq nam "verb.perception"))
          (("40") (setq nam "verb.possession"))
          (("41") (setq nam "verb.social"))
          (("42") (setq nam "verb.stative"))
          (("43") (setq nam "verb.weather"))
          (("44") (setq nam "adj.ppl"))
	(else (begin (setq nam "unknown" ) ))
	)
nam 
)


(defun lexGroupSSType(lexNum)
	(case lexNum
		  ((00 01 44) (setq nam "adjective"))
          ((02) (setq nam "adverb"))
          ((03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28) (setq nam "noun"))
          ((29 30 31 32 33 34 35 36 37 38 39 40 41 42 43) (setq nam "verb"))
   (else (begin (setq nam "unknown" ) ))
	)
nam 
)


(defun displaysynProperty(synProperty1)
	(setq nam (lexGrouping synProperty1[0])) 
	(display " <" nam "> " )
	(display (underscoreToSpace synProperty1[2][0]) "#" synProperty1[1][0])
	(loop for synProp2 from 1 until (length synProperty1[2])
		(begin
			(display "," (underscoreToSpace synProperty1[2][synProp2]) "#" synProperty1[1][synProp2])
		)
	)
	(display "--(" synProperty1[3] ")")
	(writeln)
)


(defun displaysynPropertyShort(synProperty1)
	(setq nam (lexGrouping synProperty1[0])) 
	(display " <" nam "> " )
	(display (underscoreToSpace synProperty1[2][0]) "#" synProperty1[1][0])
	(loop for synProp2 from 1 until (length synProperty1[2])
		(begin
			(display "," (underscoreToSpace synProperty1[2][synProp2]) "#" synProperty1[1][synProp2])
		)
	)
	(writeln)
)


(defun displayssType(ssTypeCode)
	(case ssTypeCode
			((0) (setq ssType "noun")	)
			((1) (setq ssType "verb")	)
			((2) (setq ssType "adv")	)
			((3) (setq ssType "adj")	)
			((4) (setq ssType "adj")	)
	)
ssType
)


(defun mergeSSTypes3and4(grMWVector)
(setq ssTypePolymVec (new Vector: ))
(loop for igrMWVector from  0 until (length grMWVector)
		(begin
			(setq flagAdj 0)
			(setq senseAccum 0)
			(loop for igrMWVectorRec from  0 until (length  grMWVector[igrMWVector] )
				(begin
					(if (or (= grMWVector[igrMWVector][igrMWVectorRec][0] 3) (= grMWVector[igrMWVector][igrMWVectorRec][0] 4)) 
						(begin
							(setq flagAdj (+ flagAdj 1))
							(setq senseAccum (+ senseAccum grMWVector[igrMWVector][igrMWVectorRec][1]))
					))
			))
			(loop for k22 from 0 until (length grMWVector[igrMWVector])
				(begin
					(if (<= grMWVector[igrMWVector][k22][0] 2)
						(begin
							(setq ssTypePolymVec (append ssTypePolymVec (new Vector:1 (pair grMWVector[igrMWVector][k22] (new Vector:1 grMWVector[igrMWVector][k22] )))))
					))
					(if (and (= flagAdj 1) (or (= grMWVector[igrMWVector][k22][0] 3) (= grMWVector[igrMWVector][k22][0] 4)))
						(begin
							(setq ssTypePolymVec (append ssTypePolymVec (new Vector:1 (pair grMWVector[igrMWVector][k22] (new Vector:1 grMWVector[igrMWVector][k22] )))))
					))
			))
			(if (= flagAdj 2)
				(begin
					(setq adjRec (new Vector: ))
					(loop for k33 from 0 until (length grMWVector[igrMWVector])
						(begin
							(if (or (= grMWVector[igrMWVector][k33][0] 3) (= grMWVector[igrMWVector][k33][0] 4))
								(setq adjRec (append adjRec (new Vector:1  grMWVector[igrMWVector][k33])))
					)))
					(setq ssTypePolymVec (append ssTypePolymVec (new Vector:1 (pair (new Vector:4 3 senseAccum   grMWVector[igrMWVector][0][2] grMWVector[igrMWVector][0][3]) adjRec  ))))
			))
))
ssTypePolymVec 
)

(defun finalSort(ssTypePolymVec)
	(loop for j from 0 until (length ssTypePolymVec) by 1 do
		(if (= (car ssTypePolymVec[j])[0] 2)
			(setq (car ssTypePolymVec[j])[0] (+ (car ssTypePolymVec[j])[0] 10) )
		)
	)
	(loop for i from 0 until (- (length ssTypePolymVec) 1) by 1 do
		(begin
			(setq min i )
			(loop for j from (+ i 1) until (length ssTypePolymVec) by 1 do
				(if (or (< (car ssTypePolymVec[j])[0] (car ssTypePolymVec[min])[0]) (and (= (car ssTypePolymVec[j])[0] (car ssTypePolymVec[min])[0]) (< (car ssTypePolymVec[j])[3] (car ssTypePolymVec[min])[3]))) then (setq min j))
			)
			(setq t ssTypePolymVec[i]) 
			(setq ssTypePolymVec[i] ssTypePolymVec[min])
			(setq ssTypePolymVec[min] t)
	))
	(loop for j from 0 until (length ssTypePolymVec) by 1 do
		(if (= (car ssTypePolymVec[j])[0] 12)
			(setq (car ssTypePolymVec[j])[0] (- (car ssTypePolymVec[j])[0] 10) )
		)
	)
ssTypePolymVec
)


(defun purgeDup(retMW)
	(setq toretMW retMW)
	(setq prevMW retMW[0])
	(setq rootW (new Vector:))
	(loop for jretMW from 1 until (length retMW) by 1 do
		(begin
			(if (and (= retMW[jretMW][0] prevMW[0]) (= retMW[jretMW][1] prevMW[1]))
				(begin
					(if (isMember prevMW[3] rootW)
						(begin 	(setq toretMW (remove  prevMW toretMW)) )
					else
						(begin (setq toretMW (remove  retMW[jretMW] toretMW)))
					)
				)
			else
				(begin (setq rootW (append rootW (new Vector:1 retMW[jretMW][3] ))))
			)
		(setq prevMW retMW[jretMW])
	))
	(loop for i from 1 until (length toretMW) by 1 do
		(setq v toretMW[i] )
		(setq j i)
		(while (or (> toretMW[(- j 1)][3] v[3]) (and (= toretMW[(- j 1)][3] v[3]) (> toretMW[(- j 1)][0] v[0]))) 
			(begin
				(setq toretMW[j] toretMW[(- j 1)])
				(setq j (- j 1))
			)
		(setq toretMW[j] v)
		)
	)
toretMW
)


(defun rPad(wordToPad lengthString)
	(setq S " ")
	(setq S (rept S (- lengthString (length wordToPad))))
	(setq wordToPad (append wordToPad S))
wordToPad
)

(defun spaceToUnderScore(str)
	(setq str (trim str))
	(setq tok (Thesaurus.tokenizer str " "))
	(setq strRet tok[0])
	(loop for jtok from 1 until (length tok) by 1 do
		(begin
			(setq strRet (append strRet "_" tok[jtok]))


		)
	)
strRet
)


(defun underscoreToSpace(str)
	(setq str (trim str))
	(setq tok (Thesaurus.tokenizer str "_"))
	(setq strRet tok[0])
	(loop for jtok from 1 until (length tok) by 1 do
		(begin
			(setq strRet (append strRet " " tok[jtok]))
		)
	)
strRet
)

(defun sortParent(p c)
	(loop for i from 1 until (length p) by 1 do
		(begin
			(setq v1 p[i] ) (setq v2 c[i] )
			(setq j i)
			(while (> p[(- j 1)] v1)
				(begin
					(setq p[j] p[(- j 1)]) (setq c[j] c[(- j 1)])
					(setq j (- j 1))
				)
				(setq p[j] v1) (setq c[j] v2)
			)
		)
	)
(pair p c)
)


(defun removeAtLoc(p loc)
	(setq pOut (new Vector: ))
	(setq kP -1)
	(loop for countP from 0 until (length p) by 1 do
		(if (<> loc countP) 
			(begin
				(++ kP)
				(insert pOut kP p[countP])
			)
		)
	)
pOut
)


(defun findChildren(parent p c)
	(setq children (new Vector: ))
	(setq pTemp p)
	(setq cTemp c)
	(setq pTemp1 pTemp)
	(setq cTemp1 cTemp)
	(setq loc (binarySearch pTemp1 parent)) 
	(while (isNumber loc)
		(begin
			(if (= (isMember cTemp[loc] children) false)
				(begin
					(setq children (insert children 0 cTemp1[loc]))
					(setq pTemp1 (Thesaurus.removeAtLoc pTemp1 loc)) 
					(setq cTemp1 (Thesaurus.removeAtLoc cTemp1 loc)) 
					(setq pTemp (Thesaurus.removeAtLoc pTemp loc)) 
					(setq cTemp (Thesaurus.removeAtLoc cTemp loc)) 
					(setq loc (binarySearch pTemp1 parent ))
				)
			else
				(begin
					(setq pTemp1 (Thesaurus.removeAtLoc pTemp1 loc)) 
					(setq cTemp1 (Thesaurus.removeAtLoc cTemp1 loc)) 
					(setq loc (binarySearch pTemp1 parent ))

				)
			) 
		)
	)
children
)


(defun stackADT()
	pvars:(
			(Vector: Stack #(#() #()))
			(Vector: popReturn #())
			Push
			Pop
		 )

(defun Push(item1 item2)
	(setq Stack[0][0] (+ Stack[0][0] 1 ))
	(setq Stack[0] (insert Stack[0] Stack[0][0] item1))
	(setq Stack[1][0] (+ Stack[1][0] 1 ))
	(setq Stack[1] (insert Stack[1] Stack[1][0] item2))
Stack
)

(defun Pop()
	(setq loc1 Stack[0][0])
	(setq popReturn1 Stack[0][loc1])  
	(setq Stack[0] (Thesaurus.removeAtLoc Stack[0] loc1))
	(setq Stack[0][0] (- Stack[0][0] 1 ))

	(setq loc2 Stack[1][0])
	(setq popReturn2 Stack[1][loc2])  
	(setq Stack[1] (Thesaurus.removeAtLoc Stack[1] loc2))
	(setq Stack[1][0] (- Stack[1][0] 1 ))
	(setq popReturn (new Vector:2 popReturn1 popReturn2))
popReturn
)

)

)

(deforphan Thesaurus:selfTest()
;**************************************************
;******** selfTest of Thesaurus Lambda *****************
;**************************************************


(writeln "********* Steps to test Thesaurus Lambda at the console****************")
(writeln "********* These sequence of commands can pasted and executed at the console*****")

;; TEST

(writeln (Thesaurus.findLoc "takeover"))
(writeln (Thesaurus.findWord 82587))
(writeln (Thesaurus.findLoc "linear_programming"))
(writeln (Thesaurus.findWord 122245))
(writeln (Thesaurus.findWord 1279129999))
(writeln (Thesaurus.findLoc "xxxs"))
(writeln (Thesaurus.determineDictType 82587))
(writeln (Thesaurus.determineDictType 122245))
(writeln (Thesaurus.findInterval1 "takeover"))
(writeln (Thesaurus.findInterval2 "linear_programming"))
(writeln (Thesaurus.findSynsetIdLoc 100))
(writeln (Thesaurus.findLocSynSetId 100042855))
(writeln (Thesaurus.findSSTypeLoc 74644))
(writeln (Thesaurus.findSSTypeLoc 82587))
(writeln (Thesaurus.displaySSTypePolymCount "set"))
(writeln (Thesaurus.displaySSTypePolymCount "takeover"))
(writeln (Thesaurus.retSSTypePolymCount 74644))
(writeln (Thesaurus.retSSTypePolymCountW "set"))
(writeln (Thesaurus.retSynIdTagCountNoun 74644 13))
(writeln (Thesaurus.retSynIdTagCount 74644 0 13))
(writeln (Thesaurus.retSynIdTagCount 74644 1 24))
(writeln (Thesaurus.retSynIdTagCount 16792 2 4))
(writeln (Thesaurus.retSynIdTagCount 56977 3 1))
(writeln (Thesaurus.retSynIdTagCount 26797 4 2))
(writeln (Thesaurus.retSynProperty 107452170))
(writeln (Thesaurus.morphy "better")) 
(writeln (Thesaurus.stemMorph "better"))
(writeln (Thesaurus.stemMorph "axes"))  
(writeln (Thesaurus.retWndb "better")) 
(writeln (Thesaurus.retWndbW "better")) 
(writeln (Thesaurus.findWndbLoc 297))
(writeln (Thesaurus.findLocWndb "better"))
(writeln (Thesaurus.retPerWordProperty "arranged")) 
(writeln (Thesaurus.retPerWordProperty "better"))
(Thesaurus.displayAllWordSSType "takeover")
(Thesaurus.displayAllWordSSType "arranged")
(Thesaurus.displayAllWordSSType "java")
(writeln (Thesaurus.retHyperTree 108357098))
(Thesaurus.displayHyperTree "java" 0)
(Thesaurus.displayHyperTree "better" 0)
(Thesaurus.displayHyperTree "better" 1)
(writeln (Thesaurus.retHolonymRegular 106409151))
(Thesaurus.displayRegularHolonym "letter")
(writeln (Thesaurus.retHolonymInherited 106409151))
(Thesaurus.displayInheritedHolonym "letter")
(writeln (Thesaurus.retDomain 106129345))
(Thesaurus.displayDomain "set" 0)
(Thesaurus.displayDomain "set" 1)
(Thesaurus.displayDomain "men" 0)
(writeln (Thesaurus.retDomainTerm 108312482))
(Thesaurus.displayDomainTerm "math" 0)
(Thesaurus.displayDomainTerm "law" 0)
(writeln (Thesaurus.tokenizer "abc@def@g@hij" "@"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(Thesaurus.displayAllWordSSType "takeover")
(Thesaurus.displayAllWordSSType "investment")
(Thesaurus.displayAllWordSSType "hedge funds")
(Thesaurus.displayHyperTree "hedge funds" 0)
(Thesaurus.displayHyperTree "Dow Jones" 0)
(Thesaurus.displayHyperTree "flotation" 0)
(Thesaurus.displayRegularHolonym "stocks")
(Thesaurus.displayInheritedHolonym "stocks")
(Thesaurus.displayDomain "dollars" 0)
(Thesaurus.displayDomainTerm "stock market" 0)























true)




	
			

