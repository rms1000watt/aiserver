;;*************************************
;;*************************************
;; Exported Lambda File Cabinet Document
;;*************************************
;;*************************************

;;**EXPORTKEY**:invertIndex
 
(defun invertIndex()

;;************************************************************************
;; September 19, 2006 1:30
;;
;; invertIndex Lambda
;;
;;	
;;
;;For a more refined search, the following should be done:
;;	1. Return unstemmed words for highlighting at the display
;;	2. Utilize Wordnet Data such as Polysemy count, Part of Speech, others....
;;	3. Put priority weights on the words extracted from some XML tags. Fo example, the
;;		 words inside the <Title> </Title> tags should have higher weights than those inside 
;;		 <Description> </Description> tags
;;
;;		TO BE DONE LATER......
;;
;;***************************************************************************
;;
;; An implementation of Vector Space Search Engine
;;
;; At the bottommost of this program, you can copy test commands and execute them at the console 
;;
;; WordWeight is a based on frequency 
;;
;; Each word is represented by an axis in the vector space.
;; Each document is treated as a vector, where each coordinate is represented by the frequency of a word
;;
;; For example:
;;		Suppose the universal U set of documents consists only of 3 words: "cat", "dog", "mouse"
;;		Therefore, U is represented as a 3-tuple ("cat", "dog", "mouse"); each word is an axis in the vector space
;;		Vector (3,1,4) for a document X, will mean that at document X "cat" has a frequency of 3; "dog" has frequency of 1; "mouse" has frequency of 4.
;;		
;; The magnitude for each vector is computed using Phytagorean theorem.
;; If a search word is an element of U, then it can be represented as a unit vector. 
;;		For example, the search word "cat" will be represented as (1,0,0)
;;
;; The cosine of the angles formed by the search word unit vector and each of the document vectors are compared. 
;; The smaller the cosine of the angle, the more prioritized is the document that will be returned during a search.
;;
;;************************************************************************


pvars:(
	initDocuments	  			; enter the sample XML documents here for testing
								; this will save documents to a repository DocumentsForInvert.db 
								; at the console, the index tree is displayed
;; *******************************************************************************************
;;
	
;; these are in the main body of invertIndex
	prepareDocuments 		
		; remove the XML tags; stripped out the junk words; get the stems of the non-junk words; get the frequency for each word and compute the magnitude of each document.
		; this will save to a repository readyDocumentTable.db
		; Sample Output: #(#(#("coffee" 1 ) #("invest" 4 ) #("investor" 1 ) #("java" 1 ) #("love" 1 ) ) 8 4.472135955 ) #(2 )
		; This sample output means: frequency of "coffee" is 1; frequency of "invest" is 4;...; total count of stems is 8; Magnitude is  4.472135955; the document number is 2.

	constructInvertIndexTable
		; get the inverted indices of the documents
		; this will save inverted indices to a repository weightedInvertIndexTable.db
		; Sample Output: java #(#(0.22360679775 2 ) #(0.6172133998484 1 ) )
		; This sample output means: the cosine of the angle of word "java" at document 2 is 0.22360679775

	searchDocs				
		; return the filenames for a given search string
		; stripped out the junk words; get the stems of the non-junk words; duplicate words are purged
		
	searchDocsWithStemWords
		; return the filenames and their corresponding stemmed words for a given search string
		; stripped out the junk words; get the stems of the non-junk words; duplicate words are purged

	getStem						
		; invoke the PorterStemmer

	removeJunkwords
		; remove the junk words for a given string
		; List of junk words are prepared in  "junkWords.db". A separate function is used to create this. 
		; There are 583 junk words.

;; these are utility functions which are also in the main body of invertIndex
	notjunkWord
	conjunction

	sortToPrepareGetFrequency
	getFrequency1

	assignWeightsPerPage
		; invokes getStem and getFrequency1
		;(invertIndex.assignWeightsPerPage "java loves java" 1)
		;returns: #(1 #(#(#("java" 2 ) #("love" 1 ) ) 3 2.2360679775 ) )

	getFrequency

	sortWeightedPages			
		; insertion sort - sort according to ascending weights
		; for example:
		;	(setq V (new Vector: 4 #(0.152380952381 2 ) #(0.2285714285714 1 ) #(0.4571428571429 4 ) #(0.5485714285714 3 ) ))
		;	(invertIndex.sortWeightedPages V) will sort V according to ascending V[j][0]

	sortWeightedPages2			
		; insertion sort - sort according to ascending page numbers
		; for example:
		;	(setq V (new Vector: 4 #(0.152380952381 2 ) #(0.2285714285714 1 ) #(0.4571428571429 4 ) #(0.5485714285714 3 ) ))
		;	(invertIndex.sortWeightedPages V) will sort V according to ascending V[j][1]


	sortWeightedPages1
		 ;selection sort - decreasing weights

	constructJunkWordIndex
		; 583 junk words are hardcoded in this function, where they are written to "junkWords.db" repository. 
			
	prepareInvertIndexDB

	removeXMLTags
		; remove the XML tags; remove unwanted characters ; change all letters to lower caps

	initDocsForMatch	
		; not currently utilized		

	)


;**************************************************
;************** main body of invertIndex ***********
;**************************************************
(defun initDocuments()
	 vars:(repos documindex i entry ontologyNames myDocsName T)
	(setq repos (new ObjectRepository: "DocumentsForInvert.db"))
	(clear repos)   
	(setq documindex (new index documindex: repos create: memory:))
	(writeln "initDocs: " )
	;;*****************************************************************************************************************
	
	(browseLib.setFocus "CoreContent")
	(setq ontologyNames (^new Vector: 0))
	(setq ontologyNames (browseLib.getChildNames))
	(setq N (length ontologyNames))
    (setq myDocsName (^new Vector: 0))
	(setq T (^new Vector: 0))
	(loop for n from 0 until N do
         ;;(setq myDocsName[(length myDocsName)] ontologyNames[n])
		 (setq T (new Vector: 1 (string (browseLib.checkout "CoreContent" ontologyNames[n]))))
		 (setq documindex[T] ontologyNames[n])
    ) ; end loop
	(documindex.Pv.save)
	(writeln)
	(writeln "--------------------------------------------------------")
	(writeln "Sample documents are saved at DocumentsForInvert.db")
	;(documindex.Pv.__showTree)
)

(defun initDocsForMatch()
	 vars:(repos docIndxMatch i entry ontologyNamesMatch myDocsNameMatch T)
	(setq repos (new ObjectRepository: "DocumentsForMatching.db"))
	(clear repos)   
	(setq docIndxMatch (new index docIndxMatch: repos create: memory:))
	(browseLib.setFocus "CoreContent")
	(setq ontologyNamesMatch (^new Vector: 0))
	(setq ontologyNamesMatch (browseLib.getChildNames))
	(setq N (length ontologyNamesMatch))
   (setq myDocsNameMatch (^new Vector: 0))
	(setq T (^new Vector: 0))
	(loop for n from 0 until N do
        (setq myDocsNameMatch[(length myDocsNameMatch)] ontologyNamesMatch[n])
		(setq T (string (browseLib.checkout "CoreContent" ontologyNamesMatch[n])))
		(setq docIndxMatch[ontologyNamesMatch[n]] T )
     )
	(docIndxMatch.Pv.save)
	(writeln "--------------------------------------------------------")
	(writeln "Sample documents are saved at DocumentsForMatching.db")
	;(docIndxMatch.Pv.__showTree)
)



(defun prepareDocuments()
	(setq countPrepDoc 0)
	 vars:(repos documindex i entry)
	(setq repos (new ObjectRepository: "DocumentsForInvert.db"))
	(setq documindex (new index documindex: repos create: memory:))

	vars:(repos readyDocumentIndex i entry)
	(setq repos (new ObjectRepository: "readyDocumentTable.db"))
	(clear repos)   
	(setq readyDocumentIndex (new index  readyDocumentIndex: repos create: memory:)) 
	(loop for nW from 0 until  documindex.Pv.numKeys by 1 do
		(begin
			(setq countPrepDoc (+ countPrepDoc 1))
			(if  (or (= countPrepDoc 1000) (= countPrepDoc 2000))
				(begin
			  		(writeln " countPrepDocxx  "  countPrepDoc )
			 		(readyDocumentIndex.Pv.save)
				)
			)
			
			(writeln "Document number " countPrepDoc "  is: "  documindex[nW 1][0])
			(setq weightedInvert (invertIndex.assignWeightsPerPage   documindex[nW 0][0][0]  documindex[nW 1][0]  ) ) 
			(setq readyDocumentIndex[weightedInvert[1]] weightedInvert[0] )
		)
	)
	(writeln " countPrepDocxx  "  countPrepDoc )
	(readyDocumentIndex.Pv.save)
	(writeln "--------------------------------------------------------")
	(writeln "These prepared XML documents are ready for Invert Index generation and saved at readyDocumentTable.db ")
	;(readyDocumentIndex.Pv.__showTree)
)


(defun constructInvertIndexTable()
	(writeln "start constructInvertIndexTable")
	(setq countInvertIndxDoc 0)
	 vars:(repos readyDocumentIndex i entry)
	(setq repos (new ObjectRepository: "readyDocumentTable.db"))
	(setq readyDocumentIndex (new index  readyDocumentIndex: repos create: memory:)) 
	vars:(repos weightedInvertIndex i entry)
	(setq repos (new ObjectRepository: "weightedInvertIndexTable.db"))
	(clear repos)   
	(setq weightedInvertIndex (new index weightedInvertIndex: repos create: memory:)) 
	(loop for n from 0 until readyDocumentIndex.Pv.numKeys by 1 do
		(begin
		(setq V1  readyDocumentIndex[n 0][0][0])
		(loop for m from 0 until (length V1) by 1 do
			(begin
			(setq countInvertIndxDoc  (+ countInvertIndxDoc  1))
			(if  (or (= countInvertIndxDoc  7000) (= countInvertIndxDoc  14000))
				(begin
			  		(writeln " countInvertIndxDoc  "  countInvertIndxDoc  )
			 		(weightedInvertIndex.Pv.save)
				)
			)
			(setq cIndex (new Vector: 2 (/ V1[m][1] readyDocumentIndex[n 0][0][2])  readyDocumentIndex[n 1][0] ))
			(setq  weightedInvertIndex[V1[m][0]]  cIndex)
			)
		)
		)
	)
	(writeln " countInvertIndxDoc  "  countInvertIndxDoc  )
	(weightedInvertIndex.Pv.save)
	(writeln "--------------------------------------------------------")
	(writeln "The inverted indices of the documents are saved at weightedInvertIndexTable.db")
	;(weightedInvertIndex.Pv.__showTree)
)


(defun searchDocs(S)
	(writeln "S: " S)
	(setq trivial 1)
	vars:(repos weightedInvertIndex i entry)
	(setq repos (new ObjectRepository: "weightedInvertIndexTable.db"))
	(setq weightedInvertIndex (new index weightedInvertIndex: repos create: memory:)) 
	(setq completeStem (getStem (downcase S)))
	(if (= (length completeStem) 0) then (setq retXMLdoc (new Vector:))
	else 
	(begin
	(setq trivial 0)
	(setq InvertIndexWords (weightedInvertIndex.Pv.refAttributes))
	(setq ret (weightedInvertIndex.Pv.refValues (string completeStem[0])))
	(setq retWStemmed (new Vector: ))
	(loop for cRet from 0 until (length ret)
		(begin
			(setq retWStemmed (insert retWStemmed 0 (new Vector: 3 ret[cRet][0] ret[cRet][1]  completeStem[0]  ) ))
		)
	)
	(loop for countCompleteStem from 1 until (length completeStem) by 1 do
		(begin
			(if (isMember (string completeStem[countCompleteStem]) InvertIndexWords)
				(begin
					(setq ret (append ret (weightedInvertIndex.Pv.refValues (string completeStem[countCompleteStem]) )))
				)
			)
		)
	)
	(setq ret (sortWeightedPages2 ret))
	(setq temp 0)
	(setq nPage 0)
	(setq retPage (new Vector: ))
	(setq retXMLdoc (new Vector: ))
	
	(loop for eachWord from 0 until (length ret) by 1 do
		(begin
			(if (= temp ret[eachWord][1])
				(begin
					(setq h (- nPage 1))
					(setq  retPage[h][0] (+ retPage[h][0] ret[eachWord][0] 10)) 
				)
			else
				(begin
					(insert retPage nPage ret[eachWord] )
					(++ nPage)
				)
			)
			(setq temp ret[eachWord][1])
		)
	)
	(setq retpage (sortWeightedPages1 retPage))
	(loop for docCount from 0 until (length retpage) by 1 do
		(insert retXMLdoc docCount retpage[docCount][1])
	)
	)
	)
	(writeln)
	(if (= trivial 1)
		(writeln "Search string is too trivial. No document returned. ")
	else
		(if (= (length retXMLdoc) 0)
			(writeln "No document has that words")
		else
			(begin
				(writeln "Arranged from the most relevant document: ")
				(loop for nretXML from 0 until (length retXMLdoc) by 1 do
					(writeln nretXML "     " retXMLdoc[nretXML])
				)
			)
		)
	)
	
;retXMLdoc
)



(defun searchDocsWithStemWords(S)
	(writeln "S: " S)
	(setq trivial 1)
	vars:(repos weightedInvertIndex i entry)
	(setq repos (new ObjectRepository: "weightedInvertIndexTable.db"))
	(setq weightedInvertIndex (new index weightedInvertIndex: repos create: memory:)) 
	(setq completeStem (getStem (downcase S)))
	(if (= (length completeStem) 0) then (setq retXMLdoc (new Vector:))
	else 
	(begin
	(setq trivial 0)
	(setq InvertIndexWords (weightedInvertIndex.Pv.refAttributes))
	(setq ret (weightedInvertIndex.Pv.refValues (string completeStem[0])))
	(setq retWStemmed (new Vector: ))
	(loop for cRet from 0 until (length ret)
		(begin
			(setq retWStemmed (insert retWStemmed 0 (new Vector: 3 ret[cRet][0] ret[cRet][1]  (new Vector: 1 completeStem[0])  ) ))
		)
	)
	(loop for countCompleteStem from 1 until (length completeStem) by 1 do
		(begin
			(if (isMember (string completeStem[countCompleteStem]) InvertIndexWords)
				(begin
					(setq forAppendRet (weightedInvertIndex.Pv.refValues (string completeStem[countCompleteStem]) ))
					(setq ret (append ret forAppendRet))
					(loop for cRet from 0 until (length forAppendRet)
						(begin
							(setq newRetStemEntry (new Vector: 3 forAppendRet[cRet][0] forAppendRet[cRet][1]  (new Vector: 1 completeStem[countCompleteStem] ) ) )
							(setq retWStemmed (insert retWStemmed 0 newRetStemEntry ) )
						)
					)
				)
			)
		)
	)
	(setq retWStemmed (sortWeightedPages2 retWStemmed))
	(setq temp 0)
	(setq nPage 0)
	(setq retPageWord (new Vector: ))
	(loop for eachWord from 0 until (length retWStemmed) by 1 do
		(begin
			(if (= temp retWStemmed[eachWord][1])
				(begin
					(setq h (- nPage 1))
					(setq  retPageWord[h][0] (+ retPageWord[h][0] retWStemmed[eachWord][0] 10))
					(setq  retPageWord[h][2] (append retPageWord[h][2] retWStemmed[eachWord][2]))
				)
			else
				(begin
					(insert retPageWord nPage retWStemmed[eachWord] )
					(++ nPage)
				)
			)
			(setq temp retWStemmed[eachWord][1])
		)
	)
	(setq retPageWord (sortWeightedPages1 retPageWord))
	)
	)
	(writeln)
	(if (= trivial 1)
		(writeln "Search string is too trivial. No document returned. ")
	else
		(if (= (length retPageWord) 0)
			(writeln "No document has that words")
		else
			(begin
				(writeln "Arranged from the most relevant document: ")
				(loop for nretPageWord from 0 until (length retPageWord) by 1 do
					(writeln nretPageWord "    " retPageWord[nretPageWord][1] "    " retPageWord[nretPageWord][2])
				)
			)
		)
	)
	
;retXMLdoc
)


(defun getStem(inputString)
	(setq count_stem -1)
	(setq stemResults (new Vector: ))
	(setq inputWords (copy (parseLib.defaultLexer  inputString) ))
	(setq noJunkWords (invertIndex.removeJunkwords inputWords))
	(if (= (length noJunkWords) 0) then
		(setq stemPorter (new Vector: ))
    else
		(begin
			(setq noJunkWordsString noJunkWords[0])
			(loop for n from 1 until (length noJunkWords) by 1 do
				(begin
					(setq  noJunkWordsString (append noJunkWordsString " " noJunkWords[n]))
				)
			)
			(setq porterStemmer.htmlTagsOn false)
			(setq stemPorter (porterStemmer noJunkWordsString))
		)

	)
;(writeln "after getStem: " getStem)
stemPorter

)



(defun removeJunkwords(withJunk)
	 vars:(repos junkwordindex i entry)
	(setq repos (new ObjectRepository: "junkWords.db"))
	(setq junkwordindex (new index junkwordindex: repos create: memory:)) 
	(setq withoutJunk (new Vector: ))
	(loop for m from 0 until (length withJunk) by 1 do
			(if (= (member withJunk[m] (junkwordindex.Pv.refAttributes)) false) then
				(begin
					(insert withoutJunk 0 withJunk[m])
				)
			)
		)
		withoutJunk
)


(defun notjunkWord(W)
	 vars:(repos junkwordindex i entry)
	(setq repos (new ObjectRepository: "junkWords.db"))
	(setq junkwordindex (new index  junkwordindex: repos create: memory:)) 
	(setq R (new Vector: 1 (string W) ))
	(if (= (member R (junkwordindex.Pv.refAttributes)) false) then
				(return true)
	)
false
)


(defun conjunction(V1 V2)
	(setq V3 (new Vector: ))
	(loop for m from 0 until (length V1) by 1 do
		(if (isMember V1[m] V2)
			(begin
				(insert V3 0 V1[m])
			)
		)
	)
	(return V3)
)


(defun sortToPrepareGetFrequency(a)  ; insertion sort - increasing
	;(setq a (new Vector: 6 #("investment" "investment" "investment" ) #("girls" "girl" "girl" ) #("boys" "boi" "boy" ) #("coffee" "coffee" "coffee" ) #("java" "java" "java" ) #("budgeting" "budget" "budget" ) ) )
	(loop for i from 1 until (length a) by 1 do
		(setq v a[i] )
		(setq j i)
		(while (> a[(- j 1)][2] v[2])
			(begin
				(setq a[j] a[(- j 1)])
				(setq j (- j 1))
			)
		(setq a[j] v)
		)
	)
a
)

(defun getFrequency1(V) 
	(setq F (new Vector: ))
	(setq k 0)
	(setq temp1 "")
	(setq temp (string temp1))
	(setq V (sort V <))
	(loop for mTriple from 0 until (length V) by 1 do
		(begin
		  (if (<> V[mTriple] "")
			(begin
				(setq vWord (string  V[mTriple]))
				(if (= temp vWord)
					(begin
						(setq h (- k 1))
						(setq  F[h] (++ F[h][1]))
					)
				else
					(begin
						(insert F k (new Vector: 2 vWord 1))
						(++ k)
					)
				)
				(setq temp vWord)
			)
		  )
		)
	)
	(setq sumF 0)
	(loop for nF from 0 until (length F) by 1 do
		(begin
			(setq sumF (+ sumF (*  F[nF][1]  F[nF][1] )  ))
			
		)
	)
	(setq res (new Vector: 3 F (length V) (sqrt sumF) ))
res
)



(defun assignWeightsPerPage(S Page)
	(setq stemmed (getStem S))
	(setq wordFrequency (getFrequency1 stemmed))
	(setq wordsWithPage (new Vector: 2 Page wordFrequency))
wordsWithPage
)



(defun getFrequency(V)
	(setq F (new Vector: ))
	(setq k 0)
	(sort V <)
	(loop for m from 0 until (length V) by 1 do
		(begin
			(if (= temp V[m])
				(begin
					(setq h (- k 1))
					(setq  F[h] (++ F[h][1]))
				)
			else
				(begin
					(insert F k (new Vector:2 V[m] 1))
					(++ k)
				)
			)
			(setq temp V[m])
		)
	)
	(return (new Vector: 2 F (length V)))
)


(defun sortWeightedPages(a)  ; insertion sort - sort according to ascending weights
	;(setq a (new Vector: 4 #(0.152380952381 2 ) #(0.2285714285714 1 ) #(0.4571428571429 4 ) #(0.5485714285714 3 ) ))
	(loop for i from 1 until (length a) by 1 do
		(setq v a[i] )
		(setq j i)
		(while (> a[(- j 1)][0] v[0])
			(begin
				(setq a[j] a[(- j 1)])
				(setq j (- j 1))
			)
		(setq a[j] v)
		)
	)
a
)


(defun sortWeightedPages2(a)  ; insertion sort - sort according to ascending page numbers
	;(setq a (new Vector: 4 #(0.152380952381 2 ) #(0.2285714285714 1 ) #(0.4571428571429 4 ) #(0.5485714285714 3 ) ))
	(loop for i from 1 until (length a) by 1 do
		(setq v a[i] )
		(setq j i)
		(while (> a[(- j 1)][1] v[1])
			(begin
				(setq a[j] a[(- j 1)])
				(setq j (- j 1))
			)
		(setq a[j] v)
		)
	)
a
)

(defun sortWeightedPages1(a)  ;selection sort -decreasing
	;(setq a (new Vector: 4 #(0.152380952381 2 ) #(0.2285714285714 1 ) #(0.4571428571429 4 ) #(0.5485714285714 3 ) ))
	(loop for i from 0 until (length a) by 1 do
		(setq max i )
		(loop for j from (+ i 1) until (length a) by 1 do
			(if (> a[j] a[max]) then (setq max j))
		)
	(setq t a[max]) (setq a[max] a[i]) (setq a[i] t)
		
	)
a
)

(defun constructJunkWordIndex()
	 vars:(repos junkwordindex i entry)
 
	(setq repos (new ObjectRepository: "junkWords.db"))
	(clear repos)   

	(setq junkwordindex (new index junkwordindex: repos create: memory:)) 
	(writeln "after junkword: " )
(setq junkwordindex["i'm"] 1)
(setq junkwordindex["web"] 1)
(setq junkwordindex["don't"] 1)
(setq junkwordindex["i've"] 1)
(setq junkwordindex["we've"] 1)
(setq junkwordindex["they've"] 1)
(setq junkwordindex["she's"] 1)
(setq junkwordindex["he's"] 1)
(setq junkwordindex["it's"] 1)
(setq junkwordindex["great"] 1)
(setq junkwordindex["old"] 1)
(setq junkwordindex["can't"] 1)
(setq junkwordindex["tell"] 1)
(setq junkwordindex["tells"] 1)
(setq junkwordindex["busy"] 1)
(setq junkwordindex["doesn't"] 1)
(setq junkwordindex["you're"] 1)
(setq junkwordindex["your's"] 1)
(setq junkwordindex["didn't"] 1)
(setq junkwordindex["they're"] 1)
(setq junkwordindex["night"] 1)
(setq junkwordindex["nights"] 1)
(setq junkwordindex["anyone"] 1)
(setq junkwordindex["isn't"] 1)
(setq junkwordindex["i'll"] 1)
(setq junkwordindex["actual"] 1)
(setq junkwordindex["actually"] 1)
(setq junkwordindex["presents"] 1)
(setq junkwordindex["presenting"] 1)
(setq junkwordindex["presenter"] 1)
(setq junkwordindex["present"] 1)
(setq junkwordindex["presented"] 1)
(setq junkwordindex["presentation"] 1)
(setq junkwordindex["we're"] 1)
(setq junkwordindex["wouldn't"] 1)
(setq junkwordindex["example"] 1)
(setq junkwordindex["examples"] 1)
(setq junkwordindex["i'd"] 1)
(setq junkwordindex["haven't"] 1)
(setq junkwordindex["etc"] 1)
(setq junkwordindex["won't"] 1)
(setq junkwordindex["myself"] 1)
(setq junkwordindex["we've"] 1)
(setq junkwordindex["they've"] 1)
(setq junkwordindex["aren't"] 1)
(setq junkwordindex["we'd"] 1)
(setq junkwordindex["it'd"] 1)
(setq junkwordindex["ain't"] 1)
(setq junkwordindex["i'll"] 1)
(setq junkwordindex["who've"] 1)
(setq junkwordindex["-year-old"] 1)
(setq junkwordindex["kind"] 1)
(setq junkwordindex["kinds"] 1)
(setq junkwordindex["builds"] 1)
(setq junkwordindex["build"] 1)
(setq junkwordindex["built"] 1)
(setq junkwordindex["com"] 1)
(setq junkwordindex["make"] 1)
(setq junkwordindex["makes"] 1)
(setq junkwordindex["making"] 1)
(setq junkwordindex["made"] 1)
(setq junkwordindex["you'll"] 1)
(setq junkwordindex["couldn't"] 1)
(setq junkwordindex["use"] 1)
(setq junkwordindex["uses"] 1)
(setq junkwordindex["used"] 1)
(setq junkwordindex["using"] 1)
(setq junkwordindex["take"] 1)
(setq junkwordindex["takes"] 1)
(setq junkwordindex["taking"] 1)
(setq junkwordindex["taken"] 1)
(setq junkwordindex["exactly"] 1)
(setq junkwordindex["we'll"] 1)
(setq junkwordindex["it'll"] 1)
(setq junkwordindex["certainly"] 1)
(setq junkwordindex["he'd"] 1)
(setq junkwordindex["shown"] 1)
(setq junkwordindex["they'd"] 1)
(setq junkwordindex["wasn't"] 1)
(setq junkwordindex["yeah"] 1)
(setq junkwordindex["to-day"] 1)
(setq junkwordindex["lya"] 1)
(setq junkwordindex["a"] 1)
(setq junkwordindex["ability"] 1)
(setq junkwordindex["able"] 1)
(setq junkwordindex["aboard"] 1)
(setq junkwordindex["about"] 1)
(setq junkwordindex["above"] 1)
(setq junkwordindex["absolute"] 1)
(setq junkwordindex["absolutely"] 1)
(setq junkwordindex["across"] 1)
(setq junkwordindex["act"] 1)
(setq junkwordindex["acts"] 1)
(setq junkwordindex["add"] 1)
(setq junkwordindex["additional"] 1)
(setq junkwordindex["additionally"] 1)
(setq junkwordindex["after"] 1)
(setq junkwordindex["afterwards"] 1)
(setq junkwordindex["again"] 1)
(setq junkwordindex["against"] 1)
(setq junkwordindex["ago"] 1)
(setq junkwordindex["ahead"] 1)
(setq junkwordindex["aimless"] 1)
(setq junkwordindex["aimlessly"] 1)
(setq junkwordindex["al"] 1)
(setq junkwordindex["albeit"] 1)
(setq junkwordindex["align"] 1)
(setq junkwordindex["all"] 1)
(setq junkwordindex["allow"] 1)
(setq junkwordindex["almost"] 1)
(setq junkwordindex["along"] 1)
(setq junkwordindex["alongside"] 1)
(setq junkwordindex["already"] 1)
(setq junkwordindex["also"] 1)
(setq junkwordindex["alternate"] 1)
(setq junkwordindex["alternately"] 1)
(setq junkwordindex["although"] 1)
(setq junkwordindex["always"] 1)
(setq junkwordindex["am"] 1)
(setq junkwordindex["amid"] 1)
(setq junkwordindex["amidst"] 1)
(setq junkwordindex["among"] 1)
(setq junkwordindex["amongst"] 1)
(setq junkwordindex["an"] 1)
(setq junkwordindex["and"] 1)
(setq junkwordindex["announce"] 1)
(setq junkwordindex["announced"] 1)
(setq junkwordindex["announcement"] 1)
(setq junkwordindex["announces"] 1)
(setq junkwordindex["another"] 1)
(setq junkwordindex["anti"] 1)
(setq junkwordindex["any"] 1)
(setq junkwordindex["anything"] 1)
(setq junkwordindex["appaling"] 1)
(setq junkwordindex["appalingly"] 1)
(setq junkwordindex["appear"] 1)
(setq junkwordindex["appeared"] 1)
(setq junkwordindex["appears"] 1)
(setq junkwordindex["are"] 1)
(setq junkwordindex["around"] 1)
(setq junkwordindex["as"] 1)
(setq junkwordindex["ask"] 1)
(setq junkwordindex["asked"] 1)
(setq junkwordindex["asking"] 1)
(setq junkwordindex["asks"] 1)
(setq junkwordindex["at"] 1)
(setq junkwordindex["await"] 1)
(setq junkwordindex["awaited"] 1)
(setq junkwordindex["awaits"] 1)
(setq junkwordindex["awaken"] 1)
(setq junkwordindex["awakened"] 1)
(setq junkwordindex["awakens"] 1)
(setq junkwordindex["aware"] 1)
(setq junkwordindex["away"] 1)
(setq junkwordindex["b"] 1)
(setq junkwordindex["back"] 1)
(setq junkwordindex["backed"] 1)
(setq junkwordindex["backing"] 1)
(setq junkwordindex["backs"] 1)
(setq junkwordindex["be"] 1)
(setq junkwordindex["became"] 1)
(setq junkwordindex["because"] 1)
(setq junkwordindex["become"] 1)
(setq junkwordindex["becomes"] 1)
(setq junkwordindex["becoming"] 1)
(setq junkwordindex["been"] 1)
(setq junkwordindex["before"] 1)
(setq junkwordindex["began"] 1)
(setq junkwordindex["begin"] 1)
(setq junkwordindex["begins"] 1)
(setq junkwordindex["behind"] 1)
(setq junkwordindex["being"] 1)
(setq junkwordindex["believe"] 1)
(setq junkwordindex["believed"] 1)
(setq junkwordindex["between"] 1)
(setq junkwordindex["both"] 1)
(setq junkwordindex["brang"] 1)
(setq junkwordindex["bring"] 1)
(setq junkwordindex["brings"] 1)
(setq junkwordindex["brought"] 1)
(setq junkwordindex["but"] 1)
(setq junkwordindex["by"] 1)
(setq junkwordindex["c"] 1)
(setq junkwordindex["call"] 1)
(setq junkwordindex["called"] 1)
(setq junkwordindex["calling"] 1)
(setq junkwordindex["calls"] 1)
(setq junkwordindex["can"] 1)
(setq junkwordindex["cannot"] 1)
(setq junkwordindex["carried"] 1)
(setq junkwordindex["carries"] 1)
(setq junkwordindex["carry"] 1)
(setq junkwordindex["carrying"] 1)
(setq junkwordindex["change"] 1)
(setq junkwordindex["changed"] 1)
(setq junkwordindex["changes"] 1)
(setq junkwordindex["choose"] 1)
(setq junkwordindex["chooses"] 1)
(setq junkwordindex["chose"] 1)
(setq junkwordindex["clearly"] 1)
(setq junkwordindex["close"] 1)
(setq junkwordindex["closed"] 1)
(setq junkwordindex["closes"] 1)
(setq junkwordindex["closing"] 1)
(setq junkwordindex["come"] 1)
(setq junkwordindex["comes"] 1)
(setq junkwordindex["coming"] 1)
(setq junkwordindex["consider"] 1)
(setq junkwordindex["considerable"] 1)
(setq junkwordindex["considering"] 1)
(setq junkwordindex["could"] 1)
(setq junkwordindex["couldn"] 1)
(setq junkwordindex["d"] 1)
(setq junkwordindex["dare"] 1)
(setq junkwordindex["daren"] 1)
(setq junkwordindex["day"] 1)
(setq junkwordindex["days"] 1)
(setq junkwordindex["despite"] 1)
(setq junkwordindex["did"] 1)
(setq junkwordindex["didn"] 1)
(setq junkwordindex["do"] 1)
(setq junkwordindex["does"] 1)
(setq junkwordindex["doesn"] 1)
(setq junkwordindex["doing"] 1)
(setq junkwordindex["done"] 1)
(setq junkwordindex["down"] 1)
(setq junkwordindex["downward"] 1)
(setq junkwordindex["downwards"] 1)
(setq junkwordindex["e"] 1)
(setq junkwordindex["each"] 1)
(setq junkwordindex["eight"] 1)
(setq junkwordindex["either"] 1)
(setq junkwordindex["else"] 1)
(setq junkwordindex["elsewhere"] 1)
(setq junkwordindex["especially"] 1)
(setq junkwordindex["even"] 1)
(setq junkwordindex["eventually"] 1)
(setq junkwordindex["ever"] 1)
(setq junkwordindex["every"] 1)
(setq junkwordindex["everybody"] 1)
(setq junkwordindex["everyone"] 1)
(setq junkwordindex["f"] 1)
(setq junkwordindex["far"] 1)
(setq junkwordindex["feel"] 1)
(setq junkwordindex["felt"] 1)
(setq junkwordindex["few"] 1)
(setq junkwordindex["final"] 1)
(setq junkwordindex["finally"] 1)
(setq junkwordindex["find"] 1)
(setq junkwordindex["five"] 1)
(setq junkwordindex["for"] 1)
(setq junkwordindex["found"] 1)
(setq junkwordindex["four"] 1)
(setq junkwordindex["fourth"] 1)
(setq junkwordindex["from"] 1)
(setq junkwordindex["get"] 1)
(setq junkwordindex["gets"] 1)
(setq junkwordindex["getting"] 1)
(setq junkwordindex["gave"] 1)
(setq junkwordindex["give"] 1)
(setq junkwordindex["gives"] 1)
(setq junkwordindex["go"] 1)
(setq junkwordindex["goes"] 1)
(setq junkwordindex["going"] 1)
(setq junkwordindex["gone"] 1)
(setq junkwordindex["good"] 1)
(setq junkwordindex["got"] 1)
(setq junkwordindex["h"] 1)
(setq junkwordindex["had"] 1)
(setq junkwordindex["has"] 1)
(setq junkwordindex["have"] 1)
(setq junkwordindex["he"] 1)
(setq junkwordindex["held"] 1)
(setq junkwordindex["her"] 1)
(setq junkwordindex["here"] 1)
(setq junkwordindex["heretofore"] 1)
(setq junkwordindex["hereby"] 1)
(setq junkwordindex["herewith"] 1)
(setq junkwordindex["hers"] 1)
(setq junkwordindex["herself"] 1)
(setq junkwordindex["high"] 1)
(setq junkwordindex["him"] 1)
(setq junkwordindex["himself"] 1)
(setq junkwordindex["his"] 1)
(setq junkwordindex["hitherto"] 1)
(setq junkwordindex["happen"] 1)
(setq junkwordindex["happened"] 1)
(setq junkwordindex["happens"] 1)
(setq junkwordindex["hour"] 1)
(setq junkwordindex["hours"] 1)
(setq junkwordindex["how"] 1)
(setq junkwordindex["however"] 1)
(setq junkwordindex["i"] 1)
(setq junkwordindex["ii"] 1)
(setq junkwordindex["iii"] 1)
(setq junkwordindex["iv"] 1)
(setq junkwordindex["if"] 1)
(setq junkwordindex["in"] 1)
(setq junkwordindex["include"] 1)
(setq junkwordindex["included"] 1)
(setq junkwordindex["includes"] 1)
(setq junkwordindex["including"] 1)
(setq junkwordindex["inside"] 1)
(setq junkwordindex["into"] 1)
(setq junkwordindex["is"] 1)
(setq junkwordindex["isn"] 1)
(setq junkwordindex["it"] 1)
(setq junkwordindex["its"] 1)
(setq junkwordindex["itself"] 1)
(setq junkwordindex["j"] 1)
(setq junkwordindex["just"] 1)
(setq junkwordindex["k"] 1)
(setq junkwordindex["l"] 1)
(setq junkwordindex["la"] 1)
(setq junkwordindex["larger"] 1)
(setq junkwordindex["largest"] 1)
(setq junkwordindex["last"] 1)
(setq junkwordindex["later"] 1)
(setq junkwordindex["latest"] 1)
(setq junkwordindex["le"] 1)
(setq junkwordindex["least"] 1)
(setq junkwordindex["leave"] 1)
(setq junkwordindex["leaves"] 1)
(setq junkwordindex["leaving"] 1)
(setq junkwordindex["les"] 1)
(setq junkwordindex["let"] 1)
(setq junkwordindex["less"] 1)
(setq junkwordindex["like"] 1)
(setq junkwordindex["ll"] 1)
(setq junkwordindex["m"] 1)
(setq junkwordindex["made"] 1)
(setq junkwordindex["main"] 1)
(setq junkwordindex["mainly"] 1)
(setq junkwordindex["make"] 1)
(setq junkwordindex["makes"] 1)
(setq junkwordindex["man"] 1)
(setq junkwordindex["many"] 1)
(setq junkwordindex["may"] 1)
(setq junkwordindex["me"] 1)
(setq junkwordindex["means"] 1)
(setq junkwordindex["meant"] 1)
(setq junkwordindex["meanwhile"] 1)
(setq junkwordindex["men"] 1)
(setq junkwordindex["might"] 1)
(setq junkwordindex["missed"] 1)
(setq junkwordindex["more"] 1)
(setq junkwordindex["moreover"] 1)
(setq junkwordindex["most"] 1)
(setq junkwordindex["mostly"] 1)
(setq junkwordindex["move"] 1)
(setq junkwordindex["moved"] 1)
(setq junkwordindex["moving"] 1)
(setq junkwordindex["mr"] 1)
(setq junkwordindex["mrs"] 1)
(setq junkwordindex["much"] 1)
(setq junkwordindex["must"] 1)
(setq junkwordindex["mustn"] 1)
(setq junkwordindex["my"] 1)
(setq junkwordindex["need"] 1)
(setq junkwordindex["needs"] 1)
(setq junkwordindex["neither"] 1)
(setq junkwordindex["never"] 1)
(setq junkwordindex["new"] 1)
(setq junkwordindex["newer"] 1)
(setq junkwordindex["news"] 1)
(setq junkwordindex["nine"] 1)
(setq junkwordindex["no"] 1)
(setq junkwordindex["non"] 1)
(setq junkwordindex["none"] 1)
(setq junkwordindex["nor"] 1)
(setq junkwordindex["not"] 1)
(setq junkwordindex["now"] 1)
(setq junkwordindex["o"] 1)
(setq junkwordindex["of"] 1)
(setq junkwordindex["off"] 1)
(setq junkwordindex["often"] 1)
(setq junkwordindex["on"] 1)
(setq junkwordindex["once"] 1)
(setq junkwordindex["one"] 1)
(setq junkwordindex["only"] 1)
(setq junkwordindex["or"] 1)
(setq junkwordindex["other"] 1)
(setq junkwordindex["our"] 1)
(setq junkwordindex["out"] 1)
(setq junkwordindex["over"] 1)
(setq junkwordindex["own"] 1)
(setq junkwordindex["owns"] 1)
(setq junkwordindex["p"] 1)
(setq junkwordindex["particularly"] 1)
(setq junkwordindex["per"] 1)
(setq junkwordindex["percent"] 1)
(setq junkwordindex["primarily"] 1)
(setq junkwordindex["put"] 1)
(setq junkwordindex["q"] 1)
(setq junkwordindex["quickly"] 1)
(setq junkwordindex["r"] 1)
(setq junkwordindex["remain"] 1)
(setq junkwordindex["remaining"] 1)
(setq junkwordindex["respond"] 1)
(setq junkwordindex["responded"] 1)
(setq junkwordindex["responding"] 1)
(setq junkwordindex["responds"] 1)
(setq junkwordindex["return"] 1)
(setq junkwordindex["ran"] 1)
(setq junkwordindex["rather"] 1)
(setq junkwordindex["run"] 1)
(setq junkwordindex["running"] 1)
(setq junkwordindex["runs"] 1)
(setq junkwordindex["s"] 1)
(setq junkwordindex["said"] 1)
(setq junkwordindex["say"] 1)
(setq junkwordindex["says"] 1)
(setq junkwordindex["same"] 1)
(setq junkwordindex["see"] 1)
(setq junkwordindex["seek"] 1)
(setq junkwordindex["seeking"] 1)
(setq junkwordindex["seeks"] 1)
(setq junkwordindex["seen"] 1)
(setq junkwordindex["send"] 1)
(setq junkwordindex["sent"] 1)
(setq junkwordindex["set"] 1)
(setq junkwordindex["sets"] 1)
(setq junkwordindex["seven"] 1)
(setq junkwordindex["several"] 1)
(setq junkwordindex["she"] 1)
(setq junkwordindex["should"] 1)
(setq junkwordindex["shouldn"] 1)
(setq junkwordindex["side"] 1)
(setq junkwordindex["since"] 1)
(setq junkwordindex["six"] 1)
(setq junkwordindex["sixes"] 1)
(setq junkwordindex["slow"] 1)
(setq junkwordindex["slowed"] 1)
(setq junkwordindex["slows"] 1)
(setq junkwordindex["small"] 1)
(setq junkwordindex["smaller"] 1)
(setq junkwordindex["so"] 1)
(setq junkwordindex["some"] 1)
(setq junkwordindex["someone"] 1)
(setq junkwordindex["something"] 1)
(setq junkwordindex["somewhat"] 1)
(setq junkwordindex["somewhere"] 1)
(setq junkwordindex["soon"] 1)
(setq junkwordindex["sought"] 1)
(setq junkwordindex["spread"] 1)
(setq junkwordindex["stay"] 1)
(setq junkwordindex["stayed"] 1)
(setq junkwordindex["still"] 1)
(setq junkwordindex["substantially"] 1)
(setq junkwordindex["such"] 1)
(setq junkwordindex["suppose"] 1)
(setq junkwordindex["t"] 1)
(setq junkwordindex["take"] 1)
(setq junkwordindex["takes"] 1)
(setq junkwordindex["taken"] 1)
(setq junkwordindex["th"] 1)
(setq junkwordindex["than"] 1)
(setq junkwordindex["that"] 1)
(setq junkwordindex["the"] 1)
(setq junkwordindex["their"] 1)
(setq junkwordindex["them"] 1)
(setq junkwordindex["themselves"] 1)
(setq junkwordindex["then"] 1)
(setq junkwordindex["there"] 1)
(setq junkwordindex["thereby"] 1)
(setq junkwordindex["therefore"] 1)
(setq junkwordindex["these"] 1)
(setq junkwordindex["they"] 1)
(setq junkwordindex["thing"] 1)
(setq junkwordindex["things"] 1)
(setq junkwordindex["thi"] 1)
(setq junkwordindex["this"] 1)
(setq junkwordindex["those"] 1)
(setq junkwordindex["though"] 1)
(setq junkwordindex["thus"] 1)
(setq junkwordindex["three"] 1)
(setq junkwordindex["through"] 1)
(setq junkwordindex["throughout"] 1)
(setq junkwordindex["to"] 1)
(setq junkwordindex["together"] 1)
(setq junkwordindex["too"] 1)
(setq junkwordindex["took"] 1)
(setq junkwordindex["toward"] 1)
(setq junkwordindex["towards"] 1)
(setq junkwordindex["tried"] 1)
(setq junkwordindex["tries"] 1)
(setq junkwordindex["try"] 1)
(setq junkwordindex["trying"] 1)
(setq junkwordindex["two"] 1)
(setq junkwordindex["u"] 1)
(setq junkwordindex["unable"] 1)
(setq junkwordindex["under"] 1)
(setq junkwordindex["underneath"] 1)
(setq junkwordindex["undid"] 1)
(setq junkwordindex["undo"] 1)
(setq junkwordindex["undoes"] 1)
(setq junkwordindex["undone"] 1)
(setq junkwordindex["undue"] 1)
(setq junkwordindex["undoubtedly"] 1)
(setq junkwordindex["unfortunately"] 1)
(setq junkwordindex["unless"] 1)
(setq junkwordindex["unnecessarily"] 1)
(setq junkwordindex["unofficially"] 1)
(setq junkwordindex["until"] 1)
(setq junkwordindex["unusually"] 1)
(setq junkwordindex["unsure"] 1)
(setq junkwordindex["up"] 1)
(setq junkwordindex["upon"] 1)
(setq junkwordindex["upward"] 1)
(setq junkwordindex["us"] 1)
(setq junkwordindex["use"] 1)
(setq junkwordindex["used"] 1)
(setq junkwordindex["uses"] 1)
(setq junkwordindex["using"] 1)
(setq junkwordindex["usual"] 1)
(setq junkwordindex["usually"] 1)
(setq junkwordindex["v"] 1)
(setq junkwordindex["ve"] 1)
(setq junkwordindex["very"] 1)
(setq junkwordindex["via"] 1)
(setq junkwordindex["view"] 1)
(setq junkwordindex["viewed"] 1)
(setq junkwordindex["w"] 1)
(setq junkwordindex["wait"] 1)
(setq junkwordindex["waited"] 1)
(setq junkwordindex["waits"] 1)
(setq junkwordindex["want"] 1)
(setq junkwordindex["wanted"] 1)
(setq junkwordindex["wants"] 1)
(setq junkwordindex["was"] 1)
(setq junkwordindex["wasn"] 1)
(setq junkwordindex["watched"] 1)
(setq junkwordindex["watching"] 1)
(setq junkwordindex["way"] 1)
(setq junkwordindex["ways"] 1)
(setq junkwordindex["we"] 1)
(setq junkwordindex["went"] 1)
(setq junkwordindex["were"] 1)
(setq junkwordindex["what"] 1)
(setq junkwordindex["whatever"] 1)
(setq junkwordindex["when"] 1)
(setq junkwordindex["whenever"] 1)
(setq junkwordindex["where"] 1)
(setq junkwordindex["whereever"] 1)
(setq junkwordindex["whether"] 1)
(setq junkwordindex["which"] 1)
(setq junkwordindex["whichever"] 1)
(setq junkwordindex["while"] 1)
(setq junkwordindex["who"] 1)
(setq junkwordindex["whoever"] 1)
(setq junkwordindex["whom"] 1)
(setq junkwordindex["whomsoever"] 1)
(setq junkwordindex["whose"] 1)
(setq junkwordindex["whosever"] 1)
(setq junkwordindex["why"] 1)
(setq junkwordindex["wide"] 1)
(setq junkwordindex["wider"] 1)
(setq junkwordindex["will"] 1)
(setq junkwordindex["with"] 1)
(setq junkwordindex["without"] 1)
(setq junkwordindex["won"] 1)
(setq junkwordindex["would"] 1)
(setq junkwordindex["wouldn"] 1)
(setq junkwordindex["wow"] 1)
(setq junkwordindex["wows"] 1)
(setq junkwordindex["www"] 1)
(setq junkwordindex["x"] 1)
(setq junkwordindex["xii"] 1)
(setq junkwordindex["xiii"] 1)
(setq junkwordindex["xiv"] 1)
(setq junkwordindex["xv"] 1)
(setq junkwordindex["xvi"] 1)
(setq junkwordindex["xvii"] 1)
(setq junkwordindex["xviii"] 1)
(setq junkwordindex["xix"] 1)
(setq junkwordindex["xx"] 1)
(setq junkwordindex["y"] 1)
(setq junkwordindex["year"] 1)
(setq junkwordindex["you"] 1)
(setq junkwordindex["your"] 1)
(setq junkwordindex["yours"] 1)
(setq junkwordindex["yourself"] 1)
(setq junkwordindex["yourselves"] 1)


(setq junkwordindex["|'|"] 1)

;(junkwordindex.Pv.__showTree)
(junkwordindex.Pv.save)
(writeln)
(writeln "--------------------------------------------------------")
(writeln "Junk Words saved on junkWords.db")
)

(defun prepareInvertIndexDB()
(writeln "before junkword: ")
	(constructJunkWordIndex)
(writeln "after junkword: " )
	(initDocuments)
(writeln "after init: " )
	(prepareDocuments)
(writeln "after prepare: " )
	(constructInvertIndexTable)
(writeln "after constructInvertIndexTable: " )
)

#if 0
(defun removeXMLTags(xmlString)
	(setq state 0)
	(setq invalidchar (new Vector: 34 "$" "%" "`" "*" "=" "|" "\\" "/" "'" "\"" "#" "&" "{" "}" "," "+" "-" "(" ")" "." "[" "]" "<" ">" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
	
	(setq wordOutside "")
	(loop for k from 0 until (length xmlString) by 1 do
		(begin
			(if (= (string xmlString[k]) "_") (setq xmlString[k] " " ))
			(case state
				(	(0) 
					(if (= (string xmlString[k]) "<") 
						(begin 
							(setq state 1) 
							(setq wordOutside (append wordOutside " "))
						) 
					 else
						(begin
							(setq state 0)
							(if (= (isMember (string xmlString[k]) invalidchar ) false) then
								(setq wordOutside (append wordOutside xmlString[k]))
							)
						)
					)
				)
				(	(1) 
					(if (= (string xmlString[k]) ">") 
						(begin 
							(setq state 0) 
						) 
					else 
						(begin
							(setq state 1)
						)
					)
				)
			)
		)
	)
;(trim (downcase wordOutside))
(trim (clean (downcase wordOutside)))
)

#endif



)


(deforphan invertIndex:selfTest()
;**************************************************
;******** selfTest of invertIndex Lambda *****************
;**************************************************


(writeln "********* Steps to test inverIndex Lambda at the console****************")
(writeln "********* These sequence of commands can be executed at the console*****")
(writeln "********* Results are displayed at the console *************************")


;; TEST
;;(invertIndex.prepareInvertIndexDB) ; this will take almost an hour. 
;;			so that you can proceed testing the search right away, it is suggested that you just copy 4 files into C:/Refguide
					;junkWords.db
					;DocumentsForInvert.db 
					;readyDocumentTable.db
					;weightedInvertIndexTable.db
 

;; TEST THE SEARCH

(invertIndex.searchDocs "virtual machine")
(invertIndex.searchDocsWithStemWords "virtual machine")

(invertIndex.searchDocs "neural nets")
(invertIndex.searchDocsWithStemWords "neural nets")

(invertIndex.searchDocs "context functions")
(invertIndex.searchDocsWithStemWords "context functions")

(invertIndex.searchDocs "alice neural")
(invertIndex.searchDocsWithStemWords "alice neural")

(invertIndex.searchDocs "date")
(invertIndex.searchDocsWithStemWords "date")

(invertIndex.searchDocs "cumulus clouds")
(invertIndex.searchDocsWithStemWords "cumulus clouds")

(invertIndex.searchDocs "you are the one")
(invertIndex.searchDocsWithStemWords "you are the one")

true)





;;**EXPORTKEY**:invertIndex2
(defun invertIndex2()


;;************************************************************************
;; September 19, 2006 1:30
;;
;; invertIndex Lambda
;;
;;	
;;
;;For a more refined search, the following should be done:
;;	1. Return unstemmed words for highlighting at the display
;;	2. Utilize Wordnet Data such as Polysemy count, Part of Speech, others....
;;	3. Put priority weights on the words extracted from some XML tags. Fo example, the
;;		 words inside the <Title> </Title> tags should have higher weights than those inside 
;;		 <Description> </Description> tags
;;
;;		TO BE DONE LATER......
;;
;;***************************************************************************
;;
;; An implementation of Vector Space Search Engine
;;
;; At the bottommost of this program, you can copy test commands and execute them at the console 
;;
;; WordWeight is a based on frequency 
;;
;; Each word is represented by an axis in the vector space.
;; Each document is treated as a vector, where each coordinate is represented by the frequency of a word
;;
;; For example:
;;		Suppose the universal U set of documents consists only of 3 words: "cat", "dog", "mouse"
;;		Therefore, U is represented as a 3-tuple ("cat", "dog", "mouse"); each word is an axis in the vector space
;;		Vector (3,1,4) for a document X, will mean that at document X "cat" has a frequency of 3; "dog" has frequency of 1; "mouse" has frequency of 4.
;;		
;; The magnitude for each vector is computed using Phytagorean theorem.
;; If a search word is an element of U, then it can be represented as a unit vector. 
;;		For example, the search word "cat" will be represented as (1,0,0)
;;
;; The cosine of the angles formed by the search word unit vector and each of the document vectors are compared. 
;; The smaller the cosine of the angle, the more prioritized is the document that will be returned during a search.
;;
;;************************************************************************

pvars:( (myOntology "CoreContent")   	;; Where Documents are found    
		(String:myCurrentDocHTML)		;; HTML Version of Webpage
		 myCurrentDocXML				;; XML Version of Webpage   
		(String:myCurrentDocTEXT "")  	;; Webpage without HTML tags
		(Integer:currentDocNum)			;; current document evaluated
		docName

		;; Child Lambdas
		constructJunkwordsIndex
		removeJunkWords
		getFrequency

) ;; end pvars


vars:( (Integer:totalDocNum)		 	;; totla number of docs in CoreContent
		m currentWord freqNum valueVec valueVecLen currentPos
	 ) ;; end vars

 

(defun constructJunkwordsIndex()
	 vars:(repos   i entry)
    pvars: (junkwordindex)
	(setq repos (new ObjectRepository: "junkWords.db"))
	(clear repos)   

	(setq junkwordindex (new index junkwordindex: repos create: memory:)) 
	 
(setq junkwordindex["i'm"] 1)
(setq junkwordindex["web"] 1)
(setq junkwordindex["don't"] 1)
(setq junkwordindex["i've"] 1)
(setq junkwordindex["we've"] 1)
(setq junkwordindex["they've"] 1)
(setq junkwordindex["she's"] 1)
(setq junkwordindex["he's"] 1)
(setq junkwordindex["it's"] 1)
(setq junkwordindex["great"] 1)
(setq junkwordindex["old"] 1)
(setq junkwordindex["can't"] 1)
(setq junkwordindex["tell"] 1)
(setq junkwordindex["tells"] 1)
(setq junkwordindex["busy"] 1)
(setq junkwordindex["doesn't"] 1)
(setq junkwordindex["you're"] 1)
(setq junkwordindex["your's"] 1)
(setq junkwordindex["didn't"] 1)
(setq junkwordindex["they're"] 1)
(setq junkwordindex["night"] 1)
(setq junkwordindex["nights"] 1)
(setq junkwordindex["anyone"] 1)
(setq junkwordindex["isn't"] 1)
(setq junkwordindex["i'll"] 1)
(setq junkwordindex["actual"] 1)
(setq junkwordindex["actually"] 1)
(setq junkwordindex["presents"] 1)
(setq junkwordindex["presenting"] 1)
(setq junkwordindex["presenter"] 1)
(setq junkwordindex["present"] 1)
(setq junkwordindex["presented"] 1)
(setq junkwordindex["presentation"] 1)
(setq junkwordindex["we're"] 1)
(setq junkwordindex["wouldn't"] 1)
(setq junkwordindex["example"] 1)
(setq junkwordindex["examples"] 1)
(setq junkwordindex["i'd"] 1)
(setq junkwordindex["haven't"] 1)
(setq junkwordindex["etc"] 1)
(setq junkwordindex["won't"] 1)
(setq junkwordindex["myself"] 1)
(setq junkwordindex["we've"] 1)
(setq junkwordindex["they've"] 1)
(setq junkwordindex["aren't"] 1)
(setq junkwordindex["we'd"] 1)
(setq junkwordindex["it'd"] 1)
(setq junkwordindex["ain't"] 1)
(setq junkwordindex["i'll"] 1)
(setq junkwordindex["who've"] 1)
(setq junkwordindex["-year-old"] 1)
(setq junkwordindex["kind"] 1)
(setq junkwordindex["kinds"] 1)
(setq junkwordindex["builds"] 1)
(setq junkwordindex["build"] 1)
(setq junkwordindex["built"] 1)
(setq junkwordindex["com"] 1)
(setq junkwordindex["make"] 1)
(setq junkwordindex["makes"] 1)
(setq junkwordindex["making"] 1)
(setq junkwordindex["made"] 1)
(setq junkwordindex["you'll"] 1)
(setq junkwordindex["couldn't"] 1)
(setq junkwordindex["use"] 1)
(setq junkwordindex["uses"] 1)
(setq junkwordindex["used"] 1)
(setq junkwordindex["using"] 1)
(setq junkwordindex["take"] 1)
(setq junkwordindex["takes"] 1)
(setq junkwordindex["taking"] 1)
(setq junkwordindex["taken"] 1)
(setq junkwordindex["exactly"] 1)
(setq junkwordindex["we'll"] 1)
(setq junkwordindex["it'll"] 1)
(setq junkwordindex["certainly"] 1)
(setq junkwordindex["he'd"] 1)
(setq junkwordindex["shown"] 1)
(setq junkwordindex["they'd"] 1)
(setq junkwordindex["wasn't"] 1)
(setq junkwordindex["yeah"] 1)
(setq junkwordindex["to-day"] 1)
(setq junkwordindex["lya"] 1)
(setq junkwordindex["a"] 1)
(setq junkwordindex["ability"] 1)
(setq junkwordindex["able"] 1)
(setq junkwordindex["aboard"] 1)
(setq junkwordindex["about"] 1)
(setq junkwordindex["above"] 1)
(setq junkwordindex["absolute"] 1)
(setq junkwordindex["absolutely"] 1)
(setq junkwordindex["across"] 1)
(setq junkwordindex["act"] 1)
(setq junkwordindex["acts"] 1)
(setq junkwordindex["add"] 1)
(setq junkwordindex["additional"] 1)
(setq junkwordindex["additionally"] 1)
(setq junkwordindex["after"] 1)
(setq junkwordindex["afterwards"] 1)
(setq junkwordindex["again"] 1)
(setq junkwordindex["against"] 1)
(setq junkwordindex["ago"] 1)
(setq junkwordindex["ahead"] 1)
(setq junkwordindex["aimless"] 1)
(setq junkwordindex["aimlessly"] 1)
(setq junkwordindex["al"] 1)
(setq junkwordindex["albeit"] 1)
(setq junkwordindex["align"] 1)
(setq junkwordindex["all"] 1)
(setq junkwordindex["allow"] 1)
(setq junkwordindex["almost"] 1)
(setq junkwordindex["along"] 1)
(setq junkwordindex["alongside"] 1)
(setq junkwordindex["already"] 1)
(setq junkwordindex["also"] 1)
(setq junkwordindex["alternate"] 1)
(setq junkwordindex["alternately"] 1)
(setq junkwordindex["although"] 1)
(setq junkwordindex["always"] 1)
(setq junkwordindex["am"] 1)
(setq junkwordindex["amid"] 1)
(setq junkwordindex["amidst"] 1)
(setq junkwordindex["among"] 1)
(setq junkwordindex["amongst"] 1)
(setq junkwordindex["an"] 1)
(setq junkwordindex["and"] 1)
(setq junkwordindex["announce"] 1)
(setq junkwordindex["announced"] 1)
(setq junkwordindex["announcement"] 1)
(setq junkwordindex["announces"] 1)
(setq junkwordindex["another"] 1)
(setq junkwordindex["anti"] 1)
(setq junkwordindex["any"] 1)
(setq junkwordindex["anything"] 1)
(setq junkwordindex["appaling"] 1)
(setq junkwordindex["appalingly"] 1)
(setq junkwordindex["appear"] 1)
(setq junkwordindex["appeared"] 1)
(setq junkwordindex["appears"] 1)
(setq junkwordindex["are"] 1)
(setq junkwordindex["around"] 1)
(setq junkwordindex["as"] 1)
(setq junkwordindex["ask"] 1)
(setq junkwordindex["asked"] 1)
(setq junkwordindex["asking"] 1)
(setq junkwordindex["asks"] 1)
(setq junkwordindex["at"] 1)
(setq junkwordindex["await"] 1)
(setq junkwordindex["awaited"] 1)
(setq junkwordindex["awaits"] 1)
(setq junkwordindex["awaken"] 1)
(setq junkwordindex["awakened"] 1)
(setq junkwordindex["awakens"] 1)
(setq junkwordindex["aware"] 1)
(setq junkwordindex["away"] 1)
(setq junkwordindex["b"] 1)
(setq junkwordindex["back"] 1)
(setq junkwordindex["backed"] 1)
(setq junkwordindex["backing"] 1)
(setq junkwordindex["backs"] 1)
(setq junkwordindex["be"] 1)
(setq junkwordindex["became"] 1)
(setq junkwordindex["because"] 1)
(setq junkwordindex["become"] 1)
(setq junkwordindex["becomes"] 1)
(setq junkwordindex["becoming"] 1)
(setq junkwordindex["been"] 1)
(setq junkwordindex["before"] 1)
(setq junkwordindex["began"] 1)
(setq junkwordindex["begin"] 1)
(setq junkwordindex["begins"] 1)
(setq junkwordindex["behind"] 1)
(setq junkwordindex["being"] 1)
(setq junkwordindex["believe"] 1)
(setq junkwordindex["believed"] 1)
(setq junkwordindex["between"] 1)
(setq junkwordindex["both"] 1)
(setq junkwordindex["brang"] 1)
(setq junkwordindex["bring"] 1)
(setq junkwordindex["brings"] 1)
(setq junkwordindex["brought"] 1)
(setq junkwordindex["but"] 1)
(setq junkwordindex["by"] 1)
(setq junkwordindex["c"] 1)
(setq junkwordindex["call"] 1)
(setq junkwordindex["called"] 1)
(setq junkwordindex["calling"] 1)
(setq junkwordindex["calls"] 1)
(setq junkwordindex["can"] 1)
(setq junkwordindex["cannot"] 1)
(setq junkwordindex["carried"] 1)
(setq junkwordindex["carries"] 1)
(setq junkwordindex["carry"] 1)
(setq junkwordindex["carrying"] 1)
(setq junkwordindex["change"] 1)
(setq junkwordindex["changed"] 1)
(setq junkwordindex["changes"] 1)
(setq junkwordindex["choose"] 1)
(setq junkwordindex["chooses"] 1)
(setq junkwordindex["chose"] 1)
(setq junkwordindex["clearly"] 1)
(setq junkwordindex["close"] 1)
(setq junkwordindex["closed"] 1)
(setq junkwordindex["closes"] 1)
(setq junkwordindex["closing"] 1)
(setq junkwordindex["come"] 1)
(setq junkwordindex["comes"] 1)
(setq junkwordindex["coming"] 1)
(setq junkwordindex["consider"] 1)
(setq junkwordindex["considerable"] 1)
(setq junkwordindex["considering"] 1)
(setq junkwordindex["could"] 1)
(setq junkwordindex["couldn"] 1)
(setq junkwordindex["d"] 1)
(setq junkwordindex["dare"] 1)
(setq junkwordindex["daren"] 1)
(setq junkwordindex["day"] 1)
(setq junkwordindex["days"] 1)
(setq junkwordindex["despite"] 1)
(setq junkwordindex["did"] 1)
(setq junkwordindex["didn"] 1)
(setq junkwordindex["do"] 1)
(setq junkwordindex["does"] 1)
(setq junkwordindex["doesn"] 1)
(setq junkwordindex["doing"] 1)
(setq junkwordindex["done"] 1)
(setq junkwordindex["down"] 1)
(setq junkwordindex["downward"] 1)
(setq junkwordindex["downwards"] 1)
(setq junkwordindex["e"] 1)
(setq junkwordindex["each"] 1)
(setq junkwordindex["eight"] 1)
(setq junkwordindex["either"] 1)
(setq junkwordindex["else"] 1)
(setq junkwordindex["elsewhere"] 1)
(setq junkwordindex["especially"] 1)
(setq junkwordindex["even"] 1)
(setq junkwordindex["eventually"] 1)
(setq junkwordindex["ever"] 1)
(setq junkwordindex["every"] 1)
(setq junkwordindex["everybody"] 1)
(setq junkwordindex["everyone"] 1)
(setq junkwordindex["f"] 1)
(setq junkwordindex["far"] 1)
(setq junkwordindex["feel"] 1)
(setq junkwordindex["felt"] 1)
(setq junkwordindex["few"] 1)
(setq junkwordindex["final"] 1)
(setq junkwordindex["finally"] 1)
(setq junkwordindex["find"] 1)
(setq junkwordindex["five"] 1)
(setq junkwordindex["for"] 1)
(setq junkwordindex["found"] 1)
(setq junkwordindex["four"] 1)
(setq junkwordindex["fourth"] 1)
(setq junkwordindex["from"] 1)
(setq junkwordindex["get"] 1)
(setq junkwordindex["gets"] 1)
(setq junkwordindex["getting"] 1)
(setq junkwordindex["gave"] 1)
(setq junkwordindex["give"] 1)
(setq junkwordindex["gives"] 1)
(setq junkwordindex["go"] 1)
(setq junkwordindex["goes"] 1)
(setq junkwordindex["going"] 1)
(setq junkwordindex["gone"] 1)
(setq junkwordindex["good"] 1)
(setq junkwordindex["got"] 1)
(setq junkwordindex["h"] 1)
(setq junkwordindex["had"] 1)
(setq junkwordindex["has"] 1)
(setq junkwordindex["have"] 1)
(setq junkwordindex["he"] 1)
(setq junkwordindex["held"] 1)
(setq junkwordindex["her"] 1)
(setq junkwordindex["here"] 1)
(setq junkwordindex["heretofore"] 1)
(setq junkwordindex["hereby"] 1)
(setq junkwordindex["herewith"] 1)
(setq junkwordindex["hers"] 1)
(setq junkwordindex["herself"] 1)
(setq junkwordindex["high"] 1)
(setq junkwordindex["him"] 1)
(setq junkwordindex["himself"] 1)
(setq junkwordindex["his"] 1)
(setq junkwordindex["hitherto"] 1)
(setq junkwordindex["happen"] 1)
(setq junkwordindex["happened"] 1)
(setq junkwordindex["happens"] 1)
(setq junkwordindex["hour"] 1)
(setq junkwordindex["hours"] 1)
(setq junkwordindex["how"] 1)
(setq junkwordindex["however"] 1)
(setq junkwordindex["i"] 1)
(setq junkwordindex["ii"] 1)
(setq junkwordindex["iii"] 1)
(setq junkwordindex["iv"] 1)
(setq junkwordindex["if"] 1)
(setq junkwordindex["in"] 1)
(setq junkwordindex["include"] 1)
(setq junkwordindex["included"] 1)
(setq junkwordindex["includes"] 1)
(setq junkwordindex["including"] 1)
(setq junkwordindex["inside"] 1)
(setq junkwordindex["into"] 1)
(setq junkwordindex["is"] 1)
(setq junkwordindex["isn"] 1)
(setq junkwordindex["it"] 1)
(setq junkwordindex["its"] 1)
(setq junkwordindex["itself"] 1)
(setq junkwordindex["j"] 1)
(setq junkwordindex["just"] 1)
(setq junkwordindex["k"] 1)
(setq junkwordindex["l"] 1)
(setq junkwordindex["la"] 1)
(setq junkwordindex["larger"] 1)
(setq junkwordindex["largest"] 1)
(setq junkwordindex["last"] 1)
(setq junkwordindex["later"] 1)
(setq junkwordindex["latest"] 1)
(setq junkwordindex["le"] 1)
(setq junkwordindex["least"] 1)
(setq junkwordindex["leave"] 1)
(setq junkwordindex["leaves"] 1)
(setq junkwordindex["leaving"] 1)
(setq junkwordindex["les"] 1)
(setq junkwordindex["let"] 1)
(setq junkwordindex["less"] 1)
(setq junkwordindex["like"] 1)
(setq junkwordindex["ll"] 1)
(setq junkwordindex["m"] 1)
(setq junkwordindex["made"] 1)
(setq junkwordindex["main"] 1)
(setq junkwordindex["mainly"] 1)
(setq junkwordindex["make"] 1)
(setq junkwordindex["makes"] 1)
(setq junkwordindex["man"] 1)
(setq junkwordindex["many"] 1)
(setq junkwordindex["may"] 1)
(setq junkwordindex["me"] 1)
(setq junkwordindex["means"] 1)
(setq junkwordindex["meant"] 1)
(setq junkwordindex["meanwhile"] 1)
(setq junkwordindex["men"] 1)
(setq junkwordindex["might"] 1)
(setq junkwordindex["missed"] 1)
(setq junkwordindex["more"] 1)
(setq junkwordindex["moreover"] 1)
(setq junkwordindex["most"] 1)
(setq junkwordindex["mostly"] 1)
(setq junkwordindex["move"] 1)
(setq junkwordindex["moved"] 1)
(setq junkwordindex["moving"] 1)
(setq junkwordindex["mr"] 1)
(setq junkwordindex["mrs"] 1)
(setq junkwordindex["much"] 1)
(setq junkwordindex["must"] 1)
(setq junkwordindex["mustn"] 1)
(setq junkwordindex["my"] 1)
(setq junkwordindex["need"] 1)
(setq junkwordindex["needs"] 1)
(setq junkwordindex["neither"] 1)
(setq junkwordindex["never"] 1)
(setq junkwordindex["new"] 1)
(setq junkwordindex["newer"] 1)
(setq junkwordindex["news"] 1)
(setq junkwordindex["nine"] 1)
(setq junkwordindex["no"] 1)
(setq junkwordindex["non"] 1)
(setq junkwordindex["none"] 1)
(setq junkwordindex["nor"] 1)
(setq junkwordindex["not"] 1)
(setq junkwordindex["now"] 1)
(setq junkwordindex["o"] 1)
(setq junkwordindex["of"] 1)
(setq junkwordindex["off"] 1)
(setq junkwordindex["often"] 1)
(setq junkwordindex["on"] 1)
(setq junkwordindex["once"] 1)
(setq junkwordindex["one"] 1)
(setq junkwordindex["only"] 1)
(setq junkwordindex["or"] 1)
(setq junkwordindex["other"] 1)
(setq junkwordindex["our"] 1)
(setq junkwordindex["out"] 1)
(setq junkwordindex["over"] 1)
(setq junkwordindex["own"] 1)
(setq junkwordindex["owns"] 1)
(setq junkwordindex["p"] 1)
(setq junkwordindex["particularly"] 1)
(setq junkwordindex["per"] 1)
(setq junkwordindex["percent"] 1)
(setq junkwordindex["primarily"] 1)
(setq junkwordindex["put"] 1)
(setq junkwordindex["q"] 1)
(setq junkwordindex["quickly"] 1)
(setq junkwordindex["r"] 1)
(setq junkwordindex["remain"] 1)
(setq junkwordindex["remaining"] 1)
(setq junkwordindex["respond"] 1)
(setq junkwordindex["responded"] 1)
(setq junkwordindex["responding"] 1)
(setq junkwordindex["responds"] 1)
(setq junkwordindex["return"] 1)
(setq junkwordindex["ran"] 1)
(setq junkwordindex["rather"] 1)
(setq junkwordindex["run"] 1)
(setq junkwordindex["running"] 1)
(setq junkwordindex["runs"] 1)
(setq junkwordindex["s"] 1)
(setq junkwordindex["said"] 1)
(setq junkwordindex["say"] 1)
(setq junkwordindex["says"] 1)
(setq junkwordindex["same"] 1)
(setq junkwordindex["see"] 1)
(setq junkwordindex["seek"] 1)
(setq junkwordindex["seeking"] 1)
(setq junkwordindex["seeks"] 1)
(setq junkwordindex["seen"] 1)
(setq junkwordindex["send"] 1)
(setq junkwordindex["sent"] 1)
(setq junkwordindex["set"] 1)
(setq junkwordindex["sets"] 1)
(setq junkwordindex["seven"] 1)
(setq junkwordindex["several"] 1)
(setq junkwordindex["she"] 1)
(setq junkwordindex["should"] 1)
(setq junkwordindex["shouldn"] 1)
(setq junkwordindex["side"] 1)
(setq junkwordindex["since"] 1)
(setq junkwordindex["six"] 1)
(setq junkwordindex["sixes"] 1)
(setq junkwordindex["slow"] 1)
(setq junkwordindex["slowed"] 1)
(setq junkwordindex["slows"] 1)
(setq junkwordindex["small"] 1)
(setq junkwordindex["smaller"] 1)
(setq junkwordindex["so"] 1)
(setq junkwordindex["some"] 1)
(setq junkwordindex["someone"] 1)
(setq junkwordindex["something"] 1)
(setq junkwordindex["somewhat"] 1)
(setq junkwordindex["somewhere"] 1)
(setq junkwordindex["soon"] 1)
(setq junkwordindex["sought"] 1)
(setq junkwordindex["spread"] 1)
(setq junkwordindex["stay"] 1)
(setq junkwordindex["stayed"] 1)
(setq junkwordindex["still"] 1)
(setq junkwordindex["substantially"] 1)
(setq junkwordindex["such"] 1)
(setq junkwordindex["suppose"] 1)
(setq junkwordindex["t"] 1)
(setq junkwordindex["take"] 1)
(setq junkwordindex["takes"] 1)
(setq junkwordindex["taken"] 1)
(setq junkwordindex["th"] 1)
(setq junkwordindex["than"] 1)
(setq junkwordindex["that"] 1)
(setq junkwordindex["the"] 1)
(setq junkwordindex["their"] 1)
(setq junkwordindex["them"] 1)
(setq junkwordindex["themselves"] 1)
(setq junkwordindex["then"] 1)
(setq junkwordindex["there"] 1)
(setq junkwordindex["thereby"] 1)
(setq junkwordindex["therefore"] 1)
(setq junkwordindex["these"] 1)
(setq junkwordindex["they"] 1)
(setq junkwordindex["thing"] 1)
(setq junkwordindex["things"] 1)
(setq junkwordindex["thi"] 1)
(setq junkwordindex["this"] 1)
(setq junkwordindex["those"] 1)
(setq junkwordindex["though"] 1)
(setq junkwordindex["thus"] 1)
(setq junkwordindex["three"] 1)
(setq junkwordindex["through"] 1)
(setq junkwordindex["throughout"] 1)
(setq junkwordindex["to"] 1)
(setq junkwordindex["together"] 1)
(setq junkwordindex["too"] 1)
(setq junkwordindex["took"] 1)
(setq junkwordindex["toward"] 1)
(setq junkwordindex["towards"] 1)
(setq junkwordindex["tried"] 1)
(setq junkwordindex["tries"] 1)
(setq junkwordindex["try"] 1)
(setq junkwordindex["trying"] 1)
(setq junkwordindex["two"] 1)
(setq junkwordindex["u"] 1)
(setq junkwordindex["unable"] 1)
(setq junkwordindex["under"] 1)
(setq junkwordindex["underneath"] 1)
(setq junkwordindex["undid"] 1)
(setq junkwordindex["undo"] 1)
(setq junkwordindex["undoes"] 1)
(setq junkwordindex["undone"] 1)
(setq junkwordindex["undue"] 1)
(setq junkwordindex["undoubtedly"] 1)
(setq junkwordindex["unfortunately"] 1)
(setq junkwordindex["unless"] 1)
(setq junkwordindex["unnecessarily"] 1)
(setq junkwordindex["unofficially"] 1)
(setq junkwordindex["until"] 1)
(setq junkwordindex["unusually"] 1)
(setq junkwordindex["unsure"] 1)
(setq junkwordindex["up"] 1)
(setq junkwordindex["upon"] 1)
(setq junkwordindex["upward"] 1)
(setq junkwordindex["us"] 1)
(setq junkwordindex["use"] 1)
(setq junkwordindex["used"] 1)
(setq junkwordindex["uses"] 1)
(setq junkwordindex["using"] 1)
(setq junkwordindex["usual"] 1)
(setq junkwordindex["usually"] 1)
(setq junkwordindex["v"] 1)
(setq junkwordindex["ve"] 1)
(setq junkwordindex["very"] 1)
(setq junkwordindex["via"] 1)
(setq junkwordindex["view"] 1)
(setq junkwordindex["viewed"] 1)
(setq junkwordindex["w"] 1)
(setq junkwordindex["wait"] 1)
(setq junkwordindex["waited"] 1)
(setq junkwordindex["waits"] 1)
(setq junkwordindex["want"] 1)
(setq junkwordindex["wanted"] 1)
(setq junkwordindex["wants"] 1)
(setq junkwordindex["was"] 1)
(setq junkwordindex["wasn"] 1)
(setq junkwordindex["watched"] 1)
(setq junkwordindex["watching"] 1)
(setq junkwordindex["way"] 1)
(setq junkwordindex["ways"] 1)
(setq junkwordindex["we"] 1)
(setq junkwordindex["went"] 1)
(setq junkwordindex["were"] 1)
(setq junkwordindex["what"] 1)
(setq junkwordindex["whatever"] 1)
(setq junkwordindex["when"] 1)
(setq junkwordindex["whenever"] 1)
(setq junkwordindex["where"] 1)
(setq junkwordindex["whereever"] 1)
(setq junkwordindex["whether"] 1)
(setq junkwordindex["which"] 1)
(setq junkwordindex["whichever"] 1)
(setq junkwordindex["while"] 1)
(setq junkwordindex["who"] 1)
(setq junkwordindex["whoever"] 1)
(setq junkwordindex["whom"] 1)
(setq junkwordindex["whomsoever"] 1)
(setq junkwordindex["whose"] 1)
(setq junkwordindex["whosever"] 1)
(setq junkwordindex["why"] 1)
(setq junkwordindex["wide"] 1)
(setq junkwordindex["wider"] 1)
(setq junkwordindex["will"] 1)
(setq junkwordindex["with"] 1)
(setq junkwordindex["without"] 1)
(setq junkwordindex["won"] 1)
(setq junkwordindex["would"] 1)
(setq junkwordindex["wouldn"] 1)
(setq junkwordindex["wow"] 1)
(setq junkwordindex["wows"] 1)
(setq junkwordindex["www"] 1)
(setq junkwordindex["x"] 1)
(setq junkwordindex["xii"] 1)
(setq junkwordindex["xiii"] 1)
(setq junkwordindex["xiv"] 1)
(setq junkwordindex["xv"] 1)
(setq junkwordindex["xvi"] 1)
(setq junkwordindex["xvii"] 1)
(setq junkwordindex["xviii"] 1)
(setq junkwordindex["xix"] 1)
(setq junkwordindex["xx"] 1)
(setq junkwordindex["y"] 1)
(setq junkwordindex["year"] 1)
(setq junkwordindex["you"] 1)
(setq junkwordindex["your"] 1)
(setq junkwordindex["yours"] 1)
(setq junkwordindex["yourself"] 1)
(setq junkwordindex["yourselves"] 1)


(setq junkwordindex["|'|"] 1)

;(junkwordindex.Pv.__showTree)
(junkwordindex.Pv.save)
 
(writeln)
(writeln "--------------------------------------------------------")
(writeln "Junk Words saved on junkWords.db")
)


;;=================
;; getFrequency
;;===================

(defun getFrequency(currentWord V) 
	 vars:(     freqNum   vecNum     V currentWord
		  ) ;; end vars
pvars:(indexVec)
 
	 (setq indexVec 0)
		(setq freqNum 0)
		(setq vecNum 0)
		(setq indexVec (member currentWord V))
 
		(if (isInteger indexVec)
			(begin
				
				 
				(while (= V[indexVec] currentWord) do (++ freqNum) (++ indexVec))
			 ) ;; end begin
		) ;; end if
freqNum
);; end getFrequency



	;; =====================
	;; MAIN LOGIC
	;; ======================

	(browseLib.setFocus myOntology)
	(setq ontologyNames (^new Vector: 0))
	(setq wordFrequencyVec (^new Vector: ))
	(setq ontologyNames (browseLib.getChildNames))
	(setq totalDocNum (length ontologyNames))
	(constructJunkwordsIndex)


	(if (= wordREPOS #void)
		(begin
			(setq wordREPOS (new ObjectRepository: "wordREPOS.db"))
			(clear wordREPOS)
		) ;; end begin   
		else
		(begin
			(setq wordREPOS (new ObjectRepository: "wordREPOS.db"))		 
		)
	) ;; end if

	(if (= docREPOS #void)
		(begin
			(setq docREPOS (new ObjectRepository: "docREPOS.db"))
			(clear docREPOS)
		) ;; end begin   
		else
 		(begin
			(setq docREPOS (new ObjectRepository: "docREPOS.db"))
		) ;; end begin
	) ;; end if

	(if (= freqREPOS #void)
		(begin
			(setq freqREPOS (new ObjectRepository: "freqREPOS.db"))
			(clear freqREPOS)
		) ;; end begin   
		else
		(begin
			(setq freqREPOS (new ObjectRepository: "freqREPOS.db"))		 
		) ;; end begin   

	) ;; end if
 
	(beginTransaction wordREPOS)
	(beginTransaction docREPOS)
	(beginTransaction freqREPOS)
 
	;(setq value1 (new Vector: Byte: 1000))
 
	;; Initialize the variables
	(setq myWordIndex (new index myWordIndex: wordREPOS create: )) 
	(setq myDocIndex (new index myDocIndex: docREPOS create: )) 
	(setq myFreqIndex (new index myDocIndex: freqREPOS create: )) 


 
	(loop for currentDocNum from 0 until totalDocNum do
		(setq myCurrentDocTXTVEC (new Vector:))
		(setq myCurrentDocHTML (new String: ""))
		(setq myCurrentDocXML (^new Structure:))
		(setq freqVec (new Vector: Integer:))
		 
        (setq myCurrentDocHTML (browseLib.checkout myOntology ontologyNames[currentDocNum]))	
		(writeln "checking out: ======  " ontologyNames[currentDocNum]  " ====== " )
		(setq docName ontologyNames[currentDocNum])

		(setq myCurrentDocXML (xml myCurrentDocHTML))
  
		(setq porterStemmer.htmlTagsOn false)
		(setq myCurrentDocHTML  (porterStemmer myCurrentDocHTML 1))		 
  	 
		(setq myCurrentDocTXTVEC (parseLib.defaultLexer myCurrentDocHTML))
	 		 
		(setq myCurrentDocTXTVEC (sort myCurrentDocTXTVEC <))
		(setq lengthVec (length myCurrentDocTXTVEC))
 
		(setq m 0)
		(setq f 0)
			
		;; Routine to loop over all the words in the document
		FetchWord::
		(if (> m lengthVec) (goto End:))
		(setq currentWord myCurrentDocTXTVEC[m])
 		 
		;; Remove Junk words and Non words
		(if (= (isCharName currentWord) false)  (begin (++ m)  (goto FetchWord:)))
		(if (junkwordindex.Pv.isMember currentWord) (begin (++ m) (goto FetchWord:)))
		 
		(setq freqNum (getFrequency currentWord myCurrentDocTXTVEC))
		(if (> freqNum 1) (begin (setq m indexVec)  ))
			
		(setq wordLen (length currentWord))
	 
		(setq freqVec f freqNum)
		(++ f)
		(setq key1 (ref currentWord 0))
		
		 
		;; Set value into wordIndex
		(setq myWordIndex[key1] currentWord)
		(setq myDocIndex[currentDocNum] currentWord)
		;(setq myFreqIndex[currentDocNum] freqNum)

		End::
		(if (<= m lengthVec) 
			(begin 
				(if (isExact (ndiv m 200)) 
					(begin 					
						(myWordIndex.Pv.save) 						 
						(myDocIndex.Pv.save)				 
					) ;; end begin
				) ;; end if
				(++ m) 
				(goto FetchWord:)
			) ;; end begin
		) ;; end if
		(myWordIndex.Pv.save)
		(myDocIndex.Pv.save)
		
	 
		;; End Routine 
	 
		(setq myFreqIndex[currentDocNum] freqVec)
		(myFreqIndex.Pv.save)

	) ;; end loop for document

 

) ;; end invertIndex2







;;**EXPORTKEY**:invertIndex3
(defun invertIndex3()


	;; =====================
	;; MAIN LOGIC
	;; ======================

vars: (key word num)
regs: ((Integer:n 0) (CharPointer:kp)(CharPointer:wp) cc  )
pvars:(generateRandomWord)

	(if (= wordREPOS #void)
		(begin
			(setq wordREPOS (new ObjectRepository: "wordREPOS.db"))
			(clear wordREPOS)
		) ;; end begin   
		else
	) ;; end if

 
	(beginTransaction wordREPOS)
	(setq key (new String: ""))
 
	(loop for num from 0 until 14000000 do
		;; Generate random word
		(setq word (invertIndex3.generateRandomWord))
		(setq wp word)
		(setq kp key)
		(setq n 0)
		;; Start load
		;; Get the first two letters of the word as key
		(vmregRunInHardware start:)
        (while (< n 2)  do (setq cc wp[n]) (setq kp[n] cc) (setq kp[(++ n)] 0))
		(vmregRunInHardware stop:)	 
 
		(setq wordREPOS[key] word)
(writeln "word: " word " - key: " key)
	 	(setq kp[0] 0)
	    (if (isExact (ndiv num 500000)) 
			(begin	 
				(checkPointTransaction wordREPOS)		 
			) 
		) ;; end if
	) ;; end loop


) ;; end invertIndex3






;;**EXPORTKEY**:invertIndex3:generateRandomWord
(defchild invertIndex3:generateRandomWord()
;;**********************************************
;; This child Lambda generates random words 
;; with characters from a-z and 1-9 for the 
;; purpose of our invert index prototype Lambda
;;**********************************************
vars: ( (charVector "abcdefghijklmnopqrstuvwxyz123456789")
		(maxString  "abcdefghijklmnopqrstuvwxyz123456789abcdefghijklmnopqrstuvwxyz123456789abcdefghijklmnopqrstuvwxyz123456789")
		word
	  ) ;; end vars
regs:( (Integer:n 0)
	   (CharPointer:cp)
	   (CharPointer:wp)
       (Integer:charIndex)        ;; MFK: Let's use registers for these variables
       (Integer:wordLength)       ;; MFK: Let's use registers for these variables
       (Integer:maxCharacters 36) ;; MFK: Let's use registers for these constants
       (Integer:maxWordLength 20) ;; MFK: Let's use registers for these constants
) ;; end regs

(setq cp charVector)
(setq maxCharacters (length charVector))
(setq wordLength (integer (random maxWordLength)))
(setq word (new String: (left maxString (+ 2 wordLength)))) ;; MFK: If we don't preallocate word to the correct size, we will overwrite memory.
(setq wp word)
(setq wp[0] 0)
(setq charIndex (integer (random maxCharacters)))
 
;; Here we loop through the word string setting characters at random always ending with the proper null character. 
(while (<= n wordLength) do  ;; MFK: We need at least one character to make a word.
	(setq charIndex (integer (random maxCharacters)))
	(setq wp[n] cp[charIndex])
	(++ n)
	(setq wp[n] 0)
) ;; end while


word)




