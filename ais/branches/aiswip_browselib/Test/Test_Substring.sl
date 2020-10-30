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
;;  Title:    Lisp Substring Procedures Test
;;
;;  Author:   Michael F. Korns
;;			  Franklin Chua
;;			  Tim May
;;
;;  Project:  AIS Regression Suite
;;
;;  Notes:    The SmartBase features in the Substring Procedures
;;            chapter are tested in this test suite script.
;;
;;  Files:    RegTest.sl
;;

;#memory=500
;#memoryObjectHeaders=100
(runScript "RegTest.sl")
(testStart "Test_Substring.sl")

;;************************************************************************
;;  Substring Creation
;;************************************************************************
(setq strBase (new String: "Hello Lisp world!"))
(setq byteVecBase (new ByteVector: 12 72 101 108 108 111 32 119 111 114 108 100 33))
(setq symBase ThisIsARegularSymbol:)
(setq quotedSymBase (makeSymbol "This is a quoted symbol"))

(testit "(type (setq strSubstr (new Substring: strBase 0 5)))" Substring:)
(testit "(type (setq byteSubstr (new Substring: byteVecBase 0 5)))" Substring:)
(testit "(type (setq symSubstr (new Substring: symBase 7 7)))" Substring:)
(testit "(type (setq quotedSubstr (new Substring: quotedSymBase 17 6)))" Substring:)

;;************************************************************************
;;  Substring to String Conversion
;;************************************************************************
(testit "(string strSubstr)" "Hello")
(testit "(string byteSubstr)" "Hello")
(testit "(string symSubstr)" "Regular")
(testit "(string quotedSubstr)" "symbol")

;;************************************************************************
;;  Substring Length
;;************************************************************************
(testit "(length strSubstr)" 5)
(testit "(length byteSubstr)" 5)
(testit "(length symSubstr)" 7)
(testit "(length quotedSubstr)" 6)

;;************************************************************************
;;  Parse Function
;;************************************************************************
(setq strBase (new String: "1024 ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(setq strSubstr (new Substring: strBase 0 4))
(testit "(type (parse strSubstr))" Integer:)
(testit "(parse strSubstr)" 1024)
(setq strBase (new String: "ABCDEFGHIJ 123.45"))
(setq strSubstr (new Substring: strBase 11 6))
(testit "(type (parse strSubstr))" Number:)
(testit "(parse strSubstr)" 123.45)

;;************************************************************************
;;  AppendWriteln Function
;;************************************************************************
(setq outputByteVector (new ByteVector: 1024))
(setq outputByteVector[0] 0)
(testit "(type (appendWriteln outputByteVector strSubstr))" ByteVector:)
(testit "(string outputByteVector)" "123.45")

;;************************************************************************
;;  ObjectToStructure Function
;;************************************************************************
(setq vec1 (new Vector: 1))
(setq vec2 (new Vector: 1))
(setq mat1 (new Matrix: 1))
(setq mat2 (new Matrix: 1))
(setq objvec1 (new Vector: Object: 1))
(setq objvec2 (new Vector: Object: 1))

(setq strKeyBase (string "Set of Keys" true))
(setq strValue (string "Value"))
(testit "(setq strKeySubstr (new Substring: strKeyBase 8 3))" "Key")

(setq vec1[0] strKeySubstr)
(setq vec2[0] strValue)
(setq mat1[0] strKeySubstr)
(setq mat2[0] strValue)
(setq objvec1[0] strKeySubstr)
(setq objvec2[0] strValue)

(testit "(type (setq tmpStruct (objectToStructure vec1 vec2)))" Structure:)
(testit "(string tmpStruct true)" "#{Key: \"Value\"}")
(testit "(type (setq tmpStruct (objectToStructure vec1 mat2)))" Structure:)
(testit "(string tmpStruct true)" "#{Key: \"Value\"}")
(testit "(type (setq tmpStruct (objectToStructure vec1 objvec2)))" Structure:)
(testit "(string tmpStruct true)" "#{Key: \"Value\"}")

(testit "(type (setq tmpStruct (objectToStructure mat1 mat2)))" Structure:)
(testit "(string tmpStruct true)" "#{Key: \"Value\"}")
(testit "(type (setq tmpStruct (objectToStructure mat1 objvec2)))" Structure:)
(testit "(string tmpStruct true)" "#{Key: \"Value\"}")
(testit "(type (setq tmpStruct (objectToStructure mat1 vec2)))" Structure:)
(testit "(string tmpStruct true)" "#{Key: \"Value\"}")

(testit "(type (setq tmpStruct (objectToStructure objvec1 objvec2)))" Structure:)
(testit "(string tmpStruct true)" "#{Key: \"Value\"}")
(testit "(type (setq tmpStruct (objectToStructure objvec1 vec2)))" Structure:)
(testit "(string tmpStruct true)" "#{Key: \"Value\"}")
(testit "(type (setq tmpStruct (objectToStructure objvec1 mat2)))" Structure:)
(testit "(string tmpStruct true)" "#{Key: \"Value\"}")

;;************************************************************************
;;  Compress Function
;;************************************************************************
(setq strBase "abcde fghij klmno pqrst uvwxy z")
(testit "(setq strSubstr (new Substring: strBase 0 11))" "abcde fghij")
(testit "(type (setq compressed (compress strSubstr)))" ByteVector:)
(testit "(setq uncompressed (uncompress compressed))" "abcde fghij")

;;************************************************************************
;;  Date/Time Functions
;;************************************************************************
(setq strTimeBase "abcde 10:11:12 fghij")
(setq strDateBase "abcde 8/8/2008 fghij")
(setq myTimeBase (time 10 11 12))
(setq myDateBase (date 1983 10 28))
(setq strDateSym "... #Oct,28,1983 ...")

(testit "(setq timeSubstr (new Substring: strTimeBase 6 8))" "10:11:12")
(testit "(type (setq myTime (time timeSubstr)))" Number:)
(testit "myTime" myTimeBase)
(testit "(hour timeSubstr)" 10)
(testit "(minute timeSubstr)" 11)
(testit "(second timeSubstr)" 12)

(testit "(setq dateSubstr (new Substring: strDateBase 6 8))" "8/8/2008")
(testit "(day dateSubstr)" 8)
(testit "(month dateSubstr)" 8)
(testit "(year dateSubstr)" 2008)

(testit "(setq dateSubstr (new Substring: strDateSym 4 12))" "#Oct,28,1983")
(testit "(type (setq myDate (date dateSubstr)))" Date:)
(testit "myDate" myDateBase)

;;************************************************************************
;;  Lisp Functions
;;************************************************************************
;(setq strLispBase "abcde 1 + 2 fghij")
;(testit "(setq strSubstr (new Substring: strLispBase 6 5))" "1 + 2")
;(testit "(type (setq pairObj (lisp strSubstr)))" Pair:)
;(testit "(string pairObj true)" "(1 + 2)")

;;************************************************************************
;;  MakeString Function
;;************************************************************************
(setq strBase "abcde fghij klmno pqrst")
(testit "(setq strSubstr (new Substring: strBase 6 5))" "fghij")
(testit "(setq strNew (makeString strSubstr))" "fghij")

;;************************************************************************
;;  MakeSymbol Function
;;************************************************************************
(testit "(setq strSubstr (new Substring: strBase 12 5))" "klmno")
(testit "(type (setq symNew (makeSymbol strSubstr)))" Symbol:)
(testit "symNew" "klmno")

;;************************************************************************
;;  HashString Function
;;************************************************************************
(testit "(setq strSubstr (new Substring: strBase 18 5))" "pqrst")
(setq hashResult (hashString "pqrst"))
(testit "(hashString strSubstr)" hashResult)

;;************************************************************************
;;  cStringVector Function
;;************************************************************************
(setq string1 (makeString "Hello"))
(setq string2 (makeString "World"))
(setq substr1 (makeSubstring string1 0 5))
(setq substr2 (makeSubstring string2 0 5))
(setq symbol1 (makeSymbol "Hello"))
(setq symbol2 (makeSymbol "World"))
(setq byteVec1 (new ByteVector: 5 72 101 108 108 111))
(setq byteVec2 (new ByteVector: 5 87 111 114 108 100))



;; Comparison between Substrings
(setq argVec1 (new Vector: 4 substr1 substr1 substr2 substr2))
(setq srcVec1 (new Vector: 4 substr1 substr2 substr1 substr2))
(setq dstVec1 (new Vector: Integer: 4 0))
(cStringVector argVec1 srcVec1 dstVec1)

(testit "(type (cStringVector argVec1 srcVec1 dstVec1))" IntVector:)
(testit "(= dstVec1[0] 0)" true)
(testit "(< dstVec1[1] 0)" true)
(testit "(> dstVec1[2] 0)" true)
(testit "(= dstVec1[3] 0)" true)



;; Comparison between Substrings and Strings
(setq argVec1 (new Vector: 4 substr1 substr1 substr2 substr2))
(setq srcVec1 (new Vector: 4 string1 string2 string1 string2))
(setq dstVec1 (new Vector: Integer: 4 0))
(cStringVector argVec1 srcVec1 dstVec1)

(testit "(type (cStringVector argVec1 srcVec1 dstVec1))" IntVector:)
(testit "(= dstVec1[0] 0)" true)
(testit "(< dstVec1[1] 0)" true)
(testit "(> dstVec1[2] 0)" true)
(testit "(= dstVec1[3] 0)" true)

;; Comparison between Strings and Substrings
(setq argVec1 (new Vector: 4 string1 string1 string2 string2))
(setq srcVec1 (new Vector: 4 substr1 substr2 substr1 substr2))
(setq dstVec1 (new Vector: Integer: 4 0))
(cStringVector argVec1 srcVec1 dstVec1)

(testit "(type (cStringVector argVec1 srcVec1 dstVec1))" IntVector:)
(testit "(= dstVec1[0] 0)" true)
(testit "(< dstVec1[1] 0)" true)
(testit "(> dstVec1[2] 0)" true)
(testit "(= dstVec1[3] 0)" true)



;; Comparison between Substrings and Texts
(setq argVec1 (new Vector: 4 substr1 substr1 substr2 substr2))
(setq srcVec1 (new Vector: 4 "Hello" "World" "Hello" "World"))
(setq dstVec1 (new Vector: Integer: 4 0))
(cStringVector argVec1 srcVec1 dstVec1)

(testit "(type (cStringVector argVec1 srcVec1 dstVec1))" IntVector:)
(testit "(= dstVec1[0] 0)" true)
(testit "(< dstVec1[1] 0)" true)
(testit "(> dstVec1[2] 0)" true)
(testit "(= dstVec1[3] 0)" true)

;; Comparison between Texts and Substrings
(setq argVec1 (new Vector: 4 "Hello" "Hello" "World" "World"))
(setq srcVec1 (new Vector: 4 substr1 substr2 substr1 substr2))
(setq dstVec1 (new Vector: Integer: 4 0))
(cStringVector argVec1 srcVec1 dstVec1)

(testit "(type (cStringVector argVec1 srcVec1 dstVec1))" IntVector:)
(testit "(= dstVec1[0] 0)" true)
(testit "(< dstVec1[1] 0)" true)
(testit "(> dstVec1[2] 0)" true)
(testit "(= dstVec1[3] 0)" true)



;; Comparison between Substrings and Symbols
(setq argVec1 (new Vector: 4 substr1 substr1 substr2 substr2))
(setq srcVec1 (new Vector: 4 symbol1 symbol2 symbol1 symbol2))
(setq dstVec1 (new Vector: Integer: 4 0))
(cStringVector argVec1 srcVec1 dstVec1)

(testit "(type (cStringVector argVec1 srcVec1 dstVec1))" IntVector:)
(testit "(= dstVec1[0] 0)" true)
(testit "(< dstVec1[1] 0)" true)
(testit "(> dstVec1[2] 0)" true)
(testit "(= dstVec1[3] 0)" true)

;; Comparison between Symbols and Substrings
(setq argVec1 (new Vector: 4 symbol1 symbol1 symbol2 symbol2))
(setq srcVec1 (new Vector: 4 substr1 substr2 substr1 substr2))
(setq dstVec1 (new Vector: Integer: 4 0))
(cStringVector argVec1 srcVec1 dstVec1)

(testit "(type (cStringVector argVec1 srcVec1 dstVec1))" IntVector:)
(testit "(= dstVec1[0] 0)" true)
(testit "(< dstVec1[1] 0)" true)
(testit "(> dstVec1[2] 0)" true)
(testit "(= dstVec1[3] 0)" true)



;; Comparison between Substrings and ByteVector
(setq argVec1 (new Vector: 4 substr1 substr1 substr2 substr2))
(setq srcVec1 (new Vector: 4 byteVec1 byteVec2 byteVec1 byteVec2))
(setq dstVec1 (new Vector: Integer: 4 0))
(cStringVector argVec1 srcVec1 dstVec1)

(testit "(type (cStringVector argVec1 srcVec1 dstVec1))" IntVector:)
(testit "(= dstVec1[0] 0)" true)
(testit "(< dstVec1[1] 0)" true)
(testit "(> dstVec1[2] 0)" true)
(testit "(= dstVec1[3] 0)" true)

;; Comparison between ByteVector and Substrings
(setq argVec1 (new Vector: 4 byteVec1 byteVec1 byteVec2 byteVec2))
(setq srcVec1 (new Vector: 4 substr1 substr2 substr1 substr2))
(setq dstVec1 (new Vector: Integer: 4 0))
(cStringVector argVec1 srcVec1 dstVec1)

(testit "(type (cStringVector argVec1 srcVec1 dstVec1))" IntVector:)
(testit "(= dstVec1[0] 0)" true)
(testit "(< dstVec1[1] 0)" true)
(testit "(> dstVec1[2] 0)" true)
(testit "(= dstVec1[3] 0)" true)

;;************************************************************************
;;  Count Function
;;************************************************************************
(setq string1 (makeString "Hello"))
(setq string2 (makeString "World"))
(setq substr1 (makeSubstring string1 0 5))
(setq substr2 (makeSubstring string2 0 5))
(testit "(count substr1)" 1)
(testit "(count substr1 substr2)" 2)

;;************************************************************************
;;  Data-type Convert Functions
;;************************************************************************

;; (money)
(setq string1 (makeString "$1.00 $2.34"))
(setq substr1 (makeSubstring string1 0 5))
(setq substr2 (makeSubstring string1 6 5))
(testit "(type (money substr1))" Money:)
(testit "(type (money substr2))" Money:)
(testit "(money substr1)" $1.0)
(testit "(money substr2)" $2.34)

;; (integer)
(setq string1 (makeString "1024 65536"))
(setq substr1 (makeSubstring string1 0 4))
(setq substr2 (makeSubstring string1 5 5))
(testit "(type (integer substr1))" Integer:)
(testit "(type (integer substr2))" Integer:)
(testit "(integer substr1)" 1024)
(testit "(integer substr2)" 65536)

;; (number)
(setq string1 (makeString "11.22 34567890.1234"))
(setq substr1 (makeSubstring string1 0 5))
(setq substr2 (makeSubstring string1 6 13))
(testit "(type (number substr1))" Number:)
(testit "(type (number substr2))" Number:)
(testit "(number substr1)" 11.22)
(testit "(number substr2)" 34567890.1234)

;; (character)
(setq string1 (makeString "true false"))
(setq substr1 (makeSubstring string1 0 4))
(setq substr2 (makeSubstring string1 5 5))
(testit "(type (character substr1))" Character:)
(testit "(type (character substr2))" Character:)
(testit "(character substr1)" "t")
(testit "(character substr2)" "f")

;; (boolean)
(setq string1 (makeString "true false"))
(setq substr1 (makeSubstring string1 0 4))
(setq substr2 (makeSubstring string1 5 5))
(testit "(type (boolean substr1))" Boolean:)
(testit "(type (boolean substr2))" Boolean:)
(testit "(boolean substr1)" true)
(testit "(boolean substr2)" false)

;; (string)
(setq string1 (makeString "mercury venus achievement"))
(setq substr1 (makeSubstring string1 0 7))
(setq substr2 (makeSubstring string1 8 5))
(setq substr3 (makeSubstring string1 14 11))
(testit "(type (string substr1))" Text:)
(testit "(type (string substr2))" Text:)
(testit "(type (string substr3))" String:)
(testit "(string substr1)" "mercury")
(testit "(string substr2)" "venus")
(testit "(string substr3)" "achievement")

;;************************************************************************
;;  Predicate Functions
;;************************************************************************

;; (downcase)
(setq strBase (new String: "1024 ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(setq strSubstr (new Substring: strBase 5 13))
(testit "(downcase strSubstr)" "abcdefghijklm")

;; (upcase)
(setq strBase (new String: "1024 abcdefghijklmnopqrstuvwxyz"))
(setq strSubstr (new Substring: strBase 15 10))
(testit "(upcase strSubstr)" "KLMNOPQRST")

;; (isString)
(testit "(isString strSubstr)" true)

;; (isText)
(testit "(isText strSubstr)" true)

;; (isCharAlphabetic)
(setq strBase (new String: "1024 abcd efghi1234"))
(setq substr1 (makeSubstring strBase 0 4))
(setq substr2 (makeSubstring strBase 5 4))
(setq substr3 (makeSubstring strBase 10 9))
(testit "(isCharAlphabetic substr1)" false)
(testit "(isCharAlphabetic substr2)" true)
(testit "(isCharAlphabetic substr3)" false)

;; (isCharLowercase)
(setq strBase (new String: "1024 abcd EFGH ab12"))
(setq substr1 (makeSubstring strBase 0 4))
(setq substr2 (makeSubstring strBase 5 4))
(setq substr3 (makeSubstring strBase 10 4))
(setq substr4 (makeSubstring strBase 15 4))
(testit "(isCharLowercase substr1)" false)
(testit "(isCharLowercase substr2)" true)
(testit "(isCharLowercase substr3)" false)
(testit "(isCharLowercase substr4)" false)

;; (isCharUppercase)
(setq strBase (new String: "1024 abcd EFGH EF12"))
(setq substr1 (makeSubstring strBase 0 4))
(setq substr2 (makeSubstring strBase 5 4))
(setq substr3 (makeSubstring strBase 10 4))
(setq substr4 (makeSubstring strBase 15 4))
(testit "(isCharUppercase substr1)" false)
(testit "(isCharUppercase substr2)" false)
(testit "(isCharUppercase substr3)" true)
(testit "(isCharUppercase substr4)" false)

;; (isCharNumeric)
(setq strBase (new String: "1024 abcd 12ab"))
(setq substr1 (makeSubstring strBase 0 4))
(setq substr2 (makeSubstring strBase 5 4))
(setq substr3 (makeSubstring strBase 10 4))
(testit "(isCharNumeric substr1)" true)
(testit "(isCharNumeric substr2)" false)
(testit "(isCharNumeric substr3)" false)

;; (isCharWhitespace)
(setq strBase (new String: "1024  abcd"))
(setq substr1 (makeSubstring strBase 0 4))
(setq substr2 (makeSubstring strBase 4 2))
(testit "(isCharWhitespace substr1)" false)
(testit "(isCharWhitespace substr2)" true)

;; (isCharAlphanumeric)
(setq strBase (new String: "1024 abcd ab12 34dc %^&*"))
(setq substr1 (makeSubstring strBase 0 4))
(setq substr2 (makeSubstring strBase 5 4))
(setq substr3 (makeSubstring strBase 10 4))
(setq substr4 (makeSubstring strBase 15 4))
(setq substr5 (makeSubstring strBase 20 4))
(testit "(isCharAlphanumeric substr1)" true)
(testit "(isCharAlphanumeric substr2)" true)
(testit "(isCharAlphanumeric substr3)" true)
(testit "(isCharAlphanumeric substr4)" true)
(testit "(isCharAlphanumeric substr5)" false)

;; (isCharName)
(setq strBase (new String: "1024 abcd a__2 3  c %^&*"))
(setq substr1 (makeSubstring strBase 0 4))
(setq substr2 (makeSubstring strBase 5 4))
(setq substr3 (makeSubstring strBase 10 4))
(setq substr4 (makeSubstring strBase 15 4))
(setq substr5 (makeSubstring strBase 20 4))
(testit "(isCharName substr1)" true)
(testit "(isCharName substr2)" true)
(testit "(isCharName substr3)" true)
(testit "(isCharName substr4)" false)
(testit "(isCharName substr5)" false)

;; (isType)
(testit "(isType 'Substring substr1)" true)
(testit "(isType 'String substr1)" true)

;; (compare)
(setq string1 (makeString "Hello"))
(setq string2 (makeString "World"))
(setq substr1 (makeSubstring string1 0 5))
(setq substr2 (makeSubstring string2 0 5))
(setq symbol1 (makeSymbol "Hello"))
(setq symbol2 (makeSymbol "World"))
(setq byteVec1 (new ByteVector: 5 72 101 108 108 111))
(setq byteVec2 (new ByteVector: 5 87 111 114 108 100))

(testit "(compare substr1 substr1)" 0)
(testit "(compare substr1 substr2)" -1)
(testit "(compare substr2 substr1)" 1)
(testit "(compare substr2 substr2)" 0)

(testit "(compare substr1 string1)" 0)
(testit "(compare substr1 string2)" -1)
(testit "(compare string1 substr1)" 0)
(testit "(compare string2 substr1)" 1)

(testit "(compare substr1 symbol1)" 0)
(testit "(compare substr1 symbol2)" -1)
(testit "(compare symbol1 substr1)" 0)
(testit "(compare symbol2 substr1)" 1)

(testit "(compare substr1 byteVec1)" 0)
(testit "(compare substr1 byteVec2)" -1)
(testit "(compare byteVec1 substr1)" 0)
(testit "(compare byteVec2 substr1)" 1)

(setq substr1 (makeSubstring string1 0 1))
(setq substr2 (makeSubstring string2 0 1))
(setq char1 (character "H"))
(setq char2 (character "W"))

(testit "(compare substr1 char1)" 0)
(testit "(compare substr1 char2)" -1)
(testit "(compare substr2 char1)" 1)
(testit "(compare substr2 char2)" 0)

(testit "(compare char1 substr1)" 0)
(testit "(compare char1 substr2)" -1)
(testit "(compare char2 substr1)" 1)
(testit "(compare char2 substr2)" 0)

;; (isIdentical)
(setq string1 (makeString "Hello"))
(setq string2 (makeString "World"))
(setq substr1 (makeSubstring string1 0 5))
(setq substr2 (makeSubstring string2 0 5))

(testit "(isIdentical substr1 string1)" true)
(testit "(isIdentical substr1 string2)" false)
(testit "(isIdentical string1 substr1)" true)
(testit "(isIdentical string1 substr2)" false)

(testit "(isIdentical substr1 \"Hello\")" true)
(testit "(isIdentical substr1 \"World\")" false)
(testit "(isIdentical \"Hello\" substr1)" true)
(testit "(isIdentical \"Hello\" substr2)" false)

(setq substr1 (makeSubstring string1 0 1))
(setq substr2 (makeSubstring string2 0 1))
(setq char1 (character "H"))
(setq char2 (character "W"))

(testit "(isIdentical substr1 char1)" true)
(testit "(isIdentical substr1 char2)" false)

;;************************************************************************
;;  Substring Functions
;;************************************************************************

;; (substringFill)
(setq string1 (makeString "Hello World"))
(setq substr1 (makeSubstring string1 0 11))
(testit "(substringFill substr1 0 4 #\\X)" "XXXXX World")

;; (substring)
(testit "(substring substr1 0 4)" "Hello")
(testit "(substring substr1 6 10)" "World")

(testit "(gc)" #void)

;;************************************************************************
;;  Substring Compare Functions
;;************************************************************************
(setq string1 (makeString "... Hello ..."))
(setq string2 (makeString "... World ..."))
(setq string3 (makeString "... hello ..."))
(setq string4 (makeString "... world ..."))

(setq substr1 (makeSubstring string1 4 5)) ;; Hello
(setq substr2 (makeSubstring string2 4 5)) ;; World
(setq substr3 (makeSubstring string3 4 5)) ;; hello
(setq substr4 (makeSubstring string4 4 5)) ;; world

(setq text1 "Hello")
(setq text2 "World")
(setq text3 "hello")
(setq text4 "world")

;; (substringLT)
;; Hello < World = true
;; World < Hello = false
;; Hello < Hello = false
;; Hello < Hello = false

;; World < World = false
;; World < World = false
;; World < Hello = false
;; Hello < World = true

;; Hello < world = true
;; world < Hello = false
;; Hello < hello = true
;; hello < Hello = false

;; World < world = true
;; world < World = false
;; World < hello = true
;; hello < World = false

;; Comparison with String
(testit "(substringLT substr1 0 4 string2 4 8)" true)
(testit "(substringLT string2 4 8 substr1 0 4)" false)
(testit "(substringLT substr1 0 4 string1 4 8)" false)
(testit "(substringLT string1 4 8 substr1 0 4)" false)

(testit "(substringLT substr2 0 4 string2 4 8)" false)
(testit "(substringLT string2 4 8 substr2 0 4)" false)
(testit "(substringLT substr2 0 4 string1 4 8)" false)
(testit "(substringLT string1 4 8 substr2 0 4)" true)

;; Comparison with Text
(testit "(substringLT substr1 0 4 text2 0 4)" true)
(testit "(substringLT text2 0 4 substr1 0 4)" false)
(testit "(substringLT substr1 0 4 text1 0 4)" false)
(testit "(substringLT text1 0 4 substr1 0 4)" false)

(testit "(substringLT substr2 0 4 text2 0 4)" false)
(testit "(substringLT text2 0 4 substr2 0 4)" false)
(testit "(substringLT substr2 0 4 text1 0 4)" false)
(testit "(substringLT text1 0 4 substr2 0 4)" true)

;; Comparison with Substring
(testit "(substringLT substr1 0 4 substr4 0 4)" true)
(testit "(substringLT substr4 0 4 substr1 0 4)" false)
(testit "(substringLT substr1 0 4 substr3 0 4)" true)
(testit "(substringLT substr3 0 4 substr1 0 4)" false)

(testit "(substringLT substr2 0 4 substr4 0 4)" true)
(testit "(substringLT substr4 0 4 substr2 0 4)" false)
(testit "(substringLT substr2 0 4 substr3 0 4)" true)
(testit "(substringLT substr3 0 4 substr2 0 4)" false)

;; (substringGT)
;; Hello > World = false
;; World > Hello = true
;; Hello > Hello = false
;; Hello > Hello = false

;; World > World  = false
;; World > World = false
;; World > Hello = true
;; Hello > World = false

;; Hello > world = false
;; world > Hello = true
;; Hello > hello = false
;; hello > Hello = true

;; World > world = false
;; world > World = true
;; World > hello = false
;; hello > World = true

;; Comparison with String
(testit "(substringGT substr1 0 4 string2 4 8)" false)
(testit "(substringGT string2 4 8 substr1 0 4)" true)
(testit "(substringGT substr1 0 4 string1 4 8)" false)
(testit "(substringGT string1 4 8 substr1 0 4)" false)

(testit "(substringGT substr2 0 4 string2 4 8)" false)
(testit "(substringGT string2 4 8 substr2 0 4)" false)
(testit "(substringGT substr2 0 4 string1 4 8)" true)
(testit "(substringGT string1 4 8 substr2 0 4)" false)

;; Comparison with Text
(testit "(substringGT substr1 0 4 text2 0 4)" false)
(testit "(substringGT text2 0 4 substr1 0 4)" true)
(testit "(substringGT substr1 0 4 text1 0 4)" false)
(testit "(substringGT text1 0 4 substr1 0 4)" false)

(testit "(substringGT substr2 0 4 text2 0 4)" false)
(testit "(substringGT text2 0 4 substr2 0 4)" false)
(testit "(substringGT substr2 0 4 text1 0 4)" true)
(testit "(substringGT text1 0 4 substr2 0 4)" false)

;; Comparison with Substring
(testit "(substringGT substr1 0 4 substr4 0 4)" false)
(testit "(substringGT substr4 0 4 substr1 0 4)" true)
(testit "(substringGT substr1 0 4 substr3 0 4)" false)
(testit "(substringGT substr3 0 4 substr1 0 4)" true)

(testit "(substringGT substr2 0 4 substr4 0 4)" false)
(testit "(substringGT substr4 0 4 substr2 0 4)" true)
(testit "(substringGT substr2 0 4 substr3 0 4)" false)
(testit "(substringGT substr3 0 4 substr2 0 4)" true)

;; (substringEQ)
;; Hello = World = false
;; World = Hello = false
;; Hello = Hello = true
;; Hello = Hello = true

;; World = World = true
;; World = World  = true
;; World = Hello = false
;; Hello = World = false

;; Hello = world = false
;; world = Hello = false
;; Hello = hello = false
;; hello = Hello = false

;; World = world = false
;; world = World = false
;; World = hello = false
;; hello = World = false

;; Comparison with String
(testit "(substringEQ substr1 0 4 string2 4 8)" false)
(testit "(substringEQ string2 4 8 substr1 0 4)" false)
(testit "(substringEQ substr1 0 4 string1 4 8)" true)
(testit "(substringEQ string1 4 8 substr1 0 4)" true)

(testit "(substringEQ substr2 0 4 string2 4 8)" true)
(testit "(substringEQ string2 4 8 substr2 0 4)" true)
(testit "(substringEQ substr2 0 4 string1 4 8)" false)
(testit "(substringEQ string1 4 8 substr2 0 4)" false)

;; Comparison with Text
(testit "(substringEQ substr1 0 4 text2 0 4)" false)
(testit "(substringEQ text2 0 4 substr1 0 4)" false)
(testit "(substringEQ substr1 0 4 text1 0 4)" true)
(testit "(substringEQ text1 0 4 substr1 0 4)" true)

(testit "(substringEQ substr2 0 4 text2 0 4)" true)
(testit "(substringEQ text2 0 4 substr2 0 4)" true)
(testit "(substringEQ substr2 0 4 text1 0 4)" false)
(testit "(substringEQ text1 0 4 substr2 0 4)" false)

;; Comparison with Substring
(testit "(substringEQ substr1 0 4 substr4 0 4)" false)
(testit "(substringEQ substr4 0 4 substr1 0 4)" false)
(testit "(substringEQ substr1 0 4 substr3 0 4)" false)
(testit "(substringEQ substr3 0 4 substr1 0 4)" false)

(testit "(substringEQ substr2 0 4 substr4 0 4)" false)
(testit "(substringEQ substr4 0 4 substr2 0 4)" false)
(testit "(substringEQ substr2 0 4 substr3 0 4)" false)
(testit "(substringEQ substr3 0 4 substr2 0 4)" false)

;; (substringLE)
;; Hello <= World = true
;; World <= Hello = false
;; Hello <= Hello = true
;; Hello <= Hello = true

;; World <= World = true
;; World <= World = true
;; World <= Hello = false
;; Hello <= World = false

;; Hello <= world = true
;; world <= Hello = false
;; Hello <= hello = true
;; hello <= Hello = false

;; World <= world = true
;; world <= World = false
;; World <= hello = true
;; hello <= World = false

;; Comparison with String
(testit "(substringLE substr1 0 4 string2 4 8)" true)
(testit "(substringLE string2 4 8 substr1 0 4)" false)
(testit "(substringLE substr1 0 4 string1 4 8)" true)
(testit "(substringLE string1 4 8 substr1 0 4)" true)

(testit "(substringLE substr2 0 4 string2 4 8)" true)
(testit "(substringLE string2 4 8 substr2 0 4)" true)
(testit "(substringLE substr2 0 4 string1 4 8)" false)
(testit "(substringLE string1 4 8 substr2 0 4)" true)

;; Comparison with Text
(testit "(substringLE substr1 0 4 text2 0 4)" true)
(testit "(substringLE text2 0 4 substr1 0 4)" false)
(testit "(substringLE substr1 0 4 text1 0 4)" true)
(testit "(substringLE text1 0 4 substr1 0 4)" true)

(testit "(substringLE substr2 0 4 text2 0 4)" true)
(testit "(substringLE text2 0 4 substr2 0 4)" true)
(testit "(substringLE substr2 0 4 text1 0 4)" false)
(testit "(substringLE text1 0 4 substr2 0 4)" true)

;; Comparison with Substring
(testit "(substringLE substr1 0 4 substr4 0 4)" true)
(testit "(substringLE substr4 0 4 substr1 0 4)" false)
(testit "(substringLE substr1 0 4 substr3 0 4)" true)
(testit "(substringLE substr3 0 4 substr1 0 4)" false)

(testit "(substringLE substr2 0 4 substr4 0 4)" true)
(testit "(substringLE substr4 0 4 substr2 0 4)" false)
(testit "(substringLE substr2 0 4 substr3 0 4)" true)
(testit "(substringLE substr3 0 4 substr2 0 4)" false)

;; (substringGE)
;; Hello >= World = false
;; World >= Hello = true
;; Hello >= Hello = true
;; Hello >= Hello = true

;; World >= World = true
;; World >= World = true
;; World >= Hello = true
;; Hello >= World = false

;; Hello >= world = false
;; world >= Hello = true
;; Hello >= hello = false
;; hello >= Hello = true

;; World >= world = false
;; world >= World = true
;; World >= hello = false
;; hello >= World = true

;; Comparison with String
(testit "(substringGE substr1 0 4 string2 4 8)" false)
(testit "(substringGE string2 4 8 substr1 0 4)" true)
(testit "(substringGE substr1 0 4 string1 4 8)" true)
(testit "(substringGE string1 4 8 substr1 0 4)" true)

(testit "(substringGE substr2 0 4 string2 4 8)" true)
(testit "(substringGE string2 4 8 substr2 0 4)" true)
(testit "(substringGE substr2 0 4 string1 4 8)" true)
(testit "(substringGE string1 4 8 substr2 0 4)" false)

;; Comparison with Text
(testit "(substringGE substr1 0 4 text2 0 4)" false)
(testit "(substringGE text2 0 4 substr1 0 4)" true)
(testit "(substringGE substr1 0 4 text1 0 4)" true)
(testit "(substringGE text1 0 4 substr1 0 4)" true)

(testit "(substringGE substr2 0 4 text2 0 4)" true)
(testit "(substringGE text2 0 4 substr2 0 4)" true)
(testit "(substringGE substr2 0 4 text1 0 4)" true)
(testit "(substringGE text1 0 4 substr2 0 4)" false)

;; Comparison with Substring
(testit "(substringGE substr1 0 4 substr4 0 4)" false)
(testit "(substringGE substr4 0 4 substr1 0 4)" true)
(testit "(substringGE substr1 0 4 substr3 0 4)" false)
(testit "(substringGE substr3 0 4 substr1 0 4)" true)

(testit "(substringGE substr2 0 4 substr4 0 4)" false)
(testit "(substringGE substr4 0 4 substr2 0 4)" true)
(testit "(substringGE substr2 0 4 substr3 0 4)" false)
(testit "(substringGE substr3 0 4 substr2 0 4)" true)

;; (substringNE)
;; Hello != World = true
;; World != Hello = true
;; Hello != Hello = false
;; Hello != Hello = false

;; World != World = false
;; World != World  = false
;; World != Hello = true
;; Hello != World = true

;; Hello != world = true
;; world != Hello = true
;; Hello != hello = true
;; hello != Hello = true

;; World != world = true
;; world != World = true
;; World != hello = true
;; hello != World = true

;; Comparison with String
(testit "(substringNE substr1 0 4 string2 4 8)" true)
(testit "(substringNE string2 4 8 substr1 0 4)" true)
(testit "(substringNE substr1 0 4 string1 4 8)" false)
(testit "(substringNE string1 4 8 substr1 0 4)" false)

(testit "(substringNE substr2 0 4 string2 4 8)" false)
(testit "(substringNE string2 4 8 substr2 0 4)" false)
(testit "(substringNE substr2 0 4 string1 4 8)" true)
(testit "(substringNE string1 4 8 substr2 0 4)" true)

;; Comparison with Text
(testit "(substringNE substr1 0 4 text2 0 4)" true)
(testit "(substringNE text2 0 4 substr1 0 4)" true)
(testit "(substringNE substr1 0 4 text1 0 4)" false)
(testit "(substringNE text1 0 4 substr1 0 4)" false)

(testit "(substringNE substr2 0 4 text2 0 4)" false)
(testit "(substringNE text2 0 4 substr2 0 4)" false)
(testit "(substringNE substr2 0 4 text1 0 4)" true)
(testit "(substringNE text1 0 4 substr2 0 4)" true)

;; Comparison with Substring
(testit "(substringNE substr1 0 4 substr4 0 4)" true)
(testit "(substringNE substr4 0 4 substr1 0 4)" true)
(testit "(substringNE substr1 0 4 substr3 0 4)" true)
(testit "(substringNE substr3 0 4 substr1 0 4)" true)

(testit "(substringNE substr2 0 4 substr4 0 4)" true)
(testit "(substringNE substr4 0 4 substr2 0 4)" true)
(testit "(substringNE substr2 0 4 substr3 0 4)" true)
(testit "(substringNE substr3 0 4 substr2 0 4)" true)

;; (substringCiLT)
;; hello < World = true
;; World < hello = false
;; hello < Hello = false
;; Hello < hello = false

;; world < World = false
;; World < world = false
;; world < Hello = false
;; Hello < world = true

;; Comparison with String
(testit "(substringCiLT substr3 0 4 string2 4 8)" true)
(testit "(substringCiLT string2 4 8 substr3 0 4)" false)
(testit "(substringCiLT substr3 0 4 string1 4 8)" false)
(testit "(substringCiLT string1 4 8 substr3 0 4)" false)

(testit "(substringCiLT substr4 0 4 string2 4 8)" false)
(testit "(substringCiLT string2 4 8 substr4 0 4)" false)
(testit "(substringCiLT substr4 0 4 string1 4 8)" false)
(testit "(substringCiLT string1 4 8 substr4 0 4)" true)

;; Comparison with Text
(testit "(substringCiLT substr3 0 4 text2 0 4)" true)
(testit "(substringCiLT text2 0 4 substr3 0 4)" false)
(testit "(substringCiLT substr3 0 4 text1 0 4)" false)
(testit "(substringCiLT text1 0 4 substr3 0 4)" false)

(testit "(substringCiLT substr4 0 4 text2 0 4)" false)
(testit "(substringCiLT text2 0 4 substr4 0 4)" false)
(testit "(substringCiLT substr4 0 4 text1 0 4)" false)
(testit "(substringCiLT text1 0 4 substr4 0 4)" true)

;; Comparison with Substring
(testit "(substringCiLT substr1 0 4 substr4 0 4)" true)
(testit "(substringCiLT substr4 0 4 substr1 0 4)" false)
(testit "(substringCiLT substr1 0 4 substr3 0 4)" false)
(testit "(substringCiLT substr3 0 4 substr1 0 4)" false)

(testit "(substringCiLT substr2 0 4 substr4 0 4)" false)
(testit "(substringCiLT substr4 0 4 substr2 0 4)" false)
(testit "(substringCiLT substr2 0 4 substr3 0 4)" false)
(testit "(substringCiLT substr3 0 4 substr2 0 4)" true)

;; (substringCiLE)
;; hello <= World = true
;; World <= hello = false
;; hello <= Hello = true
;; Hello <= hello = true

;; world <= World = true
;; World <= world = true
;; world <= Hello = false
;; Hello <= world = true

;; Comparison with String
(testit "(substringCiLE substr3 0 4 string2 4 8)" true)
(testit "(substringCiLE string2 4 8 substr3 0 4)" false)
(testit "(substringCiLE substr3 0 4 string1 4 8)" true)
(testit "(substringCiLE string1 4 8 substr3 0 4)" true)

(testit "(substringCiLE substr4 0 4 string2 4 8)" true)
(testit "(substringCiLE string2 4 8 substr4 0 4)" true)
(testit "(substringCiLE substr4 0 4 string1 4 8)" false)
(testit "(substringCiLE string1 4 8 substr4 0 4)" true)

;; Comparison with Text
(testit "(substringCiLE substr3 0 4 text2 0 4)" true)
(testit "(substringCiLE text2 0 4 substr3 0 4)" false)
(testit "(substringCiLE substr3 0 4 text1 0 4)" true)
(testit "(substringCiLE text1 0 4 substr3 0 4)" true)

(testit "(substringCiLE substr4 0 4 text2 0 4)" true)
(testit "(substringCiLE text2 0 4 substr4 0 4)" true)
(testit "(substringCiLE substr4 0 4 text1 0 4)" false)
(testit "(substringCiLE text1 0 4 substr4 0 4)" true)

;; Comparison with Substring
(testit "(substringCiLE substr1 0 4 substr4 0 4)" true)
(testit "(substringCiLE substr4 0 4 substr1 0 4)" false)
(testit "(substringCiLE substr1 0 4 substr3 0 4)" true)
(testit "(substringCiLE substr3 0 4 substr1 0 4)" true)

(testit "(substringCiLE substr2 0 4 substr4 0 4)" true)
(testit "(substringCiLE substr4 0 4 substr2 0 4)" true)
(testit "(substringCiLE substr2 0 4 substr3 0 4)" false)
(testit "(substringCiLE substr3 0 4 substr2 0 4)" true)

;; (substringCiEQ)
;; hello = World = false
;; World = hello = false
;; hello = Hello = true
;; Hello = hello = true

;; world = World = true
;; World = world = true
;; world = Hello = false
;; Hello = world = false

;; Comparison with String
(testit "(substringCiEQ substr3 0 4 string2 4 8)" false)
(testit "(substringCiEQ string2 4 8 substr3 0 4)" false)
(testit "(substringCiEQ substr3 0 4 string1 4 8)" true)
(testit "(substringCiEQ string1 4 8 substr3 0 4)" true)

(testit "(substringCiEQ substr4 0 4 string2 4 8)" true)
(testit "(substringCiEQ string2 4 8 substr4 0 4)" true)
(testit "(substringCiEQ substr4 0 4 string1 4 8)" false)
(testit "(substringCiEQ string1 4 8 substr4 0 4)" false)

;; Comparison with Text
(testit "(substringCiEQ substr3 0 4 text2 0 4)" false)
(testit "(substringCiEQ text2 0 4 substr3 0 4)" false)
(testit "(substringCiEQ substr3 0 4 text1 0 4)" true)
(testit "(substringCiEQ text1 0 4 substr3 0 4)" true)

(testit "(substringCiEQ substr4 0 4 text2 0 4)" true)
(testit "(substringCiEQ text2 0 4 substr4 0 4)" true)
(testit "(substringCiEQ substr4 0 4 text1 0 4)" false)
(testit "(substringCiEQ text1 0 4 substr4 0 4)" false)

;; Comparison with Substring
(testit "(substringCiEQ substr1 0 4 substr4 0 4)" false)
(testit "(substringCiEQ substr4 0 4 substr1 0 4)" false)
(testit "(substringCiEQ substr1 0 4 substr3 0 4)" true)
(testit "(substringCiEQ substr3 0 4 substr1 0 4)" true)

(testit "(substringCiEQ substr2 0 4 substr4 0 4)" true)
(testit "(substringCiEQ substr4 0 4 substr2 0 4)" true)
(testit "(substringCiEQ substr2 0 4 substr3 0 4)" false)
(testit "(substringCiEQ substr3 0 4 substr2 0 4)" false)

;; (substringCiNE)
;; hello != World = true
;; World != hello = true
;; hello != Hello = false
;; Hello != hello = false

;; world != World = false
;; World != world = false
;; world != Hello = true
;; Hello != world = true

;; Comparison with String
(testit "(substringCiNE substr3 0 4 string2 4 8)" true)
(testit "(substringCiNE string2 4 8 substr3 0 4)" true)
(testit "(substringCiNE substr3 0 4 string1 4 8)" false)
(testit "(substringCiNE string1 4 8 substr3 0 4)" false)

(testit "(substringCiNE substr4 0 4 string2 4 8)" false)
(testit "(substringCiNE string2 4 8 substr4 0 4)" false)
(testit "(substringCiNE substr4 0 4 string1 4 8)" true)
(testit "(substringCiNE string1 4 8 substr4 0 4)" true)

;; Comparison with Text
(testit "(substringCiNE substr3 0 4 text2 0 4)" true)
(testit "(substringCiNE text2 0 4 substr3 0 4)" true)
(testit "(substringCiNE substr3 0 4 text1 0 4)" false)
(testit "(substringCiNE text1 0 4 substr3 0 4)" false)

(testit "(substringCiNE substr4 0 4 text2 0 4)" false)
(testit "(substringCiNE text2 0 4 substr4 0 4)" false)
(testit "(substringCiNE substr4 0 4 text1 0 4)" true)
(testit "(substringCiNE text1 0 4 substr4 0 4)" true)

;; Comparison with Substring
(testit "(substringCiNE substr1 0 4 substr4 0 4)" true)
(testit "(substringCiNE substr4 0 4 substr1 0 4)" true)
(testit "(substringCiNE substr1 0 4 substr3 0 4)" false)
(testit "(substringCiNE substr3 0 4 substr1 0 4)" false)

(testit "(substringCiNE substr2 0 4 substr4 0 4)" false)
(testit "(substringCiNE substr4 0 4 substr2 0 4)" false)
(testit "(substringCiNE substr2 0 4 substr3 0 4)" true)
(testit "(substringCiNE substr3 0 4 substr2 0 4)" true)

;; (substringCiGT)
;; hello > World = false
;; World > hello = true
;; hello > Hello = false
;; Hello > hello = false

;; world > World = true
;; World > world = true
;; world > Hello = true
;; Hello > world = false

;; Comparison with String
(testit "(substringCiGT substr3 0 4 string2 4 8)" false)
(testit "(substringCiGT string2 4 8 substr3 0 4)" true)
(testit "(substringCiGT substr3 0 4 string1 4 8)" false)
(testit "(substringCiGT string1 4 8 substr3 0 4)" false)

(testit "(substringCiGT substr4 0 4 string2 4 8)" false)
(testit "(substringCiGT string2 4 8 substr4 0 4)" false)
(testit "(substringCiGT substr4 0 4 string1 4 8)" true)
(testit "(substringCiGT string1 4 8 substr4 0 4)" false)

;; Comparison with Text
(testit "(substringCiGT substr3 0 4 text2 0 4)" false)
(testit "(substringCiGT text2 0 4 substr3 0 4)" true)
(testit "(substringCiGT substr3 0 4 text1 0 4)" false)
(testit "(substringCiGT text1 0 4 substr3 0 4)" false)

(testit "(substringCiGT substr4 0 4 text2 0 4)" false)
(testit "(substringCiGT text2 0 4 substr4 0 4)" false)
(testit "(substringCiGT substr4 0 4 text1 0 4)" true)
(testit "(substringCiGT text1 0 4 substr4 0 4)" false)

;; Comparison with Substring
(testit "(substringCiGT substr1 0 4 substr4 0 4)" false)
(testit "(substringCiGT substr4 0 4 substr1 0 4)" true)
(testit "(substringCiGT substr1 0 4 substr3 0 4)" false)
(testit "(substringCiGT substr3 0 4 substr1 0 4)" false)

(testit "(substringCiGT substr2 0 4 substr4 0 4)" false)
(testit "(substringCiGT substr4 0 4 substr2 0 4)" false)
(testit "(substringCiGT substr2 0 4 substr3 0 4)" true)
(testit "(substringCiGT substr3 0 4 substr2 0 4)" false)

;; (substringCiGE)
;; hello >= World = false
;; World >= hello = true
;; hello >= Hello = true
;; Hello >= hello = true

;; world >= World = true
;; World >= world = true
;; world >= Hello = true
;; Hello >= world = false

;; Comparison with String
(testit "(substringCiGE substr3 0 4 string2 4 8)" false)
(testit "(substringCiGE string2 4 8 substr3 0 4)" true)
(testit "(substringCiGE substr3 0 4 string1 4 8)" true)
(testit "(substringCiGE string1 4 8 substr3 0 4)" true)

(testit "(substringCiGE substr4 0 4 string2 4 8)" true)
(testit "(substringCiGE string2 4 8 substr4 0 4)" true)
(testit "(substringCiGE substr4 0 4 string1 4 8)" true)
(testit "(substringCiGE string1 4 8 substr4 0 4)" false)

;; Comparison with Text
(testit "(substringCiGE substr3 0 4 text2 0 4)" false)
(testit "(substringCiGE text2 0 4 substr3 0 4)" true)
(testit "(substringCiGE substr3 0 4 text1 0 4)" true)
(testit "(substringCiGE text1 0 4 substr3 0 4)" true)

(testit "(substringCiGE substr4 0 4 text2 0 4)" true)
(testit "(substringCiGE text2 0 4 substr4 0 4)" true)
(testit "(substringCiGE substr4 0 4 text1 0 4)" true)
(testit "(substringCiGE text1 0 4 substr4 0 4)" false)

;; Comparison with Substring
(testit "(substringCiGE substr1 0 4 substr4 0 4)" false)
(testit "(substringCiGE substr4 0 4 substr1 0 4)" true)
(testit "(substringCiGE substr1 0 4 substr3 0 4)" true)
(testit "(substringCiGE substr3 0 4 substr1 0 4)" true)

(testit "(substringCiGE substr2 0 4 substr4 0 4)" true)
(testit "(substringCiGE substr4 0 4 substr2 0 4)" true)
(testit "(substringCiGE substr2 0 4 substr3 0 4)" true)
(testit "(substringCiGE substr3 0 4 substr2 0 4)" false)

;;************************************************************************
;;  Trim Function
;;************************************************************************
(setq string1 (makeString "The  Brown  Fox"))
(setq substr1 (makeSubstring string1 0 (length string1)))
(setq substr2 (makeSubstring string1 3 12))
(setq substr3 (makeSubstring string1 0 12))
(testit "(trim substr1)" "The Brown Fox")
(testit "(trim substr2)" "Brown Fox")
(testit "(trim substr3)" "The Brown")

;;************************************************************************
;;  Substitute Function
;;************************************************************************
(setq string1 (makeString "It is better to be silent and be thought a fool"))
(setq substr1 (makeSubstring string1 0 25))
(setq string2 (makeString "BETTER and SILENT"))
(setq string3 (makeString "better and silent"))
(setq substr2 (makeSubstring string2 0 6))
(setq substr3 (makeSubstring string3 0 6))

;; substitute something existing
(testit "(substitute substr1 \"is\" \"IS\")" "It IS better to be silent")
(testit "substr1" "It is better to be silent")
(testit "string1" "It is better to be silent and be thought a fool")

;; substitute something not existing
(testit "(substitute substr1 \"fool\" \"FOOL\")" "It is better to be silent")
(testit "substr1" "It is better to be silent")
(testit "string1" "It is better to be silent and be thought a fool")

;; substitute using a substring
(testit "(substitute substr1 substr3 substr2)" "It is BETTER to be silent")

(setq string4 (makeString "aa bb cc dd aa bb cc dd aa"))
(setq substr4 (makeSubstring string4 0 (length string4)))
(testit "(substitute substr4 \"aa\" \"AA\")" "AA bb cc dd AA bb cc dd AA")
(testit "(substitute substr4 \"aa\" \"AA\" 1)" "AA bb cc dd aa bb cc dd aa")
(testit "(substitute substr4 \"aa\" \"AA\" 2)" "AA bb cc dd AA bb cc dd aa")
(testit "(substitute substr4 \"aa\" \"AA\" 3)" "AA bb cc dd AA bb cc dd AA")
(testit "(substitute substr4 \"aa\" \"AA\" 4)" "AA bb cc dd AA bb cc dd AA")

;;************************************************************************
;;  Repeat Function
;;************************************************************************
(setq string5 (makeString "abcde fghij"))
(setq substr5 (makeSubstring string5 0 5))
(testit "(rept substr5 0)" "")
(testit "(rept substr5 1)" "abcde")
(testit "(rept substr5 2)" "abcdeabcde")

;;************************************************************************
;;  Replace Function
;;************************************************************************
(setq string5 (makeString "abcde fghij"))
(setq substr5 (makeSubstring string5 0 (length string5)))
(testit "(replace substr5 0 5 \"fghij\")" "fghij fghij")
(testit "(replace substr5 6 5 \"abcde\")" "abcde abcde")
(testit "substr5" "abcde fghij")

;;************************************************************************
;;  Mid Function
;;************************************************************************
(setq string6 (makeString "abcde fghij klmno"))
(setq substr6 (makeSubstring string6 6 11))
(testit "(mid substr6 0 5)" "fghij")
(testit "(mid substr6 6 5)" "klmno")

;;************************************************************************
;;  Right Function
;;************************************************************************
(testit "(right substr6 5)" "klmno")
(testit "(right substr6 11)" "fghij klmno")

;;************************************************************************
;;  Left Function
;;************************************************************************
(testit "(left substr6 5)" "fghij")
(testit "(left substr6 11)" "fghij klmno")

;;************************************************************************
;;  Find Function
;;************************************************************************
(setq string7 (makeString "abcde aa bb cc aa bb cc fghij"))
(setq substr7 (makeSubstring string7 0 (length string7)))
(testit "(find \"abcde\" substr7)" 0)
(testit "(find \"aa\" substr7)" 6)
(testit "(find \"fghij\" substr7)" 24)

;;************************************************************************
;;  Clean Function
;;************************************************************************
(setq byteCodes (new ByteVector: 10 72 101 108 108 111 16 17 18 19 0))
(setq substr8 (makeSubstring byteCodes 0 10))
(testit "(clean substr8)" "Hello")

(setq byteCodes (new ByteVector: 10 16 17 18 19 72 101 108 108 111 0))
(setq substr8 (makeSubstring byteCodes 0 10))
(testit "(clean substr8)" "Hello")

;;************************************************************************
;;  Code Function
;;************************************************************************
(setq codeA (code "A"))
(setq codeB (code "B"))
(setq string8 (makeString "A B C D"))
(setq substr1 (makeSubstring string8 0 3))
(setq substr2 (makeSubstring string8 2 3))
(testit "(code substr1)" codeA)
(testit "(code substr2)" codeB)

;;************************************************************************
;;  stringToBVector Function
;;************************************************************************
(setq string7 (makeString "abcde aa bb cc aa bb cc fghij"))
(setq substr7 (makeSubstring string7 0 (length string7)))
(testit "(type (setq objvec7 (stringToBVector substr7 \" \")))" ObjVector:)
(testit "(type objvec7[0])" ByteVector:)
(testit "objvec7[0]" "abcde ")
(testit "(type objvec7[1])" ByteVector:)
(testit "objvec7[1]" "aa ")
(testit "(type objvec7[2])" ByteVector:)
(testit "objvec7[2]" "bb ")
(testit "(type objvec7[3])" ByteVector:)
(testit "objvec7[3]" "cc ")
(testit "(type objvec7[4])" ByteVector:)
(testit "objvec7[4]" "aa ")
(testit "(type objvec7[5])" ByteVector:)
(testit "objvec7[5]" "bb ")
(testit "(type objvec7[6])" ByteVector:)
(testit "objvec7[6]" "cc ")
(testit "(type objvec7[7])" ByteVector:)
(testit "objvec7[7]" "fghij")

(setq string7 (makeString "abcde aa bb cc aa bb cc fghij"))
(setq key7 (makeSubstring string7 5 1))
(setq substr7 (makeSubstring string7 0 (length string7)))
(testit "(type (setq objvec7 (stringToBVector substr7 key7)))" ObjVector:)
(testit "(type objvec7[0])" ByteVector:)
(testit "objvec7[0]" "abcde ")
(testit "(type objvec7[1])" ByteVector:)
(testit "objvec7[1]" "aa ")
(testit "(type objvec7[2])" ByteVector:)
(testit "objvec7[2]" "bb ")
(testit "(type objvec7[3])" ByteVector:)
(testit "objvec7[3]" "cc ")
(testit "(type objvec7[4])" ByteVector:)
(testit "objvec7[4]" "aa ")
(testit "(type objvec7[5])" ByteVector:)
(testit "objvec7[5]" "bb ")
(testit "(type objvec7[6])" ByteVector:)
(testit "objvec7[6]" "cc ")
(testit "(type objvec7[7])" ByteVector:)
(testit "objvec7[7]" "fghij")

(setq string8 (makeString "abcde aa bb cc aa bb cc fghij"))
(setq key8 (makeSubstring string8 5 1))
(setq substr8 (makeSubstring string8 0 (length string8)))
(testit "(type (setq vec8 (stringToVector substr8 key8)))" Vector:)
(testit "(type vec8[0])" String:)
(testit "vec8[0]" "abcde")
(testit "(type vec8[1])" String:)
(testit "vec8[1]" "aa")
(testit "(type vec8[2])" String:)
(testit "vec8[2]" "bb")
(testit "(type vec8[3])" String:)
(testit "vec8[3]" "cc")
(testit "(type vec8[4])" String:)
(testit "vec8[4]" "aa")
(testit "(type vec8[5])" String:)
(testit "vec8[5]" "bb")
(testit "(type vec8[6])" String:)
(testit "vec8[6]" "cc")
(testit "(type vec8[7])" String:)
(testit "vec8[7]" "fghij")

(testit "(gc)" #void)

;;************************************************************************
;;  String Compare Functions
;;************************************************************************
(setq string1 (makeString "Hello"))
(setq string2 (makeString "World"))
(setq string3 (makeString "hello"))
(setq string4 (makeString "world"))
(setq string5 (makeString "..Hello.."))
(setq string6 (makeString "..World.."))
(setq string7 (makeString "..hello.."))
(setq string8 (makeString "..world.."))

(setq substr1 (makeSubstring string5 2 5)) ;; Hello
(setq substr2 (makeSubstring string6 2 5)) ;; World
(setq substr3 (makeSubstring string7 2 5)) ;; hello
(setq substr4 (makeSubstring string8 2 5)) ;; world

(setq text1 "Hello")
(setq text2 "World")
(setq text3 "hello")
(setq text4 "world")

;; (stringCiLT)
;; hello < World = true
;; World < hello = false
;; hello < Hello = false
;; Hello < hello = false

;; world < World = false
;; World < world = false
;; world < Hello = false
;; Hello < world = true

;; Comparison with String
(testit "(stringCiLT substr3 string2)" true)
(testit "(stringCiLT string2 substr3)" false)
(testit "(stringCiLT substr3 string1)" false)
(testit "(stringCiLT string1 substr3)" false)

(testit "(stringCiLT substr4 string2)" false)
(testit "(stringCiLT string2 substr4)" false)
(testit "(stringCiLT substr4 string1)" false)
(testit "(stringCiLT string1 substr4)" true)

;; Comparison with Text
(testit "(stringCiLT substr3 text2)" true)
(testit "(stringCiLT text2 substr3)" false)
(testit "(stringCiLT substr3 text1)" false)
(testit "(stringCiLT text1 substr3)" false)

(testit "(stringCiLT substr4 text2)" false)
(testit "(stringCiLT text2 substr4)" false)
(testit "(stringCiLT substr4 text1)" false)
(testit "(stringCiLT text1 substr4)" true)

;; Comparison with Substring
(testit "(stringCiLT substr1 substr4)" true)
(testit "(stringCiLT substr4 substr1)" false)
(testit "(stringCiLT substr1 substr3)" false)
(testit "(stringCiLT substr3 substr1)" false)

(testit "(stringCiLT substr2 substr4)" false)
(testit "(stringCiLT substr4 substr2)" false)
(testit "(stringCiLT substr2 substr3)" false)
(testit "(stringCiLT substr3 substr2)" true)

;; (stringCiLE)
;; hello <= World = true
;; World <= hello = false
;; hello <= Hello = true
;; Hello <= hello = true

;; world <= World = true
;; World <= world = true
;; world <= Hello = false
;; Hello <= world = true

;; Comparison with String
(testit "(stringCiLE substr3 string2)" true)
(testit "(stringCiLE string2 substr3)" false)
(testit "(stringCiLE substr3 string1)" true)
(testit "(stringCiLE string1 substr3)" true)

(testit "(stringCiLE substr4 string2)" true)
(testit "(stringCiLE string2 substr4)" true)
(testit "(stringCiLE substr4 string1)" false)
(testit "(stringCiLE string1 substr4)" true)

;; Comparison with Text
(testit "(stringCiLE substr3 text2)" true)
(testit "(stringCiLE text2 substr3)" false)
(testit "(stringCiLE substr3 text1)" true)
(testit "(stringCiLE text1 substr3)" true)

(testit "(stringCiLE substr4 text2)" true)
(testit "(stringCiLE text2 substr4)" true)
(testit "(stringCiLE substr4 text1)" false)
(testit "(stringCiLE text1 substr4)" true)

;; Comparison with Substring
(testit "(stringCiLE substr1 substr4)" true)
(testit "(stringCiLE substr4 substr1)" false)
(testit "(stringCiLE substr1 substr3)" true)
(testit "(stringCiLE substr3 substr1)" true)

(testit "(stringCiLE substr2 substr4)" true)
(testit "(stringCiLE substr4 substr2)" true)
(testit "(stringCiLE substr2 substr3)" false)
(testit "(stringCiLE substr3 substr2)" true)

;; (stringCiEQ)
;; hello = World = false
;; World = hello = false
;; hello = Hello = true
;; Hello = hello = true

;; world = World = true
;; World = world = true
;; world = Hello = false
;; Hello = world = false

;; Comparison with String
(testit "(stringCiEQ substr3 string2)" false)
(testit "(stringCiEQ string2 substr3)" false)
(testit "(stringCiEQ substr3 string1)" true)
(testit "(stringCiEQ string1 substr3)" true)

(testit "(stringCiEQ substr4 string2)" true)
(testit "(stringCiEQ string2 substr4)" true)
(testit "(stringCiEQ substr4 string1)" false)
(testit "(stringCiEQ string1 substr4)" false)

;; Comparison with Text
(testit "(stringCiEQ substr3 text2)" false)
(testit "(stringCiEQ text2 substr3)" false)
(testit "(stringCiEQ substr3 text1)" true)
(testit "(stringCiEQ text1 substr3)" true)

(testit "(stringCiEQ substr4 text2)" true)
(testit "(stringCiEQ text2 substr4)" true)
(testit "(stringCiEQ substr4 text1)" false)
(testit "(stringCiEQ text1 substr4)" false)

;; Comparison with Substring
(testit "(stringCiEQ substr1 substr4)" false)
(testit "(stringCiEQ substr4 substr1)" false)
(testit "(stringCiEQ substr1 substr3)" true)
(testit "(stringCiEQ substr3 substr1)" true)

(testit "(stringCiEQ substr2 substr4)" true)
(testit "(stringCiEQ substr4 substr2)" true)
(testit "(stringCiEQ substr2 substr3)" false)
(testit "(stringCiEQ substr3 substr2)" false)

;; (stringCiNE)
;; hello != World = true
;; World != hello = true
;; hello != Hello = false
;; Hello != hello = false

;; world != World = false
;; World != world = false
;; world != Hello = true
;; Hello != world = true

;; Comparison with String
(testit "(stringCiNE substr3 string2)" true)
(testit "(stringCiNE string2 substr3)" true)
(testit "(stringCiNE substr3 string1)" false)
(testit "(stringCiNE string1 substr3)" false)

(testit "(stringCiNE substr4 string2)" false)
(testit "(stringCiNE string2 substr4)" false)
(testit "(stringCiNE substr4 string1)" true)
(testit "(stringCiNE string1 substr4)" true)

;; Comparison with Text
(testit "(stringCiNE substr3 text2)" true)
(testit "(stringCiNE text2 substr3)" true)
(testit "(stringCiNE substr3 text1)" false)
(testit "(stringCiNE text1 substr3)" false)

(testit "(stringCiNE substr4 text2)" false)
(testit "(stringCiNE text2 substr4)" false)
(testit "(stringCiNE substr4 text1)" true)
(testit "(stringCiNE text1 substr4)" true)

;; Comparison with Substring
(testit "(stringCiNE substr1 substr4)" true)
(testit "(stringCiNE substr4 substr1)" true)
(testit "(stringCiNE substr1 substr3)" false)
(testit "(stringCiNE substr3 substr1)" false)

(testit "(stringCiNE substr2 substr4)" false)
(testit "(stringCiNE substr4 substr2)" false)
(testit "(stringCiNE substr2 substr3)" true)
(testit "(stringCiNE substr3 substr2)" true)

;; (stringCiGT)
;; hello > World = false
;; World > hello = true
;; hello > Hello = false
;; Hello > hello = false

;; world > World = true
;; World > world = true
;; world > Hello = true
;; Hello > world = false

;; Comparison with String
(testit "(stringCiGT substr3 string2)" false)
(testit "(stringCiGT string2 substr3)" true)
(testit "(stringCiGT substr3 string1)" false)
(testit "(stringCiGT string1 substr3)" false)

(testit "(stringCiGT substr4 string2)" false)
(testit "(stringCiGT string2 substr4)" false)
(testit "(stringCiGT substr4 string1)" true)
(testit "(stringCiGT string1 substr4)" false)

;; Comparison with Text
(testit "(stringCiGT substr3 text2)" false)
(testit "(stringCiGT text2 substr3)" true)
(testit "(stringCiGT substr3 text1)" false)
(testit "(stringCiGT text1 substr3)" false)

(testit "(stringCiGT substr4 text2)" false)
(testit "(stringCiGT text2 substr4)" false)
(testit "(stringCiGT substr4 text1)" true)
(testit "(stringCiGT text1 substr4)" false)

;; Comparison with string
(testit "(stringCiGT substr1 substr4)" false)
(testit "(stringCiGT substr4 substr1)" true)
(testit "(stringCiGT substr1 substr3)" false)
(testit "(stringCiGT substr3 substr1)" false)

(testit "(stringCiGT substr2 substr4)" false)
(testit "(stringCiGT substr4 substr2)" false)
(testit "(stringCiGT substr2 substr3)" true)
(testit "(stringCiGT substr3 substr2)" false)

;; (stringCiGE)
;; hello >= World = false
;; World >= hello = true
;; hello >= Hello = true
;; Hello >= hello = true

;; world >= World = true
;; World >= world = true
;; world >= Hello = true
;; Hello >= world = false

;; Comparison with String
(testit "(stringCiGE substr3 string2)" false)
(testit "(stringCiGE string2 substr3)" true)
(testit "(stringCiGE substr3 string1)" true)
(testit "(stringCiGE string1 substr3)" true)

(testit "(stringCiGE substr4 string2)" true)
(testit "(stringCiGE string2 substr4)" true)
(testit "(stringCiGE substr4 string1)" true)
(testit "(stringCiGE string1 substr4)" false)

;; Comparison with Text
(testit "(stringCiGE substr3 text2)" false)
(testit "(stringCiGE text2 substr3)" true)
(testit "(stringCiGE substr3 text1)" true)
(testit "(stringCiGE text1 substr3)" true)

(testit "(stringCiGE substr4 text2)" true)
(testit "(stringCiGE text2 substr4)" true)
(testit "(stringCiGE substr4 text1)" true)
(testit "(stringCiGE text1 substr4)" false)

;; Comparison with string
(testit "(stringCiGE substr1 substr4)" false)
(testit "(stringCiGE substr4 substr1)" true)
(testit "(stringCiGE substr1 substr3)" true)
(testit "(stringCiGE substr3 substr1)" true)

(testit "(stringCiGE substr2 substr4)" true)
(testit "(stringCiGE substr4 substr2)" true)
(testit "(stringCiGE substr2 substr3)" true)
(testit "(stringCiGE substr3 substr2)" false)

;;************************************************************************
;;  Balance Function
;;************************************************************************
(setq string1 (makeString "(+ 1 (+ 2 (+ 3 4)))"))
(setq substr1 (makeSubstring string1 0 (length string1)))
(testit "(balance substr1 0)" false)
(testit "(balance substr1 1)" "(+ 1 (+ 2 (+ 3 4)))")
(testit "(balance substr1 6)" "(+ 2 (+ 3 4))")
(testit "(balance substr1 11)" "(+ 3 4)")

;;************************************************************************
;;  Eval Function
;;************************************************************************
(setq string1 (makeString "(+ 1 1) (+ 2 2) (* 4 4)"))
(setq substr1 (makeSubstring string1 0 7))
(setq substr2 (makeSubstring string1 8 7))
(setq substr3 (makeSubstring string1 16 7))
(testit "(eval substr1)" 2)
(testit "(eval substr2)" 4)
(testit "(eval substr3)" 16)

;;************************************************************************
;;  Dimension Function
;;************************************************************************
(setq string1 (makeString "The Art of Demotivation"))
(setq substr1 (makeSubstring string1 0 3))
(setq substr2 (makeSubstring string1 4 3))
(setq substr3 (makeSubstring string1 8 2))
(setq substr4 (makeSubstring string1 11 12))

(testit "(dimension substr1)" 1)
(testit "(dimension substr2)" 1)
(testit "(dimension substr3)" 1)

;;************************************************************************
;;  Length Function
;;************************************************************************
(testit "(length substr1)" 3)
(testit "(length substr2)" 3)
(testit "(length substr3)" 2)
(testit "(length substr4)" 12)

;;************************************************************************
;;  Append Function
;;************************************************************************
(testit "(append \"The Art \" substr3 \" Demotivation\")" "The Art of Demotivation")
(testit "(append substr1 \" \" substr2)" "The Art")

;;************************************************************************
;;  ObjectRepository Function
;;************************************************************************
(setq fileString (makeString "..... Substring.db ....."))
(setq fileSubstr (makeSubstring fileString 6 12))
(testit "(type (setq objRepo (new ObjectRepository: fileSubstr clear:)))" ObjectRepository:)
(setq objRepo["key"] "Value")
(setq objRepo #void)
(testit "(type (setq objRepo (new ObjectRepository: \"Substring.db\")))" ObjectRepository:)
(testit "objRepo[\"key\"]" "Value")

;;************************************************************************
;;  ObjectToDictionary Function
;;************************************************************************
(setq keyString (makeString "... key1 ... key2 ..."))
(setq key1 (makeSubstring keyString 4 4))
(setq key2 (makeSubstring keyString 13 4))
(testit "(type (setq vec (new Vector: 4 key1 \"Value1\" key2 \"Value2\")))" Vector:)
(testit "(type (setq dic (objectToDictionary vec)))" Dictionary:)
(testit "dic[\"key1\"]" "Value1")
(testit "dic[\"key2\"]" "Value2")
(testit "dic[key1]" "Value1")
(testit "dic[key2]" "Value2")
(testit "(type (setq dic[key1] \"NewValue1\"))" Dictionary:)
(testit "(type (setq dic[key2] \"NewValue2\"))" Dictionary:)
(testit "dic[key1]" "NewValue1")
(testit "dic[key2]" "NewValue2")

;;************************************************************************
;;  ObjVector
;;************************************************************************
(setq objVec (new ObjVector: 2))
(setq valueStr (makeString "... value1 value2 ..."))
(testit "(setq value1 (makeSubstring valueStr 4 6))" "value1")
(testit "(setq value2 (makeSubstring valueStr 11 6))" "value2")
(testit "(type (setq objVec[0] value1))" ObjVector:)
(testit "(type (setq objVec[1] value2))" ObjVector:)
(testit "(type objVec[0])" String:)
(testit "(type objVec[1])" String:)
(testit "objVec[0]" "value1")
(testit "objVec[1]" "value2")

;;************************************************************************
;;  Substring usage in Brick type
;;************************************************************************
(setq rec (new Brick: 2 String1:Character:10 String2:Character:10))
(setq rec[0].String1 "abcdefghij")
(setq rec[1].String1 "abcdefghi")
(testit "(type rec[0])" BrickRow:)
(testit "(type rec[1])" BrickRow:)
(testit "(length rec[0])" 2)
(testit "(length rec[1])" 2)
(testit "rec[0].String1" "abcdefghij")
(testit "rec[1].String1" "abcdefghi")
(setq rec[0].String2 "abcde")
(setq rec[1].String2 "abcdefghij")
(testit "(type rec[0].String2)" Substring:)
(testit "(type rec[1].String2)" Substring:)
(testit "(length rec[0].String1)" 10)
(testit "(length rec[1].String1)" 9)
(testit "rec[0].String2" "abcde")
(testit "rec[1].String2" "abcdefghij")

;;************************************************************************
;;  Substring usage in Structure type
;;************************************************************************
(setq struct (new Structure:))
(setq keyvalStr (makeString "key1 key2 key3 value1 value2 value3"))
(setq key1 (makeSubstring keyvalStr 0 4))
(setq key2 (makeSubstring keyvalStr 5 4))
(setq key3 (makeSubstring keyvalStr 10 4))
(setq val1 (makeSubstring keyvalStr 15 6))
(setq val2 (makeSubstring keyvalStr 22 6))
(setq val3 (makeSubstring keyvalStr 29 6))
(testit "(type (setq struct[key1] val1))" Structure:)
(testit "(type (setq struct[key2] val2))" Structure:)
(testit "(type (setq struct[key3] val3))" Structure:)
(testit "struct[key1:]" "value1")
(testit "struct[key2:]" "value2")
(testit "struct[key3:]" "value3")

;;************************************************************************
;;  Saving and Loading from Object Repository
;;************************************************************************
(setq objRepo (new ObjectRepository: "Substring.db" clear:))
(setq substr (makeSubstring (makeString "Hello World") 0 11))
(testit "(type (setq objRepo.key substr))" ObjectRepository:)
(testit "(setq substr #void)" #void)
(testit "(gc compact:)" #void)
(testit "(type (setq substr objRepo.key))" Substring:)
(testit "substr" "Hello World")


;;************************************************************************
;;  Garbage Collection
;;************************************************************************
(setq str (makeString "Hello World"))
(setq substr (makeSubstring str 0 5))
(testit "substr" "Hello")
(testit "str" "Hello World")
(testit "(gc compact:)" #void)
(testit "substr" "Hello")
(testit "str" "Hello World")

(setq argVec1 #void)
(setq byteCodes #void)
(setq byteVec1 #void)
(setq byteVec2 #void)
(setq byteVecBase #void)
(setq char1 #void)
(setq char2 #void)
(setq codeA #void)
(setq codeB #void)
(setq dstVec1 #void)
(setq fileString #void)
(setq fileSubstr #void)
(setq hashResult #void)
(setq key1 #void)
(setq key2 #void)
(setq key3 #void)
(setq key7 #void)
(setq key8 #void)
(setq keyString #void)
(setq keyvalStr #void)
(setq mat1 #void)
(setq mat2 #void)
(setq myDateBase #void)
(setq myTimeBase #void)
(setq objRepo #void)
(setq objVec #void)
(setq objvec1 #void)
(setq objvec2 #void)
(setq outputByteVector #void)
(setq quotedSymBase #void)
(setq rec #void)
(setq srcVec1 #void)
(setq str #void)
(setq strBase #void)
(setq strDateBase #void)
(setq strDateSym #void)
(setq string1 #void)
(setq string2 #void)
(setq string3 #void)
(setq string4 #void)
(setq string5 #void)
(setq string6 #void)
(setq string7 #void)
(setq string8 #void)
(setq strKeyBase #void)
(setq strLispBase #void)
(setq strSubstr #void)
(setq strTimeBase #void)
(setq struct #void)
(setq strValue #void)
(setq substr #void)
(setq substr1 #void)
(setq substr2 #void)
(setq substr3 #void)
(setq substr4 #void)
(setq substr5 #void)
(setq substr6 #void)
(setq substr7 #void)
(setq substr8 #void)
(setq symBase #void)
(setq symbol1 #void)
(setq symbol2 #void)
(setq text1 #void)
(setq text2 #void)
(setq text3 #void)
(setq text4 #void)
(setq val1 #void)
(setq val2 #void)
(setq val3 #void)
(setq valueStr #void)
(setq vec1 #void)
(setq vec2 #void)

(gc compact:)

(testEnd "Test_Substring.sl")
