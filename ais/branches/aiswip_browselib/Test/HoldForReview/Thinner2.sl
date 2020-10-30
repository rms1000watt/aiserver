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
;; Thinner2.sl
;; Strips whitelines and PROGRESS-style comments.
;;
;; Version notes:
;; - use icompare rather than < > = etc.
;; - store fullSource[sourcePos] in a variable rather than repeatedly referencing it
;; - store (length fullSource) in a variable rather than repeatedly referencing it
;; - use integer rather than boolean, so that we could use icompare.

(defun thinner(sourceFileName targetFileName)
    vars:(commentLevel fileId fileLength fullSource fullTarget i1 isWhiteLine lineBuffer linePos numLines sourcePos targetPos)

    ; Read the source file into a byte vector.
    (setq fileId (fileOpen sourceFileName 0 0)) ;; 0=existing file, 0=text file
    (setq fullSource (fileRead fileId))          
    (fileClose fileId 1) ;; 1=don't erase

    ; Create a target byte vector, using the source vector's length as the new target vector's length.
    (setq fullTarget (new Vector: byte: (length fullSource)))

    ; Line buffer. Use this so we can trap "white" lines. Assume 2048 chars is enough.
    (setq lineBuffer (new Vector: byte: 2048))

    ; Initialize variables.
    (setq isWhiteLine 1)
    (setq linePos 0)
    (setq numLines 0)
    (setq sourcePos 0)
    (setq targetPos 0)
    (setq fileLength (length fullSource))
 
    (while (icompareLT sourcePos fileLength) do

        (setq fullSourceItem fullSource[sourcePos])
        
        ; Carriage return or eof
        ; Note: dos=cr+lf, unix=lf, mac=cr. The following won't work on mac.
        (if (or (= #\newline fullSourceItem) (icompareEQ sourcePos (isub fileLength 1)))
            (begin
                (if (icompareEQ isWhiteLine 0)
                    ; not whiteline - add the line buffer's chars to the target vector.
                    (begin
                        (setq numLines (iadd 1 numLines))
                        (loop for i1 from 0 until linePos do
                            (setq fullTarget[targetPos] lineBuffer[i1])
                            (setq targetPos (iadd 1 targetPos)) )
                        (setq fullTarget[targetPos] #\newline)
                        (setq targetPos (iadd 1 targetPos)) ) )
                ; Reset the line buffer
                (setq linePos 0)
                (setq isWhiteLine 1)
                ; we're done this iteration
                (goto NEXTCHAR:) ) )

        (if (and (= #\/ fullSourceItem) (= #\* fullSource[(iadd 1 sourcePos)]))
            (setq commentLevel (iadd 1 commentLevel)) )

        (if (icompareEQ 0 commentLevel)
            (begin
                (setq lineBuffer[linePos] (setq fullSourceItem fullSourceItem))
                (setq linePos (iadd linePos 1))
                ; watch for non-whitespace in the line
                (if (and
                         (icompareEQ 1 isWhiteLine)
                         (<> #\tab fullSourceItem)
                         (<> #\space fullSourceItem)
                         (<> #\return fullSourceItem) )
                    (setq isWhiteLine 0) ) ) )

        (if (and (= #\* fullSource[(isub sourcePos 1)]) (= #\/ fullSourceItem))
            (setq commentLevel (isub commentLevel 1)) )

        NEXTCHAR::
        (setq sourcePos (iadd 1 sourcePos)) )

    ; Remove extra space from the end of the target byte vector.
    (resize fullTarget targetPos)

    ; Write the file out.
    (setq fileId (fileOpen targetFileName 1 0)) ;; 1=new file, 0=text file
    (fileWrite fileId fullTarget)   
    (fileClose fileId 1) ;; 1=don't erase

    numLines )



(defun runThinner()
    vars:(i1 numLines)
    (writeln "Start the clock...")
    (loop for i1 from 0 until 100 do
        (setq numLines (thinner "skdbak.ppr" "skdbak.out")) )
    (writeln numLines)
    true )



