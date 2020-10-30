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
;;  Title:    Test Archive Database Test Script
;;
;;  Author:   Michael F. Korns
;;
;;  Project:  Contains the Gemini Text Archive Database
;;            application, procedures, and structures. The
;;            Text Archival Database is a container for 
;;            managing multiple versions of multiple ascii
;;            text files.
;;
;;  Notes:    No other dependencies.  
;;

;;************************************************************************
;; Test script global variables
;;************************************************************************
(setq scriptName "Text Archive Test")

;; *************************************************************************************
;; Archive Test Suite Auxilliary Procedures
;; *************************************************************************************

(writeln   scriptName " started")

;; *******************************************************************
;; name:     diagnostic
;; 
;; summary:  Displays the diagnostic error message.
;; *******************************************************************
(defun diagnostic(msg) ((ringBell) (error "test:failure" msg)))

;; *******************************************************************
;; name:     timingTest
;; 
;; summary:  Define a simple VmScript timing test procedure.
;; args:
;;           proc:      the procedure to be tested
;;           count:     the number of iterations to test
;; *******************************************************************
(define (timingTest proc count) vars:(startTime endTime)
    (setq startTime (getTickCount 0))
    (proc count) 
    (setq endTime (getTickCount startTime))
    (writeln "Elapsed time is " endTime " Seconds" ) endTime)

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
;; name:     compareTextFiles
;; 
;; summary:  Compare the contents of the specified files.
;; args:  fone:      The path and file name of the first file.
;;        ftwo:      The path and file name of the second file.
;; return:   result:    TRUE if the files are equal. 
;; *******************************************************************
(define (compareTextFiles fone ftwo) 
   vars:(i j fileOne  fileTwo)
   (setq fileOne (readTextFile fone))
   (setq fileTwo (readTextFile ftwo))
   (= fileOne fileTwo))  

;; *******************************************************************
;; name:     loadObjectFile
;; 
;; summary:  Perform a single load of the specified object file.
;; Parms:    This procedure accepts two arguments
;;           name:      The name of the file to open.
;;           type:      The type of the file to open.
;;                      1 = a Spreadsheet object file.
;;                      2 = a Smarttable object file.
;;                      3 = a Workspace object file.
;;                      4 = a binary object file.
;; *******************************************************************
(define (loadObjectFile name type) 
   vars:(fileID self)
   (setq fileID (fileOpen name 0 type))
   (setq self (loadObject fileID))
   (fileClose fileID 1)
   self)

;; *******************************************************************
;; name:     saveObjectFile
;; 
;; summary:  Perform a single save of the specified object file.
;; Parms:    This procedure accepts three arguments
;;           name:      The name of the file to create.
;;           type:      The type of the file to create:
;;                      1 = a Spreadsheet object file.
;;                      2 = a Smarttable object file.
;;                      3 = a Workspace object file.
;;                      4 = a binary object file.
;;           self:      The object to be saved in the file.
;; *******************************************************************
(define (saveObjectFile name type self) 
   vars:(fileID)
   (setq fileID (fileOpen name 1 type))
   (saveObject fileID self)
   (fileClose fileID 1)
   self)

;; *************************************************************************************
;; name:     Archive
;; 
;; summary:  Create the structure which holds the standard Archive.
;;           Archives are used to hold indexed objects of a general
;;           nature. Each data object is stored in a contiguous file
;;           of type database, and is indexed by an arbitrary symbol
;;           or string. This allows easy symbolic access to arbitrary 
;;           objects stored in the Archive file on disk.
;;            
;; fields:   name:      The name of the Archive. The default name is:
;;                      "Archive1". This is also the file name.
;;           index:     The index of the Archive Database file.
;;           words:     The Dictionary of all words in the Archive Database file.
;;           fileID:    The file ID of the Archive Database when open;
;;                      otherwise the fileID is #void.
;; Note:     This structure is used to access the database file, of the
;;           same name, which is stored on disk and which contains all of
;;           the stored objects.  
;; *************************************************************************************
(defineStructure Archive: name: index: words: fileID:) 
(defmethod Dictionary: refIndexPage(self key) self[key]) 
(defmethod Dictionary: SetIndexPage(self key objID) (setq self[key] objID)) 
(defmethod Dictionary: delIndexPage(self key) (delete self key))

;; *************************************************************************************
;; name:     History
;; 
;; summary:  Create the structure which holds the standard Archive
;;           History object. History objects hold the contents of 
;;           each file, along with all of its revisions, checked into 
;;           the Archive database.
;;            
;; fields:   name:      The name of the History. This is either the symbolic
;;                      name of the file |foo.c| or a Vector containing the
;;                      symbolic name of the file and the name of the file
;;                      and the History name #(|foo.c| |main branch|).
;;           lines:     The History's line vocabulary Object Vector, containing
;;                      each line in any of the file's revisions, as a unique
;;                      Byte Vector object.
;;           files:     The History's file Dictionary. The key to this Dictionary
;;                      is the unique Revision object for each revision of the file.
;;                      The values of this Dictionary are Object Vectors containing
;;                      the lines (Byte Vectors) in the order they appear in the file.
;;           notes:     The History's notes Dictionary. The key to this Dictionary
;;                      is the unique Revision object for each revision of the file.
;;                      The values of this Dictionary are text or String objects
;;                      containing user entered documentation about the revision.
;;           lineProc: The History's line extraction procedure. This is a procedure
;;                      of one argument (the file contents) which returns a Vector of
;;                      unique line Symbol objects in the order they were extracted
;;                      from the file.
;;           wordProc: The History's word extraction procedure. This is a procedure
;;                      of one argument (the file contents) which returns a Vector of
;;                      unique word Symbol objects in the order they were extracted
;;                      from the file.
;;
;; *************************************************************************************
(defineStructure History: name: lines: files: notes: lineProc: wordProc: lock:) 

;; *************************************************************************************
;; name:     Revision
;; 
;; summary:  Create the structure which holds the standard Archive
;;           Revision object. Revision objects identify the contents of 
;;           each file revision, checked into the Archive database.
;;            
;; fields:   name:      The name of the History. This is either the symbolic
;;                      name of the file |foo.c| or a Vector containing the
;;                      symbolic name of the file and the name of the file
;;                      and the History name #(|foo.c| |main branch|).
;;           time:      The date and time stamp when the revision was archived.
;;
;; *************************************************************************************
(defineStructure Revision: name: time:) 

;; *************************************************************************************
;; name:     makeHistory
;; 
;; summary:  Returns a new History object from the specified arguments.
;; Args:     name:       The name of the new History object (mandatory).
;;           lineProc:  The line extraction procedure for the History.
;;           wordProc:  The word extraction procedure for the History.
;; Return:   self:       The new History object.
;; *************************************************************************************
(defun  makeHistory(name lineProc wordProc)
    vars:(self)
    ;; Create a void History object.
    (setq self (new History: name: name lineProc: lineProc wordProc: wordProc))
    ;; We must create the new History's dictionaries.
    (setq self.lines (makeVector object: 0))
    (setq self.files (makeDictionary))
    (setq self.notes (makeDictionary))
    ;; We must return the new History object.
    self)

;; *************************************************************************************
;; name:     makeArchive
;; 
;; summary:  Returns a new Archive object from the specified arguments.
;; Args:     name:       The name of the new Archive object. If name is #void,
;;                       the Archive will be given a default name. The name
;;                       is the same as the Archive database file name on disk.
;;           New:        If true, a new Archive database file is created on disk. Any data
;;                       in the old Archive is destroyed.  If missing, #void, or false,
;;                       open an existing Archive database file.
;; Return:   archive:    The new Archive object.
;; *************************************************************************************
(defun  makeArchive(name ...)
    vars:(self index words i n New)
    pvars:(archive-counter)
    ;; Do we create a new Archive database file?
    (if (>= (argCount) 2)
        (setq New (argFetch 1))
        (setq New false))
    ;; Create a void Archive object.
    (setq self (new Archive:))
    ;; We must create the new Archive's name
    (if (= name #void)
        (setq self.name (append "Archive" (++ archive-counter)))
        (setq self.name (string name)))
    ;; Are we creating a new Archive database file on disk?
    ;; The new Archive's index is a Dictionary by default.
    ;; The new Archive's word index is a Dictionary by default.
   (if New
        (begin
           (setq self.index (makeDictionary))
           (setq self.words (makeDictionary))
           (setq self.fileID (databaseOpen 0 self.name new:))
           (archiveClose self))
    ;; Are we opening an existing Archive database file on disk?
        (begin
           (archiveOpen self)
           (archiveClose self)))
    ;; We must return the new Archive object.
    self)

;; *************************************************************************************
;; name:     archiveOpen
;; 
;; summary:  Opens the specified Archive database file on disk.
;; Args:     self:       The Archive object to be opened.
;; Return:   self:       The Archive object.
;; *************************************************************************************
(defun  archiveOpen(self)
    ;; Open the database file associated with this Archive object?
    (if (= self.fileID #void)
        (begin
           (setq self.fileID (databaseOpen 0 self.name update:))
           (setq self.index (databaseLoadIndex self.fileID))
           (setq self.words (databaseLoad self.fileID 0 true))))
    ;; We must return the Archive object.
    self)

;; *************************************************************************************
;; name:     archiveClose
;; 
;; summary:  Closes the specified Archive database file on disk.
;; Args:     self:       The Archive object to be closed.
;; Return:   self:       The Archive object.
;; *************************************************************************************
(defun  archiveClose(self)
    vars:(i n more data)
    ;; Close the database file associated with this Archive object?
    (if (<> self.fileID #void)
        (begin
           (databaseSaveIndex self.fileID self.index)
           (databaseSave self.fileID self.words 0 true)
           (databaseClose self.fileID commit:)
           (setq self.fileID #void)))
    ;; We must return the Archive object.
    self)

;; *************************************************************************************
;; name:     archiveSave
;; 
;; summary:  Saves the specified data object into the database file on disk and
;;           associates the data object with the specified key in the Archive 
;;           index object.
;; Args:     self:       The Archive object in which to store the data object.
;;           key:        The index key by which the data object can be retrieved
;;                       later (see the archiveLoad procedure).
;;           data:       The data object to be stored in the database disk file and
;;                       associated with the key in the Archive index.
;;           more:       If true, do not close the database file after saving. If
;;                       missing, #void, or false, close the database file after saving.
;; Return:   data:       The data object which was stored.
;; *************************************************************************************
(defun  archiveSave(self key data ...)
    vars:(i n more objID)
    ;; Do we close the Archive database file after saving?
    (if (>= (argCount) 4)
        (setq more (argFetch 3))
        (setq more false))
    ;; Open the database file associated with this Archive object?
    (archiveOpen self)
    ;; Save the data object and associate it with the index key.
    (setq objID (send refIndexPage: self.index key))
    (setq objID (databaseSave self.fileID data objID true))
    (send SetIndexPage: self.index key objID)
    ;; Close the database file associated with this Archive object?
    (if (not more)
        (archiveClose self))
    ;; We must return the saved data object.
    data)

;; *************************************************************************************
;; name:     archiveLoad
;; 
;; summary:  Loads the specified data object, associated with the specified key, 
;;           from the Archive database file on disk.
;; Args:     self:       The Archive object in which the data object was stored.
;;           key:        The index key by which the data object can be retrieved
;;                       (see the archiveSave procedure).
;;           more:       If true, do not close the database file after loading. If
;;                       missing, #void, or false, close the database file after loading.
;; Return:   data:       The data object which was loaded.
;; *************************************************************************************
(defun  archiveLoad(self key ...)
    vars:(i n more objID data)
    ;; Do we close the Archive database file after loading?
    (if (>= (argCount) 3)
        (setq more (argFetch 2))
        (setq more false))
    ;; Open the database file associated with this Archive object?
    (archiveOpen self)
    ;; Load the data object associated with the index key.
    (setq objID (send refIndexPage: self.index key))
    (if (>= objID 0)
        (setq data (databaseLoad self.fileID objID true))
        (setq data #void))
    ;; Close the database file associated with this Archive object?
    (if (not more)
        (archiveClose self))
    ;; We must return the loaded data object.
    data)

;; *************************************************************************************
;; name:     archiveKill
;; 
;; summary:  Deletes the specified data object from the Archive database file on disk and
;;           deletes the specified key from the Archive index object.
;; Args:     self:       The Archive object from which to delete the data object.
;;           key:        The index key to delete from the index.
;;           more:       If true, do not close the Archive database file after delete. If
;;                       missing, #void, or false, close the database file after delete.
;; Return:   self:       The Archive object after deletion.
;; *************************************************************************************
(defun  archiveKill(self key ...)
    vars:(i n more objID)
    ;; Do we close the Archive database file after saving?
    (if (>= (argCount) 3)
        (setq more (argFetch 2))
        (setq more false))
    ;; Open the database file associated with this Archive object?
    (archiveOpen self)
    ;; Delete the data object and delete the index key.
    (setq objID (send refIndexPage: self.index key))
    (if (>= objID 0)
        (databaseFree self.fileID objID))
    (send delIndexPage: self.index  key)
    ;; Close the database file associated with this Archive object?
    (if (not more)
        (archiveClose self))
    ;; We must return the Archive object.
    self)

;; *************************************************************************************
;; name:     archiveTextWord
;; 
;; summary:  Converts a text data string into an Object Vector of Symbol objects
;;           which represent the words in the original string.
;; Args:     data:       The text data string to be converted.
;; Return:   words:      A Vector containing word String objects.
;; *************************************************************************************
(defun archiveTextWord(data)
    vars:(vec words i n)
 ;; Define the procedure to extract words from lines.
    (defun string->words(data)
        vars:(i j m n vec word cc)
        ;; Create character classifier procedures.
     (defun isDigit(cc) (and (>= cc #\0) (<= cc #\9)))
     (defun isAlpha(cc) 
                       (or 
                         (and (>= cc #\a) (<= cc #\z))
                         (and (>= cc #\A) (<= cc #\Z))))
     (defun isAlphanum(cc) 
                       (or 
                         (and (>= cc #\0) (<= cc #\9))
                         (and (>= cc #\a) (<= cc #\z))
                         (and (>= cc #\A) (<= cc #\Z))))
        ;; Create an empty Vector to hold the word Symbols.
     (setq m 0)
     (setq vec (makeVector normal: m))
     ;; Examine each character in the text string and break the
     ;; text string into words accordingly.
     (setq n (length data))
        (setq j 0)
     (loop for i from 0 until n do
        (setq cc data[i])
        (cond
           ;; Any control character starts a word which ends with
        ;; any non-control character.
           ((<= cc #\space)
           (setq j i)
        (while (and (< i n) (<= data[i] #\space)) do
                 (++ i))
        (-- i)
        (setq word (substring data j i)))

           ;; Any letter starts a word which ends with
        ;; any non-alphanumeric character.
           ((isAlpha cc)
           (setq j i)
        (while (and (< i n) (isAlphanum data[i])) do
                 (++ i))
        (-- i)
        (setq word (substring data j i)))

           ;; Any digit starts a word which ends with
        ;; any non-digit character.
           ((isDigit cc)
           (setq j i)
        (while (and (< i n) (isDigit data[i])) do
                 (++ i))
        (-- i)
        (setq word (substring data j i)))

           ;; Any other character defines its own word.
           (else
        (setq word (substring data i i))))
        (setq vec[m] word)
        (++ m))
        ;; Return the Vector of word Symbol objects.
        vec)

    ;; Create an empty Vector to hold the line Strings.
 (setq vec (stringToVector data _eol))
 (setq n (length vec))
 ;; Append the words in each line to a Vector of words in all lines.
 (setq words (makeVector 0))
 (loop for i from 0 until n do
    (setq words (append words (string->words vec[i]))))
    words)

;; *************************************************************************************
;; name:     archiveTextWord
;; 
;; summary:  Converts a text data string into an Object Vector of Symbol objects
;;           which represent the words in the original text.
;; Args:     data:       The text data string to be converted.
;; Return:   vec:        An Object Vector containing word Symbol objects.
;; *************************************************************************************
(defun archiveTextWord(data)
   (stringToVector data (append _eol " ,.()[]{}\"|:" #\tab) true))

;; *************************************************************************************
;; name:     archiveTextLine
;; 
;; summary:  Converts a text data string into an Object Vector of Symbol objects
;;           which represent the lines in the original string.
;; Args:     data:       The text data string to be converted.
;; Return:   vec:        An Object Vector containing line Symbol objects.
;; *************************************************************************************
(defun archiveTextLine(data)
   (stringToBVector data _eol))

;; *************************************************************************************
;; name:     archiveBinWord
;; 
;; summary:  Converts a binary data string into an Object Vector of Symbol objects
;;           which represent the words in the original string.
;; Args:     data:       The binary data string to be converted.
;; Return:   words:      An Object Vector containing word Symbol objects.
;; *************************************************************************************
(defun archiveBinWord(data) (makeVector normal: 0))

;; *************************************************************************************
;; name:     archiveBinLine
;; 
;; summary:  Converts a binary data string into an Object Vector of Byte Vector objects
;;           which represent the lines in the original string.
;; Args:     data:       The binary data string to be converted.
;; Return:   vec:        An Object Vector containing line Byte Vector objects.
;; *************************************************************************************
(defun archiveBinLine(data)
   pvars:((delim (append (char 10) (char 64)  (char 128)  (char 160)  (char 204))))
   (stringToBVector data delim true))

;; *************************************************************************************
;; name:     archiveDataCheckin
;; 
;; summary:  Checks the specified file contents into the Archive database.
;; Args:     self:       The Archive object into which the file is to be stored.
;;           data:       The contents of the file to be stored in the Archive.
;;           key:        The name of the History. This is either the symbolic
;;                       name of the file "foo.c" or a Vector containing the
;;                       name of the file and the History name #("foo.c" "main branch").
;;           author:     The name of the author who is checking the file in.
;; Return:   return:     If properly saved, an SPair containing the file size in bytes, 
;;                       and the number of changed lines in the file; otherwise, false.
;; *************************************************************************************
(defun  archiveDataCheckin(self data key author)
    vars:(i n history revision ret)
 ;; Define the procedure which will inventory the
 ;; words which have been extracted from the file.
 (defun inventory-words(self data history revision)
       vars:(i n vec word wcount)
       ;; Now we extract the words from the file data
       ;; and add them to the Archive's words Dictionary.
       (setq vec (history.wordProc data))
    (setq n (length vec))
    (loop for i from 0 until n do
       ;; Find the entry for the current word (ignore #void).
       (if (<> vec[i] #void)
           (begin
              (setq word self.words[vec[i]])
              ;; If we have never seen this word before, then
              ;; we must create an a initial entry for it.
              (if (= word #void)
                  (begin
                     (setq word (makeDictionary))
                     (setq self.words[vec[i]] word)))
              ;; Increment the word count for the current revision.
              (setq wcount word[revision])
              (if (= wcount #void)
                  (setq wcount 1)
                  (++ wcount))
              (setq word[revision] wcount)))))
    ;; Define the procedure which will inventory the
 ;; lines which have been extracted from the file.
 (defun inventory-lines(self data history revision)
       vars:(i j n vec line filevec fsize lcount data)
       ;; Here we reset the line count and save the file size.
    (setq lcount 0)
    (setq fsize (length data))
       ;; Now we extract the lines from the file data
       ;; and add them to the Archive's lines Dictionary
       ;; and to the Archive's files Dictionary.
    (setq filevec (makeVector object: 0))
       (setq vec (history.lineProc data))
    (setq n (length vec))
    (loop for i from 0 until n do
       ;; Find the entry for the current line (ignore #void).
       (if (<> vec[i] #void)
           (begin
              (setq line (binaryInsert history.lines vec[i]))
              (setq line history.lines[line])
     (if (= (inspect line) (inspect vec[i]))
         (++ lcount))
              (setq filevec[(length filevec)] line))))
    (setq history.files[revision] filevec)
    (spair fsize lcount))
    ;; If there is no History, with the specified key,
 ;; we must create a new History with the assumption
 ;; that the new History is for a simple text file.
 (setq history (archiveLoad self key true))
 (if (= history #void)
     (setq history (makeHistory key archiveTextLine archiveBinWord)))
 ;; If the History is locked, to another author, we return false.
 (if (and (<> history.lock #void) (<> history.lock author))
     (return false))
 ;; Now we make a new Revision object to use
 ;; when we check the file into the History.
 (setq revision (new Revision: name: key time: (now)))
 ;; Now we extract the words from the file data
 ;; and add them to the Archive's words Dictionary.
 (inventory-words self data history revision)
 ;; Now we extract the lines from the file data
 ;; and add them to the Archive's lines and files Dictionaries.
 (setq ret (inventory-lines self data history revision))
 ;; We must return the Revision object and unlock the History.
 (setq history.lock #void)
 (archiveSave self key history)
 ret)

;; *************************************************************************************
;; name:     archiveCheckin
;; 
;; summary:  Checks the specified file into the Archive database.
;; Args:     self:       The Archive object into which the file is to be stored.
;;           name:       The name of the file to be stored in the Archive.
;;           key:        The name of the History. This is either the symbolic
;;                       name of the file "foo.c" or a Vector containing the
;;                       name of the file and the History name #("foo.c" "main branch").
;;           author:     The name of the author who is checking the file in.
;; Return:   return:     If properly saved, an SPair containing the file size in bytes, 
;;                       and the number of changed lines in the file; otherwise, false.
;; *************************************************************************************
(defun  archiveCheckin(self name key author)
    vars:(data)
    ;; Now we read the file into memory so we can process it.
    (setq data (readTextFile name))
    ;; Now we archive the contents of the file.
    (writeln "Checking in file: " name)
    (archiveDataCheckin self data key author))

;; *************************************************************************************
;; name:     archiveCheckout
;; 
;; summary:  Retrieves the specified file from the Archive database.
;; Args:     self:       The Archive object from which the file is to be retrieved.
;;           name:       The path and file name where the retrieved file is to be written.
;;           key:        The name of the History. This is either the symbolic
;;                       name of the file "foo.c" or a Vector containing the
;;                       name of the file and the History name #("foo.c" "main branch").
;;           author:     The name of the author who is checking the file in.
;; Return:   return:     If properly checked out, true; otherwise false.
;; *************************************************************************************
(defun  archiveCheckout(self name key author)
    vars:(n data history revision)
 ;; Define the procedure to write a revision file to disk.
    (defun write-revision-file(name history revision) 
       vars:(i j filevec fileID (type 0))
       (setq filevec history.files[revision])
       (setq fileID (fileOpen name 1 type))
       (loop for i from 0 until (length filevec) do 
          (fileWrite fileID filevec[i]))
       (fileClose fileID 1))  
 ;; We must retrieve the History from the Archive database.
    (setq history (archiveLoad self key))
 (if (or (= history #void) (= (setq n (length history.files)) 0))
  (error "archiveFile"))
    ;; If the History is locked, to another author, we return false.
    (if (and (<> history.lock #void) (<> history.lock author))
        (return false))
    ;; Now we obtain the last revision stored in the History.
    (setq revision history.files[(sub1 n) 0])
    ;; Now we archive the contents of the file.
    (write-revision-file name history revision)
    true)

;; *************************************************************************************
;; name:     archive-inspect
;; 
;; summary:  Checks the specified file contents into the Archive database.
;; Args:     self:       The Archive object which is to be inspected.
;;           show:       The signal to display inspection statistics.
;; Return:   return:     true.
;; *************************************************************************************
(defun  archive-inspect(self data)
    vars:(ret fileID)
    (setq fileID (databaseOpen 0 self.name share:))
 (setq ret (databaseInspect fileID data))
    (databaseClose fileID refuse:)
    ret)

;; *************************************************************************************
;; name:     remote-word
;; 
;; summary:  Converts a binary data string into an Object Vector of Symbol objects
;;           which represent the words in the original string.
;; Notes:  Represents the storing of behavior in the object database. This procedure
;;           could perform any number of tasks not supported in the application C/C++ code.
;; Args:     data:       The binary data string to be converted.
;; Return:   words:      An Object Vector containing word Symbol objects.
;; *************************************************************************************
(defun remote-word(data) 
 ;; Simulation of extended behavior
 (writeln "**************************************************")
 (writeln "Simulation of remote archive access, or sending")
 (writeln "email or other notification anytime a file is")
 (writeln "checked in. One could also execute make scripts")
 (writeln "and not checkin a file until the test suite has")
 (writeln "actually run.")
 (writeln "**************************************************")
 (makeVector normal: 0))

;; *******************************************************************
;; name:     showWords
;; 
;; summary:  Display the contents of an Archive's word Dictionary.
;; args:  archive:   The Archive whose word Dictionary is to be shown.
;; return:   none:      
;; *******************************************************************
(define (showWords archive) 
   vars:(i j)
   (loop for i from 0 until (length archive.words) do 
      (writeln "[" i "]=" archive.words[i 0])
      (loop for j from 0 until (length archive.words[i]) do 
         (writeln "   " archive.words[i][j 0].name 
                  "{" (date archive.words[i][j 0].time)
                  "}=[" archive.words[i][j] "]"))))

;; *******************************************************************
;; name:     showLines
;; 
;; summary:  Display the contents of a History's line Dictionary.
;; args:  history:   The History whose line Dictionary is to be shown.
;; return:   none:      
;; *******************************************************************
(define (showLines history) 
   vars:(i j)
   (loop for i from 0 until (length history.lines) do 
      (display "[" i "]=" (string history.lines[i]))))

;; *******************************************************************
;; name:     showRevision
;; 
;; summary:  Display the contents of a revision stored in a History.
;; args:  history:   The History whose line Dictionary is to be shown.
;;           revision:  The Revision whose contents are to be shown.
;; return:   none:      
;; *******************************************************************
(define (showRevision history revision) 
   vars:(i j filevec)
   (setq filevec history.files[revision])
   (loop for i from 0 until (length filevec) do 
      (display "[" i "]=" (string filevec[i]))))

;; *************************************************************************************
;; name:     randomize-checkin
;; 
;; summary:  Randomizes then checks the specified file into the Archive database.
;; Args:     self:       The Archive object into which the file is to be stored.
;;           name:       The name of the file to be stored in the Archive.
;;           key:        The name of the History. This is either the symbolic
;;                       name of the file "foo.c" or a Vector containing the
;;                       name of the file and the History name #("foo.c" "main branch").
;;           author:     The name of the author who is checking the file in.
;; Return:   return:     If properly saved, an SPair containing the file size in bytes, 
;;                       and the number of changed lines; otherwise, false.
;; *************************************************************************************
(defun  randomize-checkin(self name key author)
    vars:(data i m n location)
    ;; Now we read the file into memory so we can randomize it.
    (setq data (readTextFile name))
    ;; Now we randomize the contents of the file
    ;; to simulate editing changes between versions.
    (setq data (randomize data))
    ;; Now we archive the contents of the revised file.
    (archiveDataCheckin self data key author))

;; *************************************************************************************
;; name:     multiple-checkin
;; 
;; summary:  Randomizes then checks the specified file into the Archive database, with
;;           the specified multiple of revisions.
;; Args:     self:       The Archive object into which the file is to be stored.
;;           name:       The name of the file to be stored in the Archive.
;;           key:        The name of the History. This is either the symbolic
;;                       name of the file "foo.c" or a Vector containing the
;;                       name of the file and the History name #("foo.c" "main branch").
;;           author:     The name of the author who is checking the file in.
;;           count:      The number of revisions to check in.
;; Return:   return:     If properly saved, an SPair containing the total bytes checked in, 
;;                       and the total number of changed lines; otherwise, false.
;; *************************************************************************************
(defun  multiple-checkin(self name key author count)
    vars:(ret i m n (fsize 0) (lcount 0))
    (loop for i from 1 until count do
       (writeln "checking in revision [" i "] of file " name)
       (setq ret (randomize-checkin self name key author))
       (+= fsize ret[0])
       (+= lcount ret[1]))
    (setq ret (archiveCheckin self name key author))
    (+= fsize ret[0])
    (+= lcount ret[1])
    (spair fsize lcount))

;; *************************************************************************************
;; name:     randomize
;; 
;; summary:  Randomize makes random alterations in the specified data.
;; Args:     data:       The data to be randomized.
;; Return:   data:       The data after randomizing.
;; *************************************************************************************
(defun  randomize(data)
    vars:(i m n location)
    ;; Now we randomize the contents of the data
 ;; to simulate editing changes between versions.
 (setq n (length data))
 (setq m 10)
 (loop for i from 0 until m do
    (setq location (integer (random n)))
    (setq data[location] (+ (modi data[location] 5) 49)))  
    data)

;; *************************************************************************************
;; name:     file-length
;; 
;; summary:  Uses the Microsoft NT dir command to return a vector of the files in
;;           the specified directory.
;; Args:     name:       The Microsoft NT path for the file.
;; Return:   length:     Size of the specified file in bytes.
;; *************************************************************************************
(defun  file-length(name)
    vars:(data)
 ;; Read the file into memory.
    (setq data (readTextFile name))
 (length data))

;; *************************************************************************************
;; Archive Test Suite
;; *************************************************************************************

(setq revisionCount 10)

;; *************************************************************************************
;; Archiving multiple copies of a single ascii file without compression.
;; *************************************************************************************

(if true
    (begin
    (writeln "****************************************")
    (writeln "[test.db] Archiving multiple copies of a single ascii file.")
    (setq revisionCount 10)
    (setq fileName1 "test1.txt")
    (setq archiveName "test.db")
    (setq author Korns:)
    (setq archive (makeArchive archiveName true))
    (writeln "Starting randomize checking in " revisionCount " revisions to " fileName1)
    (setq startTime (getTickCount 0))
    (setq ret (multiple-checkin archive fileName1 fileName1 author revisionCount))
    (archiveCheckout archive "temp.tst" fileName1 author)
    (if (compareTextFiles fileName1 "temp.tst")
        (writeln fileName1 " checked in and out sucessfully")
        (writeln "** " fileName1 " check in and out failure **"))
    (setq endTime (getTickCount startTime))
    (writeln "Total file bytes checked in = " ret[0] ", total lines changed = " ret[1])
    (writeln "Total archive database size in bytes = " (file-length archiveName))
    (writeln "Checking in " revisionCount " revisions to " fileName1 " required " endTime " seconds.")
    (writeln "****************************************")))

;; *************************************************************************************
;; Archiving multiple copies of a single ascii file with word dictionary.
;; *************************************************************************************

(if true
    (begin
    (writeln "[test.db] Archiving multiple copies of a single ascii file with word dictionary.")
    (setq revisionCount 3)
    (setq fileName2 "test2.txt")
    (setq archiveName "test.db")
    (setq author Korns:)
    (setq archive (makeArchive archiveName))
    (writeln "Starting randomize checking in " revisionCount " revisions to " fileName2)
    (setq startTime (getTickCount 0))
    (setq ret (multiple-checkin archive fileName2 fileName2 author revisionCount))
    (archiveCheckout archive "temp.tst" fileName2 author)
    (if (compareTextFiles fileName2 "temp.tst")
        (writeln fileName2 " checked in and out sucessfully")
        (writeln "** " fileName2 " check in and out failure **"))
    (setq endTime (getTickCount startTime))
    (writeln "Total file bytes checked in = " ret[0] ", total lines changed = " ret[1])
    (writeln "Total archive database size in bytes = " (file-length archiveName))
    (writeln "Checking in " revisionCount " revisions to " fileName2 " required " endTime " seconds.")
    (writeln "****************************************")))

;; *************************************************************************************
;; Archiving multiple copies of a single ascii file with compression.
;; *************************************************************************************

(if true
    (begin
    (writeln "[test.db] Archiving multiple copies of a single ascii file with compression.")
    (setq revisionCount 10)
    (setq fileName3 "test3.txt")
    (setq archiveName "test.db")
    (setq author Korns:)
    (setq archive (makeArchive archiveName))
    (writeln "Starting randomize checking in " revisionCount " revisions to " fileName3)
    (setq startTime (getTickCount 0))
    (setq ret (multiple-checkin archive fileName3 fileName3 author revisionCount))
    (archiveCheckout archive "temp.tst" fileName3 author)
    (if (compareTextFiles fileName3 "temp.tst")
        (writeln fileName3 " checked in and out sucessfully")
        (writeln "** " fileName3 " check in and out failure **"))
    (setq endTime (getTickCount startTime))
    (writeln "Total file bytes checked in = " ret[0] ", total lines changed = " ret[1])
    (writeln "Total archive database size in bytes = " (file-length archiveName))
    (writeln "Checking in " revisionCount " revisions to " fileName3 " required " endTime " seconds.")
    (writeln "****************************************")))


;; *************************************************************************************
;; Check out previous files to make sure they are still there.
;; *************************************************************************************

(archiveCheckout archive "temp.tst" fileName1 author)
(if (compareTextFiles fileName1 "temp.tst")
    (writeln fileName1 " checked in and out sucessfully")
    (writeln "** " fileName1 " check in and out failure **"))
(archiveCheckout archive "temp.tst" fileName2 author)
(if (compareTextFiles fileName2 "temp.tst")
    (writeln fileName2 " checked in and out sucessfully")
    (writeln "** " fileName2 " check in and out failure **"))
(archiveCheckout archive "temp.tst" fileName3 author)
(if (compareTextFiles fileName3 "temp.tst")
    (writeln fileName3 " checked in and out sucessfully")
    (writeln "** " fileName3 " check in and out failure **"))

;; *************************************************************************************
;; Clean up Archiving multiple copies of a single ascii file with compression.
;; *************************************************************************************

Last::

(archive-inspect archive pages:)
(setq fileID (fileOpen "temp.tst" 1 0)) 
(fileClose fileID 0) 
(setq fileID (fileOpen "test.db" 1 5)) 
(fileClose fileID 0) 

(writeln "****************************************")

(writeln   scriptName " completed")


