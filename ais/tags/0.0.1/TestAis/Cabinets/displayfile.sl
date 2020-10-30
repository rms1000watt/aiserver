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
;; *******************************************************************
;; summary:  Display file named iFileName.
;; Args :    iFileName    The name of the file to be displayed. 
;; Return:   The number of records displayed.
;; *******************************************************************
(defun displayFile(iFileName ) 
	vars:(aFileID			;; The file id for fileName
		aBuffer				;; A data buffer used internally by fileReadRecord
		(aFileType 0)		;; The file type for standard files.
		aRecord				;; Holds record returned from read. 
		(aRecordCount 0)	;; The number of records in the file.
	) ;; end of temporary variables

	;; Open file for reading.
	(setq aFileID (fileOpen iFileName 0 aFileType))
 
	;; Read each record in the file
	(setq aBuffer (fileReadRecord aFileID))
	(while (<> (setq aRecord (fileReadRecord aFileID aBuffer)) #void) do
		(writeln aRecord)
		(++ aRecordCount)
	)
	;; Close file.
	(fileClose aFileID 1)
 
	(return aRecordCount)
) ;; end of displayFile
