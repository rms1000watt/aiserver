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
(defmacro deforphan(parentName childName ...)
   vars:(result tail)
   (setq tail (setLastCdr (list lambda:) (argFetch 2)))
   (if (isSymbol parentName)
       (setq result (list set: (list ref: (symbol parentName) ''Pv) (makeQuotedSymbol childName) tail)))
   (if (isPair parentName) 
       (setq result (list set: (list ref: parentName ''Pv) (makeQuotedSymbol childName) tail)))
   (setq result (list 'ref result (makeQuotedSymbol childName)))
   result)

(defmacro defchild(parentName childName ...)
   vars:(result tail)
   (setq result (apply makeQuotedList (setLastCdr (list defun: childName) (argFetch 2))))
   (setq result (list morph: result))
   (if (isSymbol parentName)
       (setq tail (list 'makeLambda ''Pv (list 'ref (symbol parentName) ''Pv) ''Cv (list 'ref (symbol parentName) ''Cv))))
   (if (isPair parentName)
       (setq tail (list 'makeLambda ''Pv (list 'ref parentName ''Pv) ''Cv (list 'ref parentName ''Cv))))
   (setq result (list 'ref (list compile: result tail true) (makeQuotedSymbol childName)))
   result)

(defmacro defriend(parentName childName ...)
   vars:(result tail orphan)
   (setq orphan (apply makeQuotedList (setLastCdr (list lambda:) (argFetch 2))))
   (setq orphan (list morph: orphan))
   (if (isSymbol parentName)
       (setq tail (list 'makeLambda ''Cv (list 'ref (symbol parentName) ''Pv))))
   (if (isPair parentName)
       (setq tail (list 'makeLambda ''Cv (list 'ref parentName ''Pv))))
   (setq tail (list eval: (list compile: orphan tail true)))
   (if (isSymbol parentName)
       (setq result (list set: (list ref: (symbol parentName) ''Pv) (makeQuotedSymbol childName) tail)))
   (if (isPair parentName) 
       (setq result (list set: (list ref: parentName ''Pv) (makeQuotedSymbol childName) tail)))
   (setq result (list 'ref result (makeQuotedSymbol childName)))
   result)

