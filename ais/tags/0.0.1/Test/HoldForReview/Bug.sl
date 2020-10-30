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
(define diagnostic writeln)
(debug on:)
(diagnostic "Randomly string replacements")
(setq tststring (rept "ORIGINAL" maxoccur))
(setq strlen (* (count "ORIGINAL") maxoccur))

(loop for i from 1 until noruns do
	(setq start (ceiling (random strlen)))
	(setq length (ceiling (random strlen)))
	(diagnostic start)
	(diagnostic length)
	(setq repstring (rept "R" length))
	(setq tststring (replace tststring start length repstring))	; BUG !EvalVal!
	(setq newstrlen (count tststring))
	(diagnostic newstrlen)
	)

