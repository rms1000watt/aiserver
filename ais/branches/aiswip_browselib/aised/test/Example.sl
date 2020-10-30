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
;; Example.sl
;; *************************************************************
;; Summary: Strategy Central main agent
;; Summary
;; *************************************************************
(defun strategyCentral(...)
pvars:(
	;; Variables
	myPv					;

	;; Speech Act Agents
	getData				; 
	

	;; Child Agents
	_updateTraderAgentGenomes	;; Read the expressions.txt file and produce entries in the SCIndex


) ;; end of persistent variables

faces:((isAMP true)) ;; The glue layer checks for isAMP true before calling an AMP agent

vars:(Msg SpeechAct)

;; ***************************************************
;; Define the private child agents 
;; NOTE: private child agent names must begin with an underscore
;; ***************************************************


;; ***************************************************
;; Define the speech-acts
;; ***************************************************
(defun getData(Msg) 
	(setq Msg[Data:] "Test Data")
)

;; ************************************************
;; Define the main entry code for this parent agent
;; ************************************************
;; Return true if no message argument was given
(if (= (argCount) 0) (return false)) ;; Invalid call

(setq Msg (argFetch 0)) ;; get the message into a local var

(if (not (isStructure Msg)) (return #{_error: "strategyCentral: Invalid message"}))

(if (not (isMember strategyCentral: Msg)) (return #{_error: "strategyCentral: Invalid message - no target agent"}))

;; Get the name of the speech act
(setq SpeechAct Msg[strategyCentral:]) ;; The speech act is the value of the first name value pair

;; Speech acts can not begin with an underscore as this is reserved for naming internal agents
;; Note that this means all child agents not beginning with an underscore are considered to be potential speech acts
(if (= (left SpeechAct 1) "_") (return #{_error: "strategyCentral: Invalid message - bad speech act"}))

;; Place a reference to the agents Pv structure in a handy location
(setq myPv strategyCentral[Pv:])

;; Make sure the speech act is in the agent's Pv structure
(setq SpeechAct (symbol SpeechAct))
(if (not (isMember SpeechAct myPv)) (return #{_error: "strategyCentral: Invalid message - bad speech act"}))

;; Make sure the speech act is indeed an agent
(if (not (isAgent myPv[SpeechAct])) (return #{_error: "strategyCentral: Invalid message - bad speech act"}))

;; Execute the speech act
(setq Msg (myPv[SpeechAct] Msg)) ;; Execute specified speech act passing Msg as only argument

Msg) ;; end StrategyCentral
