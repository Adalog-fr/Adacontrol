;;; ada-control.el --- Subversion interface for emacs
;; Copyright (C) 2007-2007 by Jérôme Haguet

;; Author: Jérôme Haguet, <j.haguet@cadwin.com>
;; $Revision: 4257 $

;; ada-control.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; ada-control.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; ada-control.el is tested with GNU Emacs 22.0.50.1.1 on windows

;; ada-control.el needs at least Ada Control 1.6

;; ada-control.el provides a specific mode to AdaControl rules files (aru)
;; (see http://www.adalog.fr/adacontrol1.htm)
;;

;; To use ada-control.el put the following line in your .emacs:
;; (require 'ada-control)

;; The latest version of psvn.el can be found at:
;;   ???

;; TODO:
;;   Add entries to Ada menu to run Adactl 

(require 'generic)

(defconst ada-control-rule-identifier-1.6
  (list "Abnormal_Function_Return" "Allocators" "Array_Declarations"
	"Barrier_Expressions"
	"Case_Statement" "Control_Characters" "Declarations"
	"Default_Parameter" "Directly_Accessed_Globals" "Entities"
	"Entity_Inside_Exception" "Exception_Propagation" "Expressions"
	"Global_References"
	"Header_Comments"
	"If_For_Case" "Instantiations" "Insufficient_Parameters"
	"Local_Hiding" "Local_Instantiation" 
	"Max_Blank_Lines" "Max_Call_Depth" "Max_Line_Length" "Max_Nesting" "Max_Parameters" "Max_Statement_Nesting" "Movable_Accept_Statements"
	"Naming_Convention" "No_Safe_Initialization" "Non_Static" "Not_Elaboration_Calls"
	"Other_Dependencies"
	"Parameter_Aliasing" "Potentially_Blocking_Operations" "Pragmas"
	"Reduceable_Scope" "Representation_Clauses" "Return_Type"
	"Side_Effect_Parameters" "Silent_Exceptions" "Simplifiable_Expressions" "Special_Comments" "Statements" "Style"
	"Terminating_Tasks"
	"Uncheckable" "Unnecessary_Use_Clause" "Unsafe_Paired_Calls" "Unsafe_Unchecked_Conversion" "Usage" "Use_Clauses"
	"With_Clauses"))

(define-generic-mode ada-control-rule-file-mode
  ;; comment-list
  '(?#) 
  ;; keyword-list
  (list "check" "search" "count")
  ;; font-lock-alist
  (eval-when-compile 
    (list
     (generic-make-keywords-list 
      ada-control-rule-identifier-1.6
      font-lock-function-name-face)
     ))
  ;; auto-mode-list  
  '("\\.aru\\'") 
  ;; function-list
  (list 
   (function
    (lambda () 
      (setq font-lock-keywords-case-fold-search t)
      ))
   )
  "AdaControl rule file mode"
  )
(provide 'ada-control)


