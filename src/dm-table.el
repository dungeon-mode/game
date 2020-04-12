;;; dm-table.el --- define and access dungeon-mode tables  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;  A very simple ETL tool for org.

;; We define a batch process in terms of a number of steps each
;; represented by a function returning the next step, if any.

;;  Support "tables" (or maybe another org elemnt type bound to
;;  dm-table-element) in terms of a set functions to:
;;   - Extract e.g. read from the source document
;;   - Transform, e.g. create a result or side effect from source
;;   - Load e.g. write the sorce, back to the document or elsewhere

;;  As any implicit cooperation amung these methods is left to their
;;  implemention, this module simple provides an API for driving the
;;  implicit state machine via interactive commands, functions, and
;;  hooks.  Each game intrinsic implements this API to allow
;;  intersection amung game sources as we work with with them.

;;; Code:

(require 'org-table)
(eval-when-compile (require 'cl-lib)
		   (require 'subr-x))

(defvar dm-table-alist nil
  "While editing or playing, an alist of source scope and state.")

(defvar dm-table-load-table-var 'dm-table-alist
  "Variable updated by the default load function `dm-table-load-table'.")

(defvar dm-table-load-table-fun (lambda (item list) (push item list))
  "Function to update `dm-table-load-table-var'.")

(defvar dm-table-states
  '((ledger :extract #'dm-table-extract-function
	    :transform #'dm-table-transform-function
	    :load #'dm-table-load-function))
  "While editing or playing, available source state transitions.

This is an irregular alist with items taking the form:

  (SCOPE :STATE1 FUN1 ...)

Where SCOPE is a symbol representing the intrinsic (e.g. source
category, or frame of reference), and each :STATE FUN pair
represent a supported set pairing one of :extract :transform or
:load with a function, called with the parsed element (or with
point at the start of the unparsed element, in the case of
:extract with no prior state) followed by any prior state
information for the current workflow.  FUN may return NEW-STATE
to continue the workflow or nil to terminate it.

Prior state as well as NEW-STATE may either be a keyword symbol
or a con cell in the form:
  (SCOPE . STATE)")

(defvar dm-table-extract-function #'dm-table-extract-table
  "Function used to read from org into to memory.

The default implemention deligates to `org-table-to-lisp'.")

(defvar dm-table-transform-function #'dm-table-tranform-table
  "Function used to lexically transform org sources.

The default implemention relies on `dm-coalesce-hash'.")

(defvar dm-table-load-function #'dm-table-load-table
  "Function used to store a table, e.g. in a var..")

(defvar dm-table-step-function #'dm-table-step
  "Function to deduce the (next) step for `dm-table-batch'.")

(defvar dm-table-coalesce-args nil "Options for dm-coalesce-hash.")

(defun dm-table-extract-table
    (&rest _args)
  "Return :transform form passing table at point as elisp.

Parsing and converison via `org-table-to-lisp', which see."
  ;; (when-let ((coalesce-prop (org-entry-get (point) "coalesce" t)))
  ;;   (setq dm-table-coalesce-args (read coalesce-prop)))
  (message "[xract] file:%s buffer:%s line:%s pos:%s around:%s"
	   (buffer-file-name) (buffer-name)
	   (line-number-at-pos) (point)
	   (concat (buffer-substring (- (point) 50) (point))
		   "⧆"
		   (buffer-substring  (point) (+ (point) 50))))
  (list :transform (delq 'hline (org-table-to-lisp))))

(defun dm-table-tranform-table (table)
  "Return :load form with TABLE as an hash-table.

This version creates an hashmap of properties from each row from
header labels in the first table row, take the first column as key."
  (eval `(list :load (dm-coalesce-hash ,(cdr table)
			 ,@(or dm-table-coalesce-args
			       (seq-map (lambda (label) (intern (downcase label)))
					(car table)))
		       ))))

;; (defun dm-table-tranform-table-before-props (table)
;;   "Return :load form with TABLE as an hash-table.
;; This version creates an hashmap of properties from each row from
;; header labels in the first table row, take the first column as key."
;;   (let ((cols (or dm-table-slots
;; 		  (seq-map (lambda (label) (intern (downcase label)))
;; 			   (car table)))))
;;     (eval `(list :load (dm-coalesce-hash ,(cdr table)
;; 			   ,cols
;; 			 ,@(when dm-table-key
;; 			     `(:key-symbol ,dm-table-key)))))))

;;  (setq x (cons x y))
;;    (list 'features (nth 2 row))

(defun dm-table-load-table (table)
  "Apply `dm-table-load-table-fun' to `dm-table-load-table-var' and TABLE.

Return a terminating form."
  (message "[load] fun:%s var:%s" dm-table-load-table-fun dm-table-load-table-var)
  (list nil (set dm-table-load-table-var table)))

;; (dm-table-load-table (cdr (apply 'dm-table-tranform-table (cdr (dm-table-extract-table 'map)))) '(map))

(cl-defun dm-table-defstates (type
			      &optional &key
			      (extract dm-table-extract-function)
			      (transform dm-table-transform-function)
			      (load dm-table-load-function))
  "Define EXTRACT TRANSFORM and LOAD functions for TYPE, a symbol."
  (if (assoc type dm-table-states)
      (setcdr (assoc type dm-table-states)
	      (list :extract extract :transform transform :load load))
    (push (list type :extract extract :transform transform :load load)
	  dm-table-states)))

(defun dm-table-step (scope &optional step &rest _data)
  "Find the best next function to advance SCOPE.

SCOPE is a symbol.  STEP is a keyword symbol amung :extract
:transform or :load, defaulting to first given for SCOPE.."
  ;; (message "[step] scope:%s step:%s scope-states:%s next-step:%s step-func:%s"
  ;; 	   scope step (cdr-safe (assq scope dm-table-states))
  ;; 	   (or step (car-safe (cdr-safe (assq scope dm-table-states))))
  ;; 	   (car-safe (cdr-safe (plist-get (cdr-safe (assq scope dm-table-states))
  ;; 					  (or step (car-safe (cdr-safe (assq scope dm-table-states))))))))
  (message "[step] scope:%s step:%s" scope step)
  (when-let* ((scope-states (cdr-safe (assoc scope dm-table-states 'equal)))
	      (next-step (or step (car-safe scope-states)))
	      (step-func (plist-get scope-states next-step)))
    (message "[step] fun:%s next:%s states:%s" step-func next-step scope-states)
    (if (functionp step-func) step-func
      (eval step-func))))

(defun dm-table-batch(scope &optional step &rest data)
  "Execute functions in batch until unable to advance SCOPE.

SCOPE is a symbol.  STEP is a keyword symbol, nominally amung
:extract :transform or :load, defaulting to the first for SCOPE
among those given in `dm-table-states'.  Any DATA is passed to
the first step function (generally \":extract\").

Functions may return nil to halt the batch or a cons cell to
continue:

  (NEXT-STEP RESULT)

Where NEXT-STEP is a keyword symbol representing the next
step (or nil, to stop), and LAST-RESULT is the current result-set
being carried forward, if any."
  (let ((step-func (funcall dm-table-step-function scope step data))
	(result (cons t data)))
    (while (and (car-safe result) step-func)
      (message "[batch] before func:%s data:%s" step-func data)
      (setq result (apply step-func (cdr result)))
      ;;(message "[batch] result:%s" result)
      (when (car-safe result)
	(setq step-func (funcall dm-table-step-function scope (car result)))
	(message "[batch] newfunc:%s" step-func)
	step-func)))
  ;;(message "var:%s eval:%s" dm-table-load-table-var (eval dm-table-load-table-var))
  dm-table-load-table-var)
;; ^-- redo as when-let?

(provide 'dm-table)
;;; dm-table.el ends here
