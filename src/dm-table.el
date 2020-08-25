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

;; DEVEL hack: prefer version from CWD, if any
(let ((load-path (append (list ".") load-path)))
  (require 'dungeon-mode))

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

(defvar dm-table-property-name "ETL"
  "Document property specifying table handling.")

(defvar dm-table-type nil
  "When set, force tables to be processed as the given type.")

(defvar dm-table-property-tags nil
  "List of known tags.")

(defvar dm-table-property-last-tags nil
  "The tags of last property value read, if any.")

(defvar dm-table-property-tag-re
  "^\\([^ \t:]*\\)\\([:]\\([!]\\)?\\(.*?\\)\\)?$"
  ;;  1 base  2 tag lit 3 invrs ind 4 TAG
  "Regex used to parse tags in property values.

Capture groups:
  1. Basename, e.g. reference scope.
  2. Tag literal including seperator, empty when there is no tag
  3. Inversion/negation indicator, if any (e.g. \"!\").
  4. Tag, e.g. \"settings\" from \"character:settings\".")

(defvar dm-table-property-last-value nil
  "The last property value read, if any.")

(defun dm-table-known-state-p (state)
  "Return t when STATE has associated behavior."
  (or (assoc state dm-table-states)
      (let ((str (if (stringp state)))))))

(defun dm-table-scope (value)
  "Return inital scope (sequence) for VALUE, if any."
  (let ((sym (if (stringp value) (intern value) value))
	str)
    (or (and sym dm-table-states (assoc sym dm-table-states))
	(and (string-match
	      dm-table-property-tag-re
	      (setq str (org-string-nw-p
			 (if (stringp value) value
			   (prin1-to-string value t)))))
	     (when-let ((tag (org-string-nw-p (match-string 4 str))))
	       (assoc (intern tag) dm-table-states))))))

(defun dm-table-settings-scope (value)
  "Return settings scope for VALUE, if any."
  (let ((str (org-string-nw-p
	      (if (stringp value) value
		(prin1-to-string value t))))
	alt-scope)
    (and (string-match dm-table-property-tag-re str)
	 (org-string-nw-p (match-string 4 str))
	 (setq alt-scope (org-string-nw-p (match-string 1 str)))
	 (assoc (intern alt-scope) dm-table-states))))

(defun dm-table-property (&optional table prop)
  "Read PROP from TABLE using `org-entry-get'."
  (when-let ((type (org-string-nw-p
		    (org-entry-get
		     (dm-table-start-pos table)
		     (or prop dm-table-property-name)
		     t))))
    (intern type)))

(defun dm-table-property-old (&optional table prop)
  "Read PROP from TABLE using `org-entry-get'."
  (or dm-table-type
      (dm-table-property--lv-get)
      (when-let ((type (org-string-nw-p
			(org-entry-get
			 (dm-table-start-pos table)
			 (or prop dm-table-property-name)
			 t))))
	(dm-table-property--lv-set type)
	(dm-table-property--lv-get))))

(defun dm-table-property--lv-get ()
  "Return the last property tag name or value or nil."
  dm-table-property-last-value)

(defun dm-table-property--lv-get-old ()
  "Return the last property tag name or value or nil."
  (or (and dm-table-property-last-tags
	   (nth (1- (length dm-table-property-last-tags))
		dm-table-property-last-tags))
      dm-table-property-last-value))

(defun dm-table-property--lv-set (name)
  "Set the table name from NAME, a string, list, or symbol."
  (setq dm-table-property-last-value
	(if (not (stringp name)) name
	  (intern name))))

(defun dm-table-property--lv-set-old (name)
  "Set the table name from NAME, a string, list, or symbol."
  (setq dm-table-property-last-value
	(if (not (stringp name)) name
	  (if (not (string-match dm-table-property-tag-re name))
	      (intern name)
	    (let ((base (intern (match-string 1 name)))
		  ;;(neg (org-string-nw-p (match-string 3 name)))
		  (tag (intern (match-string 4 name))))
	      (dm-table-property-add-tags tag)
	      base)))))

(defun dm-table-property-clear (&optional seen-p)
  "Clear values assocated to last property.

When SEEN-P is given a truthy value also clear tag cache."
  (when seen-p (setq dm-table-property-tags nil))
  (setq dm-table-property-last-tags nil)
  (setq dm-table-property-last-value nil))

(defun dm-table-start-pos (&optional table)
  "Find start pos of TABLE or at point in current buffer."
  (if table
      (save-excursion (org-element-property :begin table))
    (org-table-begin)))

(defun dm-table-property-has-tag (&optional tag seen-p)
  "Return t when TAG is/was available.

When TAG is nill return t if any tags are available.
When SEEN-P is provided with a truthy value, check
`dm-table-property-tags' (e.g. globally) instead of
'dm-table-property-last-tags (e.g. last/current run)."
  (if (not seen-p) dm-table-property-tags
    (member tag dm-table-property-last-tags)))

(defun dm-table-property-add-tags (&rest tags)
  "Add TAGS to the List of those \"seen\"."
  (mapc (lambda (tag)
	  (unless (dm-table-property-has-tag tag t)
	    (push tag dm-table-property-tags))
	  (setq dm-table-property-last-tags
		(append (delete tag dm-table-property-last-tags)
			(list tag))))
	tags))



(defun dm-table--dispatch-table-impl (&optional table)
  "Start a batch to process TABLE."
  (save-excursion
    (goto-char (dm-table-start-pos table))
    (forward-line 1)
    (if dm-table-type (dm-table-batch dm-table-type)
      (when-let* ((type (dm-table-property table)))
	(when (dm-table-scope type)
	    ;;(assoc type dm-table-states 'equal)
	  (if (and (boundp 'dm-settings-current-scope)
		   ;;(not (string= "settings" (symbol-name type)))
		   (dm-table-property-has-tag))
	      (let ((dm-settings-current-scope dm-table-property-last-value))
		(message "creating lexical scope for settings %s"
			 dm-table-property-last-value)
		(dm-table-batch type))
	    (message "no tags for property %s (%s)"
		     dm-table-property-last-value
		     dm-table-property-last-tags)
	    (dm-table-batch type)))))))

;; TODO: make interactive
(defun dm-table-dispatch-table ()
  "Start a batch to process table at point."
  (when (org-at-table-p)
    (dm-table--dispatch-table-impl)))

(defun dm-table-extract-current-buffer ()
  "Process tables within the current buffer."
  (interactive)
  (org-element-map (org-element-parse-buffer) 'table
    #'dm-table--dispatch-table-impl))

(defun dm-table-extract-files (&rest files)
  "Process tables within FILES."
  (mapc
   (lambda (this-file)
     (dm-msg :file "dm-table" :fun "extract-files" :args (list :file this-file))
     (with-temp-buffer
       (insert-file-contents this-file)
       (dm-table-extract-current-buffer)))
   files)
  nil)

(defun dm-table-extract-row (&optional inhibit-error)
  "Load the \"logical\" table row at point.

When INHIBIT-ERROR is non-nil suppress error when point is not at
an org table.  A logical row is the span of contigious rows in
the group including the current and any additional rows from
above the current as/if needed until an identifier is found."
  (interactive)
  (if (not (org-at-table-p))
      (unless inhibit-error (user-error "Not at a table"))
    (org-table-analyze)
    (let ((start-line (org-table-current-line))
	  (type (dm-table-property))
	  cur-line end-line row-id)
      (save-excursion
	(setq end-line (setq cur-line start-line))
	(org-table-goto-column 0)
	(while (and (not (setq row-id (org-string-nw-p (org-table-get-field))))
		    (< 0 (cl-decf cur-line)))
	  (org-table-goto-line cur-line))
	(if (not row-id)
	    (unless inhibit-error (user-error "Can't find row identifier"))
	  (while (and (< (point) (org-table-end))
		      (not (equal row-id (dm-map--string-nopoe
					  (org-table-get-field)))))
	    (forward-line)
	    (cl-incf end-line)) ;; (org-table-goto-line)
	  (setq row-id (intern (org-trim row-id)))
	  (let* ((row-idx-list
		  (append (list 0) (number-sequence cur-line end-line)))
		 (rows
		  (mapcar
		   (lambda (idx)
		     (org-table-goto-line idx)
		     (mapcar
		      (lambda (col-idx)
			(dm-map--string-nopoe
			 (org-table-get-field col-idx)))
		      (number-sequence 1 org-table-current-ncol)))
		   row-idx-list)))
	    (message "Loaded \"%s\" (%s..%s)" row-id cur-line start-line)
	    (if dm-table-type
		(dm-table-batch dm-table-type)
	      (when type
		(when (assoc type dm-table-states 'equal)
		  (apply 'dm-table-batch (list type :transform rows)))))))))))

(defun dm-table-extract-table
    (&rest _args)
  "Return :transform form passing table at point as elisp.

Parsing and converison via `org-table-to-lisp', which see."
  ;; (when-let ((coalesce-prop (org-entry-get (point) "coalesce" t)))
  ;;   (setq dm-table-coalesce-args (read coalesce-prop)))
  (dm-msg :fmt "[dm-table-extract-table] $args around..\n$context"
	  :args (list :file (buffer-file-name)
		      :buffer (buffer-name)
		      :line (line-number-at-pos)
		      :pos (point))
	  :context (concat (buffer-substring (if (< 150 (point)) (- (point) 150)
					       (point-min)) (point))
			    "⧆"
			    (buffer-substring  (point) (+ (point) 10))))
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
  ;;(message "[load] fun:%s var:%s" dm-table-load-table-fun dm-table-load-table-var)
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
  (when (and scope dm-table-states)
    (when-let* ((scope-states (cdr-safe (assoc scope dm-table-states 'equal)))
		(next-step (or step (car-safe scope-states)))
		(step-func (plist-get scope-states next-step)))
      ;;(message "[step] fun:%s next:%s states:%s" step-func next-step scope-states)
      (if (functionp step-func) step-func
	(eval step-func)))))

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
  (let ((scope (car (dm-table-scope scope)))
	(settings-scope (car (dm-table-settings-scope scope))))
    (when scope
      (let ((step-func (funcall dm-table-step-function scope step data))
	    (result (cons t data)))
	(if (and settings-scope (boundp 'dm-settings-current-scope))
	    (let ((dm-settings-current-scope settings-scope))
	      (while (and (car-safe result) step-func)
		(message "[batch] scope:%s step%s settings:%s"
			 scope (car-safe result) dm-settings-current-scope)
		(setq result (apply step-func (cdr result)))
		;;(message "[batch] result:%s" result)
		(when (car-safe result)
		  (setq step-func (funcall dm-table-step-function scope (car result)))
		  ;;(message "[batch] newfunc:%s" step-func)
		  step-func)))
	  (while (and (car-safe result) step-func)
	    (message "[batch] scope:%s step%s settings:%s" scope (car-safe result)
		     (if (boundp 'dm-settings-current-scope)
			 dm-settings-current-scope
		       'undefined))
	    (setq result (apply step-func (cdr result)))
	    ;;(message "[batch] result:%s" result)
	    (when (car-safe result)
	      (setq step-func (funcall dm-table-step-function scope (car result)))
	      ;;(message "[batch] newfunc:%s" step-func)
	      step-func))))))
  ;;(message "var:%s eval:%s" dm-table-load-table-var (eval dm-table-load-table-var))
  dm-table-load-table-var)
;; ^-- redo as when-let?

(provide 'dm-table)
;;; dm-table.el ends here
