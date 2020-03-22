;;; ox-ox.el --- define an org exporter using property and calc syntax  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Keywords: outlines, tools

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

;; define an org exporter using property and calc syntax

;; 1. by default, maybe a #+ style keyword and property combination
;;    for each document classifiction
;; 2. bind some default expressions
;; 2.1
;; 2.2 sparce, pick just a few such as section, block and tables to
;;     bind by default.
;; 2.3 simple, produce well formatted easy to understand elisp

;; 2.4 direct, require explicit customization/configuration before
;; emitting additional information beyond that provided by ox.el,
;; et al, for example include

;;; Code:
(require 'org-table)
(require 'seq)

(defgroup export-export-◻ nil
  "Customize ox-◻.

Add properties to sections, tables, and blocks to \"hand-hold\"
further processing by other exporters or create lisp data and
programs from org documents and, optionally, execute them.

This package creates an org exporter on the fly incorperating
expressions to declaritively create options, transcoders,
filters, and menu entries using syntax similar to table TBLFM and
org's support for calc.

Similar to `calc' support as from `org-table', ox-◻ will
interpolate variables such as absolute and border position
relitive or \"remote\" table references, extended to address
sections and blocks and to provide shorthand for fetching
document and entity properties as provided by `ox'.

Quoted expressions used handled are differently from TBLFM.
Implicitly, expressions handled by ox-◻ are intended to be read
by emacs (and then executed to create code exporting org
documents).  Thus, notwitstanding interpolation as described
above, `quote' works per normal for elisp code.

Use slash \"\\\" as a pre-fix to avoid interpolation
e.g. \"\\$1, mam\", \"\@2. Gesundheit.\"."
  :group 'org-export)

(defcustom ox-◻-transcoder-alist nil
  "Map each element per `org-export' to export settings.

  (ELEMENT . SETTINGS)

Where SETTINGS may be nil (or an ELEMENT omitted) or otherwise
generally takes the from of arguments to `org-export-create-backend'."
  :type 'alist)

;;(defvar ox-◻-regexp-defaults)

;; org-table-fedit-finish
;; "^\\(@[-+I<>0-9.$@]+\\|@?[0-9]+\\|\\$\\([a-zA-Z0-9]+\\|[<>]+\\)\\) *= *\\(.*\\(\n[ \t]+.*$\\)*\\)"
;; org-table-fomula-make-cmp-string
;; "^\\(@\\([0-9]+\\)\\)?\\(\\$?\\([0-9]+\\)\\)?\\(\\$?[a-zA-Z0-9]+\\)?"
;; org-table-goto-field
;; "\\`@\\([1-9][0-9]*\\)\\$\\([1-9][0-9]*\\)\\'"

(defmacro ox-◻-rx-add (key regexp)
  "Add REGEXP for KEY in `ox-◻-regexp-alist'."
  `(add-to-list ox-◻-regexp-alist
		(cons ,key (rx ,regexp))))

(defsubst ox-◻-re (key &optional inhibit-eval)
  "Retreive regex for KEY in `ox-◻-regexp-alist'.

Unless optional INHIBIT-EVAL is truthy, evaluate regex
in the context of `rx' before returning it."
  (if inhibit-eval
      (assoc key ox-◻-regexp-alist)
    (rx (assoc key ox-◻-regexp-alist))))

;; taken from org-table.el:org-table-range-regexp
(defconst ox-◻-table-row-regexp
  "\\(@\\([-+]?\\)\\(I*\\)\\([-+]?\\)\\([0-9]*\\)\\)"
  ;;  1   2          3       4          5
  "Regular expression for matchinga \"row\" transcoding macro.")

(defconst ox-◻-table-col-regexp
  "\\(\\$\\([-+]?\\)\\(I*\\)\\([0-9]+\\)\\)"
  ;;    1   2          3       4
  "Regular expression for matching a \"column\" transcoding macro.")

(defconst ox-◻-table-cell-regexp
  (concat "\\(" ox-◻-table-row-regexp ox-◻-table-col-regexp
	  "?\\|" ox-◻-table-col-regexp "\\)")
  "Regular expression for matching a cell transcoding macro.")

(defcustom ox-◻-trancoder-model-alist
  `(table
    (:regexp ((rx (or ,org-table-range-regexp
		      ,ox-◻-table-cell-regexp))
	      . #'ox-◻--defx-table)))
  "Keys are elements as symbols, the values plists.

Plist keys are amung the following:
  :EXPR a default expression template
  :VARS a list of cons cells in the form
      (REGEX FUNC-OR-EXPR)

Expression template is a standard Emacs Lisp expression
optionally containing interpolation macros (e.g. @1$1, $*, etc.).

REGEX is a regular expression as a string capturing any
interpolationmacros supported by this model, and FUNC-OR-EXPR is
a function or expression to provide that implemention.

The complete match from REGEX is replaced with the result of
evaluating FUNC-OR-EXPR given it does not return nil, in which
event no replacement is made."
  :type 'alist)

(defcustom ox-◻-inline-options-default nil
  "Default value for `ox-◻-inline-options'.

May be nil to allow only confiration per
ox-◻-element-transcoder-alist, t to expose all possible
transformer and filter options, or a list of allowed options."
  :type '(or nil t (list symbol)))

(defcustom ox-◻-refs-keep-info nil
  "If t, ox-ref cookies are not removed from INFO.

This exposs them to transcoders via the \"$:\" INFO interpolation
variable.  This could be used to export tables as alists of cells:

  #+OX_TABLE: `($_:name ,(append $*))
  #+OX_ROW: (nconc $*)
  #+OX_CELL: '(($::ox-ref $*))"
  :type '(or nil t (list symbol)))

(defcustom ox-◻-element-drop-parent nil
  "If t, remove \":parent\" from ELEMENT exposed to transformers.

This can produce much smaller exporter (intermediary) sources,
for example when using \"$_\" unqualified to export the parsed
element source, e.g. perhaps with:

  #+OX_ALL: $_")

(defcustom ox-◻-refs-add-to-element nil
  "If nil, do not modify elements while creating transcoders.

Otherwise, if t, add \":ox-ref\" to each element.  Otherwise this
is a list of elements to modify as symbols."
  :type '(or nil t (list symbol)))

(defvar ox-◻-var-alist '(("$*" . transcoder-element)
			  ("$_" . transcoder-content)
			  ("$:" . transcoder-info))
  "List of \"variable\" interpolation macros.

This includes only those available to all transcoders. The `car'
is a substitution marker string and `cdr' is a functions or macro
to be evaluated in the context of `ox-◻-deftrancoder', which see.")

(defvar-local ox-◻-options nil
  "The list of options enabled by/during export.

Inital values are supplied by `ox-◻-transcoder-alist'
however, individual documents may add or replace values,
depending on the value of `ox-◻-inline-options'.")

(defvar-local ox-◻-inline-options ox-◻-inline-options-default
  "The list options currently enabled while exporting.

Inital value may be customized via `ox-◻-inline-options-default'.
`ox-◻-inline-options-default'.")

(defvar-local ox-◻-refs nil
  "While exporting, hash-table of interpolation references.

Each key is a REF, a string locating ELEMENT in abolute terms.

Entries are lists in the form (REF ELEMENT CONTENT INFO).  The
same list may be identified by multiple hash-table keys.  Not all
inputs are provided for all elements/to all transformers.  INFO
is used as a communiation channel during export and may change
during the export, e.g. by processing other than this module.")

(defconst ox-◻-rx-alist
  '((var-sigel ?$)
    (col-sigel ?$)
    (row-sigel ?@)
    (key-sigel ?:)
    (comma (and (0+ blank) ?, (0+ blank)))
    (sign (group (any ?+ ?-)))
    (boundry (group (1+ (or ?< ?>))))
    (numbered (group (1+ digit)))
    (named (group (and (any "a-z" "A-Z" "_")
		       (0+ (any alnum ?_)))))
    (key (and key-sigel named))
    (special (or (and var-sigel (group (any ?* ?_ ?: ?#)) (opt key))
		 (and ?@ (group ?#))))
    (remote (item) (and "remote(" (0+ blank) (group (1+ (not ?,)))
			comma item ")"))
    (col (or named boundry (and (opt sign) numbered)))
    (row (or boundry
	     (and (opt sign) (group (1+ ?I))
		  (opt (and sign numbered)))
	     (and (opt sign) numbered)))
    (cell (or (and row-sigel row (opt (and col-sigel col)))
	      (and col-sigel col)))
    (range (and cell ".." (or cell row)))
    (range? (and cell (opt (and ".." (or cell row)))))
    (var (or special range? (remote range))))
  "Regular experssions fragments for `rx-let'.")

(defconst ox-◻-re (eval `(rx-let ,my:rx (rx var)))
  "Regular expression used to parse interpolation macros.")

(defconst ox-◻-re-labels
  '(full-match ; 0
    special-name ; 1
    special-keyword ; 2
    row-count ; 3
    row-boundry ; 4
    row-hline-sign ; 5
    row-hline ; 6
    row-hline-adj-sign ; 7
    row-hline-adj ; 8
    row-sign ; 9
    row-number ; 10
    field-name ; 11
    field-boundry ; 12
    field-sign ; 13
    field-number ; 14
    col-name ; 15
    col-boundry ; 16
    col-sign ; 17
    col-number ; 18
    right-row-boundry ; 19
    right-row-hline-sign ; 20
    right-row-hline ; 21
    right-row-hline-adj-sign ; 22
    right-row-hline-adj ; 23
    right-row-sign ; 24
    right-row-number ; 25
    right-field-name ; 26
    right-field-boundry ; 27
    right-field-sign ; 28
    right-field-number ; 29
    right-col-name ; 30
    right-col-boundry ; 31
    right-col-sign ; 32
    right-col-number ; 33
    ;; skip 34-40, remote doesn't handle:
    ;; | @#, 1 grp | special, 2 grp | col-*, 4 grp | = 6 |
    nil nil nil nil nil nil nil
    remote-name ; 41
    remote-row-boundry ; 42
    remote-row-hline-sign ; 43
    remote-row-hline ; 44
    remote-row-hline-adj-sign ; 45
    remote-row-hline-adj ; 46
    remote-row-sign ; 47
    remote-row-number ; 48
    remote-field-name ; 49
    remote-field-boundry ; 50
    remote-field-sign ; 51
    remote-field-number ; 52
    remote-col-name ; 53
    remote-col-boundry ; 54
    remote-col-sign ; 55
    remote-col-number ; 56
    remote-right-row-boundry ; 57
    remote-right-row-hline-sign ; 58
    remote-right-row-hline ; 59
    remote-right-row-hline-adj-sign ; 60
    remote-right-row-hline-adj ; 61
    remote-right-row-sign ; 62
    remote-right-row-number ; 63
    remote-right-field-name ; 64
    remote-right-field-boundry ; 65
    remote-right-field-sign ; 66
    remote-right-field-number ; 67
    remote-right-col-name ; 68
    remote-right-col-boundry ; 69
    remote-right-col-sign ; 70
    remote-right-col-number ; 71
    )
  "List of symbols valid groups in `ox-◻-re'.")

(defun ox-◻--defx-section (element model expr)
  "Generate implementation for a section like ELEMENT.

MODEL and EXPR are as for `ox-◻-deftranscoder'.")

(defun ox-◻--defx-block (element model expr)
  "Generate implementation for a block like ELEMENT.

MODEL and EXPR are as for `ox-◻-deftranscoder'.")

(defun ox-◻--defx-wrapping (element model expr)
  "Generate implementation for a \"wrapping\" ELEMENT.

Most elements are handled here. This includes paragraph text as
well as formatting elements such as italics, centering, etc.

MODEL and EXPR are as for `ox-◻-deftranscoder'.")

(defun ox-◻--defx-table (element model expr)
  "Generate implementation for table ELEMENT.

MODEL and EXPR are as for `ox-◻-deftranscoder'."
  )

(defun ox-◻-deftranscoder (element model expr)
  "Create trancoder for ELEMENT.

ELEMENT is an `org-mode' element kind, as a symbol.  MODEL is a
macro or function accepting EXPR and yielding a generated
transcoder, a function or lambda which called with element, info
and transcoder to perform any required interpolation into EXPR.

MODEL has access to some lexically bound variables during
macro-expansion:

  \"transcoder-element-type\", given as ELEMENT to this macro
  \"transcoder-element\", the document element being handled, if any
  \"transcoder-content\", the content of \"element\", if any, and
  \"transcoder-info\", the communications channel."
  (let* ((element-var-re (or (assoc :regexp model)
			     (error "Model lacks :regexp" model)))
	 (var-re (rx (group (or (group element-var-re)
				(group ox-◻-var-alist)))))
	 (result-expr (or expr (assoc :regexp model)))
	 (start 0)
	 error-data)
    (save-match-data
      (condition-case error-data
	  (while (string-match var-re result-expr start)
	    (pcase (match-data)
	      (`(,var
		 (let ((newtext "OK"))
		   (setq result-expr (replace-match nil t result-expr))))))
	    )
	(error (user-error "Unknown interpolation syntax"
			   (list (match-string 0 result-expr)
				 element expr)
			   ))))))

(provide 'ox-ox)
;;; ox-ox.el ends here
