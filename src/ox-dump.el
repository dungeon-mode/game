;;; ox-dump.el --- export documents as elisp data structures  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Keywords: docs, lisp

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

;; Export `org-mode' documents as elisp data structures.


;; 1. custom property: "dump"
;;(defcustom ox-dump-)

;; 1.1. option: "expr", an expression describing what to dump, how to
;;      store it, etc.  This is essencially a standard Emacs Lisp
;;      expression ("sexp") except that certain `org-mode' constructs
;;      such as table-references are expanded into equilivant elisp
;;      expressions before evaluation.

;;; Code:

(require 'ox)
(require 'ox-publish)
(require 'cl-lib)

;; user customizable settings
(defgroup org-export-dump nil
  "Options for exporting Org mode files as elisp data."
  :group 'org-export)

(defvar-local ox-dump-last-result nil
  "Default var holding the value last exported last by `ox-dump'.")

(defcustom ox-dump-default-var 'ox-dump-last-result
  "Symbol naming the var which should hold values exported by `ox-dump'."
  :type 'sexp)
;;(setq ox-dump-default-var 'ox-dump-last-result)

(defcustom ox-dump-default-binder #'setq-local
  "Symbol naming the function `ox-dump' uses by default to bind values."
  :type 'sexp)

(defcustom org-dump-expr `(setq ,ox-dump-default-var *)
  "Default expression for `ox-dump'."
  :group 'org-export-dump
  :type 'sexp)
;;(setq org-dump-expr `(setq ,ox-dump-default-var *))
;;(setq org-dump-expr "*")

(defcustom ox-dump-indent-after-insert nil
  "When t, indent after inserting to a buffer."
  :type 'boolean)

(defvar org-babel-default-header-args:dump
  `((:expr . ,org-dump-expr))
  "Default arguments to use when evaluating a LaTeX source block.")
;;(setq org-babel-default-header-args:dump `((:expr . ,org-dump-expr)))

;; proudly cargo culted from: https://code.orgmode.org/bzg/worg/src/\
;;  org-export-backend-tutorial/org-tutorials/org-export-backend.org
(org-export-define-backend 'dump
  '(
    (babel-call . (lambda (&rest args) (ox-dump--debug-transcoder 'babel-call "not implemented")))
    (body . (lambda (&rest args) (ox-dump--debug-transcoder 'body "not implemented")))
    (bold . (lambda (&rest args) (ox-dump--debug-transcoder 'bold "not implemented")))
    (center-block . (lambda (&rest args) (ox-dump--debug-transcoder 'center-block "not implemented")))
    (clock . (lambda (&rest args) (ox-dump--debug-transcoder 'clock "not implemented")))
    (code . (lambda (&rest args) (ox-dump--debug-transcoder 'code "not implemented")))
    (diary-sexpexample-block
     . (lambda (&rest args) (ox-dump--debug-transcoder 'diary-sexpexample-block "not implemented")))
    (drawer . (lambda (&rest args) (ox-dump--debug-transcoder 'drawer "not implemented")))
    (dynamic-block . (lambda (&rest args) (ox-dump--debug-transcoder 'dynamic-block "not implemented")))
    (entity . (lambda (&rest args) (ox-dump--debug-transcoder 'entity "not implemented")))
    (example-block . (lambda (&rest args) (ox-dump--debug-transcoder 'example-block "not implemented")))
    (export-block . (lambda (&rest args) (ox-dump--debug-transcoder 'export-block "not implemented")))
    (export-snippet . (lambda (&rest args) (ox-dump--debug-transcoder 'export-snippet "not implemented")))
    (final-output . (lambda (&rest args) (ox-dump--debug-transcoder 'final-output "not implemented")))
    (fixed-width . (lambda (&rest args) (ox-dump--debug-transcoder 'fixed-width "not implemented")))
    (footnote-definition . (lambda (&rest args) (ox-dump--debug-transcoder 'footnote-definition "not implemented")))
    (footnote-reference . (lambda (&rest args) (ox-dump--debug-transcoder 'footnote-reference "not implemented")))

    (headline . ox-dump-headline)

    (horizontal-rule . (lambda (&rest args) (ox-dump--debug-transcoder 'horizontal-rule "not implemented")))
    (inline-babel-call . (lambda (&rest args) (ox-dump--debug-transcoder 'inline-babel-call "not implemented")))
    (inline-src-block . (lambda (&rest args) (ox-dump--debug-transcoder 'inline-src-block "not implemented")))
    (inlinetask . (lambda (&rest args) (ox-dump--debug-transcoder 'inlinetask "not implemented")))
    (italic . (lambda (&rest args) (ox-dump--debug-transcoder 'italic "not implemented")))
    (item . (lambda (&rest args) (ox-dump--debug-transcoder 'item "not implemented")))

    (keyword . (lambda (&rest args) ""))

    (latex-environment . (lambda (&rest args) (ox-dump--debug-transcoder 'latex-environment "not implemented")))
    (latex-fragment . (lambda (&rest args) (ox-dump--debug-transcoder 'latex-fragment "not implemented")))
    (line-break . (lambda (&rest args) (ox-dump--debug-transcoder 'line-break "not implemented")))
    (link . (lambda (&rest args) (ox-dump--debug-transcoder link "not implemented")))
    (node-property . (lambda (&rest args) (ox-dump--debug-transcoder 'node-property "not implemented")))
    (options . (lambda (&rest args) (ox-dump--debug-transcoder 'options "not implemented")))

    (paragraph . ox-dump-paragraph)

    (parse-tree . (lambda (&rest args) (ox-dump--debug-transcoder 'parse-tree "not implemented")))
    (plain-list . (lambda (&rest args) (ox-dump--debug-transcoder 'plain-list "not implemented")))

    (plain-text . ox-dump-plain-text)

    (planning . (lambda (&rest args) (ox-dump--debug-transcoder 'planning "not implemented")))
    (property-drawer . (lambda (&rest args) (ox-dump--debug-transcoder 'property-drawer "not implemented")))
    (quote-block . (lambda (&rest args) (ox-dump--debug-transcoder 'quote-block "not implemented")))
    (radio-target . (lambda (&rest args) (ox-dump--debug-transcoder 'radio-target "not implemented")))

    (section . ox-dump-section)

    (special-block . (lambda (&rest args) (ox-dump--debug-transcoder 'special-block "not implemented")))
    (src-block . (lambda (&rest args) (ox-dump--debug-transcoder 'src-block "not implemented")))
    (statistics-cookie . (lambda (&rest args) (ox-dump--debug-transcoder 'statistics-cookie "not implemented")))
    (strike-through . (lambda (&rest args) (ox-dump--debug-transcoder 'strike-through "not implemented")))
    (subscript . (lambda (&rest args) (ox-dump--debug-transcoder 'subscript "not implemented")))
    (superscript . (lambda (&rest args) (ox-dump--debug-transcoder 'superscript "not implemented")))

    (table . ox-dump-table)
    (table-cell . ox-dump-table-cell)
    (table-row . ox-dump-table-row)

    (target . (lambda (&rest args) (ox-dump--debug-transcoder 'target "not implemented")))
    (timestamp . (lambda (&rest args) (ox-dump--debug-transcoder 'timestamp "not implemented")))
    (underline . (lambda (&rest args) (ox-dump--debug-transcoder 'underline "not implemented")))
    (verbatim . (lambda (&rest args) (ox-dump--debug-transcoder 'verbatim "not implemented")))
    (verse-block . (lambda (&rest args) (ox-dump--debug-transcoder 'verse-block "not implemented")))
    ;;     ;; where i started before having above mentioned tutorial
    ;;     ;; (headline . ox-dump-headline)
    ;;     ;; (plain-text . ox-dump-text)
    ;;     ;; (table . ox-dump-table)
    ;;     ;; (table-cell . ox-dump-table-cell)
    ;;     ;; (table-row . ox-dump-table-row)
    )
    :menu-entry
    '(?d "Export to Emacs Lisp"
	 ((?D "As buffer" ox-dump-export-as-dump)
	  (?d "As file" ox-dump-export-to-dump)
	  (?x "Evaluate (silent)" ox-dump-export-and-eval)
	  (?X "As buffer and evaluate" ox-dump-export-as-dump-and-eval)))
    :options-alist
    '((:dump-expr "DUMP_EXPR" nil org-dump-expr t)))

(defun ox-dump--debug-transcoder (transcoder result)
  "Display a message with RESULT from TRANSCODER."
  (message "--%s-->%s<--%s--" transcoder result transcoder))

(defun ox-dump-rtrim (contents)
  "Return CONTENTS with trailing whitespace removed."
  (replace-regexp-in-string "[\s\r\n\t\v]+$" "" contents))
;;(ox-dump-rtrim "foo\n")

(defun ox-dump-wrap (contents &optional prefix)
  "When PREFIX is non-nil, return CONTENTS wrapped there with.

PREFIX may contain the following special symbols:

 *	is replaced with result of dumping %ITEM and %CHILDREN.
 \\n	is replaced with a newline character according to
	default encoding.

All other elements of PREFIX are taken literally."
  (if prefix
      (let* ((ml
	      (replace-regexp-in-string "\\\\n" "\n" prefix)
	      )
	   (cstr (format "%s" contents))
	   (results (replace-regexp-in-string "\\*" cstr ml t t)))
	(ox-dump--debug-transcoder 'ml ml)
	(ox-dump--debug-transcoder 'wrap results)
	(when (org-not-nil results) results))
    contents))

;;(replace-regexp-in-string "\\*" (format "%s" "some text") (replace-regexp-in-string "\\\\n" "\n" "(foo\\n *\\n bar)"))

(defun ox-dump--make-wrap-prefix (element info)
  "Make a wrap prefix when ELEMENT or INFO supply :dump-expr."
  ;;(message "PREFIX:%s (var:%s)\ninfo[%s]\nelement[%s]" (plist-get info :dump-expr) org-dump-expr info element)
  (let
      ;;((prefix (org-element-property :dump-expr element)))
      ((prefix (format "%s" (or (org-element-property :DUMP_EXPR element)
				(plist-get info :dump-expr)
				org-dump-expr))))
    (ox-dump--debug-transcoder 'prefix-prop (org-element-property :DUMP_EXPR element))
    (ox-dump--debug-transcoder 'prefix-info (plist-get info :dump-expr))
    (ox-dump--debug-transcoder 'prefix-vars org-dump-expr)
    (ox-dump--debug-transcoder 'prefix prefix)
    ;; (when prefix
    ;;   (replace-regexp-in-string
    ;;    "[()]" ""
    ;;    (replace-regexp-in-string
    ;; 	"\\*.*$" ""
    ;; 	prefix)))
    prefix
    ))

(defun ox-dump--not-implemented (element-type)
  "Add a warning when we encounter an unhanlded ELEMENT-TYPE."
  (ox-dump--debug-transcoder 'not-imlemented element-type)
  (format "\n;; Element of type '%s' not implemented!\n" element-type))

(defun ox-dump-asis (obj contents info)
  "Return CONTENTS unaltered.  Ingore OBJ and INFO."
  (let ((result (format ";; %s" contents)))
    (ox-dump--debug-transcoder 'asis result)
    result))

(defun ox-dump-text (contents info)
  "Return CONTENTS unaltered.  Ingore INFO."
  (let ((result (ox-dump-rtrim
		 (org-export-data-with-backend
		  (org-element-property :title contents)
		  'dump info))))
    (ox-dump--debug-transcoder 'text result)
    result))

;; (defun ox-dump-table-cell (table-cell contents info)
;;   "Transcode a TABLE-CELL element from Org to Emacs Lisp.
;; CONTENTS is the cell contents.  INFO is a plist used as
;; a communication channel."
;;    contents)


;;;; Table Row

(defun ox-dump-headline (_headline contents _info)
  "Transcode a _HEADLINE element from Org to elisp.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((title (ox-dump-rtrim (org-element-property :raw-value _headline)))
	 (result (concat (format "\"%s\"" title) "\n"
			 (ox-dump-rtrim contents))
	 ;;(concat (format "%s" _headline) "\n" contents)
	 ;;contents
		 )
	 (result (ox-dump-wrap result (ox-dump--make-wrap-prefix _headline _info))))
    (ox-dump--debug-transcoder 'headline result)
    result))


;; (defun ox-dump-headline (_headline contents _info)
;;   "Transcode a _HEADLINE element from Org to elisp.
;; CONTENTS holds the contents of the headline.  INFO is a plist
;; holding contextual information."
;;   (org-export-data-with-backend
;;    (org-element-property :title _headline)
;;    'dump _info))



(defun ox-dump-table (table contents info)
  "Transcode a TABLE element from Org to elisp.
CONTENTS holds the contents of the table.  INFO is a plist holding
contextual information."
  (let ((results (ox-dump-wrap (ox-dump-rtrim contents))))
    (ox-dump--debug-transcoder 'table contents)
    results))

(defun ox-dump-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to elisp.
CONTENTS holds the contents of the table-row.  INFO is a plist holding
contextual information."
  (when (eq 'standard (org-element-property :type table-row))
    (let ((results (format "%s\n" (ox-dump-wrap contents))))
      (ox-dump--debug-transcoder 'row results)
      results)))

(defun ox-dump-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to elisp.
CONTENTS holds the contents of the table-cell.  INFO is a plist holding
contextual information."
  (let* ((row (org-element-property :parent table-cell))
         (table (org-element-property :parent row))
         (has-header (org-export-table-has-header-p table info))
         (group (org-export-table-row-group row info))
         (is-header (and has-header (eq 1 group)))
         (sep (if is-header " " " "))
	 (result (format
		  "\"%s\"%s" (if contents contents "")
		  (if (org-export-last-sibling-p table-cell info) "" " "))))
    ;; (format "\"%s %s\" %s" sep (if contents contents "")
    ;;         (if (org-export-last-sibling-p table-cell info) sep ""))
    (ox-dump--debug-transcoder 'cell result)
    result))

(defun ox-dump-section (_section contents _info)
  "Transcode a SECTION element from Org to elisp.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let ((results (ox-dump-rtrim contents)))
    (ox-dump--debug-transcoder 'section results)
    results))

(defun ox-dump-paragraph (_paragraph contents _info)
  "Dump _PARAGRAPH CONTENTS given _INFO."
  (let ((results (format "\"%s\"" (ox-dump-rtrim contents))))
    (ox-dump--debug-transcoder 'para results)
    results))


;; (defun ox-dump-table (table contents info)
;;   "Transcode a TABLE element from Org to Emacs Lisp.
;; CONTENTS is the contents of the table.  INFO is a plist holding
;; contextual information."
;;   (message "found a table!")
;;   (format "%s"
;; 	  (org-trim (org-element-interpret-data
;; 		     `(table nil ,@(org-element-contents table))))))

;; ;; ~/.emacs.d/elpa/org-9.3.2/ox-latex.el@3515
(defun ox-dump-export-as-dump
    (&optional async subtreep visible-only body-only ext-plist)
  "docstring"
  (interactive)
  (org-export-to-buffer 'dump "*Org Elisp Export*"
    async subtreep visible-only body-only ext-plist
    (lambda ()
      (emacs-lisp-mode)
      (when ox-dump-indent-after-insert
	(save-excursion (indent-region (point-min) (point-max))))
      )))

;;;; CRUFT
;; 1.1. option: "formatter", a function to transform document content into elisp data
;; 1.2. option: "names", a list or function providing names for the destructured data
;; 1.3. option: "binder", a symbol to identify the means of binding data to names (default: setq)


;; (defun dm-map-find-level (level-name)
;;   "Find the Dungon map LEVEL-NAME in default `dm-sources'."
;;   (let ((vars (ox-dump-export "my-file.org")))
;;     (when (memq 'maps vars)
;;       ;; if we have a map set, return
;;       ;; the key that matches level-name, if any)
;;       (memq level-name (cdr (assq 'maps vars))))))


(provide 'ox-dump)
;;; ox-dump.el ends here
