;;; dm-object-to-org.el --- stringify objects to org format  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Keywords: lisp

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

;;; require core
(require 'seq)

;; use format sequences to stringify objects for use with `org-mode'

;;; Code:

(defvar dm-object-to-org-keyword-alist
  '((:h* dm-object-to-org--format-h*)
    (:begin_src   "#+BEGIN_SRC")
    (:end_src     "#+END_SRC")
    (:begin_quote "#+BEGIN_QUOTE")
    (:end_quote   "#+BEGIN_QUOTE")
    (:begin_src   "|")
    (:end_src     "|"))
  "Alist mapping keywords to org syntax.

Maps a keyword representing an ORG-MODE-BLOCK to a STRING
representation.  Values may be either string or other
literal (which will be converted to string) or a function which
will receive the keyword along with the format sequence in which
and index at which it appeared.")

(defun dm-object-to-org--format-h* (kw obj seq ix)
  "Format org headline type KW as a string.

OBJ, SEQ and IX are unused."
  (let ((length (string-to-number (substring (symbol-name kw) 2))))
    (concat (make-string (* 2 length) ?\  ) (make-string length ?*))))
;;(dm-object-to-org--format-h* :h3 nil nil nil)

(defun dm-object-to-org (obj &rest seq)
  "Return string continaing org markup for OBJ.

SEQ are org blocks, OBJ slots, other objects, and instructions
for formatting these.

Supported Directives:

A list found following an org specifier is taken as args and
appended to the (first line of) output concaterating all elements
and any prior text on the line with a space.  The same is true
for a list found following a symbol with a function defination,
see below, however list is inital args to function al-al
`apply-partially'.  Similarly, a list found following an object
is assumed to be inputs to this function to support a recursive
call.

  - Org Directives:

      * :h1 .. :hN - per `TODO var name for max org levels??'
      * :begin-src and :end-src - begin/end org code block
      * :begin-quote and :end-quote - begin/end org quote block
      * :start-table and :end-table - begin/end org table

  - Object/Slot/Other Access Directives

      * OBJECT - insert the result of calling this function on OBJECT
      * SLOT (unquoted) - include content of SLOT of OBJ
      * FUNCTION (unquoted) - call FUNCTION passing obj, sections
        and position into sections as a zero based index.

  - Any other values treated as literal and converted to
    string as necessary."
  (when obj
    (apply 'concat
     (delete
      nil
      (seq-map-indexed
       (lambda (format findex)
	 (cond ((keywordp format)
		(let ((fmtr
		       (cadr (assq format dm-object-to-org-keyword-alist))))
		  (if fmtr
		      (if (stringp fmtr)
			  fmtr
			(when (functionp fmtr)
			  (funcall fmtr format obj seq findex)))
		    (when  (string= "h" (substring (symbol-name format) 1 2))
		      (funcall (cadr (assq ':h* dm-object-to-org-keyword-alist))
			       format obj seq findex)))))
	       )) seq)))
    ))

;;(dm-obj->org t :h1 :h3 :begin_src :h4)

(provide 'dm-object-to-org)
;;; dm-object-to-org.el ends here
