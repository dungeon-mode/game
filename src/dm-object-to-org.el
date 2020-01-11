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
(require 'eieio)
(require 'subr)
(require 'seq)

;; use format sequences to stringify objects for use with `org-mode'

;;; Code:

;;; CONSIDER:
;;;  - use DATA cell (first slot) for alist
;;;    ( #+NAME . ( PROPERTIES-DRAWER ))
;;; Construct Doc:
;;;  - format per DATA
;;;  - SRC block for object's elisp source
;;; Construct Obj:
;;;  - when SRC block exists:
;;;    * load from block
;;;    * update from any org sources for named items given in DATA
;;;  - when no SRC block exists:
;;;    * prompt user for any missing info (e.g. slots, instance types,...)
;;;  - must be able to define classes as well as instances



(defvar dm-object-to-org:keyword-alist
  '((:h* dm-object-to-org--format-h*)
    (:begin_src   "\n#+BEGIN_SRC")
    (:end_src     "\n#+END_SRC\n")
    (:begin_quote "\n#+BEGIN_QUOTE")
    (:end_quote   "\n#+END_QUOTE\n")
    (:begin_table "\n|")
    (:end_table   "|\n"))
  "Alist mapping keywords to org syntax.

Maps a keyword representing an ORG-MODE-BLOCK to a STRING
representation.  Values may be either string or other
literal (which will be converted to string) or a function which
will receive the keyword along with the format sequence in which
and index at which it appeared.")

(defun dm-object-to-org--format-h* (kw &rest ignored)
  "Format org headline type KW as a string.

Other inputs are IGNORED.

\(fn (kw &optional obj seq ix))"
  (ignore ignored)
  (concat "\n"
	  (make-string
	   (string-to-number (substring (symbol-name kw) 2))
	   ?*)
	  " "))
;;(dm-object-to-org--format-h* :h3)

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
    string as necessary.

Note, although most unrecognized keywords will be silently
ignored, due to the implemention of :h1 and friends those
beginning with :h won't be.  A confusing error will be throw if
not such a keyword is found which does not appear in
`dm-object-to-org-keyword-alist'."
  (when obj
    (apply 'concat
	   (delete
	    nil
	    (seq-map-indexed
	     (lambda (fmt findex)
	       (cond ((stringp fmt) fmt)
		     ((keywordp fmt)
		      (let ((fmtr
			     (cadr (assq fmt dm-object-to-org-keyword-alist))))
			(if fmtr
			    (if (functionp fmtr)
				(funcall fmtr fmt obj seq findex)
			      (format "%s" fmtr))
			  (when  (string= "h" (substring (symbol-name fmt) 1 2))
			    (funcall (cadr (assq ':h* dm-object-to-org-keyword-alist))
				     fmt obj seq findex)))))
		     ((slot-exists-p obj fmt) (format "%s" (oref obj fmt)))
		     ((eieio-object-p fmt)
		      (ingore "TODO: save this for when it moves into dm-persistent"))
		     ((functionp fmt) (funcall fmt fmt obj seq findex))
		     (t (format "%s" fmt)))) seq)))
    ))

(dm-object-to-org (my-class) :h1 "hi" :h3 "hi back" :begin_src :h4 'my-foo)

(defun my-foo (&rest args) "Docstring using ARGS." (ignore args) "Hi")
(defclass my-class () ((foo :initform "bar" :initarg :foo)))

(provide 'dm-object-to-org)
;;; dm-object-to-org.el ends here
