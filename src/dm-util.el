;;; dm-util.el --- dungeon-mode table binding internals  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Corwin Brust

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

;; TODO dm-util:1 refactor to use `cl-destructuring-bind' then factor
;;  out most or all of the keyword implementations compisitonally.

;; TODO dm-util:2 binds should be the first arg and strings the second
;;  this can be done irrespective of when we take on the dm-util:1.

;;; Code:

(defgroup dm-util nil "Localizable internals.

These settings are generally intended to be changed lexically
from elisp-code; however, for game development it may be
convenient to customize some of defaults." :group 'dungeon-mode)

(defcustom dm-util-default-coalesce-key '(id)
  "Default key when coalescing strings to hashes.

See `dm-coalesce-hash."
  :group 'dm-util
  :type '(sequence symbol))
;;(setq dm-util-default-coalesce-key '(id))

(defun dm--remove-keywords (form)
  "Return FORM with any :keyword and following args removed."
  (if (and (car-safe form)
	   (cdr-safe form)
	   (car-safe (cdr form))
	   (keywordp (car form)))
      (dm--remove-keywords (cdr-safe (cdr form)))
    (if (cdr-safe form)
	(cons (car-safe form) (dm--remove-keywords (cdr form)))
      (unless (and (car-safe form) (keywordp (car form)))
	  form))))
;;(equal (dm--remove-keywords '(foo :bar baz qwz)) '(foo qwz))
;;(equal (dm--remove-keywords '(:foo 1 baz qwz)) '(baz qwz))

(cl-defmacro dm-coalesce-hash
    (strings &optional (bindings dm-util-default-coalesce-key) &body body
	     &key (hash-symbol 'hash)
	     (hash-table `(make-hash-table) hash-p)
	     (row-symbol 'row)
	     (key-symbol (car (delq '_ (delq nil bindings))))
	     (start-column 0)
	     (result-symbol 'result)
	     enable-after-nil
	     (after `(when (and ,key-symbol
				(or ,enable-after-nil ,result-symbol))
		       (puthash (if (stringp ,key-symbol)
				    (intern ,key-symbol)
				  ,key-symbol)
				,result-symbol
				,hash-symbol)))
	     &allow-other-keys)
  "Build HASH by repeatedly applying BINDINGS to STRINGS for BODY.

STRINGS is list of string lists, ((\"aa\" \"ab\") (\"ba\" \"bb\")).

BINDINGS are symbols, each bound from STRINGS at the corrisonding
index.  Use \"_\" to ignore given positions.  BODY is evaluated
once per outer STRING list item after BINDINGS.  BINDINGS are
taken from `dm-util-default-coalesce-key' when nil or omitted.

HASH-TABLE allows hash-table reuse.  HASH-SYMBOL (default:
\"hash\") exposes HASH-TABLE to BODY.  ROW-SYMBOL (default:
\"row\") is bound full list taken from STRINGS for a given
evaluation of BODY.  KEY-SYMBOL provides the value for the key
written in HASH by the default AFTER implementation; it defaults
to to the first of BINDINGS.  START-COLUMN (default: 0) allows
specifying the first column index for BINDINGS.
RESULT-SYMBOL (default: \"result\") exposes the result of BODY
after each evaluation.  When ENABLE-AFTER-NIL is nil (the
default) AFTER is not executed unless FORMS returns a nil value.
When AFTER expression is non-nill it is evaluated after BODY in
the same context given either BODY result or ENABLE-AFTER-NILL is
not nil.  The default AFTER implementation inserts the value of
RESULT-SYMBOL into the hash identified by HASH-SYMBOL creating a
key by interning the value of KEY-SYMBOL."
  (declare (indent 2))
  (let* ((column-count start-column)
	 (body-form (if body (dm--remove-keywords body)
		      `((list ,@(mapcan
				 (lambda (var) `(',var ,var))
				 (delq '_ (delq nil bindings)))))))
	 (symbol-bindings
	  (delete nil (mapcar
		       (lambda (var)
			 (prog1 (when (and var
					   (not (equal '_ var))
					   (>= column-count start-column))
				  (list var `(nth ,column-count ,row-symbol)))
			   (setq column-count (1+ column-count))))
		       bindings)))
	 (row-form `(lambda (,row-symbol)
		      ,(if (and symbol-bindings (length symbol-bindings))
			   `(let (,@symbol-bindings)
			      (prog1 (setq ,result-symbol (progn ,@body-form))
				,after))
			 `(if ,after
			      (prog1 (setq ,result-symbol (progn ,@body-form))
				,after)
			    (progn ,@body-form))))))
    ;;(prin1 symbol-bindings)
    `(let* ((,hash-symbol ,hash-table) ,row-symbol ,result-symbol)
       (mapcar ,row-form (quote ,strings))
       ,hash-symbol)))

(dm-coalesce-hash (("h1" "H2" "H3")
		   ("r1" "V12" "V13")
		   ("r2" "V22" "V23")
		   ("r3" "V32" "V33"))
    nil
  (puthash (nth 0 row) row hash)
  :after nil)

(dm-coalesce-hash (("h1" "H2" "H3")
			 ("r1" "V12" "V13")
			 ("r2" "V22" "V23")
			 ("r3" "V32" "V33"))
    (_)
  (puthash (intern (nth 0 row)) row hash)
  :after nil
  )

(dm-coalesce-hash (("h1" "H2" "H3")
			 ("r1" "V12" "V13")
			 ("r2" "V22" "V23")
			 ("r3" "V32" "V33"))
    (_)
  :after
  (puthash (intern (nth 0 row)) (cdr row) hash))

(dm-coalesce-hash (("h1" "H2" "H3")
			 ("r1" "V12" "V13")
			 ("r2" "V22" "V23")
			 ("r3" "V32" "V33"))
    ;;(id h2)
  )

(let ((dm-default-coalesce-key '(id _ h1)))
  (dm-coalesce-hash (("h1" "H2" "H3")
		     ("r1" "V12" "V13")
		     ("r2" "V22" "V23")
		     ("r3" "V32" "V33"))))

(dm-coalesce-hash (("h1" "H2" "H3")
		   ("r1" "V12" "V13")
		   ("r2" "V22" "V23")
		   ("r3" "V32" "V33"))
    (id bob)
  (list (quote bob) bob))



(dm-coalesce-hash (("h1" "H2" "H3")
			 ("r1" "V12" "V13")
			 ("r2" "V22" "V23")
			 ("r3" "V32" "V33"))
    (id _ h2)
  :start-column 0
  (list 'id (format "id:<%s>" id) '2 h2 'raw row))

(let ((h (dm-coalesce-hash (("h1" "H2" "H3")
				  ("r1" "V12" "V13")
				  ("r2" "V22" "V23")
				  ("r3" "V32" "V33"))
	     (id h2)
	   :hash-table #s(hash-table size 30 test equal)
	   :start-column 1
	   (list 'id id 'h2 h2))))
  ;;(print h)
  h)

(provide 'dm-util)
;;; dm-util.el ends here
