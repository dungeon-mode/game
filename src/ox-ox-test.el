;;; ox-ox-test.el --- tests for ox-ox                -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Keywords:

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

;; tests, mostly of rx stuff

;; this initial version proves out some regexes used to support TBLFM
;; like interpolation syntax for @n$x, $#

;;; Code:

(require 'rx)

;; Mostly taken from `org-table' then converted for `rx'
(defvar my:rx)
(setq my:rx
      '((var-sigel ?$)
	(col-sigel ?$)
	(row-sigel ?@)
	(key-sigel ?:)
	(named (group (1+ (any alnum ?_))))
	(numbered (group (1+ digit)))
	(sign (group (any ?+ ?-)))
	(boundry (group (1+ (or ?< ?>))))
	(special (or (seq var-sigel (group (any ?* ?_ ?: ?#)) (opt key))
		     (seq ?@ (group ?#))))
	;;(nonzero (seq "[1-9]" (1+ "[0-9]")))
	(key (seq key-sigel named))
	(col (or named
		 boundry
		 (seq (opt sign) numbered)))
	(row (or boundry
		 (seq (opt sign)
		      (group (1+ ?I))
		      (opt (seq sign numbered)))
		 (seq (opt sign) numbered)))
	(cell (or (seq row-sigel row (opt (seq col-sigel col)))
		  (seq col-sigel col)))
	(range (seq cell (opt (seq ".." (or cell row)))))
	(var (or special range))))
;; (eval (macroexpand `(rx-let ,my-rx (string-match-p (rx var) "$foo"))))


(defvar my:test-strings nil "These are test strings that should all match.")
(setq my:test-strings '("$foo" "$foo:kw"
	"$_" "$_:kw" "$*" "$*:kw" "$:" "$::kw"
	;; "@#" "$#"
	"@#" "$#"
	"$1" "$+2" "$-3" "$>" "$>>>" "$<" "$<<"
	"@1$1" "@-2$2" "@3:-3" "@+4$+4"
	"@<$5" "@6$>" "@>>$7" "@8$<<"
	"@I$9" "@II$10" "@I$-1" "@I+1$-2" "@I$+1" "@I-1$+2"
	"@III-3$11" "@+I$12" "@-II$13" "@+III-17$14"
	"@+II-2$foo..@<<$bar" "@>>>>>$-4..@-IIIII$bar:kw"
	"@<$<..@+III-17$14" "@+III-17..$<" "@+III-17..$+14"))

(defmacro def-rx-var-test (str &optional result)
  "Create an `ert' function testing STR expecting RESULT."
  (declare (indent 2))
  (ignore result)
  `(rx-let ,my:rx
     (when (string-match (rx var) ,str)
       (mapconcat
	'identity
	(append
	 '("| n  | val |")
	 (seq-map-indexed
	  (lambda (_ n)
	    (format "| %s | %s |" n (match-string n ,str)))
	  (make-list 34 nil)))
	"\n")
       )))

(defun my:do-rx-test ()
  "Run `my:test-strings' against `my:rx'."
  (interactive)
  (with-current-buffer (get-buffer-create "**ox re test result")
    (erase-buffer)
    (goto-char (point-min))
    (insert "\n#+begin_src emacs-lisp\n")
    (print (eval `(macroexpand (rx-let ,my:rx (rx var))))
	   (current-buffer))
    (insert "\n#+end_src")
    (dolist (test-form my:test-strings)
      ;; (prin1 (list test-form (test-string test-form)))
      (insert (format "\n* %s\n" test-form))
      (insert (def-rx-var-test test-form)))
    (pop-to-buffer (current-buffer))
    (org-mode)))

(provide 'ox-ox-test)
;;; ox-ox-test.el ends here
