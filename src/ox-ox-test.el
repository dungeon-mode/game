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
	(named (group (seq (any "[a-zA-Z_]")
			   (0+ (any alnum ?_)))))
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

(defvar my:labels nil "Labels for the groups of var in `my:rx'.")
(setq my:labels '(full-match ; 0
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
		  ))

(defvar my:test-strings nil "These are test strings that should all match.")
(setq my:test-strings '("$foo" "$foo:kw"
	"$_" "$_:kw" "$*" "$*:kw" "$:" "$::kw"
	;; "@#" "$#"
	"@#" "$#"
	"$1" "$+2" "$-3" "$>" "$>>>" "$<" "$<<"
	"@1$1" "@-2$2" "@3$-3" "@+4$+4" "@+4$foo"
	"@<$5" "@6$>" "@>>$7" "@8$<<" "@>>>>>$foo"
	"@I$9" "@II$10" "@I$-1" "@I+1$-2" "@I$+1" "@I-1$+2"
	"@III-3$11" "@+I$12" "@-II$13" "@+III-17$14"
	"@1$2..@3$4" "@+1$-2..@-3$+4"
	"@+II-2$foo..@<<$bar" "@>>>>>$-4..@-IIIII$bar:kw"
	"@>$<..@+III-17$14" "@+III-17..$<" "@+III-17..$+14"))

(defmacro def-rx-var-test (str &optional result)
  "Create an `ert' function testing STR expecting RESULT."
  (declare (indent 2))
  (ignore result)
  `(rx-let ,my:rx
     (when (string-match (rx var) ,str)
       (mapconcat
	'identity
	(delq nil (seq-map-indexed
		   (lambda (_ n)
		     (when-let ((m (match-string n ,str))
				(g (nth-value n my:labels)))
		       (format "| %s | %s | %s |" n m g)))
		   (make-list 34 nil)))
	"\n")
       )))

(defun my:do-rx-test ()
  "Run `my:test-strings' against `my:rx'."
  (interactive)
  (let ((re-string )))
  (with-current-buffer (get-buffer-create "**ox re test result")
    (erase-buffer)
    (goto-char (point-min))
    (insert "#+title: Test Results: ~ox-ox rx~\n
Try ~occur~ where ~n~ is a group number in:
#+begin_example
| n | [^ ]
#+end_example\n
* Regular Expression\n
#+name: regex
#+begin_src emacs-lisp")
    (print (eval `(macroexpand (rx-let ,my:rx (rx var))))
	   (current-buffer))
    (insert "#+end_src\n\n* Test Strings")
    (dolist (test-form my:test-strings)
      (insert (format "\n\n** %s\n\n| n  | match | group |\n" test-form))
      (insert (def-rx-var-test test-form)))
    (pop-to-buffer (current-buffer))
    (org-mode)))

(provide 'ox-ox-test)
;;; ox-ox-test.el ends here
