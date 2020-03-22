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
	(var (or special range? (remote range)))))
;; (eval (macroexpand `(rx-let ,my-rx (string-match-p (rx var) "$foo"))))

(defvar my:rx-labels nil "Labels for the groups of var in `my:rx'.")
(setq my:rx-labels
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
	;; 34-40 never fill because remote doesn't handle:
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
	))

(defvar my:rx-test-strings nil "These are test strings that should all match.")
(setq my:rx-test-strings
      '("$foo" ;; "remote(Bar,$foo)" "$foo:kw" ;;ZZZ: ?
	"$_" "$_:kw" "$*" "$*:kw" "$:" "$::kw"
	"@#" "$#" "$1" "$+2" "$>>>"
	"@1$1"  "@1$1..@-2$+1" "remote(Bar,@1$1..@-2$+1)"
	"@-2$+1"  "@-2$+1..@3$>>>" "remote(Bar,@-2$+1..@3$>>>)"
	"@3$>>>"  "@3$>>>..@+4$foo" "remote(Bar,@3$>>>..@+4$foo)"
	"@+4$foo"
	"@+4$foo..@<$1" "remote(Bar,@+4$foo..@<$1)"
	"@<$1"  "@<$1..@<<$-2" "remote(Bar,@<$1..@<<$-2)"
	"@<<$-2"  "@<<$-2..@>>>$<<<" "remote(Bar,@<<$-2..@>>>$<<<)"
	"@>>>$<<<"  "@>>>$<<<..@>>>>$foo" "remote(Bar,@>>>$<<<..@>>>>$foo)"
	"@>>>>$foo"
	"@>>>>$foo..@I$1" "remote(Bar,@>>>>$foo..@I$1)"
	"@I$1"  "@I$1..@I+2$>>" "remote(Bar,@I$1..@I+2$>>)"
	"@I+2$>>"  "@I+2$>>..@III$foo" "remote(Bar,@I+2$>>..@III$foo)"
	"@III$foo"  "@III$foo..@+IIII-44$+4" "remote(Bar,@III$foo..@+IIII-44$+4)"
	"@+IIII-44$+4"
	"@+IIII-44$+4..@III-3$11" "remote(Bar,@+IIII-44$+4..@III-3$11)"
	"@III-3$11"  "@III-3$11..@+I$12" "remote(Bar,@III-3$11..@+I$12)"
	"@+I$12"  "@+I$12..@-II$13" "remote(Bar,@+I$12..@-II$13)"
	"@-II$13"  "@-II$13..@+III-17$14" "remote(Bar,@-II$13..@+III-17$14)"
	"@>$<..@+III-17$foo" "remote(Bar,@>$<..@+III-17$foo)"
	"@+III-17$foo..$<" "remote(Bar,@+III-17$foo..$<)"
	"@+2$foo..@<<<$+14" "remote(Bar,@+2$foo..@<<<$+14)"
	"@>>$foo..$+42"	"remote(Bar,@>>$foo..$+42)"))

(defmacro my:rx-test (str &optional result)
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
				(g (nth-value n my:rx-labels)))
		       (format "| %s | %s | %s |" n m g)))
		   (make-list 72 nil)))
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
    (dolist (test-form my:rx-test-strings)
      (insert (format "\n\n** %s\n\n| n  | match | group |\n" test-form))
      (insert (my:rx-test test-form)))
    (pop-to-buffer (current-buffer))
    (org-mode)))

(provide 'ox-ox-test)
;;; ox-ox-test.el ends here
