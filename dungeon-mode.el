;;; dungeon-mode.el --- interactive dungeon game  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is the dungeon REPL, a package for interactively creating
;; games and working with journals, i.e. game data.

;;; Requires:
(require 'comint)
(require 'ielm)

;;; Code:

;; preprocesser
(defun idm-preprocess-input (str)
  "Return INPUT wrapped in parens.

TODO Implement all differences from raw elisp treatment of input"
  (message "hi from comint input filter")
  (when (comint-nonblank-p str)
    (concat "(" str ")")))

(defvar idm-input "Semaphore for console input")

(defun idm-comint-input-sender (input)
  "Further process INPUT parsed by `comint'.

Preprocessing is performed by `idm-preprocess-input'.
Result is stored in `idm-input' and returned."
  (setq idm-input (idm-preprocess-input input)))

(defun idm-eval-prompt-to-point (&optional for-effect)
  "Evaluate the expression after the prompt.

Suppress result echo when FOR-EFFECT is t.
TODO: truthy? double check
This is idential to `ielm-process-input' except that
it reads from `idm-input' instead of `ielm-input'."
  (interactive)
  (let (idm-input)                    ; set by idm-comint-input-sender
    (comint-send-input)               ; update history, markers etc.
    (ielm-eval-input idm-input for-effect)))

(defun idm-return (&optional for-effect)
  "Newline and indent, or evaluate the sexp before the prompt.

We are attempting to impelment the same general behavior as from
\`ielm-return' (inkluging supporting `ielm-dynamic-return')
with the added complity that our input expression requires
additional processing to be an valid Emacs Lisp Expression."
  (interactive)
  (if (and nil ielm-dynamic-return)
      (let ((state
             (save-excursion
               (end-of-line)
               (parse-partial-sexp (ielm-pm)
                                   (point)))))
        (if (and (< (car state) 1) (not (nth 3 state)))
            (ielm-send-input for-effect)
          (when (and ielm-dynamic-multiline-inputs
                     (save-excursion
                       (beginning-of-line)
                       (looking-at-p comint-prompt-regexp)))
            (save-excursion
              (goto-char (ielm-pm))
              (newline 1)))
          (newline-and-indent)))
    (newline)))

(defvar idm-map
  (let ((map (make-sparse-keymap)))
    ;;(define-key map "\t" 'ielm-tab)
    (define-key map "\C-m" 'ielm-return)
    (define-key map "\e\C-m" 'ielm-return-for-effect)
    (define-key map "\C-j" 'idm-eval-prompt-to-point)
    (define-key map "\e\C-x" 'eval-defun)         ; for consistency with
    (define-key map "\e\t" 'completion-at-point)  ; lisp-interaction-mode
    ;; These bindings are from `lisp-mode-shared-map' -- can you inherit
    ;; from more than one keymap??
    (define-key map "\e\C-q" 'indent-sexp)
    (define-key map "\177" 'backward-delete-char-untabify)
    ;; Some convenience bindings for setting the working buffer
    ;;(define-key map "\C-c\C-b" 'ielm-change-working-buffer)
    ;;(define-key map "\C-c\C-f" 'ielm-display-working-buffer)
    ;;(define-key map "\C-c\C-v" 'ielm-print-working-buffer)
    map)
  "Keymap for iDM.")

;; ;;; takover some ielm and comint hooks, install our pre-processor
;; (setq comint-input-filter-functions nil
;;       comint-input-filter nil)

(define-derived-mode idm ielm "IDM"
  "Interactive multi-user dungeon game."
  )


;; (defvar elm-input)
;; ;;d:/emax64/share/emacs/26.1/lisp/ielm.el
;; (defun idm-send-input (&optional for-effect)
;;   "Evaluate the (almost) Emacs Lisp expression after the prompt."
;;   (interactive)
;;   (let (ielm-input)                     ; set by ielm-input-sender
;;     (comint-send-input)                 ; update history, markers etc.
;;     (setq ielm-input (idm-input-filter elm-input)
;;     (ielm-eval-input ielm-input for-effect)))



;; (defun ielm-input-sender2 (_proc input)
;;   ;; Just sets the variable ielm-input, which is in the scope of
;;   ;; `ielm-send-input's call.
;;   (setq ielm-input input))

;; (defun ielm-input-sender (_proc input)
;;   "Replace the input sender method from ielm.

;; Like the original it takes _PROC (ignored) and INPUT.  Unlike the
;; original we apply our preprocessor.  If this wasn't an hack for a
;; game I'd start with a patch for this.  As is, I want to get to
;; POC before I suggest anyone else change anything.  Maybe
;; preprocessing input to ielm is a terrible idea.  Maybe deriving
;; from ielm is a terrible idea.  Maybe ielm needs a
;; input-filter-function that can act as a mutator.  Who knows.

;; TODO: this should be done as defadvice local binding"
;;   ;; Just sets the variable ielm-input, which is in the scope of
;;   ;; `ielm-send-input's call.
;;   (setq ielm-input (idm-input-filter input)))

;; (defadvice ielm-input-sender (before (input) activate)
;;   "Preprocess input from dungeon-mode to `ielm'."
;;   (setq input


(provide 'dungeon-mode)
;;; dungeon-mode.el ends here
