;;; sample-init.el --- setup emacs with use-package the load dungeon-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Keywords: games, convenience

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

;; This is an example Emacs configuration file that sets up various
;; packages from MELPA are repository of features generally neither
;; provided nor supported by GNU.  Likewise the authors of
;; dungeon-mode cannot explicitly vouch for these packages except to
;; say that we, ourselves, use them and, as users, recommend them.

;;; Code:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; Hard-coding the path to init-dm seem to be enough for now.
;; this will be used in a load-library call at the end of the script
(defvar my:dungeon-mode-init-path
  "d:/Projects/dungeon-mode/emacs-init-scripts/user/init-dm.el"
  "Location for the `dungeon-mode' init-dm.el script.")

(require 'package)
(add-to-list 'package-archives
	     '("MELPA" . "https://melpa.org/packages/") t)
(when (version< emacs-version "27.0")
  (package-initialize))

;; !ALERT! This script requires and does not attempt attempt to
;; !ALERT! bootstrap use-package.  Please install manually.
(require use-package)

(use-package diminish :ensure t)

;; shhh
(defalias 'yes-or-no-p 'y-or-n-p)

;; disable boilerplate UX gadgets
;;(scroll-bar-mode -1)
;;(tool-bar-mode -1)
;;(menu-bar-mode -1)

;; unfuckify
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t
      delete-by-moving-to-trash t)
(show-paren-mode t)
(prefer-coding-system 'utf-8)

;; clipboard
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; display line endings
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;;;;;;;;;;
;; line setup from https://github.com/mrvdb/emacs-config
(setq-default truncate-lines nil)
;; Similar to mail messages, use vertical bar for wrapped paragaphs
(setq visual-line-fringe-indicators '(vertical-bar nil))

;; For all text modes use visual-line-mode
(add-hook 'text-mode-hook 'visual-line-mode)

;; clobber tab related default
(setq tab-width 2
      indent-tabs-mode nil)

(setq column-number-mode t)
;;(require 'autopair)
(use-package rainbow-delimiters :ensure t)

(use-package powershell :ensure t)

;; help learning the keys
(use-package which-key
  :ensure t
  :defer 0.5
  :delight
  :config (which-key-mode))

;;; playing with smart-hungry delete
(use-package smart-hungry-delete
  :ensure t
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
		 ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks
  :config (smart-hungry-delete-add-default-hooks))

(use-package emojify
  :ensure t
  ;; :after erc
  :defer 15
  :config
  (global-emojify-mode)
  (global-emojify-mode-line-mode -1))

;; Save places in buffers between sessions
(use-package saveplace
  :ensure nil ;builtin
  :init
  (setq-default save-place-mode t))

;; local sources.  I think...

;; whitespace on-save cleanup
;; (use-package ws-butler
;;   :ensure nil ;; ALERT: local sources!
;;   :diminish
;;   :init
;;   (add-hook 'prog-mode-hook 'ws-butler-mode)
;;   (add-hook 'text-mode-hook 'ws-butler-mode)
;;   ;;(ws-butler-global-mode)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion setup

;; basic completion with hippie-expand

;; https://github.com/asummers/.emacs.d/blob/master/init.el
(use-package hippie-expand
  :ensure nil
  :bind
  ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
	'(try-expand-dabbrev
	  try-expand-dabbrev-all-buffers
	  try-expand-dabbrev-from-kill
	  try-complete-file-name-partially
	  try-complete-file-name
	  try-expand-all-abbrevs
	  try-expand-list
	  try-expand-line)))

;; "smex is a necessity. It provides history and searching on top of M-x."
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ido filesystem nav
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window and frame setup and movement

;; undo/redo window arrangment commands with C-c <left-or-right>

;;(require 'winner)
;;(winner-mode)

;; zip window to window with C-M-<arrow>
;; binds are over in init-keybinds.el
;;(require 'windmove)
;; change frame when zipping past edgeward window
;; ALERT local source: https://www.emacswiki.org/emacs/framemove.el
;;(require 'framemove)
;;(setq framemove-hook-into-windmove t)

;; end windown and frame setup and movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; linting with flycheck
;; https://gist.github.com/daniel-vu/25826682d9af2a201a150aa3c4a4321a
(use-package flycheck
  :ensure t
  :defer 2
  :delight
  :init (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay .3)
  :config
  ;;(add-hook 'typescript-mode-hook 'flycheck-mode)
  )

(use-package company
  :ensure t
  :config
;;  (add-to-list 'company-backends 'company-math-symbols-unicode)
;;  (add-to-list 'company-backends 'company-elisp)
;;  (add-to-list 'company-backends 'company-emoji)
  ;; extra documentation while you wait
;;  (use-package company-quickhelp :ensure t :config
;;    (company-quickhelp-mode))
  ;; M-<number> to select a completion
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  ;; invert direction when popup is near bottom of window
  ;; TODO: can we also invert the keymap here somehow?
  (setq company-tooltip-flip-when-above t)
  (setq company-tooltip-align-annotations t)
  (global-company-mode)) ;; use everywhere

(use-package helpful :ensure t
  :config
  ;; config and commentay from https://github.com/Wilfred/helpful
;;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)

;;;; I also recommend the following keybindings to get the most out of helpful:
  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)

;;;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function)

;;;; Look up *C*ommands.
  ;;
  ;; By default, C-h C is bound to describe `describe-coding-system'. I
  ;; don't find this very useful, but it's frequently useful to only
  ;; look at interactive functions.
  (global-set-key (kbd "C-h C") #'helpful-command)

;;;; Ivy users can use Helpful with counsel commands:
  ;; (setq counsel-describe-function-function #'helpful-callable)
  ;; (setq counsel-describe-variable-function #'helpful-variable)
  ;;
) ;;;;;;;;;;; helpful
(require 'powerline)
(powerline-default-theme)


;; Added by Erik Elmshauser to enable dungeon-mode at emacs launch
(load-library my:dungeon-mode-init-path)

(provide 'sample-init)
;;; sample-init.el ends here
