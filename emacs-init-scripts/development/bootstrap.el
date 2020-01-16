;;; bootstrap.el --- bootstrap emacs for dungeon-mode development  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Corwin Brust

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

;; Automatically install packages used in deveopment of dungeon-mode
;; URL: https://github.com/mplsCorwin/dungeon-mode

;; Packages are installed alongside the default install path, in
;;   ~/.emacs.d/dm-elpa

;; Add the following line near the top of your init.el or .emacs.d
;; nicked from straight.el (https://github.com/raxod502/straight.el)
;; to auto-install dungeon-mode development configuration
;; dependancies.

;; (let ((bootstrap-file
;;        (expand-file-name "dm-elpa/bootstrap.el" user-emacs-directory)))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          (concat "https://raw.githubusercontent.com/mplsCorwin/dungeon-mode"
;; 		 "/master/emacs-init-scripts/development/bootstrap.el")
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; install deps to a custom location using -Q, e.g:
;; emacs -Q -e "(setq dm-devel-packages-path \"d:/dm-packages\")" -l bootstrap.el 

;; then you can link in the new libraries with this:
;;   (add-to-list 'load-path (expand-file-name dm-devel-packages-path))

;;; Code:

(progn
  (unless (bound-and-true-p dm-devel-packages-path)
    (defvar dm-devel-packages-path
      (expand-file-name "dm-elpa" user-emacs-directory)
      "Folder to store packages installed for dungeon-mode development."))
  (setq
   package-user-dir dm-devel-packages-path
   package-archives  '(("melpa-stable" . "http://stable.melpa.org/packages/")
		       ("melpa" . "http://melpa.org/packages/")
		       ("gnu" . "http://elpa.gnu.org/packages/"))
   package-selected-packages (quote (all-the-icons company-quickhelp which-key rainbow-delimiters use-package delight bug-hunter pos-tip posframe org elisp-format org-make-toc paredit gist fullframe forge diminish diminish-buffer pdf-tools ascii-art-to-unicode company company-box company-emoji treemacs-projectile use-ttf tramp doom-themes treemacs darktooth-theme emojify easymenu cyberpunk-theme writegood-mode smex markdown-mode magit treemacs-magit htmlize flycheck autopair ac-slime helpful)))
  (package-initialize)
  (package-refresh-contents)
  (package-install-selected-packages))
