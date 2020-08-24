;;; dm-character.el --- work with character sheets   -*- lexical-binding: t; -*-

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

;; Load, display, manipulate and store player characters.

;;; Code:

;; DEVEL hack: prefer version from CWD, if any
(let ((load-path (append '(".") load-path)))
  (require 'dm-table)
  (require 'dm-map))

(dm-files-defilters :character
		    (apply-partially #'dm-files-filter-by-re
				     "Docs/Characters/.*[.]org$"))

(defgroup dm-character nil
  "Customize character and related UI/UX settings."
  :group 'dungeon-mode)

(defcustom dm-character-files (dm-files-select :character)
  "Files containing player characters."
  :type '(repeat (choice file directory))
  :group 'dm-character)

(defvar dm-characters nil "Player character information as an hash-table.")

(defvar dm-character-collections
  (list 'attribute 'possession 'setting 'armor 'weapon 'use-counter)
  "Groups of player character information.")

(defvar dm-character-party nil "Parties and intersectional character data.")

(defvar dm-character-party-collections
  (list 'elf 'dwarf 'human 'heals 'know-chamber 'know-event)
  "List of party attributes to collect from characters.")

(defvar dm-character-read-current-character-name nil
  "The name of the current character while loading.")

(defvar dm-character-read-buffer nil
  "Data of the character in process while loadding.")

(defun dm-character-transform (table)
  "Transform a TABLE of strings into an hash-map."
  (let* ((cols (seq-map (lambda (label) (intern (downcase label)))
			(car table)))
	 (character-name (if (not (string= "name" (caadr table)))
			     (or dm-character-read-current-character-name
				 (error "Cannot find character name"))
			   (setq dm-character-read-buffer (dm-make-hashtable))
			   (setq dm-character-read-current-character-name
				 (intern (nth 2 (cadr table))))))
	 (cform
	  `(dm-coalesce-hash ,(cdr table) ,cols
	     :hash-table dm-character-read-buffer
	     (append (gethash (intern ,(car cols))
			      dm-character-read-buffer)
		     (mapcan (lambda (col)
			       (list col (symbol-value col)))
			     ',cols))))
	 (result (eval `(list :load ,cform))))
    ;;(message "cform:%s" cform)
    (puthash dm-character-read-current-character-name
	     dm-character-read-buffer dm-characters)))

;; DEVEL: give us a global key binding precious
(global-set-key (kbd "<f7>") 'dm-character-draw)

(defvar dm-table-load-function)
(let (dm-table-load-function) ;; no Load function
  (dm-table-defstates 'character :transform #'dm-character-transform))


;; test case
(let ((dm-table-type 'character))
  (setq dm-characters (dm-make-hashtable))
  (apply #'dm-table-extract-files dm-character-files))

(provide 'dm-character)
;;; dm-character.el ends here
