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

(require 'subr-x)
(require 'cl-macs)
(require 'seq)

;; DEVEL hack: prefer version from CWD, if any
(let ((load-path (append '(".") load-path)))
  (require 'dm-table)
  (require 'dm-map))

(defun dm-character-files-select (file)
  "Return FILE when it could contain character information."
  (and (string-match-p "[Dd]ocs?/[Cc]haracters?/.*[.]org$" file)
       (not (string-match-p "tiles?-" file))))

(dm-files-defilters :character #'dm-character-files-select)

(defgroup dm-character nil
  "Customize character and related UI/UX settings."
  :group 'dungeon-mode)

(defcustom dm-character-files (dm-files-select :character)
  "Files containing player characters."
  :type '(repeat (choice file directory))
  :group 'dm-character)

(defvar dm-characters nil "Player character information as a plist.")

(defvar dm-character-name nil "The name of the current character.")

(defvar dm-character-tiles-default-props
  (list 'name 'value
	'text 'label
	'documentation 'note
	'count 'remaining
	'treatment)
  "List, propoperties to expose when creating layout cells.")

(defvar dm-character-tiles-alist
  (list (list 'name 'pos (cons 1 1)
	      'style (list (cons 'font-size 5)
			   (cons 'y 2.75))
	      'text "value"
	      ;;'props (list 'name 'value)
	      'value #'dm-character-get-attr)
	(list 'armor-label 'pos (cons 1 5)
	      'style (list (cons 'font-size 3)
			   ;;(cons 'dominant-baseline "hanging")
			   (cons 'font-variant "small-caps"))
	      'label "Armor and Shields")
	(list 'armor 'pos (cons 1 6)
	      'text #'dm-character-describe-armor
	      'value #'dm-character-get-text)
	(list 'weapon-label 'pos (cons 32 5)
	      'style (list (cons 'font-size 3)
			   (cons 'font-variant "small-caps"))
	      'label "Weapons")
	(list 'weapon 'pos (cons 32 6)
	      'text #'dm-character-describe-weapon
	      'value #'dm-character-get-text)
	(list 'skills-label 'pos (cons 1 11)
	      'label "Skills and Stats"
	      'style (list (cons 'font-size 3)
			   (cons 'font-variant "small-caps")))
	(list 'skills 'pos (cons 1 12)
	      'text #'dm-character-describe-skill
	      'value #'dm-character-get-text)
	(list 'possession-label 'pos (cons 32 11)
	      'label "Possessions"
	      'style (list (cons 'font-size 3)
			   (cons 'font-variant "small-caps")))
	(list 'possession 'pos (cons 32 12)
	      'value #'dm-character-get-text)
	;;(list 'setting)
	;;(list 'use-counter)
	)
  "Groups of player character information.")

(defvar dm-character-party nil "Parties and intersectional character data.")

(defvar dm-character-party-collections
  (list 'elf nil
	'dwarf nil
	'human nil
	'heals nil
	'know-chamber nil
	'know-event nil)
  "Plist, attributes mapped to zero or more of `dm-characters'.")

(defvar dm-character--var 'dm-characters
  "Variable referenced by 'dm-character-set' and friends.")

(defvar dm-character-dom-attr-alist
  (list (cons 'font-size  1.7)
	(cons 'stroke  "black")
	(cons 'text-anchor "top")
	(cons 'dominant-baseline "hanging")
	;;(cons 'alignment-baseline "bottom")
	;;(cons 'dominant-baseline "hanging")
	(cons 'fill "black"))
  "Default SVG attributes for generated text elements.")

(defun dm-character-weapon-value-parse (value)
  "Return a vector [SWINGS PLUSES DAMAGE] from VALUE."
  (string-match (mapconcat 'identity
			   (list "^\\(?:\\\([-+.0-9]+\\\)@\\)?"
				 "\\(?:\\\([-+.0-9]+\\\)\\)?"
				 "\\(?:x\\\([-+.0-9]+\\\)\\)?$") "")
		value)
  (let ((atks (or (match-string 1 value) "0"))
	(pluses (or (match-string 2 value) "0"))
	(dmg (or (match-string 3 value) "1")))
    (vector (string-to-number atks)
	    (string-to-number pluses)
	    (string-to-number dmg))))

(defun dm-character-describe-skill (item)
  "Create description text from ITEM."
  (or (apply #'dm-character-get-attr
	     (append (dm-character-prop (plist-get item 'name) 'documentation)
		     (list 'documentation)))
      (dm-character-get-text dm-character-name 'attributes
			     (plist-get item 'name))))

(defun dm-character-describe-weapon (item)
  "Create description text from ITEM."
  (or (plist-get item 'documentation)
      (plist-get item 'note)
      (let* ((name (plist-get item 'name))
	     (cnt (string-to-number
		   (or (org-string-nw-p (plist-get item 'count)) "0")))
	     (val (dm-character-weapon-value-parse
		   (org-string-nw-p (plist-get item 'value))))
	     (treatment (org-string-nw-p (plist-get item 'treatment)))
	     (wielding (and treatment (string-match-p "(in-hand" treatment) t))
	     (atks (aref val 0))
	     (pluses (aref val 1))
	     (dmg (aref val 2)))
	;;(message "v:%s atks:%s pluses:%s dmg:%s" val atks pluses dmg)
	(format "%4s %11s%s%s"
		(if (> cnt 0) (format "(%2s)" cnt) " ")
		(if (> atks 1)
		    (if (> pluses 0)
			(if (> dmg 1) (format " %d atk %+d×%d " atks pluses dmg)
			  (format " %d atk %+d " atks pluses))
		      (if (> dmg 1) (format " %d atk ×%d " atks dmg)
			(format " %d attack " atks)))
		  (if (> pluses 0)
			(if (> dmg 1) (format " %+d×%d " pluses dmg)
			  (format " %+d " pluses))
		    (if (> dmg 1) (format " ×%d " dmg) "")))
		name
		(if wielding " (in hand)" "")))))

(defun dm-character-describe-armor (item)
  "Create description text from ITEM."
  (or (plist-get item 'documentation)
      (plist-get item 'note)
      (format "%s%s"
	      (let ((val (plist-get item 'value)))
		(if (org-string-nw-p val)
		    (let ((cnt (plist-get item 'count)))
		      (if (and (org-string-nw-p cnt) (not (equal cnt val)))
			  (format "%s/%s Hit " cnt val)
			(format "%s Hit " val)))
		  ""))
	      (replace-regexp-in-string
	       "[#].*$" ""
	       (mapconcat 'upcase-initials
			  (split-string (symbol-name (plist-get item 'name))
					"-") " ") t t))))

(defvar dm-character-grid nil "Layout for character sheets.")
(defvar dm-character-tiles nil "Tiles for character sheets.")
(defvar dm-character-grid-size (cons 53 96) "Grid size for character sheets.")
(defvar dm-character-grid-scale 33 "Pixels per dungeon unit for character sheets.")
(defvar dm-character-grid-nudge (cons 2 2) "Grid outer padding for character sheets.")

(defvar dm-map-level)
(defvar dm-map-tiles)
(defvar dm-map-nudge)
(defvar dm-map-scale)
(defvar dm-map-level-size)

(defun dm-character-buffer-name (&optional character)
  "Return name of buffer for CHARACTER or `dm-character-name'."
  (concat "**" (or dm-character-name
		   character
		   (error "No character for dm-character-buffer-name"))
	  "**"))

(defun dm-character-draw (&optional arg)
  "Draw character sheet for CHARACTER-NAME or `dm-character-name'.

With prefix ARG, reload first."
  (interactive "P")
  (let* ((dm-table-type 'character)
	 (dm-map-level (or dm-character-grid (dm-make-hashtable)))
	 (dm-map-tiles (or dm-character-tiles (dm-make-hashtable)))
	 (dm-map-tags t)
	 (dm-map-menus-level-cells-draw-all t)
	 dm-map-level-transform-detect-size
	 ;;dm-map-preview-buffer-name
	 dm-map-menus-play-mode)
    ;; prompt if no files or neg prefix arg
    (when (or arg (not (hash-table-p dm-character-tiles)))
      (unless (hash-table-p dm-character-grid)
	(setq dm-map-level (setq dm-character-grid (dm-make-hashtable)))
	(setq dm-map-tiles (setq dm-character-tiles (dm-make-hashtable))))
      (if (or (not dm-character-files) (equal '- arg))
	  (setq dm-character-files
		(completing-read-multiple
		 "Character Sheets (files, csv):"
		 (dm-files-select :character)
		 nil t ;; no prediecate, require match
		 (mapconcat 'identity dm-character-files ","))))
      (apply #'dm-table-extract-files dm-character-files)
      (let ((alists (dm-character-make-tiles)))
	(dm-alist-to-hash (car alists) dm-character-grid)
	(dm-alist-to-hash (cdr alists) dm-character-tiles))
      (cl-psetq dm-character-grid (dm-make-hashtable)
		dm-character-tiles (dm-make-hashtable)))

    ;; ZZZ drawing a "blank" character sheet isn't possible rn
    (when (null dm-character-name) (error "No character selected"))

    ;; try not to clober already localized settings
    (unless (local-variable-p 'dm-map-scale)
      (setq dm-map-preview-buffer-name (dm-character-buffer-name))
      (dm-map-pop-to-buffer
	(setq dm-map-scale dm-character-grid-scale)
	(setq dm-map-nudge dm-character-grid-nudge)
	(setq dm-map-level-size dm-character-grid-size)))

    (cl-psetq dm-character-grid dm-map-level
	      dm-character-tiles dm-map-tiles)

    (dm-map-draw)))

(defun dm-character-prop (name &optional attr &rest ignore-groups)
  "Return first NAME (skiping IGNORE-GROUPS) with ATTR."
  (let ((character
	 (seq-partition (plist-get dm-characters dm-character-name) 2))
	group rv)
    (while (and (null rv) (setq group (pop character)))
      (unless (and ignore-groups (member (car group) ignore-groups))
	(if (null attr)
	    (when (plist-member (cadr group) name)
	      (setq rv (list dm-character-name (car group) name)))
	  (when (plist-member (plist-get (cadr group) name) attr)
	    (setq rv (list dm-character-name (car group) name))))))
    rv))


(defun dm-character-make-label (label &optional pos class style &rest dom-attributes)
  "Create Lisp code to draw LABEL at POS.

When CLASS is nil, LABEL is used as a class name.  STYLE is
expected to be a list of cons cells as are DOM-ATTRIBUTES both of
which are combined with `dm-character-dom-attr-alist'."
  (if-let ((text (org-string-nw-p
		  (if (symbolp label) (symbol-name label) label))))
      (let ((class (if (null class) text
		     (if (symbolp class) (symbol-name class) class))))
	(list 'text (dm-ensure-dom-attr
		     (dm-merge-alists
		      (append (delq nil
				    (list (cons 'class class)
					  (when pos
					    (cons 'x (car pos))
					    (cons 'y (cdr pos)))))
			      dm-character-dom-attr-alist
			      dom-attributes
			      style)))
	      text))))

(defmacro dm-character-make-tile-name (parts)
  "Add POS and PARTS to LAYOUT."
  `(mapconcat (apply-partially 'format "%s") ,parts "-"))

(cl-defun dm-character-make-layouts
    (tile refs
	  &key
	  (x 0) (y 0)
	  (x-incr 0) (y-incr 2))
  "Return an alist of layouts from REFS and TILE.

Each of REFS is an n-length list of symbols naming a character
property.  X (or 0) is the starting x position.  Y (or zero) is
the starting y position.  X-INCR (or 0) is the amount to
increment x and Y-INCR (or 2) is the amount increment to y after
creating each."
  (message "refs:%s" refs)
  (let ((x x) (y y))
    (mapcar
     (lambda (prop)
       (message "make:%s" prop)
       (prog1 (append (list (cons x y) 'path (list tile))
		      (dm-merge-plists
		       (apply #'dm-character-get prop)
		       (delq
			nil
			(let ((prop-name (nth (length prop) prop)))
			  (mapcon
			   (lambda (attr)
			     (when-let ((val (dm-character-get-attr nil nil
							      prop-name attr)))
			       (list attr val)))
			   dm-character-tiles-default-props)))))
	 (setq x (+ x x-incr))
	 (setq y (+ y y-incr))))
     (remove nil refs))))

(defun dm-character-make-tiles ()
  "Initialize a tileset from `dm-character-tiles-alist'.

Return cons (LAYOUT . TILES) where LAYOUT and TILES are alists
TILES contain one entry per each of `dm-character-tiles-alist',
and LAYOUT has one entry for each entried returned by calling
`dm-character-find' on each car from `dm-character-tiles-alist'."
  (let* ((layout)
	 (tiles
	  (mapcan
	   (lambda (tilespec)
	     (when-let* ((tile (car tilespec))
			 (plist (cdr tilespec))
			 (pos (plist-get plist 'pos)))
	       (if-let ((value-func (plist-get plist 'value)))
		   (progn
		     (let* ((text-func (plist-get plist 'text))
			    (text (if text-func
				      (if (stringp text-func)
					  (format "$%s" text-func)
					"$text")
				    "$value"))
			    (refs (dm-character-find dm-character-name tile)))
		       (if text-func
			   (if (stringp text-func)
			       (format "$%s" text-func)
			     (message "not a string")
			     (dolist (ref (remove nil refs))
			       ;;(message "t:%s s:%s v:%v")
			       (dm-character-set-attr
				dm-character-name (nth 1 ref) (nth 2 ref) 'text
				(funcall text-func (apply #'dm-character-get ref)))))
			 (if-let ((value-func (plist-get plist 'value)))
			     (setq text "$text")
			     (dolist (ref (remove nil refs))
			       ;;(message "t:%s s:%s v:%v")
			       (dm-character-set-attr
				dm-character-name (nth 1 ref) (nth 2 ref) 'text
				(funcall value-func
					 dm-character-name
					 tile
					 (nth-value (- (length ref) 1) ref))))))
		       (mapc (lambda (cell) (push cell layout))
			     (dm-character-make-layouts tile refs
							:x (car pos)
							:y (cdr pos)))
		       (list (list tile 'path '(noop) 'overlay
				   (list
				    (dm-character-make-label
				     text
				     (cons (car pos) (cdr pos))
				     (plist-get plist 'class)
				     (or (plist-get plist 'style))))))))
		 (push
		  (append (list pos 'path (list tile))
			  (when-let ((args (dm-character-prop tile)))
			    (apply 'dm-character-get args)))
		  layout)
		 (list (list tile 'path '(noop) 'overlay
			     (list
			      (dm-character-make-label
			       (or (plist-get plist 'label) tile)
			       (cons (car pos) (cdr pos))
			       (plist-get plist 'class)
			       (or (plist-get plist 'style)))))))))
					;(cons layout dm-character-tiles-alist)
	   dm-character-tiles-alist)))
    (cons (reverse layout) tiles)))

(defun dm-character-find (character-name propset)
  "Return PROPSET for CHARACTER-NAME."
  (when-let ((character (plist-get (symbol-value dm-character--var)
				   character-name)))
    (mapcan
     (lambda (group)
       (if (equal (car group) propset)
	   (mapcan
	    (lambda (item)
	      (list (list character-name (car group) (car item))))
	    (seq-partition (cadr group) 2))
	 (let ((regex (concat "^" (symbol-name propset))))
	   (mapcan
	    (lambda (item)
	      (if-let ((treatment (plist-get (car (cdr item)) 'treatment)))
		  (when (string-match-p regex treatment)
		    (list (list character-name (car group) (car item))))
		(let ((item-name (symbol-name (car item))))
		  (when (string-match-p regex item-name)
		    (list (list character-name (car group) (car item)))))))
	    (seq-partition (cadr group) 2)))))
     (seq-partition character 2))))

(defun dm-character-set (character-name
			 propset
			 prop
			 &rest props)
  "Set PROP in PROPSET to PROPS for CHARACTER-NAME."
  (set dm-character--var
       (plist-put (symbol-value dm-character--var) character-name
		   (if-let ((character (plist-get dm-characters
						  character-name)))
		       (if-let ((plist (plist-get character propset)))
			   (plist-put character propset
				      (plist-put plist prop props))
			 (plist-put character propset
				    (plist-put nil prop props)) )
		     (plist-put nil propset
				(plist-put nil prop props))))))
;; (mapcar (apply-partially 'apply 'dm-character-get-text)
;; 	(dm-character-find dm-character-name 'skills))

(defun dm-character-set-attr (character-name propset prop attr value)
  "Set ATTR of PROP in PROPSET to VALUE for CHARACTER-NAME."
  (when-let* ((item (dm-character-get character-name propset prop))
	      (new-item (plist-put item attr value)))
    (message "item:%s" new-item)
    (apply #'dm-character-set (append (list character-name propset prop)
				      new-item))))

(defun dm-character-get (character-name propset prop)
  "Get PROP from PROPSET of CHARACTER-NAME."
  (when-let ((character (plist-get dm-characters character-name)))
      (when-let ((plist (plist-get character propset)))
	(plist-get plist prop))))

(defun dm-character-get-attr (character-name propset prop &optional attr)
  "Get ATTR (or \"value\") of PROP from PROPSET of CHARACTER-NAME."
  (if (null attr)
      (plist-get (dm-character-get character-name propset prop) 'value)
    (when-let ((ref (dm-character-prop prop attr)))
      (plist-get (apply #'dm-character-get ref) attr))))

(defun dm-character-get-text (character-name propset prop)
  "Get \"note\" or \"documentation\" or \"value\" of PROP.

Searches PROPSET of CHARACTER-NAME."
  (or (org-string-nw-p (dm-character-get-attr nil nil prop 'text))
      (org-string-nw-p (dm-character-get-attr nil nil prop 'notes))
      (org-string-nw-p (dm-character-get-attr nil nil prop 'documentation))
      (dm-character-get-attr nil nil prop 'name)))

(defun dm-character-transform (table)
  "Transform a TABLE of strings into an hash-map."
  (let* ((cols (seq-map (lambda (label) (intern (downcase label)))
			(car table)))
	 (character-name (if (not (string= "name" (caadr table)))
			     (or dm-character-name
				 (error "Cannot find character name"))
			   (setq dm-character-name (nth 2 (cadr table)))))
	 (first-col-sym (car cols)))
    (mapc
     (lambda (row)
       (apply
	#'dm-character-set
	(append (list dm-character-name first-col-sym (intern (car row)))
		(append (list 'name (intern (car row)))
			(apply #'append
			       (seq-map-indexed
				(lambda (col ix) (list col (nth (1+ ix) row)))
				(cdr cols)))))))
     (cdr table))))

(defun dm-character-tiles-make ()
  "Create tiles from propsets."
  (let ((hash (dm-make-hashtable)))
    (mapc)))

;; DEVEL: give us a global key binding precious
(global-set-key (kbd "<f7>") 'dm-character-draw)

(defvar dm-table-load-function)
(let (dm-table-load-function) ;; no Load function
  (dm-table-defstates 'character :transform #'dm-character-transform))


;; test case
;; (let ((dm-table-type 'character))
;;   (setq dm-characters nil) ;; (dm-make-hashtable)
;;   (apply #'dm-table-extract-files dm-character-files))
;;(dm-character-make-layouts 'name (list (list dm-character-name 'intrinsic 'name)(list dm-character-name 'intrinsic 'name)) :x 10 :y 10)
;;(progn (setq foo nil)(cl-psetq dm-map-level (dm-make-hashtable) dm-map-tiles (dm-make-hashtable)) (let (eval-expression-print-length eval-expression-print-level print-level print-length) (print (setq foo (dm-character-make-tiles))) (dm-character-draw)))

(provide 'dm-character)
;;; dm-character.el ends here
