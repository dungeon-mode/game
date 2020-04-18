;;; dm-map.el --- render dungeon-mode map as SVG     -*- lexical-binding: t; -*-

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
;; * Implementation

;; This file implements SVG rendering of maps for, e.g.
;; [[https://github.com/mplsCorwin/dungeon-mode][dungeon-mode]]
;; a multi-player dungeon-crawl style RPG for
;; [[https://www.fsf.org/software/emacs][GNU Emacs]]
;; See [[Docs/Maps][Docs/Maps]] for example map definations from the
;; sample dungeon.
;; ** Overview

;;  * Implement ~dm-map~ as an EIEIO class having two slots:
;;    * an [[https://www.gnu.org/software/emacs/manual/html_node/elisp/SVG-Images.html][~svg~]] object containing all graphic elements except the
;;      main path element
;;    * path-data for the main path element as a list of strings
;; ** Cursor Drawing using the [[https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths][SVG path element]]

;; Dungeon uses Scalable Vector Graphic (SVG)
;; [[https://www.w3.org/TR/SVG/paths.html][path element]] to show maps
;; within Emacs using a simple cursor based drawing approach similar
;; to
;; [[https://en.wikipedia.org/wiki/Logo_(programming_language)][Logo]]
;; (e.g. [[https://github.com/hahahahaman/turtle-geometry][turtle
;; graphics]]).  By concatenating all of the required draw
;; instructions for the visible features of the map (along with
;; suitable fixed-address based movement instructions between) we can
;; add most non-text elements within a single path.

;; This imposes limitations in terms, for example, of individually
;; styling elements such as secret doors (drawn in-line, currently) but
;; seems a good starting point in terms of establishing a baseline for
;; the performance rendering SVG maps on-demand within Emacs.

;;; Requirements:

(eval-when-compile (require 'eieio)
		   (require 'cl-lib)
		   (require 'subr-x)
		   ;;(require 'subr)
		   )

(require 'org-element)

;; DEVEL hack: prefer version from CWD, if any
(let ((load-path (append '(".") load-path)))
  (require 'dm-util)
  (require 'dm-svg)
  (require 'dm-table))

;;; Code:

(defgroup dm-map nil "Game map settings." :group 'dungeon-mode)
(defcustom dm-map-files '() "List of files from which load game maps." :type (list 'symbol))
(defcustom dm-map-property "ETL" "Property to insepect when finding tables." :type 'string)

(defvar dm-map-table-var-alist '((cell dm-map-level)
				 (tile dm-map-tiles))
  "Alist mapping table types to variables storing an hash-table for each.")

(defvar dm-map-level nil "Hash-table; draw code for game map levels.")
(defvar dm-map-level-cols '(x y path) "List of colums from feature tables.")
(defvar dm-map-level-key '(cons (string-to-number x) (string-to-number y))
  "Expression to create new keys in `dm-map-tiless'.")

(defvar dm-map-tiles nil
  "Hash-table; draw code for reusable `dungeon-mode' map features.")

(defvar dm-map-tiles-cols nil "List of colums from tile tables.")

(defvar dm-map-current-level nil
  "Hash-table; draw code for the current game map.")

(defvar dm-map-draw-prop 'path
  "Default column/property for the base draw path.

Used during level/cell transform as well as during map rendering.
See also: `dm-map-draw-other-props'")

(defvar dm-map-draw-other-props '(stairs water beach neutronium decorations)
  "List of additional columns that may contain SVG path fragments.")

(defvar dm-map-overlay-prop 'overlay
  "Default column/property for SVG elements overlaying maps.")

(defvar dm-map-tag-prop 'tag
  "Default attributes for cell tags as they are loaded.

Tags (e.g. \":elf\") allow conditional inclusion of draw code.")

(defvar dm-map-draw-attributes-default '((fill . "none")
					 (stroke . "green")
					 (stroke-width . "1"))
  "Default attributes for the main SVG path used for corridor, etc.")

(defvar dm-map-draw-attributes  `(,dm-map-draw-prop
				  ,dm-map-draw-attributes-default
				  ,@(mapcan (lambda (x) (list x nil))
					    dm-map-draw-other-props))
  "Default attributes for SVG path elements, a plist.")

(defvar dm-map-cell-defaults `(,dm-map-draw-prop nil
			       ,dm-map-tag-prop nil
			       ,dm-map-overlay-prop nil
			       ,@(mapcan (lambda (x) (list x nil))
					 dm-map-draw-other-props))
  "Default attributes for cells or tiles as they are loaded.")

(defvar dm-map-extra-tiles '('background 'graphpaper)
  "Extra tiles to include when rendering a level.")

(defvar dm-map-tags '()
  "List of keyword symbols to include from tile paths.")

(defvar dm-map-tile-tag-re "^\\(.*\\)\\([:]\\([!]\\)?\\(.*?\\)\\)$"
                        ;;     1 base  2 lit 3 invt-p  4 TAG
  "Regex used to parse tags in tile symbols.

Capture groups:
  1. Basename, e.g. reference tile.
  2. Tag literal including seperator, empty when tile is not tagged.
  3. Inversion/negation indicator, if any (e.g. \"!\").
  4. Tag, e.g. \"elf\" from \"secret-door:elf\".")

(defmacro dm-map-cell-defaults ()
  "Return an empty map cell plist."
  `(list ',dm-map-draw-prop nil
	 ',dm-map-tag-prop nil
	 ',dm-map-overlay-prop nil
	 ,@(mapcan (lambda (x) (list `(quote ,x) nil))
		   dm-map-draw-other-props)))

(defun dm-map-tile-tag-inverted-p (tile)
  "Return t when TILE has an inverted.tag.

Inverted tags look like :!, such as \"SYMBOL:!KEYWORD\".
TODO: this can be greatly simplified. macro/inline/remove?"
  (when-let* ((ref (and (string-match dm-map-tile-tag-re tile)
			(match-string 1 tile)))
	      (kw (match-string 2 tile))
	      (tag (intern kw))
	      (ref-sym (intern ref))
	      (referent (gethash ref-sym dm-map-tiles)))
    ;;(message "[tile-xform] tag %s ⇒ %s (target: %s)" tag ref-sym referent)
    (when ;;(and (< 1 (length kw)) (string= "!" (substring kw 1 2)))
	(match-string 3 tile)
      t)))
;;(equal '(t nil) (seq-map (lambda(x) (dm-map-tile-tag-inverted-p x)) '("◦N:!elf" "◦N:elf")))

;; ZZZ are this and the above really needed? corwin
(defun dm-map-tile-tag-basename (tile)
  "Return the basename prefixing any keywords in TILE, a symbol."
  (and (string-match dm-map-tile-tag-re tile)
       (match-string 1)))

;; TODO write dm-map-tile-tag-maybe-invert ?
(defun dm-map-tile-tag-maybe-invert (tile)
  "Return tags for TILE.

Select amung normal and inverted paths based on `dm-map-tags', which see."
  (delq nil (mapcan
	     (lambda (tag)
	       (list (if (or (eq t dm-map-tags)
			     (member (car tag) dm-map-tags))
			 (and (< 1 (length tag)) (nth 1 tag))
		       (and (< 2 (length tag)) (nth 2 tag)))))
	     (plist-get (gethash tile dm-map-tiles) dm-map-tag-prop))))
;;(equal '((◦N:elf) (◦N:elf) (◦N:!elf)) (mapcar (lambda (ts) (let ((dm-map-tags ts)) (dm-map-tile-tag-maybe-invert '◦N))) '(t (:elf) nil)))

(defsubst dm-map-header-to-cols (first-row)
  "Create column identifier from FIRST-ROW of a table."
  (seq-map
   (lambda (label)
     (intern (downcase label)))
   first-row))

(cl-defsubst dm-map-table-cell (cell
				&optional &key
				(table dm-map-level)
				(defaults (dm-map-cell-defaults)))
  "Return value for CELL from TABLE or a default."
  (if (and cell table)
      (gethash cell table defaults)
    defaults))

(cl-defsubst dm-map-path (cell
			   &optional &key
			   (table dm-map-level table)
			   (prop dm-map-draw-prop))
  "When CELL exists in TABLE return PROP therefrom.

CELL is an hash-table key, TABLE is an hash-table (default:
`dm-map-level'), PROP is a property (default: \"path\")."
  (if (and (hash-table-p table) (gethash cell table))
      (plist-get (gethash cell table) prop))
  nil)

(defsubst dm-map-parse-plan-part (word)
  "Given trimmed WORD of a plan, DTRT.

TODO implement leading & as a quoting operator for tile refs"
  ;;(message "[parse] word:%s type:%s" word (type-of word))
  (delq nil (nconc
	     (when (org-string-nw-p word)
	       (cond ((string-match-p "<[^ ]\\|>[^ ]\\|[^ ]<\\|[^ ]>" word)
		      (list word))
		     ((string-match-p "[()]" word)
		      (list (read word)))
		     ((string-match-p "[ \t\n\"]" word)
		      (mapcan 'dm-map-parse-plan-part
			      (split-string word nil t "[ \t\n]+")))
		     (;; TODO: RX ?M ?m ?V ?v ?H ?h ?S ?s ?A ?a ?L ?l
		      (string-match-p "^[MmVvHhSsAaL][0-9.,+-]+$" word)
		      (list (list (intern (substring word 0 1))
				  (mapcar 'string-to-number
					  (split-string (substring word 1) "[,]")))))
		     (t (list (intern (if (and (< 1 (length word))
					       (string-match-p "^[&]" word))
					  (substring word 1)
					word)))))))))


(cl-defmacro dm-map-tile-path (tile &optional &key (prop dm-map-draw-prop))
  "When TILE exists in `dm-map-tiles' return PROP therefrom.

TILE is an hash-table key, PROP is a property (default: \"path\"."
  `(dm-map-path ,tile :table dm-map-tiles :prop ',prop))

(defmacro dm-map-draw-test (svg &rest body)
  "Open (erase) buffer evaluate BODY and dump SVG."
  (declare (indent 1))
  `(with-current-buffer (pop-to-buffer "**dm-map**")
     (dm-map-mode)
     (erase-buffer)
     (goto-char (point-min))
     (insert (format-time-string "%D %-I:%M:%S %p on %A, %B %e, %Y\n"))
     ,@body
     ;;(insert (format "\n\n[SVG CODE]:\n%s" (dom-pp (oref  svg))))
     (insert (format "\n\n[SVG DUMP]:\n%s" ,svg))
     (beginning-of-buffer)))

(defun dm-map-load (&rest files)
  "Truncate and replace `dm-map-tiless' and `dm-map-levels' from FILES.

Kick off a new batch for each \"feature\" or \"level\" table found."
  (dolist (var dm-map-table-var-alist)
    (set (cadr var) (dm-make-hashtable)))
  (list nil
	(seq-map
	 (lambda (this-file)
	   (message "[map-load] file:%s" this-file)
	   (with-temp-buffer
	     (insert-file-contents this-file)
	     ;; (org-macro-initialize-templates)
	     ;; (org-macro-replace-all org-macro-templates)
	     (org-element-map (org-element-parse-buffer) 'table
	       (lambda (table)
		 (save-excursion
		   (goto-char (org-element-property :begin table))
		   (forward-line 1)
		   (when-let* ((tpom (save-excursion (org-element-property :begin table)))
			       (type (org-entry-get tpom dm-map-property t))
			       (type (intern type))
			       (var (cadr (assoc type dm-map-table-var-alist 'equal))))
		     ;;(when (not (org-at-table-p)) (error "No table at %s %s" this-file (point)))
		     ;;(let ((dm-table-load-table-var var)))
		     (dm-table-batch type)))))))
	 (or files dm-map-files))))

(defvar dm-map--last-plist nil
  "While transforming tile tables, the last plist created.")
(defvar dm-map--xml-strings nil
  "While transforming tile tables, unparsed XML strings.")
(defun dm-map--xml-parse ()
  "Add parsed XML to PLIST and clear `dm-map--xml-strings."
  (prog1 (when (and dm-map--last-plist dm-map--xml-strings)
	    (plist-put dm-map--last-plist dm-map-overlay-prop
		       (append (plist-get dm-map--last-plist
					  dm-map-overlay-prop)
			       (with-temp-buffer
				 (insert (mapconcat 'identity (nreverse dm-map--xml-strings) ""))
				 (message "[tile-xform] XML %s" (buffer-string))
				 (libxml-parse-xml-region (point-min)
							  (point-max))))))
    (setq dm-map--xml-strings nil)))

(defun dm-map-level-transform (table)
  "Transform a TABLE of strings into an hash-map."
  (let* ((cols (or dm-map-level-cols
		   (seq-map (lambda (label) (intern (downcase label)))
			    (car table))))
	 (cform
	  `(dm-coalesce-hash ,(cdr table) ,cols
	     ,@(when dm-map-level-key `(:key-symbol ,dm-map-level-key))
	     :hash-table dm-map-level
	     (when (and (boundp 'path) path)
	       (list 'path (dm-map-parse-plan-part path)))))
	 (result (eval `(list :load ,cform))))))

(defun dm-map-tiles-transform (table)
  "Transform a TABLE of strings into an hash-map."
  (let* ((cols (or dm-map-tiles-cols (dm-map-header-to-cols (car table))))
	 (last-key (gensym))
	 (cform
	  `(dm-coalesce-hash ,(cdr table) ,cols
	     :hash-table dm-map-tiles
	     :hash-symbol hash
	     :key-symbol last-key
	     (progn
	       (when (and (boundp 'tile) (org-string-nw-p tile))
		 ;; This row includes a new tile-name; process any saved up
		 ;; XML strings before we overwrite last-key
		 (dm-map--xml-parse)

		 (setq last-key (intern tile)
		       dm-map--last-plist (dm-map-table-cell tile :table hash))
		 (when (string-match dm-map-tile-tag-re tile)
		   ;; Tile name contains a tag i.e. "some-tile:some-tag".
		   ;; Ingore inverted tags ("some-time:!some-tag"). Add others
		   ;; in last-past as alist of: (KEYWORD TAG-SYM INV-TAG-SYM)
		   (when-let* ((ref (match-string 1 tile))
			       (kw (match-string 2 tile))
			       (tag (intern kw))
			       (ref-sym (intern ref))
			       (referent (gethash ref-sym hash)))
;;;(message "[tile-xform] tag %s ⇒ %s (target: %s)" tag ref-sym referent)
		     (unless (match-string 3 tile)
		       (plist-put referent ',dm-map-tag-prop
				  (append
				   (plist-get referent ',dm-map-tag-prop)
				   (list
				    (list tag last-key
					  (intern (concat (match-string 1 tile)
							  ":!" (match-string 4 tile)))))))))))
	       (mapc (lambda (prop)
		       (when (boundp `,prop)
			 (plist-put dm-map--last-plist `,prop
				    (nconc (plist-get dm-map--last-plist `,prop)
					   (dm-map-parse-plan-part (symbol-value `,prop))))))
		     (delq nil'(,dm-map-draw-prop ,@dm-map-draw-other-props)))
	       (when (and (boundp (quote ,dm-map-overlay-prop)) ,dm-map-overlay-prop)
		 (push ,dm-map-overlay-prop dm-map--xml-strings))
;;;(message "[tile-xform] %s ⇒ lkey:%s lpath:%s" tile last-key dm-map--last-plist)
	       dm-map--last-plist)))
	 (result
	  (prog1
;;;(message "[tile-xform] macro ⇒ %s" (prin1-to-string cform))
	      (eval `(list :load ,cform))
	    ;; process any overlay XML from last row
	    (dm-map--xml-parse))))))

(dm-table-defstates 'map :extract #'dm-map-load)
(dm-table-defstates 'tile :transform #'dm-map-tiles-transform)
(dm-table-defstates 'cell :transform #'dm-map-level-transform)


;; quick and dirty procedural approach

(cl-defun dm-map--dom-attr-scale-nth (dom-node scale (attr n))
  "Apply Nth of SCALE to ATTR of DOM-NODE if present."
  (when (numberp (car (dom-attr dom-node attr)))
    (dom-set-attribute dom-node attr (* (nth n scale)
					(car (dom-attr dom-node attr))))))
;; (dm-map--dom-attr-scale-nth (dom-node 'foo '((bar 1))) '(2 3 4) '(bar 0))
;; second test case to figure out looping a list
;; (let ((dom-node (dom-node 'foo '((text-size 1) (y 1)))))
;;   (dolist (args '((text-size 0) (x 0) (y 1)) dom-node)
;;     (dm-map--dom-attr-scale-nth dom-node '(12 23) args))
;;   dom-node)

(defun dm-map-default-scale-function (scale &rest cells)
  "Return CELLS with SCALE applied.

SCALE is the number of pixes per map cell, given as a cons cell:

  (X . Y)

CELLS may be either svg `dom-nodes' or cons cells in the form:

  (CMD . (ARGS))

Where CMD is an SVG path command and ARGS are arguments thereto.

SCALE is applied to numeric ARGS of cons cells and to the width,
height and font-size attributes of each element of each
`dom-node' which contains them in the parent (e.g. outter-most)
element.  SCALE is applied to only when the present value is
between 0 and 1 inclusive."
  ;;(message "scale:%s cells:%s" scale cells)
  (let ((cells (copy-tree cells)))
    (dolist (cell cells cells)
      ;;(message "cell:%s" cell)
      (cond
       ((dm-svg-dom-node-p cell 'text)
	;;(message "text:%s" cell)
	(dolist (args '((text-size 0) (x 0) (y 1)) )
	  (dm-map--dom-attr-scale-nth cell scale args)))
       ((consp cell) ;;(message "consp!%s" cell)
	(pcase cell
	  ;; move or line in the form (sym (h v)
	  (`(,(or 'm 'l)
	     (,(and x (guard (numberp x)))
	      ,(and y (guard (numberp y)))))
	   (setcdr cell (list (* x (car scale))
			      (* y (cdr scale)))))

	  ;; h and v differ only in which part of scale applies
	  (`(h (,(and d (guard (numberp d)))))
	   (setcdr cell (list (* d (car scale)))))
	  (`(v (,(and d (guard (numberp d)))))
	   (setcdr cell (list (* d (cdr scale)))))

	  ;; arc has tons of args but we only mess with the last two
	  (`(a (,rx ,ry ,x-axis-rotation ,large-arc-flag ,sweep-flag
		    ,(and x (guard (numberp x)))
		    ,(and y (guard (numberp y)))))
	   (setcdr cell (list rx ry x-axis-rotation large-arc-flag sweep-flag
			      (* x (car scale))
			      (* y (cdr scale)))))

	  ;; fall-back to a message
	  ;;(_ (message "unhandled %s => %s" (type-of cell) cell))
	  ))
       ))))

;;(dm-map-default-scale-function '(100 . 1000) (dom-node 'text '((text-size .5)(x .2))) '(h (.1)) '(v (.2))'(m (.3 .4)) '(l (.5 .6)) '(x (.7 .8)) '(a (0.05 0.05 0 1 1 -0.1 0.1)))

;; (a (.7 .7 0 1 1 0 .14))
;; (a (0.05 0.05 0 1 1 -0.1 0.1))

;; image scale
;;;;;;;;;

;; simple draw methods for testing

(defun dm-map-resolve (tile)
  "Get draw code for TILE."
  (when-let* ((plist (gethash tile dm-map-tiles))
	      (paths (append
		      (plist-get plist dm-map-draw-prop)
		      (dm-map-tile-tag-maybe-invert tile))))
    (mapcan (lambda (stroke)
	      (let ((stroke stroke))
;;;(message "[resolver] stroke:%s (type: %s)" stroke (type-of stroke))
		(if (symbolp stroke)
		    (dm-map-resolve stroke)
		  (list stroke)))) paths)))
;;(not (equal (dm-map-resolve '◦N) (let ((dm-map-tags t)) (dm-map-resolve '◦N))))


(defun dm-map-resolve-cell (cell &optional no-follow)
  "Get draw code for CELL.

Follow references unless NO-FOLLOW is truthy."
  (when-let* ((plist (gethash cell dm-map-level))
	      (paths (plist-get plist dm-map-draw-prop)))
    (mapcan (lambda (stroke)
	      (let ((stroke stroke))
;;;(message "[resolver] stroke:%s (type: %s)" stroke (type-of stroke))
		(pcase stroke
		  ;; ignore references when drawing a complete level
		  (`(,(and x (guard (numberp x))). ,(and y (guard (numberp y))))
		   (if no-follow
		       nil ;;(message "[draw] ignored ref %s to %d,%d" cell x y)
		     (dm-map-resolve-cell stroke)))
		  ((pred stringp)
		   (message "[resolve] ignoring svg: %s" stroke))
		  ((pred symbolp) (dm-map-resolve stroke))
		  ;;(t (message "[draw] TODO %s" path-list))
		  (_ (list stroke)))
		;;(if (symbolp stroke)   (dm-map-resolve stroke) (list stroke))
		))
	    paths)))

(defun dm-map-to-string (&rest cells)
  "Return CELLS as a single string."
  ;;(message "[to-string] cells:%s" cells)
  (when (listp cells)
    (apply 'concat
	   (mapcan
	     (lambda (stroke)
	       ;;(message "[to-string] stroke:%s (%s)" (prin1-to-string stroke) (type-of stroke))
	       (if (consp stroke)
		   (mapcar (apply-partially 'format "%s ") stroke)
		 (format "%s " stroke) ;; (list stroke)
		 ;;(message "unhandled %s => %s" (type-of cell) cell)
		 ))
	     cells))))

(cl-defun dm-map-quick-draw (&optional
			     (cells (when dm-map-level (hash-table-keys dm-map-level)))
			     &key
			     path-attributes
			     (scale 100)
			     (scale-function #'dm-map-default-scale-function)
			     (tiles dm-map-tiles)
			     (level dm-map-level)
			     (size '(2400 . 1700))
			     ;;(svg (dm-svg))
			     (svg-attributes '(:stroke-color white
					       :stroke-width 1))
			     ;;viewbox
			     &allow-other-keys)
  "No-frills draw routine.

CELLS is the map cells to draw defaulting to all those found in
`dm-map-level'.  TILES is an hash-table containing draw code and
documentation, defaulting to `dm-map-tiles'.  SCALE sets the
number of pixels per map cell.  SIZE constrains the rendered
image canvas size.  VIEWBOX is a list of four positive numbers
controlling the X and Y origin and displayable width and height.
SVG-ATTRIBUTES is an alist of additional attributes for the
outer-most SVG element.  LEVEL allows override of level info.

PATH-ATTRIBUTES is an alist of attributes for the main SVG path
element.  The \"d\" property provides an initial value to which
we append the drawing instructions to implement reusable tiles.
These may be referenced in other tiles or by map cells in
sequence or mixed with additional SVG path instructions.  For
this to work tiles must conclude with movement commands to return
the stylus to the origin (e.g. M0,0) as it's final instruction.

SCALE-FUNCTION may be used to supply custom scaling."
  (ignore tiles cells scale size path-attributes scale-function)
  (let* ((img (dm-svg :svg (apply 'svg-create
				  (append (list (car size) (cdr size))
					  svg-attributes))))
	 (pstr (apply 'dm-map-to-string
		      (apply (apply-partially scale-function
					      (cons scale scale))
			     (apply
			      'append
			      (delq
			       nil
			       (seq-map (lambda(x)
					  (append (list
						   (list 'M
							 (* scale (car x))
							 (* scale (cdr x))))
						  (dm-map-resolve-cell x t)))
					cells)))))))
    ;;(message "[draw] path:%s" pstr)
    (add-path-data img pstr)
    img))

;; => drop in some SVG path code for testing
;; (let* ((svg (svg-create 800 800 :stroke-color 'green :stroke-width 10))
;;        (path '(path (""))
;;       (pelm  (dm-svg-create-path path)))
;;   ;;(svg-circle svg 200 200 100 :stroke-color 'red)
;;   (dom-append-child svg pelm)
;;   (dm-map-draw-test svg (put-image (svg-image svg) (point-max))))

;; => basic test of quick-draw but manually add an SVG element
;; (let ((svg (dm-map-quick-draw)))
;;   (dm-map-draw-test svg
;;     (add-svg-element svg (dom-node 'circle '((cx . 200) (cy . 200)
;; 					     (r . 100) (stroke . red)
;; 					     (stroke-width . 10))))
;;     (render-and-insert svg)))



(defun dm-map-draw (&optional arg)
  "Draw all from `dm-map-level'.  With prefix ARG, reload first."
  (interactive "P")
  (when arg (if dm-map-files (dm-map-load)
	      (user-error "Draw failed.  No files in `dm-map-files'")))
  (let ((svg (dm-map-quick-draw)))
    (dm-map-draw-test (oref svg path-data)
      (render-and-insert svg))))

;; global key binding
(global-set-key (kbd "<f9>") 'dm-map-draw)

;; basic major mode for viewing maps
(defvar dm-map-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'kill-buffer-and-window)
    (define-key map (kbd "x") 'kill-buffer)
    map))

(define-derived-mode dm-map-mode fundamental-mode "MAP"
  "Major mode for `dugeon-mode' maps.")

;; (setq dm-map-files '("d:/projects/dungeon-mode/Docs/Maps/map-tiles.org"
;; 		     "d:/projects/dungeon-mode/Docs/Maps/testmap.org"))
;;(setq dm-map-tags t)

(provide 'dm-map)
;;; dm-map.el ends here
