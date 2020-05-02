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
;; the performance rendering SVG maps on-demand within Emacs.cl-defsubst

;;; Requirements:

(eval-when-compile (require 'eieio)
		   (require 'cl-lib)
		   (require 'subr-x)
		   (require 'pixel-scroll)
		   ;;(require 'subr)
		   )

(require 'image-mode)
(require 'org-element)

;; DEVEL hack: prefer version from CWD, if any
(let ((load-path (append '(".") load-path)))
  (require 'dungeon-mode)
  (require 'dm-util)
  (require 'dm-svg)
  (require 'dm-table))

;;; Code:

(defgroup dm-map nil
  "Mapping settings.

These settings control display of game maps."
  :group 'dungeon-mode)

(defcustom dm-map-files nil
  ;; (append (dm-files-select :map-tiles)
  ;; (dm-files-select :map-cells))
  "List of files from which load game maps."
  :type (list 'symbol)
  :group 'dm-files
  :group 'dm-map)

(defcustom dm-map-property "ETL"
  "Property to insepect when finding tables." :type 'string)

(defcustom dm-map-scale 100
  "Number of pixes per \"Dungeon Unit\" when mapping.

This setting controls the number of actual screen pixes
assigned (in both dimensions) while drwaing the map."
  :type 'number)

(defcustom dm-map-nudge `(,dm-map-scale . ,dm-map-scale)
  "Top and left padding in pixels as a cons cell (X . Y)."
  :type 'cons)

(defcustom dm-map-scale-nudge 5
  "Map scale nudge factor in pixels.

Controls amount by which func `dm-map-scale' adjusts var
`dm-map-scale' when called with a prefix argument."
  :type 'integer)

(defcustom dm-map-scroll-nudge 2
  "Map scrolling nudge factor in pixels."
  :type 'integer)

(defcustom dm-map-background t
  "List of SVG `dom-node's or t for dynamic background.

Set to nil to suppress adding any background elements."
  :type 'list)

(defvar dm-map-table-var-alist '((cell dm-map-level)
				 (tile dm-map-tiles))
  "Alist mapping table types to variables storing an hash-table for each.")

(defvar dm-map-level nil "Hash-table; draw code for game map levels.")

(defvar dm-map-level-cols '(x y path) "List of colums from feature tables.")

(defvar dm-map-level-key '(when (and (boundp 'x) (boundp' y))
			    (cons (string-to-number x) (string-to-number y)))
  "Expression to create new keys in `dm-map-tiless'.")

(defvar dm-map-level-size '(24 . 24)
  "Map size in dungeon units as a cons cell in the form:

  (HEIGHT . WIDTH)")

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
					 (stroke . "black")
					 (stroke-width . "1"))
  "Default attributes for the main SVG path used for corridor, etc.")

(defvar dm-map-draw-other-attributes-default
  '((water ((fill . "blue")
	    (stroke . "none")
	    (stroke-width . "1")))
    (beach ((fill . "yellow")
	    (stroke . "none")
	    (stroke-width . "1")))
    (stairs ((fill . "#FF69B4") ;; pink
	     (stroke . "none")
	     (stroke-width . "1")))
    (neutronium ((fill . "orange")
		 (stroke . "orange")
		 (stroke-width . "1")))
    (decorations ((fill . "cyan")
		  (stroke . "cyan")
		  (stroke-width . "1"))))
  "Default attributes for other SVG paths used, stairs, water, etc.")

(defvar dm-map-draw-attributes
  `(,dm-map-draw-prop
    ,dm-map-draw-attributes-default
    ,@(mapcan (lambda (x)
		(assoc x dm-map-draw-other-attributes-default))
	      dm-map-draw-other-props))
  "Default attributes for SVG path elements, as a plist.")

(defvar dm-map-cell-defaults `(,dm-map-draw-prop nil
			       ,dm-map-tag-prop nil
			       ,dm-map-overlay-prop nil
			       ,@(mapcan (lambda (x) (list x nil))
					 dm-map-draw-other-props))
  "Default attributes for cells or tiles as they are loaded.")

(defvar dm-map-extra-tiles '('Background 'Graphpaper)
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

(defvar dm-map--scale-props '((font-size 0) (x 0) (y 1))
  "SVG element properties and scale association.

Scale association may be 0 to use x scaling factor, or 2 for y.")

(defvar dm-map--last-cell nil "While resolving the last cell resolved.")

(defvar dm-map--last-plist nil
  "While transforming tile tables, the last plist created.")

(defvar dm-map--xml-strings nil
  "While transforming tile tables, unparsed XML strings.")


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

Select each amung normal and inverted based on `dm-map-tags'."
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
  (mapcar
   (lambda (label)
     (intern (downcase label)))
   first-row))


(cl-defun dm-map-table-cell (cell
				&optional &key
				(table dm-map-level)
				(defaults (dm-map-cell-defaults)))
  "Return value for CELL from TABLE or a default."
  (if (and cell table)
      (gethash (if (symbolp cell) cell (intern cell)) table defaults)
    defaults))

(cl-defsubst dm-map-path (cell
			   &optional &key
			   (table dm-map-level table)
			   (prop dm-map-draw-prop))
  "When CELL exists in TABLE return PROP therefrom.

CELL is an hash-table key, TABLE is an hash-table (default:
`dm-map-level'), PROP is a property (default: \"path\")."
  (if (hash-table-p table) (plist-get (gethash cell table) prop))
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
  "Open (erase) buffer evaluate BODY, dump and return SVG."
  (declare (indent 1))
  `(with-current-buffer (pop-to-buffer "**dungeon map**")
     (let ((image-transform-resize 1))
       (cl-letf (((symbol-function 'message) #'format)
		 ;;((symbol-function 'image-fit-to-window) (lambda (_) nil))
		 )
	 (when (image-get-display-property)
	   (image-mode-as-text)
	   (erase-buffer)
	   (beginning-of-buffer))
	 ;;(let ((inhibit-read-only t)))
	 ;;(insert (format-time-string "%D %-I:%M:%S %p on %A, %B %e, %Y\n"))
	 ;;(insert (format "\n\n[SVG CODE]:\n%s" (dom-pp (oref  svg))))
	 ;;(insert (format "\n\n[SVG DUMP]:\n%s" ,svg))
	 ,@body
	 (dm-map-mode)
	 ;;(pixel-scroll-mode t)
	 ;;(image-transform-set-scale 1)
	 (image-toggle-display-image)
	 ))))

(defun dm-map-load (&rest files)
  "Truncate and replace `dm-map-tiless' and `dm-map-levels' from FILES.

Kick off a new batch for each \"feature\" or \"level\" table found."
  (dolist (var dm-map-table-var-alist)
    (set (cadr var) (dm-make-hashtable)))
  (list nil
	(seq-map
	 (lambda (this-file)
	   (dm-msg :file "dm-map" :fun "load" :args (list :file this-file))
	   (with-temp-buffer
	     (insert-file-contents this-file)
	     (org-macro-initialize-templates)
	     (org-macro-replace-all org-macro-templates)
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

(defun dm-map--xml-attr-to-number (dom-node)
  "Return DOM-NODE with attributes parsed as numbers.

Only consider attributes listed in `dm-map--scale-props'"
  ;;(message "[xml-to-num] arg:%s" dom-node)
  (when (dm-svg-dom-node-p dom-node)
    ;;(message "[xml-to-num] dom-node:%s" (prin1-to-string dom-node))
    (dolist (attr (mapcar 'car dm-map--scale-props) dom-node)
      ;; (message "[xml-to-num] before %s -> %s"
      ;; 	       attr (dom-attr dom-node attr))
      (when-let ((val (dom-attr dom-node attr)))
	(when (string-match-p "^[0-9.+-]+$" val)
	  (dom-set-attribute dom-node attr (string-to-number val))
	  ;; (message "[xml-to-num] after(1) -> %s"
	  ;; 	   (prin1-to-string (dom-attr dom-node attr)))
	  )))
    (dolist (child (dom-children dom-node))
      (dm-map--xml-attr-to-number child))
    ;;(message "[xml-to-num] after ⇒ %s" (prin1-to-string dom-node))
    dom-node))

(defun dm-map--xml-parse ()
  "Add parsed XML to PLIST and clear `dm-map--xml-strings."
  (when (and dm-map--last-plist dm-map--xml-strings)
    (let ((xml-parts (dm-map--xml-attr-to-number
		      (with-temp-buffer
			(insert "<g>"
				(mapconcat
				 'identity
				 (reverse dm-map--xml-strings) "")
				"</g>")
			(libxml-parse-xml-region (point-min)
						 (point-max))))))
      (prog1 (plist-put dm-map--last-plist dm-map-overlay-prop
			(append (plist-get dm-map--last-plist
					   dm-map-overlay-prop)
				(list xml-parts)))
	(setq dm-map--xml-strings nil)))))

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

;; ZZZ consider functions returning lambda per column; lvalue for d-c-h cols?

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
		   ;; Ingore inverted tags ("some-tile:!some-tag"). Add others
		   ;; in last-plist as alist of: (KEYWORD TAG-SYM INV-TAG-SYM)
		   (when-let* ((ref (match-string 1 tile))
			       (kw (match-string 2 tile))
			       (tag (intern kw))
			       (ref-sym (intern ref))
			       (referent (gethash ref-sym hash)))
		     (unless (or (match-string 3 tile)
				 (assoc tag (plist-get referent ',dm-map-tag-prop)))
		       (plist-put referent ',dm-map-tag-prop
				  (append
				   (plist-get referent ',dm-map-tag-prop)
				   (list
				    (list tag last-key
					  (intern (concat (match-string 1 tile)
							  ":!" (match-string 4 tile)))))))))))
	       (mapc (lambda (prop)
		       (when  (boundp `,prop)
			 (prog1
			  (plist-put dm-map--last-plist `,prop
				     (nconc (plist-get dm-map--last-plist `,prop)
					    (dm-map-parse-plan-part (symbol-value `,prop)))))))
		     (delq nil'(,dm-map-draw-prop ,@dm-map-draw-other-props)))
	       (when (and (boundp (quote ,dm-map-overlay-prop)) ,dm-map-overlay-prop)
		 (push ,dm-map-overlay-prop dm-map--xml-strings))
	       dm-map--last-plist)))
	 (result
	  (prog1 (eval `(list :load ,cform))
	    ;; process any overlay XML from last row
	    (dm-map--xml-parse))))))

(dm-table-defstates 'map :extract #'dm-map-load)
(dm-table-defstates 'tile :transform #'dm-map-tiles-transform)
(dm-table-defstates 'cell :transform #'dm-map-level-transform)


;; quick and dirty procedural approach

(cl-defun dm-map--dom-attr-scale-nth (dom-node scale) ;; (attr n)
  "Apply Nth of SCALE to ATTR of DOM-NODE if present."
  (dolist (attr dm-map--scale-props dom-node)
    (when-let* ((n (if (listp (cdr attr)) (car (cdr attr)) (cdr attr)))
		(sl (list (car scale) (cdr scale)))
		(factor (nth n sl))
		(attr (car attr))
		(val (dom-attr dom-node attr)))
      (when (numberp val)
	(dom-set-attribute dom-node attr (* factor val))))))


;; (dm-map--dom-attr-scale-nth (dom-node 'foo '((bar . 10))) '(2 3 4) '(bar 0))
;; (dm-map--dom-attr-scale-nth (dom-node 'foo '((x . 10))) '(2 . 4 ))

;; second test case to figure out looping a list
;; (let ((dom-node (dom-node 'foo '((font-size . 10) (y . 10)) "content")))
;;   (dm-map--dom-attr-scale-nth dom-node '(12 23))
;;   dom-node)

(cl-defun dm-map--dom-attr-nudge (nudge scale pos dom-node)
  "Position and scale DOM-NODE.

Accept any DOM node, consider only X, Y and font-size properties.
NUDGE, SCALE and POS are cons cells in the form (X . Y).  NUDGE
gives the canvas padding in pixes while SCALE gives the number of
pixels per dungeon unit and POS gives the location of the origin
of the current cell in dungeon units."
  ;; (message "[nudge] DOMp:%s scale:%s pos:%s node:%s"
  ;; 	   (dm-svg-dom-node-p dom-node) scale pos dom-node)
  (when (dm-svg-dom-node-p dom-node)
    (when-let ((x-scale (car scale))
	       (y-scale (if (car-safe (cdr scale))
			    (cadr scale)
			  (car scale)))
	       (x-orig (dom-attr dom-node 'x))
	       (y-orig (dom-attr dom-node 'y))
	       (x-new (+ (car nudge)
			 (* x-scale (car pos))
			 (* x-scale x-orig)))
	       (y-new (+ (cdr nudge)
			 (* y-scale (cdr pos))
			 (* y-scale y-orig))))
      ;; (message "[nudge] x=(%s*%s)+(%s*%s)=%s, y=(%s*%s)+(%s*%s)=%s"
      ;; 	       x-scale x-orig x-scale (car pos) x-new
      ;; 	       y-scale y-orig y-scale (cdr pos) y-new)
      (when-let ((font-size (dom-attr dom-node 'font-size)))
	(dom-set-attribute dom-node 'font-size (* x-scale font-size)))
      (when-let ((path-data (dom-attr dom-node 'd)))
	(dom-set-attribute
	 dom-node 'd (concat
		      "M" (number-to-string (car pos))
		      "," (number-to-string (cdr pos))
		      " " path-data)))
      (dom-set-attribute dom-node 'x x-new)
      (dom-set-attribute dom-node 'y y-new))
    ;; process child nodes
    (dolist (child (dom-children dom-node))
      (dm-map--dom-attr-nudge nudge scale pos child))
    dom-node))

;;(dm-map--dom-attr-nudge '(1 2)  '(4 . 8) (dom-node 'text '((x . 16)(y . 32))))

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
  ;;(let ((cells (copy-tree cells))))
  (dolist (cell cells cells)
   (pcase cell
     ;; capital letter means absolutely positioned; ignore
     (`(,(or 'M 'L 'H 'V 'A) ,_) nil)
     ;; move or line in the form (sym (h v)
     (`(,(or 'm 'l)
	(,(and x (guard (numberp x)))
	 ,(and y (guard (numberp y)))))
      (setcdr cell (list (list (round (* x (car scale)))
			       (round (* y (cdr scale)))))))
     ;; h and v differ only in which part of scale applies
     (`(h (,(and d (guard (numberp d)))))
      (setcdr cell (list (round (* d (car scale))))))
     (`(v (,(and d (guard (numberp d)))))
      (setcdr cell (list (list (* d (cdr scale))))))
     ;; arc has tons of args but we only mess with the last two
     (`(a (,rx ,ry ,x-axis-rotation ,large-arc-flag ,sweep-flag
	       ,(and x (guard (numberp x)))
	       ,(and y (guard (numberp y)))))
      (setcdr cell (list (list rx ry x-axis-rotation large-arc-flag sweep-flag
			       (round (* x (car scale)))
			       (round (* y (cdr scale)))))))
     ;; fall-back to a message
     (_ (warn "Can't scale SVG path %s => %s " (type-of cell) (prin1-to-string cell t)))
     )))

;;(dm-map-default-scale-function '(100 . 1000) (dom-node 'text '((font-size . .5)(x . .2))) '(h (.1)) '(v (.2))'(m (.3 .4)) '(l (.5 .6)) '(x (.7 .8)) '(a (0.05 0.05 0 1 1 -0.1 0.1)))
;;(dm-map-default-scale-function '(100 . 100) '(text ((x . .5) (y . 2.25) (font-size . .6) (fill . "blue")) "General Store"))
;; (dm-map-default-scale-function '(100 . 1000) (dom-node 'text '((font-size . .5)(x . .2))))
;; (dm-map-default-scale-function '(100 100) (dom-node 'text '((font-size . .5))))
;; (a (.7 .7 0 1 1 0 .14))
;; (a (0.05 0.05 0 1 1 -0.1 0.1))

;; image scale
;;;;;;;;;

;; simple draw methods for testing

(defvar dm-map-current-tiles nil "While drawing, the map tiles used so far.")
(defvar dm-map-seen-cells '() "List of map cells we've seen so far.")

(cl-defun dm-map-resolve (tile &optional &key
			       (prop dm-map-draw-prop)
			       inhibit-collection
			       inhibit-tags)
  "Get draw code for TILE.

PROP is a symbol naming the proppery containing draw code.  When
INHIBIT-COLLECTION is truthy, don't collect tiles resolved into
'dm-map-resolve'.  When INHIBIT-TAGS is truthy do not add
addional tiles based on tags."
  (when-let ((plist (gethash tile dm-map-tiles)))
    (unless (or inhibit-tags (member tile dm-map-current-tiles))
      (push tile dm-map-current-tiles))
    (when-let (paths
	       (delq
		nil
		(append (plist-get plist prop)
			(unless inhibit-tags
			  (dm-map-tile-tag-maybe-invert tile)))))
      (mapcan (lambda (stroke)
		(let ((stroke stroke))
		  (if (symbolp stroke)
		      (dm-map-resolve
		       stroke
		       :inhibit-collection inhibit-collection
		       :inhibit-tags inhibit-tags)
		    ;;(list stroke)
		    (list (if (listp stroke) (copy-tree stroke) stroke))))) paths))))
;;(not (equal (dm-map-resolve '◦N) (let ((dm-map-tags nil)) (dm-map-resolve '◦N))))


(cl-defun dm-map-resolve-cell (cell &optional
				    &key (prop dm-map-draw-prop)
				    no-follow
				    inhibit-collection
				    inhibit-tags)
  "Get draw code for CELL.

PROP is a symbol naming the proppery containing draw code.
Follow references unless NO-FOLLOW is truthy.  When
INHIBIT-COLLECTION is truthy, don't collect tiles resolved.  When
INHIBIT-TAGS is truthy do not add addional tiles based on tags."
  (setq dm-map--last-cell cell)
  (when-let* ((plist (gethash cell dm-map-level))
	      (paths (plist-get plist prop)))
    (mapcan (lambda (stroke)
	      (let ((stroke stroke))
		(pcase stroke
		  ;; ignore references when drawing a complete level
		  (`(,(and x (guard (numberp x))). ,(and y (guard (numberp y))))
		   (unless no-follow
		     (dm-map-resolve-cell
		      stroke :prop prop
		      :inhibit-collection inhibit-collection
		      :inhibit-tags inhibit-tags)))
		  ((pred stringp)
		   (warn "XML found in paths from %s ignoring %s" cell stroke))
		  ((pred symbolp)
		   (dm-map-resolve
		    stroke :prop prop
		    :inhibit-collection inhibit-collection
		    :inhibit-tags inhibit-tags))
		  (_ (list (if (listp stroke) (copy-tree stroke) stroke))))
		))
	    paths)))

(defun dm-map--positioned-cell (scale prop cell)
  "Return paths preceded with origin move for CELL.

CELL must be a position referncing draw code.  Referneces are not
followed.  SCALE is the number of pixels per cell.  PROP is the
property containing draw instructions."
  ;;(message "[--pos] scale:%s cell:%s prop:%s" scale cell prop)
  (append (list (list 'M
		      (* scale (car cell))
		      (* scale (cdr cell))))
	  (dm-map-resolve-cell cell prop t)))

(defun dm-map--flatten-paths (paths)
  "Return PATHS as a flatter list removing nil entries."
  (apply 'append (delq nil paths)))

;; TODO overlay, etc are for tiles only; add support from map-cells
(defun dm-map-cell (cell &optional follow)
  "Return plist of draw code for CELL.

When FOLLOW is truthy, follow to the first cell with drawing
instructions."
  (let ((plist (dm-map-cell-defaults))
	dm-map-current-tiles
	)
    (when-let ((val (dm-map-resolve-cell cell :no-follow (null follow))))
      (plist-put plist dm-map-draw-prop val))
    (when-let ((val (seq-map
		     (lambda (tile)
		       (dm-map-resolve tile :prop dm-map-overlay-prop
				       :inhibit-tags t
				       :inhibit-collection t))
		     dm-map-current-tiles)))
      (plist-put plist dm-map-overlay-prop (delq nil val)))
    (dolist (prop dm-map-draw-other-props)
      (when-let ((val (seq-map
		       (lambda (tile)
			 (dm-map-resolve tile :prop prop
					 ;; FIXME: commenting crashes render
					 :inhibit-tags t
					 :inhibit-collection t))
		       dm-map-current-tiles)))
	(plist-put plist prop (delq nil val))))
    (append (list :cell (copy-tree cell)) plist)))

(defun dm-map-path-string (paths)
  "Return a list of svg drawing PATHS as a string."
  (mapconcat
   'concat
   (delq nil (mapcar
	      (lambda (path-part)
		(pcase path-part
		  ((pred stringp) path-part)
		  (`(,cmd ,args)
		   (mapconcat 'concat (append (list (symbol-name cmd))
					      (mapcar 'number-to-string
						      (if (listp args)
							  args
							(list args))))
			      " "))))
	      paths))
   " "))

(defun dm-map--cell-paths (cells plist prop scale)
  "Return paths for PROP from CELLS PLIST, if any.

SCALE is the number of pixels per dungeon unit, used to add
absolute movement to cell's origin as the first instruction."
  (apply 'append (delq nil (mapcar
			    (lambda (paths)
			      (append
			       (list (list 'M (list (* scale (car (plist-get plist :cell)))
						    (* scale (cdr (plist-get plist :cell))))))
			       paths))
			    (plist-get plist prop)))))

(cl-defun dm-map-quick-draw (&optional
			     (cells (when dm-map-level (hash-table-keys dm-map-level)))
			     &key
			     (path-attributes dm-map-draw-attributes)
			     (scale dm-map-scale)
			     (scale-function #'dm-map-default-scale-function)
			     (nudge dm-map-nudge)
			     (background dm-map-background)
			     (tiles dm-map-tiles)
			     (level dm-map-level)
			     (size (cons (* scale (car dm-map-level-size))
					 (* scale (cdr dm-map-level-size))))
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
  (setq dm-map-current-tiles nil)
  (let* ((maybe-add-abs
	  (lambda (pos paths)
	    (when paths (append
			 (list (list 'M (list
					 (+ (car nudge) (* scale (car pos)))
					 (+ (cdr nudge) (* scale (cdr pos))))))
			 paths))))
	 (draw-code (delq nil (mapcar 'dm-map-cell cells)))
	  ;; handle main path seperately
	 (main-path (apply
		     'append
		     (mapcar
		      (lambda (cell)
			(funcall maybe-add-abs
				 (plist-get cell :cell)
				 (plist-get cell dm-map-draw-prop)))
		      draw-code)))
	 ;; now the rest of the path properties
	 (paths (mapcar
		 (lambda(prop)
		   (mapcan
		    (lambda (cell)
		      (when-let ((strokes (plist-get cell prop))
				 (pos (plist-get cell :cell)))
			(append
			 (list
			  (list 'M (list
				    (+ (car nudge) (* scale (car pos)))
				    (+ (cdr nudge) (* scale (cdr pos))))))
			 (apply 'append strokes))))
		    draw-code))
		 dm-map-draw-other-props))
	 ;; scale the main path
	 (main-path (progn
		      (dm-map-path-string
		       (apply
			(apply-partially scale-function (cons scale scale))
			main-path))))
	 ;; scale remaining paths
	 (paths
	  (mapcar
	   (lambda(o-path)
	     (dm-map-path-string
	      (apply
	       (apply-partially scale-function (cons scale scale))
	       o-path)))
	   paths))
	 ;; make svg path elements
	 (paths (delq nil (seq-map-indexed
			   (lambda(path ix)
			     (when (org-string-nw-p path)
			       (dm-svg-create-path path
						   (plist-get
						    path-attributes
						    (nth ix dm-map-draw-other-props)))))
			   paths)))
	 ;; hack in a single SVG XML prop, for now scale inline
	 (nudge-svg (apply-partially 'dm-map--dom-attr-nudge
				     (list scale scale)))
	 ;; fetch or build background if not disabled
	 (background
	  (cond ((equal background t)
		 (list
		  (dm-map-background :scale (cons scale scale)
				     :size size :nudge nudge)))
		(background)))
	 (overlays (mapcan
		    (lambda (cell)
		      (mapcar (apply-partially 'dm-map--dom-attr-nudge
					       nudge
					       (list scale scale)
					       (plist-get cell :cell))
			      (apply
			       'append
			       (plist-get cell dm-map-overlay-prop))))
		    draw-code))
	 (img (dm-svg :svg (append (apply 'svg-create
					  (append (list (+ (* 2 (car nudge)) (car size))
							(+ (* 2 (cdr nudge)) (cdr size)))
						  svg-attributes))
				   (append background paths overlays))
		      :path-data (dm-svg-create-path
				  main-path (plist-get path-attributes
						       dm-map-draw-prop)))))
    ;;(message "[draw] draw-code:%s" (prin1-to-string draw-code))
    ;;(message "[draw] path-props:%s" path-attributes)
    ;;(message "[draw] XML SVG overlays:%s" (prin1-to-string overlays))
    ;;(message "[draw] other paths:%s" (prin1-to-string paths))
    ;;(message "[draw] background:%s" (prin1-to-string background))
    ;;(when (boundp 'dm-dump) (setq dm-dump draw-code))
    img))

(cl-defun dm-map-background (&optional
			     &key
			     (scale (cons dm-map-scale dm-map-scale))
			     (size (cons (* (car scale) (car dm-map-level-size))
					 (* (cdr scale) (cdr dm-map-level-size))))
			     (nudge dm-map-nudge)
			     (v-rule-len (+ (car size) (* 2 (car nudge))))
			     (h-rule-len (+ (cdr size) (* 2 (cdr nudge))))
			     (svg (svg-create v-rule-len v-rule-len))
			     no-canvas
			     (canvas (unless no-canvas
				       (svg-rectangle `(,svg '(()))
						      0 0 v-rule-len h-rule-len
						      :fill "#fffdd0"
						      :stroke-width  0)))
			     no-graph
			     (graph-attr '((fill . "none")
					   (stroke . "blue")
					   (stroke-width . ".25"))))
  "Create a background SVG with SCALE and SIZE and NUDGE."
  (dm-msg :file "dm-map" :fun "background" :args
	  (list :scale scale :size size :nudge nudge :svg svg))
  (prog1 svg
    (unless no-graph
      (dom-append-child
       svg
       (dm-svg-create-path
	(mapconcat
	 'identity
	 (append
	  (cl-mapcar
	   (apply-partially 'format "M0,%d h%d")
	   (number-sequence (cdr nudge)
			    (+ (car size) (cdr nudge))
			    (car scale))
	   (make-list (1+ (ceiling (/ (car size) (car scale)))) v-rule-len))
	  (cl-mapcar
	   (apply-partially 'format "M%d,0 v%d")
	   (number-sequence (car nudge)
			    (+ (car size) (car nudge))
			    (cdr scale))
	   (make-list (1+ (ceiling (/ (cdr size) (cdr scale)))) h-rule-len)))
	 " ")
	graph-attr)))))



;; commands and major mode

(defun dm-map-kill-buffer (&rest _)
  "Remove the dungeon-mode map buffer."
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(defun dm-map-quit (&rest _)
  "Remove the map buffer any window it created."
  (interactive)
  (quit-restore-window nil 'kill))

(defun dm-map-draw (&optional arg)
  "Draw all from `dm-map-level'.  With prefix ARG, reload first."
  (interactive "P")
  (when arg ;; prompt if no files or neg prefix arg
    (if (or (not dm-map-files) (equal '- arg))
	(setq dm-map-files (completing-read-multiple
			    "Files:"
			    (append (dm-files-select :map-tiles)
				    (dm-files-select :map-cells))
			    nil t ;; no prediecate, require match
			    (mapconcat 'identity dm-map-files ",")
			    ))
      ;;(user-error "Draw failed.  No files in `dm-map-files'")
      nil)
    (dm-map-load)) ;; any prefix arg causes reload from files
  (let ((svg (dm-map-quick-draw)))
    (prog1 svg
      (dm-map-draw-test svg ;; (oref svg path-data)
	(render-and-insert-string svg)))))

;; (defun dm-map-center (&optional x y)
;;   "Center map buffer, on POS when given.

;; Interactively, prompt for a new center location or use the
;; physical center of the image.  X and Y locaton of the target cell
;; in Dungeon units."
;;   (interactive "nCenter (cell x):\nnCenter (cell y):")
;;   (let ((lx (car dm-map-level-size))
;; 	(ly (cdr dm-map-level-size))
;; 	(frame (window-frame (selected-window)))
;; 	(wx (window-body-width nil t))
;; 	(wy (window-body-height nil t)))
;;     (let ((nx (cond ((< x 0) 0)
;; 		    ((> x (- lx 2)) (1- lx))
;; 		    (t (1- x))))
;; 	  (ny (cond ((< y 0) 0)
;; 		    ((> y (- ly 2)) (1- ly))
;; 		    (t y)))
;; 	  (dx (/ wx dm-map-scale))
;; 	  (dy (/ wy dm-map-scale))
;; 	  (cw (frame-char-width frame)))
;;       (let ((sx (* dx nx))
;; 	    (sy (* dy ny)))
;; 	(message "x:%s/%s at %s ea (%s), y:%s/%s at %s ea. (%s)"
;; 		 sx wx dx nx sy wy dy ny)))))

(defun dm-map-scale (&optional arg)
  "Set mapping scale to ARG pixels per dungeon-unit."
  (interactive
   (list
    (string-to-number
     (read-string (format "Scale (in pixels, currently %s): " dm-map-scale)
		  nil nil dm-map-scale ))))
  ;;(interactive "NEnter map scale (pixels):")
  (let ((win (selected-window)))
    (let ((width (frame-char-width (window-frame win))))
      (let ((hscroll (* (window-hscroll win) width))
	    (vscroll (window-vscroll win t))
	    (oscale dm-map-scale))
	(when (not (eql oscale arg))
	  (setq dm-map-scale arg)
	  (dm-map-draw)
	  (set-window-hscroll win (round (/ (* hscroll arg) oscale width)))
	  (set-window-vscroll win (round (/ (* vscroll arg) oscale)) t))))))

(defun dm-map-scale-nudge (&optional arg)
  "Adjust scale ARG increments of var `dm-map-scale-nudge'.

ARG is the factor for applying 'dm-map-scale-nudge' to `dm-map-scale'."
  (interactive "p")
  (dm-map-scale (+ dm-map-scale (* dm-map-scale-nudge arg))))

(defun dm-map-scale-nudge-invert (&optional arg)
  "Inverse of `dm-map-scale-nudge'; ARG is inverted."
  (interactive "p")
  (dm-map-scale-nudge (- arg)))

  ;; (let ((nudge (if (< 0 arg) )
  ;; 	 (cond ((and (numberp arg) (> 0 arg)) (* arg dm-map-scale-nudge))
  ;; 	       ((and (numberp arg)) (- 0 (* arg dm-map-scale-nudge)))
  ;; 	       ((and arg (symbolp arg)) (- 0 dm-map-scale-nudge))
  ;; 	       ((eq (car-safe arg) 16) "")
  ;; 	       (arg "")))
  ;; 	))

(defun dm-map-scroll (&optional arg)
  "Scroll the map by ARG or `dm-map-scroll-nudge'."
  (interactive "p")
  (scroll-down-line (or dm-map-scroll-nudge
			(* dm-map-scale (cdr dm-map-nudge)))))

(defun dm-map-scroll-invert (&optional arg)
  "Scroll the map by - ARG or `dm-map-scroll-nudge'."
  (interactive "p")
  (scroll-up-line (or dm-map-scroll-nudge
		   (* dm-map-scale (car dm-map-nudge)))))

(defun dm-map-hscroll (&optional arg)
  "Scroll the map horizontally by ARG or `dm-map-scroll-nudge'."
  (interactive "p")
  (scroll-left (or dm-map-scroll-nudge
		   (* dm-map-scale (car dm-map-nudge)))))

(defun dm-map-hscroll-invert (&optional arg)
  "Scroll the map by - ARG or `dm-map-scroll-nudge'."
  (interactive "p")
  (scroll-right (or dm-map-scroll-nudge
		   (* dm-map-scale (car dm-map-nudge)))))

;; global key binding
(global-set-key (kbd "<f9>") 'dm-map-draw)

;; basic major mode for viewing maps
(defvar dm-map-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+") 'dm-map-scale-nudge)
    (define-key map (kbd "-") 'dm-map-scale-nudge-invert)
    ;; (define-key map (kbd "<up>") 'dm-map-scroll)
    ;; (define-key map (kbd "<down>") 'dm-map-scroll-invert)
    ;; (define-key map (kbd "<right>") 'dm-map-hscroll)
    ;; (define-key map (kbd "<left>") 'dm-map-hscroll-invert)
    (define-key map (kbd "d") 'dm-map-draw)
    (define-key map (kbd "g") 'dm-map-draw)
    (define-key map (kbd "k") 'dm-map-kill-buffer)
    (define-key map (kbd "q") 'dm-map-quit)
    (define-key map (kbd "r") 'dm-map-draw)
    (define-key map (kbd "s") 'dm-map-scale)
    map))

(define-derived-mode dm-map-mode image-mode "MAP"
  "Major mode for `dugeon-mode' maps.")

;; (setq dm-map-files '("d:/projects/dungeon-mode/Docs/Maps/map-tiles.org"
;; 		     "d:/projects/dungeon-mode/Docs/Maps/Levels/A-Maze_level1.org"))
;; (setq dm-map-tags t)

(provide 'dm-map)
;;; dm-map.el ends here
