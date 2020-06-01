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
(require 'mouse)
(require 'sgml-mode)

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

(defcustom dm-map-files (append (dm-files-select :map-tiles)
				(list (car-safe (dm-files-select :map-cells))))
  "List of files from which load game maps."
  :type (list 'symbol)
  :group 'dm-files
  :group 'dm-map)

(defcustom dm-map-menus-file-style 'radio
  "How to display file menus; raidio or toggle."
  :group 'dm-map
  :type (list 'or 'radio 'toggle))

(defcustom dm-map-menus-file-redraw t
  "How to display file menus; raidio or toggle."
  :group 'dm-map
  :type 'boolean)

(defcustom dm-map-preview-buffer-name "**dungeon map**"
  "The name of the buffer created when previewing the map."
  :group 'dm-map
  :type 'string)

(defcustom dm-map-menus-level-cells-draw-all t
  "When t, draw all cells."
  :type 'boolean)

(defcustom dm-map-property "ETL"
  "Property to insepect when finding tables." :type 'string)

(defcustom dm-map-scale 100
  "Number of pixes per \"Dungeon Unit\" when mapping.

This setting controls the number of actual screen pixes
assigned (in both dimensions) while drwaing the map."
  :type 'number)

(defcustom dm-map-nudge '(1 . 1)
  "Top and left padding in dungeon units as a cons cell (X . Y)."
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

;;;
;; position "seen" cells history

(define-widget 'dm-custom-cons-x-y 'lazy
  "A number or cons cell in the form (X . Y)."
  :tag "Pos (X . Y)"
  :type '(cons (number :tag "X")
	       (number :tag "Y")))

(defcustom dm-map-pos nil
  "When playing, the position of the party on the map."
  :type 'dm-custom-cons-x-y)

(defcustom dm-map-pos-decoration-id "pos-decoration"
  "Value for the ID attribute for the SVG element used to identity's position."
  :type 'string)

(defcustom dm-map-pos-decoration `(circle ((id . ,dm-map-pos-decoration-id)
					   (cx . .5) (cy . .5) (r . .4)
					   (stroke . "purple")
					   (stroke-width . .2)))
  "SVG ELement used to indicate the position of the party."
  :type 'sexp)

(defcustom dm-map-current-level-cells nil
  "List of visible map cells."
  :type '(repeat :tag "Cell Positions"
		 dm-custom-cons-x-y))

(defcustom dm-map-level-cells nil
  "The displayable cells for each map level, as a plist."
  :type '(plist :value-type
		'(repeat :tag "Cell Positions"
			 dm-custom-cons-x-y)))

(defvar dm-map-svg nil
  "The current map display as an SVG.")

(defvar dm-map-current-level nil
  "The current map level being displayed.")
;; end "seen" cells history
;;;

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

(defvar dm-map-draw-uni-prop 'universal
  "Name of the column that adds commands to all paths.")

(defvar dm-map-underlay-prop 'underlay
  "Default column/property for SVG elements underlaying maps.")

(defvar dm-map-overlay-prop 'overlay
  "Default column/property for SVG elements overlaying maps.")

(defvar dm-map-prop-tmp-alist
  `(,@(mapcar (lambda (prop)
		(cons prop (intern (concat (symbol-name prop)
					   "-xml-strings"))))
	      (list dm-map-overlay-prop dm-map-underlay-prop)))
  "Mapping properties to temporary storage when reading XML.")

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
			       ,dm-map-underlay-prop nil
			       ,dm-map-draw-uni-prop nil
			       ,@(mapcan (lambda (x) (list x nil))
					 dm-map-draw-other-props))
  "Default attributes for cells or tiles as they are loaded.")

;; (defvar dm-map-extra-tiles '('Background 'Graphpaper)
;;   "Extra tiles to include when rendering a level.")

;; tagging stuff

(defvar dm-map-tags '()
  "List of keyword symbols to include from tile paths.")

(defvar dm-map-menus-tags-list nil
  "List of tags to display on the menu.")

(defvar dm-map-tile-tag-re "^\\(.*\\)\\([:]\\([!]\\)?\\(.*?\\)\\)$"
                        ;;     1 base  2 lit 3 invt-p  4 TAG
  "Regex used to parse tags in tile symbols.

Capture groups:
  1. Basename, e.g. reference tile.
  2. Tag literal including seperator, empty when tile is not tagged.
  3. Inversion/negation indicator, if any (e.g. \"!\").
  4. Tag, e.g. \"elf\" from \"secret-door:elf\".")

(defvar dm-map--scale-props '((font-size 0)
			      (x 0) (y 1)
			      (r 0) (cx 0) (cy 1))
  "SVG element properties and scale association.

Scale association may be 0 to use x scaling factor, or 2 for y.")

(defvar dm-map--last-cell nil "While resolving the last cell resolved.")

(defvar dm-map--last-plist nil
  "While transforming tile tables, the last plist created.")

(defvar dm-map--xml-strings (list dm-map-underlay-prop nil
				  dm-map-overlay-prop nil)
  "While transforming tile tables, unparsed XML strings.")

(defmacro dm-map-cell-defaults ()
  "Return an empty map cell plist."
  `(list ',dm-map-draw-prop nil
	 ',dm-map-tag-prop nil
	 ',dm-map-overlay-prop nil
	 ',dm-map-underlay-prop nil
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

;; check if we've seen a given map-cell
(defsubst dm-map-seen-cell-p (cell-pos dir &optional dist)
  "Return t when the referenced cell has been revealed.

The referenced cell is decribed in terms of:
  CELL-POS, cons (X . Y) given a start position
  DIST, number of dungeon-units to travel, 1 by default.
  DIR, direction of travel, as a symbol, one of:
  'north
  'east
  'south
  'west"
  (let* ((dist (or dist 1))
	 (target-pos
	  (pcase dir
	    ('north (cons (car cell-pos) (- dist (cdr cell-pos))))
	    ('east (cons (+ (car cell-pos) dist) (cdr cell-pos)))
	    ('south (cons (car cell-pos) (+ dist (cdr cell-pos))))
	    ('west (cons (- (car cell-pos) dist) (cdr cell-pos)))))
	 (target-cell (gethash target-pos dm-map-level)))
    (while (pcase (car-safe (plist-get target-cell dm-map-draw-prop))
	     (`(,(and x (guard (numberp x))) . ,(and y (guard (numberp y))))
	      (setq target-cell (gethash (cons x y) dm-map-level)))))
    (member target-pos dm-map-current-level-cells)))


(defun dm-map-tile-tag-seen-p (tag)
  "Return t when TAG includes a satasfied seen predicate.

Seen predicates look like:
  tile:tag:seen(:dir n)

Where tile:tag represents the tile's basename, :dir is a
direction of travel and n is a number of cells to travel."
  (let ((str (if (stringp tag) tag (symbol-name tag))))
    ;;(message "SEEN: last-cell:%s tag:%s  RESULT:%s" dm-map--last-cell tag result)
    (when (string-match "seen([ ]*:?\\([^) ]+\\)[ ]*\\([0-9]*\\)[ ]*)" str)
      (dm-map-seen-cell-p dm-map--last-cell
			  (intern (match-string 1 str))
			  (string-to-number (or (match-string 2 str) "1"))
			  ))))

;; TODO write dm-map-tile-tag-maybe-invert ?
(defun dm-map-tile-tag-maybe-invert (tile)
  "Return tags for TILE.

Select each amung normal and inverted based on `dm-map-tags'."
  (delq nil (mapcan
	     (lambda (tag)
	       (list (if (or (eq t dm-map-tags)
			     (member (car tag) dm-map-tags)
			     (dm-map-tile-tag-seen-p (car tag)))
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
		      (string-match-p "^[MmVvHhCcSsAaL][0-9.,+-]+$" word)
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

(defmacro dm-map-with-map-erased (&rest body)
  "Erase the map buffer evaluate BODY the restore the image."
  `(with-current-buffer (pop-to-buffer dm-map-preview-buffer-name)
     (let ((image-transform-resize 1))
       (cl-letf (((symbol-function 'message) #'format))
	 (unless (derived-mode-p 'dm-map-mode) (dm-map-mode))
	 (let ((inhibit-read-only t))
	   ;; (if (image-get-display-property)
	   ;;     (image-mode-as-text)
	   ;;   (when (eq major-mode 'hexl-mode)
	   ;;     (image-mode-as-text)))
	   (erase-buffer)
	   ,@body
	   (unless (derived-mode-p dm-map-image-mode)
	     (funcall (symbol-function dm-map-image-mode)))
	   (unless (image-get-display-property) (image-toggle-display-image))
	   (beginning-of-buffer))))))

(defmacro dm-map-with-map-source (&rest body)
  "With the map source buffer current, evaluate BODY."
  `(with-current-buffer (pop-to-buffer dm-map-preview-buffer-name)
     (let ((image-transform-resize 1))
       (cl-letf (((symbol-function 'message) #'format))
	 (unless (derived-mode-p 'dm-map-mode) (dm-map-mode))
	 (let ((inhibit-read-only t))
	   (if (image-get-display-property)
	       (image-mode-as-text)
	     (when (eq major-mode 'hexl-mode)
	       (image-mode-as-text)))
	   ,@body
	   (beginning-of-buffer))))))

(defun dm-map-load (&rest files)
  "Truncate and replace `dm-map-tiless' and `dm-map-levels' from FILES.

Kick off a new batch for each \"feature\" or \"level\" table found."
  (dolist (var dm-map-table-var-alist)
    (set (cadr var) (dm-make-hashtable)))
  (setq dm-map-menus-tags-list nil)
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

(defvar dm-map--inhibit-clearing-xml-strings nil
  "When t, do not remove XML strings once processed.

This may result in duplicated tags when tiles are \"striped\"
e.g, are defined more than once, but allows the unparsed XML to
be viewed within `dm-map-tiles'.")

(defun dm-map--xml-parse (prop)
  "Add parsed XML for PROP to PLIST."
  (let ((tmp (cdr-safe (assoc prop dm-map-prop-tmp-alist))))
    ;;(message "checking for XML in %s for %s" tmp prop)
    (when (and tmp dm-map--last-plist (plist-get dm-map--last-plist tmp))
      ;;(message "found XML strings %s" (plist-get dm-map--last-plist tmp))
      (when-let* ((xml-parts (dm-map--xml-attr-to-number
			      (with-temp-buffer
				(insert "<g>"
					(mapconcat
					 'identity
					 (plist-get dm-map--last-plist tmp)
					 " ")
					"</g>")
				(libxml-parse-xml-region (point-min)
							 (point-max)))))
		  (xml-parts (dom-children xml-parts)))
	;; (message "[xml-parse] tmp:%s raw:%s parsed:%s"
	;; 	 tmp (plist-get dm-map--last-plist tmp) xml-parts)
	(prog1 (plist-put dm-map--last-plist prop
			  (append (plist-get dm-map--last-plist prop)
				  xml-parts))
	  (unless dm-map--inhibit-clearing-xml-strings
	    (plist-put dm-map--last-plist tmp nil)))))))

(defun dm-map-level-transform (table)
  "Transform a TABLE of strings into an hash-map."
  (let* ((cols (or dm-map-level-cols
		   (seq-map (lambda (label) (intern (downcase label)))
			    (car table))))
	 (last-key (gensym))
	 (cform
	  `(dm-coalesce-hash ,(cdr table) ,cols
	     ,@(when dm-map-level-key `(:key-symbol ,dm-map-level-key))
	     :hash-table dm-map-level
	     (when (and (boundp 'path) path)
	       (setq dm-map-level-size ,dm-map-level-key)
	       (list 'path (dm-map-parse-plan-part path)))))
	 (result (eval `(list :load ,cform))))))

(defmacro dm-map--when-tiles-message (message strings last-key tile &rest data)
  "Display MESSAGE when LAST-KEY or TILE match one of STRINGS.

Data is appended via `prin1-to-string'."
  `(let ((search-tiles ,strings))
     (when (or (and (boundp ',tile) (member ,tile search-tiles))
	       (member ,last-key (mapcar 'intern search-tiles)))
       (message "%s tile:%s last-key:%s plist:%s%s"
		,message (and (boundp ',tile) ,tile)
		,last-key dm-map--last-plist
		,(if (null data) `""
		   `(prin1-to-string ,data t))))))

;; ZZZ consider functions returning lambda per column; lvalue for d-c-h cols?

(defun dm-map-tiles-transform (table)
  "Transform a TABLE of strings into an hash-map."
  ;;(message "starting tile transform")
  (let* ((cols (or dm-map-tiles-cols (dm-map-header-to-cols (car table))))
	 (last-key (gensym))
	 (cform
	  `(dm-coalesce-hash ,(cdr table) ,cols
	     :hash-table dm-map-tiles
	     :hash-symbol hash
	     :key-symbol last-key
	     (progn
	       ;; (dm-map--when-tiles-message
	       ;; 	"starting row" (list "cW◦NES" "c4" "foo") last-key tile)
	       (when (and (boundp 'tile) (org-string-nw-p tile))
		 ;; This row includes a new tile-name; process any saved up
		 ;; XML strings before we overwrite last-key
		 (dolist (prop (list dm-map-overlay-prop dm-map-underlay-prop))
		   (dm-map--xml-parse prop));; TODO: add last-key tile; use when-tiles-msg
		 (setq last-key (intern tile)
		       dm-map--last-plist (dm-map-table-cell tile :table hash))
		 ;;(dm-map--when-tiles-message "new tile" (list "cW◦NES" "c4" "foo") last-key tile)
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
		       ;; add new keywords to the menu unless they include a predicate
		       (unless (string-match-p "seen(" kw)
			 (add-to-list 'dm-map-menus-tags-list tag))

		       (plist-put referent ',dm-map-tag-prop
				  (append
				   (plist-get referent ',dm-map-tag-prop)
				   (list
				    (list tag last-key
					  (intern (concat (match-string 1 tile)
							  ":!" (match-string 4 tile)))))))))))
	       ;;(dm-map--when-tiles-message "pre-parser" (list "cW◦NES" "c4" "foo") last-key tile)
	       (mapc (lambda (prop)
		       (when  (boundp `,prop)
			 (prog1 (plist-put dm-map--last-plist `,prop
					   (nconc (plist-get dm-map--last-plist `,prop)
						  (dm-map-parse-plan-part (symbol-value `,prop))))
			   ;;(dm-map--when-tiles-message "pre-parser" (list "cW◦NES" "c4" "foo") last-key tile)
			   )))
		     (delq nil'(,dm-map-draw-prop ,dm-map-draw-uni-prop ,@dm-map-draw-other-props)))
	       (mapc (lambda (prop)
		       (when (boundp `,prop)
			 (let ((tmp (cdr (assoc prop dm-map-prop-tmp-alist))))
			   (prog1 (plist-put dm-map--last-plist tmp
					     (append (plist-get dm-map--last-plist tmp)
						     (list (symbol-value `,prop))))))))
		     (delq nil '(,dm-map-overlay-prop ,dm-map-underlay-prop)))
	       ;;(dm-map--when-tiles-message "before overlays" (list "tree") last-key tile)
	       ;;(dm-map--when-tiles-message "end row" (list "tree") last-key tile)
	       dm-map--last-plist)))
	 (result
	  (prog1 (eval `(list :load ,cform))
	    ;; process any overlay XML from last row
	    (dolist (prop (list dm-map-overlay-prop dm-map-underlay-prop))
	      (dm-map--xml-parse prop)))))))

(dm-table-defstates 'map :extract #'dm-map-load)
(dm-table-defstates 'tile :transform #'dm-map-tiles-transform)
(dm-table-defstates 'cell :transform #'dm-map-level-transform)


;; quick and dirty procedural approach

(cl-defun dm-map--dom-attr-nudge (path-fun nudge scale pos dom-node)
  "Position and scale DOM-NODE.

Accept any DOM node, consider only X, Y and font-size properties.
NUDGE, SCALE and POS are cons cells in the form (X . Y).  NUDGE
gives the canvas padding in pixes while SCALE gives the number of
pixels per dungeon unit and POS gives the location of the origin
of the current cell in dungeon units.

For \"text\" elements, also scale \"font-size\".  For \"path\"
elements prepend an absolute movement command to path-data.  Call
PATH-FUN to scale the parsed command data of any path elements
found."
  ;; (message "[nudge] DOMp:%s scale:%s pos:%s node:%s"
  ;; 	   (dm-svg-dom-node-p dom-node) scale pos dom-node)
  (when (dm-svg-dom-node-p dom-node)
    (when-let* ((x-scale (car scale))
		(y-scale (if (car-safe (cdr scale))
			     (cadr scale)
			   (car scale)))
		(x-prop (if (dom-attr dom-node 'cx) 'cx 'x))
		(y-prop (if (dom-attr dom-node 'cy) 'cy 'y))
		(x-orig (or (dom-attr dom-node x-prop) 0))
		(y-orig (or (dom-attr dom-node y-prop) 0))
		(x-new (+ (* x-scale (car nudge))
			  (* x-scale (car pos))
			  (* x-scale x-orig)))
		(y-new (+ (* y-scale (cdr nudge))
			  (* y-scale (cdr pos))
			  (* y-scale y-orig))))
      ;; (message "[nudge] x=(%s*%s)+(%s*%s)=%s, y=(%s*%s)+(%s*%s)=%s"
      ;; 	       x-scale x-orig x-scale (car pos) x-new
      ;; 	       y-scale y-orig y-scale (cdr pos) y-new)
      (when-let ((font-size (dom-attr dom-node 'font-size)))
	(dom-set-attribute dom-node 'font-size (* x-scale font-size)))
      (when-let ((r-orig (dom-attr dom-node 'r)))
	(dom-set-attribute dom-node 'r (* x-scale r-orig)))
      (when-let ((path-data (dom-attr dom-node 'd)))
	(dom-set-attribute
	 dom-node 'd
	 (concat
	  "M" (number-to-string (+ (* x-scale (car nudge))
				   (* x-scale (car pos))))
	  "," (number-to-string (+ (* y-scale (cdr nudge))
				   (* y-scale (cdr pos))))
	  " " (dm-map-path-string
	       (apply (apply-partially path-fun
				       (cons x-scale y-scale))
		      (if (listp path-data) path-data
			(dm-map-parse-plan-part path-data)))))
	 ))
      (dom-set-attribute dom-node x-prop x-new)
      (dom-set-attribute dom-node y-prop y-new))
    ;; process child nodes
    (dolist (child (dom-children dom-node))
      (if (not (stringp child))
	  ;; (string-match "[$]\\([^][(),.$]\\)+" child)
	  (dm-map--dom-attr-nudge path-fun nudge scale pos child)
	  (dom-remove-node dom-node child)
	  (dom-append-child dom-node
			    (replace-regexp-in-string
			     "[$][^][(),^$ ]+"
			     (lambda (str)
			       (let* ((plist (gethash pos dm-map-level))
				      (var (intern (substring str 1 (length str))))
				      (val (plist-get plist var)))
				 ;;(message "var:%s val:%s pos:%s plist:%s" var val pos plist)
				 (format "%s" (or val ""))))
			     child))))
    dom-node))

;;(dm-map--dom-attr-nudge dm-map-default-scale-function '(1 . 2) '(4 8) '(0 . 0) (dom-node 'text '((x . 16)(y . 32))))
;(dm-map--dom-attr-nudge #'dm-map-default-scale-function '(1 . 1) '(2 2) '(3 . 3) (dom-node 'path '((d . "h1 v1 h-1 v-1"))))

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
     (`(M  ,_) nil)
     ;; move or line in the form (sym (h v)
     (`(,(or 'm 'l 'L)
	(,(and x (guard (numberp x)))
	 ,(and y (guard (numberp y)))))
      (setcdr cell (list (list (round (* x (car scale)))
			       (round (* y (cdr scale)))))))
     ;; h and v differ only in which part of scale applies
     (`(,(or 'h 'H)
	(,(and d (guard (numberp d)))))
      (setcdr cell (list (round (* d (car scale))))))
     (`(,(or 'v 'V)
	(,(and d (guard (numberp d)))))
      (setcdr cell (list (list (* d (cdr scale))))))
     (`(,(or 's 'S)
	(,(and x2 (guard (numberp x2)))
	 ,(and y2 (guard (numberp y2)))
	 ,(and x (guard (numberp x)))
	 ,(and y (guard (numberp y)))))
      (setcdr cell (list (list (round (* x2 (car scale)))
			       (round (* y2 (cdr scale)))
			       (round (* x (car scale)))
			       (round (* y (cdr scale)))))))
     ;; curveto
     (`(,(or 'c 'C)
	(,(and x1 (guard (numberp x1)))
	 ,(and y1 (guard (numberp y1)))
	 ,(and x2 (guard (numberp x2)))
	 ,(and y2 (guard (numberp y2)))
	 ,(and x (guard (numberp x)))
	 ,(and y (guard (numberp y)))))
      (setcdr cell (list (list (round (* x1 (car scale)))
			       (round (* y1 (cdr scale)))
			       (round (* x2 (car scale)))
			       (round (* y2 (cdr scale)))
			       (round (* x (car scale)))
			       (round (* y (cdr scale)))))))
     ;; arc, scale x and y radii and pos, leave flags alone
     (`(,(or 'a 'A)
	(,(and rx (guard (numberp rx)))
	 ,(and ry (guard (numberp ry)))
	 ,x-axis-rotation ,large-arc-flag ,sweep-flag
	 ,(and x (guard (numberp x)))
	 ,(and y (guard (numberp y)))))
      (setcdr cell (list (list (round (* rx (car scale)))
			       (round (* ry (cdr scale)))
			       x-axis-rotation large-arc-flag sweep-flag
			       (round (* x (car scale)))
			       (round (* y (cdr scale)))))))
     ;; fall-back to a message
     (_ (warn "Can't scale SVG path %s => %s (%s)"
	      (type-of cell) (prin1-to-string cell t)
	      (type-of (car-safe cell))))
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
			       inhibit-tags
			       inhibit-universal)
  "Get draw code for TILE.

PROP is a symbol naming the proppery containing draw code.  When
INHIBIT-COLLECTION is truthy, don't collect tiles resolved into
'dm-map-resolve'.  When INHIBIT-TAGS is truthy do not add
addional tiles based on tags.  Then INHIBIT-UNIVERSAL is truthy
do not add additional commands to path props from the universial
path command set."
  (when-let ((plist (gethash tile dm-map-tiles)))
    (unless (or inhibit-tags (member tile dm-map-current-tiles))
      (push tile dm-map-current-tiles))
    (let ((svgp (member prop (list dm-map-overlay-prop
				   dm-map-underlay-prop))))
      (when-let (paths
		 (delq
		  nil
		  (append (when (and dm-map-draw-uni-prop
				     (not inhibit-universal)
				     (not svgp))
			    (plist-get plist dm-map-draw-uni-prop))
			  (plist-get plist prop)
			  (unless inhibit-tags
			    (dm-map-tile-tag-maybe-invert tile)))))
	(mapcan (lambda (stroke)
		  (let ((stroke stroke))
		    (if (symbolp stroke)
			(or (dm-map-resolve
			     stroke :prop prop
			     :inhibit-collection inhibit-collection
			     :inhibit-tags inhibit-tags)
			    (dm-map-resolve
			     stroke ;;:prop prop
			     :inhibit-collection inhibit-collection
			     :inhibit-tags inhibit-tags))
		      ;;(list stroke)
		      (list (if (listp stroke) (copy-tree stroke) stroke)))))
		paths)))))
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
		  (`(,(and x (guard (numberp x))) . ,(and y (guard (numberp y))))
		   (unless no-follow
		     (dm-map-resolve-cell
		      stroke :prop prop
		      :inhibit-collection inhibit-collection
		      :inhibit-tags inhibit-tags)))
		  ((pred stringp)
		   (warn "XML found in paths from %s ignoring %s"
			 cell stroke))
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
(defun dm-map-cell (cell)
  "Return plist of draw code for CELL."
  (let ((plist (dm-map-cell-defaults))
	dm-map-current-tiles
	)
    (when-let ((val (dm-map-resolve-cell
		     cell
		     :no-follow t)))
      (plist-put plist dm-map-draw-prop val))
    (dolist (prop (append dm-map-draw-other-props (list dm-map-underlay-prop
							dm-map-overlay-prop)))
      (when-let ((val (seq-map
		       (lambda (tile)
			 (dm-map-resolve tile :prop prop
					 ;; FIXME: commenting crashes render
					 :inhibit-tags t
					 :inhibit-collection t))
		       dm-map-current-tiles)))
	(plist-put plist prop (delq nil val))))
    ;; (when (equal (cons 10 11) cell)
    ;;   (message "cell:%s" (list :cell (copy-tree cell) :plist plist)))
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

(cl-defun dm-map-level-find-cells (&optional (target dm-map-pos))
  "Return all cells in the group of which TARGET is a part."
  (seq-filter
   (lambda (cell)
     (or (equal target cell)
	 (equal target (car-safe (plist-get (gethash cell dm-map-level)
					    dm-map-draw-prop)))))
   (hash-table-keys dm-map-level)))

(cl-defun dm-map-quick-draw (&optional
			     (cells (when dm-map-level
				      (if dm-map-menus-level-cells-draw-all
					  (hash-table-keys dm-map-level)
					dm-map-current-level-cells)))
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
					 (+ (* scale (car nudge)) (* scale (car pos)))
					 (+ (* scale (cdr nudge)) (* scale (cdr pos))))))
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
				    (+ (* scale (car nudge)) (* scale (car pos)))
				    (+ (* scale (cdr nudge)) (* scale (cdr pos))))))
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
	 (overlays (mapcan
		    (lambda (cell)
		      ;;(mapcar (lambda (elem) (dom-remove-node)))
		      (mapcar (apply-partially 'dm-map--dom-attr-nudge
					       scale-function
					       nudge
					       (list scale scale)
					       (plist-get cell :cell))
			      (apply
			       'append
			       (plist-get cell dm-map-overlay-prop))))
		    draw-code))
	 ;; hack in a second SVG XML prop, continue to scale inline
	 (underlays (mapcan
		     (lambda (cell)
		       (mapcar (apply-partially 'dm-map--dom-attr-nudge
						scale-function
						nudge
						(list scale scale)
						(plist-get cell :cell))
			       (apply
				'append
				(plist-get cell dm-map-underlay-prop))))
		     draw-code))
	 ;; fetch or build background if not disabled
	 (canvas-size (cons (+ (* (car nudge) scale 2) (car size) dm-map-scale)
			    (+ (* (cdr nudge) scale 2) (cdr size) dm-map-scale)))
	 (background
	  (cond ((equal background t)
		 (list
		  (dm-map-background :size canvas-size :scale (cons scale scale)
				     :x-nudge (* scale (car nudge))
				     :y-nudge (* scale (cdr nudge)))))
		(background)))
	 (pos-decoration
	  (when (and dm-map-menus-play-mode dm-map-pos)
	    (delq
	     nil
	     (mapcar
	      (lambda (cell)
		`(rect ((fill . "purple") (fill-opacity . .5)
			(stroke . "none") (stoke-width . 0)
			(x . ,(+ (* scale (car nudge)) (* scale (car cell))))
			(y . ,(+ (* scale (cdr nudge)) (* scale (cdr cell))))
			(width . ,scale)
			(height . ,scale))))
	      (dm-map-level-find-cells dm-map-pos)))
	    ;; (list ;; '(path ((fill . "purple") (fill-opacity . .5)
	    ;; 	  ;; 	  ;;(stoke . "purple") (stroke-width . 1)
	    ;; 	  ;; 	  (d . "M10 10 H100 V100 H-100 V-100")))
	    ;; 	  '(rect ((x . 100) (y . 100) (width . 100) (height . 100))))
	    ;; (list
	    ;;  (dm-map--dom-attr-nudge scale-function
	    ;; 			     nudge
	    ;; 			     (list scale scale)
	    ;; 			     (copy-tree dm-map-pos)
	    ;; 			     (list 'rect (list  (cons 'stroke "red")
	    ;; 					       (cons 'fill "red")
	    ;; 					       (cons 'width 250)
	    ;; 					       (cons 'heigth 250)))
	    ;; 			     ))
	    ))
	 (been-dots
	  (when dm-map-menus-play-mode
	    (mapcar
	     (lambda (cell)
	       `(circle ((r . 2)
			 (cx . ,(+ (* (+ .4 (* .01 (random 20))) scale)
				   (* scale (car nudge)) (* scale (car cell))))
			 (cy . ,(+ (* (+ .4 (* .01 (random 20))) scale)
				   (* scale (cdr nudge)) (* scale (cdr cell))))
			 (fill . "#0f0f0f") (stroke . "none") (stroke-width . 0)
			 )))
	     cells)))
	 (img (dm-svg :svg
		      (append (apply 'svg-create
				     (append (list (car canvas-size)
						   (cdr canvas-size))
					     svg-attributes))
			      (append background underlays
				      been-dots pos-decoration)
			      (append (list (dm-svg-create-path
					     main-path
					     (plist-get path-attributes
							dm-map-draw-prop))))
			      (append  paths overlays))
		      ;;:overlays (append paths overlays pos-decoration)
		      ;; :path-data (dm-svg-create-path
		      ;; 		  main-path (plist-get path-attributes
		      ;; 				       dm-map-draw-prop))
		      )))
    ;;(message "[draw] svg:%s" been-dots)
    ;;(message "[draw] draw-code:%s" (prin1-to-string draw-code))
    ;;(message "[draw] path-props:%s" path-attributes)
    ;;(message "[draw] XML SVG overlays:%s" (prin1-to-string overlays))
    ;;(message "[draw] other paths:%s" (prin1-to-string paths))
    ;;(message "[draw] background:%s" (prin1-to-string background))
    ;;(when (boundp 'dm-dump) (setq dm-dump draw-code))
    (setq dm-map-svg img)))

(cl-defun dm-map-background (&optional
			     &key
			     (scale (cons dm-map-scale dm-map-scale))
			     (size (cons (* (car scale) (car dm-map-level-size))
					 (* (cdr scale) (cdr dm-map-level-size))))
			     (x-nudge (* (car scale) (car dm-map-nudge)))
			     (y-nudge (* (cdr scale) (cdr dm-map-nudge)))
			     (h-rule-len (+ (* (car scale) x-nudge 2) (car size)))
			     (v-rule-len (+ (* (cdr scale) y-nudge 2) (cdr size)))
			     (svg (svg-create h-rule-len v-rule-len))
			     no-canvas
			     (canvas (unless no-canvas
				       (svg-rectangle `(,svg '(()))
						      0 0 h-rule-len v-rule-len
						      :fill "#fffdd0"
						      :stroke-width  0)))
			     			     no-border
			     (border (unless no-border
				       (svg-rectangle `(,svg '(()))
						      (- x-nudge 2) (- y-nudge 2)
						      (- (car size) (* 2 x-nudge) -5)
						      (- (cdr size) (* 2 y-nudge) -5)
						      :fill "none"
						      :stroke "black"
						      :stroke-width  5)
				       (svg-rectangle `(,svg '(()))
						      (1- x-nudge) (1- y-nudge)
						      (- (car size) (* 2 x-nudge) -3)
						      (- (cdr size) (* 2 y-nudge) -3)
						      :fill "none"
						      :stroke "white"
						      :stroke-width  1)))
			     no-graph
			     (graph-attr '((fill . "none")
					   (stroke . "blue")
					   (stroke-width . ".25"))))
  "Create a background SVG with SCALE and SIZE and NUDGE."
  ;; (dm-msg :file "dm-map" :fun "background" :args
  ;; 	  (list :scale scale :size size :nudge nudge :svg svg))

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
	   (number-sequence y-nudge
			    (+ (cdr size) y-nudge y-nudge)
			    (car scale))
	   (make-list (1+ (ceiling (/ (cdr size) (cdr scale)))) h-rule-len))
	  (cl-mapcar
	   (apply-partially 'format "M%d,0 v%d")
	   (number-sequence x-nudge
			    (+ (car size) x-nudge x-nudge)
			    (cdr scale))
	   (make-list (1+ (ceiling (/ (car size) (car scale)))) v-rule-len)))
	 " ")
	graph-attr)))))


;; commands and major mode

(defun dm-map-kill-buffer (&rest _)
  "Remove the dungeon-mode map buffer."
  (interactive)
  (when-let ((buf (get-buffer dm-map-preview-buffer-name)))
    (when (buffer-live-p buf)
      (let (kill-buffer-query-functions) (kill-buffer buf)))))

(defun dm-map-quit (&rest _)
  "Remove the map buffer any window it created."
  (interactive)
  (when-let ((buf (get-buffer dm-map-preview-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf (quit-restore-window (get-buffer-window buf t) 'kill)))))

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
      (if dm-map-menus-play-mode
	  (dm-map-with-map-erased (render-and-insert svg))
	(dm-map-with-map-erased (render-and-insert-string svg)))
      (with-current-buffer dm-map-preview-buffer-name
	(image-mode-setup-winprops)
	(let ((inhibit-read-only t)
	      (start (point-min))
	      (end (point-max)))
	  (put-text-property start end 'pointer 'hand)
	  (when dm-map-image-map
	      (put-text-property start end :map dm-map-image-map))
	  (unless dm-map-draw-inhibit-help-echo
	    (put-text-property
	     start end 'help-echo
	     (if dm-map-menus-play-mode
		 (lambda (&rest _)
		   (let ((pos (dm-map--pos-impl)))
		     (with-slots (svg) dm-map-svg
		       (when dm-map--pos-mouse-overlay
			 (svg-remove svg "dm-map--pos-mouse-overlay"))
		       (setq
			dm-map--pos-mouse-overlay
			(svg-rectangle svg
				       (+ (* dm-map-scale (car dm-map-nudge))
					  (* dm-map-scale (car pos)))
				       (+ (* dm-map-scale (cdr dm-map-nudge))
					  (* dm-map-scale (cdr pos)))
				       dm-map-scale
				       dm-map-scale
				       :id "dm-map--pos-mouse-overlay"
				       :fill "red" :fill-opacity .5
				       :stroke "none" :stoke-width 0)))
		     (format "%s" pos)))
	       (lambda (&rest _)
		 (let ((pos (dm-map--pos-impl)))
		   (format "%s" pos)))))))))))

(defvar dm-map-image-map nil
  "List of mouse hit regions for image view.")

(defvar dm-map-image-mode 'dm-map-mode
  "Major mode used when displaying the map as an image.")

(defvar dm-map-draw-inhibit-help-echo nil
  "When t do not add an help-echo property to the map image.")

(defvar dm-map--pos-mouse-overlay nil
  "When displaying map in map-mode, the svg element indicating mouse position.")

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
    (let ((raw
	   (or (read-string (format "Scale (in pixels, currently %s): " dm-map-scale)
			    nil nil dm-map-scale )
	       0)))
      (if (numberp raw) raw (string-to-number raw)))))
  ;;(interactive "NEnter map scale (pixels):")
  (let ((win (selected-window)))
    (let ((width (frame-char-width (window-frame win))))
      (let ((hscroll (* (window-hscroll win) width))
	    (vscroll (window-vscroll win t))
	    (oscale dm-map-scale))
	(when (not (eql oscale arg))
	  (setq dm-map-scale arg)
	  (dm-map-draw)
	  (set-window-hscroll win (round (/ (* hscroll arg) arg width)))
	  (set-window-vscroll win (round (/ (* vscroll arg) arg)) t))))))

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

(defun dm-map--pos-pixels-to-cell (x y)
  "Return map cell under the given X Y position in pixels."
  (let* ((win (selected-window))
	 (nudge-X (* dm-map-scale (car dm-map-nudge)))
	 (nudge-Y (* dm-map-scale (cdr dm-map-nudge)))
	 (du-X (/ (- x nudge-X) dm-map-scale))
	 (du-Y  (/ (- y nudge-Y) dm-map-scale))
	 ;; account for any cells scrolled out of window
	 ;; hscroll is in characters; calc using frame char width
	 (char-width (frame-char-width (window-frame win)))
	 (scroll-X (* (window-hscroll win) char-width))
	 (scroll-Y (window-vscroll win t))
	 (du-scroll-X  (/ scroll-X dm-map-scale))
	 (du-scroll-Y  (/ scroll-Y dm-map-scale)))
    (cons (+ du-X du-scroll-X) (+ du-Y du-scroll-Y))))

(defun dm-map--pos-impl (&rest _)
  "Return map cell under mouse as a cons (X . Y)."
  (let* ((mpos (mouse-pixel-position))
	 (wpos (caddr
		(posn-at-x-y (cadr mpos) (cddr mpos)
			     (selected-frame)))))
    (dm-map--pos-pixels-to-cell (car wpos) (cdr wpos))))

(defun dm-map-add-cell (x y &optional arg)
  "Add the cell at X Y to and redraw the map.

When ARG is non nil reload level and tile tables."
  (interactive "nx:\nny:\np")
  (add-to-list 'dm-map-current-level-cells (cons x y))
  (dm-map-draw arg))

(defun dm-map-remove-cell (x y &optional arg)
  "Remove the cell at X Y to and redraw the map.

When ARG is non nil reload level and tile tables."
  (interactive "nx:\nny:\np")
  (setq dm-map-current-level-cells
	(remove (cons x y)
		dm-map-current-level-cells))
  (dm-map-draw arg))

(defun dm-map-toggle-cell
    (x y &optional arg)
  "Toggle visibility of the cell at X Y to and redraw the map.

When ARG is non nil reload level and tile tables."
  (interactive "nx:\nny:\np")
  (let ((cell (cons x y)))
    (setq dm-map-pos cell)
    (funcall (if (member cell dm-map-current-level-cells)
		 'dm-map-remove-cell
	       'dm-map-add-cell)
	     x y arg)))

(defun dm-map-mouse-toggle-cell ()
  "Toggle display of the map cell under mouse."
  (interactive "@")
  (let ((cell (dm-map--pos-impl)))
    (dm-map-toggle-cell (car cell) (cdr cell)
			current-prefix-arg)))

(defun dm-map-pos ()
  "Display the map cell under mouse."
  (interactive "@")
  (message "cell: %s" (dm-map--pos-impl)))

(defun dm-map-pos-pixels ()
  "Display window pos under mouse in pixels."
  (interactive "@")
  (let* ((mpos (mouse-pixel-position))
	 (wpos (posn-at-x-y (cadr mpos) (cddr mpos)
			    (selected-frame) t)))
    (message "%s" (caddr wpos))))

(defun dm-map-view-source (&rest _)
  "Display the SVG source of the map."
  (interactive)
  (when-let ((buf (get-buffer dm-map-preview-buffer-name)))
    ;; (with-current-buffer buf
    ;;   (fundamental-mode)
    ;;   (sgml-pretty-print (point-min) (point-max))
    ;;   (indent-region (point-min) (point-max))
    ;;   (dm-map-source-mode)
    ;;   (goto-char (point-min)))
    (dm-map-with-map-source
      (fundamental-mode)
      (sgml-pretty-print (point-min) (point-max))
      (indent-region (point-min) (point-max))
      (dm-map-source-mode))
    ))

(defun dm-map-menus-toggle-file-style (&optional arg)
  "Toggle map file menus between  \"toggle\" and \"radio\".

When prefix ARG is positive always use \"toggle\", when negitive
always use \"raidio\"."
  (interactive "p")
  (setq dm-map-menus-file-style
	(if (or (and arg (< 0 arg))
		(equal 'radio dm-map-menus-file-style))
	    'toggle
	  'radio)))

(defun dm-map-menus-toggle-file (file)
  "Toggle membership of FILE in `dm-map-files'."
  ;;(message "file toggle %s ⇒ %s" file dm-map-files)
  (if (member file dm-map-files)
      (setq dm-map-files (seq-filter
			  (lambda (cur-file)
			    (not (equal cur-file file)))
			  dm-map-files))
    (add-to-list 'dm-map-files file))
  (when dm-map-menus-file-redraw (dm-map-draw 1)))

(defun dm-map-menus-toggle-tag (files-select-tag file)
  "Make FILE the only member of FILES-SELECT-TAG in `dm-map-files'."
  (let ((fileset (dm-files-select files-select-tag)))
    ;;(message "file toggle %s ⇒ %s" file dm-map-files)
    (setq dm-map-files
	  (seq-uniq
	   (seq-filter
	    (lambda (cur-file)
	      ;;(message "file toggle %s ⇒ %s" file fileset)
	      (or (equal cur-file file)
		  (not (member cur-file fileset))))
	    (append (list file) dm-map-files)))))
  (when (equal :map-cells files-select-tag)
    (dm-map-switch-level file))
  (when dm-map-menus-file-redraw (dm-map-draw 1)))

(defun dm-map-switch-level (file)
  "Display FILE, a dungeon-mode map."
  ;; save the visible cell list for the prior level
  (setq dm-map-level-cells
	(plist-put dm-map-level-cells
		   dm-map-current-level
		   dm-map-current-level-cells))
  ;; change to the new level
  (setq dm-map-current-level file)
  ;; load any saved cells for the new level
  (setq dm-map-current-level-cells
	(plist-get dm-map-level-cells  dm-map-current-level)))

(defun dm-map-menus-files-impl (files-select-tag)
  "Create menu options for FILES-SELECT-TAG."
  (mapcar
   (lambda (map-file)
     (let ((tagged-files
	    (pcase dm-map-menus-file-style
	      ('radio (list 'dm-map-menus-toggle-tag files-select-tag map-file))
	      (_ (list 'dm-map-menus-toggle-file map-file)))))
       `[,(file-name-nondirectory map-file)
	 ,tagged-files
	 :help ,map-file
	 :style ,dm-map-menus-file-style
	 :selected (member ,map-file dm-map-files)]))
   (append (reverse (dm-files-select files-select-tag)))))

(defun dm-map-menus-tags-toggle (tag)
  "Toggle membership of TAG in `dm-map-menus-tags-list'."
  (interactive "sTag:")
  (when tag
    (unless (symbolp tag)
      (setq tag (intern tag)))
    (unless (equal t dm-map-tags)
      (if (member tag dm-map-tags)
	  (progn ;;(message "remove")
		 (setq dm-map-tags (delete tag dm-map-tags)))
	(add-to-list 'dm-map-tags tag))
      (when dm-map-menus-file-redraw
	(dm-map-draw current-prefix-arg)))))

(defun dm-map-menus-tags-impl ()
  "Display a menu of the available map tags."
  (mapcar
   (lambda (tag)
     `[,(symbol-name tag)
       (dm-map-menus-tags-toggle ',tag)
       :help "Toggle visability"
       :style toggle
       :selected (member ',tag dm-map-tags)])
   dm-map-menus-tags-list))

(defun dm-map-menus-toggle-draw-all (&optional arg)
  "Enable or disable drawing all cells of the current map..

When prefix ARG is positive always enable, when negitive always
disable."
  (interactive "p")
  (setq dm-map-menus-level-cells-draw-all
	(if (or (and arg (< 0 arg))      ;; force disable?
		dm-map-menus-level-cells-draw-all) ;; currently enabled?
	    nil ;; disable
	  t)))  ;; enable

(defun dm-map-predicated-drawing-p ()
  "Return t when predicated drawing is enabled."
  (not (equal t dm-map-tags)))

(defcustom dm-map-menus-play-mode nil
  "When t display for game-play instead of editing."
  :type 'boolean)

(defun dm-map-menus-files (&rest _)
  "Create menu options for tile files."
  (append (append (list "Tile-sets"
			(append (list "Mapping Documents")
				(dm-map-menus-files-impl :map-tiles))
			(append (list "Dungeon Levels")
				(dm-map-menus-files-impl :map-cells))))
	  (when (not (equal t dm-map-tags))
	    `(("Drawing Predicates" ,@(dm-map-menus-tags-impl))))
	  (list "-" "Settings")
	  (list '["Draw complete levels"
		  (setq dm-map-menus-level-cells-draw-all
			(not dm-map-menus-level-cells-draw-all))
		  :style toggle
		  :selected dm-map-menus-level-cells-draw-all
		  :help "When selected draw the complete map level each time."])
	  (list `["Play-mode (overlays)"
		  (setq dm-map-menus-play-mode (not dm-map-menus-play-mode))
		  :style toggle
		  :selected dm-map-menus-play-mode])
	  (list '["Predicated drawing"
		  (setq dm-map-tags (if (equal t dm-map-tags) nil t))
		  :style toggle
		  :selected (dm-map-predicated-drawing-p)])
	  (list '["Exclusive selections"
		  (setq dm-map-menus-file-style
			(if (equal 'toggle dm-map-menus-file-style)
			    'radio
			  'toggle))
		  :style toggle
		  :selected (equal 'radio dm-map-menus-file-style)
		  :help "Select one or several files from each category."])
	  (list '["Redraw when selecting"
		  (when (setq dm-map-menus-file-redraw
			      (not dm-map-menus-file-redraw))
		    (dm-map-draw 1))
		  :style toggle :selected dm-map-menus-file-redraw
		  :help "Control whether selecting files will redraw the map."])))

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
    (define-key map (kbd ".") 'dm-map-pos)
    (define-key map (kbd ",") 'dm-map-pos-pixels)
    (define-key map (kbd ";") 'dm-map-view-source)
    (define-key map [mouse-1] 'dm-map-mouse-toggle-cell)
    (define-key map [mouse-2] 'dm-map-pos)
    (define-key map "\C-c\C-c" 'dm-map-source-toggle)
    (define-key map "\C-c\C-x" 'ignore)
    map)
  "Mode keymap for `dm-map-mode'.")

(defconst dm-map-mode-menus
  '("Dungeon Mode"
    :help "Toggle map files"
    :filter dm-map-menus-files))

(easy-menu-define dm-map-menu
  dm-map-mode-map "Dungeon-mode menus" dm-map-mode-menus)

(defun dm-map-source-toggle (&rest _)
  "Toggle map between graphical and source views."
  (interactive)
  (if (derived-mode-p 'dm-map-mode)
      (dm-map-view-source)
    (with-current-buffer (get-buffer dm-map-preview-buffer-name)
      (dm-map-mode))))

;; based on share/emacs/27.0.90_2/lisp/image-mode.el
(defvar dm-map-source-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'dm-map-source-toggle)
    map)
  "Mode keymap for `dm-map-source-mode'.")

(define-derived-mode dm-map-source-mode xml-mode "MAP (source)"
  "Major mode for `dungeon-mode' map source.")

(define-derived-mode dm-map-mode image-mode "MAP"
  "Major mode for `dugeon-mode' maps.")

(defun dm-map--string-nopoe (obj)
  "Trim OBJ and remove text properties.

If OBJ is not a string return the empty string."
  (or (and (org-string-nw-p obj)
	   (progn (set-text-properties 0 (length obj) nil obj)
		  (org-trim obj)))
      ""))

(defun dm-map-row-load-and-draw (&optional inhibit-error)
  "Load map draw instructions from the table row at point."
  (interactive)
  (if (not (org-at-table-p))
      (unless inhibit-error (user-error "Not at a table"))
    (org-table-analyze)
    (let ((start-line (org-table-current-line))
	  cur-line
	  end-line
	  tile)
      (save-excursion
	(setq end-line (setq cur-line start-line))
	(org-table-goto-column 0)
	(while (and (not (setq tile (org-string-nw-p (org-table-get-field))))
		    (< 0 (cl-decf cur-line)))
	  (org-table-goto-line cur-line))
	(if (not tile)
	    (unless inhibit-error
	      (user-error "Can't find row identifier"))
	  (while (and (< (point) (org-table-end))
		      (not (equal tile
				  (dm-map--string-nopoe
				   (org-table-get-field)))))
	    (forward-line)
	    (cl-incf end-line)) ;; (org-table-goto-line)
	  (setq tile (intern (org-trim tile)))
	  (let* ((row-idx-list
		  (append
		   (list 0)
		   (number-sequence cur-line end-line)))
		 (rows
		  (mapcar
		   (lambda (idx)
		     (org-table-goto-line idx)
		     (mapcar
		      (lambda (col-idx)
			(dm-map--string-nopoe
			 (org-table-get-field col-idx)))
		      (number-sequence 1 org-table-current-ncol)))
		   row-idx-list)))
	    (puthash tile (seq-copy dm-map-cell-defaults) dm-map-tiles)
	    ;; (when-let* ((plist (gethash tile dm-map-tiles)))
	    ;;   (dolist (prop (number-sequence 2 org-table-current-ncol))
	    ;; 	(plist-put plist prop nil)))
	    (if (equal "X" (upcase (caar rows)))
		(dm-map-level-transform rows)
	      (dm-map-tiles-transform rows))
	    (dm-map-draw)
	    ;;(print (gethash 'tree dm-map-tiles)) (print rows)
	    (message "Loaded \"%s\" (%s..%s)" tile cur-line start-line)))))))

(defun dm-map-edit-mode-maybe ()
  "Switch to `dm-map-edit-mode' when editing map files."
  (interactive)
  (when (not (derived-mode-p 'dm-map-edit-mode))
    (when-let ((name (buffer-file-name)))
      (when (or (member name (dm-files-select :map-tiles))
		(member name (dm-files-select :map-cells)))
	(dm-map-edit-mode)))))

(defun dm-map-ctrl-c-ctrl-c (&optional _)
  "Drop in replacement for the  `org-mode' version.

Try only to changes the behaviour when in a table."
  (interactive)
  (unless (dm-map-row-load-and-draw t)
    (call-interactively #'org-ctrl-c-ctrl-c)))

(defvar dm-map-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'dm-map-ctrl-c-ctrl-c)
    map)
  "Mode keymap for `dm-map-edit-mode'.")

(define-derived-mode dm-map-edit-mode org-mode "MAP (edit)"
  "Major mode dervied for editing `dungeon-mode' maps.

This mode is dervied from from `org-mode', which see.

Rebind only C-c C-c, revert to calling `org-ctrl-c-ctrl-c' when
not within data row of a Map cell or tile table.")


;; (setq dm-map-files '("d:/projects/dungeon-mode/Docs/Maps/map-tiles.org"
;; 		     "d:/projects/dungeon-mode/Docs/Maps/Levels/A-Maze_level1.org"))
;; (setq dm-map-tags t)


;; DEVEL: give us a global key binding precious
(global-set-key (kbd "<f9>") 'dm-map-draw)
;;(global-set-key (kbd "M-<f9>") 'dm-map-view-source)

(provide 'dm-map)
;;; dm-map.el ends here
