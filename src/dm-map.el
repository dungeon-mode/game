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
				 (tile dm-map-feature))
  "Alist mapping table types to variables storing an hash-table for each.")

(defvar dm-map-level nil "Hash-table; draw code for game map levels.")
(defvar dm-map-level-cols '(x y path) "List of colums from feature tables.")
(defvar dm-map-level-key '(cons (string-to-number x) (string-to-number y))
  "Expression to create new keys in `dm-map-features'.")

(defvar dm-map-feature nil
  "Hash-table; draw code for `dungeon-mode' game map features.")

(defvar dm-map-feature-cols nil "List of colums from feature tables.")

(defvar dm-map-current-level nil
  "Hash-table; draw code for the current game map.")

(defvar dm-map-draw-prop 'path
  "Default attributes for cells as they are loaded.")

(defvar dm-map-cell-defaults `(,dm-map-draw-prop nil)
  "Default attributes for cells as they are loaded.")

(defsubst dm-map-header-to-cols (first-row)
  "Create column identifier from FIRST-ROW of a table."
  (seq-map
   (lambda (label)
     (intern (downcase label)))
   first-row))

(cl-defsubst dm-map-table-cell (cell
				(table dm-map-level)
				&optional &key
				(defaults dm-map-level-defaults))
  "Return value for CELL from TABLE or a default."
  (if (and cell table) (gethash cell table defaults) defaults))

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
  "Given trimmed WORD and FIRST-CHAR of a plan, DTRT."
  (message "[parse] word:%s type:%s" word (type-of word))
  (delq nil (list
	     (when (org-string-nw-p word)
	       (cond ((string-match-p "<[^ ]\\|>[^ ]\\|[^ ]<\\|[^ ]>" word) word)
		     ((string-match-p "[()]" word) (read word))
		     ((string-match-p "[ \t\n\"]" word)
		      (mapcan 'dm-map-parse-plan-part
			      (split-string word nil t "[ \t\n]+")))
		     (;;(memq first-char '(?M ?m ?V ?v ?H ?h ?S ?s ?A ?a ?L ?l))
		      (string-match-p "^[MmVvHhSsAaL][0-9.,+-]+$" word)
		      (list (string-to-char word)
			    (mapcar 'string-to-number
				    (split-string (substring word 1) "[,]"))))
		     (t (list (intern word))))))))


(cl-defmacro dm-map-tile-path (tile &optional &key (prop dm-map-draw-prop))
  "When TILE exists in `dm-map-feature' return PROP therefrom.

TILE is an hash-table key, PROP is a property (default: \"path\"."
  `(dm-map-path ,tile :table dm-map-feature :prop ',prop))

(defmacro dm-map-draw-test (svg &rest body)
  "Open (erase) buffer evaluate BODY and dump SVG."
  (declare (indent 1))
  `(with-current-buffer (pop-to-buffer "**dm-map**")
     (dm-map-mode)
     (erase-buffer)
     (goto-char (point-min))
     (insert (format-time-string "%D %-I:%M:%S %p on %A, %B %e, %Y\n"))
     ,@body
     (insert (format "\n\n[SVG DUMP]:\n%s" ,svg))))

(defun dm-map-load (&rest files)
  "Truncate and replace `dm-map-features' and `dm-map-levels' from FILES.

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
		   (let* ((tpom (org-element-property :begin table))
			  (prop dm-map-property)
			  (pval (org-entry-get nil prop))
			  (pvar (assoc pval dm-map-table-var-alist)))
		     (message "[map-load] tpom:%s prop:%s pval:%s pvar:%s"
			      tpom prop pval (type-of pvar)))
		   (goto-char (org-element-property :begin table))
		   (forward-line 1)
		   ;;(when (not (org-at-table-p)) (error "No table at %s %s" this-file (point)))
		   (when-let* ((tpom (save-excursion (org-element-property :begin table)))
			       (type (org-entry-get tpom dm-map-property t))
			       (type (intern type))
			       (var (cadr (assoc type dm-map-table-var-alist 'equal))))
		     ;;(let ((dm-table-load-table-var var)))
		     (dm-table-batch type)))))))
	 (or files dm-map-files))))

(defun dm-map-level-transform (table)
  "Transform a TABLE of strings into an hash-map."
  (let* ((cols (or dm-map-level-cols
		   (seq-map (lambda (label) (intern (downcase label)))
			    (car table))))
	 (cform
	  `(dm-coalesce-hash ,(cdr table) ,cols
	     ,@(when dm-map-level-key `(:key-symbol ,dm-map-level-key))
	     ;;:key-symbol (cons x y)
	     :hash-table dm-map-level
	     ;;(progn (message "[map-xform] cols:%s row:%s key:%s" ,cols row key))
	     (when (and (boundp 'path) path) (list 'path (dm-map-parse-plan-part path)))
	     ;;(list 'path nil)
	     ))
	 (result (progn
		   (print cform)
		   (eval `(list :load ,cform)))))))

(defun dm-map-feature-transform (table)
  "Transform a TABLE of strings into an hash-map."
  (let* ((cols (or dm-map-feature-cols (dm-map-header-to-cols (car table))))
	 (last-key (gensym))
	 (last-path (gensym))
	 (cform
	  `(dm-coalesce-hash ,(cdr table) ,cols
	     :hash-table dm-map-feature
	     :key-symbol last-key
	     (when (and (boundp 'path) (org-string-nw-p path))
	       (when (and (boundp 'tile) (org-string-nw-p tile))
		 (setq last-key (intern tile))
		 (setq last-path (plist-get (gethash last-key hash) 'path)))
	       ;;(message "[cell] tile:%s lkey:%s lentry:%s path:%s" tile last-key last-entry path)
	       (message "[cell] tile:%s lkey:%s lpath:%s" tile last-key last-path)
	       (setq last-path (nconc last-path (dm-map-parse-plan-part path)))
	       (list 'path last-path))))
	 (result (eval `(list :load ,cform))))))

(dm-table-defstates 'map :extract #'dm-map-load)
(dm-table-defstates 'tile :transform #'dm-map-feature-transform)
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
  (message "scale:%s cells:%s" scale cells)
  (dolist (cell cells cells)
    (message "cell:%s" cell)
    (cond
     ((dm-svg-dom-node-p cell 'text)
      (message "text:%s" cell)
      (dolist (args '((text-size 0) (x 0) (y 1)) )
	(dm-map--dom-attr-scale-nth cell scale args)))
     ((consp cell) (message "consp!%s" cell)
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
	(_ (message "unhandled %s => %s" (type-of cell) cell))
	))
     )))
;; (dm-map-default-scale-function '(100 . 1000)
;; 			       (dom-node 'text '((text-size .5)(x .2)))
;; 			       '(h (.1)) '(v (.2))'(m (.3 .4)) '(l (.5 .6)) '(x (.7 .8)) '(a (.7 .7 0 1 1 0 .14)))

;; image scale
;;;;;;;;;

;; simple draw methods for testing

(cl-defun dm-map-quick-draw (&optional
			     (cells (when dm-map-level (hash-table-keys dm-map-level)))
			     &key
			     path-attributes
			     (scale 100)
			     (scale-function 'dm-map-default-scale-function)
			     (tiles dm-map-feature)
			     (level dm-map-level)
			     (size '(800 . 800))
			     ;;(svg (dm-svg))
			     (svg-attributes '(:stroke-color white
					       :background-color purple
					       :stroke-width 1))
			     ;;viewbox
			     &allow-other-keys)
  "No-frills draw routine.

CELLS is the map cells to draw defaulting to all those found in
`dm-map-level'.  TILES is an hash-table containing draw code and
documentation, defaulting to `dm-map-feature'.  SCALE sets the
number of pixels per map cell.  SIZE constrains the rendered
image canvas size.  VIEWBOX is a list of four positive numbers
controlling the X and Y origin and displayable width and height.
SVG-ATTRIBUTES is an alist of additional attributes for the
outer-most SVG element.  LEVEL allows override of level info.

PATH-ATTRIBUTES is an alist of attributes for the main SVG path
element.  The \"d\" property provides an initial value to which
we append the drawing instructions to implement the features
included with each of CELLS therefor any inital value provided
for \"d\" (e.g. \"path-data\") *must* return the stylus to the
origin (e.g. M0,0) as it's final instruction.

SCALE-FUNCTION may be used to supply custom scaling."
  (ignore tiles cells scale size path-attributes scale-function)
  (let ((img (dm-svg :svg (apply 'svg-create (append (list (car size) (cdr size))
						     svg-attributes)))))
    ;;(add-path-data img "m100,100 h400 v400 h-400 v-400") (add-svg-element img ())
    ;;(apply    'add-path-data)
    (print `(cells ,cells))
    (append
     (list img)
     (mapcar
      (lambda (cell)
	(when-let ((path-list (car-safe (plist-get (gethash cell level)
						   dm-map-draw-prop))))
	  ;;(message "word:%s path:%s type:%s" cell path-list (type-of path-list))
	  (pcase path-list
	    ;; ignore references when drawing a complete level
	    (`(,(and x (guard (numberp x))). ,(and y (guard (numberp y))))
	     ;;(message "[draw] ignoreing reference in %s to %d %d" cell x y)
	     )
	    (t (message "[draw] TODO %s" path-list)))
	  (when (listp (car-safe path-list)) (message "strokes: %s" (mapconcat (lambda (stroke) (format "%s (%s)" stroke (type-of stroke))) path-list "\n\t")))
	  (cons cell path-list)))
      cells))
    img))

;; (setq dm-map-files '("d:/projects/dungeon-mode/Docs/Maps/DefaultFeatures.org"
;; 		     "d:/projects/dungeon-mode/Docs/Maps/testmap.org"))

;; (let ((svg (svg-create 800 800 :stroke-color 'green :stroke-width 10)))
;;   (svg-circle svg 200 200 100 :stroke-color 'red)
;;   (dm-map-draw-test svg (put-image (svg-image svg) (point-max))))

;; (let ((svg (dm-map-quick-draw)))
;;   (dm-map-draw-test svg
;;     (add-svg-element svg (dom-node 'circle '((cx . 200) (cy . 200)
;; 					     (r . 100) (stroke . red)
;; 					     (stroke-width . 10))))
;;     (render-and-insert svg)))



(defun dm-map-draw (&optional arg)
  "Draw all from `dm-map-level'.  With prefix ARG, reload first."
  (interactive "P")
  (when arg (dm-map-load))
  (let ((svg (dm-map-quick-draw)))
    (dm-map-draw-test svg (render-and-insert svg))))

;; global key binding
(global-set-key (kbd "<f9>") 'dm-map-draw)

;; basic major mode for viewing maps
(defvar dm-map-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'kill-buffer-and-window)
    (define-key map (kbd "Q") 'kill-buffer)
    map))

(define-derived-mode dm-map-mode fundamental-mode "MAP"
  "Major mode for `dugeon-mode' maps.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  zis is de line of cruft
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom dm-map-columns-header-alist '()
  "Columns to bind as an alist.")

(defvar dm-map-features-table-property-tag "MAP-FEATURES"
  "Table property flagging MAP-FEATURES.

TODO this will change with the \"striping\" design")

(defvar dm-map-on-row-hook '()
  "Called while reading, immediately after setting `dm-map--last-row'.

The default value removes hlines and saves and removes header rows.")

(defvar dm-map-default-row-functions `(dm-map-remove-hline
				       dm-map-stow-header
				       dm-map-count-rows)
  "While reading, functions for dm-map-on-row-hook.")

(defvar dm-map-feature '()
  "List of map features.  Populated while reading.")

(defvar dm-map-missing-feature-alist '()
  "List mentioned but undefined map features.

Populated while reading.")

(defvar dm-map-cell-feature-alist '()
  "Alist mapping map cells to refererenced features.

Populated while reading.  Map cells are expressed in the form:

  (x y dlv)")

;;;;;;; private vars

(defvar dm-map--last-row nil
  "While reading, the last row read.

When set to nil row is omited from read result.")

(defvar dm-map--last-feature nil
  "While reading, the last feature read.

When the identity column for a give row is blank this value
retains a key to update.  Value is either a symbol identifying
derd a map feature, or a map-level location, e.g. in the form:

  (x y dlv)")

(defvar dm-map--row-count nil
  "While reading, the count of undropped rows read.")

(defvar dm-map--last-header-row nil
  "While reading, the last header row found.")

;;;;;;; some default hooks for row triage

(defun dm-map-remove-hline (row)
  "While reading, suppress 'hline."
  (if (equal 'hline row)
      (prog1  (setq dm-map--last-row nil)
	(message "removing hline"))
    (message "not an hline:%s" row)
    t))

(defun dm-map-stow-header (row)
  "While reading, suppress and save header lines.

Header is saved in 'dm-map--last-header-row' overwriting any
value already there.  ROW is ignored."
  (ignore row)
  ;;(message "col:%s car:%s downcase-car:%s")
  (not (when (and (car-safe dm-map--last-row)
		  (not (string-blank-p (car dm-map--last-row)))
		  (not (eq (aref (car dm-map--last-row) 0)
			   (downcase (or (aref (car dm-map--last-row) 0)
					 0)))))
	 (message "found header-row:%s" dm-map--last-row)
	 (setq dm-map--last-row nil
	       dm-map--last-header-row dm-map--last-row))))

(define-inline dm-map-count-rows (row)
  "While reading, count lines that will be added.

Uses `dm-map-last-row', ROW is ignored."
  (message "hello from counter"))


(dm-map-load-feature-files  "../Docs/Maps/Design.org" "../Docs/Maps/test.org")

;;;;;;; read support

(defun dm-map-load-tagged-tables-in-files (predicate &rest org-files)
  "Return a list of rows from tables with TAG in ORG-FILES.

ORG-FILES is a list of file-names as strings.  PREDICATE is a
function which receives the point-or-marker (POM) indicating the
start position for a given `org-table' and should return true if
the table is to be included.

Results are a list each each found column as a list of strings.
Errors are thown if a file doesn't exist, cannot be opened, etc."
  (let ( dm-map--last-row
	(dm-map--row-count 0)
	(dm-map-on-row-hook
	 (seq-map (lambda (hook-func)
		    (message "adding %s" hook-func)
		    (add-hook 'dm-map-on-row-hook hook-func t t)
		    ) dm-map-default-row-functions)))
    ;; (dolist (hook-func dm-map-default-row-functions)
    ;;   (add-hook 'dm-map-on-row-hook hook-func t t))
    (seq-map
     (lambda (this-file)
       (with-temp-buffer
	 (insert-file-contents this-file)
	 (org-element-map (org-element-parse-buffer) 'table
	   (lambda (table)
	     (let ((tpom (org-element-property :begin table)))
	       (and (funcall predicate tpom)
		    (save-excursion
		      (goto-char tpom)
		      (delete
		       nil
		       (mapcar
			(lambda (row)
			  (setq dm-map--last-row row)
			  (when  (and (run-hook-with-args-until-failure
				       'dm-map-on-row-hook
				       dm-map--last-row))
			    (append (or (and (listp dm-map--last-row ) dm-map--last-row)
					(list dm-map--last-row))
				    (list (setq dm-map--row-count
						(1+ dm-map--row-count)))))
			  ) (org-table-to-lisp)))
		      ))))))) org-files)))

(defun dm-map-load-feature-files (&rest org-files)
  "Return list of map features as defined in ORG-FILES.

Results are a single list of feature attributes each in the form:

   ( \"FEATURE\" \"DRAW CODE\" \"DOCSTRING\" \"NARRATIVE\" )

Map feature tables are those contained in a section with the
MAP-FEATURES property set to a truthy value.  Errors are thown
if a file doesn't exist, cannot be opened, etc."
  (apply 'dm-map-load-tagged-tables-in-files
	 (lambda (tpom)
	   (org-entry-get tpom dm-map-features-table-property-tag)
	   ) org-files))

(defun dm-map-load-level (name &rest org-files)
  "Return a list of cells in map-level NAME from ORG-FILES."
  (apply 'dm-map-load-tagged-tables-in-files
	 (lambda (tpom)
	   (message "%s=%s" name (org-entry-get tpom "NAME"))
	   (string= name (org-entry-get tpom "NAME"))
	   ) org-files))

;;(dm-map-load-feature-files  "../Docs/Maps/Design.org" "../Docs/Maps/test.org")
;;(car(dm-map-load-feature-files "../Docs/Maps/test.org"))
;;(dm-map-load-level "regression-test-map-level" "../Docs/Maps/Design.org")
;; (apply 'dm-map--parse-map-level (dm-map-load-level "regression-test-map-level" "../Docs/Maps/Design.org"))
;; ;; examples from early dm-util test tests
;; (let ((h (dm-coalesce-hash (("h1" "H2" "H3")
;; 				  ("r1" "V12" "V13")
;; 				  ("r2" "V22" "V23")
;; 				  ("r3" "V32" "V33"))
;; 	     (id h2)
;; 	   :hash-table #s(hash-table size 30 test equal)
;; 	   :start-column 1
;; 	   (list 'id id 'h2 h2))))
;;   ;;(print h)
;;   h)
;; (dm-coalesce-hash( )(_) :after(puthash(nth 0 row)(cdr row)hash))

(defun dm-map--parse-map-level (&rest rows)
  "Return a list of map-level cells from `org-table' ROWS."
  (mapcar (lambda (cell)
	    (cl-destructuring-bind (x y plan dstr nstr) cell
	      (list 'pos (cons (string-to-number x)
			       (string-to-number y))
		    'plan plan
		    'docs  (delete "" (list dstr nstr))
		    ))) rows))

;; TODO we need to split out :keywords that may be run together
;; with repeated feature names
(defun dm-map--parse-path-command (svg-path-command-string)
  "Split SVG-PATH-COMMAND-STRING into command and args.

Returned list has the command ([MmVvHhSsAaLl]) as car followed by
the argument sequence each as a seperate list item."
  (list (substring svg-path-command-string 0 1)
	(mapcar 'string-to-number
		(split-string (substring svg-path-command-string 1)
			      "[,]"))))
;;(dm-map--parse-path-command "A-.01,1.20")
;;(split-string "A-.01,1.20" "[0-9,.-]" t nil)

(defun dm-map--parse-plan (hash)
  "Cross-map HASH keys to resolve features as SVG code.

TODO: return a list of unresolved symbols?"
  (dm-map-maybe-resolve-path hash))
  ;; (maphash (lambda (k v)
  ;; 	     (when (plist-member v 'paths)
  ;; 	       (plist-put v 'paths
  ;; 			  (mapcan (lambda (ref)
  ;; 				    (dm-map--maybe-resolve-path hash ref
  ;; 								nil t nil))
  ;; 				  (plist-get v 'paths))))
  ;; 	     ) hash))

;(gethash 'c-NSN (apply 'dm-map-defeatures (dm-map-load-feature-files "../Docs/Maps/test.org")))


(with-temp-buffer
  (let ((hash (apply 'dm-map-defeatures
		     (dm-map-load-feature-files "../Docs/Maps/Design.org"
						"../Docs/Maps/test.org")))
	(standard-output (current-buffer))
	print-length print-level)
    (seq-map (lambda (key)
	       (prin1 (format "\n\n[%s]\n" key))
	       (print (gethash key hash))
	       ) (hash-table-keys hash)))
  (buffer-string))

(let ((standard-output (current-buffer)) print-length print-level something) (insert "\n;; ") (prin1 something))


(cl-defun dm-map-quick-draw (features
			     cells
			     &key
			     (scale 100)
			     (size '(32 . 40))
			     viewbox
			     svg-attributes
			     path-attributes
			     (scale-function 'dm-map-default-scale-function)
			     ;; TODO repeat above patter for "late-binding-references"
			     ;; for binding of features from cell draw instructions
			     &allow-other-keys)
  "No-frills draw routine.

FEATURES is a map of symbols to draw code and documentation.
CELLS is the list of map cells to draw.  SCALE sets the number of
pixels per map cell.  SIZE constrains the rendered image canvas
size.  VIEWBOX is a list of four positive numbers controlling the
X and Y origin and displayable width and height.  SVG-ATTRIBUTES
is an alist of additional attributes for the outer-most SVG
element.

PATH-ATTRIBUTES is an alist of attributes for the main SVG path
element.  The \"d\" property provides an initial value to which
we append the drawing instructions to implement the features
included with each of CELLS therefor any inital value provided
for \"d\" (e.g. \"path-data\") *must* return the stylus to the
origin (e.g. M0,0) as it's final instruction."
  (ignore features cells scale size viewbox svg-attributes path-attributes scale-function)
  (let ((paths nil))
    )
  )



;;=============================================================================
;; :TODO:table-striping:
;; obviously we need to be smarter about column headings and more flexiable
;; in terms of not requring columns which won't be used to bind data in a
;; given table.

;; Given:

;;  Map tables are not a special case but rather the protypical case
;;  from which required complexity will be established.

;; Consider:

;;  1. Tables are stored in org files or sections having a
;;     `dungeon-mode' property specifying the scope of the data-set.
;;   1.1 Tables must be nested within a section or as a special case
;;       before first section, if any.
;;   1.2 Any section level is accepted
;;   1.3 The 'dungon-mode property is added the containing section,
;;       or to the document when at the top of the page.
;;   1.4 The 'dungeon-mode' property is not inheretable, e.g. by sub-
;;       elements.

;;  2. Tables may be further identified used the built-in NAME
;;     property.
;;   2.1 Given table implementations (e.g. map-features) may ignore
;;       NAME properties.
;;   2.2 Given table implementations (e.g. map-level) may require
;;       NAME properties.
;;   2.3 Name properties are positioned as per the dungon-mode
;;       property, that is within the property drawer or otherwise
;;       immediately after the beginning of the section, or in the
;;       properties drawer or otherwise at the top of the file.

;;  3. Given table implementations may support different modes of
;;     altering the target data-set as a result of reading the table
;;     content.
;;   3.1 Create - create or replace a data-set or the NAMed feature
;;                thereof.
;;   3.2 Add - add records to a data-set or the NAMED feature thereof.
;;  4. Each table must contain an header row.
;;   4.1 The header row appears exactly once as the first row of the table.
;;   4.2 Given table implementations may accept a fixed or variable column arrangement.
;;   4.3 Table implementations with variable column arrangments may define given columns as required or optional.
;;    4.3.1 Rows which omit required columns are assmed to take their values for missing columns from the prior row.
;;    4.3.2 Rows which omit all required columns or which begin the first column with a comment are considered comments and ignored.
;;   4.4 Table implementation with variable column arrangements and which support Add mode may allow incrementally accumulating row values (e.g. "striping").
;;    4.4.1 Tables are striped only when all identifiying properties are identical (`eq').
;;    4.4.2 Tables are striped only when all required columns are included.
;;=============================================================================


;; New plan:
;; Maintain two control lists as we walk the list once.
;;   lost: plist mapping symbols mentioned but not defined to a
;;   list of referent indexes within 'paths
;;  found: symbols as we find them

;; start with something to keep things tidy

(cl-defmacro dm-with-org-table-hash
    ((&whole table first-row)
     (&whole headings (first-heading (car first-row)))
     &body (body nil body-forms-p)
     &key (hash #s(hash-table size 30 test equal) hash-p)
     (key first-heading)
     intern-keys)
  "Read `org-mode' tables into HASH and execute BODY-FORMS.

This function accepts string TABLES in the form of a list of
lists of strings:

  ((\"h1\" \"h2\")
   (\"c1.1\" \"c2.2\")
   (\"c2.1\" \"c2.1\"))

TABLE may also be an expression returning a such a list.

HEADINGS is a list of symbols for binding each value from the
corrisonding column prior to evaluating BODY for a given
row (e.g. outter list item) in TABLE.

Each of HEADINGS may be an underscore to suppressing binding
values from that position.

When KEY is non-nil it is either a symbol equal to one of
HEADINGS or an expression evaluated after and in the same
context when BODY has yielded a non-nill result.  The default
value 'first-heading takes the value from the first column.

When intern-keys is non-nill, the value or result of KEY is
interned prior updating or creating an entry in HASH."
  (ignore table headings)
  (or (and (null body-forms-p)
	   hash)
      body))

(cl-defun dm-map-defeatures
    (&rest feature-attributes-list
	   &key (hash #s(hash-table size 30 test equal))
	   &allow-other-keys)
  "Read map features from FEATURE-ATTRIBUTES-LIST.

TODO Update this if the hash-table approach pans-out

FEATURES-ATTRIBUTUES-LIST is as from `dm-map-load-features' which
see.  Returns a plist mapping each feature to draw code and
documentation.

  (SYMBOL (DRAW-CODE DOCSTRING NARRATIVE)
   SYMBOL2 ... )

Where SYMBOL is a unique identifer for composing the feature into
other features or map cells and DRAW-CODE is a list of raw SVG
path-data as strings.  Features referenced within DRAW-CODE
are (recursively) resolved and added.  DOCSTRING and NARRATIVE
are help test for DMs and players, respectively.

Each SYMBOL in the returned list is unique.  In case SYMBOL is
mentioned more than once in FEATURE-ATTRIBUTES-LIST the final
occurance is included.

NOTE: documentation related features are not implemented.
TODO write 'decode-feature-name' to turn c-N into Corridor North
TODO deal with docstrings"
  (declare (indent 0))
  (let ((names
	 (mapcar 'intern
		 (seq-uniq (mapcar 'car feature-attributes-list)
			   'string=))))

    ;; Corwin: I'm somewhat attracted to plugability of having SVG
    ;; parsing completely seperated out like this but not with
    ;; repeatedly re-walking the list.
    (dolist (strings feature-attributes-list)
      (dm-map--init-hash-entry hash strings names))
    (dm-map--parse-plan hash)

    hash
    ;; (dolist (tbl tables features)
    ;;   (push (cdddr tbl) features))
    ))


;; (princ (apply 'dm-map-defeatures (dm-map-load-feature-files  "../Docs/Maps/test.org" "../Docs/Maps/test.org")))

(cl-defun dm-map--init-hash-entry
    (hash (feature plan docstring narrative) feature-list)
  "Add or update an entry in HASH for STRINGS.

Strings are a list in the form:
  (\"FEATURE\" \"PLAN\" \"DOCSTRING\" \"NARRATIVE\")

FEATURE-LIST is a list of all known features.

HASH entries are keyed on FEATURE interned as a symbol and
associated to a plist with the following keys:

  plan - the original unmodified draw plan, as a string

  paths - list of feature references and svg path commands.
  Path commands are further split into cons cells in the
  form (COMMAND . ARG-LIST) while referenced features appear as
  symbols.

  elements - TODO list of svg elements, generally used to
  embelish the feature.

  docs - list of documentation, starting with the most secret."
  (let* ((fsymbol (intern feature))
	 ;;(known (maphash (lambda (k v) k) hash))
	 (docs (delete "" (list docstring narrative)))
	 (words (split-string-and-unquote plan))
	 (commands (mapcar (lambda (step)
			     ;;(princ `(,step . ,feature-list))
			     ;;(message "step-type:%s" (type-of step))
			     ;;(message "li-type:%s" (type-of (car feature-list)))
			     (if (member (intern step) feature-list)
				 (intern step)
			       (dm-map--parse-path-command step)))
			   words)))
    (puthash fsymbol `(plan ,plan paths ,commands docs ,docs) hash)))

;; (gethash 'c-NS+sE (apply 'dm-map-defeatures (dm-map-load-feature-files  "../Docs/Maps/Design.org" "../Docs/Maps/test.org")))

(cl-defun dm-map--maybe-resolve-path
    (hash path
	  &optional error
	  (recurse t)
	  (update t)
	  (resolver 'dm-map--maybe-resolve-path))
  "Resolve PATH if it is a reference to draw code in HASH.

Returns 'path, a list of draw commands and arguments as cons
cells intermixed with `svg' dom-nodes and unresolved symbols,
when PATH is a symbol and exists as a key in hash which contains
a 'path property.

When ERROR is t signal an error upon finding an unresolvable
symbol, otherwise the symbol is returned enclosed in a list or,
in the case of recusive processing, left in their places within
the list eventually returned.

When RECURSE is t, similarly recurse to resolve symbols within
referenced draw commands.

When UPDATE is t update refferents while resolving symbols in
referenced draw commands. During recursion UPDATE is the symbol
to be modified.

RESOLVER provides an alternative to this function to be used to
resolve symbols found within reference paths.  It is called with
the same inputs as this function and expected to return a list of
symbols, `sgv' dom-elements, and draw commands as described
above."
  (or (and (symbolp path)
	   (if-let* ((ref (gethash path hash))
		     (ref-path (plist-get ref 'path)))
	       (if (null recurse) ref-path
		 ;; recursion
		 ;;(mapcan (or resolver 'dm-map--maybe-resolve-path) ref-path)
		 (if (null update)
		     (mapcan (lambda (ref-path-part)
			       (funcall resolver hash ref-path-part
					error recurse update resolver)
			       ) ref-path)
		   ;; TODO handle updating during recursion
		   ref-path
		   )
		 )
	     (when error
	       (error "Reference to missing or incomplete path: %s" path))))
      (list path)))

(cl-defun dm-map-maybe-resolve-path
    (hash &optional
	  key error
	  (recurse t) (update t) (resolver 'dm-map--maybe-resolve-path))
  "Attempt to resolve symbols from paths attributes keys of HASH.

If KEY is non-nil (recursively) process this key only.  ERROR may
be set to t to cause an error to be throw in case a symbol cannot
be resolved.  RECURSE (default: t) may be set to nil to suppress
recursive resolution of symbols found while dereferencing.
UPDATE (default: t) controls whether to update the KEY of
HASH (or all keys of HASH when KEY was nil).

RESOLVER (default: this function) is called to perform recursive
resolution including any required updates to HASH."
  (if (null key)
      ;; TODO: special case: no key means try resolving symbols in 'path of all keys
      (mapc (lambda (key)
	      (dm-map-maybe-resolve-path hash key error recurse update resolver)
	      ) (hash-table-keys hash))
    (if-let* ((ref (gethash key hash))
	      (ref-paths (plist-member ref 'path)))
      (seq-map-indexed
       (lambda (path ix)
	 (if (not (symbolp path))
	     (prog1  path (message "not a symbol:%s" path))
	   (if (not update)
	       (prog1 (funcall resolver hash path error recurse update resolver)
		 (message "not updating, recurse: %s" path))
	     (setcar
	      (nth ix ref-paths)
	      (funcall resolver hash path error recurse update resolver))
	     (message "updated %s => %s" path ref-paths)))
	 ) ref-paths)
      ref-paths)))

(defun dm-map--keys-by-desc-path-ref-count (hash)
  "Return keys of HASH with at least on symbol in path.

Keys are sorted from fewest to least symbols in their respective
'path attributes Keys without an 'path property or which 'path
contains no symbols are omitted."
  (let (ap ac bp bc drop-list)
    (seq-drop
     (lambda (x) (seq-contains x drop-list))
     (sort
      (hash-table-keys hash)
      (lambda (a b)
	(or (prog1 (null (setq bp (plist-get (gethash b hash) 'paths)))
	      (unless bp (push b drop-list)))
	    (and (prog1 (setq ap (plist-get (gethash a hash) 'paths))
		   (unless ap (push a drop-list)))
		 (prog1 (setq ac (seq-count 'symbolp
					    (plist-get ap 'paths)))
		   (unless (and ac (> ac 0))
		     (push a drop-list)))
		 (or (null (prog1 (setq bc (seq-count 'symbolp
						      (plist-get bp 'paths)))
			     (unless (and bc (> bc 0))
			       (push b drop-list))))
		     (> bc ac)))))))))
;(dm-map--keys-by-desc-path-ref-count (apply 'dm-map-defeatures (dm-map-load-feature-files "../Docs/Maps/test.org")))

;; I invented intern, but dumber.
;;(equal (car (let ((x "foo")) (read-from-string x))) (intern "foo")) => t
;; <mplsCorwin> bpalmer: I have to add - I'm going to feel pretty foolish
;;       when^Dif I do need to intern here.  It was literally the first thing I
;;       thought of.  I just keep thinking that I'd be doing that rather alot of
;;       times as I loop cols across rows across tables across files, and all
;;       that long before anything is getting bound for eval.
;; <bpalmer> drappist: if you just want to read the emacs manual, you can just
;;       search for it on the web.
;; <bpalmer> mplsCorwin: intern just makes a symbol, it doesn't bind it or
;;       evaluate it
;; <mplsCorwin> ok, sold.  thanks once again.
;; <bpalmer> e.g., (symbol-value (intern "something-never-before-seen")) gives
;;       you a void variable complaint, just like you had typed
;;       something-never-before-seen

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; This does resolve refs but only one layer deep; won't deal with nexted use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun dm-map--parse-plan (hash)
;;   "Cross-map HASH all hash keys to resolve features to SVG code."
;;   (maphash (lambda (outer-k outer-v)
;; 	     (when (plist-member outer-v 'paths)
;; 	       (plist-put outer-v
;; 			  'paths
;; 			  (mapcan (lambda (word)
;; 				    (message "refs:%s" (gethash word hash word))
;; 				    (or (plist-get (gethash word hash word) 'paths)
;; 					(list word))
;; 				    ) (plist-get outer-v 'paths))))
;; 	     ) hash))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; probably not..
;; (defun dm-map-resolve-feature (hash feature &optional inhibit-error)
;;   "Return the drawing instructions for FEATURE using HASH.
;;
;; If INHIBIT-ERROR is t, return FEATURE rather than raising an error."
;;   (or (gethash feature hash (and inhibit-error feature))
;;       (error "No feature \"%s\" found" feature)))

;; (let (print-level print-length)
;;   (prin1-to-string (apply 'dm-map-defeatures
;; 			  (dm-map-load-feature-files "../Docs/Maps/test.org"
;; 						     "../Docs/Maps/test.org"))))

;; (let ((sl (list "aa" "bb" "aa" "cc"))) (seq-uniq sl 'string=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stopping with this in favor of getting the above procedural
;;; approach working first.

(deftype dm-map-viewbox () ;;(&optional minX minY maxW maxH)
  "Type predicate for SVG view-box.

A viewbox is a list of four integers.

  (list x-pos y-pos width height)

TODO MINX, MINY, MAXW and MAXH can be used to further constrain each value."
  '(list number number number number))

(deftype dm-map-size (&optional min max)
  "Type predicate for image size:

  (list x y)

Where x and y are both either positive integers (0..*) or floats
between zero and one. (0..1)."
  `(or null (list (number 0 ,min)
		  (number 0 ,max))))

;; (defclass dm-map ()
;;   ((svg :type (or dm-svg-or-nil dm-svg) :initarg :svg :initform (dm-svg))
;;    (size :type dm-map-size :initarg :size :initform (list 1.0 1.0))
;;    (scale :type (integer 1 100) :initarg :scale :initform 100)
;;    (view-box :type (or null dm-map-viewbox) :initarg :view-box :initform nil)
;;    (svg-attributes :type list :initarg :svg-attributes :initform nil)
;;    (path-attributes :type list :initarg :path-attributes :initform nil)
;;    (scale-function :type function :initarg 'dm-map-s))
;;   :documentation
;;   "Create and update RPG maps displayed as inline SVG images.")

;; (cl-defmethod initialize-instance :after ((object dm-map) &rest _)
;;   "Initialize instance slots after inital argument processing."
;;   (with-slots (svg size svg-attributes) object
;;     (unless (dm-svg-p 'svg)
;;       (if (dm-svg-dom-node-p object 'svg)
;; 	  (setq svg (dm-svg :svg svg))
;; 	(let ((so (svg-create (car size) (cdr size))))
;; 	  (dom-set-attributes so svg-attributes)
;; 	  (setq svg (dm-svg :svg so)))))))

;;(dm-map)

;; *** public variables :code:
;; TODO defcustom these instead?
(defvar dm-map-scale 100 "Drawing scale; pixles per 10' of map.

TODO: generate this var from the table above")

(defvar dm-map-view-box nil "Croping view-box for the SVG tag.

TODO: see `dm-scale' for detail.")

;; *** private variables :code:
(defvar dm-map--path-data nil "Main SVG path data as a string.

TODO: see `dm-scale' for detail.")

(defvar dm-map--map-data nil "SVG data as a string.

TODO: see `dm-scale' for detail.")

(defvar dm-map--svg-data "Non-path SVG instructions.

Filled by `dm-map--append' for `dm-map-append' while appending to
path data.")

;; ** functions :code:
;; These functions support noweb syntax with ~org-babel~ to eliminate
;; boilerplate for basic cursor based drawing using the
;; [[https://css-tricks.com/svg-path-syntax-illustrated-guide/][SVG
;; path element]].

(defmacro dm-map-append (&rest forms)
    "Add FORMS to map.

Generally meaning, append to the \"d\" attribute value for the
primary path representing chambers, corridor and secret doors in
map.

  FORMS may be any of:
   - strings
     - when starting with a \"<\"*, literal SVG source
     - otherwise literal path data
   - keywords
     - taken as SVG basic drawing elements
     - followed by a plist taking the form:
       ( :ATTRIBUTE1 \"value1\" ... )
   - functions
     - called without arguments
     - return treated as per FORMS

Returns a cons cell in the form:

  ( SVG-STRING . PATH-STRING )

Where SVG-STRING is SVG code other than the main draw path and
PATH-STRING is the path-data for the main-draw path."
    ;; TODO put some code here
    (let* (new-svg
	   (f (apply-partially 'dm-map--append 'new-svg))
	   (new-path (mapconcat f forms)))
      `(cons (setq dm-map--svg-data
		   (concat dm-map--svg-data ,new-svg))
	     (setq dm-map--path-data
		   (concat dm-map--path-data ,new-path)))))

(defun dm-map--append (svg-elements form)
  "Implemention for `dm-map-append' which see.

Return strings remaining after recursively processing FORM.
SVG-ELEMENTS are a list to which any SVG (e.g. non-path) elements
found will be added."
  (cond (nil "nope")
	(t "yep")
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; newer cruft

(defun dm-map-load-feature-files--CRUFTY (&rest org-files)
  "Return list of map feature tables from ORG-FILES.

ALERT keeping this version around for a bit; returns table hashes

Map feature tables are those contained in a section with the
MAP-FEATURES property set to a truthy value.  Errors are thown
if a file doesn't exist, cannot be opened, etc."
  (let ((files org-files)
	this-file
	feature-tables)
    (while (setq this-file (pop files))
      (with-temp-buffer
	(insert-file-contents this-file)
	(org-element-map (org-element-parse-buffer) 'table
	  (lambda (table)
	    (and (org-entry-get (org-element-property :begin table)
				dm-map-features-table-property-tag)
		 (push table feature-tables))))))
    feature-tables))

(provide 'dm-map)
;;; dm-map.el ends here
