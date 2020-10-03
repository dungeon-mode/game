;;; dm-draw.el --- render dungeon-mode map as SVG     -*- lexical-binding: t; -*-

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

;;; Requirements:

(eval-when-compile (require 'cl-lib))

(let ((load-path (append '(".") load-path)))
  (require 'dungeon-mode)
  (require 'dm-util)
  (require 'dm-env))

;;; Code:

;; testcase
(when nil
  (with-current-buffer (pop-to-buffer " *draw test*")
    (erase-buffer)
    (let (i (dm-draw-size (cons 48 24))
	    (dm-draw-scale 17)
	    svg)
      (setq svg (dm-draw (mapcar 'dm-map-cell2
				 (hash-table-keys dm-map-level))
			 ;;:layers '((background scaled)(path path) (overlay svg))
			 ))
      (setq i (svg-insert-image svg))
      (sit-for 3)
      ;;(dm-draw '((:cell (3 . 3) path ((h (2))(v (2))(h (-2))(v (-2))))) :svg svg)
      (svg-possibly-update-image svg))))

(defvar dm-draw-scale 60 "Pixels per dungeon unit.")
(defvar dm-draw-nudge (cons 1 1) "Frame padding in dungeon units.")
(defvar dm-draw-size (cons 6 6) "Drawing size in dungeon units.")

(defvar dm-draw-svg-attr-alist nil "SVG attributes as an alist.")

(defvar dm-draw-layer-attr-alist
  '((t ((fill . "none")
	(stroke . "black")
	(stroke-width . "1")))
    (water ((fill . "blue")
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
  "Default attributes for SVG and path layers.")

(defvar dm-draw-layer-alist '((background scaled)
			      (underlay dom)
			      (water)
			      (beach)
			      (stairs)
			      (neutronium)
			      (path)
			      (tracks scaled)
			      (overlay dom)
			      (decorations)
			      (focus-overlay scaled))
  "Alist of layers and expected data format.")

(defvar dm-draw-square nil
  "The current square when drawing.")

(defvar dm-draw-dom-interpolate-function #'dm-draw-dom-interpolate-default
  "Function to call to replace vars (\"$foo\") in Text nodes.")

(defmacro dm-draw--dqnconc (&rest lists)
  "Return LISTS joined by `nconc' after removing nil."
  (declare (indent 0))
  `(delq nil (nconc (progn ,@lists))))

(defmacro dm-draw--dqnmap (list sym &rest body)
  "Return LIST joined by `mapcan' and removing nil.

SYM is the loop iterator as BODY processes each list item."
  (declare (indent 2))
  `(delq nil (mapcon (lambda (,sym) ,@body) ,list)))

(defmacro dm-draw--adqnmap (list var expr &rest body)
  "Evaluate BODY with EXPR result bound to VAR and append to LIST."
  (declare (indent 3))
  `(append ,list (delq nil (mapcar (lambda (,var) ,@body) ,expr))))
;; (dm-draw--adqnmap '(a b c) x '(d e) x)

(defmacro dm-draw--M (scale nudge pos paths)
  "Return absolute move to POS at NUDGE and SCALE, then PATHS."
  (declare (indent 3))
  `(when ,paths
    (append
     (list (list 'M (list
		     (+ (* (car ,scale) (car ,nudge)) (* (car ,scale) (car ,pos)))
		     (+ (* (cdr ,scale) (cdr ,nudge)) (* (cdr ,scale) (cdr ,pos))))))
     ,paths)))

(defsubst dm-draw-tile-area (tile &optional layers)
  "Find the area covered by LAYERS of TILE."
  (let ((minX 0) (maxX 0)
	(minY 0) (maxY 0)
	X Y)
    (dolist (prop (or layers (mapcar 'car dm-draw-layer-alist)))
      (setq X 0 Y 0)
      (dolist (cell (dm-draw-resolve-tile-path tile prop))
	(message "area p:%s c:%s" prop cell)
	(pcase cell
	  (`(M  ,_) nil)
	  (`(,(or 'm 'l 'L)
	     (,(and x (guard (numberp x)))
	      ,(and y (guard (numberp y)))))
	   (progn
	     (setq X (+ X x)
		   Y (+ Y y))
	     (if (< X minX) (setq minX X)
	       (when (> X maxX) (setq maxX X)))
	     (if (< Y minY) (setq minX Y)
	       (when (> Y maxY) (setq maxY Y)))))
	  (`(,(or 'h 'H)
	     (,(and d (guard (numberp d)))))
	   (progn
	     (setq X (+ X d))
	     (if (< X minX) (setq minX X)
	       (when (> X maxX) (setq maxX X)))))
	  (`(,(or 'v 'V)
	     (,(and d (guard (numberp d)))))
	   (progn
	     (setq Y (+ Y d))
	     (if (< Y minY) (setq minY Y)
	       (when (> Y maxY) (setq maxY Y)))))
	  )
	;;(message "area:%s" (list 'X X minX maxX 'Y Y minY maxY 'cell cell))
	))
    (setq minX (round (* -1 minX)) minY (round (* -1 minY)))
    (setq maxX (ceiling (+ maxX minX)) maxY (ceiling (+ maxY minY)))
    (cons (cons minX minY)
	  (cons (if (< 1 maxX) maxX 2)(if (< 1 maxY) maxY 2)))))

(defsubst dm-draw-path-area (plan)
  "Find the area covered by PLAN."
  (let ((X 0) (minX 0) (maxX 0)
	(Y 0) (minY 0) (maxY 0))
    (dolist (cell plan)
      (pcase cell
	(`(M  ,_) nil)
	(`(,(or 'm 'l 'L)
	   (,(and x (guard (numberp x)))
	    ,(and y (guard (numberp y)))))
	 (progn
	   (setq X (+ X x)
		 Y (+ Y y))
	   (if (< X minX) (setq minX X)
	     (when (> X maxX) (setq maxX X)))
	   (if (< Y minY) (setq minX Y)
	     (when (> Y maxY) (setq maxY Y)))))
	(`(,(or 'h 'H)
	   (,(and d (guard (numberp d)))))
	 (progn
	   (setq X (+ X d))
	   (if (< X minX) (setq minX X)
	     (when (> X maxX) (setq maxX X)))))
	(`(,(or 'v 'V)
	   (,(and d (guard (numberp d)))))
	 (progn
	   (setq Y (+ Y d))
	   (if (< Y minY) (setq minY Y)
	     (when (> Y maxY) (setq maxY Y)))))
	)
      ;;(message "area:%s" (list 'X X minX maxX 'Y Y minY maxY 'cell cell))
      )
    (setq minX (round (* -1 minX)) minY (round (* -1 minY)))
    (setq maxX (ceiling (+ maxX minX)) maxY (ceiling (+ maxY minY)))
    (cons (cons minX minY)
	  (cons (if (< 1 maxX) maxX 2)(if (< 1 maxY) maxY 2)))))


(defsubst dm-draw-path-parse (word)
  "Given trimmed WORD of a plan, DTRT."
  (dm-draw--dqnconc
    (when (org-string-nw-p word)
      (cond ((string-match-p "<[^ ]\\|>[^ ]\\|[^ ]<\\|[^ ]>" word)
	     (list word))
	    ((string-match-p "[()]" word)
	     (list (read word)))
	    ((string-match-p "[ \t\n\"]" word)
	     (list (mapcan 'dm-draw-path-parse
			   (split-string word nil t "[ \t\n]+"))))
	    (;; TODO: RX ?M ?m ?V ?v ?H ?h ?S ?s ?A ?a ?L ?l
	     (string-match-p "^[MmVvHhCcSsAaLl][0-9.,+-]+$" word)
	     (list (list (intern (substring word 0 1))
			 (mapcar 'string-to-number
				 (split-string (substring word 1) "[,]")))))
	    (t (list (intern
		      (if (and (< 1 (length word))
			       (string-match-p "^[&]" word))
			  (substring word 1)
			word))))))))

(defsubst dm-draw-dom-parse (strings)
  "Parse STRINGS as a single XML DOM."
  (when-let ((xml-parts
	      (dm-map--xml-attr-to-number
	       (with-temp-buffer
		 (insert "<g>"
			 (mapconcat
			  'identity
			  strings
			  " ")
			 "</g>")
		 (libxml-parse-xml-region (point-min)
					  (point-max))))))
    (dom-children xml-parts)))

(defmacro dm-draw--ensure-cons (var def)
  "Take VAR (or DEF) as a cons cell."
  (declare (indent 1))
  `(if (not (null ,var))
       (if (consp ,var) ,var
	 (if (proper-list-p ,var)
	     (cons (car ,var) (nth 1 ,var))
	   (cons ,var ,var)))
     ,def))

(defmacro dm-draw--nudge-impl (nudge scale)
  "Return NUDGE at SCALE."
  (declare (indent 1))
  `(cons (* (car ,nudge) (car ,scale))
	 (* (cdr ,nudge) (cdr ,scale))))

(defsubst dm-draw-size (&optional size)
  "Return SIZE of drawing as a cons (WIDTH . HEIGHT)."
  (dm-draw--ensure-cons size
    (dm-draw--ensure-cons dm-draw-size
      (cons 10 10))))

(defsubst dm-draw-scale (&optional scale)
  "Return the current map SCALE."
  (dm-draw--ensure-cons scale
    (dm-draw--ensure-cons dm-draw-scale (cons 100 100))))

(defmacro dm-draw--pos-impl (pos nudge scale)
  "Return NUDGE at SCALE.

When UNSCALED is true don't scale.  POS is a cons cell indicating
the position.  NUDGE is a cons cell representing offset from the
upper left.  SCALE is a cons cell giving pixels per
dungeon-unit."
  (declare (indent 1))
  `(cons (+ (* (car ,nudge) (car ,scale))
	    (* (car ,pos) (car ,scale)))
	 (+ (* (cdr ,nudge) (cdr ,scale))
	    (* (cdr ,pos) (cdr ,scale)))))

(defmacro dm-draw--unpos-impl (pos nudge scale)
  "Return POS in dungeon-units given NUDGE and SCALE."
  (declare (indent 1))
  `(cons (/ (- (car ,pos) (car ,nudge))
	    (car ,scale))
	 (/ (- (cdr ,pos) (cdr ,nudge))
	    (cdr ,scale))))

(defsubst dm-draw-nudge (&optional nudge scale unscaled)
  "Return the scaled map nudge.

When UNSCALED is true don't scale.  NUDGE replaces the default
supplied from the variable `dm-draw-nudge'.  SCALE replaces the
default supplied by the function `dm-draw-scale'."
  (if unscaled (dm-draw--ensure-cons nudge
		 (dm-draw--ensure-cons dm-draw-nudge
		   (cons 0 0)))
    (dm-draw--nudge-impl
     (dm-draw--ensure-cons nudge
       (dm-draw--ensure-cons dm-draw-nudge
	 (cons 0 0)))
     (dm-draw-scale scale))))

(defsubst dm-draw-pos (&optional pos nudge scale unscaled)
  "Return NUDGE at SCALE.

When UNSCALED is true don't scale.  POS is a cons cell indicating
the position.  NUDGE is a cons cell giving offset from the upper
left.  SCALE is a cons cell giving pixels per dungeon unit."
  (if unscaled
      (dm-draw--pos-impl
	  (dm-draw--ensure-cons pos (cons 0 0))
	(dm-draw-nudge nudge nil t)
	(cons 1 1))
    (dm-draw--pos-impl
	  (dm-draw--ensure-cons pos (cons 0 0))
      (dm-draw-nudge nudge nil t)
      (dm-draw-scale scale))))

(defsubst dm-draw-unpos (pos &optional nudge scale)
  "Return POS in dungeon units given NUDGE and SCALE."
  (when pos
    (dm-draw--unpos-impl
	(dm-draw--ensure-cons pos nil)
      (dm-draw-nudge nudge (dm-draw-scale scale))
      (dm-draw-scale scale))))

(defun dm-draw-row (&optional no-error)
  "Draw the org-table row at point."
  (let ((row (apply 'append (cdr (dm-table-row nil no-error))))
	plist)
    ;;(message "draw-row: ROW:%s" row)
    (dolist (prop dm-draw-layer-alist plist)
      ;;(message "draw-row: PROP %s->%s" prop (plist-get row (car prop)))
      (pcase (cadr prop)
	('scaled nil)
	('dom (let ((dom (dm-draw-dom-parse (list (plist-get row (car prop))))))
		;;(message "draw-row: DOM:%s" dom)
		(setq plist (plist-put plist (car prop) dom))))
	(_ (let ((paths (apply 'append
			       (dm-draw-path-parse
				(plist-get row (car prop))))))
	     ;;(message "draw-row: PATH %s" (cadr prop))
	     (setq plist (plist-put
			  plist (car prop)
			  (mapcan
			   (lambda (path)
			     (if (not (symbolp path)) (list path)
			       ;; (message "resolve tile:%s prop:%s =>%s"
			       ;; 		path (car prop)
			       ;; 		(list (plist-get
			       ;; 		       (dm-draw-resolve-tile path)
			       ;; 		       (car prop))))
			       (or (plist-get (dm-draw-resolve-tile path)
					      (car prop))
				   (and (not (eq 'path (car prop)))
					(plist-get (dm-draw-resolve-tile path) 'path)))))
			   paths)))))))))
;; (append (list :cell (cons 2 1)) plist)

;; http://kitchingroup.cheme.cmu.edu/blog/2016/03/16 \
;;   /Getting-graphical-feedback-as-tooltips-in-Emacs/
(defun dm-draw-image-tooltip (_window _object position)
  "Draw the tile in the row just hovered.

POSITION indicates the location of the text hovered."
  (save-excursion
    (goto-char position)
    (when-let ((row (dm-draw-row t)))
      (let* ((area (dm-draw-path-area (plist-get row 'path)))
	     (row (append (list :cell (car area)) row))
	     (dm-draw-size (cdr area))
	     (dm-draw-nudge (cons .15 .15))
	     (dm-map-tags t)
	     (svg (dm-draw (list row) :no-border t))
             (s (propertize " " 'display (svg-image svg))))
	;;(message "row:%s" row)
	(format "%s" s)))))

(defun dm-draw-tile-fontify-re ()
  "Provide the regular expression to fontify tiles."
  (mapconcat (apply-partially #'format "|[ \t]*%s")
	     (hash-table-keys dm-map-tiles)
	     "\\|"))

(defun dm-draw-tile-fontify-add ()
  "Add font locking keywords for each known tile."
  (font-lock-add-keywords
   nil
   `((,(dm-draw-tile-fontify-re) 0
      '(face font-lock-keyword-face
	     help-echo dm-draw-image-tooltip)))))

(defun dm-draw-resolve-tile-path (tile prop &optional no-fallback fallback)
  "Resolve PROP of TILE.

When NO-FALLBACK is non-nill, do not try again using FALLBACK (or \"path\")
when nothing is found for PROP."
  (mapcan
   (lambda (path)
     (if (not (symbolp path)) (list path)
       (or (plist-get (dm-draw-resolve-tile path) prop)
	   (and (not (or no-fallback (eq 'path prop)))
		(plist-get (dm-draw-resolve-tile path)
			   (or fallback 'path))))))
   (if (not (symbolp tile)) tile
     (let ((plist (dm-draw-resolve-tile tile)))
       (or (plist-get plist prop)
	   (and (not no-fallback)
		(plist-get plist (or fallback 'path))))))))

(defun dm-draw-resolve-tile (tile &optional cell)
  "Return draw code for TILE as a plist.

CELL supplies a position for the tile, if any."
  (mapcan (lambda (prop)
	    (append
	     (when cell (list :cell (copy-tree cell)))
	     (list prop
		   (dm-map-resolve tile :prop prop
				   ;; FIXME: commenting crashes render
				   ;;:inhibit-tags t
				   :inhibit-collection t))))
	  (mapcar 'car dm-draw-layer-alist)))

(defun dm-draw-resolve-cell (cell)
  "Return plist of draw code for CELL."
  (let ((plist (dm-map-cell-defaults)) dm-map-current-tiles)
    (when-let ((val (dm-map-resolve-cell cell :no-follow t)))
      (plist-put plist 'path val))
    (dolist (prop (remove 'path (mapcar 'car dm-draw-layer-alist)))
      (when-let ((val (apply
		       'append
		       (seq-map
			(lambda (tile)
			  (dm-map-resolve tile :prop prop
					  ;; FIXME: commenting crashes render
					  :inhibit-tags t
					  :inhibit-collection t))
			dm-map-current-tiles))))
	(plist-put plist prop (delq nil val))))
    (append (list :cell (copy-tree cell)) plist)))

(defun dm-draw-dom-interpolate-default (square var)
  "Replace VAR with property value from the current SQUARE."
  (format "%s" (or (plist-get square var) "")))

(cl-defun dm-draw (squares
		   &optional &key
		   (layers dm-draw-layer-alist)
		   (layer-attr dm-draw-layer-attr-alist)
		   (scale (dm-draw-scale))
		   (size (dm-draw-size))
		   (nudge (dm-draw-nudge nil nil t))
		   (canvas-size (cons (+ (* (car scale) (car nudge) 2)
					 (* (car scale) (car size)))
				      (+ (* (cdr scale) (cdr nudge) 2)
					 (* (cdr scale) (cdr size)))))
		   (svg-attr (cdr (or (and layer-attr (assoc t layer-attr))
				      (assoc t dm-draw-layer-attr-alist))))
		   (svg (append (apply 'svg-create
				       (append (list (car canvas-size)
						     (cdr canvas-size))
					       (mapcan
						(lambda (attr)
						  (list (car attr) (cdr attr)))
						(apply 'append svg-attr))
					       )))
			own-svg-p)
		   ;; fetch or build background if not disabled
		   no-border
		   no-background
		   (background
		    (or no-background
			(dm-draw-background :svg svg :size canvas-size :scale scale
					   :nudge nudge :no-border no-border)))
		   scale-background
		   (scaled (or scale-background no-background
			       (list 'background background)))
		   (dom-scale-fun #'dm-draw-dom-scale)
		   (path-scale-fun #'dm-draw-path-scale)
		   &allow-other-keys)
  "No-frills draw routine."
  ;;(dm-svg :svg)
  (if (null own-svg-p)
      (progn
	(message "add: %s"
		 (list (list (list :cell (cons 2 2) 'path
				   '((h (1))(v (1))(h (-1))(v (-1)))))
		       '(path) (dm-draw-scale)
		       (dm-draw-nudge nil nil t)
		       layer-attr scaled #'dm-draw-path-scale #'dm-draw-dom-scale))
	(setq svg (dm-draw--adqnmap svg prop layers
		    (dm-draw-compose-layer squares prop
					   (dm-draw-scale scale)
					   (dm-draw-nudge nudge nil t)
					   layer-attr scaled
					   path-scale-fun dom-scale-fun))))
    (let ((layer-ix -1) (layers-last-ix (length layers)))
      (dolist (prop layers svg)
	(setq layer-ix (1+ layer-ix))
	(when-let ((new-group
		    (dm-draw-compose-layer squares prop scale nudge
					   layer-attr scaled
					   path-scale-fun dom-scale-fun)))
	  (if-let ((old-group
		    (dom-elements  svg 'id (format "^%s$" (car prop)))))
	      (mapc (apply-partially #'dom-append-child old-group)
		    (dom-children new-group))
	    (let (next-sib (next-sib-ix layer-ix))
	      (while (and (null next-sib)
			  (> layers-last-ix (cl-incf next-sib-ix)))
		(setq next-sib
		      (car
		       (dom-elements svg 'id
				     (format "^%s$"
					     (car (nth next-sib-ix
						       layers)))))))
	      (if next-sib (dom-add-child-before svg new-group next-sib)
		(dom-append-child svg new-group))))))))
  svg)

(defun dm-draw-compose-layer (squares prop
				      scale nudge
				      layer-attr scaled
				      path-scale-fun dom-scale-fun)
  "Create drawing instructions for PROP from SQUARES.

SQUARES are plists containing draw code and other data.  PROP is
a symbol representing a layer.  SCALE is a cons providing the
number of pixels per dungeon unit.  Nudge is a cons cell
supplying padding in dungeon units from the top left corner of
the image.  LAYER-ATTR is a plist of SVG attributes by layer.
SCALED supplies a cache of pre-rendered and scaled SVG data.
PATH-SCALE-FUN and DOM-SCALE-FUN are functions responsible for
element scaling and placement."
  (pcase (cadr prop)
    ('scaled (plist-get scaled (car prop)))
    ('dom
     (let ((dom
	    (dm-draw--dqnmap squares cell
	      (mapcar
	       (apply-partially dom-scale-fun path-scale-fun nudge scale
				(copy-tree (plist-get (car cell) :cell)))
	       (copy-tree (plist-get (car cell) (car prop)))))))
       (when (and dom (proper-list-p dom) (< 0 (length dom)))
	 (append (list 'g (list (cons 'id (format "%s" (car prop))))) dom))))
    (_ (let ((path-string
	      (dm-map-path-string
	       (dm-draw--dqnmap squares cell
		 (when-let ((strokes (plist-get (car cell) (car prop)))
			    (pos (copy-tree (plist-get (car cell) :cell))))
		   (dm-draw--M scale (dm-draw-nudge nudge nil t) pos
		     (apply
		      (apply-partially path-scale-fun scale)
		      (copy-tree strokes))))))))
	 (when (org-string-nw-p path-string)
	   (list 'g (apply
		     'append
		     (list (cons 'id (prin1-to-string (car prop))))
		     (copy-tree (cdr (or (assoc (car prop) layer-attr)
					 (assoc t layer-attr)))))
		 (dm-svg-create-path
		  path-string)))))))


;; (message "%s=>(%s) svg:%s" (car prop) (plist-get (car cell) :cell) (plist-get (car cell) (car prop)))


(cl-defun dm-draw-background
    (&optional
     &key
     (scale (dm-draw-scale))
     (x-scale (car scale))
     (y-scale (cdr scale))

     (base-size (dm-draw-size))
     (x-base-size (car base-size))
     (y-base-size (cdr base-size))

     (size (cons (* x-scale x-base-size)
		 (* y-scale x-base-size)))
     (x-size (car size))
     (y-size (cdr size))

     (nudge (dm-draw-nudge nil scale))
     (x-nudge (car nudge))
     (y-nudge (cdr nudge))

     (h-rule-len (+ (* x-nudge 2) x-size))
     (v-rule-len (+ (* y-nudge 2) y-size))

     (svg (svg-create h-rule-len v-rule-len))

     no-canvas
     (canvas (unless no-canvas
	       (svg-rectangle svg
			      0 0 h-rule-len v-rule-len
			      :fill "#fffdd0"
			      :stroke-width  0)))

     no-border
     (border (unless no-border
	       (svg-rectangle svg
			      (- x-nudge 2)
			      (- y-nudge 2)
			      (- x-size (* 2 x-nudge) -5)
			      (- y-size (* 2 y-nudge) -5)
			      :fill "none"
			      :stroke "black"
			      :stroke-width  5)
	       (svg-rectangle svg
			      (1- x-nudge)
			      (1- y-nudge)
			      (- x-size (* 2 x-nudge) -3)
			      (- y-size (* 2 y-nudge) -3)
			      :fill "none"
			      :stroke "white"
			      :stroke-width  1)))

     no-graph
     (graph-attr '((fill . "none")
		   (stroke . "blue")
		   (stroke-width . ".25"))))
  "Create a background SVG with SCALE and SIZE and NUDGE."
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
			  (+ x-size y-nudge y-nudge)
			  x-scale)
	 (make-list (1+ (ceiling (/ y-size y-scale))) h-rule-len))
	(cl-mapcar
	 (apply-partially 'format "M%d,0 v%d")
	 (number-sequence x-nudge
			  (+ x-size x-nudge x-nudge)
			  y-scale)
	 (make-list (1+ (ceiling (/ x-size x-scale))) v-rule-len)))
       " ")
      graph-attr)))
  ;;`(g nil ,(dom-children svg))
  svg
  )


(cl-defun dm-draw-dom-scale (path-fun nudge scale pos dom-node)
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
  (let ((nudge (dm-draw-nudge nudge nil t))
	(scale (dm-draw-scale scale)))
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
			  (apply 'append (dm-draw-path-parse path-data))))))
	   ))
	(dom-set-attribute dom-node x-prop x-new)
	(dom-set-attribute dom-node y-prop y-new))
      (dolist (child (dom-children dom-node))
	(if (not (stringp child)) ;; process child nodes
	    (dm-draw-place-dom path-fun nudge scale pos child)
	  ;; process text
	  (dom-remove-node dom-node child)
	  (dom-append-child
	   dom-node
	   (replace-regexp-in-string
	    "[$][^][(),^$ ]+"
	    (if (not (functionp dm-draw-dom-interpolate-function)) ""
	      (lambda (str)
		(funcall dm-draw-dom-interpolate-function
			 (intern (substring str 1 (length str))))))
	    child))))
      dom-node)))

(defun dm-draw-path-scale (scale &rest cells)
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
  (let ((scale (dm-draw-scale scale)))
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
	))))



;; mouse position

(defun dm-draw-mouse-position (&rest _)
  "Return map cell under mouse as a cons (X . Y)."
  (dm-with-env (buffer-name) (image-buffer org-buffer)
    (let* ((mpos (mouse-pixel-position))
	   (wpos (caddr (posn-at-x-y (cadr mpos) (cddr mpos) (selected-frame))))
	   (w-x (car wpos)) (w-y (cdr wpos))
	   (win (selected-window))
	   (scale (dm-draw-scale)) (x-scale (car scale)) (y-scale (cdr scale))
	   (nudge (dm-draw-nudge nil scale)) (nudge-X (car nudge)) (nudge-Y (cdr nudge))
	   (du-X (/ (- w-x nudge-X) x-scale))
	   (du-Y (/ (- w-y nudge-Y) y-scale))
	   ;; account for any cells scrolled out of window
	   ;; hscroll is in characters; calc using frame char width
	   (char-width (frame-char-width (window-frame win)))
	   (scroll-X (* (window-hscroll win) char-width))
	   (scroll-Y (window-vscroll win t))
	   (du-scroll-X  (/ scroll-X x-scale))
	   (du-scroll-Y  (/ scroll-Y y-scale)))
      (cons (+ du-X du-scroll-X) (+ du-Y du-scroll-Y)))))

(provide 'dm-draw)
;;; dm-draw.el ends here
