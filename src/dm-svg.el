;;; dm-svg.el --- render dungeon-mode map as SVG     -*- lexical-binding: t; -*-

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

;;  * Implement ~dm-svg~ as an EIEIO class having two slots:
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

(eval-when-compile 'cl-generic)

(require 'eieio)
(require 'svg)

(eval-when-compile (require 'cl-lib))

;;; Code:

(defvar dm-draw-scale 60 "Pixels per dungeon unit.")
(defvar dm-draw-nudge (cons 1 1) "Frame padding in dungeon units.")
(defvar dm-draw-size (cons 6 6) "Drawing size in dungeon units.")

(defvar dm-draw-svg-attr-alist nil "SVG attributes as an alist.")

(defvar dm-draw-layer-attr-alist
  '(t (fill . "none")
      (stroke . "black")
      (stroke-width . "1")
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

(defvar dm-draw-layer-alist '((background svg)
			      (underlay svg)
			      (water)
			      (beach)
			      (stairs)
			      (neutronium)
			      (path)
			      (overlay svg)
			      (decorations))
  "Alist of layers and expected data format.")

(defmacro dm-draw--M (scale nudge pos paths)
  "Return absolute move to POS at NUDGE and SCALE, then PATHS."
  `(when ,paths
    (append
     (list (list 'M (list
		     (+ (* (car ,scale) (car ,nudge)) (* (car ,scale) (car ,pos)))
		     (+ (* (cdr ,scale) (cdr ,nudge)) (* (cdr ,scale) (cdr ,pos))))))
     ,paths)))

(cl-defun dm-draw (squares
		   &optional &key
		   (layers dm-draw-layer-alist)
		   (scale (if (consp dm-draw-scale) dm-draw-scale
			    (cons dm-draw-scale dm-draw-scale)))
		   (size (cons (* scale (car dm-draw-size))
			       (* scale (cdr dm-draw-size))))
		   (nudge dm-draw-nudge)
		   (canvas-size (cons (+ (* (car scale) (car nudge) 2)
					 (* (car scale) (car size)))
				      (+ (* (cdr scale) (cdr nudge) 2)
					 (* (cdr scale) (cdr size)))))
		   ;; fetch or build background if not disabled
		   no-background
		   (background
		    (or no-background
			(list
			 (dm-map-background :size canvas-size :scale scale
					    :x-nudge (* (car scale) (car nudge))
					    :y-nudge (* (cdr scale) (cdr nudge))))))
		   scale-background
		   (scaled (or scale-background (list 'background background)))
		   (svg-attr (cdr (or dm-draw-svg-attr-alist
				      (assoc t dm-draw-layer-attr-alist))))
		   (svg (append (apply 'svg-create
				       (append (list (car canvas-size)
						     (cdr canvas-size))
					       svg-attr))))
		   (scale-function #'dm-map-default-scale-function)
		   &allow-other-keys)
  "No-frills draw routine."
  (setq dm-map-current-tiles nil)
  (let* ((maybe-add-abs
	  (lambda (pos paths)
	    (when paths (append
			 (list (list 'M (list
					 (+ (* scale (car nudge)) (* scale (car pos)))
					 (+ (* scale (cdr nudge)) (* scale (cdr pos))))))
			 paths))))
	 (draw-code (delq nil (mapcar 'dm-map-cell squares)))
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

	 (img (dm-svg :svg
		      (append (apply 'svg-create
				     (append (list (car canvas-size)
						   (cdr canvas-size))
					     svg-attributes))
			      (append background underlays
				      been-dots pos-decoration
				      paths) ;; the non-primary paths, beach, etc.
			      (append (list (dm-svg-create-path
					     main-path
					     (plist-get path-attributes
							dm-map-draw-prop))))
			      (append overlays))
		      )))))

(cl-defun dm-draw-place-dom (path-fun nudge scale pos dom-node)
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
	  (dm-map--dom-attr-nudge path-fun nudge scale pos child)
	(dom-remove-node dom-node child)
	(dom-append-child dom-node
			  (replace-regexp-in-string
			   "[$][^][(),^$ ]+"
			   (if dm-draw-dom-interpolate-function
			       (lambda (str)
				 (funcall dm-draw-dom-interpolate-function
					  (intern (substring str 1 (length str)))))
			       "")
			     child))))
    dom-node))

(defvar dm-draw-square nil
  "The current square when drawing.")

(defvar dm-draw-dom-interpolate-function #'dm-draw-dom-interpolate-default
  "Function to call to replace vars (\"$foo\") in Text nodes.")

(defun dm-draw-dom-interpolate-default (var)
  "Replace VAR with property value from the current square."
  (format "%s" (or (plist-get square var) "")))

;; Predicates

(defun dm-svg-dom-node-p (object &optional tag)
  "Return t when OBJECT is a dom-node.

When TAG is non-nill (car OBJECT) must also `equal' TAG."
;;; (message "dm-svg-dom-node-p:car:%s=>%s" (type-of (car object)) (car object))
  (and (listp object)
       (symbolp (car object))
       (not  (null (car object)))
       (or (null tag)
	   (equal tag (car object)))))

(defun dm-svg-dom-nodes-or-nil-p (obj)
  "Return t when OBJ is a list of `dom-0nopde' or nil."
  (or (null obj)
      (and (listp obj)
	   (seq-every-p #'dm-svg-dom-node-p obj))))

(defun dm-svg-or-nil-p (obj)
  "True if OBJ is nil or an SVG `dom'."
  (or (null obj)
      (dm-svg-dom-node-p obj 'svg)))

(defun dm-svg-path-or-path-data-p (obj)
  "True when OBJ is nil, a string, or a \"path\" `dom-node'."
  (or (null obj)
      (stringp obj)
      (dm-svg-dom-node-p obj 'path)))

;; eieio

(defun dm-svg-create-path (&optional path-data properties children)
  "Create a 'path `dom-node'.

PATH-DATA is an optional string controling the \"d\" attribute.
PROPERTIES and CHILDREN map to corrisponding inputs accepted
by, e.g. `dom-node' which see."
  (if children
      (dom-node 'path `((d . ,path-data) ,@properties) children)
    (dom-node 'path `((d . ,path-data) ,@properties))))

(defclass dm-svg ()
  ((svg :type dm-svg-or-nil :initarg :svg :initform nil)
   (path-data :type dm-svg-path-or-path-data
	      :initarg :path-data
	      :initform nil)
   (overlays :type dm-svg-dom-nodes-or-nil-p :initarg :overlays :initform nil))
  :documentation
  "Wrap `svg' as an 'eieio' object.

This allows us to built up data for a \"main\" SVG element in
parallel to adding elements to the SVG in slot 1 in the usual
way.  PATH-DATA may be a string or a `dom-node' containing a
'path element, or nil.  PATH-DATA is internally represented as a
'dom-node', created when a string is provided by wrapping the
initarg value as a new 'path and using the string as the value of
the \"d\" attribute.")

(cl-defmethod initialize-instance :after ((object dm-svg) &rest _)
  "Docstring using MYOBJECT."
  (with-slots (svg path-data) object
    (unless (dm-svg-dom-node-p svg 'svg)
      (setq svg (dom-node 'svg)))
    (unless (dm-svg-dom-node-p path-data 'path)
      (setq path-data (dm-svg-create-path (and (stringp path-data)
					       path-data))))
    ))

(cl-defmethod add-path-data ((object dm-svg) &rest strings)
  "Add STRINGS to `dm-svg' OBJECT's path-data."
  (with-slots (path-data) object
    (dom-set-attribute path-data 'd
		       (concat (cdr (assoc 'd (dom-attributes path-data)))
			       (mapconcat 'concat strings "")))))

(cl-defmethod add-svg-element ((object dm-svg) &rest elements)
  "Add ELEMENTS to `dm-svg' OBJECT's path-data."
  (with-slots (svg) object
    (dolist (element elements)
      (setq svg (dom-append-child svg element)))
    svg))

(cl-defmethod render-and-insert ((object dm-svg))
  "Render and insert `svg' from a DM-SVG.

Image is inserted to `current-buffer' at `point' after appending
a path element using path-data of DM-SVG. See `add-path-data'."
  (with-slots (svg path-data overlays) object
    (when path-data (dom-append-child svg path-data))
    (mapc (lambda (elem)
	     (dom-append-child svg elem))
	   overlays)
    ;;(with-temp-file "out.svg" (set-buffer-multibyte nil) (svg-print svg))
    (svg-insert-image svg)))

(cl-defmethod render-and-insert-string ((object dm-svg))
  "Render and insert `svg' from a DM-SVG.

Image is inserted to `current-buffer' at `point' after appending
a path element using path-data of DM-SVG. See `add-path-data'."
  (with-slots (svg path-data overlays) object
    (when path-data (dom-append-child svg path-data))
    (when overlays
      (mapc (lambda (elem)
	      (dom-append-child svg elem))
	   overlays))
      ;;(with-temp-file "out.svg" (set-buffer-multibyte nil) (svg-print svg))
      ;;(svg-insert-image svg)
      (svg-print svg)))

(provide 'dm-svg)
;;; dm-svg.el ends here
