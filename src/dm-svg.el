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

;;; Code:

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
