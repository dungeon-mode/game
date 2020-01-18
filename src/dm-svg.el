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

(defun dm-svg-or-nil-p (obj)
  "True if OBJ is nil or an SVG `dom'."
  (or (null obj)
      (eq 'svg (and (listp obj) (car obj)))))

(defclass dm-svg ()
  ((svg :type dm-svg-or-nil :initarg :svg :initform nil)
   (path-data :type (or null (satisfies stringp))
	      :initarg :path-data :initform nil))
  :documentation
  "Wrap `svg' as an 'eieio' object.

This allows us to built up data for a \"main\" path represented
as a string in parallel to adding elements to the SVG in slot 1
in the usual way.")

(cl-defmethod add-path-data ((object dm-svg) &rest strings)
  "Add STRINGS to `dm-svg' OBJECT's path-data."
  (oset object path-data
  	(concat (oref object path-data)
		(mapconcat 'concat strings ""))))

(cl-defmethod render-and-insert ((object dm-svg))
  "Insert the SVG image in `current-buffer'at `point'."
  (let* ((svg (oref object svg))
	 (path-data (oref object path-data))
	 (path (dom-node 'path '((d . path-data)))))
    (dom-append-child svg path)
    (svg-insert-image svg)))

;; (render-and-insert (dm-svg :svg (svg-create 500 500)))

(provide 'dm-svg)
;;; dm-svg.el ends here
