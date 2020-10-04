;;; dm-sketch.el --- draw interactive SVGs          -*- lexical-binding: t; -*-

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

;; Create interactive SVG gave interfaces using mouse and keyboard
;; commands.  Underlying SVG data is housed in org-mode formatted
;; tables, thus saving creates a (maybe) human readable document.
;; These (SVG image and org-mode) are considered "scratch" buffers
;; because they need to be assoicated with files on disk to work.

;;; Code:

(require 'cl-lib)
(require 'cl-extra)
(require 'subr-x)

(let ((load-path (append '(".") load-path)))
  (require 'dungeon-mode)
  (require 'dm-util)
  (require 'dm-env)
  (require 'dm-draw))

;;; Code:

(defvar dm-sketch-prefix "** "
  "Prefix used to name sketch buffers.")

(defvar dm-sketch-group "Dungeon"
  "Group name for use in naming sketch buffers.")

(defvar dm-sketch-count 0
  "Count of sketch buffers named.")

(defvar dm-sketch-count-fmt "<%s>"
  "Format used to display count in buffer names.")

(defvar dm-sketch-org-indctr " (org)"
  "Suffix appended to sketch buffers named.")

(defvar dm-sketch-suffix " **"
  "Suffix appended to sketch buffers named.")

(defvar dm-sketch-name-function
  #'dm-sketch-name
  "Function to name sketch buffers.

Receies a list of string arguments, returns a string.")

(defvar dm-sketch-org-template-function
  #'dm-sketch-org-template:dev
  "Function to populate new `org-mode' sketch buffers.")

(defmacro dm-sketch-buffer-name--impl
    (&optional group orgp no-incr no-count)
  "Smoosh together parts for a buffer name.

GROUP provides the group name and defaults to
`dm-sketch-group'.  When ORGP is truthy include
`dm-sketch-org-indctr'.  When NO-INCR is non-nil do
not increment `dm-sketch-count'; when NO-COUNT is
non-nil do not include it."
  `(apply
    #'concat
    (delq
     nil
     (list dm-sketch-prefix
	   (or ,group dm-sketch-group)
	   ,(when (null no-count)
	      (if (null no-incr)
		  `(format dm-sketch-count-fmt (cl-incf dm-sketch-count))
		`(format "%s" dm-sketch-count)))
	   ,(and orgp dm-sketch-org-indctr)
	   dm-sketch-suffix))))

;; create a custom environment for sketch buffers
;;(dm-def-with dm-with-sketch (svg-buffer org-buffer))

(defun dm-sketch-name (&optional group)
  "Return a list of buffer names.

Returned list is in the form (SVG-BUF-NAME ORG-BUF-NAME).  Use
GROUP instead of `dm-sketch-buffer-group' when given.  This
function adds the new buffer to `dm-env-alist' and may
increment and use `dm-sketch-count' to obtain uniqueness."
  (let* ((name (dm-sketch-buffer-name--impl group nil t t))
	 (uniqp (when (not (dm-env-has name)) t)))
    (dm-env-push
      (list
       (setq name (if (not uniqp) (dm-sketch-buffer-name--impl group)
		    (cl-incf dm-sketch-count)
		    name))
       (if uniqp (dm-sketch-buffer-name--impl group t t t)
	 (dm-sketch-buffer-name--impl group t t))))))

;; brutally destroy all env data
(defun dm-sketch-forget () "Forget previous sketch buffers."
  (dm-env-forget) (setq dm-sketch-count 0))



;; controlling buffers
;; populate an org-document as a blank drawing

(define-skeleton dm-sketch-org-template:dev
  "Initial HTML5 template"
  nil
  "#+TITLE Test: Map Cells with Paths

# Copyright (C) 2020 Corwin Brust, Erik C. Elmshauser, Jon Lincicum, Hope Christiansen, Frank Runyon

# TODO: full-path here, prompt for inital canvas size, etc.

This tests map rendering using a tile and cell containing a only path commands..

This file is a part of dungeon-mode.  It is not a part of GNU Emacs.
This softare is released under the GNU Public Licence version three
or, at your disgression, any newer version of the GNU Public
License.  For specific terms please see [[LICENSE]] at end-of-file.

* Tiles
:PROPERTIES:
:ETL: tile
:LAYERS: (underlay dom) path (overlay dom)
:END:

Tiles define an reusable set of SVG drawing instructions which
may be composed into one or several layers, each including either
SVG DOM elements or SVG Path data.  Each layer is represented by
a column in the table below.

The first column names the tile and the first (non-empty, etc.)
row must contain a column namee.  Further rows extend the last
tile \"named\".  Completely empty rows and hlines are ignored.

| Tile | Underlay | Path | Overlay | Documentation |
|------+----------+------+---------+---------------|
|      |          |      |         |               |

* Layout Squares
:PROPERTIES:
:ETL: square
:END:

Squares represent individual layout cells.  The top row indicates
the horizontal or \"Y\" access while the first column represents
the vertical or \"X\" access.  The cell at a given intersction
lists tiles to be placed there.

| x/y | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |  9 |
|   0 |   |   |   |   |   |   |   |   |   |    |
|   1 |   |   |   |   |   |   |   |   |   |    |
|   2 |   |   |   |   |   |   |   |   |   |    |
|   3 |   |   |   |   |   |   |   |   |   |    |
|   4 |   |   |   |   |   |   |   |   |   |    |
|   5 |   |   |   |   |   |   |   |   |   |    |
|   6 |   |   |   |   |   |   |   |   |   |    |
|   7 |   |   |   |   |   |   |   |   |   |    |
|   8 |   |   |   |   |   |   |   |   |   |    |
|   9 |   |   |   |   |   |   |   |   |   |    |

* LICENSE

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
")

(defsubst dm-sketch-set-props (props)
  "Set PROPS a plist of text properties."
  (let ((inhibit-read-only t)
	      (start (point-min))
	      (end (point-max)))
    (mapc (lambda (prop)
	    (put-text-property start end (car prop) (cadr prop)))
	  (seq-partition props 2))))

(defun dm-sketch-create (name)
  "Create (but do not select or display) image buffer NAME."
  (dm-with-env name (org-buffer sketch-buffer)
    (dm-env-put name :canvas (dm-draw nil :no-border t))
    (with-current-buffer (get-buffer-create name)
      (with-silent-modifications
	(unless (derived-mode-p 'image-mode) (dm-sketch-mode))
	(erase-buffer)
	(svg-insert-image (dm-env-get name :canvas))
	(image-mode-setup-winprops)
	;;(run-hooks dm-map-buffer-created-maybe-hook)
	(dm-sketch-set-props (list 'pointer 'hand
				   'help-echo #'dm-sketch-mouseover))
	(dm-env-get name :canvas)))))

(cl-defun dm-sketch-create-org (name &optional (template-function nil tmpl-p))
  "Create (but do not select) buffer NAME in `org-mode'.

Buffer is filled via TEMPLATE-FUNCTION or, when not provided, by
`dm-sketch-org-template-function'.  The buffer of emptied."
  (with-current-buffer (get-buffer-create name)
    (with-silent-modifications
      (erase-buffer)
      (org-mode)
      (when (and (null tmpl-p) (null template-function))
	(setq template-function dm-sketch-org-template-function))
      (when template-function
	(funcall template-function)))))



;; user activity and action state handling

;; interactive drawing tools

(defvar dm-sketch-tool-selected nil "Currently selected tool, as a symbol.")

(defvar dm-sketch-tools
  (list nil '(pointer hand help-echo-fmt "%s")
	'place '(pointer nil help-echo-fmt "placing.. %s"))
  "Plist of tools.")

(defvar dm-sketch-tile-selected nil "Currently selected tile, as a symbol.")
(defvar dm-sketch-stencil nil "Scaled SVG or DOM element following the pointer.")
(defvar dm-sketch-layer-selected nil "Current selected layer, as a symbol.")
(defvar dm-sketch-stencil-timer nil "Timer controling stencil move-with-mouse.")
(defvar dm-sketch-stencil-svg nil  "Unscaled draw-code for stencil.")
(defvar dm-sketch-stencil-data nil "Data used by the current stencil.")
(defvar dm-sketch-stencil-end-pos nil "Last position in stencil.")

(defmacro dm-sketch-stencil:path-make-end-point (svg pos &optional no-fill)
  "Create an end-point for a path stencil in SVG at POS.

When NO-FILL is non-nill set fill to \"none\"."
  (declare (indent 2))
  `(svg-circle ,svg (car ,pos) (cdr ,pos) 10
	       :id "dm-sketch-mousepos"
	       :stroke-color "red" :stroke-opacity .8
	       :fill ,(if no-fill "none" "red")
	       :fill-opacity .8))

(defmacro dm-sketch-stencil:path-make-segment (svg from to)
  "Draw a transient line-segment between FROM and TO in SVG."
  (declare (indent 2))
  `(progn
     (let* ((from (dm-sketch-stencil:path-last-pos))
	    (from (cons (car from) (if (proper-list-p (cdr from))
				       (cadr from)
				     (cdr from)))))
       (message "to:%s,%s from:%s,%s"
		(car from) (cdr from)
		(car ,to) (cdr ,to))
       (svg-node ,svg 'path
		 :id "dm-sketch-segment"
		 :stroke-color "red" :stroke-opacity .8
		 :d (format "M %s,%s L %s,%s"
			    (car from) (cdr from)
			    (car ,to) (cdr ,to))
		 ;;,@(unless no-fill `(:fill "red"))
		 ))))

(defmacro dm-sketch-canvas (&optional svg)
  "Return SVG or via `dm-env-get'."
  `(or ,svg (dm-sketch-stencil:path-set (dm-env-get (buffer-name) :canvas))))

(defmacro dm-sketch-stencil:plan (&optional plan)
  "Return the PLAN for the stencil.."
  `(or ,plan (plist-get dm-sketch-stencil-data :plan)))

(defmacro dm-sketch-stencil:plan-set (&optional plan)
  "Set the stencil draw PLAN."
  (declare (indent 0))
  `(plist-put dm-sketch-stencil-data :plan ,plan))

(defmacro dm-sketch-has-plan ()
  "Return non-nil when there is plan data for the sketch."
  (< 0 (length (dm-sketch-stencil:plan))))

(defmacro dm-sketch-stencil:path-make-plan (svg plan)
  "Draw PLAN in SVG."
  `(svg-node ,svg 'path
	     :id "dm-sketch-plan"
	     :fill "none" :stroke-color "green" :stroke-opacity .8
	     :stroke-width 10 :stroke-linejoin "bevel" :stroke-linecap "round"
	     :d (dm-map-path-string
		  (dm-draw--M (dm-draw-scale) (dm-draw-nudge nil nil t)
			      (cons 0 0)
		    (reverse ,plan)))))

(defmacro dm-sketch-stencil:path-last-part (&optional plan)
  "Return the last of PLAN."
  ;; `(car (nthcdr (1- (length (or ,plan (plist-get dm-sketch-stencil-data :plan))))
  ;; 		(or ,plan (plist-get dm-sketch-stencil-data :plan))))
  `(car (dm-sketch-stencil:plan ,plan)))


(defmacro dm-sketch-stencil:path-last-pos (&optional plan inhibit-default)
  "Return the coordinates of the last step in PLAN, if any.

When INHIBIT-DEFAULT is non-nil return nil instead of (0 . 0) when plan is empty."
  `dm-sketch-stencil-end-pos
  ;; `(let ((plan (dm-sketch-stencil:plan ,plan)))
  ;;    ,(if inhibit-default
  ;; 	  `(and ,plan (cadar ,plan))
  ;; 	`(or (and ,plan (cadar ,plan))
  ;; 	     (0 . 0))))
  )


(defun dm-sketch-stencil:path-init ()
  "Called when selecting the \"path\" tool."
  (interactive)
  (dm-sketch-set-tool 'place)
  (setq dm-sketch-stencil-data (list :command 'L :cmd-args nil :plan nil))
  (dm-sketch-stencil:path-set (dm-sketch-canvas)))

(defun dm-sketch-stencil:path-add (pos)
  "Add a line-to POS to path stencil."
  (setq dm-sketch-stencil-end-pos pos)
  (let ((plist dm-sketch-stencil-data)
	(plan (plist-get dm-sketch-stencil-data :plan)))
    (dm-sketch-stencil:plan-set
     (push (list (plist-get plist :command)
		 (append (plist-get plist :cmd-args)
			 (list (car pos)
			       (if (proper-list-p pos)
				   (cadr pos)
				 (cdr pos)))))
	   plan)))
  (dm-sketch-stencil:path-set
   (dm-sketch-canvas)
   (dm-draw-unpos pos)))

(defun dm-sketch-stencil:path-move (pos &optional relitive path-part)
  "Move PATH-PART or last of :plan to POS.

When RELITIVE is non-nil move by (instead of to) POS."
  (when-let* ((part (or path-part (dm-sketch-stencil:path-last-part)))
	      (pos (if (null relitive) pos
		     (cons (+ (caadr part) (car pos))
			   (+ (cadadr part) (if (proper-list-p pos)
						(cadr pos)
					      (cdr pos)))))))
    (setq dm-sketch-stencil-end-pos pos)
    (setcar (cadr part) (car pos))
    (setcdr (cadr part) (list (cdr pos))))
  (dm-sketch-stencil:path-set (dm-sketch-canvas)))

(defun dm-sketch-stencil:path-remove (&optional path-part)
  "Remove PATH-PART or the last part of :plan."
  (if-let ((plan (dm-sketch-stencil:plan)))
      (dm-sketch-stencil:plan-set
	(remove (or path-part (dm-sketch-stencil:path-last-part plan)) plan))
    (setq dm-sketch-stencil-end-pos (cdar (dm-sketch-stencil:plan))))
  (dm-sketch-stencil:path-set (dm-sketch-canvas)))

(defun dm-sketch-stencil:path-set (svg &optional pos)
  "Sketch path to POS in SVG."
  (let* ((plan (dm-sketch-stencil:plan)))
    (if plan (dm-sketch-stencil:path-make-plan svg plan)
      (svg-remove svg "dm-sketch-plan")))
  (dm-sketch-stencil-place (buffer-name) pos))

(defun dm-sketch-stencil-set (svg &optional pos)
  "Add stencil to SVG at POS."
  (setq dm-sketch-stencil
	(let ((pos (dm-draw-pos pos)))
	  ;; TODO: scale and place/replace the stencil svg if any
	  ;;
	  ;;(dm-sketch-stencil:path-set svg pos)
	  (message "path-preview->pos:%s plan-p:%s last:%s"
		   pos (dm-sketch-has-plan)
		   (dm-sketch-stencil:path-last-pos nil t))
	  (dm-sketch-stencil:path-make-end-point svg
	      pos
	    (not (null (dm-sketch-stencil:plan))))
	  (when (dm-sketch-has-plan)
	    (dm-sketch-stencil:path-make-segment svg
		(dm-sketch-stencil:path-last-pos nil t)
	      pos))
	  (dom-by-id svg "dm-sketch-mousepos")))) ;; return element at point

(defun dm-sketch-stencil-place (buffer &optional pos &rest _)
  "Move BUFFER stencil to POS."
  (when-let ((svg (dm-env-get buffer :canvas)))
    (with-current-buffer buffer
      (dm-with-env buffer (image-buffer org-buffer)
	(dm-sketch-stencil-set svg pos)))))

(defsubst dm-sketch-stencil-timer-cancel ()
  "Stop the stencil position update timer."
  (when dm-sketch-stencil-timer
    (cancel-timer dm-sketch-stencil-timer)
    (setq dm-sketch-stencil-timer nil)))

(defsubst dm-sketch-stencil-timer-reset (buffer &optional pos)
  "Set BUFFER stencil timer to move stencil to POS."
  (dm-sketch-stencil-timer-cancel)
  (setq dm-sketch-stencil-timer
	(run-at-time .1 nil #'dm-sketch-stencil-place buffer pos)))

(defun dm-sketch-set-tool (tool)
  "Set TOOL (a symbol) as the selected tool."
  (pcase (setq dm-sketch-tool-selected tool)
    ('place (dm-sketch-set-props
	     (list 'pointer (plist-get dm-sketch-tools 'place))))
    (_ ;; no tool seleected
     (dm-sketch-set-props (list 'pointer (plist-get dm-sketch-tools nil))))))

(defun dm-sketch-set-tile (tile)
  "Set TILE (a symbol) as the selected tile."
  (if (null (setq dm-sketch-tile-selected tile))
      (setq dm-sketch-stencil-svg nil)
    (setq dm-sketch-stencil-svg
	  (dm-draw-resolve-tile tile))))

(defun dm-sketch-mouseover (&rest _)
  "Update image based on mouse movement."
  (when dm-sketch-tool-selected
    (let ((mpos (dm-draw-mouse-position))
	  (plist (plist-get dm-sketch-tools dm-sketch-tool-selected)))
      (and (plist-member plist 'mouseover)
	   (funcall (plist-member plist 'mouseover) mpos))
      (when dm-sketch-stencil
	(dm-sketch-stencil-timer-reset (buffer-name) mpos)
	(format (plist-get plist 'help-echo-fmt) mpos)))))

;; other interactive commands

;; primary entry point

(cl-defun dm-sketch
    (&optional
     &key
     no-focus
     (group-name dm-sketch-group)
     (buffer-names (dm-sketch-name group-name)))
  "Create a new set of sketch buffers.

Focus SVG buffer unless NO-FOCUS is non-nil. GROUP-NAME is the
base name for both buffers.  BUFFER-NAMES is a list in the
form (SVG-BUF-NAME ORG-BUF-NAME)."
  (interactive)

  ;; hack to sync scale until we things up..
  (let ((scale (car (cdr (assoc 'dm-draw-scale dm-env-vars)))))
    (setq dm-map-scale scale dm-draw-scale scale))

  (dm-sketch-create (car buffer-names))
  (dm-sketch-create-org (cadr buffer-names))
  (unless no-focus (pop-to-buffer (car buffer-names)))
  ;; force the drawing to start in "stencil"/line-drawing mode
  (dm-sketch-stencil:path-init))

(defun dm-sketch-mouse-remove-last (&rest _)
  "Handle click on sketch buffer."
  (interactive "@")
  ;; TODO: handle prefix arg to remove other than the last command
  (dm-sketch-stencil:path-remove))

(defun dm-sketch-mouse1 (&rest _)
  "Handle click on sketch buffer."
  (interactive "@")
  (dm-sketch-stencil:path-add (dm-draw-pos (dm-draw-mouse-position))))

(defun dm-sketch-move-to (&rest _)
  "Handle secondary click on sketch buffer."
  (interactive "@")
  (let* ((pos (dm-draw-mouse-position))
	 (old (plist-get dm-sketch-stencil-data :command)))
    (setq dm-sketch-stencil-data
	  (plist-put dm-sketch-stencil-data :command 'M))
    (dm-sketch-stencil:path-add (dm-draw-pos pos))
    (message "pos:%s old:%s dat:%s" pos old dm-sketch-stencil-data)
    (setq dm-sketch-stencil-data
	  (plist-put dm-sketch-stencil-data :command old))))

(defvar dm-sketch-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "+") 'dm-map-scale-nudge)
    ;; (define-key map (kbd "-") 'dm-map-scale-nudge-invert)
    ;; (define-key map (kbd "<up>") 'dm-map-scroll)
    ;; (define-key map (kbd "<down>") 'dm-map-scroll-invert)
    ;; (define-key map (kbd "<right>") 'dm-map-hscroll)
    ;; (define-key map (kbd "<left>") 'dm-map-hscroll-invert)
    ;;(define-key map (kbd "s") 'dm-map-scale)
    (define-key map (kbd "<C-delete>") 'dm-sketch-stencil:path-init)
    (define-key map (kbd "q") 'dm-scratch-quit)
    (define-key map [mouse-1] 'dm-sketch-mouse1)
    (define-key map [mouse-2] 'dm-sketch-move-to)
    (define-key map [mouse-4] 'dm-sketch-move-to)
    (define-key map (kbd "<delete>") 'dm-sketch-mouse-remove-last)
    (define-key map "\C-c\C-x" 'ignore)
    ;;(define-key map "\C-c\C-c" 'dm-map-source-toggle)
    map)
  "Mode keymap for `dm-sketch-mode'.")

(define-derived-mode dm-sketch-mode image-mode "Sketch"
  "Major mode for `dugeon-mode' interactive drawing.")



(provide 'dm-sketch)
;;; dm-sketch.el ends here
