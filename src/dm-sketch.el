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
    (dm-env-put name :canvas (dm-draw nil :no-border t :id-prefix "dm-sketch-"))
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



;; mung

(defvar dm-sketch-map-file nil "File containing a dungeon map..")

(defvar dm-sketch-map-table "cell" "ETL property for the map table.")

(defmacro dm-sketch-with-map-file (&rest body)
  "Execute BODY with `dm-sketch-map-file' as the current buffer."
  (declare (indent 0))
  `(if (null dm-sketch-map-file)
       (user-error "No map file selected")
     (with-current-buffer (get-buffer-create dm-sketch-map-file)
       (message "current-buffer:%s" (buffer-name))
       ,@body)))

(defmacro dm-sketch-field-to-number (num)
  "Get field NUM of map table."
  `(string-to-number
    (or (org-string-nw-p (org-no-properties (org-table-get-field ,num)))
        "-1")))

(defun dm-sketch-get-map-cell (pos)
  "Get map cell at POS."
  (when dm-sketch-map-file
    (with-current-buffer (or (find-buffer-visiting dm-sketch-map-file)
                             (get-buffer-create dm-sketch-map-file))
      (when-let ((tpos (seq-first
                        (org-element-map (org-element-parse-buffer) 'table
                          (lambda (table)
                            (when (string= (dm-table-property table)
                                           dm-sketch-map-table)
                              (dm-table-start-pos table)))))))
        (goto-char tpos)
        (while (and (org-at-table-p)
                    (not (equal (dm-sketch-field-to-number 2)
                                (cdr pos))))
          (forward-line))
        (while (and (org-at-table-p)
                    (not (equal (dm-sketch-field-to-number 1)
                                (car pos))))
          (forward-line))
        (if (not (and (equal (dm-sketch-field-to-number 2)
                             (cdr pos))
                      (equal (dm-sketch-field-to-number 1)
                             (car pos))))
            (user-error "Cannot find pos in map table" pos)
          (org-trim (org-no-properties (org-table-get-field 3)))))
      )))

(defun dm-sketch-set-map-cell (pos plan)
  "Set map cell at POS to PLAN."
  (when-let ((old-value (dm-sketch-get-map-cell pos)))
    (org-table-get-field 3 (prin1-to-string plan)))
  (org-table-align))



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
(defvar dm-sketch-stencil-cpt-selected nil
  "Currently seelected control-point in stencil.")
(defvar dm-sketch-stencil-cpt-selected-index nil
  "Index into stencil path sequence of the currently selected control-point.")

(defmacro dm-sketch-cpt ()
  "Return position of the currently selected control point, if any."
  `(if dm-sketch-stencil-cpt-selected-index
       (cadr (nth dm-sketch-stencil-cpt-selected-index
		   (dm-sketch-stencil:plan)))
     (and (dm-sketch-has-plan)
	  dm-sketch-stencil-end-pos)))

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
       (message "from:%s,%s to:%s,%s"
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

(defvar dm-sketch-tile-node nil "SVG node for the selcted tile.")

(defmacro dm-sketch-stencil:make-tile (svg pos)
  "Place the selected tile at POS in SVG."
  `(progn
     (when dm-sketch-tile-node (dom-remove-node ,svg dm-sketch-tile-node))
     (when (setq dm-sketch-tile-node
		 (when (and dm-sketch-tile-selected dm-sketch-stencil-svg)
		   (plist-put dm-sketch-stencil-svg :cell ,pos)
		   (let ((node (dm-draw (list dm-sketch-stencil-svg)
					:no-background t
					;;:svg (dm-env-get (buffer-name) :canvas)
					:no-append t)))
		     (dom-set-attribute node 'id "dm-sketch-tile")
		     node)))
       (dom-append-child ,svg dm-sketch-tile-node))
     (svg-possibly-update-image ,svg)
     dm-sketch-tile-node))

(defmacro dm-sketch-stencil:place-tile (svg pos)
  "Place stencil into SVG at POS."
  (dm-map-s))

;; `(prog1
;;        (setq dm-sketch-tile-node
;; 	     (if (null dm-sketch-tile-selected)
;; 		 (progn (dom-remove-node svg (dom-by-id ,svg "dm-sketch-tile")) nil)
;; 	       (let ((node (dm-draw (list (dm-draw-resolve-tile
;; 					   dm-sketch-tile-selected
;; 					   ,pos))
;; 				    :no-background t
;; 				    :id-prefix "dm-sketch-tile-"
;; 				    :no-append t
;; 				    ;;:svg ,svg ;; :layer-attr dm-draw-layer-attr-alist
;; 				    ;;:nudge (cons 0 0)
;; 				    )))
;; 		 (dom-set-attribute node 'id "dm-sketch-tile")
;; 		 node)))
;;      (svg-possibly-update-image ,svg))

(defmacro dm-sketch-canvas (&optional svg)
  "Return SVG or via `dm-env-get'."
  `(or ,svg (dm-sketch-stencil:path-preview (dm-env-get (buffer-name) :canvas))))

(defmacro dm-sketch-stencil:plan (&optional plan)
  "Return the PLAN for the stencil.."
  `(or ,plan (plist-get dm-sketch-stencil-data :plan)))

(defmacro dm-sketch-stencil:plan-set (&optional plan)
  "Set the stencil draw PLAN."
  (declare (indent 0))
  `(plist-put dm-sketch-stencil-data :plan ,plan))

(defmacro dm-sketch-has-plan ()
  "Return non-nil when there is plan data for the sketch."
  `(< 0 (length (dm-sketch-stencil:plan))))

(defmacro dm-sketch-stencil:path-make-cpt
    (svg counter x y size &rest args)
  "Add a control-point to SVG.

X, Y, SIZE, and ARGS control presentation.  COUNTER trackes the
control points created."
  `(svg-circle ,svg  (caadr stroke) (cadadr stroke) 8
	       :id (format "dm-sketch-cpt-%d" (cl-incf ,counter))
	       :class "dm-sketch-cpt"
	       :fill "none" :stroke-color "green" :stroke-opacity .6
	       ))

(defmacro dm-sketch-stencil:path-make-plan (svg plan)
  "Draw PLAN in SVG."
  (let ((counter (gensym "dm-sketch-cpt-"))
	(pos (gensym "dm-sketch-start-point-")))
    `(let ((,counter 0)
	   (,pos (dm-draw-pos)))
       (svg-node ,svg 'path
		 :id "dm-sketch-plan"
		 :fill "none" :stroke-color "green" :stroke-opacity .4
		 :stroke-width 4 :stroke-linejoin "bevel" :stroke-linecap "round"
		 :d (dm-map-path-string
		      (dm-draw--M (dm-draw-scale) (dm-draw-nudge nil nil t)
				  (cons 0 0)
			(reverse ,plan))))
       (svg-circle ,svg  (car ,pos) (cdr ,pos) 8
		   :id (format "dm-sketch-cpt-%d" (cl-incf ,counter))
		   :class "dm-sketch-cpt"
		   :fill "none" :stroke-color "blue" :stroke-opacity .6
			      )
       (mapc (lambda (stroke)
	       (pcase (car stroke)
		 ('M
		  (svg-circle ,svg  (caadr stroke) (cadadr stroke) 8
			      :id (format "dm-sketch-cpt-%d" (cl-incf ,counter))
			      :class "dm-sketch-cpt"
			      :fill "none" :stroke-color "green" :stroke-opacity .6
			      ))
		 (_ (svg-circle ,svg  (caadr stroke) (cadadr stroke) 6
				:id (format "dm-sketch-cpt-%d" (cl-incf ,counter))
				:class "dm-sketch-cpt"
				:fill "green" :fill-opacity .4 :stroke-color "none"
				))))
	     ,plan))))

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


(defun dm-sketch-stencil:tile-move (pos &optional relitive node)
  "Move the selected tile to POS.

POS is absolute dungeon-units unless RELITIVE is non-nil."
  (when-let* ((svg (dm-env-get (buffer-name) :canvas t t))
	      (node (dm-sketch-stencil:make-tile svg pos))
	      ;; (pos (if (null relitive) pos
	      ;; 	     (cons (+ (string-to-number (dom-attr node 'x))
	      ;; 		      (car pos))
	      ;; 		   (+ (string-to-number (dom-attr node 'y))
	      ;; 		      (if (proper-list-p pos) (cadr pos)
	      ;; 			(cdr pos))))))
	      )
    ;;(message "moved tile to: %s" pos)
    (setq dm-sketch-stencil-end-pos pos)
    (svg-possibly-update-image svg)
    node))

(defun dm-sketch-stencil:path-init ()
  "Called when selecting the \"path\" tool."
  (interactive)
  (setq dm-sketch-stencil-end-pos (cons 1 1))
  (setq dm-sketch-stencil-data (list :command 'L :cmd-args nil
				     :plan nil
				     ;; :plan
				     ;; `((M (,@(let ((v (dm-draw-pos)))
				     ;; 	       (list (car v) (cdr v))))))
				     ))
  (setq dm-sketch-stencil-cpt-selected-index 0)
  (dm-sketch-stencil:path-preview (dm-sketch-canvas)))

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
  (dm-sketch-stencil:path-draw))

(defun dm-sketch-stencil:path-draw ()
  "Display the stencil path."
  (if-let ((svg (dm-env-get (buffer-name) :canvas t t)))
      (if-let ((plan (dm-sketch-stencil:plan)))
	  (dm-sketch-stencil:path-make-plan svg plan)
	;; no plan, remove the control points and stencil
	(mapc (apply-partially #'dom-remove-node svg)
	      (dom-by-class svg "dm-sketch-cpt"))
	(svg-remove svg "dm-sketch-plan"))
    (warn "Missing svg or not a sketch buffer (%s,%s)" svg (buffer-name))))

(defun dm-sketch-stencil:path-preview (svg &optional pos)
  "Add stencil to SVG at POS."
  ;;(setq dm-sketch-stencil)
  (let ((pos (dm-draw-pos pos)))
    ;; TODO: scale and place/replace the stencil svg if any
    ;;
    ;;(dm-sketch-stencil:path-preview svg pos)
    (message "path-preview->pos:%s plan-p:%s last:%s"
	     pos (dm-sketch-has-plan)
	     (dm-sketch-stencil:path-last-pos nil t))
    (dm-sketch-stencil:path-make-end-point svg
	pos
      (not (null (dm-sketch-has-plan))))
    (if (not (dm-sketch-has-plan))
	(svg-remove svg "dm-sketch-segment")
      (dm-sketch-stencil:path-make-segment svg
	  (dm-sketch-stencil:path-last-pos nil t)
	pos))))


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
  (dm-sketch-stencil:path-preview (dm-sketch-canvas)))

(defun dm-sketch-stencil:path-remove (&optional path-part)
  "Remove PATH-PART or the last part of :plan."
  (if (not (dm-sketch-has-plan))
      (setq dm-sketch-stencil-end-pos (dm-draw-pos (cons 1 1)))
    (dm-sketch-stencil:plan-set
      (remove (or path-part
		  (dm-sketch-stencil:path-last-part(dm-sketch-stencil:plan)))
	      (dm-sketch-stencil:plan))))
  (setq dm-sketch-stencil-end-pos (cadar (dm-sketch-stencil:plan)))
  (dm-sketch-stencil:path-preview (dm-sketch-canvas)))


(defun dm-sketch-stencil-place (buffer &optional pos &rest _)
  "Place stencil at POS in BUFFER."
  ;; (setq dm-sketch-stencil-end-pos
  ;; 	(or pos dm-sketch-stencil-end-pos
  ;; 	    (cons 1 1)))
  (when-let ((svg (dm-env-get buffer :canvas)))
    (with-current-buffer buffer
      (dm-with-env buffer (image-buffer org-buffer)
	(pcase dm-sketch-tool-selected
	  ('place
	   (progn (dm-sketch-set-tool 'place t)
		  (dm-sketch-stencil:make-tile svg (dm-draw--ensure-cons pos (cons 1 1)))))
	  ('line
	   (dm-sketch-stencil:path-preview svg (dm-draw--ensure-cons pos (cons 1 1)))))))))

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

(defun dm-sketch-set-tool (tool &optional inhibit-update)
  "Set TOOL (a symbol) as the selected tool.

When INHIBIT-UPDATE is non-nil set `dm-sketch-tool-selected' only."
  (when-let ((svg (dm-env-get (buffer-name) :canvas)))
    (setq dm-sketch-tool-selected tool)
    (unless inhibit-update
      (pcase dm-sketch-tool-selected
	('place (when dm-sketch-tile-selected
		  (dm-sketch-stencil:make-tile svg (or dm-sketch-stencil-end-pos
						       (cons 0 0)))))
	('line (progn
		 (dm-sketch-set-props
		  (list 'pointer (plist-get dm-sketch-tools 'place)))
		 (dm-sketch-stencil:path-init)))
	(_ ;; no tool seleected
	 (dm-sketch-set-props (list 'pointer (plist-get dm-sketch-tools nil))))))))

(defvar dm-sketch-select-tool-hist nil "Tile selection istory.

Used by 'dm-sketch-select-tool', which see.")

(defun dm-sketch-select-tool (&optional tool)
  "Select the TOOL used to manipulate the sketch.

When called interactively without, prompt for TOOL, otherwise TOOL
may be a string or a symbol."
  (interactive
   (list
    (intern
     (let ((tool-list (list 'place 'line)))
       (completing-read (format "Tool%s: " (if dm-sketch-tool-selected
					       (format "[%s]" dm-sketch-tool-selected)
					     ""))
			tool-list
			nil
			t
			nil
			dm-sketch-select-tool-hist)))))
  (dm-sketch-set-tool tool)
  (message "Selected the %s tool." tool))

(defvar dm-sketch-select-tile-hist nil "Tile selection istory.

Used by 'dm-sketch-select-tool', which see.")

(defun dm-sketch-select-tile (&optional tile)
  "Select a TILE to place into the sketch.

Tiles are named, pre-preparred multi-layer sub-images.  When
called interactively without, prompt for TILE, otherwise TITLE
may be a string or a symbol."
  (interactive
   (list
    (intern
     (completing-read (format "Tile%s: " (if dm-sketch-tile-selected
					     (format "(currently: %s)" dm-sketch-tile-selected)
					   ""))
		      (mapcar 'prin1-to-string (hash-table-keys dm-map-tiles))
		      (lambda (text) (not (null (gethash (intern text) dm-map-tiles))))
		      t
		      (prin1-to-string dm-sketch-tile-selected)
		      dm-sketch-select-tile-hist
		      (prin1-to-string dm-sketch-tile-selected)))))
  (dm-sketch-set-tile tile)
  (message "Set tile: %s" tile))

(defun dm-sketch-set-tile (tile)
  "Set TILE (a symbol) as the selected tile."
  (if (null (setq dm-sketch-tile-selected tile))
      (setq dm-sketch-stencil-svg nil)
    (let ((svg (dm-env-get (buffer-name) :canvas t t))
	  (pos (or dm-sketch-stencil-end-pos (cons 1 1))))
      (setq dm-sketch-stencil-svg
	    (dm-draw-resolve-tile tile pos))
      (when svg (dm-sketch-stencil:make-tile svg pos)))))

(defun dm-sketch-mouseover (&rest _)
  "Update image based on mouse movement."
  (when (and (dm-env-has (buffer-name)) dm-sketch-tool-selected)
    (let ((mpos (dm-draw-mouse-position))
	  (plist (plist-get dm-sketch-tools dm-sketch-tool-selected)))
      (and (plist-member plist 'mouseover)
	   (funcall (plist-member plist 'mouseover) mpos))
      (when dm-sketch-tool-selected
	(dm-sketch-stencil-timer-reset (buffer-name) mpos)
	;;(format (plist-get plist 'help-echo-fmt) mpos)
	))))

;; other interactive commands

;; primary entry point

(cl-defun dm-sketch
    (&optional
     &key
     no-focus
     (group-name dm-sketch-group)
     (buffer-names (dm-sketch-name group-name)))
  "Create a new set of sketch buffers.

Focus SVG buffer unless NO-FOCUS is non-nil.  GROUP-NAME is the
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
  (dm-sketch-set-tool 'line))

(defun dm-sketch-ctp-select-next (&optional arg)
  "Select the next prior control point.

When given the negitive prefix ARG, clear the selected control
point.  When ARG is numeric when ARG is numeric move ARG distance
toward the beginning or (when negitive), toward the end,
otherwise move by one along the list of control points ending in
`dm-sketch-stencil-end-pos'.  When arg is out of bound, use the
pos at the nearest boundry.  When no stencil path do nothing."
  (interactive "p")
  (message "arg:%s" arg)
  (when (dm-sketch-has-plan)
    (if (equal '- arg)
	(setq dm-sketch-stencil-cpt-selected-index 0)
      (setq dm-sketch-stencil-cpt-selected-index
	    (if dm-sketch-stencil-cpt-selected-index
		(+ dm-sketch-stencil-cpt-selected-index
		   (or arg 1))
	      (or arg 1))))
    (if (> 0 dm-sketch-stencil-cpt-selected-index)
	(setq dm-sketch-stencil-cpt-selected-index 0)
      (when (> dm-sketch-stencil-cpt-selected-index
	       (1- (length (dm-sketch-stencil:plan))))
	(setq dm-sketch-stencil-cpt-selected-index
	      (1- (length (dm-sketch-stencil:plan))))))))

(defun dm-sketch-mouse-remove-last (&rest _)
  "Handle click on sketch buffer."
  (interactive "@")
  ;; TODO: handle prefix arg to remove other than the last command
  (dm-sketch-stencil:path-remove))

(defun dm-sketch-mouse1 (&rest _)
  "Handle click on sketch buffer."
  (interactive "@")
  (pcase dm-sketch-tool-selected
    ('line
     (dm-sketch-stencil:path-add (dm-draw-pos (dm-draw-mouse-position))))
    ('place
     (dm-sketch-stencil:make-tile ))))

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
    (define-key map (kbd "t") 'dm-sketch-select-tile)
    (define-key map (kbd "T") 'dm-sketch-select-tool)
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
