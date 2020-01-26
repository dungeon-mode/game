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
		   (require 'cl))

;; DEVEL hack: prefer version from CWD, if any
(let ((load-path (append '(".") load-path))) (require 'dm-svg))

;;; Code:

(defvar dm-map-features-table-property-tag "MAP-FEATURES")

;;;;;
;; quick and dirty procedural approach

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
  (ignore scale cells)
  (dolist (cell cells)
    (message "cell:%s" cell)
    (cond
     ((dm-svg-dom-node-p cell 'text)
      (message "svg text!")
      (when (eq 'text (car cell))
	;; scale SVG text elements right here!
	(dom-set-attribute cell 'text-size (* (car (dom-attr cell 'text-size))
					      (car scale)))
	))
     ((consp cell)
      (message "consp!")
      (pcase cell
	;; move or line in the form (sym (h v)
	(`(,(or 'm 'l)
	   (,(and h (guard (numberp h)))
	    ,(and v (guard (numberp v)))))
	 ;;(message "1:%s 2:%s" (caadr cell) (cadadr cell))
	 (setcdr cell (list (* h (car scale))
			    (* v (cdr scale)))))

	;; h and v differ only in which part of scale applies
	(`(h (,(and d (guard (numberp d)))))
	 (setcdr cell (list (* d (car scale)))))
	(`(v (,(and d (guard (numberp d)))))
	 (setcdr cell (list (* d (cdr scale)))))

	(`(a (,rx ,ry ,x-axis-rotation ,large-arc-flag ,sweep-flag
		,(and h (guard (numberp h)))
		,(and v (guard (numberp v)))))
	 (setcdr cell (list rx ry x-axis-rotation large-arc-flag sweep-flag
			    (* h (car scale))
			    (* v (cdr scale)))))

	;; fall-back to a message
	(_ (message "unhandled %s => %s" (type-of cell) cell))
	))
     ))
  cells)

;;(dm-map-default-scale-function '(100 . 1000) (dom-node 'text '((text-size .5))) '(h (.1)) '(v (.2))'(m (.3 .4)) '(l (.5 .6)) '(x (.7 .8)) '(a (.7 .7 0 1 1 0 .14)))

(cl-defun dm-map-quick-draw (features
			     cells
			     &key
			     (scale 100)
			     (size '(32 . 40))
			     viewbox
			     svg-attributes
			     path-attributes
			     (scale-function 'dm-map-default-scale-function)
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
  (ignore features cells scale size viewbox svg-attributes path-attributes scale-function))

(defun dm-map-load-tagged-tables-in-files (predicate &rest org-files)
  "Return a list of rows from tables with TAG in ORG-FILES.

ORG-FILES is a list of file-names as strings.  PREDICATE is a
function which receives the point-or-marker (POM) indicating the
start position for a given `org-table' and should return true if
the table is to be included.

Results are a single list where each element represents a table
row populated with the cell values as strings.  Errors are thown
if a file doesn't exist, cannot be opened, etc."
  (let ((files org-files)
	this-file
	feature-tables)
    (while (setq this-file (pop files))
      (with-temp-buffer
	(insert-file-contents this-file)
	(org-element-map (org-element-parse-buffer) 'table
	  (lambda (table)
	    (let ((tpom (org-element-property :begin table)))
	      (and (funcall predicate tpom)
		   (push (save-excursion
			   (goto-char tpom)
			   (cdr (delete 'hline (org-table-to-lisp))
				)) feature-tables)))))))
    (apply `seq-concatenate 'list feature-tables)))

(defun dm-map-load-level (name &rest org-files)
  "Return a list of cells in map-level NAME from ORG-FILES."
  (apply 'dm-map-load-tagged-tables-in-files
	 (lambda (tpom)
	   (message "%s=%s" name (org-entry-get tpom "NAME"))
	   (string= name (org-entry-get tpom "NAME"))
	   ) org-files))
;;(dm-map-load-level "regression-test-map-level" "../Docs/Maps/Design.org")

(defun dm-map--parse-map-level (&rest rows)
  "Return a list of map-level cells from `org-table' ROWS."
  (mapcar (lambda (cell)
	    (cl-destructuring-bind (x y plan dstr nstr) cell
	      (list 'pos (cons (string-to-number x)
			       (string-to-number y))
		    'plan plan
		    'docs  (delete "" (list dstr nstr))
		    ))) rows))

;; (apply 'dm-map--parse-map-level (dm-map-load-level "regression-test-map-level" "../Docs/Maps/Design.org"))

;; TODO we need to split out :keywords that may be run together
;; with repeated feature names

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

;;(dm-map-load-feature-files  "../Docs/Maps/Design.org" "../Docs/Maps/test.org")
;;(car(dm-map-load-feature-files "../Docs/Maps/test.org"))

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
    (dolist (strings feature-attributes-list)
      (dm-map--init-hash-entry hash strings names))
    (dm-map--parse-plan hash)
    hash
    ;; (dolist (tbl tables features)
    ;;   (push (cdddr tbl) features))
    ))


;; (apply 'dm-map-defeatures (dm-map-load-feature-files  "../Docs/Maps/test.org" "../Docs/Maps/test.org"))

(cl-defun dm-map--init-hash-entry (hash (feature plan docstring narrative)
					feature-list)
  "Add or update an entry in HASH for STRINGS.

Strings are a list in the form:
  (\"FEATURE\" \"PLAN\" \"DOCSTRING\" \"NARRATIVE\")

FEATURE-LIST is a list of all known features.

While HASH entries are keyed on FEATURE associated to a plist
with the following possable keys:

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

(defun dm-map--parse-plan (hash)
  "Cross-map HASH keys to resolve features as SVG code."
  (maphash (lambda (outer-k outer-v)
	     (when (plist-member outer-v 'paths)
	       (plist-put outer-v
			  'paths
			  (mapcan (lambda (word)
				    (if (symbolp word)
					(plist-get (gethash word hash word)
						   'paths)
				      (list word)))
				  (plist-get outer-v 'paths))))
	     ) hash))

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

;;(apply 'dm-map-defeatures (dm-map-load-feature-files  "../Docs/Maps/test.org" "../Docs/Maps/test.org"))

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

(defclass dm-map ()
  ((svg :type (or dm-svg-or-nil dm-svg) :initarg :svg :initform (dm-svg))
   (size :type dm-map-size :initarg :size :initform (list 1.0 1.0))
   (scale :type (integer 1 100) :initarg :scale :initform 100)
   (view-box :type (or null dm-map-viewbox) :initarg :view-box :initform nil)
   (svg-attributes :type list :initarg :svg-attributes :initform nil)
   (path-attributes :type list :initarg :path-attributes :initform nil)
   (scale-function :type function :initarg 'dm-map-s))
  :documentation
  "Create and update RPG maps displayed as inline SVG images.")

(cl-defmethod initialize-instance :after ((object dm-map) &rest _)
  "Initialize instance slots after inital argument processing."
  (with-slots (svg size svg-attributes) object
    (unless (dm-svg-p 'svg)
      (if (dm-svg-dom-node-p object 'svg)
	  (setq svg (dm-svg :svg svg))
	(let ((so (svg-create (car size) (cdr size))))
	  (dom-set-attributes so svg-attributes)
	  (setq svg (dm-svg :svg so)))))))

(dm-map)

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

  Generally meaning, append to the \"d\" attribute value for the primary
  path representing chambers, corridor and secret doors in map.

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
    ( SVG-STRING . PATH-STRING ) Where SVG-STRING is SVG code other
  than the main draw path and PATH-STRING is the path-data for the
  main-draw path."
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
