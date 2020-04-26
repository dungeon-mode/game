;;; dungeon-mode.el --- create and play turn-based multi-player RPG style games  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Keywords: games, outlines, multimedia, wp

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

;; This is the main program file for `dungeon-mode', an Emacs package
;; providing RPG gaming features including turn-based play within
;; Emacs and enhancements to `org-mode' for campaign game development.

;; The project is currently homed on GitHub; we welcome your
;; contributions especially including issue reports and pull requests.

;; http://github.com/mplsCorwin/dungeon-mode

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'seq)

;; Use this in other libraries but avoid linking
;; in any but utilities/macros from this file:
;; DEVEL hack: prefer version from CWD, if any
;; (let ((load-path (append (list ".") load-path)))
;;   (require 'dm-util)
;;   (require 'dm-table)
;;   (require 'dm-map))

(defgroup dungeon-mode nil
  "Settings for RPG Gameplay and Authoring.

Settings in this group control `dungeon-mode', an Emacs Lisp and
org-mode extention for RPG developing and playing turn-based
role-playing syle games in Emacs, and for publishing such games
for binding for traditional or semi-computer assisted play.

Dungeon is based on \"Minneapols Dungeon\" a predecesor to early
popular close-source RPG titles.  Dungeon shares with Emacs and
GNU culture a tradition of distain for closing off ideas and
limiting access to tools behind pay-walls and trademarks.

The Default Dungeon provided is a playable example game as well
as the context for most if not all examples in the (info
\"Dungeon\"), which see.  See the Extending Dungeon-Mode section
for help using dungeon-mode to create or emulate other games and
systems.

The settings below provide control over editing and game-play
including for both players and \"Dungeon Masters\"."
  :group 'games)

(defgroup dm-files nil "File Path and Permission Settings."
  :group 'dungeon-mode)

;; file system settings
(defcustom dm-files nil
  "A list of game source paths.

This ia a list of files or folders `dungeon-mode' can open,
looking for game sources, your character sheets, and eveything
else it needs.  Files in this list are not implicitly writable;
see `dm-files-writable'."
  :group 'dungeon-mode
  :group 'dm-files
  :type 'list)

(defcustom dm-files-expand t
  "Expand directories when encountered.

When t, this setting enables updating `dm-files', expanding
directory contents recursively.  Otherwise 'dm-files' is not
modified.  When set to another non-nil value it is a function to
perform the expansion, replacing the default
`dm-files-select-impl'.")

(defcustom dm-files-writable nil
  "A list of writable game source paths.

This is a list of files or folders `dungeon-mode' can write to,
for example to save campaign notes during gameplay.  Note,
`dungeon-mode' does not attempt to inhibit any direct editing."
  :type 'list)

(defcustom dm-files-alist
  (list
   (list :map-cells
	 (apply-partially #'dm-files-filter-by-re
			  "Docs/Maps/levels/A-Maze_level[[:digit:]]+.org$"))
   (list :map-tiles (apply-partially #'dm-files-filter-by-re
				     "Docs/Maps/map-tiles.org$")))
  "Alist mapping game features to file filter functions.

Entries have the form:

  (KEYWORD . FUNCTION)

Where KEYWORD is a symbol representing types of information
`dungon-mode' can work with, and FUNCTION is an Emacs Lisp
function which receives and returns a list of files and
folders, or a list of such functions."
  :type 'alist)

(defvar dm-files-select-hook nil
  "Default hooks for `dm-files-select', if any.

Functions listed here are executed for each file or directory
cosidered after `dm-files-select-impl' but before any supplied in
`dm-files-alist'.")

(defun dm-files-filter-by-folder (folder file)
  "Return FILE if it is within FOLDER, otherwise nil."
  (if folder
      (if (string-match-p (concat "^" folder) (directory-file-name file))
	  file
	nil)
    (user-error "No folder for files filter")))

(defun dm-files-filter-by-re (re file)
  "Match FILE with RE (a string) and return FILE, otherwise nil."
  (if re (if (string-match-p re (directory-file-name file)) file nil)
    (user-error "No RE for files filter")))

(defun dm-files-select-impl (file)
  "Turn FILE into a list, recursively find all when FILE is a directory."
  (if file
      ;; FIXME we absolutely require the trailing slash to ID folders
      (if (directory-name-p file)
	  (directory-files-recursively file ".*" t nil t)
	(if (file-readable-p file) (list file) nil))
    nil))

(defun dm-files-select (&optional keyword for-writing)
  "Return the files that may be read.

With KEYWORD, a keyword symbol, refine the results.  When
FOR-WRITING is t, also return folders but only files or folder
which may be written to."
  (let ((files dm-files) this-file dm-files-select-hook results)
    (mapc (lambda (func) (add-hook 'dm-files-select-hook func))
	  (delq nil (append (list #'dm-files-select-impl)
			    (when for-writing
			      (list (lambda (x)
				      (member x dm-files-writable))))
			    dm-files-select-hook
			    (when keyword
		              (cdr (assoc keyword dm-files-alist))))))
    (while (setq this-file (pop files))
      (if (directory-name-p this-file)
	  (setq files
		(append files (directory-files-recursively this-file ".*" t)))
	(if (run-hook-with-args-until-failure 'dm-files-select-hook this-file)
	  (push this-file results))))
    results))

;; message settings
(defgroup dm-msg nil "Messaging and notification settings."
  :group 'dungeon-mode)

(defcustom dm-msg-enabled nil
  "Buffer where messages are written or nil to suppress all messages."
  :type 'string)

(defcustom dm-msg-format "[$file-$fun] $msg $args"
  "Format for `dungeon-mode' messages.

Placeholders are a \"$\" (dollar sign) followed by a key from a
message plist.  Key are potentially any value but must strictly
match a key from the plist from the message being formatted,
otherwise the empty string is subsitiuted instead."
  :type 'string)

(defcustom dm-msg-props (list :file :fun :msg :args)
  "List of known message properties."
  :type '(list string))

(defcustom dm-msg-arg-props nil
  "A list of message properties to always treat as arguments.

Generally this means appending them along with their values to
the end of each message shown."
  :type 'list)

(defvar dm-msg-filter-hook nil
  "Abnormal hook to filter and format `dungeon-mode' messages.

Functions on this hook match this prototype:

  [string | t] DM-MSG-HOOK-FUNC (PLIST ARGS)

Where PLIST contains properties about the message and sender,
ARGS are the message arguments suitable for a call to apply
`format', and MSG-HOOK-FUNC will return either a formatted
message or t to display the message (when not otherwise
inhibited, e.g. by `dm-msg-enabled').  See `dm-msg'.")

(defun dm-msg-impl (&rest plist)
  "Implement message display.

Keys of PLIST are replaced into `dm-msg-format' where they
appear.  Keys of PLIST mentioned in `dm-msg-arg-props' are
appended to the end along with associated values."
  (let* ((result (or (plist-get plist :fmt) dm-msg-format))
	(plist (copy-sequence plist))
	(keys (seq-uniq (append (seq-filter 'keywordp plist)
				dm-msg-props dm-msg-arg-props)))
	(re (apply
	     'concat
	     (list
	      "$\\("
	      (mapconcat (lambda (key)
			   (if (keywordp key)
			       (let ((nm (symbol-name key)))
				 (substring nm 1 (length nm)))
			     key))
			 keys
			 "\\|") "\\)"))))
    (unless (plist-member plist :args)
      (plist-put plist :args dm-msg-arg-props))
    (when-let ((args (plist-get plist :args)))
      (plist-put plist :args
		 (prin1-to-string
		  (mapcar
		   (lambda (arg)
		     (cons arg (plist-get plist arg)))
		   (plist-get plist :args))
		  t)))
    (mapc (lambda (prop)
	    (plist-put plist prop (format "%s" (or (plist-get plist prop) ""))))
	  keys)
    (unless (plist-member plist :args) (plist-put plist :args dm-msg-arg-props))
    (when (and re result)
      (replace-regexp-in-string
       re
       (lambda (x)
	 (let ((sym-name (intern (concat ":" (substring x 1)))))
	   (or (plist-get plist sym-name) "")))
       result))))
;;(dm-msg-impl (list :fmt "$file:$bar" :file "foo" :bar "baz" :args (list :bar))

(defun dm-msg (&rest args)
  "Maybe display a message.

ARGS form a plist used to filter messages for display as well as
to format them.  The output buffer is set by `dm-msg-enabled'.
Add filters to `dm-msg-filter-hook'.  The output format is
control by `dm-msg-format'.  A list of additional \"argument\"
keys can be provided either by binding `dm-msg-arg-props' or by
sending the \":args\" keyword within a particular message."
  (and dm-msg-enabled args
       (let ((result (run-hook-with-args-until-success
		      'dm-msg-filter-hook args)))
	 (message (if (equal t result) (apply 'dm-msg-impl args)
		    result)))))

(defmacro dm-msg-enable-for-impl (plist)
  "Create a filter function to match messages per PLIST."
  `(lambda (args)
     (if (and ,@(mapcar
		 (lambda (arg)
		   `(equal ,(plist-get plist arg)
			   (plist-get args ,arg)))
		 (seq-filter 'keywordp plist)))
	 t nil)))

(defun dm-msg-enable-for (&rest plist)
  "Enable messages per PLIST.

PLIST provides constrains to enable messages.  Keys are keyword
symbols as for `dm-msg' and values are literals to match."
  (setq dm-msg-enabled t)
  (add-to-list 'dm-msg-filter-hook
	       (if (null plist)
		   (lambda (&rest _) t)
		 (eval `(dm-msg-enable-for-impl ,plist)))))

;; (defun dm-mag-enable (&optional arg)
;;   "Enable messages from `dungeon-mode'.
;; With a prefix ARG, first clear existing message filters.  PLIST
;; controls which messages are displayed.  Interactively, create
;; PLIST by prompting in turn first for a key, then for a value
;; until given a blank entry.  An empty PLIST enables all messages."
;;   (interactive ;; load-library@1041c:/emacs26.3/share/emacs/26.3/lisp/files.el
;;    (let ((last-key)
;; 	 (args (seq-uniq (append dm-msg-props dm-msg-arg-props))))
;;      (list (completing-read (format "Filter (%s): " (if last-key "value" "key"))
;; 			    args
;; 			    (lambda(f) (string-match-p ".elc?$" f))))
;;      (message "args:%s" args)
;;      args)))

(provide 'dungeon-mode)
;;; dungeon-mode.el ends here
