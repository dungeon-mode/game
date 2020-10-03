;;; dm-env.el --- define persistantly shadowed bindings  -*- lexical-binding: t; -*-

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

;; Global collection and convenience methods for shadow bindings and other
;; contexualixed (e.g. per buffer, section, etc.) stashes of information.

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(let ((load-path (append '(".") load-path)))
  (require 'dungeon-mode)
  (require 'dm-util))

(defvar dm-env-alist nil
  "Atypical alist, envs created this session.

Entries are in the form (NAME ORG-NAME [DATA]), where NAME and
ORG-NAME are strings giving the name of the buffer and `org-mode'
buffer, and DATA provides a plist of other information.")

(defvar dm-env-plist-offset 2 "Index into alist where plist starts.")

(defvar dm-env-vars
  '((dm-draw-size  (10 . 10))
    (dm-draw-scale (40 . 40))
    (dm-draw-nudge (1 . 1))
    (dm-draw-layer-alist ((background scaled)
			  (underlay dom)
			  (path)
			  (overlay dom)))
    (dm-map-tiles dm-make-hashtable)
    (dm-map-level dm-make-hashtable))
  "Plist containing default env settings.")

(defmacro dm-foo-defvar (name vars)
  "Avoid warnings about undefined VARS."
    (declare
     (indent defun)
     (debug (&define name lambda-list)))
    `(ingore ,name ,vars))

(defun dm-env-forget (&optional name-or-filter)
  "Forget NAME-OR-FILTER or all environments."
  (setq dm-env-vars
	(when name-or-filter
	  (if (functionp name-or-filter)
	      (delq nil (mapcar name-or-filter dm-env-vars))
	    (remove (assoc name-or-filter dm-env-alist) dm-env-alist)))))

(defun dm-env-push (env)
  "Place ENV at `car' of `dm-env-alist'."
  (declare (indent 0))
  (car (push env dm-env-alist)))

(defsubst dm-env-has (name)
  "Return nil when NAME des not exist."
  (assoc name dm-env-alist))

(defun dm-env-put (name var value &optional no-error)
  "Store the NAME specific VALUE for VAR.

When NO-ERROR is non-nil do not error when NAME is unknown."
  (declare (indent defun))
  (if-let ((env-info (assoc name dm-env-alist)))
       (setcdr (nthcdr (1- dm-env-plist-offset) env-info)
	       (plist-put (nthcdr dm-env-plist-offset env-info)
			  var value))
     (unless no-error (user-error "Unknown env \"%s\"" name))))

(defun dm-env-get (name var &optional no-default no-error)
  "Get NAME specific value of VAR.

When VAR is unset and NO-DEFAULT is non-nil return nil instead of
a default.  Do not error on unknown NAME if NO-ERROR is non-nil."
  (declare (indent 1))
  (if-let ((env-info (assoc name dm-env-alist)))
      (when-let ((val
		  (let ((plist (nthcdr dm-env-plist-offset env-info)))
		    (if no-default
			(plist-get plist var)
		      (if (plist-member plist var)
			    (plist-get plist var)
			  (cadr (assoc var dm-env-vars)))))))
	(if (functionp val) (funcall (symbol-function val)) val))
    (unless no-error (user-error "Unknown env \"%s\"" name))))

(defmacro dm-with-env (name vars &rest body)
  "Bind VARS from the environment of NAME and execute BODY."
  (declare (indent defun))
  (let ((vars (if (null vars) (list 'dm-env-name)
		(if (symbolp vars) (list vars)
		  vars))))
    `(let ,(append
	   (mapcar
	    (lambda (vix)
	      (let ((var (nth vix vars)))
		`(,var (nth ,vix (dm-env-has ,name)))))
	    (number-sequence 0 (1- (length vars))))
	   (mapcar
	    (lambda (var)
	      `(,var (dm-env-get ,name ',var nil t)))
	    (mapcar 'car dm-env-vars)))
       (prog1 ,@body
	 ,@(mapcar (lambda (var) `(dm-env-put ,name ',var ,var t))
		   (mapcar 'car dm-env-vars))))))

(provide 'dm-env)
;;; dm-env.el ends here
