;;; dm-svg-tests.el --- tests for dm-svg             -*- lexical-binding: t; -*-

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

;; Tests for `dm-svg' which see

;;; Code:

(defmacro dm-test-load-path ()
  "Build a temporary `load-path' for a test's context package."
  `(list (file-name-directory (or buffer-file-name (buffer-name)))))

(defun dm-test-maybe-require (package &optional forcep)
  "Require PACKAGE when not loaded or FORCEP is t."
  (unless (or forcep (featurep 'dm-svg))
    (require package)))

;; TODO factor out above macro and func
;; TODO consider buttercup.el https://github.com/jorgenschaefer/emacs-buttercup

(ert-deftest dm-svg--require ()
  "require `dm-svg'"
  :tags '(:dm-svg :requires)
  (let ((load-path (dm-test-load-path)))
    (should (eq 'dm-svg (require 'dm-svg)))))

(ert-deftest dm-svg-dom-node-p ()
  "predicate `dm-svg-dom-node-p'"
  :tags '(:dm-svg :dom :predicate)
  (should (equal nil (dm-svg-dom-node-p nil)))
  (should (equal nil (dm-svg-dom-node-p '())))
  (should (equal t (dm-svg-dom-node-p '(tag))))
  (should (equal nil (dm-svg-dom-node-p '(tag) 'nottag)))
  (should (equal t (dm-svg-dom-node-p '(tag) 'tag)))
  (should (equal t (dm-svg-dom-node-p (dom-node 'tag) 'tag)))
  (should (equal t (dm-svg-dom-node-p (dom-node 'tag '((attr . "val"))
						) 'tag)))
  (should (equal t (dm-svg-dom-node-p (dom-node 'tag nil (dom-node 'child)
						) 'tag)))
  (should (equal t (dm-svg-dom-node-p (dom-node 'tag
						'((attr . "val"))
						(dom-node 'child)
						) 'tag))))

(ert-deftest dm-svg-or-nil-p ()
  "predicate `dm-svg-or-nil-p'"
  :tags '(:dm-svg :svg :predicate)
  (should (eq 2 (length (delete
			 nil
			 (mapcar 'dm-svg-or-nil-p
				 (list nil t (svg-create 500 500))))))))

(ert-deftest dm-svg-path-or-path-data-p ()
  "predicate `dm-svg-path-or-path-data-p'"
  :tags '(:dm-svg :svg :predicate)
  (should (eq nil (dm-svg-path-or-path-data-p 'foo)))
  (should (eq t (dm-svg-path-or-path-data-p nil)))
  (should (eq t (dm-svg-path-or-path-data-p "")))
  (should (eq t (dm-svg-path-or-path-data-p (dom-node 'path)))))

(ert-deftest dm-svg-create-path ()
  "function `dm-svg-create-path'"
  :tags '(:dm-svg :svg :predicate)
  (should (dm-svg-create-path))
  (should (equal (dom-node 'path '((d)))
		 (dm-svg-create-path)))
  (should (equal (dom-node 'path '((d)))
		 (dm-svg-create-path nil)))
  (should (equal (dom-node 'path '((d . "")))
		 (dm-svg-create-path "")))
  (should (equal (dom-node 'path '((d . "m1,1")))
		 (dm-svg-create-path "m1,1")))
  (should (equal (dom-node 'path '((d . "") (stroke . "black")))
		 (dm-svg-create-path "" '((stroke . "black")))))
  (should (equal (dom-node 'path '((d . "")) (dom-node 'child))
		 (dm-svg-create-path "" nil (dom-node 'child) ))))

(ert-deftest dm-svg--create ()
  "constructor `dm-svg'"
  :tags '(:dm-svg :svg :create)
  (should
   (prog1 t (dolist (x (list '(nil . nil) `(,(svg-create 500 500) . "v100")))
	      (dm-svg :svg (car x) :path-data (cdr x))))))

(ert-deftest dm-svg-add-svg-element ()
  "method `add-path-data'"
  :tags '(:dm-svg :svg :method)
  (should
   (equal (dom-append-child (dom-node 'svg) (dom-node 'child))
	    (let ((o (dm-svg)))
	      (add-svg-element o (dom-node 'child))
	      ))))

(ert-deftest dm-svg-add-path-data ()
  "method `add-svg-element'"
  :tags '(:dm-svg :svg :method)
  (should
   (string= "v100h100v-100h-100"
	    (let ((o (dm-svg :svg (svg-create 500 500) :path-data "v100")))
	      (add-path-data o "h100v-100h-100")))))

(ert-deftest dm-svg-render-and-insert ()
  "method `render-and-insert'"
  :tags '(:dm-svg :svg :method)
  (let* ((c (with-temp-buffer
	      (render-and-insert (dm-svg :svg (svg-create 100 100)
					 :path-data
					 "m100,100h100v100h-100v-100"))
	      ;;(buffer-string)
	      (plist-get (text-properties-at 1) 'display)))
	 (d (plist-get (cdr c) ':data))
	 (s (plist-get (cdr  c) ':type)))
    (message "c:%s\nd:%s\ns:%s" c d s)
    (should (eq s 'svg))
    (should (string= d (concat
			"<svg width=\"100\" height=\"100\""
			" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">"
			" <path d=\"m100,100h100v100h-100v-100\"></path></svg>")))
    ))

;; :ease-of-eval
;; (render-and-insert (dm-svg :svg (svg-create 500 500 :stroke "none" :fill "none")
;; 			   :path-data
;; 			   (dm-svg-create-path "m100,100h100v100h-100v-100"
;; 					       '((stroke . "green")
;; 						 (stroke-width  .  3))
;; 					       )))



(provide 'dm-svg-tests)
;;; dm-svg-tests.el ends here
