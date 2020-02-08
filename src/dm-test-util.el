;;; dm-test-util.el --- macros and internal settings for testing dungeon-mode  -*- lexical-binding: t; -*-

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

;; testing utiliities.

;;; Code:

(defmacro dm-test-buffer-or-file-load-path (&optional bof-name)
  "Build a temporary `load-path' for a test's context package.

BOF-NAME allows an alternative to searching first amung variable
`buffer-file-name' and then function `buffer-name'."
  `(list (file-name-directory (or bof-name
				  buffer-file-name
				  (buffer-name)))))

(defun dm-test-maybe-require (feature &optional forcep)
  "Load FEATURE when not loaded or FORCEP is t.

Differs from `require' (which it uses) in that `load-path' is
limited to the directory of the current buffer-file or buffer,
assumed to be a test program being harnessed or evaluated."
  (unless (or forcep (featurep feature))
    (let ((load-path (dm-test-buffer-or-file-load-path)))
      (require feature)
      t))) ;; t if require was called and no error raised

(provide 'dm-test-util)
;;; dm-test-util.el ends here
