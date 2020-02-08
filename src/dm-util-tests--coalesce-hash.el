;;; dm-util-tests--coalesce-hash.el --- test coalesce-hash from dm-util  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'dm-util)

(ert-deftest dm-test-coalesce-hash/strings ()
  "macro `dm-coalesce-hash'"
  :tags '(:dm-util :macro :internal)
  (dm-coalesce-hash (("h1" "H2" "H3")
		     ("r1" "V12" "V13")
		     ("r2" "V22" "V23")
		     ("r3" "V32" "V33"))))

(ert-deftest dm-test-coalesce-hash ()
  "macro `dm-coalesce-hash'"
  :tags '(:dm-util :macro :internal)
  (let ((dm-default-coalesce-key '(id _ h1)))
    (dm-coalesce-hash (("h1" "H2" "H3")
		       ("r1" "V12" "V13")
		       ("r2" "V22" "V23")
		       ("r3" "V32" "V33")))))

(ert-deftest dm-test-coalesce-hash ()
  "macro `dm-coalesce-hash'"
  :tags '(:dm-util :macro :internal)
  (dm-coalesce-hash (("h1" "H2" "H3")
		     ("r1" "V12" "V13")
		     ("r2" "V22" "V23")
		     ("r3" "V32" "V33"))
      nil
    (puthash (nth 0 row) row hash)
    :after nil))

(ert-deftest dm-test-coalesce-hash ()
  "macro `dm-coalesce-hash'"
  :tags '(:dm-util :macro :internal)
  (dm-coalesce-hash (("h1" "H2" "H3")
		     ("r1" "V12" "V13")
		     ("r2" "V22" "V23")
		     ("r3" "V32" "V33"))
      (_)
    (puthash (intern (nth 0 row)) row hash)
    :after nil
    ))

(ert-deftest dm-test-coalesce-hash ()
  "macro `dm-coalesce-hash'"
  :tags '(:dm-util :macro :internal)
  (dm-coalesce-hash (("h1" "H2" "H3")
		     ("r1" "V12" "V13")
		     ("r2" "V22" "V23")
		     ("r3" "V32" "V33"))
      (_)
    :after
    (puthash (intern (nth 0 row)) (cdr row) hash)))

(ert-deftest dm-test-coalesce-hash ()
  "macro `dm-coalesce-hash'"
  :tags '(:dm-util :macro :internal)
  (dm-coalesce-hash (("h1" "H2" "H3")
		     ("r1" "V12" "V13")
		     ("r2" "V22" "V23")
		     ("r3" "V32" "V33"))
      (id bob)
    (list (quote bob) bob)))



(ert-deftest dm-test-coalesce-hash ()
  "macro `dm-coalesce-hash'"
  :tags '(:dm-util :macro :internal)
  (dm-coalesce-hash (("h1" "H2" "H3")
		     ("r1" "V12" "V13")
		     ("r2" "V22" "V23")
		     ("r3" "V32" "V33"))
      (id _ h2)
    :start-column 0
    (list 'id (format "id:<%s>" id) '2 h2 'raw row)))

(ert-deftest dm-test-coalesce-hash ()
  "macro `dm-coalesce-hash'"
  :tags '(:dm-util :macro :internal)
  (let ((h (dm-coalesce-hash (("h1" "H2" "H3")
			      ("r1" "V12" "V13")
			      ("r2" "V22" "V23")
			      ("r3" "V32" "V33"))
	       (id h2)
	     :hash-table #s(hash-table size 30 test equal)
	     :start-column 1
	     (list 'id id 'h2 h2))))
    ;;(print h)
    h))

(provide 'dm-util-tests--coalesce-hash)
;;; dm-util-tests--coalesce-hash.el ends here
