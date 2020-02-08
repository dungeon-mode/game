;;; dm-util-tests--remove-keywords.el --- test --remove-keywords from dm-util  -*- lexical-binding: t; -*-

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

(require 'dm-test-util)

(ert-deftest dm-test-remove-keywords-nil ()
  "transformation `dm--remove-keywords'"
  :tags '(:dm-util :internal :transformation)
  (should (equal nil (dm--remove-keywords nil)))
  )

(ert-deftest dm-test-remove-keywords-nil2 ()
  "transformation `dm--remove-keywords'"
  :tags '(:dm-util :internal :transformation)
  (should (equal nil (dm--remove-keywords '())))
  )

(ert-deftest dm-test-remove-keywords-arg ()
  "transformation `dm--remove-keywords'"
  :tags '(:dm-util :internal :transformation)
  (should (equal '(arg) (dm--remove-keywords '(arg))))
  )

(ert-deftest dm-test-remove-keywords-kw ()
  "transformation `dm--remove-keywords'"
  :tags '(:dm-util :internal :transformation)
  (should (equal nil (dm--remove-keywords '(:kw))))
  )

(ert-deftest dm-test-remove-keywords-kw+arg ()
  "transformation `dm--remove-keywords'"
  :tags '(:dm-util :internal :transformation)
  (should (equal nil (dm--remove-keywords '(:kw arg))))
  )

(ert-deftest dm-test-remove-keywords-arg+kw ()
  "transformation `dm--remove-keywords'"
  :tags '(:dm-util :internal :transformation)
  (should (equal '(arg) (dm--remove-keywords '(arg :kw))))
  )

(ert-deftest dm-test-remove-keywords-arg+kw+arg ()
  "transformation `dm--remove-keywords'"
  :tags '(:dm-util :internal :transformation)
  (should (equal '(arg) (dm--remove-keywords '(arg :kw arg2))))
  )

(ert-deftest dm-test-remove-keywords-arg+kw+arg+arg ()
  "transformation `dm--remove-keywords'"
  :tags '(:dm-util :internal :transformation)
  (should (equal '(arg arg3) (dm--remove-keywords '(arg :kw arg2 arg3))))
  )

(ert-deftest dm-test-remove-keywords-kw+arg+arg+arg ()
  "transformation `dm--remove-keywords'"
  :tags '(:dm-util :internal :transformation)
  (should (equal '(arg arg3) (dm--remove-keywords '(:kw arg2 arg arg3))))
  )


(provide 'dm-util-tests--remove-keywords)
;;; dm-util-tests--remove-keywords.el ends here
