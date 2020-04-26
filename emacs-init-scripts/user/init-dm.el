;;; init-dm.el --- initialize dungeon-mode for GNU Emacs  -*- lexical-binding: t; -*-

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

;; This is a sample init script for dugeon-mode.

;; You should change the value of `my-path' to wherever you have
;; install or clone path.

;; FFI: https://github.com/dungeon-mode/game

;; .el

;;; Code:
(let* ((my-path "d:/projects/dungeon-mode/"))
  (add-to-list 'load-path (concat my-path "src/"))
  (setq dm-files (list my-path))
  (setq dm-map-tags t)
  (require 'dungeon-mode)
  (require 'dm-map))

(provide 'init-dm)
;;; init-dm.el ends here
