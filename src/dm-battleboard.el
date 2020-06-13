;;; dm-battleboard.el --- track player chracter hit points visually  -*- lexical-binding: t; -*-

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

;; POC for hit counter using dm-map; variously quick *and* dirty

;; NOTE: This expects write accccess to the below hard-coded file.

;;; Code:

;; DEVEL hack: prefer version from CWD, if any
(let ((load-path (append '(".") load-path)))
  (require 'dm-map))

(defvar dm-bb-file (dm-files-select :bb-party)
  "File containing the party vital statistics.")

(defvar dm-bb-cols nil "The battleboard `org-table' colums.")

(defvar dm-bb-party nil)

(defvar dm-bb-slot-to-pos nil)


;; DEVEL: give us a global key binding precious
(global-set-key (kbd "<f8>") 'dm-bb-draw)


(add-to-list 'dm-files-alist
	     (list :bb-party
		   (apply-partially #'dm-files-filter-by-re
				    "Docs/battleboard/bb-party[.]org$")))

(defun dm-bb-party-transform (table)
  "Transform a TABLE of strings into an hash-map."
  (let* ((cols (setq dm-bb-cols
		     (seq-map (lambda (label) (intern (downcase label)))
			      (car table))))
	 (cform
	  `(dm-coalesce-hash ,(cdr table) ,cols
	     :key-symbol (let ((x (string-to-number slot))
			       (y (string-to-number line)))
			   (let ((pos
				  (cons (+ 1 (* 3 x) (* 6 x))
					(+ 9 (* 4 y) (* 6 y)))))
			     (setq dm-bb-slot-to-pos
				   (append dm-bb-slot-to-pos
					   (list (list (cons x y) pos))))
			     pos))
	     :hash-table dm-map-level
	     (append (list dm-map-draw-prop (list 'bb-char-box))
		     (mapcan
		      (lambda (col)
			(list col (symbol-value col)))
		      ',cols))))
	 (result (eval `(list :load ,cform)))
	 )
    ;;(message "cform:%s" cform)
    result))

(defun dm-bb-file-set-field (slot row field value)
  "Update the file backing the battle-board.

Set FIELD to VALUE for character in SLOT/ROW."
  (when-let ((field-ix (seq-position dm-bb-cols
				     (if (stringp field)
					 (intern field)
				       field))))
    (save-window-excursion
      (with-temp-buffer
	(find-file (car-safe (dm-files-select :bb-party)))
	(org-with-wide-buffer
	 (beginning-of-buffer)
	 (while (not (or (eobp) (org-at-table-p)))
	   (forward-line))
	 (when (org-at-table-p)
	   ;;(let ((spos (point))))
	   (org-table-analyze)
	   (forward-line (+ 2 slot (* 4 row)))
	   (org-table-get-field
	    (1+ field-ix)
	    (if (equal 0 value) "" (format "%s" value)))
	   ;;(message "content %s" (org-table-get-field (1+ field-ix)))
	   (save-buffer 0)
	   (dm-bb-draw)))))))

(defvar dm-bb-dmg 1 "Number by which to increment/decrement damage.")

(defun dm-bb-dmg (dmg)
  "Set the default DMG for incrementing/decrementing."
  (interactive "NBase damage:")
  (setq dm-bb-dmg dmg))

(defun dm-bb-mouse1 ()
  "Handle mouse1 on the battle-board."
  (interactive "@")
  (let* ((dm-map-scale 20)
	 (pos (dm-map--pos-impl)))
    (if (or (> 9 (cdr pos))
	    (< 26 (cdr pos))
	    (< 35 (car pos))
	    (seq-find (apply-partially 'equal (car pos))
		      (list 0 9 18 27)))
	(message "clicked %s" pos)
      (let ((kx  (floor (/ (car pos) 9)))
	    (ky (if (> (cdr pos) 16) 1 0))
	    (rt (not (> 4 (mod (car pos) 9))))
	    (bot (not (> 4 (mod (cdr pos) 9)))))
	(let ((field (if (and bot rt) 's2d
		       (if rt 'ad
			 (if bot 'sd
			   'bd)))))
	  (let ((old-value
		 (string-to-number
		  (or (plist-get
		       (gethash (cadr (assoc (cons kx ky) dm-bb-slot-to-pos 'equal))
				dm-map-level)
		       field)
		      "0"))))
	    ;; (message "HIT slot:%s row:%s rt:%s bot:%s field:%s old:%s %s"
	    ;; 	     kx ky rt bot field old-value
	    ;; 	     (plist-get dm-bb-slot-to-pos (cons kx ky)))
	    (dm-bb-file-set-field kx ky field (+ old-value dm-bb-dmg))))))))

(defun dm-bb-mouse2 ()
  "Handle mouse2 on the battle-board."
  (interactive "@")
  (let ((dm-bb-dmg (- dm-bb-dmg))) (dm-bb-mouse1)))

(setq-local dm-map-level nil)

(defvar dm-bb-mode-map
  (let ((map (make-sparse-keymap)))
    ;;(define-key map (kbd "+") 'dm-map-scale-nudge)
    ;;(define-key map (kbd "-") 'dm-map-scale-nudge-invert)
    ;; (define-key map (kbd "<up>") 'dm-map-scroll)
    ;; (define-key map (kbd "<down>") 'dm-map-scroll-invert)
    ;; (define-key map (kbd "<right>") 'dm-map-hscroll)
    ;; (define-key map (kbd "<left>") 'dm-map-hscroll-invert)
    (define-key map (kbd "*") 'dm-bb-dmg)
    (define-key map (kbd "d") 'dm-bb-draw)
    (define-key map (kbd "g") 'dm-bb-draw)
    (define-key map (kbd "r") 'dm-bb-draw)
    (define-key map [mouse-1] 'dm-bb-mouse1)
    (define-key map [mouse-2] 'dm-bb-mouse2)
    map)
  "Mode keymap for `dm-bb-mode'.")

(define-derived-mode dm-bb-mode dm-map-mode "BB"
  "Major mode for `dugeon-mode' battle-board.")

(defun dm-bb-draw ()
  "Draw the battleboard."
  (interactive)
  ;;(let ((party (list (list 'war01 'warrior 3 15 6 6)))))
  (let (;;(dm-map-files dm-bb-file)
	(dm-map-scale 20)
	(dm-map-menus-level-cells-draw-all t)
	(dm-map-preview-buffer-name "**battle board**")
	(dm-map-table-var-alist (append (list (list 'bb-char 'dm-bb-party))
					dm-map-table-var-alist))
	(dm-table-states (append (list (list 'bb-char :extract #'dm-table-extract-table
					     :transform #'dm-bb-party-transform))
				 dm-table-states))
	(dm-map-tags t)
	;;(dm-map-level (dm-make-hashtable))
	(dm-map-tiles (dm-make-hashtable))
	(dm-map-level-size (cons 44 33))
	(dm-bb-line-sep 1) ;; blanks between rows
	(dm-bb-top-sep 3)
	(cur-line 0)
	(cur-line-pos 0)
	(dm-map-draw-inhibit-help-echo t)
	(dm-map-image-map (list (cons 'poly (vector 1 9 5 9 5 11 1 11))))
	(dm-map-image-mode 'dm-bb-mode)
	dm-map-menus-play-mode
	;;dm-bb-party
	)
    (setq dm-bb-slot-to-pos nil)
    (dm-map-load (car-safe (dm-files-select :bb-party)))
    (puthash 'bb-char-box
	     (list dm-map-draw-prop
		   (list (list 'm (list 4 0))
			 (list 'v (list 8))
			 (list 'm (list -4 -4))
			 (list 'h (list 8))
			 (list 'm (list -2 -2))
			 (list 'v (list 4))
			 (list 'h (list -4))
			 (list 'v (list -4))
			 (list 'h (list 4)))
		   'overlay
		   (list
		    (list 'g
			  (list (cons 'x 2) (cons 'y 2)
				(cons 'width 4)
				(cons 'height 4))
			  (list 'text
				(list (cons 'stroke "blue")
				      (cons 'fill "blue")
				      (cons 'text-anchor "middle")
				      (cons 'dominant-baseline "central")
				      (cons 'font-size 1.7)
				      (cons 'x 4)
				      (cons 'y -.4))
				"$name")
			  (list 'text
				(list (cons 'stroke "red")
				      (cons 'fill "red")
				      (cons 'text-anchor "middle")
				      (cons 'dominant-baseline "central")
				      (cons 'font-size 1.5)
				      (cons 'x 1)
				      (cons 'y 3.5))
				"$bd")
			  (list 'text
				(list (cons 'stroke "black")
				      (cons 'fill "black")
				      (cons 'text-anchor "middle")
				      (cons 'dominant-baseline "central")
				      (cons 'font-size 1.5)
				      (cons 'x 3)
				      (cons 'y 3.5))
				"$bh")
			  (list 'text
				(list (cons 'stroke "red")
				      (cons 'fill "red")
				      (cons 'text-anchor "middle")
				      (cons 'dominant-baseline "central")
				      (cons 'font-size 1.5)
				      (cons 'x 7)
				      (cons 'y 3.5))
				"$ad")
			  (list 'text
				(list (cons 'stroke "black")
				      (cons 'fill "black")
				      (cons 'text-anchor "middle")
				      (cons 'dominant-baseline "middle")
				      (cons 'font-size 1.5)
				      (cons 'x 5)
				      (cons 'y 3.5))
				"$ah")
			  (list 'text
				(list (cons 'stroke "red")
				      (cons 'fill "red")
				      (cons 'text-anchor "middle")
				      (cons 'dominant-baseline "central")
				      (cons 'font-size 1.5)
				      (cons 'x 1)
				      (cons 'y 5.5))
				"$sd")
			  (list 'text
				(list (cons 'stroke "black")
				      (cons 'fill "black")
				      (cons 'text-anchor "middle")
				      (cons 'font-size 1.5)
				      (cons 'x 3)
				      (cons 'y 5.5))
				"$sh")
			  (list 'text
				(list (cons 'stroke "red")
				      (cons 'fill "red")
				      (cons 'text-anchor "middle")
				      (cons 'dominant-baseline "central")
				      (cons 'font-size 1.5)
				      (cons 'x 7)
				      (cons 'y 5.5))
				"$s2d")
			  (list 'text
				(list (cons 'stroke "black")
				      (cons 'fill "black")
				      (cons 'text-anchor "middle")
				      (cons 'font-size 1.5)
				      (cons 'x 5)
				      (cons 'y 5.5))
				"$s2h")))
		   ;;'decorations ;;dm-map-underlay-prop
		   )
	     dm-map-tiles)
    ;; (mapc
    ;;  (lambda (pos)
    ;;    (puthash pos (list dm-map-draw-prop (list 'bb-char-box))
    ;; 	      dm-map-level))
    ;;  (list (cons 0 0)
    ;; 	 (cons 9 0)
    ;; 	 (cons 9 9)))
    (dm-map-draw)
    ;;(print (list dm-map-level))
    ))

(provide 'dm-battleboard)
;;; dm-battleboard.el ends here
