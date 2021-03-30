;;; org-safe.el --- For keeping org-mode content safe from butter-fingers  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  coffeepenbit

;; Author:  coffeepenbit@gmail.com
;; Keywords: outlines

;;; Commentary:

;; For keeping org-mode content safe from butter-fingers

;;; License:

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

;;; Code:


(defgroup org-safe ()
  "org-safe minor mode."
  :group 'editing)


(defvar org-safe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap org-delete-backward-char] 'org-safe-delete-backward-char)
    (define-key map [remap org-delete-char] 'org-safe-delete-char)
    map)
  "Keymap used for `org-safe-mode.'.")


(define-minor-mode org-safe-mode
  "For keeping org-mode content safe from butter-fingers."
  :init-value nil
  :lighter " org-safe"
  :group 'org-safe
  :keymap org-safe-mode-map)


(defun org-safe-delete-backward-char nil
  "Execute org-delete-backward-char if non-protected content.

N is number of chars to consider."
  (if (not (org-safe-point-on-headline-stars-p))
      (org-delete-backward-char 1)
    (message "Cant delete headline stars")))


(defun org-safe-point-on-headline-stars-p nil
  "Return non-nil if point is within or immediately after headline stars."
  (or (and (looking-back "^[*]+")
           (looking-at "[*]? "))
      (looking-at "
[*]? ")))


(defun org-safe-delete-char (N)
  "Execute org-delete-char if non-protected content.

N is number of chars to consider."
  (interactive "p")
  (org-delete-char N))


(provide 'org-safe)
;;; org-safe.el ends here
