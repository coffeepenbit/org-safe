;;; org-safe.el --- For keeping org-mode content safe from butter-fingers  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  coffeepenbit

;; Author:  coffeepenbit@gmail.com
;; Keywords: outlines
;; Version: 0.0.1

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
;;;; Required packages
(require 'org)

;;;; Customization
(defgroup org-safe nil
  "org-safe minor mode."
  :group 'editing)


(defcustom org-safe-prohibited-duration 1
  "Disables org-safe protection for specified numebr of seconds."
  :group 'org-safe
  :type 'float)


;;;; Vars
(defvar org-safe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap org-delete-backward-char] 'org-safe-delete-backward-char)
    (define-key map [remap org-delete-char] 'org-safe-delete-char)
    map)
  "Keymap used for `org-safe-mode.'.")


(defvar org-safe--prohibited-var nil
  "If true, prevent function from activating.")


(define-minor-mode org-safe-mode
  "For keeping org-mode content safe from butter-fingers."
  :init-value nil
  :lighter " org-safe"
  :group 'org-safe
  :keymap org-safe-mode-map)


(defun org-safe-temp-allow-deletion nil
  "Prohibit `org-safe' protection."
  (interactive)
  (org-safe--prohibit)
  (org-safe-start-prohibited-timer))


(defun org-safe-start-prohibited-timer nil
  "Enable `org-safe' again after timer is done."
  (run-with-timer org-safe-prohibited-duration nil 'org-safe--enable))


(defun org-safe-prohibited-p nil
  "Check if `org-safe' protection is prohibited."
  org-safe--prohibited-var)


(defun org-safe--prohibit nil
  "Prohibit `org-safe' safety functions."
  (setq org-safe--prohibited-var t))


(defun org-safe--enable nil
  "Re-enables org safe after prohibited."
  (setq org-safe--prohibited-var nil))


(defun org-safe-delete-backward-char nil
  "Execute org-delete-backward-char if non-protected content.

N is number of chars to consider."
  (interactive)
  (if (not (org-safe-looking-back-at-headline-stars-p))
      (org-delete-backward-char 1)
    (message "Cant delete headline stars")))


(defun org-safe-looking-back-at-headline-stars-p nil
  "Return non-nil if point is looking back at headline stars."
  (and (looking-back "^\\*+" nil)
       ;; A space must follow the last headline star
       (looking-at "\\*? ")))


(defun org-safe-point-looking-at-headline-stars-p nil
  "Return non-nil if point is within or immediately after headline stars."
  (looking-at "\\(
\\)?\\*+\\ [^\\ ]?"))


(defun org-safe-delete-char nil
  "Execute org-delete-char if non-protected content."
  (interactive)
  (if (not (org-safe-looking-back-at-headline-stars-p))
      (org-delete-char 1)
    (message "Cant delete headline stars")))


(provide 'org-safe)
;;; org-safe.el ends here
