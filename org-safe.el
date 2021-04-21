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
(require 'cl-lib)
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
    ;; FIXME: make this remap instead of keybinding
    (define-key map (kbd "DEL") 'org-safe-delete-backward-char)
    (define-key map (kbd "C-d") 'org-safe-delete-char)
    map)
  "Keymap used for `org-safe-mode'.")

(define-minor-mode org-safe-mode
  "For keeping org-mode content safe from butter-fingers."
  :init-value nil
  :lighter " org-safe"
  :group 'org-safe
  :keymap 'org-safe-mode-map)

(defvar org-safe-prohibited-var nil
  "If true, prevent function from activating.")

(defun org-safe-temp-allow-deletion nil
  "Prohibit `org-safe' protection."
  (interactive)
  (org-safe-prohibit)
  (org-safe-start-prohibited-timer))

(defun org-safe-start-prohibited-timer nil
  "Enable `org-safe' again after timer is done."
  (run-with-timer org-safe-prohibited-duration nil 'org-safe-enable))

(defun org-safe-prohibited-p nil
  "Check if `org-safe' protection is prohibited."
  org-safe-prohibited-var)

(defun org-safe-prohibit nil
  "Prohibit `org-safe' safety functions."
  (setq org-safe-prohibited-var t))

(defun org-safe-enable nil
  "Re-enables org safe after prohibited."
  (setq org-safe-prohibited-var nil))

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

(defun org-safe-looking-at-headline-stars-p nil
  "Return non-nil if point is within or immediately after headline stars."
  (looking-at (concat "\\(
\\)?" ; Count org headlines and next line as well
                      org-outline-regexp)))

(defun org-safe-delete-char nil
  "Execute org-delete-char if non-protected content."
  (interactive)
  (if (not (org-safe-looking-at-headline-stars-p))
      (org-delete-char 1)
    (message "Can't delete headline stars")))

(defconst org-safe-logbook-drawer-re
  ;; NOTE: This constant is defined in `org' 9.4. Defining it here
  ;; to remove dependency on `org' 9.4
  (rx (seq bol (0+ (any "\t ")) ":LOGBOOK:" (0+ (any "\t ")) "\n"
	       (*? (0+ nonl) "\n")
	       (0+ (any "\t ")) ":END:" (0+ (any "\t ")) eol))
  "Matches an entire LOGBOOK drawer.")

(defconst org-comment-regexp
  ;; NOTE: This constant is defined in `org' 9.4. Defining it here
  ;; to remove dependency on `org' 9.4
  (rx (seq bol (zero-or-more (any "\t ")) "#" (or " " eol)))
  "Regular expression for comment lines.")

(defun org-safe-looking-at-drawer-p nil
  "Return non-nil if point is looking at drawer."
  (or (org-safe-drawer-on-this-line-p)
      (when (not (eq (point) (point-max)))
        (save-excursion
          (forward-char)
          (org-safe-drawer-on-this-line-p)))))

(defun org-safe-drawer-on-this-line-p nil
  "Return non-nil if point is looking at drawer on current line."
  ;; NOTE: `looking-at' is faster than `face-at-point'
  (save-excursion
    (beginning-of-line)
    (or (looking-at org-drawer-regexp)
        (looking-at org-property-drawer-re)
        (looking-at org-property-re)
        (looking-at org-safe-logbook-drawer-re))))

(defun org-safe-looking-back-at-drawer-p nil
  "Return non-nil if point is looking back at drawer."
  (save-excursion
    (beginning-of-line)
    (or (org-safe-drawer-on-this-line-p)
        (condition-case nil
            (progn
              (backward-char)
              (beginning-of-line)
              (org-safe-drawer-on-this-line-p))
          (error nil)))))

(defun org-safe-looking-at-document-header-properties-p nil
  "Return non-nil if looking at org docuent header properties."
  (save-excursion
    (beginning-of-line)
    (looking-at "#\[^ ]*:\ ")))

(defun org-safe-looking-back-at-document-header-properties-p nil
  "Return non-nil if looking back at org document header properties."
  (or (org-safe-looking-at-document-header-properties-p)
      (save-excursion
        (backward-char)
        (org-safe-looking-at-document-header-properties-p))))

(defun org-safe-document-header-properties-in-region-p nil
  "Return non-nil if document header properties in region."
  (org-safe-dolines-some-p (mark)
                           (point)
                           'org-safe-looking-at-document-header-properties-p))

(defun org-safe-dolines (beg end &optional func exit-condition)
  "Loop over lines from line at belonging to BEG to END in buffer.

BEG and END are points.

FUNC is ran on each line and its results are collected.

Exits from loop if EXIT-CONDITION is satisfied."
  (when (< beg (point-min))
    (error "BEG is less-than point-min"))
  (when (> end (point-max))
    (error "END is greater-than point-max"))
  (let ((beg-line-num (line-number-at-pos beg))
        (end-line-num (line-number-at-pos end)))
    (if (eq beg-line-num end-line-num)
        (if (not (null func))
            (list (funcall func))
          (list nil))
      (let ((direction (if (> beg end) -1 1))
            (nlines-to-loop (abs (- end-line-num beg-line-num)))
            (return-value nil))
        (save-excursion
          (goto-char beg)
          (when (not (null func))
            (push (funcall func) return-value))
          (catch 'loop-exit
            (dotimes (_ nlines-to-loop)
              (forward-line direction)
              (when (not (null func))
                (push (funcall func) return-value))
              (when (and (not (null exit-condition))
                         (funcall exit-condition))
                (throw 'loop-exit t)))
            (nreverse return-value)))))))

(defun org-safe-dolines-some-p (beg end pred)
  "Return non-nil if PRED is true on any line from BEG to END.

BEG and END are points."
  (when mark-active
    (save-excursion
      (cl-some 'identity (org-safe-dolines beg end pred)))))

(defun org-safe-headline-in-region-p nil
  "Return non-nil if headline in region."
  (org-safe-dolines-some-p (mark)
                           (point)
                           (lambda nil
                             (save-excursion
                               (beginning-of-line)
                               (org-safe-looking-at-headline-stars-p)))))

(defun org-safe-drawer-in-region-p nil
  "Return non-nil if drawer in region."
  (org-safe-dolines-some-p (mark)
                           (point)
                           (lambda nil
                             (save-excursion
                               (beginning-of-line)
                               (org-safe-looking-at-drawer-p)))))

(defun org-safe-looking-at-document-footer-properties-p nil
  "Return non-nil if looking at document footer properties."
  (looking-at (concat
               "\(
\)?"
               "^[ ]*"
               (regexp-quote "# Local variables:"))))

(defun org-safe-looking-at-document-footer-properties-on-this-line-p nil
  ""
  (looking-at (concat
               "^[ ]*"
               (regexp-quote "# Local variables:"))))

(provide 'org-safe)
;;; org-safe.el ends here
