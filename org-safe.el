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

(defcustom org-safe-disabled-duration 3
  "Disables org-safe protection for specified number of seconds."
  :group 'org-safe
  :type 'float)

(defcustom org-safe-prohibit-delete-char-contexts
  '(;; Headline stars
    org-safe-looking-at-headline-stars-p

    ;; Headline spaces
    org-safe-looking-at-headline-star-space-p

    ;; Headline region
    org-safe-headline-in-region-p

    ;; Drawers
    org-safe-looking-at-drawer-p
    org-safe-looking-at-logbook-note-p
    org-safe-drawer-in-region-p

    ;; Document properties
    org-safe-looking-at-document-footer-properties-p
    org-safe-looking-at-document-header-properties-p
    org-safe-document-header-properties-in-region-p
    org-safe-document-footer-properties-in-region-p)
  "Functions that prevent deletion when returning non-nil."
  :group 'org-safe
  :type 'list)

(defcustom org-safe-prohibit-delete-backward-char-contexts
  '(;; Headline stars
    org-safe-looking-back-at-headline-stars-p

    ;; Headline spaces
    org-safe-looking-back-at-headline-star-space-p

    ;; Headline region
    org-safe-headline-in-region-p

    ;; TODO don't allow backward delete to delete end of tags
    ;; Headline tags
    ;; org-safe-looking-back-at-headline-tags-end

    ;; Drawers
    org-safe-looking-at-drawer-p
    org-safe-looking-at-logbook-note-p
    org-safe-drawer-in-region-p

    ;; Document properties
    org-safe-looking-at-document-footer-properties-p
    org-safe-looking-at-document-header-properties-p
    org-safe-document-header-properties-in-region-p
    org-safe-document-footer-properties-in-region-p)
  "Functions that prevent deletion when returning non-nil."
  :group 'org-safe
  :type 'list)

(defcustom org-safe-prohibit-self-insert-command-contexts
  '(;; Headline stars
    org-safe-looking-at-headline-stars-p
    org-safe-looking-back-at-headline-stars-p

    ;; Headline spaces
    org-safe-looking-at-headline-star-space-p

    ;; Headline region
    org-safe-headline-in-region-p

    ;; Drawers
    org-safe-looking-at-drawer-p
    org-safe-looking-at-logbook-note-p
    org-safe-drawer-in-region-p

    ;; Document properties
    org-safe-looking-at-document-footer-properties-p
    org-safe-looking-at-document-header-properties-p
    org-safe-document-header-properties-in-region-p
    org-safe-document-footer-properties-in-region-p)
  "Functions that prevent deletion when returning non-nil."
  :group 'org-safe
  :type 'list)

;;;; Vars
(defvar org-safe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap delete-backward-char] 'org-safe-delete-backward-char)
    (define-key map [remap delete-char] 'org-safe-delete-char)
    map)
  "Keymap used for `org-safe-mode'.")

(define-minor-mode org-safe-mode
  "For keeping org-mode content safe from butter-fingers.

`org-safe-mode-map' bindings:

\\{org-safe-mode-map}"
  :init-value nil
  :lighter " org-safe"
  :group 'org-safe
  :keymap 'org-safe-mode-map
  ;; NOTE Disabling hook until fix issues with self-insert-command advice
  ;; (if org-safe-mode ; `org-safe-mode' was just enabled
  ;;     (advice-add 'self-insert-command :before-until
  ;;                 #'org-safe-prohibit-self-insert-command-advice)
  ;;   ;; FIXME this removes advice even if org-safe is enabled in other buffers
  ;;   (advice-remove 'self-insert-command #'org-safe-prohibit-self-insert-command-advice))
  )

(defvar org-safe--disabled-var nil
  "If true, prevent function from activating.")

(defun org-safe-temp-allow-deletion nil
  "Prohibit `org-safe' protection."
  (interactive)
  (when (timerp org-safe--disabled-timer)
    (cancel-timer org-safe--disabled-timer))
  (org-safe-disable)
  (org-safe--start-disabled-timer))

(defvar org-safe--disabled-timer nil)

(defun org-safe--start-disabled-timer nil
  "Enable `org-safe' again after timer is done."
  (setq org-safe--disabled-timer (run-with-timer org-safe-disabled-duration
                                                 nil
                                                 'org-safe-enable)))

(defun org-safe-disabled-p nil
  "Check if `org-safe' protection is prohibited."
  org-safe--disabled-var)

(defun org-safe-disable nil
  "Disable `org-safe' prohibiting functionality."
  (setq org-safe--disabled-var t))

(defun org-safe-enable nil
  "Re-enables org safe after prohibited."
  (setq org-safe--disabled-var nil))

(defun org-safe-delete-backward-char nil
  "Execute org-delete-backward-char if non-protected content.

N is number of chars to consider."
  (interactive)
  (if (org-safe-disabled-p)
      (progn
        (org-delete-backward-char 1)
        (call-interactively 'org-safe-temp-allow-deletion))
    (progn
      (let ((prohibited-reason (org-safe-delete-backward-char-prohibited-context-p)))
        (if (not prohibited-reason)
            (org-delete-backward-char 1)
          (message "delete-backward-char prohibited [reason(s): %s]" prohibited-reason))))))

(defun org-safe-delete-char nil
  "Execute org-delete-char if non-protected content."
  (interactive)
  (if (org-safe-disabled-p) ; TODO reset disabled timer if running
      (org-delete-char 1)
    (progn
      (let ((prohibited-reason (org-safe-delete-char-prohibited-context-p)))
        (if (not prohibited-reason)
            (org-delete-char 1)
          (message "delete-char prohibited [reason(s): %s]" prohibited-reason))))))

(defun org-safe-looking-back-at-headline-stars-p nil
  "Return non-nil if point is looking back at headline stars."
  (and (eq (char-before) ?*)
       (org-at-heading-p)))

(defun org-safe-looking-at-headline-stars-p nil
  "Return non-nil if point is within or immediately after headline stars."
  (looking-at (concat "\\(
\\)?" ; Count org headlines and next line as well
                      org-outline-regexp)))

(defun org-safe-looking-at-headline-star-space-p nil
  "Return non-nil if headline in region."
  (and (looking-at " ")
       ;; TODO add limit to looking-back
       (looking-back "^\\*+ ?")
       (org-at-heading-p)))

(defun org-safe-looking-back-at-headline-star-space-p nil
  "Return non-nil if headline in region."
  ;; TODO add limit to looking-back
  (and (looking-back "^\\*+ +")
       (org-at-heading-p)))

(defun org-safe-self-insert-command-prohibited-context-p nil
  "Return non-nil if action should be prohibited."
  (mapcan (lambda (func)
            (when (and func
                       (funcall func))
              (list (symbol-name func))))
          org-safe-prohibit-self-insert-command-contexts))

(defun org-safe-delete-char-prohibited-context-p nil
  "Return non-nil if action should be prohibited."
  (mapcan (lambda (func)
            (when (and func
                       (funcall func))
              (list (symbol-name func))))
          org-safe-prohibit-delete-char-contexts))

(defun org-safe-delete-backward-char-prohibited-context-p nil
  "Return non-nil if action should be prohibited."
  (mapcan (lambda (func)
            (when (and func
                       (funcall func))
              (list (symbol-name func))))
          org-safe-prohibit-delete-backward-char-contexts))

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
        (looking-at org-safe-logbook-drawer-re)
        (org-safe-looking-at-logbook-note-p))))

(defun org-safe-looking-at-logbook-note-p nil
  "Return non-nil if point is in logbook note."
  (when (and (not (org-before-first-heading-p))
             (org-safe-looking-at-logbook-note-element-type))
    (save-excursion
      (catch 'looking-at-logbook-note-p
        (org-safe-dolines-some-p
         (point)
         (save-excursion
           (org-back-to-heading)
           (point))
         (lambda nil
           (beginning-of-line)
           (when (looking-at org-safe-logbook-drawer-re)
             (throw 'looking-at-logbook-note-p t))
           (unless (org-safe-looking-at-logbook-note-element-type)
             (throw 'looking-at-logbook-note-p nil))))))))

(defun org-safe-looking-at-logbook-note-element-type nil
  "Return non-nil if element at point is of a logbook note type."
  (member (car (org-element-at-point))
          '(paragraph list plain-list)))

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
  (org-safe-dolines-some-in-region-p
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
  (save-excursion
    (cl-some 'identity (org-safe-dolines beg end pred))))

(defun org-safe-dolines-some-in-region-p (pred)
  "Return non-nil if PRED is true on any line from mark to point."
  (when mark-active
    (org-safe-dolines-some-p (mark) (point) pred)))

(defun org-safe-headline-in-region-p nil
  "Return non-nil if headline in region."
  (org-safe-dolines-some-in-region-p
   (lambda nil
     (save-excursion
       (beginning-of-line)
       (org-safe-looking-at-headline-stars-p)))))

(defun org-safe-document-footer-properties-in-region-p nil
  "Return non-nil if headline in region."
  (org-safe-dolines-some-in-region-p
   (lambda nil
     (save-excursion
       (beginning-of-line)
       (org-safe-looking-at-document-header-properties-p)))))

(defun org-safe-drawer-in-region-p nil
  "Return non-nil if drawer in region."
  (org-safe-dolines-some-in-region-p
   (lambda nil
     (save-excursion
       (beginning-of-line)
       (org-safe-looking-at-drawer-p)))))

(defun org-safe-looking-at-document-footer-properties-p nil
  "Return non-nil if looking at document footer properties."
  (or (org-safe-looking-at-document-footer-properties-on-this-line-p)
      (when (not (eq (point) (point-max)))
        (save-excursion
          (forward-char)
          (org-safe-looking-at-document-footer-properties-on-this-line-p)))))

(defun org-safe-looking-at-document-footer-properties-on-this-line-p nil
  "Return non-nil if looking at document footer properties on current line."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat
                 "^[ ]*"
                 (regexp-quote "# Local variables:")))))

(defun org-safe-prohibit-self-insert-command-advice (N &optional C)
  "Return non-nil if `self-insert-command' should be prohibited.

Use this function by adding it as advice :before-until to `self-insert-command',
i.e. run `self-insert-command' only if this function returns nil.

See `self-insert-command' docs for N and C descriptions."
  ;; TODO have self-insert-command obey disabled timer
  (when (and (eq major-mode 'org-mode)
             org-safe-mode) ; Prevent running advice in non org-safe buffers
    (if (or (org-safe-attemping-insert-first-star-newline-p C)
            (not (org-safe-self-insert-command-prohibited-context-p)))
        nil ; Allow `self-insert-command'
      (progn ; Prevent `self-insert-command'
        ;; TODO provide reason to user why self-insert-command is prohibited
        ;; FIXME still not allowing newline insert if calling:
        ;; - org-open-line
        ;; - org-return
        (message "org-safe prohibiting self-insert-command")
        t))))

(defun org-safe-attemping-insert-first-star-newline-p (C)
  "Return non-nil if attempt to insert newline before first headline star.

See `self-insert-command' docs for C description."
  (and (org-safe-looking-at-first-headline-star-p)
       (eq C 10)))

(defun org-safe-looking-at-first-headline-star-p nil
  "Return non-nil if point is looking at first headline star."
  (and (org-at-heading-p)
       (or (eq (point) (point-min))
           (eq (char-before) 10))
       (eq (char-after) ?*)))

(provide 'org-safe)
;;; org-safe.el ends here
