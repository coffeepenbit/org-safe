;;; test-org-safe.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2021  coffeepenbit

;; Author: coffeepenbit@gmail.com
;; Keywords:

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

;; Test cases for `org-safe-mode'

;;; Code:
(require 'org-safe)

(require 'ert)
(require 'my-ert "~/.emacs.d/site-lisp/my-ert.el")


(ert-delete-all-tests)

(my-ert-reload-feature 'org-safe)


;;;; remapped-functions
;; (ert-deftest test-delete-backward-char/dont-prevent-title-delete-end nil
;;   (should (string=
;;            "* headlin"
;;            (my-ert-org-buffer
;;             "* headline"
;;             (lambda nil
;;               (org-safe-mode)
;;               (goto-char (point-max))
;;               (org-safe-delete-backward-char)
;;               (buffer-string))))))


;;;; delete-char-forwards
(ert-deftest test-delete-char/dont-prevent-title-delete-end nil
  (should (equal
           "* hedline"
           (my-ert-org-buffer
            "* headline"
            (lambda nil
              (org-safe-mode)
              (goto-char 5)
              (org-safe-delete-char)
              (buffer-string))))))


(ert-deftest test-delete-char-prevent-one-asterisk nil
  (should (string=
           "* headline"
           (my-ert-org-buffer
            "* headline"
            (lambda nil
              (org-safe-mode)
              (org-safe-delete-char)
              (buffer-string))))))


;; (ert-deftest test-delete-char-prevent-two-asterisks nil
;;   (should (string=
;;            "** headline"
;;            (my-ert-org-buffer
;;             "** headline"
;;             (lambda nil
;;               (org-safe-mode)
;;               (goto-char 2) ; After first asterisk
;;               (org-safe-delete-char)
;;               (buffer-string))))))


;; (ert-deftest test-delete-char-allow-nonheadline-delete nil
;;   (should (string=
;;            "this is not a headline*"
;;            (my-ert-org-buffer
;;             "*this is not a headline*"
;;             (lambda nil
;;               (org-safe-mode)
;;               (goto-char 2) ; After first asterisk
;;               (org-safe-delete-char)
;;               (buffer-string)))))

;;   (should (string=
;;            "*this is not a headline"
;;            (my-ert-org-buffer
;;             "*this is not a headline*"
;;             (lambda nil
;;               (org-safe-mode)
;;               (goto-char (point-max)) ; After last asterisk
;;               (org-safe-delete-char)
;;               (buffer-string))))))


;; (ert-deftest test-delete-char-allow-nonheadline-delete-2 nil
;;   (should (string=
;;            "asterisk"
;;            (my-ert-org-buffer
;;             "asterisk*"
;;             (lambda nil
;;               (org-safe-mode)
;;               (goto-char (point-max)) ; After first asterisk
;;               (org-safe-delete-char)
;;               (buffer-string))))))


;;;; delete-char-backwards
(ert-deftest test-delete-backward-char/dont-prevent-title-delete-end nil
  (should (equal
           "* hadline"
           (my-ert-org-buffer
            "* headline"
            (lambda nil
              (org-safe-mode)
              (goto-char 5)
              (org-safe-delete-backward-char)
              (buffer-string))))))


(ert-deftest test-delete-backward-char-prevent-one-asterisk nil
  (should (string=
           "* headline"
           (my-ert-org-buffer
            "* headline"
            (lambda nil
              (org-safe-mode)
              (goto-char 2) ; After first asterisk
              (org-safe-delete-backward-char)
              (buffer-string))))))


(ert-deftest test-delete-backward-char-prevent-two-asterisks nil
  (should (string=
           "** headline"
           (my-ert-org-buffer
            "** headline"
            (lambda nil
              (org-safe-mode)
              (goto-char 2) ; After first asterisk
              (org-safe-delete-backward-char)
              (buffer-string))))))


(ert-deftest test-delete-backward-char-allow-nonheadline-delete nil
  (should (string=
           "this is not a headline*"
           (my-ert-org-buffer
            "*this is not a headline*"
            (lambda nil
              (org-safe-mode)
              (goto-char 2) ; After first asterisk
              (org-safe-delete-backward-char)
              (buffer-string)))))

  (should (string=
           "*this is not a headline"
           (my-ert-org-buffer
            "*this is not a headline*"
            (lambda nil
              (org-safe-mode)
              (goto-char (point-max)) ; After last asterisk
              (org-safe-delete-backward-char)
              (buffer-string))))))


(ert-deftest test-delete-backward-char-allow-nonheadline-delete-2 nil
  (should (string=
           "asterisk"
           (my-ert-org-buffer
            "asterisk*"
            (lambda nil
              (org-safe-mode)
              (goto-char (point-max)) ; After first asterisk
              (org-safe-delete-backward-char)
              (buffer-string))))))


;;;; point-on-headline-stars
(ert-deftest test-point-on-headline-stars nil
  (should (equal
           t
           (my-ert-org-buffer
            "* headline"
            (lambda nil
              (org-safe-mode)
              (goto-char 2) ; After first asterisk
              (org-safe-point-on-headline-stars-p)))))

  (should (equal
           t
           (my-ert-org-buffer
            "
* headline"
            (lambda nil
              (org-safe-mode)
              (goto-char (point-min)) ; At line above asterisk
              (org-safe-point-on-headline-stars-p))))))


(ert-deftest test-point-not-on-headline-stars nil
  (should (equal
           nil
           (my-ert-org-buffer
            "*headline"
            (lambda nil
              (org-safe-mode)
              (goto-char 2) ; After first asterisk
              (org-safe-point-on-headline-stars-p)))))

  (should (equal
           nil
           (my-ert-org-buffer
            " * headline"
            (lambda nil
              (org-safe-mode)
              (goto-char 3) ; After first asterisk
              (org-safe-point-on-headline-stars-p)))))

  (should (equal
           nil
           (my-ert-org-buffer
            "* headline*"
            (lambda nil
              (org-safe-mode)
              (goto-char (point-max)) ; After first asterisk
              (org-safe-point-on-headline-stars-p))))))


;;;; prohibited-p
(ert-deftest test-org-safe-prohibited-p/not-prohibited nil
  :tags '(org-safe-prohibited-p)
  (should (equal
           nil
           (my-ert-org-buffer
            "
* headline"
            (lambda nil
              (org-safe-mode)
              (let ((org-safe--prohibited-var nil))
                (org-safe-prohibited-p)))))))


(ert-deftest test-org-safe-prohibited-p/is-prohibited nil
  :tags '(org-safe-prohibited-p)
  (should (equal
           t
           (my-ert-org-buffer
            "
* headline"
            (lambda nil
              (org-safe-mode)
              (let ((org-safe--prohibited-var t))
                (org-safe-prohibited-p)))))))


;;;; set prohibited state
(ert-deftest test-org-safe--prohibit/make-prohibited nil
  ;; :tags '(org-safe-prohibited-p)
  (my-ert-org-buffer
   "
* headline"
   (lambda nil
     (org-safe-mode)
     (let ((org-safe--prohibited-var nil))
       (should (equal nil
                      (org-safe-prohibited-p)))
       (org-safe--prohibit)
       (should (equal t
                      (org-safe-prohibited-p)))))))


(ert-deftest test-org-safe--prohibit/make-un-prohibited nil
  :tags '(org-safe-prohibited-p)
  (my-ert-org-buffer
   "
* headline"
   (lambda nil
     (org-safe-mode)
     (let ((org-safe--prohibited-var t))
       (should (equal t
                      (org-safe-prohibited-p)))
       (org-safe--enable)
       (should (equal nil
                      (org-safe-prohibited-p)))
       ))))


;;;; prohibited-timer
(ert-deftest test-org-safe-disabled-timer/prohibited-nil-to-t-to-nil nil
  "Verify that timer is un-prohibiting after given time."
  :tags '(disabled-timer)
  (my-ert-org-buffer
   "
* headline"
   (lambda nil
     (org-safe-mode)
     (let ((org-safe-prohibited-duration 0.1))
       ;; Start prohibited
       (org-safe--prohibit)

       ;; Verify that we are starting prohibited
       (should (equal t (org-safe-prohibited-p)))

       ;; Run prohibited timer and wait for it to finish
       (org-safe-start-prohibited-timer)
       (sit-for (+ org-safe-prohibited-duration 0.01))

       ;; Verify that `org-safe' is re-enabled
       (should
        (equal nil (org-safe-prohibited-p)))))))


;;;; End of tests
(ert t)

(provide 'test-org-safe)
;;; test-org-safe.el ends here
