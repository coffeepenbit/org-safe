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
(require 'ert)
(ert-delete-all-tests)

(if (featurep 'org-safe)
    (unload-feature 'org-safe t))
(require 'org-safe "./org-safe.el")

(ert-deftest test-delete-backward-char nil
  (should (string=
           "* headlin"
           (with-temp-buffer
             (insert "* headline")
             (org-mode)
             (goto-char (point-max))
             (org-safe-delete-backward-char)
             (buffer-string)))))

(ert-deftest test-delete-backward-char-prevent-one-asterisk nil
  (should (string=
           "* headline"
           (with-temp-buffer
             (insert
"* headline")
             (org-mode)
             (goto-char 2) ; After first asterisk
             (org-safe-delete-backward-char)
             (buffer-string)))))

(ert-deftest test-delete-backward-char-prevent-two-asterisks nil
  (should (string=
           "** headline"
           (with-temp-buffer
             (insert
"** headline")
             (org-mode)
             (goto-char 2) ; After first asterisk
             (org-safe-delete-backward-char)
             (buffer-string)))))

(ert-deftest test-delete-backward-char-allow-nonheadline-delete nil
  (should (string=
           "this is not a headline*"
           (with-temp-buffer
             (insert
"*this is not a headline*")
             (org-mode)
             (goto-char 2) ; After first asterisk
             (org-safe-delete-backward-char)
             (buffer-string))))
  (should (string=
           "*this is not a headline"
           (with-temp-buffer
             (insert
"*this is not a headline*")
             (org-mode)
             (goto-char (point-max)) ; After last asterisk
             (org-safe-delete-backward-char)
             (buffer-string)))))

(ert-deftest test-delete-backward-char-allow-nonheadline-delete-2 nil
  (should (string=
           "asterisk"
           (with-temp-buffer
             (insert
"asterisk*")
             (org-mode)
             (goto-char (point-max)) ; After first asterisk
             (org-safe-delete-backward-char)
             (buffer-string)))))


(ert-deftest test-point-on-headline-stars nil
  (should (equal
           t
           (with-temp-buffer
             (insert
"* headline")
             (org-mode)
             (goto-char 2) ; After first asterisk
             (org-safe-point-on-headline-stars-p))))
  (should (equal
           t
           (with-temp-buffer
             (insert
"
* headline")
             (org-mode)
             (goto-char (point-min)) ; At line above asterisk
             (org-safe-point-on-headline-stars-p)))))


(ert-deftest test-point-not-on-headline-stars nil
  (should (equal
           nil
           (with-temp-buffer
             (insert
"*headline")
             (org-mode)
             (goto-char 2) ; After first asterisk
             (org-safe-point-on-headline-stars-p))))
  (should (equal
           nil
           (with-temp-buffer
             (insert
" * headline")
             (org-mode)
             (goto-char 3) ; After first asterisk
             (org-safe-point-on-headline-stars-p))))
  (should (equal
           nil
           (with-temp-buffer
             (insert
"* headline*")
             (org-mode)
             (goto-char (point-max)) ; After first asterisk
             (org-safe-point-on-headline-stars-p)))))

(ert t)

(provide 'test-org-safe)
;;; test-org-safe.el ends here
