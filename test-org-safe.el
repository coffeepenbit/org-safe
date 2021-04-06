;;; test-org-safe.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2021  coffeepenbit

;; Author: coffeepenbit@gmail.com
;; Keywords: outline

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
(require 'buttercup)
(require 'org-safe)

(defun org-temp-buffer (buffer-text &optional func)
  "Useful for testing `org-mode' functions.

BUFFER-TEXT is the initial state of the `org-mode' buffer.

FUNC is what is ran after creating the buffer."
  (with-temp-buffer
    (insert buffer-text)
    (goto-char (point-min))
    (org-mode)
    (if func
        (funcall func)
      (buffer-string))))

(xdescribe "org-safe-mode"
           (xit "remaps bindings")
           ;; TODO add tests for bindings
           )

(describe "org-safe-delete-char"
          (before-each (setq (inhibit-message t)))
          (it "deletes headline title chars"
              (expect "* hedline" :to-equal
                      (org-temp-buffer
                       "* headline"
                       (lambda nil
                         (org-safe-mode)
                         (goto-char 5)
                         (org-safe-delete-char)
                         (buffer-string))))))

(describe "org-safe-delete-backward-char"
          (before-each (setq inhibit-message t))
          (it "allows deletion of title characters"
              (expect "* hadline" :to-equal
                      (org-temp-buffer
                       "* headline"
                       (lambda nil
                         (org-safe-mode)
                         (goto-char 5)
                         (org-safe-delete-backward-char)
                         (buffer-string)))))
          (it "prohibits deleting headline asteriks"
              (expect "* headline" :to-equal
                      (org-temp-buffer
                       "* headline"
                       (lambda nil
                         (org-safe-mode)
                         (goto-char 2) ; After first asterisk
                         (org-safe-delete-backward-char)
                         (buffer-string))))
              (expect "** headline" :to-equal
                      (org-temp-buffer
                       "** headline"
                       (lambda nil
                         (org-safe-mode)
                         (goto-char 2) ; After first asterisk
                         (org-safe-delete-backward-char)
                         (buffer-string)))))
          (it "allows deletion of non-headline asterisks"
              (expect "this is not a headline*" :to-equal
                      (org-temp-buffer
                       "*this is not a headline*"
                       (lambda nil
                         (org-safe-mode)
                         (goto-char 2) ; After first asterisk
                         (org-safe-delete-backward-char)
                         (buffer-string))))
              (expect "*this is not a headline" :to-equal
                      (org-temp-buffer
                       "*this is not a headline*"
                       (lambda nil
                         (org-safe-mode)
                         (goto-char (point-max)) ; After last asterisk
                         (org-safe-delete-backward-char)
                         (buffer-string))))
              (expect "asterisk" :to-equal
                      (org-temp-buffer
                       "asterisk*"
                       (lambda nil
                         (org-safe-mode)
                         (goto-char (point-max)) ; After first asterisk
                         (org-safe-delete-backward-char)
                         (buffer-string))))))

(describe "org-safe-point-looking-at-headline-stars-p"
  (it "should be t when looking at a headline"
    (expect t :to-be
            (org-temp-buffer
             "* headline"
             (lambda nil
               (org-safe-mode)
               (org-safe-point-looking-at-headline-stars-p))))
    (expect t :to-be
            (org-temp-buffer
             "** headline"
             (lambda nil
               (org-safe-mode)
               (org-safe-point-looking-at-headline-stars-p))))
    (expect t :to-be
            (org-temp-buffer
             "** headline"
             (lambda nil
               (org-safe-mode)
               (goto-char 2)
               (org-safe-point-looking-at-headline-stars-p))))
    (expect t :to-be
            (org-temp-buffer
             "* headline*"
             (lambda nil
               (org-safe-mode)
               (org-safe-point-looking-at-headline-stars-p))))
    (expect t :to-be
            (org-temp-buffer
             "*    headline"
             (lambda nil
               (org-safe-mode)
               (org-safe-point-looking-at-headline-stars-p))))
    (expect t :to-be
            (org-temp-buffer
             "
* headline"
             (lambda nil
               (org-safe-mode)
               (org-safe-point-looking-at-headline-stars-p)))))
  (it "should be nil when looking at non-headlines"
    (expect nil :to-be
            (org-temp-buffer
             "*headline"
             (lambda nil
               (org-safe-mode)
               (org-safe-point-looking-at-headline-stars-p))))
    (expect nil :to-be
            (org-temp-buffer
             "*headline*"
             (lambda nil
               (org-safe-mode)
               (org-safe-point-looking-at-headline-stars-p))))))

(describe "org-safe-looking-back-at-headline-stars-p"
          (describe "when looking at headline stars"
                    (it "should be t when looking back at single headline star"
                        (expect t :to-be (org-temp-buffer
                                          "* headline"
                                          (lambda nil
                                            (org-safe-mode)
                                            (goto-char 2) ; After first asterisk
                                            (org-safe-looking-back-at-headline-stars-p)))))
                    (it "should be t when looking back at two headline stars"
                        (expect t :to-be (org-temp-buffer
                                          "** headline"
                                          (lambda nil
                                            (org-safe-mode)
                                            (goto-char 3) ; After first asterisk
                                            (org-safe-looking-back-at-headline-stars-p)))))
                    (it "should be t when between two headline stars"
                        (expect t :to-be (org-temp-buffer
                                          "** headline"
                                          (lambda nil
                                            (org-safe-mode)
                                            (goto-char 3) ; After first asterisk
                                            (org-safe-looking-back-at-headline-stars-p)))))
                    (it "should be t when looking at stars without titles"
                        ;; org-mode considers these titles still
                        (expect t :to-be (org-temp-buffer
                                          "* " ; Title
                                          (lambda nil
                                            (org-safe-mode)
                                            (goto-char 2) ; After first asterisk
                                            (org-safe-looking-back-at-headline-stars-p))))))
          (describe "when NOT looking back at headline stars"
                    (it "should be nil when asterisk belongs to bold phrase"
                        (expect nil :to-be (org-temp-buffer
                                            "*headline*"
                                            (lambda nil
                                              (org-safe-mode)
                                              (goto-char 2) ; After first asterisk
                                              (org-safe-looking-back-at-headline-stars-p)))))))

(describe "org-safe-prohibited-p"
  (it "should be t when org-safe--prohibited-var t"
    (expect t :to-equal
            (org-temp-buffer
             "
* headline"
             (lambda nil
               (org-safe-mode)
               (let ((org-safe--prohibited-var t))
                 (org-safe-prohibited-p))))))
  (it "should be nil when org-safe--prohibited-var nil"
    (expect nil :to-be
            (org-temp-buffer
             "
  * headline"
             (lambda nil
               (org-safe-mode)
               (let ((org-safe--prohibited-var nil))
                 (org-safe-prohibited-p)))))))

(describe "org-safe--prohibit"
  (it "should cause prohibited-p to be t after being nil"
    (org-temp-buffer
     "
  * headline"
     (lambda nil
       (org-safe-mode)
       (let ((org-safe--prohibited-var nil))
         (expect nil :to-be (org-safe-prohibited-p)))
       (org-safe--prohibit)
       (expect t :to-be (org-safe-prohibited-p))))))

(describe "org-safe--enable"
  (it "should cause prohibited-p to be nil after being t"
    (org-temp-buffer
     "
* headline"
     (lambda nil
       (org-safe-mode)
       (let ((org-safe--prohibited-var t))
         (expect t :to-be (org-safe-prohibited-p)))
       (org-safe--enable)
       (expect nil :to-be (org-safe-prohibited-p))))))

(describe "org-safe-disabled-timer"
  (it "prohibited-nil-to-t-to-nil"
    (org-temp-buffer
     "
* headline"
     (lambda nil
       (org-safe-mode)
       (let ((org-safe-prohibited-duration 0.1))
         ;; Start prohibited
         (org-safe--prohibit)
         ;; Verify that we are starting prohibited
         (expect t :to-be( org-safe-prohibited-p))
         ;; Run prohibited timer and wait for it to finish
         (org-safe-start-prohibited-timer)
         (sit-for (+ org-safe-prohibited-duration 0.01))
         ;; Verify that `org-safe' is re-enabled
         (expect nil :to-be (org-safe-prohibited-p)))))))

(describe "org-safe-temp-allow-deletion"
          :var ((org-safe-prohibited-duration 0.1)
                (org-safe-prohibited nil))
          (org-temp-buffer
           "
 * headline"
           (lambda nil
             (let ((org-safe-prohibited-duration 0.1)
                   (org-safe-prohibited-p nil))
               (org-safe-mode)
               (it "switches org-safe from enabled to prohibited"
                   (call-interactively 'org-safe-temp-allow-deletion)
                   (expect t :to-be (org-safe-prohibited-p)))
               (it "re-enables org-safe after org-safe-prohibited-duration"
                   (sit-for (+ org-safe-prohibited-duration 0.01))
                   (expect nil :to-be (org-safe-prohibited-p)))))))

(provide 'test-org-safe)
;;; test-org-safe.el ends here
