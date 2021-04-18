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

;; TODO replace "expect (null ..." with "expect * :to-be nil"

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

;; TODO add tests for bindings
(xdescribe "org-safe-mode"
  (xit "remaps bindings"))

(describe "org-safe-delete-char"
  (before-each (setq inhibit-message t))
  (it "deletes title chars in headline"
    (org-temp-buffer
     "* headline"
     (lambda nil
       (org-safe-mode)
       (goto-char 5)
       (org-safe-delete-char)
       (expect (buffer-string) :to-equal "* hedline"))))
  (it "prohibits deleting headline asterisks"
    (org-temp-buffer
     "* headline"
     (lambda nil
       (org-safe-mode)
       (org-safe-delete-char)
       (expect (buffer-string) :to-equal "* headline")))
    (org-temp-buffer
     "** headline" ; Point after first asterisk
     (lambda nil
       (org-safe-mode)
       (forward-char 1)
       (org-safe-delete-char)
       (expect (buffer-string) :to-equal "** headline"))))
  (it "prohibits deleting linebreak in front of headline asterisks"
    (org-temp-buffer
     ;; Point at end of first line
     "this is some test
** headline on next line"
     (lambda nil
       (org-safe-mode)
       (end-of-line)
       (org-safe-delete-char)
       (expect (buffer-string) :to-equal "this is some test
** headline on next line"))))
  (it "deletes non-headline asterisks"
    (org-temp-buffer
     "*this is not a headline*"
     (lambda nil
       (org-safe-mode) ; After first asterisk
       (org-safe-delete-char)
       (expect (buffer-string) :to-equal "this is not a headline*")))
    (org-temp-buffer
     "*this is not a headline*"
     (lambda nil
       (org-safe-mode)
       (goto-char (- (point-max) 1)) ; After last asterisk
       (org-safe-delete-char)
       (expect (buffer-string) :to-equal "*this is not a headline")))
    (org-temp-buffer
     "asterisk*"
     (lambda nil
       (org-safe-mode)
       (goto-char (- (point-max) 1)) ; After first asterisk
       (org-safe-delete-char)
       (expect (buffer-string) :to-equal "asterisk"))))
  (xit "does NOT delete property drawer")
  (xit "does NOT delete logbook drawer"))

(describe "org-safe-delete-backward-char"
  (before-each (setq inhibit-message t))
  (it "deletes title chars in headline"
    (org-temp-buffer
     "* headline"
     (lambda nil
       (org-safe-mode)
       (goto-char 5)
       (org-safe-delete-backward-char)
       (expect (buffer-string) :to-equal "* hadline"))))
  (it "prohibits deleting headline asterisks"
    (org-temp-buffer
     "* headline"
     (lambda nil
       (org-safe-mode)
       (goto-char 2) ; After first asterisk
       (org-safe-delete-backward-char)
       (expect (buffer-string) :to-equal "* headline")))
    (org-temp-buffer
     "** headline"
     (lambda nil
       (org-safe-mode)
       (goto-char 2) ; After first asterisk
       (org-safe-delete-backward-char)
       (expect "** headline" :to-equal (buffer-string)))))
  (it "allows deletion of non-headline asterisks"
    (org-temp-buffer
     "*this is not a headline*"
     (lambda nil
       (org-safe-mode)
       (goto-char 2) ; After first asterisk
       (org-safe-delete-backward-char)
       (expect (buffer-string) :to-equal "this is not a headline*")))
    (org-temp-buffer
     "*this is not a headline*"
     (lambda nil
       (org-safe-mode)
       (goto-char (point-max))
       (org-safe-delete-backward-char)
       (expect (buffer-string) :to-equal "*this is not a headline")))
    (org-temp-buffer
     "asterisk*"
     (lambda nil
       (org-safe-mode)
       (goto-char (point-max))
       (org-safe-delete-backward-char)
       (expect (buffer-string) :to-equal "asterisk"))))
  (xit "does NOT delete property drawer")
  (xit "does NOT delete logbook drawer"))

(describe "org-safe-looking-at-headline-stars-p"
  (it "should be t when looking at a headline"
    (org-temp-buffer
     "* headline"
     (lambda nil
       (org-safe-mode)
       (expect (org-safe-looking-at-headline-stars-p) :to-be t)))
    (org-temp-buffer
     "** headline"
     (lambda nil
       (org-safe-mode)
       (expect (org-safe-looking-at-headline-stars-p) :to-be t)))
    (org-temp-buffer
     "** headline"
     (lambda nil
       (org-safe-mode)
       (goto-char 2)
       (expect (org-safe-looking-at-headline-stars-p) :to-be t)))
    (org-temp-buffer
     "* headline*"
     (lambda nil
       (org-safe-mode)
       (expect (org-safe-looking-at-headline-stars-p) :to-be t)))
    (org-temp-buffer
     "*    headline"
     (lambda nil
       (org-safe-mode)
       (expect (org-safe-looking-at-headline-stars-p) :to-be t)))
    (org-temp-buffer
     "
* headline"
     (lambda nil
       (org-safe-mode)
       (expect (org-safe-looking-at-headline-stars-p)) :to-be t)))
  (it "should be nil when looking at non-headlines"
    (org-temp-buffer
     "*headline"
     (lambda nil
       (org-safe-mode)
       (expect (null (org-safe-looking-at-headline-stars-p)))))
    (org-temp-buffer
     "*headline*"
     (lambda nil
       (org-safe-mode)
       (expect (null (org-safe-looking-at-headline-stars-p)))))))

(describe "org-safe-looking-back-at-headline-stars-p"
  (describe "when looking at headline stars"
    (it "should be t when looking back at single headline star"
      (org-temp-buffer
       "* headline"
       (lambda nil
         (org-safe-mode)
         (goto-char 2) ; After first asterisk
         (expect (org-safe-looking-back-at-headline-stars-p)))))
    (it "should be t when looking back at two headline stars"
      (org-temp-buffer
       "** headline"
       (lambda nil
         (org-safe-mode)
         (goto-char 3) ; After first asterisk
         (expect (org-safe-looking-back-at-headline-stars-p)))))
    (it "should be t when between two headline stars"
      (org-temp-buffer
       "** headline"
       (lambda nil
         (org-safe-mode)
         (goto-char 3) ; After first asterisk
         (expect (org-safe-looking-back-at-headline-stars-p)))))
    (it "should be t when looking at stars without titles"
      ;; org-mode considers these titles still
      "* " ; Title
      (lambda nil
        (org-safe-mode)
        (goto-char 2) ; After first asterisk
        (expect (org-safe-looking-back-at-headline-stars-p))))))

(describe "when NOT looking back at headline stars"
  (it "should be nil when asterisk belongs to bold phrase"
    (org-temp-buffer
     "*headline*"
     (lambda nil
       (org-safe-mode)
       (goto-char 2) ; After first asterisk
       (expect (null (org-safe-looking-back-at-headline-stars-p)))))))

(describe "org-safe-prohibited-p"
  (it "should be t when org-safe-prohibited-var t"
    (let ((org-safe-prohibited-var t))
      (expect (org-safe-prohibited-p))))
  (it "should be nil when org-safe-prohibited-var nil"
    (let ((org-safe-prohibited-var nil))
      (expect (null (org-safe-prohibited-p))))))

(describe "org-safe-prohibit"
  (it "should cause prohibited-p to be t after being nil"
    (let ((org-safe-prohibited-var nil))
      (expect (null (org-safe-prohibited-p)))
      (org-safe-prohibit)
      (expect (org-safe-prohibited-p)))))

(describe "org-safe-enable"
  (it "should cause prohibited-p to be nil after being t"
    (let ((org-safe-prohibited-var t))
      (expect (org-safe-prohibited-p)))
    (org-safe-enable)
    (expect (null (org-safe-prohibited-p)))))

(describe "org-safe-disabled-timer"
  :var ((org-safe-prohibited-duration 0.1))
  (it "re-enables org-safe after prohibited duration passes"
    ;; Start in prohibited state
    (org-safe-prohibit)
    (expect (org-safe-prohibited-p))
    ;; Run prohibited timer and wait for it to finish
    (org-safe-start-prohibited-timer)
    (sit-for (+ org-safe-prohibited-duration 0.01))
    ;; Verify that `org-safe' is re-enabled
    (expect (null (org-safe-prohibited-p)))))

(describe "org-safe-temp-allow-deletion"
  :var ((org-safe-prohibited-duration 0.1)
        (org-safe-prohibited nil))
  (it "switches org-safe from enabled to prohibited"
    (call-interactively 'org-safe-temp-allow-deletion)
    (expect (org-safe-prohibited-p)))
  (it "re-enables org-safe after org-safe-prohibited-duration"
    (sit-for (+ org-safe-prohibited-duration 0.01))
    (expect (null (org-safe-prohibited-p)))))

(describe "org-safe-looking-at-drawer-p"
  (it "returns non-nil when looking at :PROPERTIES:"
    (org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:"
     (lambda nil
       (org-safe-mode)
       (forward-line)
       (expect (looking-at (regexp-quote ":PROPERTIES:")))
       (expect (org-safe-looking-at-drawer-p)))))
  (it "returns non-nil when looking at a property key"
    (org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:"
     (lambda nil
       (org-safe-mode)
       (forward-line 2)
       (expect (looking-at (regexp-quote ":foo:")))
       (expect (org-safe-looking-at-drawer-p)))))
  (it "returns non-nil when looking at a property value"
    (org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:"
     (lambda nil
       (org-safe-mode)
       (forward-line 2)
       (forward-char 6)
       (expect (looking-at (regexp-quote "bar")))
       (expect (org-safe-looking-at-drawer-p)))))
  (it "returns non-nil when drawer is on next line"
    (org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:"
     (lambda nil
       (org-safe-mode)
       (end-of-line)
       (expect (org-safe-looking-at-drawer-p)))))
  (it "returns nil when drawer is NOT on next line"
    (org-temp-buffer
     "* headline
foo bar"
     (lambda nil
       (org-safe-mode)
       (end-of-line)
       (expect (regexp-quote "\n"))
       (expect (org-safe-looking-at-drawer-p) :to-be nil))))
  (it "returns non-nil when looking at :something:"
    (org-temp-buffer
     ":something:" ; org-mode considers this to be a drawer
     (lambda nil
       (org-safe-mode)
       (expect (looking-at (regexp-quote ":something:")))
       (expect (org-safe-looking-at-drawer-p)))))
  (it "returns non-nil when looking at :END:"
    (org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:"
     (lambda nil
       (org-safe-mode)
       (forward-line 3)
       (expect (looking-at (regexp-quote ":END:")))
       (expect (org-safe-looking-at-drawer-p)))))
  (it "returns non-nil when looking at :LOGBOOK:"
    (org-temp-buffer
     "* headline
:LOGBOOK:
- Note taken on [2021-04-14 Wed 07:53] \\
  foobar eggs and spam
:END:"
     (lambda nil
       (org-safe-mode)
       (forward-line)
       (expect (looking-at (regexp-quote ":LOGBOOK:")))
       (expect (org-safe-looking-at-drawer-p)))))
  (it "returns nil when NOT looking at a drawer"
    (org-temp-buffer
     ""
     (lambda nil
       (org-safe-mode)
       (expect (org-safe-looking-at-drawer-p) :to-be nil)))
    (org-temp-buffer
     "* headline"
     (lambda nil
       (org-safe-mode)
       (expect (org-safe-looking-at-drawer-p) :to-be nil)))))

(describe "org-safe-looking-back-at-drawer-p"
  (it "returns non-nil when looking back at :PROPERTIES:"
    (org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:"
     (lambda nil
       (forward-line)
       (end-of-line)
       (expect (org-safe-looking-back-at-drawer-p)))))
  (it "returns non-nil when looking back at :END:"
    (org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:"
     (lambda nil
       (forward-line 3)
       (end-of-line)
       (expect (org-safe-looking-back-at-drawer-p)))))
  (it "returns non-nil when looking back at :LOGBOOK:"
    (org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:"
     (lambda nil
       (forward-line 3)
       (end-of-line)
       (expect (org-safe-looking-back-at-drawer-p)))))
  (it "returns nil when looking back at nothing"
    (org-temp-buffer
     ""
     (lambda nil
       (forward-line 3)
       (end-of-line)
       (expect (org-safe-looking-back-at-drawer-p) :to-be nil))))
  (it "returns non-nil when looking back at drawer on prevoius line"
    (org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:
"
     (lambda nil
       (goto-char (point-max))
       (expect (org-safe-looking-back-at-drawer-p)))))
  (it "returns nil when looking back at non-drawer"
    (org-temp-buffer
     "* headline
okay
"
     (lambda nil
       (goto-char (point-max))
       (expect (org-safe-looking-back-at-drawer-p) :to-be nil)))))

(xdescribe "org-safe-looking-at-logbook")
(xdescribe "org-safe-looking-back-at-logbook")

(provide 'test-org-safe)
;;; test-org-safe.el ends here
