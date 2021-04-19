;;; test-org-safe.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2021  coffeepenbit

;; Author: coffeepenbit@gmail.com
;; Version: 0.0.1
;; Keywords: outline
;; Package-requires: ((org "9.4.4"))

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
       (expect (org-safe-looking-at-headline-stars-p) :to-be nil)))
    (org-temp-buffer
     "*headline*"
     (lambda nil
       (org-safe-mode)
       (expect (org-safe-looking-at-headline-stars-p) :to-be nil)))))

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
       (expect (org-safe-looking-back-at-headline-stars-p) :to-be nil)))))

(describe "org-safe-prohibited-p"
  (it "should be t when org-safe-prohibited-var t"
    (let ((org-safe-prohibited-var t))
      (expect (org-safe-prohibited-p))))
  (it "should be nil when org-safe-prohibited-var nil"
    (let ((org-safe-prohibited-var nil))
      (expect (org-safe-prohibited-p) :to-be nil))))

(describe "org-safe-prohibit"
  (it "should cause prohibited-p to be t after being nil"
    (let ((org-safe-prohibited-var nil))
      (expect (org-safe-prohibited-p) :to-be nil)
      (org-safe-prohibit)
      (expect (org-safe-prohibited-p)))))

(describe "org-safe-enable"
  (it "should cause prohibited-p to be nil after being t"
    (let ((org-safe-prohibited-var t))
      (expect (org-safe-prohibited-p)))
    (org-safe-enable)
    (expect (org-safe-prohibited-p) :to-be nil)))

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
    (expect (org-safe-prohibited-p) :to-be nil)))

(describe "org-safe-temp-allow-deletion"
  :var ((org-safe-prohibited-duration 0.1)
        (org-safe-prohibited nil))
  (it "switches org-safe from enabled to prohibited"
    (call-interactively 'org-safe-temp-allow-deletion)
    (expect (org-safe-prohibited-p)))
  (it "re-enables org-safe after org-safe-prohibited-duration"
    (sit-for (+ org-safe-prohibited-duration 0.01))
    (expect (org-safe-prohibited-p) :to-be nil)))

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
  (it "returns non-nil when looking back at drawer on previous line"
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

(describe "org-safe-looking-at-document-header-properties-p"
  (it "returns non-nil when looking at document header properties"
    (org-temp-buffer
     "#+TITLE: title
#+COLUMNS: columns"
     (lambda nil
       (goto-char (point-min))
       (expect (looking-at (regexp-quote "#+TITLE: title")))
       (expect (equal major-mode 'org-mode))
       (expect (org-safe-looking-at-document-header-properties-p)))))
  (it "returns non-nil when looking at document header properties anywhere on line"
    (org-temp-buffer
     "#+TITLE: title"
     (lambda nil
       (goto-char 11) ; point position: #+TITLE: ti|tle
       (expect (org-safe-looking-at-document-header-properties-p)))))
  (it "returns nil when NOT looking at document header properties"
    (org-temp-buffer
     "#+TITLE: title
" ; Point at next line
     (lambda nil
       (forward-line) ; point position: #+TITLE: ti|tle
       (expect (org-safe-looking-at-document-header-properties-p) :to-be nil)))))

(describe "org-safe-looking-back-at-document-header-properties-p"
  (it "returns non-nil when looking back at document header properties"
    (org-temp-buffer
     "#+TITLE: title"
     (lambda nil
       (goto-char (point-max))
       (expect (org-safe-looking-back-at-document-header-properties-p)))))
  (it "returns non-nil when looking back at document header previous line"
    (org-temp-buffer
     "#+TITLE: title
"
     (lambda nil
       (goto-char (point-max))
       (expect (org-safe-looking-back-at-document-header-properties-p)))))
  (it "returns nil when NOT looking back at document header properties"
    (org-temp-buffer
     "#+TITLE: title

"
     (lambda nil
       (goto-char (point-max))
       (expect (org-safe-looking-back-at-document-header-properties-p) :to-be nil)))))

(describe "org-safe-document-header-properties-in-region-p"
  (it "returns non-nil when document header properties fully in region"
    (org-temp-buffer
     "#+TITLE: title

"
     (lambda nil
       (set-mark (point-min))
       (goto-char (point-max))
       (expect (org-safe-document-header-properties-in-region-p)))))
  (it "returns non-nil when document header properties fully in region (reversed)"
    (org-temp-buffer
     "#+TITLE: title

"
     (lambda nil
       (set-mark (point-max))
       (goto-char (point-min))
       (expect (org-safe-document-header-properties-in-region-p)))))
  (it "returns non-nil when document header properties partially in region"
    (org-temp-buffer
     "#+TITLE: title

"
     (lambda nil
       (set-mark 11) ; Point at: #+TITLE: ti|tle
       (goto-char (point-max))
       (expect (org-safe-document-header-properties-in-region-p)))))
  (it "returns non-nil when document header properties partially in region (reversed)"
    (org-temp-buffer
     "#+TITLE: title

"
     (lambda nil
       (set-mark (point-max)) ; Point at: #+TITLE: ti|tle
       (goto-char 11)
       (expect (org-safe-document-header-properties-in-region-p)))))
  (it "returns nil when document header properties not in region"
    (org-temp-buffer
     "title

"
     (lambda nil
       (set-mark (point-min)) ; Point at: #+TITLE: ti|tle
       (goto-char (point-max))
       (expect (org-safe-document-header-properties-in-region-p) :to-be nil))))
  (it "returns nil when marker is inactive"
    (org-temp-buffer
     "title

"
     (lambda nil
       (deactivate-mark)
       (expect (org-safe-document-header-properties-in-region-p) :to-be nil)))))

(xdescribe "org-safe-looking-at-document-footer-properties-p"
  (xit "returns non-nil when looking at document footer properties")
  (xit "returns non-nil when looking at document footer properties anywhere on line")
  (xit "returns non-nil when looking at document footer properties on next line")
  (xit "returns nil when NOT looking at document footer properties"))

(xdescribe "org-safe-document-footer-properties-in-region-p"
  (xit "returns non-nil when document footer properties fully in region")
  (xit "returns non-nil when document footer properties partially in region")
  (xit "returns non-nil when document footer properties not in region"))

(xdescribe "org-safe-headline-in-region-p"
  (xit "returns non-nil when headline fully in region")
  (xit "returns non-nil when headline partially in region")
  (xit "returns non-nil when headline not in region"))

(xdescribe "org-safe-drawer-in-region-p"
  (xit "returns non-nil when drawer fully in region")
  (xit "returns non-nil when drawer partially in region")
  (xit "returns non-nil when drawer not in region"))

(describe "org-safe-dolines"
  (it "does NOT error when beginning and ending line is the same"
    (with-temp-buffer
      (expect (org-safe-dolines 1 1 'ignore) :to-be nil)))
  (it "returns function value if no exit-condition provided"
    (with-temp-buffer
      (expect (org-safe-dolines 1 1 (lambda nil
                                      'foobar)) :to-be 'foobar))
    (with-temp-buffer
      (insert "
")
      (expect (org-safe-dolines 1 2 (lambda nil
                                      'foobar)) :to-be 'foobar))
    (with-temp-buffer ; end greater than point max
      (expect (org-safe-dolines 1 2 (lambda nil
                                      'foobar)) :to-throw 'error))
    (with-temp-buffer ; beginning less than point min
      (expect (org-safe-dolines 0 1 (lambda nil
                                      'foobar)) :to-throw 'error)))
  (it "does NOT error when no func is provided"
    (with-temp-buffer
      (expect (org-safe-dolines 1 1) :to-be nil)))
  (it "does NOT error if exit-condition is provided"
    (with-temp-buffer
      (expect (org-safe-dolines 1 1 'ignore nil 'ignore) :to-be nil)))
  (it "immediately returns value if exit-condition is met"
    (with-temp-buffer
      (insert "


")
      (expect (org-safe-dolines 1
                                4
                                'ignore nil (lambda nil
                                              (when (eq (line-number-at-pos)
                                                        3)) :to-be 3)))))
  (xdescribe "runs ntimes"
    (xit "runs correct number of lines down when up is nil")
    (xit "runs correct number of lines up when up is non-nil")))

(provide 'test-org-safe)
;;; test-org-safe.el ends here
