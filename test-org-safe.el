;;; test-org-safe.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2021  coffeepenbit

;; Author: coffeepenbit@gmail.com
;; Version: 0.0.1
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

(describe "Enabling and disabling org-safe-mode"
  (it "Toggles org-safe-mode"
    (test-org-safe-with-org-temp-buffer
     ""
     (lambda nil
       (expect (bound-and-true-p org-safe-mode) :to-be nil)
       (org-safe-mode)
       (expect (bound-and-true-p org-safe-mode))
       (org-safe-mode -1)
       (expect (bound-and-true-p org-safe-mode) :to-be nil))))
  (it "defines org-safe-mode keymap"
    (test-org-safe-with-org-temp-buffer
     ""
     (lambda nil
       (expect (bound-and-true-p org-safe-mode-map)))))
  (it "toggles remapping org-delete-char to org-safe-delete-char"
    (test-org-safe-with-org-temp-buffer
     ""
     (lambda nil
       (expect (key-binding (kbd "C-d")) :to-equal 'org-delete-char)
       (org-safe-mode)
       (expect (key-binding (kbd "C-d")) :to-equal 'org-safe-delete-char)
       (org-safe-mode -1)
       (expect (key-binding (kbd "C-d")) :to-equal 'org-delete-char))))
  (it "toggles remapping org-delete-backward-char to org-safe-delete-backward-char"
    (test-org-safe-with-org-temp-buffer
     ""
     (lambda nil
       (expect (key-binding (kbd "DEL")) :to-equal 'org-delete-backward-char)
       (org-safe-mode)
       (expect (key-binding (kbd "DEL")) :to-equal 'org-safe-delete-backward-char)
       (org-safe-mode -1)
       (expect (key-binding (kbd "DEL")) :to-equal 'org-delete-backward-char)))))

(describe "org-safe-delete-char"
  (before-each (setq inhibit-message t))
  (describe "does not prohibit"
    (it "deletes title chars in headline"
      (test-org-safe-with-org-temp-buffer
       "* headline"
       (lambda nil
         (org-safe-mode)
         (goto-char 5)
         (org-safe-delete-char)
         (expect (buffer-string) :to-equal "* hedline"))))
    (it "deletes non-headline asterisks"
      (test-org-safe-with-org-temp-buffer
       "*this is not a headline*"
       (lambda nil
         (org-safe-mode) ; After first asterisk
         (org-safe-delete-char)
         (expect (buffer-string) :to-equal "this is not a headline*")))
      (test-org-safe-with-org-temp-buffer
       "*this is not a headline*"
       (lambda nil
         (org-safe-mode)
         (goto-char (- (point-max) 1)) ; After last asterisk
         (org-safe-delete-char)
         (expect (buffer-string) :to-equal "*this is not a headline")))
      (test-org-safe-with-org-temp-buffer
       "asterisk*"
       (lambda nil
         (org-safe-mode)
         (goto-char (- (point-max) 1)) ; After first asterisk
         (org-safe-delete-char)
         (expect (buffer-string) :to-equal "asterisk")))))
  (describe "prohibits"
    (it "prohibits deleting headline asterisks"
      (test-org-safe-with-org-temp-buffer
       "* headline"
       (lambda nil
         (org-safe-mode)
         (org-safe-delete-char)
         (expect (buffer-string) :to-equal "* headline")))
      (test-org-safe-with-org-temp-buffer
       "** headline" ; Point after first asterisk
       (lambda nil
         (org-safe-mode)
         (forward-char 1)
         (org-safe-delete-char)
         (expect (buffer-string) :to-equal "** headline"))))
    (it "prohibits deleting linebreak in front of headline asterisks"
      (test-org-safe-with-org-temp-buffer
       ;; Point at end of first line
       "this is some test
** headline on next line"
       (lambda nil
         (org-safe-mode)
         (end-of-line)
         (org-safe-delete-char)
         (expect (buffer-string) :to-equal "this is some test
** headline on next line"))))
    (it "prohibits deleting property drawer characters"
      (test-org-safe-with-org-temp-buffer
       "* headline
:PROPERTIES:
:property: nil
:END:"
       (lambda nil
         (forward-line)
         (expect (looking-at (regexp-quote ":PROPERTIES:")))
         (org-safe-delete-char)
         (expect (looking-at (regexp-quote ":PROPERTIES:")))))
      (test-org-safe-with-org-temp-buffer
       "* headline
:PROPERTIES:
:property: nil
:END:"
       (lambda nil
         (forward-line 2)
         (expect (looking-at (regexp-quote ":property: nil")))
         (org-safe-delete-char)
         (expect (looking-at (regexp-quote ":property: nil")))))
      (test-org-safe-with-org-temp-buffer
       "* headline
:PROPERTIES:
:property: nil
:END:"
       (lambda nil
         (forward-line 2)
         (forward-char 5)
         (expect (looking-at (regexp-quote "erty: nil")))
         (org-safe-delete-char)
         (expect (looking-at (regexp-quote "erty: nil")))))
      (test-org-safe-with-org-temp-buffer
       "* headline
:PROPERTIES:
:property: nil
:END:"
       (lambda nil
         (forward-line 3)
         (expect (looking-at (regexp-quote ":END:")))
         (org-safe-delete-char)
         (expect (looking-at (regexp-quote ":END:"))))))
    (it "prohibits deleting logbook drawer"
      (test-org-safe-with-org-temp-buffer
       "* headline
:LOGBOOK:
- Note taken on [2021-04-14 Wed 07:53] \\
  foobar eggs and spam
:END:"
       (lambda nil
         (forward-line)
         (expect (looking-at (regexp-quote ":LOGBOOK:")))
         (org-safe-delete-char)
         (expect (looking-at (regexp-quote ":LOGBOOK:")))))
      (test-org-safe-with-org-temp-buffer
       "* headline
:LOGBOOK:
- Note taken on [2021-04-14 Wed 07:53] \\
  foobar eggs and spam
:END:"
       (lambda nil
         (forward-line 2)
         (forward-char 5)
         (let ((nchars (test-org-safe-nchars)))
           (expect (looking-at (regexp-quote "e taken")))
           (org-safe-delete-char)
           (expect (test-org-safe-nchars) :to-be nchars))))
      (test-org-safe-with-org-temp-buffer
       "* headline
:LOGBOOK:
- Note taken on [2021-04-14 Wed 07:53] \\
  foobar eggs and spam
:END:"
       (lambda nil
         (forward-line 4)
         (expect (looking-at (regexp-quote ":END:")))
         (org-safe-delete-char)
         (expect (looking-at (regexp-quote ":END:"))))))
    (it "prohibts deleting space between headline and asterisk"
      (test-org-safe-with-org-temp-buffer
       "* headline"
       (lambda nil
         (forward-char 1)
         (expect (looking-at (regexp-quote " headline")))
         (org-safe-delete-char)
         (expect (looking-at (regexp-quote " headline"))))))))

(describe "org-safe-looking-at-logbook-note-p"
  (describe "looking at logbook note"
    (it "returns non-nil when looking-at logbook note"
      (test-org-safe-with-org-temp-buffer
       "* headline
:LOGBOOK:
- Note taken on [2021-04-14 Wed 07:53] \\
  foobar eggs and spam
:END:"
       (lambda nil
         (forward-line 2)
         (expect (looking-at (regexp-quote "- Note taken")))
         (expect (org-safe-looking-at-logbook-note-p)))))
    (it "returns non-nil when in logbook note"
      (test-org-safe-with-org-temp-buffer
       "* headline
:LOGBOOK:
- Note taken on [2021-04-14 Wed 07:53] \\
  foobar eggs and spam
:END:"
       (lambda nil
         (forward-line 2)
         (forward-char 5)
         (expect (looking-at (regexp-quote "e taken")))
         (expect (org-safe-looking-at-logbook-note-p)))))
    (it "returns non-nil when looking at deadline reschedule"
      (test-org-safe-with-org-temp-buffer
       "* headline
:LOGBOOK:
- New deadline from \"[2021-04-24 Sat]\" on [2021-04-24 Sat 10:15] \\
  foobar eggs and spam
:END:"
       (lambda nil
         (forward-line 2)
         (forward-char 7)
         (expect (looking-at (regexp-quote "eadline from")))
         (expect (org-safe-looking-at-logbook-note-p)))))
    (it "returns non-nil when looking at schedule reschedule"
      (test-org-safe-with-org-temp-buffer
       "* headline
:LOGBOOK:
- Rescheduled from \"[2021-04-24 Sat]\" to \"[2021-04-25 Sun]\" on [2021-04-24 Sat 10:23] \\
    foobar eggs and spam
:END:"
       (lambda nil
         (forward-line 2)
         (forward-char 7)
         (expect (looking-at (regexp-quote "eduled from")))
         (expect (org-safe-looking-at-logbook-note-p)))))
    (it "returns non-nil when looking at removed deadline"
      (test-org-safe-with-org-temp-buffer
       "* headline
:LOGBOOK:
- Removed deadline, was \"[2021-04-24 Sat]\" on [2021-04-24 Sat 13:22] \\
  foobar eggs and spam
:END:"
       (lambda nil
         (forward-line 2)
         (forward-char 7)
         (expect (looking-at (regexp-quote "ed deadline")))
         (expect (org-safe-looking-at-logbook-note-p))))))
  (describe "NOT looking at logbook note"
    (it "returns nil when looking at empty buffer"
      (test-org-safe-with-org-temp-buffer
       ""
       (lambda nil
         (expect (org-safe-looking-at-logbook-note-p) :to-be nil))))
    (it "returns nil when looking at :LOGBOOK:"
      (test-org-safe-with-org-temp-buffer
       "* headline
:LOGBOOK:
- Note taken on [2021-04-14 Wed 07:53] \\
  foobar eggs and spam
:END:"
       (lambda nil
         (forward-line)
         (expect (looking-at (regexp-quote ":LOGBOOK:")))
         (expect (org-safe-looking-at-logbook-note-p) :to-be nil))))
    (it "returns nil when looking at :END:"
      (test-org-safe-with-org-temp-buffer
       "* headline
:LOGBOOK:
- Note taken on [2021-04-14 Wed 07:53] \\
  foobar eggs and spam
:END:"
       (lambda nil
         (forward-line 4)
         (expect (looking-at (regexp-quote ":END:")))
         (expect (org-safe-looking-at-logbook-note-p) :to-be nil))))
    (it "returns nil when looking before first headline"
      (test-org-safe-with-org-temp-buffer
       "foobar

* headline
:LOGBOOK:
- Note taken on [2021-04-14 Wed 07:53] \\
  foobar eggs and spam
:END:"
       (lambda nil
         (expect (looking-at (regexp-quote "foobar")))
         (expect (org-safe-looking-at-logbook-note-p) :to-be nil))))
    (it "returns nil when looking at paragraph below logbook"
      (test-org-safe-with-org-temp-buffer
       "* headline
:LOGBOOK:
- Note taken on [2021-04-14 Wed 07:53] \\
  foobar eggs and spam
:END:

foobar"
       (lambda nil
         (goto-char (point-max))
         (beginning-of-line)
         (expect (looking-at (regexp-quote "foobar")))
         (expect (org-safe-looking-at-logbook-note-p) :to-be nil))))))

(describe "org-safe-delete-backward-char"
  (before-each (setq inhibit-message t))
  (it "deletes title chars in headline"
    (test-org-safe-with-org-temp-buffer
     "* headline"
     (lambda nil
       (org-safe-mode)
       (goto-char 5)
       (org-safe-delete-backward-char)
       (expect (buffer-string) :to-equal "* hadline"))))
  (it "prohibits deleting headline asterisks"
    (test-org-safe-with-org-temp-buffer
     "* headline"
     (lambda nil
       (org-safe-mode)
       (goto-char 2) ; After first asterisk
       (org-safe-delete-backward-char)
       (expect (buffer-string) :to-equal "* headline")))
    (test-org-safe-with-org-temp-buffer
     "** headline"
     (lambda nil
       (org-safe-mode)
       (goto-char 2) ; After first asterisk
       (org-safe-delete-backward-char)
       (expect "** headline" :to-equal (buffer-string)))))
  (it "allows deletion of non-headline asterisks"
    (test-org-safe-with-org-temp-buffer
     "*this is not a headline*"
     (lambda nil
       (org-safe-mode)
       (goto-char 2) ; After first asterisk
       (org-safe-delete-backward-char)
       (expect (buffer-string) :to-equal "this is not a headline*")))
    (test-org-safe-with-org-temp-buffer
     "*this is not a headline*"
     (lambda nil
       (org-safe-mode)
       (goto-char (point-max))
       (org-safe-delete-backward-char)
       (expect (buffer-string) :to-equal "*this is not a headline")))
    (test-org-safe-with-org-temp-buffer
     "asterisk*"
     (lambda nil
       (org-safe-mode)
       (goto-char (point-max))
       (org-safe-delete-backward-char)
       (expect (buffer-string) :to-equal "asterisk"))))
  ;; TODO implement this test
  (xit "does NOT delete property drawer")
  ;; TODO implement this test
  (xit "does NOT delete logbook drawer")
  ;; TODO implement this test
  (xit "it does NOT delete space between headline and asterisk"))

(describe "org-safe-looking-at-headline-stars-p"
  (it "should be t when looking at a headline"
    (test-org-safe-with-org-temp-buffer
     "* headline"
     (lambda nil
       (expect (org-safe-looking-at-headline-stars-p) :to-be t)))
    (test-org-safe-with-org-temp-buffer
     "** headline"
     (lambda nil
       (expect (org-safe-looking-at-headline-stars-p) :to-be t)))
    (test-org-safe-with-org-temp-buffer
     "** headline"
     (lambda nil
       (goto-char 2)
       (expect (org-safe-looking-at-headline-stars-p) :to-be t)))
    (test-org-safe-with-org-temp-buffer
     "* headline*"
     (lambda nil
       (expect (org-safe-looking-at-headline-stars-p) :to-be t)))
    (test-org-safe-with-org-temp-buffer
     "*    headline"
     (lambda nil
       (expect (org-safe-looking-at-headline-stars-p) :to-be t)))
    (test-org-safe-with-org-temp-buffer
     "
* headline"
     (lambda nil
       (expect (org-safe-looking-at-headline-stars-p)) :to-be t)))
  (it "should be nil when looking at non-headlines"
    (test-org-safe-with-org-temp-buffer
     "*headline"
     (lambda nil
       (expect (org-safe-looking-at-headline-stars-p) :to-be nil)))
    (test-org-safe-with-org-temp-buffer
     "*headline*"
     (lambda nil
       (expect (org-safe-looking-at-headline-stars-p) :to-be nil)))))

(describe "org-safe-looking-back-at-headline-stars-p"
  (it "should be non-nil when looking back at single headline star"
    (test-org-safe-with-org-temp-buffer
     "* headline"
     (lambda nil
       (goto-char 2) ; After first asterisk
       (expect (org-safe-looking-back-at-headline-stars-p)))))
  (it "should be non-nil when looking back at two headline stars"
    (test-org-safe-with-org-temp-buffer
     "** headline"
     (lambda nil
       (goto-char 3) ; After first asterisk
       (expect (org-safe-looking-back-at-headline-stars-p)))))
  (it "should be non-nil when between two headline stars"
    (test-org-safe-with-org-temp-buffer
     "** headline"
     (lambda nil
       (goto-char 3) ; After first asterisk
       (expect (org-safe-looking-back-at-headline-stars-p)))))
  (it "should be non-nil when looking at stars without titles"
    ;; org-mode considers these titles still
    "* " ; Title
    (lambda nil
      (goto-char 2) ; After first asterisk
      (expect (org-safe-looking-back-at-headline-stars-p))))
  (describe "when NOT looking back at headline stars"
    (it "should be nil when asterisk belongs to bold phrase"
      (test-org-safe-with-org-temp-buffer
       "*headline*"
       (lambda nil
         (goto-char 2) ; After first asterisk
         (expect (org-safe-looking-back-at-headline-stars-p) :to-be nil))))))

(describe "org-safe-looking-at-headline-star-space"
  (it "should be non-nil when looking at single headline space"
    (test-org-safe-with-org-temp-buffer
     "* headline"
     (lambda nil
       (goto-char 1) ; After first asterisk
       (expect (org-safe-looking-at-headline-star-space)))))
  (it "should be non-nil when looking at multiple headline spaces"
    (test-org-safe-with-org-temp-buffer
     "*     headline"
     (lambda nil
       (goto-char 1) ; After first asterisk
       (expect (org-safe-looking-at-headline-star-space))))))

(describe "org-safe-looking-back-at-headline-star-space"
  (it "should be non-nil when looking back at single headline star"
    (test-org-safe-with-org-temp-buffer
     "* headline"
     (lambda nil
       (goto-char 2) ; After first asterisk
       (expect (org-safe-looking-back-at-headline-star-space))))))

(describe "org-safe-prohibited-p"
  (it "should be non-nil when org-safe-prohibited-var is t"
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
    (test-org-safe-with-org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:"
     (lambda nil

       (forward-line)
       (expect (looking-at (regexp-quote ":PROPERTIES:")))
       (expect (org-safe-looking-at-drawer-p)))))
  (it "returns non-nil when looking at a property key"
    (test-org-safe-with-org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:"
     (lambda nil
       (forward-line 2)
       (expect (looking-at (regexp-quote ":foo:")))
       (expect (org-safe-looking-at-drawer-p)))))
  (it "returns non-nil when looking at a property value"
    (test-org-safe-with-org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:"
     (lambda nil
       (forward-line 2)
       (forward-char 6)
       (expect (looking-at (regexp-quote "bar")))
       (expect (org-safe-looking-at-drawer-p)))))
  (it "returns non-nil when drawer is on next line"
    (test-org-safe-with-org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:"
     (lambda nil
       (end-of-line)
       (expect (org-safe-looking-at-drawer-p)))))
  (it "returns nil when drawer is NOT on next line"
    (test-org-safe-with-org-temp-buffer
     "* headline
foo bar"
     (lambda nil
       (end-of-line)
       (expect (regexp-quote "\n"))
       (expect (org-safe-looking-at-drawer-p) :to-be nil))))
  (it "returns non-nil when looking at :something:"
    (test-org-safe-with-org-temp-buffer
     ":something:" ; org-mode considers this to be a drawer
     (lambda nil
       (expect (looking-at (regexp-quote ":something:")))
       (expect (org-safe-looking-at-drawer-p)))))
  (it "returns non-nil when looking at :END:"
    (test-org-safe-with-org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:"
     (lambda nil
       (forward-line 3)
       (expect (looking-at (regexp-quote ":END:")))
       (expect (org-safe-looking-at-drawer-p)))))
  (it "returns non-nil when looking at :LOGBOOK:"
    (test-org-safe-with-org-temp-buffer
     "* headline
:LOGBOOK:
- Note taken on [2021-04-14 Wed 07:53] \\
  foobar eggs and spam
:END:"
     (lambda nil
       (forward-line)
       (expect (looking-at (regexp-quote ":LOGBOOK:")))
       (expect (org-safe-looking-at-drawer-p)))))
  (it "returns nil when NOT looking at a drawer"
    (test-org-safe-with-org-temp-buffer
     ""
     (lambda nil
       (expect (org-safe-looking-at-drawer-p) :to-be nil)))
    (test-org-safe-with-org-temp-buffer
     "* headline"
     (lambda nil
       (expect (org-safe-looking-at-drawer-p) :to-be nil)))))

(describe "org-safe-drawer-on-this-line-p"
  (it "does not change users point"
    (test-org-safe-with-org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:"
     (lambda nil
       (end-of-line)
       (org-safe-drawer-on-this-line-p)
       (expect (point) :to-be 11)))))

(describe "org-safe-looking-back-at-drawer-p"
  (it "returns non-nil when looking back at :PROPERTIES:"
    (test-org-safe-with-org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:"
     (lambda nil
       (forward-line)
       (end-of-line)
       (expect (org-safe-looking-back-at-drawer-p)))))
  (it "returns non-nil when looking back at :END:"
    (test-org-safe-with-org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:"
     (lambda nil
       (forward-line 3)
       (end-of-line)
       (expect (org-safe-looking-back-at-drawer-p)))))
  (it "returns non-nil when looking back at :LOGBOOK:"
    (test-org-safe-with-org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:"
     (lambda nil
       (forward-line 3)
       (end-of-line)
       (expect (org-safe-looking-back-at-drawer-p)))))
  (it "returns nil when looking back at nothing"
    (test-org-safe-with-org-temp-buffer
     ""
     (lambda nil
       (forward-line 3)
       (end-of-line)
       (expect (org-safe-looking-back-at-drawer-p) :to-be nil))))
  (it "returns non-nil when looking back at drawer on previous line"
    (test-org-safe-with-org-temp-buffer
     "* headline
:PROPERTIES:
:foo: bar
:END:
"
     (lambda nil
       (goto-char (point-max))
       (expect (org-safe-looking-back-at-drawer-p)))))
  (it "returns nil when looking back at non-drawer"
    (test-org-safe-with-org-temp-buffer
     "* headline
okay
"
     (lambda nil
       (goto-char (point-max))
       (expect (org-safe-looking-back-at-drawer-p) :to-be nil)))))

(describe "org-safe-looking-at-document-header-properties-p"
  (it "returns non-nil when looking at document header properties"
    (test-org-safe-with-org-temp-buffer
     "#+TITLE: title
#+COLUMNS: columns"
     (lambda nil
       (goto-char (point-min))
       (expect (looking-at (regexp-quote "#+TITLE: title")))
       (expect (equal major-mode 'org-mode))
       (expect (org-safe-looking-at-document-header-properties-p)))))
  (it "returns non-nil when looking at document header properties anywhere on line"
    (test-org-safe-with-org-temp-buffer
     "#+TITLE: title"
     (lambda nil
       (goto-char 11) ; point position: #+TITLE: ti|tle
       (expect (org-safe-looking-at-document-header-properties-p)))))
  (it "returns nil when NOT looking at document header properties"
    (test-org-safe-with-org-temp-buffer
     "#+TITLE: title
" ; Point at next line
     (lambda nil
       (forward-line) ; point position: #+TITLE: ti|tle
       (expect (org-safe-looking-at-document-header-properties-p) :to-be nil)))))

(describe "org-safe-looking-back-at-document-header-properties-p"
  (it "returns non-nil when looking back at document header properties"
    (test-org-safe-with-org-temp-buffer
     "#+TITLE: title"
     (lambda nil
       (goto-char (point-max))
       (expect (org-safe-looking-back-at-document-header-properties-p)))))
  (it "returns non-nil when looking back at document header previous line"
    (test-org-safe-with-org-temp-buffer
     "#+TITLE: title
"
     (lambda nil
       (goto-char (point-max))
       (expect (org-safe-looking-back-at-document-header-properties-p)))))
  (it "returns nil when NOT looking back at document header properties"
    (test-org-safe-with-org-temp-buffer
     "#+TITLE: title

"
     (lambda nil
       (goto-char (point-max))
       (expect (org-safe-looking-back-at-document-header-properties-p) :to-be nil)))))

(describe "org-safe-document-header-properties-in-region-p"
  (it "returns non-nil when document header properties fully in region"
    (test-org-safe-with-org-temp-buffer
     "#+TITLE: title

"
     (lambda nil
       (push-mark (point-min))
       (goto-char (point-max))
       (expect (org-safe-document-header-properties-in-region-p)))))
  (it "returns non-nil when document header properties fully in region (reversed)"
    (test-org-safe-with-org-temp-buffer
     "#+TITLE: title

"
     (lambda nil
       (push-mark (point-max))
       (goto-char (point-min))
       (expect (org-safe-document-header-properties-in-region-p)))))
  (it "returns non-nil when document header properties partially in region"
    (test-org-safe-with-org-temp-buffer
     "#+TITLE: title

"
     (lambda nil
       (push-mark 11) ; Point at: #+TITLE: ti|tle
       (goto-char (point-max))
       (expect (org-safe-document-header-properties-in-region-p)))))
  (it "returns non-nil when document header properties partially in region (reversed)"
    (test-org-safe-with-org-temp-buffer
     "#+TITLE: title

"
     (lambda nil
       (push-mark (point-max)) ; Point at: #+TITLE: ti|tle
       (goto-char 11)
       (expect (org-safe-document-header-properties-in-region-p)))))
  (it "returns nil when document header properties not in region"
    (test-org-safe-with-org-temp-buffer
     "title

"
     (lambda nil
       (push-mark (point-min)) ; Point at: #+TITLE: ti|tle
       (goto-char (point-max))
       (expect (org-safe-document-header-properties-in-region-p) :to-be nil))))
  (it "returns nil when marker is inactive"
    (test-org-safe-with-org-temp-buffer
     "title

"
     (lambda nil
       (deactivate-mark)
       (expect (org-safe-document-header-properties-in-region-p) :to-be nil)))))

(describe "org-safe-looking-at-document-footer-properties-p"
  (it "returns non-nil when looking at document footer properties"
    (test-org-safe-with-org-temp-buffer
     "# Local Variables:
# mode: org
# org-complete-tags-always-offer-all-agenda-tags: nil
# End:"
     (lambda nil
       (expect (org-safe-looking-at-document-footer-properties-p)))))
  (it "returns non-nil when looking at document footer properties anywhere on line"
    (test-org-safe-with-org-temp-buffer
     "# Local Variables:
# mode: org
# org-complete-tags-always-offer-all-agenda-tags: nil
# End:"
     (lambda nil
       (forward-char 4)
       (expect (looking-at "cal Variables:"))
       (expect (org-safe-looking-at-document-footer-properties-p)))))
  (it "returns non-nil when looking at document footer properties on next line"
    (test-org-safe-with-org-temp-buffer
     "foobar
# Local Variables:
# mode: org
# org-complete-tags-always-offer-all-agenda-tags: nil
# End:"
     (lambda nil
       (end-of-line)
       (expect (org-safe-looking-at-document-footer-properties-p)))))
  (it "returns nil when NOT looking at document footer properties"
    (test-org-safe-with-org-temp-buffer
     ""
     (lambda nil
       (expect (org-safe-looking-at-document-footer-properties-p) :to-be nil)))
    (test-org-safe-with-org-temp-buffer
     "# some comment"
     (lambda nil
       (expect (org-safe-looking-at-document-footer-properties-p) :to-be nil)))))

(describe "org-safe-looking-at-document-footer-properties-on-this-line-p"
  (it "returns nil when document footer properties NOT on current line"
    (test-org-safe-with-org-temp-buffer
     ""
     (lambda nil
       (expect
        (org-safe-looking-at-document-footer-properties-on-this-line-p) :to-be nil))))
  (describe "when footer properties on current line"
    (it "returns non-nil when looking at footer property"
      (test-org-safe-with-org-temp-buffer
       "# Local Variables:
# mode: org
# org-complete-tags-always-offer-all-agenda-tags: nil
# End:"
       (lambda nil
         (expect
          (org-safe-looking-at-document-footer-properties-on-this-line-p)))))
    (it "returns non-nil when looking at footer property not from beginnig of line"
      (test-org-safe-with-org-temp-buffer
       "# Local Variables:
# mode: org
# org-complete-tags-always-offer-all-agenda-tags: nil
# End:"
       (lambda nil
         (forward-char 5)
         (expect (org-safe-looking-at-document-footer-properties-on-this-line-p)))))))

(describe "org-safe-document-footer-properties-in-region-p"
  (it "returns non-nil when document footer properties fully in region"
    (test-org-safe-with-org-temp-buffer
     "# Local Variables:
# mode: org
# org-complete-tags-always-offer-all-agenda-tags: nil
# End:"
     (lambda nil
       (push-mark (point-min))
       (goto-char (point-max))
       (expect (org-safe-document-footer-properties-in-region-p) :to-be nil))))
  (it "returns non-nil when document footer properties partially in region"
    (test-org-safe-with-org-temp-buffer
     "# Local Variables:
# mode: org
# org-complete-tags-always-offer-all-agenda-tags: nil
# End:"
     (lambda nil
       (forward-char 5)
       (expect (looking-at (regexp-quote "al Variables:")))
       (push-mark (point-min))
       (goto-char (point-max))
       (expect (org-safe-document-footer-properties-in-region-p) :to-be nil))))
  (it "returns non-nil when document footer properties not in region"
    (test-org-safe-with-org-temp-buffer
     "
"
     (lambda nil
       (push-mark (point))
       (forward-char)
       (expect (org-safe-document-footer-properties-in-region-p) :to-be nil)))))

(describe "org-safe-headline-in-region-p"
  (it "returns non-nil when headline fully in region"
    (test-org-safe-with-org-temp-buffer
     "* headline
foobar
"
     (lambda nil
       (push-mark (point))
       (goto-char (point-max))
       (expect (org-safe-headline-in-region-p)))))
  (it "returns non-nil when headline partially in region"
    (test-org-safe-with-org-temp-buffer
     "* headline
foobar
"
     (lambda nil
       (forward-char 2)
       (push-mark (point))
       (goto-char (point-max))
       (expect (org-safe-headline-in-region-p)))))
  (it "returns nil when headline not in region"
    (test-org-safe-with-org-temp-buffer
     "
"
     (lambda nil
       (push-mark (point))
       (forward-char)
       (expect (org-safe-headline-in-region-p) :to-be nil))))
  (it "returns nil mark is inactive"
    (test-org-safe-with-org-temp-buffer
     ""
     (lambda nil
       (expect mark-active :to-be nil)
       (expect (org-safe-headline-in-region-p) :to-be nil)))))

(describe "org-safe-drawer-in-region-p"
  (it "returns non-nil when drawer fully in region"
    (test-org-safe-with-org-temp-buffer
     "* headline
:PROPERTIES:
:property: nil
:END:"
     (lambda nil
       (push-mark (point))
       (goto-char (point-max))
       (expect (org-safe-drawer-in-region-p)))))
  (it "returns non-nil when drawer partially in region"
    (test-org-safe-with-org-temp-buffer
     "* headline
:PROPERTIES:
:property: nil
:END:"
     (lambda nil
       (forward-line 2)
       (forward-char 5) ; Point at :prop|erty: nil
       (expect (looking-at (regexp-quote "erty: nil")))
       (push-mark (point))
       (end-of-line)
       (expect (org-safe-drawer-in-region-p)))))
  (it "returns nil when drawer not in region"
    (test-org-safe-with-org-temp-buffer
     "
"
     (lambda nil
       (push-mark (point))
       (forward-char)
       (expect (org-safe-drawer-in-region-p) :to-be nil)))))

(describe "org-safe-dolines"
  (it "does NOT error when beginning and ending line is the same"
    (with-temp-buffer
      (expect (org-safe-dolines 1 1 'ignore) :to-equal '(nil))))
  (it "does NOT error when no func is provided"
    (with-temp-buffer
      (expect (org-safe-dolines 1 1) :to-equal '(nil))))
  (it "does NOT error if exit-condition is provided"
    (with-temp-buffer
      (expect (org-safe-dolines 1 1 'ignore 'ignore) :to-equal '(nil))))
  (it "returns function value if no exit-condition provided"
    (with-temp-buffer
      (expect (org-safe-dolines 1 1 (lambda nil
                                      'foobar)) :to-equal '(foobar)))
    (with-temp-buffer
      (insert "
")
      (expect (org-safe-dolines 1 2 (lambda nil
                                      'foobar)) :to-equal '(foobar foobar)))
    (with-temp-buffer ; end greater than point max
      (expect (org-safe-dolines 1 2 (lambda nil
                                      'foobar)) :to-throw 'error))
    (with-temp-buffer ; beginning less than point min
      (expect (org-safe-dolines 0 1 (lambda nil
                                      'foobar)) :to-throw 'error)))
  (it "immediately returns value if exit-condition is met"
    (with-temp-buffer
      (insert "


")
      (expect (org-safe-dolines 1
                                4
                                'ignore
                                (lambda nil
                                  (when (eq (line-number-at-pos)
                                            3)) :to-be 3)))))
  (it "returns values in proper order (top-to-bottom)"
    (with-temp-buffer
      (insert "a
b
c
d")
      (expect (org-safe-dolines (point-min)
                                (point-max)
                                (lambda nil
                                  (char-after)))
              :to-equal (list ?a ?b ?c ?d))))
  (it "returns values in proper order (bottom-to-top)"
    (with-temp-buffer
      (insert "a
b
c
d") ; Point look at d
      (expect (org-safe-dolines 7 ; Needs to be 7, not 8, to look at "d"
                                1
                                (lambda nil
                                  (char-after)))
              :to-equal (list ?d ?c ?b ?a))))
  (describe "runs ntimes"
    :var (foo)
    (before-each
      (setf (symbol-function 'foo) 'ignore)
      (spy-on 'foo))
    (it "call func once when one line"
      (with-temp-buffer
        (org-safe-dolines 1 1 'foo)
        (expect 'foo :to-have-been-called-times 1)))
    (it "runs correct number of lines (top-to-bottom)"
      (with-temp-buffer
        (insert "


")

        (org-safe-dolines 1 4 'foo)
        (expect 'foo :to-have-been-called-times 4)))
    (it "runs correct number of lines (bottom-to-top)"
      (with-temp-buffer
        (insert "


")
        (org-safe-dolines 4 1 'foo)
        (expect 'foo :to-have-been-called-times 4)))))

;;;; Helpers
(defun test-org-safe-with-org-temp-buffer (buffer-text func)
  "Useful for testing `org-mode' functions.

BUFFER-TEXT is the initial state of the `org-mode' buffer.

FUNC is what is ran after creating the buffer."
  (with-temp-buffer
    (insert buffer-text)
    (org-mode)
    (goto-char (point-min))
    (funcall func)))

(defun test-org-safe-nchars nil
  "Count number of chars in buffer.

Works for narrowed buffers."
  (if (buffer-narrowed-p)
      (+ (- (point-max)
            (point-min))
         1)
    (point-max)))

(provide 'test-org-safe)
;;; test-org-safe.el ends here

;; Local Variables:
;; origami-fold-style: buttercup
;; origami-mode: t
;; End:
