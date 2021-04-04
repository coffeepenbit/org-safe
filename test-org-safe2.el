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


(describe "delete-char"
          (it "deletes headline title chars"
              (expect "* hedline" :to-equal
                      (org-temp-buffer
                       "* headline"
                       (lambda nil
                         (org-safe-mode)
                         (goto-char 5)
                         (org-safe-delete-char)
                         (buffer-string))))))


(describe "delete-backward-char"
  (it "doesn't prevent title deletion at end"
    (expect "* hadline" :to-equal
            (org-temp-buffer
             "* headline"
             (lambda nil
               (org-safe-mode)
               (goto-char 5)
               (org-safe-delete-backward-char)
               (buffer-string)))))
  (it "doesn't delete one asterisk"
    (expect "* headline" :to-equal
            (org-temp-buffer
             "* headline"
             (lambda nil
               (org-safe-mode)
               (goto-char 2) ; After first asterisk
               (org-safe-delete-backward-char)
               (buffer-string)))))
  (it "doesn't delete two asterisks"
    (expect "** headline" :to-equal
            (org-temp-buffer
             "** headline"
             (lambda nil
               (org-safe-mode)
               (goto-char 2) ; After first asterisk
               (org-safe-delete-backward-char)
               (buffer-string)))))
  (it "deletes non headline asterisks"
    (expect "this is not a headline*" :to-equal
            (org-temp-buffer
             "*this is not a headline*"
             (lambda nil
               (org-safe-mode)
               (goto-char 2) ; After first asterisk
               (org-safe-delete-backward-char)
               (buffer-string)))))
  (it "deletes end asterisk"
    (expect "*this is not a headline" :to-equal
            (org-temp-buffer
             "*this is not a headline*"
             (lambda nil
               (org-safe-mode)
               (goto-char (point-max)) ; After last asterisk
               (org-safe-delete-backward-char)
               (buffer-string)))))
  (expect "asterisk" :to-equal
          (org-temp-buffer
           "asterisk*"
           (lambda nil
             (org-safe-mode)
             (goto-char (point-max)) ; After first asterisk
             (org-safe-delete-backward-char)
             (buffer-string)))))


(describe "point-looking-at-headline-stars-p"
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


(describe "point-on-headline-stars-p"
          (it "should be t when point is on headline"
              (expect t :should-be
                      (org-temp-buffer
                       "* headline"
                       (lambda nil
                         (org-safe-mode)
                         (goto-char 2) ; After first asterisk
                         (org-safe-point-on-headline-stars-p))))
              (expect t :should-be
                      t
                      (org-temp-buffer
                       "
* headline"
                       (lambda nil
                         (org-safe-mode)
                         (goto-char (point-min)) ; At line above asterisk
                         (org-safe-point-on-headline-stars-p)))))
          (it "should be nil when point on non-headlines"
              (expect nil :to-be
                      (org-temp-buffer
                       "*headline"
                       (lambda nil
                         (org-safe-mode)
                         (goto-char 2) ; After first asterisk
                         (org-safe-point-on-headline-stars-p))))
              (expect nil :to-be
                      (org-temp-buffer
                       " * headline"
                       (lambda nil
                         (org-safe-mode)
                         (goto-char 3) ; After first asterisk
                         (org-safe-point-on-headline-stars-p))))
              (expect nil :to-be
                      (org-temp-buffer
                       "* headline*"
                       (lambda nil
                         (org-safe-mode)
                         (goto-char (point-max)) ; After first asterisk
                         (org-safe-point-on-headline-stars-p))))))


(describe "prohibited-p"
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


(describe "org-safe-disabled-timer/prohibited-nil-to-t-to-nil"
          (org-temp-buffer
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


;; (describe "test-org-safe-temp-allow-deletion nil
;;   :tags '(org-safe-temp-allow-deletion)
;;   (should (equal
;;            "* hadline"
;;            (org-temp-buffer
;;             "* headline"
;;             (lambda nil
;;               (org-safe-mode)
;;               (goto-char 5)
;;               (org-safe-delete-backward-char)
;;               (buffer-string))))))



(describe "test-org-safe-temp-allow-deletion nil
  :tags '(org-safe-temp-allow-deletion)
  (org-temp-buffer
   "
          * headline"
   (lambda nil
     (let ((org-safe-prohibited-duration 0.1))
       (org-safe-mode)
       ;; Verify start not prohibited
       (should (equal nil (org-safe-prohibited-p)))

       ;; Temporarily prohibit
       (call-interactively 'org-safe-temp-allow-deletion)
       (should (equal t (org-safe-prohibited-p)))

       ;; Verify prohibit status is reset
       (sit-for (+ org-safe-prohibited-duration 0.01))
       (should (equal nil (org-safe-prohibited-p)))))))


(describe "test-org-safe-temp-allow-deletion/allow-backward-delete nil
          :tags '(org-safe-temp-allow-deletion)
          (org-temp-buffer
           "** headline"
           (lambda nil
             (let ((org-safe-prohibited-duration 0.1))
               (org-safe-mode)
               (goto-char 3)

               ;; Verify start not prohibited
               (should (equal nil (org-safe-prohibited-p)))

               ;; Verify looking back at two stars
               (should (equal t (looking-back "^\\*\\*" nil)))

               (call-interactively 'org-safe-delete-backward-char)

               ;; Still looking back at two stars
               (should (equal t (looking-back "^\\*" nil)))

               (call-interactively 'org-safe-temp-allow-deletion)

               ;; Now it should be deleted (one star)
               (should (equal t (looking-back "^\\*" nil)))

               ;; Verify prohibit status is reset
               (sit-for (+ org-safe-prohibited-duration 0.01))
               (should (equal nil (org-safe-prohibited-p)))

               ;; Try deleting remaining star
               (call-interactively 'org-safe-delete-backward-char)

               ;; Verify that trying to delete again doesn't work
               (should (equal t (looking-back "^\\*" nil)))))))
