;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I have \"\\([^\"]+\\)\" tags table$"
  (lambda (tag-file)
    (visit-tags-table tag-file)))

(When "^I find a tag that named \"\\([^\"]+\\)\"$"
  (lambda (tag)
    (etags-grep/find-tags tag)))

(Then "^I should place at a line \"\\([^\"]+\\)\" of \"\\([^\"]+\\)\"$"
  (lambda (line-num file)
    ;; NOTE:
    ;;  A current buffer is not equal buffer of selected window.
    (with-current-buffer (window-buffer (selected-window))
      (assert (buffer-file-name) nil
              "\"%s\" buffer has no associated file" (current-buffer))
      (let ((line-num (string-to-number line-num))
            (cur-line (line-number-at-pos (point)))
            (cur-file (file-name-nondirectory (buffer-file-name))))
        (assert (equal cur-file file) nil
                "Expected opened \"%s\" but was \"%s\"" cur-file file)

        (assert (equal cur-line line-num) nil
                "Expected to place at line %d but was line %d"
                line-num cur-line)))))

(Then "^I should show new window$"
  (lambda ()
    (let ((number-of-window (length (window-list))))
      (assert (> number-of-window 1) nil "Has only one window"))))

(And "^I switch the focus to the window that having a \"\\([^\"]+\\)\" buffer$"
  (lambda (mode)
    (dolist (window (window-list))
      (when (equal mode
                   (with-current-buffer (window-buffer window)
                     (format "%s" major-mode)))
        (select-window window)))

    (assert (equal (format "%s" major-mode) mode) nil
            "Expected \"%s\" bus was \"%s\"" mode major-mode)))

(And "^I should show following text$"
  (lambda (text)
    (goto-char (point-min))
    (dolist (line (split-string text "\n"))
      (assert (search-forward line nil t)
              nil "Does not seem that \"%s\"" line)
      (forward-line))))

(When "^I move the cursor to \"\\([^\"]+\\)\"$"
  (lambda (text)
    (goto-char (point-min))
    (search-forward text)))

(When "^I move the cursor to line \"\\([^\"]+\\)\"$"
  (lambda (line-num)
    (let ((line-num (string-to-number line-num)))
      (goto-line line-num))))

(And "^I press \"\\([^\"]+\\)\" key$"
  (lambda (key-string)
    (call-interactively (key-binding (kbd key-string)))))

(Then "^I should place a buffer that named \"\\([^\"]+\\)\"$"
  (lambda (buffer-name)
    (assert (equal buffer-name (buffer-name)) t)))

(And "^I should show \"\\([^\"]+\\)\" in current line$"
  (lambda (text)
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))))
      (assert (string-match (regexp-quote text) line)
              nil "Does not seem that \"%s\", string of current line is \"%s\""
              text line))))

(And "^I switch the other window$"
  (lambda ()
    (other-window 1)))
