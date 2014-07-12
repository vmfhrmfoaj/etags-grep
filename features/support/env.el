(require 'f)

(defvar etags-grep-support-path
  (f-dirname load-file-name))

(defvar etags-grep-features-path
  (f-parent etags-grep-support-path))

(defvar etags-grep-root-path
  (f-parent etags-grep-features-path))

(add-to-list 'load-path etags-grep-root-path)

;; dependent packages for *-step.el
(require 'cl)
(require 'compile)

(require 'etags)
(require 'grep)

(require 'etags-grep)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 (setq ecukes-verbose nil)
 (setq tags-add-tables t)
 (remove-hook 'find-file-hooks 'vc-find-file-hook)
 (setq initial-default-directory default-directory)
 (setq initial-buffer-list (buffer-list)))

(Before
 ;; Before each scenario is run
 (setq default-directory initial-default-directory))

(After
 ;; After each scenario is run
 (tags-reset-tags-tables)
 (dolist (buffer (set-difference (buffer-list) initial-buffer-list))
   (kill-buffer buffer))
 (delete-other-windows))

(Teardown
 ;; After when everything has been run
 )
