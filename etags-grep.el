;; etags-grep.el, Copyright (C) 2014 Jinseop Kim(vmfhrmfoaj@yahoo.com)
;; etags-grep.el comes with ABSOLUTELY NO WARRANTY; for details
;; type LICENSE file.  This is free software, and you are welcome
;; to redistribute it under certain conditions; type LICENSE
;; for details.
;; If the LICENSE file does not exist,
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

(require 'cl)
(require 'dash)

(defvar etags-grep/a-start-marker nil
  "A position when calling the `find-tags'")


;;;###autoload
(defun etags-grep/find-tags (tag)
  (interactive (find-tag-interactive "Find tag: "))
  (setq etags-grep/a-start-marker (point-marker))
  (etags-grep/find-tags-regex (concat "\\<" tag "\\>")))

(defun etags-grep/find-tags-regex (tag-regex)
  (let* ((tag-files     (etags-grep/tag-files))
         (tags-base-dir (etags-grep/tags-default-directory tag-files))
         (tag-infos     (etags-grep/matched-tag-infos tags-base-dir
                                                      tag-files
                                                      tag-regex)))
    (cond ((= (length tag-infos) 0)
           (error "There is no tag to match"))
          ((= (length tag-infos) 1)
           (etags-grep/insert-marker-to-etags-marker-ring
            etags-grep/a-start-marker)
           (etags-grep/goto-first-tag tags-base-dir tag-infos))
          ((> (length tag-infos) 1)
           (pop-to-buffer (etags-grep/create-buffer "*etags-grep*"))
           (setq default-directory tags-base-dir)
           (etags-grep/insert-tags tag-infos)))
  tag-infos))

(defun etags-grep/tag-files ()
  (eval-when-compile (require 'etags))
  (mapcar 'tags-expand-table-name tags-table-list))

(defun etags-grep/matched-tag-infos (tags-base-dir tag-files tag-regex)
  "Generate a matched tags info table"
  (let* ((tag-template "^\\(.*%s\\(\\s-\\|\\s(\\|;\\).*\\)\^?\\(\\(.+\\)\^A\\)?\\([0-9]+\\),[0-9]+")
         (tag-regex (format tag-template tag-regex))
         matched-tag-infos)
    (dolist (tag-file tag-files)
      (setq matched-tag-infos
            (append matched-tag-infos
                    (etags-grep/matched-tag-infos- tags-base-dir
                                                   tag-file
                                                   tag-regex))))
    matched-tag-infos))

(defun etags-grep/matched-tag-infos- (tags-base-dir tag-file tag-regex)
  (let* ((base-dir-lst   (split-string tags-base-dir "/"))
         (tag-dir-lst    (split-string (file-name-directory tag-file) "/"))
         (common-dir-lst (set-difference tag-dir-lst
                                         base-dir-lst
                                         :test 'string-equal))
         (dir-lst (mapcar (lambda (s) (concat s "/")) common-dir-lst))
         (dir     (when common-dir-lst (apply 'concat dir-lst)))
         tag-infos)
    (-when-let (tag-buf (get-file-buffer tag-file))
      (set-buffer tag-buf)
      (goto-char (point-min))
      (while (re-search-forward tag-regex nil t)
        (let* ((tag-str  (match-string-no-properties 1))
               (file-1   (etags-grep/file-name-cur-tag))
               (file     (etags-grep/new-path dir file-1))
               (line-num (match-string-no-properties 5))
               (meta     (match-string-no-properties 4))
               (tag-info (etags-grep/set-info
                          tag-file tag-str file line-num meta)))
          (add-to-list 'tag-infos tag-info t))))
    tag-infos))

(defun etags-grep/file-name-cur-tag ()
  (save-match-data
    (save-excursion
      (backward-page)
      (forward-line 1)
      (beginning-of-line)
      (re-search-forward "^\\(.+\\),"))
    (match-string-no-properties 1)))

(defun etags-grep/goto-first-tag (tags-base-dir tag-infos)
  (let* ((tag-info (first tag-infos))
         (file-1   (etags-grep/info 'file tag-info))
         (file     (etags-grep/new-path tags-base-dir file-1))
         (line-num (string-to-number (etags-grep/info 'line-num tag-info))))
    (etags-grep/goto-line-of-file file line-num)))

(defun etags-grep/goto-line-of-file (file line &optional buf-display-fn)
  (if (null buf-display-fn)
      (find-file file)
    (funcall buf-display-fn (find-file-noselect file)))
  (goto-line line))

(defun etags-grep/insert-marker-to-etags-marker-ring (marker)
  (eval-when-compile (require 'etags))
  (ring-insert find-tag-marker-ring marker))

(defun etags-grep/create-buffer (buf-name)
  (-when-let (buf (get-buffer buf-name))
    (kill-buffer buf))
  (with-current-buffer (get-buffer-create buf-name)
    (grep-mode)
    (goto-char (point-min))
    (current-buffer)))

(defun etags-grep/insert-tags (tag-infos)
  (let ((buffer-read-only nil))
    (insert (etags-grep/mode-header))
    (dolist (tag-info tag-infos)
      (let* ((tag-file (etags-grep/info 'tag-file tag-info))
             (file     (etags-grep/info 'file     tag-info))
             (line-num (etags-grep/info 'line-num tag-info))
             (tag-str  (etags-grep/info 'tag      tag-info)))
        (insert (concat (unless (file-name-absolute-p file) "./") file ":"
                        line-num ": " tag-str "\n"))))))

(defun etags-grep/mode-header ()
  (concat
   "-*- mode: grep; default-directory: \"" default-directory "\" -*-\n\n"
   "Find tag: \"" (if tag tag tag-regex) "\"\n"))

(defun etags-grep/tags-default-directory (tag-files)
  (let* (base-dir-lst)
    (dolist (tag-file tag-files)
      (setq base-dir-lst
            (if base-dir-lst
                (reverse (intersection base-dir-lst
                                       (split-string tag-file "/")
                                       :test 'string-equal))
              (split-string (file-name-directory tag-file) "/"))))
    (apply 'concat (mapcar (lambda (s) (concat s "/")) base-dir-lst))))


(defun etags-grep/new-path (dir file)
  (if (file-name-absolute-p file) file (concat dir file)))

(defun etags-grep/set-info (tag-file tag-str file line-num meta)
  (list (etags-grep/info-element 'tag-file tag-file)
        (etags-grep/info-element 'tag      tag-str)
        (etags-grep/info-element 'file     file)
        (etags-grep/info-element 'line-num line-num)
        (etags-grep/info-element 'meta     meta)))

(defun etags-grep/info-element (k v) (list k v))
(defun etags-grep/info (k alist) (second (assoc k alist)))


(provide 'etags-grep)
