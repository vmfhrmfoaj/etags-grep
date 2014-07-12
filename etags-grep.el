;; etags-grep.el, Copyright (C) 2014 Jinseop Kim(vmfhrmfoaj@yahoo.com)
;; etags-grep.el comes with ABSOLUTELY NO WARRANTY; for details
;; type LICENSE file.  This is free software, and you are welcome
;; to redistribute it under certain conditions; type LICENSE
;; for details.
;; If the LICENSE file does not exist,
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

(require 'dash)

(defvar etags-grep/buf-name "*etags-grep*")


(defun etags-grep/find-tags (tag)
  (interactive (find-tag-interactive "Find tag: "))
  (let ((tags-info (etags-grep/generate-matched-tags-info tag)))
    (cond ((= (length tags-info) 0)
           (error "There is no tag to match"))
          ((= (length tags-info) 1)
           (etags-grep/goto-first-tag tags-info))
          ((> (length tags-info) 1)
           (etags-grep/grep-mode etags-grep/buf-name)
           (etags-grep/insert-tags etags-grep/buf-name tags-info)))))

(defun etags-grep/generate-matched-tags-info (tag)
  "Generate a matched tags info table"
  (let* ((tag-template "^\\(.*\\<%s\\>\\(\\s(\\|;\\)?\\)\^?\\(\\(.+\\)\^A\\)?\\([0-9]+\\),[0-9]+")
         (tag-regex (format tag-template tag))
         matched-tags-info)
    (dolist (tag-file (etags-grep/tag-files))
      (setq matched-tags-info (append matched-tags-info
                                      (etags-grep/generate-matched-tags-info-
                                       tag-file tag))))
    matched-tags-info))

(defun etags-grep/generate-matched-tags-info- (tag-file tag)
  (let (tags-info)
    (-when-let (tag-buf (get-file-buffer tag-file))
      (set-buffer tag-buf)
      (goto-char (point-min))
      (while (re-search-forward tag-regex nil t)
        (let* ((tag      (match-string-no-properties 1))
               (file     (etags-grep/get-file-name))
               (line-num (string-to-number (match-string-no-properties 5)))
               (meta     (match-string-no-properties 4))
               (tag-info (etags-grep/set-info
                          tag-file tag file line-num meta)))
          (add-to-list 'tags-info tag-info t))))
    tags-info))

(defun etags-grep/tag-files ()
  (eval-when-compile (require 'etags))
  (mapcar 'tags-expand-table-name tags-table-list))

(defun etags-grep/get-file-name ()
  (save-match-data
    (save-excursion
      (backward-page)
      (forward-line 1)
      (beginning-of-line)
      (re-search-forward "^\\(.+\\),"))
    (match-string-no-properties 1)))

(defun etags-grep/goto-first-tag (tags-info)
  (let* ((tag-info (first tags-info))
         (file     (etags-grep/get-info 'file tag-info))
         (line-num (etags-grep/get-info 'line-num tag-info)))
    (etags-grep/goto-line-of-file file line-num)))

(defun etags-grep/goto-line-of-file (file line &optional buf-display-fn)
  (let ((file (concat default-directory file)))
    (if (null buf-display-fn)
        (find-file file)
      (funcall buf-display-fn (find-file-noselect file))))
  (goto-line line))

(defun etags-grep/grep-mode (buf-name)
  (-when-let (buf (get-buffer buf-name))
    (kill-buffer buf))
  (with-current-buffer (get-buffer-create buf-name)
    (eval-when-compile (require 'grep))
    (grep-mode)
    (goto-char (point-min)))
  (pop-to-buffer etags-grep/buf-name))

(defun etags-grep/insert-tags (buf-name tags-info)
  (with-current-buffer (get-buffer-create buf-name)
    (let ((buffer-read-only nil))
      (setq default-directory
            (file-name-directory (etags-grep/get-info
                                  'tag-file (first tags-info))))

      (insert
       (format "-*- mode: grep; ""default-directory: \"%s\" -*-\n\n"
               default-directory))
      (insert (format "Find tag: \"%s\"\n" tag))
      (dolist (tag-info tags-info)
        (insert (format "./%s:" (etags-grep/get-info 'file tag-info)))
        (insert (format "%d: "  (etags-grep/get-info 'line-num tag-info)))
        (insert (format "%s\n"  (etags-grep/get-info 'tag tag-info)))))))


(defun etags-grep/set-info (tag-file tag file line-num meta)
  (list (etags-grep/info-element 'tag-file tag-file)
        (etags-grep/info-element 'tag      tag)
        (etags-grep/info-element 'file     file)
        (etags-grep/info-element 'line-num line-num)
        (etags-grep/info-element 'meta     meta)))

(defun etags-grep/info-element (k v) (list k v))
(defun etags-grep/get-info (k alist) (second (assoc k alist)))


(provide 'etags-grep)
