;;; roam-inline.el --- Inline backlinks for org-roam v2 -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "27.1") (org-roam "2.0"))

;;; Code:

(require 'org-roam)
(require 'org-roam-mode)
(require 'seq)

(defgroup roam-inline nil "Inline org-roam backlinks." :group 'org-roam)

(defcustom roam-inline-preview-length 200
  "Max chars shown per backlink preview."
  :type 'integer :group 'roam-inline)

(defcustom roam-inline-rg-executable "rg"
  "Ripgrep binary for unlinked references."
  :type 'string :group 'roam-inline)

(defcustom roam-inline-ignore-files nil
  "Filenames or regexps to exclude from `roam-inline-mode'.
Matched against the buffer's full file path.
Example: (setq roam-inline-ignore-files \\='(\"fleeting\\\\.org\\\\'\"))"
  :type '(repeat string) :group 'roam-inline)

(defvar-local roam-inline--beg nil)
(defvar-local roam-inline--end nil)
(defvar-local roam-inline-show-unlinked nil)

(defvar roam-inline-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'roam-inline-follow)
    (define-key m (kbd "C-c C-c") #'roam-inline-refresh)
    m))

;;;###autoload
(defun roam-inline-auto-enable ()
  (when (and buffer-file-name
             org-roam-directory
             (file-in-directory-p buffer-file-name
                                   (expand-file-name org-roam-directory))
             (not (seq-some (lambda (pat) (string-match-p pat buffer-file-name))
                             roam-inline-ignore-files)))
    (roam-inline-mode 1)))

;;;###autoload
(define-minor-mode roam-inline-mode
  "Show org-roam backlinks and unlinked references inline."
  :lighter " roam"
  (if roam-inline-mode
      (progn
        (add-hook 'before-save-hook #'roam-inline--prune nil t)
        (add-hook 'after-save-hook #'roam-inline-refresh nil t)
        (roam-inline-refresh))
    (remove-hook 'before-save-hook #'roam-inline--prune t)
    (remove-hook 'after-save-hook #'roam-inline-refresh t)
    (roam-inline--prune)))

(defun roam-inline-refresh ()
  (interactive)
  (let ((modified (buffer-modified-p)))
    (roam-inline--prune)
    (roam-inline--insert)
    (set-buffer-modified-p modified)))

(defun roam-inline--prune ()
  (when (and roam-inline--beg roam-inline--end)
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (save-excursion (delete-region roam-inline--beg roam-inline--end)))
    (setq roam-inline--beg nil roam-inline--end nil)))

(defun roam-inline--insert ()
  (when-let* ((node (or (org-roam-node-at-point)
                        (progn (ignore-errors (org-roam-db-update-file (buffer-file-name)))
                               (org-roam-node-at-point)))))
    (let* ((backlinks (org-roam-backlinks-get node :unique t))
           (inhibit-read-only t)
           (buffer-undo-list t))
      (save-excursion
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (let ((beg (point)))
          (when backlinks
            (insert (format "* Backlinks (%d)\n" (length backlinks)))
            (dolist (bl (seq-sort-by (lambda (b) (org-roam-node-title
                                                   (org-roam-backlink-source-node b)))
                                      #'string< backlinks))
              (roam-inline--insert-backlink bl))
            (insert "\n"))
          (roam-inline--insert-unlinked-section node)
          (put-text-property beg (point) 'read-only t)
          (put-text-property beg (1+ beg) 'front-sticky '(read-only))
          (overlay-put (make-overlay beg (point)) 'keymap roam-inline-map)
          (setq roam-inline--beg (copy-marker beg))
          (setq roam-inline--end (copy-marker (point) t)))))))

(defun roam-inline--insert-backlink (backlink)
  (let* ((src (org-roam-backlink-source-node backlink))
         (file (org-roam-node-file src))
         (pt (org-roam-backlink-point backlink))
         (beg (point)))
    (insert (format "** %s\n   %s\n"
                     (org-roam-node-title src)
                     (roam-inline--content file pt)))
    (set-text-properties beg (point)
                          (list 'roam-inline-file file 'roam-inline-point pt))))

(defconst roam-inline--drawer-re
  "^[ \t]*:[A-Za-z_-]+:\n\\(?:.*\n\\)*?[ \t]*:END:\n?"
  "Matches a :PROPERTIES:/:LOGBOOK:/etc drawer block.")

(defun roam-inline--content (file point)
  (or (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char point)
          (let* ((heading-beg (save-excursion
                                 (if (re-search-backward "^\\*+ " nil t) (point) (point-min))))
                 (heading-end (save-excursion
                                (if (re-search-forward "^\\*+ " nil t) (match-beginning 0) (point-max))))
                 beg end raw clean)
            (goto-char point) (backward-sentence)
            (setq beg (max (point) heading-beg))
            (goto-char point) (forward-sentence)
            (setq end (min (point) heading-end))
            (setq raw (buffer-substring (min beg end) (max beg end)))
            (setq clean (replace-regexp-in-string roam-inline--drawer-re "" raw))
            (truncate-string-to-width
             (replace-regexp-in-string "\n+" " " (string-trim clean))
             roam-inline-preview-length nil nil "…"))))
      ""))

;; Unlinked references, via ripgrep

(defun roam-inline--insert-unlinked-section (node)
  (insert "* Unlinked references\n")
  (if roam-inline-show-unlinked
      (roam-inline--insert-unlinked-refs node)
    (let ((beg (point)))
      (insert "[Show unlinked references]")
      (put-text-property beg (point) 'roam-inline-toggle t)
      (insert "\n"))))

(defun roam-inline-unlinked-toggle ()
  (interactive)
  (when (get-text-property (point) 'roam-inline-toggle)
    (setq roam-inline-show-unlinked t)
    (roam-inline-refresh)))

(defun roam-inline--insert-unlinked-refs (node)
  (let* ((terms (cons (org-roam-node-title node) (org-roam-node-aliases node)))
         (self-file (org-roam-node-file node))
         (hits (seq-mapcat (lambda (term) (roam-inline--rg-search term self-file)) terms)))
    (if (null hits)
        (insert "  (none)\n")
      (pcase-dolist (`(,file ,line ,text) hits)
        (let ((beg (point)))
          (insert (format "- %s:%s: %s\n"
                           (file-name-nondirectory file) line (string-trim text)))
          (put-text-property beg (point) 'roam-inline-file file)
          (put-text-property beg (point) 'roam-inline-line (string-to-number line)))))))

(defun roam-inline--rg-search (term self-file)
  (unless (executable-find roam-inline-rg-executable)
    (user-error "ripgrep not found"))
  (with-temp-buffer
    (call-process roam-inline-rg-executable nil t nil
                  "--line-number" "--no-heading" "--fixed-strings" "--word-regexp"
                  "--ignore-case" "--glob" "*.org" "--"
                  term (expand-file-name org-roam-directory))
    (goto-char (point-min))
    (let (results)
      (while (re-search-forward "^\\(.*?\\):\\([0-9]+\\):\\(.*\\)$" nil t)
        (let ((file (match-string 1)) (line (match-string 2)) (text (match-string 3)))
          (unless (or (string= (expand-file-name file) (expand-file-name self-file))
                      (string-match-p "\\[\\[" text))
            (push (list file line text) results))))
      (nreverse results))))

(defun roam-inline-follow ()
  (interactive)
  (cond
   ((get-text-property (point) 'roam-inline-toggle) (roam-inline-unlinked-toggle))
   ((get-text-property (point) 'roam-inline-file)
    (let ((file (get-text-property (point) 'roam-inline-file))
          (pt (get-text-property (point) 'roam-inline-point))
          (line (get-text-property (point) 'roam-inline-line)))
      (find-file file)
      (cond (pt (goto-char pt)) (line (goto-char (point-min)) (forward-line (1- line))))
      (org-fold-show-context)))))

(provide 'roam-inline)
;;; roam-inline.el ends here
