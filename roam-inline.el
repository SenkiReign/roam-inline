;;; roam-inline.el --- Inline backlinks for org-roam v2 -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "27.1") (org-roam "2.0"))

;;; Code:
;;; Version: 0.5
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

(defcustom roam-inline-anchor "#+ROAM_INLINE:"
  "Line after which the section is inserted.
Move this line to move the section.  If the buffer has no such line, the
section goes at the end of the buffer.  See `roam-inline-move-here'."
  :type 'string :group 'roam-inline)

(defcustom roam-inline-separator "\n-------\n\n"
  "Text inserted before backlinks and unlinked references.
A newline is appended if the text does not end with one.
Set to an empty string to omit the separator."
  :type 'string :group 'roam-inline)

(defvar-local roam-inline-show-unlinked nil)

(defun roam-inline--anchor-regexp ()
  (concat "^" (regexp-quote roam-inline-anchor) "[ \t]*$"))

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
        (add-hook 'after-revert-hook #'roam-inline-refresh nil t)
        (roam-inline-refresh))
    (remove-hook 'before-save-hook #'roam-inline--prune t)
    (remove-hook 'after-save-hook #'roam-inline-refresh t)
    (remove-hook 'after-revert-hook #'roam-inline-refresh t)
    (let ((modified (buffer-modified-p)))
      (roam-inline--prune)
      (set-buffer-modified-p modified))))

(defun roam-inline-refresh ()
  (interactive)
  (let ((modified (buffer-modified-p)))
    (roam-inline--prune)
    (roam-inline--insert)
    (set-buffer-modified-p modified)))

;; The section is found by text properties, not by markers.  Every generated
;; character carries `roam-inline-managed'; the first one also carries
;; `roam-inline-start'.  Pruning deletes every such run wherever it is, so a
;; copied, pasted or moved section can never be written to disk, and a buffer
;; revert can never leave stale positions that delete real text.

(defun roam-inline--prune ()
  "Delete every generated section in the buffer."
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (save-excursion
      (save-restriction
        (widen)
        (remove-overlays (point-min) (point-max) 'roam-inline t)
        (let ((pos (point-min)) start)
          (while (setq start (text-property-any pos (point-max) 'roam-inline-start t))
            (if (get-text-property start 'roam-inline-managed)
                (delete-region
                 start (or (next-single-property-change start 'roam-inline-managed)
                           (point-max)))
              (remove-text-properties start (1+ start) '(roam-inline-start nil)))
            (setq pos start)))))))

(defun roam-inline--goto-anchor ()
  "Move point to where the section goes.
Return non-nil when an anchor line was used."
  (goto-char (point-min))
  (if (re-search-forward (roam-inline--anchor-regexp) nil t)
      (progn (end-of-line) t)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    nil))

(defun roam-inline--seal (beg end)
  "Mark BEG..END as generated, read-only, and bind the section keymap."
  (add-text-properties beg end '(roam-inline-managed t
                                 read-only t
                                 rear-nonsticky (roam-inline-managed)))
  (add-text-properties beg (1+ beg) '(roam-inline-start t
                                      front-sticky (read-only)))
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'keymap roam-inline-map)
    (overlay-put ov 'roam-inline t)
    (overlay-put ov 'evaporate t)))

(defun roam-inline--insert ()
  (when-let* ((node (or (org-roam-node-at-point)
                        (progn (ignore-errors (org-roam-db-update-file (buffer-file-name)))
                               (org-roam-node-at-point)))))
    (let* ((backlinks (org-roam-backlinks-get node))
           (inhibit-read-only t)
           (buffer-undo-list t))
      (save-excursion
        (save-restriction
          (widen)
          (let* ((anchored (roam-inline--goto-anchor))
                 (beg (point)))
            ;; With an anchor, the section begins with the newline that ends
            ;; the anchor line, so prune restores the buffer exactly.
            (when anchored (insert "\n"))
            (unless (string-empty-p roam-inline-separator)
              (insert roam-inline-separator)
              (unless (string-suffix-p "\n" roam-inline-separator)
                (insert "\n")))
            (when backlinks
              (roam-inline--insert-backlinks backlinks))
            (roam-inline--insert-unlinked-section node)
            (roam-inline--seal beg (point))))))))

;;;###autoload
(defun roam-inline-move-here ()
  "Move the backlinks section to the start of the current line.
Writes the anchor line (`roam-inline-anchor') there and removes any other.
The section is generated, so move it with this command or by moving the
anchor line, not by cutting and pasting the section itself."
  (interactive)
  (roam-inline--prune)
  (let ((here (copy-marker (line-beginning-position))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (roam-inline--anchor-regexp) nil t)
        (delete-region (line-beginning-position)
                       (min (point-max) (1+ (line-end-position))))))
    (goto-char here)
    (insert roam-inline-anchor "\n")
    (set-marker here nil))
  (roam-inline-refresh))

;; Backlinks: one group per source node, one bullet per occurrence

(defun roam-inline--clean-links (text)
  "Replace org link syntax in TEXT with just its description/target."
  (let ((text (replace-regexp-in-string
               "\\[\\[[^]]*\\]\\[\\([^]]*\\)\\]\\]" "\\1" text)))
    (replace-regexp-in-string "\\[\\[\\([^]]*\\)\\]\\]" "\\1" text)))

(defun roam-inline--own-heading-p (node-id)
  "Non-nil if point is on a heading whose property drawer holds NODE-ID."
  (and (looking-at "\\*+ ")
       (save-excursion
         (forward-line 1)
         (and (looking-at "[ \t]*:PROPERTIES:")
              (let ((end (save-excursion (re-search-forward "^[ \t]*:END:" nil t))))
                (and end
                     (re-search-forward
                      (concat "^[ \t]*:ID:[ \t]+" (regexp-quote node-id) "[ \t]*$")
                      end t)))))))

(defun roam-inline--content (file point node-id)
  "Return a one-line preview for the link at POINT in FILE.
This is the line containing the link, minus list bullet or heading stars.
If that line is the heading of the source node itself (NODE-ID), the
group title already shows it, so return an empty string."
  (or (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char point)
          (beginning-of-line)
          (if (roam-inline--own-heading-p node-id)
              ""
            (let ((line (buffer-substring (line-beginning-position)
                                          (line-end-position))))
              (setq line (replace-regexp-in-string
                          "\\`[ \t]*\\(?:\\*+\\|[-+]\\|[0-9]+[.)]\\)[ \t]+" "" line))
              (truncate-string-to-width
               (string-trim (roam-inline--clean-links line))
               roam-inline-preview-length nil nil "…")))))
      ""))

(defun roam-inline--insert-backlink (backlink)
  "Insert one bullet for BACKLINK, unless it has nothing to preview."
  (let* ((src (org-roam-backlink-source-node backlink))
         (file (org-roam-node-file src))
         (pt (org-roam-backlink-point backlink))
         (text (roam-inline--content file pt (org-roam-node-id src)))
         (beg (point)))
    (unless (string-empty-p text)
      (insert (format "   - %s\n" text))
      (set-text-properties beg (point)
                           (list 'roam-inline-file file 'roam-inline-point pt)))))

(defun roam-inline--insert-backlinks (backlinks)
  "Insert BACKLINKS grouped by source node, one bullet per occurrence.
The group heading is also followable, so a link with no preview stays reachable."
  (insert (format "* Backlinks (%d)\n" (length backlinks)))
  (let ((groups (seq-group-by
                 (lambda (b) (org-roam-node-id (org-roam-backlink-source-node b)))
                 backlinks)))
    (dolist (g (seq-sort-by
                (lambda (g) (org-roam-node-title
                             (org-roam-backlink-source-node (cadr g))))
                #'string< groups))
      (let* ((bls (seq-sort-by #'org-roam-backlink-point #'< (cdr g)))
             (src (org-roam-backlink-source-node (car bls)))
             (beg (point)))
        (insert (format "** %s\n" (org-roam-node-title src)))
        (set-text-properties beg (point)
                             (list 'roam-inline-file (org-roam-node-file src)
                                   'roam-inline-point (org-roam-backlink-point (car bls))))
        (mapc #'roam-inline--insert-backlink bls))))
  (insert "\n"))

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
  (if (not (executable-find roam-inline-rg-executable))
      (insert "  (ripgrep not found)\n")
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
            (put-text-property beg (point) 'roam-inline-line (string-to-number line))))))))

(defun roam-inline--rg-search (term self-file)
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
