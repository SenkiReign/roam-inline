;;; roam-inline.el --- Inline backlinks for org-roam v2 -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "27.1") (org-roam "2.0"))

;;; Code:
;;; Version: 0.4
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
    (let* ((backlinks (org-roam-backlinks-get node :unique t))
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
              (insert (format "* Backlinks (%d)\n" (length backlinks)))
              (dolist (bl (seq-sort-by (lambda (b) (org-roam-node-title
                                                     (org-roam-backlink-source-node b)))
                                       #'string< backlinks))
                (roam-inline--insert-backlink bl))
              (insert "\n"))
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

(defun roam-inline--clean-links (text)
  "Replace org link syntax in TEXT with just its description/target."
  (let ((text (replace-regexp-in-string
               "\\[\\[[^]]*\\]\\[\\([^]]*\\)\\]\\]" "\\1" text)))
    (replace-regexp-in-string "\\[\\[\\([^]]*\\)\\]\\]" "\\1" text)))

(defconst roam-inline--drawer-re
  "^[ \t]*:[A-Za-z_-]+:\n\\(?:.*\n\\)*?[ \t]*:END:\n?"
  "Matches a :PROPERTIES:/:LOGBOOK:/etc drawer block.")

(defun roam-inline--content (file point)
  (or (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char point)
          (let* ((on-heading (progn (beginning-of-line) (looking-at "\\*+ ")))
                 (heading-beg (if on-heading (point)
                                 (save-excursion
                                   (if (re-search-backward "^\\*+ " nil t) (point) (point-min)))))
                 (heading-end (save-excursion
                                (goto-char point) (end-of-line)
                                (if (re-search-forward "^\\*+ " nil t) (match-beginning 0) (point-max))))
                 beg end raw clean)
            (if on-heading
                ;; link is the heading itself: use the entry's body text, not the title
                (progn
                  (goto-char heading-beg) (forward-line 1)
                  (when (looking-at "[ \t]*:PROPERTIES:")
                    (re-search-forward "^[ \t]*:END:" heading-end t)
                    (forward-line 1))
                  (skip-chars-forward " \t\n" heading-end)
                  (setq beg (point))
                  (forward-sentence)
                  (setq end (min (point) heading-end)))
              (progn
                (goto-char point) (backward-sentence)
                (setq beg (max (point) heading-beg))
                (goto-char point) (forward-sentence)
                (setq end (min (point) heading-end))))
            (setq raw (buffer-substring (min beg end) (max beg end)))
            (setq clean (replace-regexp-in-string roam-inline--drawer-re "" raw))
            (setq clean (roam-inline--clean-links clean))
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
