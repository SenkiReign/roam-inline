;;; roam-inline.el --- Inline backlinks for org-roam v2 -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "27.1") (org-roam "2.0"))
;; Version: 0.7.1
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

(defcustom roam-inline-anchor "#+ROAM_INLINE:"
  "Line after which the section is inserted.
Move this line to move the section.  If the buffer has no such line, the
section goes at the end of the buffer.  See `roam-inline-move-here'."
  :type 'string :group 'roam-inline)

(defcustom roam-inline-separator "\n\n-------\n\n"
  "Text inserted before backlinks and unlinked references.
A newline is appended if the text does not end with one.
Set to an empty string to omit the separator."
  :type 'string :group 'roam-inline)

(defcustom roam-inline-show-without-backlinks nil
  "Non-nil means show the section even for nodes that have no backlinks.
The section then holds only the unlinked references prompt.  By default a
node that nothing links to gets no section at all, not even the separator."
  :type 'boolean :group 'roam-inline)

(defcustom roam-inline-show-parent t
  "Non-nil means prefix each backlink with the heading it sits under.
For a link in a body line or list item this is the closest heading above
it.  For a link on a heading line it is the closest shallower heading.
Nothing is shown when that heading is the source node's own heading (the
group title already shows it) or when the link is above the first heading."
  :type 'boolean :group 'roam-inline)

(defface roam-inline-parent '((t :inherit shadow))
  "Face for the parent heading shown before a backlink."
  :group 'roam-inline)

(defvar-local roam-inline-show-unlinked nil)

(defvar roam-inline--file-cache nil
  "Hash table of FILE -> buffer holding its contents, bound during a refresh.
Each source file is read from disk once per refresh, however many
backlinks point into it.")

(defun roam-inline--anchor-regexp ()
  (concat "^" (regexp-quote roam-inline-anchor) "[ \t]*$"))

(defvar roam-inline-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'roam-inline-follow)
    (define-key m [mouse-2] #'roam-inline-follow-mouse)
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

(defun roam-inline--seal (beg end anchored)
  "Mark BEG..END as generated, read-only, and bind the section keymap.
When ANCHORED, BEG is the end of the anchor line, so insertion at BEG is
blocked to protect that line.  Otherwise BEG starts a line of its own and
stays editable, so the user can type on the last line before the section."
  (add-text-properties beg end '(roam-inline-managed t
                                 read-only t
                                 rear-nonsticky (roam-inline-managed)))
  (add-text-properties beg (1+ beg)
                       (if anchored
                           '(roam-inline-start t front-sticky (read-only))
                         '(roam-inline-start t)))
  ;; The overlay starts one char after BEG, so the section keymap (RET ->
  ;; `roam-inline-follow') is not active when point is at BEG, where the user
  ;; types.  The keymap is looked up by the character at point, so this holds
  ;; whatever the overlay's front-advance setting.
  (let ((ov (make-overlay (1+ beg) end)))
    (overlay-put ov 'keymap roam-inline-map)
    (overlay-put ov 'roam-inline t)
    (overlay-put ov 'evaporate t)))

(defun roam-inline--insert ()
  (when-let* ((node (or (org-roam-node-at-point)
                        (progn (ignore-errors (org-roam-db-update-file (buffer-file-name)))
                               (org-roam-node-at-point)))))
    (let ((backlinks (org-roam-backlinks-get node)))
      ;; No backlinks: add no section at all, unless the user opted in.
      (when (or backlinks roam-inline-show-without-backlinks)
        (let* ((inhibit-read-only t)
               (buffer-undo-list t)
               (roam-inline--file-cache (make-hash-table :test #'equal)))
          (unwind-protect
              (save-excursion
                (save-restriction
                  (widen)
                  (let* ((anchored (roam-inline--goto-anchor))
                         (beg (point)))
                    ;; With an anchor, the section begins with the newline that
                    ;; ends the anchor line, so prune restores the buffer exactly.
                    (when anchored (insert "\n"))
                    (unless (string-empty-p roam-inline-separator)
                      (insert roam-inline-separator)
                      (unless (string-suffix-p "\n" roam-inline-separator)
                        (insert "\n")))
                    (when backlinks
                      (roam-inline--insert-backlinks backlinks))
                    (roam-inline--insert-unlinked-section node)
                    (roam-inline--seal beg (point) anchored))))
            (maphash (lambda (_file buf) (when (buffer-live-p buf) (kill-buffer buf)))
                     roam-inline--file-cache)))))))

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

;; Links in previews are NOT inserted as real org link syntax.  The buffer is
;; parsed by org-roam's db sync, and `[[...]]' text in the generated section
;; could be indexed as outgoing links of this node.  Instead the description
;; is shown with link styling and the original link is kept in the
;; `roam-inline-link' text property, to be opened on RET or mouse click.

(defconst roam-inline--link-regexp
  "\\[\\[\\([^]]+\\)\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]"
  "Match an org bracket link.  Group 1 is the target, group 2 the description.")

(defun roam-inline--plain (s face)
  "Return S, styled with FACE when FACE is non-nil."
  (if (and face (not (string-empty-p s)))
      (propertize s 'face face 'font-lock-face face)
    s))

(defun roam-inline--linkify (text &optional face)
  "Return TEXT with each org link replaced by a clickable description.
Text outside links gets FACE when it is non-nil.  Both `face' and
`font-lock-face' are set, because font-lock strips `face' on refontify."
  (let ((pos 0) out)
    (while (string-match roam-inline--link-regexp text pos)
      (let* ((raw (match-string 0 text))
             (target (match-string 1 text))
             (desc (match-string 2 text))
             (shown (if (or (null desc) (string-empty-p desc)) target desc)))
        (push (roam-inline--plain (substring text pos (match-beginning 0)) face) out)
        (push (propertize shown
                          'face 'org-link
                          'font-lock-face 'org-link
                          'mouse-face 'highlight
                          'follow-link t
                          'help-echo target
                          'roam-inline-link raw)
              out)
        (setq pos (match-end 0))))
    (push (roam-inline--plain (substring text pos) face) out)
    (apply #'concat (nreverse out))))

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

(defun roam-inline--parent-heading (level node-id)
  "Return the title of the heading that contains the line at point, or nil.
Point must be at the beginning of the line.  LEVEL is the star count if
that line is itself a heading, else nil; then only a shallower heading
counts.  Return nil if the heading found is the source node's own heading
\(NODE-ID), because the group title already shows it, or if it is blank."
  (save-excursion
    (let (done title)
      (while (and (not done)
                  (re-search-backward "^\\(\\*+\\) +\\(.*\\)$" nil t))
        (when (or (null level) (< (length (match-string 1)) level))
          (setq done t)
          (let ((text (match-string 2)))
            (unless (or (string-blank-p text)
                        (roam-inline--own-heading-p node-id))
              (setq title text)))))
      title)))

(defun roam-inline--source-buffer (file)
  "Return a cached buffer holding the contents of FILE.
Must be called while `roam-inline--file-cache' is bound."
  (or (gethash file roam-inline--file-cache)
      (let ((buf (generate-new-buffer " *roam-inline-src*")))
        (condition-case err
            (with-current-buffer buf (insert-file-contents file))
          (error (kill-buffer buf) (signal (car err) (cdr err))))
        (puthash file buf roam-inline--file-cache))))

(defun roam-inline--preview (text &optional face)
  "Linkify TEXT, trim it and truncate to the preview length.
Text outside links gets FACE when it is non-nil."
  (truncate-string-to-width
   (string-trim (roam-inline--linkify text face))
   roam-inline-preview-length nil nil "…"))

(defun roam-inline--subheadings (level)
  "Return the subheadings below point as indented bullet lines.
LEVEL is the star count of the heading at point.  Each line is prefixed
with a newline so the result can be appended to a bullet."
  (end-of-line)
  (let (out)
    (while (and (re-search-forward "^\\(\\*+\\) +\\(.*\\)$" nil t)
                (> (length (match-string 1)) level))
      (push (format "\n     %s- %s"
                    (make-string (* 2 (- (length (match-string 1)) level 1)) ?\s)
                    (roam-inline--preview (match-string 2)))
            out))
    (apply #'concat (nreverse out))))

(defun roam-inline--content (file point node-id)
  "Return a preview for the link at POINT in FILE.
This is the line containing the link, minus list bullet or heading stars,
followed by the subheadings if that line is a heading.  With
`roam-inline-show-parent' it is prefixed by the heading the line sits under.
If that line is the heading of the source node itself (NODE-ID), the group
title already shows it, so return an empty string."
  (or (ignore-errors
        (with-current-buffer (roam-inline--source-buffer file)
          (save-excursion
            (goto-char point)
            (beginning-of-line)
            (if (roam-inline--own-heading-p node-id)
                ""
              (let* ((level (and (looking-at "\\*+ ") (1- (length (match-string 0)))))
                     (line (buffer-substring (line-beginning-position)
                                             (line-end-position)))
                     (parent (and roam-inline-show-parent
                                  (roam-inline--parent-heading level node-id))))
                (setq line (replace-regexp-in-string
                            "\\`[ \t]*\\(?:\\*+\\|[-+]\\|[0-9]+[.)]\\)[ \t]+" "" line))
                (concat (and parent
                             (concat (roam-inline--preview parent 'roam-inline-parent)
                                     (propertize " › "
                                                 'face 'roam-inline-parent
                                                 'font-lock-face 'roam-inline-parent)))
                        (roam-inline--preview line)
                        (and level (roam-inline--subheadings level))))))))
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
      ;; add, don't set: the link properties inside TEXT must survive.
      (add-text-properties beg (point)
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

(defun roam-inline--open-link ()
  "Open the org link stored at point.
Relative file links resolve against the source file's directory."
  (let* ((raw (get-text-property (point) 'roam-inline-link))
         (file (get-text-property (point) 'roam-inline-file))
         (default-directory (if file (file-name-directory file) default-directory)))
    (org-link-open-from-string raw)))

(defun roam-inline-follow ()
  (interactive)
  (cond
   ((get-text-property (point) 'roam-inline-toggle) (roam-inline-unlinked-toggle))
   ((get-text-property (point) 'roam-inline-link) (roam-inline--open-link))
   ((get-text-property (point) 'roam-inline-file)
    (let ((file (get-text-property (point) 'roam-inline-file))
          (pt (get-text-property (point) 'roam-inline-point))
          (line (get-text-property (point) 'roam-inline-line)))
      (find-file file)
      (cond (pt (goto-char pt)) (line (goto-char (point-min)) (forward-line (1- line))))
      (org-fold-show-context)))))

(defun roam-inline-follow-mouse (event)
  "Move point to EVENT, then follow what is there."
  (interactive "e")
  (mouse-set-point event)
  (roam-inline-follow))

(provide 'roam-inline)
;;; roam-inline.el ends here
