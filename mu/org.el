;;; package --- Org-mode helpers
;;; Commentary:
;;; Code:

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-<return>") 'mu/open-at-point)
  (define-key org-mode-map (kbd "M-<return>") 'org-insert-heading-respect-content)
  (define-key org-mode-map (kbd "M-S-<return>") 'org-insert-todo-heading-respect-content)
  (define-key org-mode-map (kbd "M-p") 'org-metaup)
  (define-key org-mode-map (kbd "M-n") 'org-metadown)
  (define-key org-mode-map (kbd "M-h") 'er/expand-region)
  (define-prefix-command 'mu/org-map)
  (define-key org-mode-map (kbd "C-c o") 'mu/org-map)
  (define-key mu/org-map (kbd "p") 'org-priority-up)
  (define-key mu/org-map (kbd "n") 'org-priority-down)
  (define-key mu/org-map (kbd "w") 'mu/org/copy-as-markdown)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (setq org-agenda-files '("~/cg/" "~/org/" "~/projects/"))
  (setq org-hide-emphasis-markers t)
  (setq org-tags-column -20)  ; Adjust the number to control tag position
  (setq org-auto-align-tags t)
  (setq org-tags-case-fold-search t)
  (setq org-todo-keywords
        '((sequence "TODO" "HOLD" "|" "DONE" "FAIL")))
  (setq org-todo-keyword-faces
        '(("HOLD" . (:foreground "orange" :weight bold))
          ("FAIL" . (:foreground "red" :weight bold))))
  (setq org-tag-faces
        '(("WIP" . (:foreground "yellow" :weight bold))
          ))
  (when org-tags-special-faces-re
    (setq org-tags-special-faces-re nil))
  (font-lock-add-keywords
   'org-mode '(("^\\*+ "
                ":" nil nil
                (0 (put-text-property (match-beginning 0) (match-end 0) 'display " ")))))
  (setq org-ellipsis "▼")
  ;; from https://zzamboni.org/post/beautifying-org-mode-in-emacs/
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (let* ((variable-tuple
          (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (headline           `(:inherit default :weight bold :foreground ,"white")))
    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple))))
     `(org-level-3 ((t (,@headline ,@variable-tuple))))
     `(org-level-2 ((t (,@headline ,@variable-tuple))))
     `(org-level-1 ((t (,@headline ,@variable-tuple))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 1.4 :underline nil))))))
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8
                           :background "#103b66"))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
  (setq org-startup-folded t)
  (require 'org-tempo)
  ;; Load org-babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  ;; Optional but recommended: Don't ask for confirmation when executing code blocks
  (setq org-confirm-babel-evaluate nil)
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(defun org-get-tag-face (kwd)
  "Get the right face for a TODO keyword KWD.
If KWD is a number, get the corresponding match group."
  (if (numberp kwd) (setq kwd (match-string kwd)))
  (let ((special-tag-face (or (cdr (assoc kwd org-tag-faces))
                              (and (string-match "^@.*" kwd)
                                   (cdr (assoc "@.*" org-tag-faces))))))
    (or (org-face-from-face-or-color 'tag 'org-tag special-tag-face)
        'org-tag)))

(defun mu/org/open-daily-note ()
  "Create and open a daily note for today."
  (interactive)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (file-name (concat "daily/" today "-daily.org")))
    (find-file (expand-file-name file-name org-directory))
    (when (= (buffer-size) 0)
      (insert (format "#+TITLE: Daily Note %s\n\n* Notes\n" today)))))

(defun mu/org/open-daily-note-at-point (orig-fun &rest args)
  "Advice to open daily notes with a custom function.

This function acts as an advice around ORIG-FUN, checking the link at the
current point in an org document.

If the link points to a daily note (`daily/.*-daily\\.org`), it
calls `mu/open-today-daily-note`.  Otherwise, it proceeds by
calling ORIG-FUN with ARGS."
  (let ((link (org-element-property :path (org-element-context))))
    (if (and link (string-match "daily/.*-daily\\.org" link))
        (mu/org/open-daily-note)
      (apply orig-fun args))))

(advice-add 'org-open-at-point
            :around
            #'mu/org/open-daily-note-at-point)

(defun mu/org-insert-example-block ()
  "Insert an example block at point."
  (interactive)
  (let ((start (point)))
    (insert "#+begin_example\n\n#+end_example")
    (goto-char (+ start 16))))

(defun mu/org-insert-src-block ()
  "Insert an example block at point."
  (interactive)
  (let ((start (point)))
    (insert "#+ATTR_LATEX: :options frame=single\n#+begin_src sh\n\n#+end_src")
    (goto-char (+ start 16))))

(add-hook 'org-mode-hook 'mu/org-fold-all-done-entries)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c c") 'mu/org-insert-src-block)
  (define-key org-mode-map (kbd "C-c e") 'mu/org-insert-example-block))

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(defun mu/org/surround-with-tilde ()
  "Surround the symbol at point or region with tildes."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert "~")
        (goto-char beg)
        (insert "~"))
    (when (thing-at-point 'symbol)
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (beg (car bounds))
             (end (cdr bounds)))
        (goto-char end)
        (insert "~")
        (goto-char beg)
        (insert "~")))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c `") #'mu/org/surround-with-tilde)) ;; replace org-table-edit-field

(defun mu/org/git-hash-to-org-link ()
  "Convert git hash at point into an org link"
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (hash (thing-at-point 'word t))
         (default-directory (if (string= (file-name-nondirectory
                                        (directory-file-name default-directory))
                                       "nb-notes")
                              (expand-file-name ".." default-directory)
                            default-directory))
         (repo-url (replace-regexp-in-string
                   "[\n\r]+$" ""
                   (shell-command-to-string "git config --get remote.origin.url")))
         (clean-url (replace-regexp-in-string "\\.git$" "" repo-url))
         (https-url (cond
                    ;; Convert SSH URL format (git@github.com:user/repo)
                    ((string-match "git@\\([^:]+\\):\\(.+\\)" clean-url)
                     (format "https://%s/%s"
                            (match-string 1 clean-url)
                            (match-string 2 clean-url)))
                    ;; Convert git:// format
                    ((string-match "git://\\(.+\\)" clean-url)
                     (format "https://%s" (match-string 1 clean-url)))
                    ;; Already HTTPS or HTTP
                    ((string-match "^https?://" clean-url) clean-url)
                    ;; Default case
                    (t clean-url)))
         (short-hash (substring hash 0 7))
         (org-link (format "[[%s/commit/%s][%s]]" https-url hash short-hash)))
    (goto-char (car bounds))
    (delete-region (car bounds) (cdr bounds))
    (insert org-link)))

(global-set-key (kbd "C-c l") 'mu/org/git-hash-to-org-link)

(defun mu/org/copy-as-markdown ()
  (interactive)
  (let ((org-export-with-toc nil)
        (wc (current-window-configuration)))
    (with-current-buffer (org-md-export-as-markdown)
      (clipboard-kill-region (point-min) (point-max))
      (kill-buffer))
    (set-window-configuration wc)
    (message "Copied as Markdown")))

(defun mu/org/auto-link--extract-real-url (ddg-url)
  "Extract the real URL from a DuckDuckGo redirect URL."
  (when (string-match "uddg=\\([^&]+\\)" ddg-url)
    (url-unhex-string (match-string 1 ddg-url))))

(defun mu/org/auto-link-dwim ()
  "Convert text to an org-mode link using DuckDuckGo search.
If region is active, use the region text.
Otherwise, use the word at point."
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'word)))
         (text (when bounds
                (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (search-url (when text
                      (concat "https://duckduckgo.com/html/?q="
                             (url-hexify-string text))))
         (url-buffer (url-retrieve-synchronously search-url))
         ddg-url
         url)
    (when (and text url-buffer)
      (with-current-buffer url-buffer
        (goto-char (point-min))
        (when (re-search-forward "class=\"result__url\" href=\"\\([^\"]+\\)\"" nil t)
          (setq ddg-url (match-string 1))
          (when ddg-url
            (setq url (mu/org/auto-link--extract-real-url ddg-url))))
        (kill-buffer))

      ;; Replace the text with org link if we found a URL
      (when (and url bounds)
        (delete-region (car bounds) (cdr bounds))
        (insert (format "[[%s][%s]]" url text))))))

; copy link with shr-maybe-probe-and-copy-url
(with-eval-after-load 'org
  (define-key mu/org-map (kbd "l") 'mu/org/auto-link-dwim))

;; from https://stackoverflow.com/questions/25161792
(defun mu/org/focus ()
  "Show next entry, keeping other entries closed."
  (interactive)
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-<tab>") 'mu/org/focus))

;; from https://claude.ai/chat/e8c83a90-3424-4873-a04f-17cd29545fae

;; (defun org-smart-paste ()
;;   "If there is an active region and the clipboard content is a URL,
;;    create an org-mode link with the region as description and the URL as target.
;;    Otherwise, perform a regular paste operation."
;;   (interactive)
;;   (let ((clipboard (current-kill 0))
;;         (url-regex "\\`\\(?:https?://\\|www\\.\\)\\(?:[-.[:alnum:]]+\\.[[:alpha:]]{2,}\\)?[[:graph:]]*"))
;;     (if (and (use-region-p)
;;              (string-match-p url-regex clipboard))
;;         (let* ((region-text (buffer-substring-no-properties
;;                             (region-beginning)
;;                             (region-end)))
;;                (link-text (format "[[%s][%s]]"
;;                                 clipboard
;;                                 region-text)))
;;           (delete-region (region-beginning) (region-end))
;;           (insert link-text))
;;       (yank))))

;; ;; Bind the function to C-y in org-mode
;; (with-eval-after-load 'org
;;   (define-key org-mode-map (kbd "C-y") 'org-smart-paste))

;; If you prefer to keep C-y as the default yank and use a different key binding:
;; (with-eval-after-load 'org
;;   (define-key org-mode-map (kbd "C-c C-y") 'org-smart-paste))

;;; org.el ends here
