;;; package --- Org-mode helpers
;;; Commentary:
;;; Code:

(defun mu/org/open-daily-note ()
  "Create and open a daily note for today."
  (interactive)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (file-name (concat "daily/" today "-daily.org")))
    (find-file (expand-file-name file-name org-directory))
    (when (= (buffer-size) 0)
      (insert (format "#+TITLE: Daily Note %s\n\n* Summary\n\n* Notes\n\n* Tasks\n" today)))))

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
  (define-key org-mode-map (kbd "C-c c") 'my/org-insert-src-block)
  (define-key org-mode-map (kbd "C-c e") 'my/org-insert-example-block))

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
