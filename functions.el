;; Written in collaboration with ChatGPT 3.5, might need improvements
(defun mu/convert-region-to-percent-w-syntax (begin end)
  "Convert a region of text inside square brackets to Ruby's %w syntax and replace it."
  (interactive "r")
  (save-excursion
    (re-search-forward "\\]" nil t)
    (when (re-search-backward "\\[" nil t)
      (let* ((start (point))
             (end (re-search-forward "\\]" nil t))
             (region-text (buffer-substring-no-properties (+ start 1) (- end 1))))
        (delete-region start end)
        (setq region-text (replace-regexp-in-string "[ \t\n,]+" " " region-text))
        (setq region-text (replace-regexp-in-string "\\(\"\\)\\(.*?\\)\\(\"\\)" "\\2" region-text))
        (insert (concat "%w[" region-text "]"))))))

(defun mu/kmacro-start-or-end-macro (arg)
  (interactive "P")
  (if (or defining-kbd-macro executing-kbd-macro)
      (kmacro-end-macro arg)
    (kmacro-start-macro arg)))

(defun mu/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun mu/touch ()
  "updates mtime on the file for the current buffer"
  (interactive)
  (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))
  (clear-visited-file-modtime))

;; hardcoded for now
(defun mu/search-gems (search-term)
  (interactive (list (read-from-minibuffer "Search gems for: ")))
  (counsel-rg search-term
              "/home/budu/.rvm/gems/ruby-2.7.6@reservotron/gems/"
              "--no-ignore"))
