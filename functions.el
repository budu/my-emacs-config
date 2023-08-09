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
