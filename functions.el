(defun ellama-translate-en ()
  "Ask ellama to translate selected region or word at point to English."
  (interactive)
  (if (region-active-p)
      (ellama-instant
       (format "Translate the following text to English, please keep any formatting and markup as is:\n%s"
	       (buffer-substring-no-properties (region-beginning) (region-end))))
    (ellama-instant
     (format "Translate %s to English" (thing-at-point 'word)))))

(defun ellama-translate-fr ()
  "Ask ellama to translate selected region or word at point to French."
  (interactive)
  (if (region-active-p)
      (ellama-instant
       (format "Translate the following text to French, please keep any formatting and markup as is:\n%s"
	       (buffer-substring-no-properties (region-beginning) (region-end))))
    (ellama-instant
     (format "Translate %s to French" (thing-at-point 'word)))))

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
        (setq region-text (replace-regexp-in-string "[ \t,]+" " " region-text))
        (setq region-text (replace-regexp-in-string "\\(\"\\)\\(.*?\\)\\(\"\\)" "\\2" region-text))
        (insert (concat "%w[" region-text "]"))))))

;; From SO with improvements made using ChatGPT 4
(defun mu/cslist-to-indented-lines-and-back (start end &optional arg)
  (interactive "r\nP")
  (unless (use-region-p)
    (call-interactively 'er/expand-region 0)
    (when (<= start end)
      (call-interactively 'er/mark-inside-pairs 2))
    (setq start (region-beginning))
    (setq end (region-end)))
  (if (string-match-p "\n" (buffer-substring start end))
      (let ((insertion
             (mapconcat
              (lambda (x) (string-trim x))
              (split-string (buffer-substring start end) "\n") " ")))
        (delete-region start end)
        (insert insertion)
        (indent-region start end)
        (when arg (forward-char (length insertion))))
      (let ((insertion
             (mapconcat
              (lambda (x) (string-trim x))
              (split-string (buffer-substring start end) ",") ",\C-j")))
        (delete-region start end)
        (insert insertion)
        (indent-region start end)
        (when arg (forward-char (length insertion))))))

(defun mu/kmacro-start-or-end-macro (arg)
  (interactive "P")
  (if (or defining-kbd-macro executing-kbd-macro)
      (kmacro-end-macro arg)
    (kmacro-start-macro arg)))

(defun mu/open-at-point ()
  "Open the thing at point, if possible.
   If the thing at point looks like a commit hash, open the
   corresponding commit in Magit. If it looks like a URL, open it in
   the browser."
  (interactive)
  (let ((thing (thing-at-point 'url)))
    (if thing (browse-url-at-point)
      (let ((thing (thing-at-point 'symbol)))
        (cond ((string-match-p "^[0-9a-f]\\{7,20\\}$" thing)
               (magit-show-commit thing))
              ((string-match-p "^[0-9a-f]\\{21,41\\}$" thing)
               (browse-url (format "https://github.com/codegenome/reservotron/commit/%s" thing)))
              (t (message "Nothing to open at point for; %s" thing)))))))

(defun mu/sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
   Prefixed with negative \\[universal-argument], sorts in reverse.

   The variable `sort-fold-case' determines whether alphabetic case
   affects the sort order.

   See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

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
              "/home/budu/.asdf/installs/ruby/2.7.6/lib/ruby/gems/2.7.0/gems"
              "--no-ignore"))

;; wrap region in quotes using Ruby string interpolation
(defun mu/wrap-in-interpolated-quotes ()
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert "}\"")
    (goto-char start)
    (insert "\"#{")
    (goto-char (+ start 1))))
