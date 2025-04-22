;;; package --- Helpers for Ruby
;;; Commentary:
;;; Code:

(defun mu/ruby/find-definition ()
  "Search for Ruby method, class, or module definition at point using project.el and ripgrep.
If only one result is found, jumps directly to it. Otherwise shows results in ivy."
  (interactive)
  (when-let* ((symbol-name (thing-at-point 'symbol))
              (proj (project-current t))
              (root (project-root proj))
              (cmd (format "rg --line-number '^\\s*(def\\s+%s|class\\s+%s|module\\s+%s)' %s"
                          (shell-quote-argument symbol-name)
                          (shell-quote-argument symbol-name)
                          (shell-quote-argument symbol-name)
                          (shell-quote-argument root)))
              (output (shell-command-to-string cmd))
              (results (split-string output "\n" t)))
    (pcase (length results)
      (0 (message "No definition found for: %s" symbol-name))
      (1 (let* ((parts (split-string (car results) ":"))
                (file (car parts))
                (line (string-to-number (cadr parts))))
           (find-file (expand-file-name file root))
           (goto-char (point-min))
           (forward-line (1- line))))
      (_ (ivy-read (format "Find definition of '%s': " symbol-name)
                   (mapcar (lambda (result)
                            (let* ((parts (split-string result ":"))
                                  (file (car parts))
                                  (line (cadr parts))
                                  (preview (string-join (cddr parts) ":")))
                              (propertize
                               (format "%s:%s: %s"
                                      (file-relative-name file root)
                                      line
                                      (string-trim preview))
                               'file file
                               'line (string-to-number line))))
                           results)
                   :action (lambda (x)
                            (find-file (expand-file-name
                                      (get-text-property 0 'file x)
                                      root))
                            (goto-char (point-min))
                            (forward-line (1- (get-text-property 0 'line x)))))))))

(defun mu/ruby/find-references ()
  "Search for all references to the Ruby symbol at point using project.el and ripgrep.
Shows results in ivy for selection."
  (interactive)
  (when-let* ((symbol-name (thing-at-point 'symbol))
              (proj (project-current t))
              (root (project-root proj))
              ;; Search for the symbol with word boundaries to avoid partial matches
              ;; Exclude def/class/module definitions to avoid overlap with find-definition
              (cmd (format "rg --line-number '[\\\\b.:]%s' --type ruby -g '!{spec,test}/**' %s"
                          (shell-quote-argument symbol-name)
                          (shell-quote-argument root)))
              (output (shell-command-to-string cmd))
              (results (split-string output "\n" t)))
    (if (null results)
        (message "No references found for: %s" symbol-name)
      (ivy-read (format "References to '%s': " symbol-name)
                (mapcar (lambda (result)
                         (let* ((parts (split-string result ":"))
                                (file (car parts))
                                (line (cadr parts))
                                (preview (string-join (cddr parts) ":")))
                           (propertize
                            (format "%s:%s: %s"
                                    (file-relative-name file root)
                                    line
                                    (string-trim preview))
                            'file file
                            'line (string-to-number line))))
                        results)
                :action (lambda (x)
                         (find-file (expand-file-name
                                   (get-text-property 0 'file x)
                                   root))
                         (goto-char (point-min))
                         (forward-line (1- (get-text-property 0 'line x))))))))

(defun rspec-spec-file-for (a-file-name)
  "Find spec for the specified file."
  (if (rspec-spec-file-p a-file-name)
      a-file-name
    (let* ((is-lib-file (string-match "/lib/" a-file-name))
           (replace-regex (cond
                           (is-lib-file "/lib/")
                           ((rspec-target-in-holder-dir-p a-file-name) "^\\.\\./[^/]+/")
                           (t "^\\.\\../")))
           (replacement (if is-lib-file "/spec/lib/" ""))
           (relative-file-name (file-relative-name a-file-name (rspec-spec-directory a-file-name))))
      (if is-lib-file
          (rspec-specize-file-name
           (replace-regexp-in-string replace-regex replacement a-file-name))
        (rspec-specize-file-name
         (expand-file-name
          (replace-regexp-in-string replace-regex "" relative-file-name)
          (rspec-spec-directory a-file-name)))))))

(defvar rubocop-cop-docs-base-urls
  '(("FactoryBot" . "https://docs.rubocop.org/rubocop-factory_bot/cops_factorybot.html")
    ("Performance" . "https://docs.rubocop.org/rubocop-performance/cops_performance.html")
    ("Rails" . "https://docs.rubocop.org/rubocop-rails/cops_rails.html")
    ("RSpec" . "https://docs.rubocop.org/rubocop-rspec/cops_rspec.html")
    ;; Default RuboCop docs, when no prefix matches
    ("default" . "https://docs.rubocop.org/rubocop/cops_%s.html"))
  "Mapping of RuboCop cop categories/plugins to documentation base URLs.")

(defun rubocop--cop-docs-url (category cop-name)
  "Build the documentation URL for CATEGORY and COP-NAME."
  (let* ((base-url (or (cdr (assoc category rubocop-cop-docs-base-urls))
                       (format (cdr (assoc "default" rubocop-cop-docs-base-urls))
                               (downcase category))))
         (anchor (downcase (format "%s/%s" category cop-name)))
         (anchor (replace-regexp-in-string "[^a-z0-9]+" "_" anchor)))
    (concat base-url "#" anchor)))

(defun rubocop-open-documentation-at-point ()
  "Open the RuboCop documentation for the cop at point.
Automatically handles main and plugin RuboCop cops."
  (interactive)
  (let* ((cop-at-point (rubocop-get-cop-at-point))
         (parts (split-string cop-at-point "/"))
         (category (car parts))
         (cop-name (cadr parts))
         (url (rubocop--cop-docs-url category cop-name)))
    (browse-url url)))

(defun rubocop-get-cop-at-point ()
  "Get the RuboCop cop name from Flycheck error at point.
First tries Flycheck overlays, then scans current line, and prompts as fallback."
  (or
   ;; First Attempt: check flycheck errors at point
   (when (and (bound-and-true-p flycheck-mode))
     (let ((errors (flycheck-overlay-errors-at (point))))
       (when errors
         (let* ((error (car errors))
                (id (flycheck-error-id error)))
           (when (and id
                      (string-match "\\([A-Za-z]+/[A-Za-z]+\\)" id))
             (match-string 1 id))))))

   ;; Second Attempt (Fallback): scan current line for cop pattern
   (let ((line (thing-at-point 'line t)))
     (when (and line
                (string-match "\\([A-Za-z]+/[A-Za-z]+\\)" line))
       (match-string 1 line)))

   ;; Final Fallback: prompt user manually
   (read-string "Cop name (e.g. Layout/SpaceAroundOperators): ")))

(eval-after-load 'rubocop
  '(let ((prefix-map (lookup-key rubocop-mode-map rubocop-keymap-prefix)))
     (define-key prefix-map (kbd "w") 'rubocop-open-documentation-at-point)))

;;; ruby.el ends here
