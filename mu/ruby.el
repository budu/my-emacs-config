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

(global-set-key (kbd "M-.") 'mu/ruby/find-definition)

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

(global-set-key (kbd "C-M-.") 'mu/ruby/find-references)
