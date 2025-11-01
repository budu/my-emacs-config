;;; expand-region.el --- Expand region configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for expand-region package for smart region expansion.
;;
;;; Code:

(use-package expand-region
  :bind (("M-h" . er/expand-region))
  :config
  (defun er/add-ruby-mode-expansions ()
    "Adds Ruby-specific expansions for buffers in ruby-mode"
    (set (make-local-variable 'er/try-expand-list)
         (remove 'er/mark-defun
                 (append
                  (default-value 'er/try-expand-list)
                  '(er/mark-ruby-instance-variable
                    er/c-mark-fully-qualified-name
                    er/mark-ruby-block-up
                    er/mark-ruby-heredoc)))))
  (er/enable-mode-expansions 'ruby-mode #'er/add-ruby-mode-expansions))

(provide 'mu/expand-region)

;;; expand-region.el ends here
