;;; ansi-color.el --- ANSI color configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for handling ANSI color codes in buffers.
;;
;;; Code:

(use-package ansi-color
  :config
  (defun my-ansi-color-apply ()
    "Interpret ANSI color escape sequences in the current buffer."
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max)))

  ;; Function to process log files
  (defun my-colorize-compilation-buffer ()
    (when (string-match-p "\\.log\\'" (or buffer-file-name ""))
      (read-only-mode -1)
      (my-ansi-color-apply)
      (read-only-mode 1)))

  ;; Automatically process log files when opened
  (add-hook 'find-file-hook 'my-colorize-compilation-buffer))

(provide 'mu/ansi-color)

;;; ansi-color.el ends here
