;;; markdown.el --- Markdown configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for markdown-mode.
;;
;;; Code:

;; fix markdown command is not found
(use-package markdown-mode
  :bind (:map markdown-mode-map
              ("C-M-b" . markdown-backward-paragraph)
              ("C-M-f" . markdown-forward-paragraph)
              ("C-c l" . mu/md-link-commit)
              ("C-c '" . mu/md-backquote))
  :hook
  (markdown-mode . markdown-toggle-markup-hiding)
  (markdown-mode . visual-line-mode)
  :config
  (setq markdown-command "~/bin/pandoc")
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(markdown-header-face-1 ((t (:inherit markdown-header-face :foreground "yellow" :height 1.0))))
   '(markdown-header-face-2 ((t (:inherit markdown-header-face :foreground "purple" :height 1.0))))
   '(markdown-header-face-3 ((t (:inherit markdown-header-face :foreground "magenta" :height 1.0))))
   '(markdown-header-face-4 ((t (:inherit markdown-header-face :foreground "dark cyan" :height 1.0))))))

(provide 'mu/markdown)

;;; markdown.el ends here
