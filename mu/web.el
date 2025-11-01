;;; web.el --- Web mode configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for web-mode.
;;
;;; Code:

(use-package web-mode
  :mode "\\.svelte\\'"
  :custom ((web-mode-markup-indent-offset 2)
           (web-mode-css-indent-offset 2)
           (web-mode-code-indent-offset 2)))

(provide 'mu/web)

;;; web.el ends here
