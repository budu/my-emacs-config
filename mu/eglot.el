;;; eglot.el --- Eglot configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for eglot LSP client.
;;
;;; Code:

(use-package eglot
  :init
  (setq eldoc-echo-area-use-multiline-p nil
        eglot-put-doc-in-help-buffer nil) ;'eglot-doc-too-large-for-echo-area)
  :custom
  (eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))

(provide 'mu/eglot)

;;; eglot.el ends here
