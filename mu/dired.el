;;; dired.el --- Dired configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for DIRED file manager.
;;
;;; Code:

(use-package dired
  :ensure nil
  :init
  (require 'dired-x) ;; I think this is required for dired-omit-mode

  :bind
  (:map dired-mode-map
	      ("C-." . dired-omit-mode))

  :hook
  (dired-mode . (lambda () (dired-omit-mode))) ;; hide .dot files by default

  :config
  (setq dired-omit-files   ;; hide .dot files when in dired-omit-mode
        (concat dired-omit-files "\\|^\\..+$")))

(provide 'mu/dired)

;;; dired.el ends here
