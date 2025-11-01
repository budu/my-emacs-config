;;; rust.el --- Rust configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for Rust development.
;;
;;; Code:

(use-package rust-mode
  :mode "\\.rs\\'"
  :custom ((rust-format-on-save t)))

(provide 'mu/rust)

;;; rust.el ends here
