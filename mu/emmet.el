;;; emmet.el --- Emmet configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for emmet-mode.
;; See https://emmet.io/
;;
;;; Code:

(use-package emmet-mode
  :hook web-mode
  :bind (:map emmet-mode-keymap
         ("C-j" . newline-and-indent)))

(provide 'mu/emmet)

;;; emmet.el ends here
