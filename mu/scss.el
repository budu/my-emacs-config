;;; scss.el --- SCSS configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for SCSS development.
;;
;;; Code:

(use-package scss-mode
  :bind ([remap scss-compile] . comment-or-uncomment-region))

(provide 'mu/scss)

;;; scss.el ends here
