;;; elisp.el --- Emacs Lisp configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for Emacs Lisp development.
;;
;;; Code:

;; docs only show usage from command line:
;; emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch *.el
;; https://github.com/gonewest818/elisp-lint
(use-package elisp-lint)

;; Usage from command line:
;; emacs -batch -f package-initialize -L . -f buttercup-run-discover
(use-package buttercup
  :bind (:map buttercup-minor-mode-map
         ("C-c C-t" . buttercup-run-at-point)))

(use-package elisp-mode
  :ensure nil
  :hook ((emacs-lisp-mode . buttercup-minor-mode)))

(provide 'mu/elisp)

;;; elisp.el ends here
