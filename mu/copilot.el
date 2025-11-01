;;; copilot.el --- Copilot configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for GitHub Copilot.
;;
;;; Code:

(use-package copilot
  :hook ((prog-mode . copilot-mode)
         (text-mode . copilot-mode)
         (git-commit-mode . copilot-mode)
         (copilot-mode . (lambda ()
                           (setq-local copilot--indent-warning-printed-p t))))
  :bind (:map copilot-completion-map
              ("<return>" . copilot-accept-completion)
              ("<M-tab>" . copilot-next-completion))
  :config
  (setq copilot-max-char-warning-disable t)
  (set-face-attribute 'copilot-overlay-face nil :foreground "#585"))

(provide 'mu/copilot)

;;; copilot.el ends here
