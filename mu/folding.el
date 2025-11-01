;;; folding.el --- Folding configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for code folding with origami.
;;
;;; Code:

;; old but working, might want to look into ts-fold
(use-package origami
  :bind (("C-c v" . origami-recursively-toggle-node)
         ("C-c V" . origami-toggle-all-nodes)
         ("C-c C-v" . origami-toggle-node))
  :hook ((prog-mode . origami-mode)
         (text-mode . origami-mode)))

(provide 'mu/folding)

;;; folding.el ends here
