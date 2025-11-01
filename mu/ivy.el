;;; ivy.el --- Ivy configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for ivy completion framework.
;;
;;; Code:

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("C-l" . ivy-alt-done)
         ("C-w" . ivy-yank-word)
         ("C-SPC" . ivy-occur)
         :map ivy-switch-buffer-map
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-d" . ivy-reverse-i-search-kill))
  :config (ivy-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  (setq ivy-truncate-lines nil)
  (ivy-posframe-mode 1)
  (set-face-attribute 'ivy-posframe nil
                      :foreground "white"
                      :background "#126"))

(provide 'mu/ivy)

;;; ivy.el ends here
