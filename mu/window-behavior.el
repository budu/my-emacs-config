;;; window-behavior.el --- Window behavior configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configuration for window management and behavior, including golden ratio mode
;;; for automatic window resizing.

;;; Code:

(use-package golden-ratio
  :config (golden-ratio-mode 1)
  :custom ((golden-ratio-auto-scale t)
           (golden-ratio-exclude-modes '("ediff-mode" "magit-popup-mode"))
           (golden-ratio-exclude-buffer-names '("*Calendar*"))))

(advice-add 'next-window-any-frame
            :after
            (lambda (&rest args) (golden-ratio)))
(advice-add 'previous-window-any-frame
            :after
            (lambda (&rest args) (golden-ratio)))

(provide 'window-behavior)

;;; window-behavior.el ends here
