;;; multiple-cursors.el --- Multiple cursors configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for multiple-cursors package.
;;
;;; Code:

(use-package multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c /") 'mc/mark-all-dwim)

(provide 'mu/multiple-cursors)

;;; multiple-cursors.el ends here
