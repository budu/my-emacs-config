;;; screenshot.el --- Screenshot configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for screenshot package.
;;
;;; Code:

(use-package screenshot
  :ensure nil
  :quelpa (screenshot :fetcher github
                      :repo "tecosaur/screenshot"
                      :branch "master"
                      :files ("screenshot.el")))

(define-key mu/cg-map (kbd "s") 'screenshot)

(provide 'mu/screenshot)

;;; screenshot.el ends here
