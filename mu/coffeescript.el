;;; coffeescript.el --- CoffeeScript configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for CoffeeScript development.
;;
;;; Code:

(use-package coffee-mode
  :bind (:map coffee-mode-map
         ("C-j" . 'coffee-newline-and-indent))
  :custom (coffee-indent-like-python-mode t))

(provide 'mu/coffeescript)

;;; coffeescript.el ends here
