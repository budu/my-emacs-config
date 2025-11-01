;;; restclient.el --- Restclient configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for restclient mode.
;;
;;; Code:

(use-package restclient
  :mode (("\\.rc\\'" . restclient-mode)))

(provide 'mu/restclient)

;;; restclient.el ends here
