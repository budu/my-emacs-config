;;; winner.el --- Winner configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for winner-mode.
;;
;;; Code:

(winner-mode 1)
(global-set-key (kbd "C-x `") 'winner-undo) ; displace next-error

(provide 'mu/winner)

;;; winner.el ends here
