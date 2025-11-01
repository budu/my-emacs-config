;;; macros.el --- Keyboard macros -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Keyboard macros and convenience functions.
;;
;;; Code:

(fset 'mu/kill-parens (kmacro-lambda-form [?\C-  ?\C-  ?\C-\M-f ?\C-h ?\C-u ?\C-  ?\C-d ? ] 0 "%d"))

(defalias 'mu/md-link-commit
   (kmacro "M-h M-w M-h [ M-f C-f ( / . . / . . / c o m m i t / C-y"))

(defalias 'mu/md-backquote
   (kmacro "M-b ` M-f `"))

(provide 'macros)

;;; macros.el ends here
