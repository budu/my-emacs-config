;;; company.el --- Company configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for company-mode completion.
;;
;;; Code:

(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :custom ((company-idle-delay 0.1)
           (company-minimum-prefix-length 2)
           (company-tooltip-align-annotations t)
           (company-tooltip-limit 10)
           (company-tooltip-flip-when-above t)))

(provide 'mu/company)

;;; company.el ends here
