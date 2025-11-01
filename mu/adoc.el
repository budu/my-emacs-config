;;; adoc.el --- AsciiDoc configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for AsciiDoc (adoc) mode.
;;
;;; Code:

(use-package adoc-mode
  :mode "\\.adoc\\'"
  :custom ((adoc-command "asciidoctor"))
  :custom-face (adoc-title-0-face ((t (:height 1.5))))
  :custom-face (adoc-title-1-face ((t (:height 1.3))))
  :custom-face (adoc-title-2-face ((t (:height 1.2))))
  :custom-face (adoc-title-3-face ((t (:height 1.1))))
  :custom-face (adoc-title-4-face ((t (:height 1.0))))
  :custom-face (adoc-title-5-face ((t (:height 0.9)))))

(provide 'mu/adoc)

;;; adoc.el ends here
