;;; mastodon.el --- Mastodon configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for mastodon package.
;;
;;; Code:

(use-package mastodon
  :custom ((mastodon-instance-url "https://ruby.social")
           (mastodon-active-user "budu")))

(provide 'mu/mastodon)

;;; mastodon.el ends here
