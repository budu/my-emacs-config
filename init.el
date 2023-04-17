;;;;;;;;
;;;;;;;; This configuration is based on David Wilson's excellent Emacs From Scratch.
;;;;;;;; series. https://github.com/daviwil/emacs-from-scratch
;;;;;;;;
;;;;;;;; Custom variables/functions should be prefixed with "mu/"
;;;;;;;;
;;;;;;;; One guiding principle of this configuration is to make key binding as close as my
;;;;;;;; bash/readline configuration.
;;;;;;;;

;;;; shims

(let ((dir (expand-file-name "~/.asdf/shims/")))
  (when (and (file-directory-p dir)
             (not (member dir exec-path)))
    (setenv "PATH" (concat (getenv "PATH") (concat ":" dir)))
    (setq exec-path (push dir exec-path))))

;;;; packages

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  ; initialize use-package on non-Linux platforms
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package load-relative)

;;;; functions

(load-relative "functions.el")

;;;; vars

(defvar mu/default-font-size 180)

(setq scheme-program-name "racket")

;;;; local vars overrides

(load-relative "local-vars.el")

;;;; minimalism

(setq inhibit-startup-message t)

(column-number-mode)                 ; show column in mode line
(global-display-line-numbers-mode t) ; show line numbers
(menu-bar-mode -1)                   ; disable the menu bar
(scroll-bar-mode -1)                 ; disable visible scrollbar
(set-fringe-mode 10)                 ; give some breathing room
(tool-bar-mode -1)                   ; disable the toolbar
(tooltip-mode -1)                    ; disable tooltips

;;;; personal preference

(setq custom-file "custom.el")

(setq-default
 fill-column 92
 indent-tabs-mode nil
 show-trailing-whitespace t
 tab-width 2)

;; don't use tabs in align-regexp
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;; fonts

(set-face-attribute 'default nil :font "Fira Code Retina" :height mu/default-font-size)

;;;; key bindings

(define-key input-decode-map [?\C-m] [C-m])

(global-set-key "\C-cl"    'magit-log-buffer-file)
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region)
(global-set-key "\C-h"     'delete-backward-char)         ; displace help-command
(global-set-key "\C-w"     'backward-kill-word)           ; displace kill-region
(global-set-key "\C-x\C-b" 'mu/switch-to-last-buffer)     ; displace list-buffers
(global-set-key "\C-x\C-c" 'kill-region)                  ; displace save-buffers-kill-terminal
(global-set-key "\C-x\M-q" 'save-buffers-kill-emacs)
(global-set-key "\M-j"     'next-window-any-frame)        ; displace default-indent-new-line
(global-set-key "\M-k"     'previous-window-any-frame)    ; displace kill-sentence

(global-set-key (kbd "C-'")   'mu/touch)
(global-set-key (kbd "<C-m>") 'mu/kmacro-start-or-end-macro) ; displace newline
(global-set-key (kbd "<f12>") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(global-set-key "\C-z"    "()\C-b") ; displace suspend-frame
(global-set-key "\M-z"    "[]\C-b") ; displace zap-to-char
(global-set-key "\C-\M-z" "{}\C-b")

;;;; appearance

(use-package all-the-icons) ; M-x all-the-icons-install-fonts

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package emojify
  :hook (after-init . global-emojify-mode)
  :config
  (setq emojify-user-emojis
        '((":memo:" . (("name" . "Memo")
                       ("image" . "~/.emacs.d/emojis/1f4dd.png")
                       ("style" . "github"))))))

;;;; backups

(setq
 backup-by-copying t    ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves/")) ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)     ; use versioned backups

;;;; ivy

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy
  :diminish
  :bind (("C-M-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("C-w" . ivy-yank-word)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;;;; help

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;;; translation

(use-package lingva
  :bind
  ("C-c t" . lingva-translate))

;;;; magit

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

;;;; projectile

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind
  (("C-c f" . projectile-find-file))
  (("C-c s" . projectile-ripgrep))
  (("C-F" . projectile-find-file))
  :init
  (when (file-directory-p "~/cg")
    (setq projectile-project-search-path '("~/cg")))
  (when (file-directory-p "~/project")
    (setq projectile-project-search-path '("~/project")))
  (setq projectile-switch-project-action #'projectile-vc))

(use-package projectile-rails
  :bind-keymap
  ("C-c r" . projectile-rails-command-map)
  :config
  (projectile-rails-global-mode))

;;;; lsp

(use-package spinner)

(defun mu/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . mu/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;;;; prog

(add-hook 'prog-mode-hook 'electric-pair-mode)

(use-package coffee-mode)
(use-package elisp-lint)
(use-package geiser-racket)
(use-package inf-ruby)
(use-package rvm :init (rvm-use-default))
(use-package scss-mode)
(use-package yaml-mode)

;;;; emmet

(use-package emmet-mode
  :hook sgml-mode
  :bind ("C-j" . newline-and-indent))

;;;; direnv

(use-package direnv
  :config
  (defun direnv--detect ()
    "/home/budu/.asdf/shims/direnv")
  (direnv-mode))

;;;; end
