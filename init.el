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

(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("emacs-libyaml")))

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

;;;; functions & macros

(load-relative "functions.el")
(load-relative "macros.el")

;;;; vars

(defvar mu/default-font-size 180)

(setq scheme-program-name "racket")

;;;; local vars overrides

(let ((filename "~/.emacs.d/local-vars.el"))
  (when (file-exists-p (expand-file-name filename))
    (load-relative filename)))

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

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load-relative "custom.el")

(setq-default
 fill-column 92
 indent-tabs-mode nil
 show-trailing-whitespace t
 tab-width 2
 css-indent-offset 2
 js-indent-level 2
 typescript-indent-level 2
 truncate-lines 0)

;; don't use tabs in align-regexp
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

(delete-selection-mode 1)

;;;; multiple-cursors

(use-package multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c /") 'mc/mark-all-like-this)

;;;; global key bindings

(define-key input-decode-map [?\C-m] [C-m])
(define-key input-decode-map [?\C-i] [C-i])
(define-key input-decode-map [?\M-i] [M-i])

(global-set-key [C-tab]    'mu/cslist-to-indented-lines-and-back)
(global-set-key "\C-c1"    'sort-lines)
(global-set-key "\C-c2"    'mu/sort-words)
(global-set-key "\C-c5"    'mu/convert-region-to-percent-w-syntax)
(global-set-key "\C-cb"    'magit-blame)
(global-set-key "\C-xl"    'magit-log-buffer-file)
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region)
(global-set-key "\C-h"     'delete-backward-char)         ; displace help-command
(global-set-key "\C-w"     'backward-kill-word)           ; displace kill-region
(global-set-key "\C-x\C-b" 'mu/switch-to-last-buffer)     ; displace list-buffers
(global-set-key "\C-x\C-c" 'kill-region)                  ; displace save-buffers-kill-terminal
(global-set-key "\C-x\M-q" 'save-buffers-kill-emacs)
(global-set-key "\M-j"     'previous-window-any-frame)    ; displace default-indent-new-line
(global-set-key "\M-k"     'next-window-any-frame)        ; displace kill-sentence
(global-set-key "\C-\M-h"  'mark-paragraph)               ; displace mark-defun
(global-set-key "\C-\M-g"  'mu/search-gems)

(global-set-key (kbd "C-'")     'mu/touch)
(global-set-key (kbd "C-:")     "&:")
(global-set-key (kbd "C-M-S-h") 'mark-defun)
(global-set-key (kbd "M-p")     "-> {  }\C-b\C-b")
(global-set-key (kbd "<C-i>")   "||\C-b") ; displace tab-bound function
(global-set-key (kbd "<M-i>")   "<% %>\C-b\C-b\C-b") ; displace tab-to-tab-stop function
(global-set-key (kbd "<C-m>")   'mu/kmacro-start-or-end-macro) ; displace newline
(global-set-key (kbd "<f12>")   (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(global-set-key "\C-cw"   "%w[")
(global-set-key "\C-z"    "()\C-b") ; displace suspend-frame
(global-set-key "\M-z"    "[]\C-b") ; displace zap-to-char
(global-set-key "\C-\M-z" "{}\C-b")
(global-set-key "\C-\M-d" 'mu/kill-parens) ; displace down-list

;;;; appearance

(set-face-attribute 'default nil
                    :font "Fira Code Retina"
                    :height mu/default-font-size)

(use-package all-the-icons) ; M-x all-the-icons-install-fonts

(use-package doom-modeline
  :custom ((doom-modeline-height 15))
  :init (doom-modeline-mode 1))

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

(use-package expand-region
  :bind (("M-h" . er/expand-region)))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-a" . counsel-apropos) ; displace beginning-of-defun
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy
  :diminish
  :bind (("C-M-s" . swiper)
         :map ivy-minibuffer-map
         ("C-l" . ivy-alt-done)
         ("C-w" . ivy-yank-word)
         ("C-SPC" . ivy-occur)
         :map ivy-switch-buffer-map
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-d" . ivy-reverse-i-search-kill))
  :config (ivy-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

;;;; help

(use-package which-key
  :diminish which-key-mode
  :custom ((which-key-frame-max-width 200)
           (which-key-idle-delay 0.5)
           (which-key-min-column-description-width 50)
           (which-key-max-description-length 50)
           (which-key-side-window-location 'bottom)
           (which-key-side-window-max-height 0.25))
  :init (which-key-mode))

(use-package helpful
  :bind (([remap describe-function] . counsel-describe-function)
         ([remap describe-command] . helpful-command)
         ([remap describe-variable] . counsel-describe-variable)
         ([remap describe-key] . helpful-key))
  :custom ((counsel-describe-function-function #'helpful-callable)
           (counsel-describe-variable-function #'helpful-variable)))

;;;; translation

(use-package lingva
  :bind ("C-c t" . lingva-translate)
  :custom (lingva-target "fr"))

;;;; spelling

(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1)))))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

;;;; winner

(winner-mode 1)
(global-set-key (kbd "C-x `") 'winner-undo) ; displace next-error

;;;; magit

(use-package magit
  :commands magit-status
  :custom (magit-display-buffer-function
           #'magit-display-buffer-same-window-except-diff-v1))

;;;; projectile

(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("C-c f" . projectile-find-file)
         ("C-c s" . projectile-ripgrep)
         ("C-S-f" . projectile-find-file)
         ("C-M-S-f" . projectile-ripgrep))
  :custom ((projectile-completion-system 'ivy))
  :init
  (when (file-directory-p "~/cg")
    (setq projectile-project-search-path '("~/cg")))
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-vc)
  :config (projectile-mode))

(use-package projectile-ripgrep)

(advice-add 'counsel-rg
            :around
            (lambda (func &rest args)
              (cl-flet ((filter-func (code) (if (= code 2) 0 code)))
                (unwind-protect
                    (progn (advice-add 'process-exit-status :filter-return #'filter-func)
                           (apply func args))
                  (advice-remove 'process-exit-status #'filter-func)))))

(use-package counsel-projectile
  :init (setq counsel-projectile-switch-project-action #'projectile-vc)
  :config (counsel-projectile-mode))

;;;; ellama

(use-package ellama
  :init
  (setopt ellama-language "French")
  (require 'llm-ollama)
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "zephyr:latest"
       :embedding-model "zephyr:latest")))

;;;; prog

(add-hook 'prog-mode-hook 'electric-pair-mode)

(use-package coffee-mode)
(use-package crystal-mode)
(use-package dap-mode)
(use-package dockerfile-mode)
(use-package elisp-lint)
(use-package geiser-racket)
(use-package slim-mode)
(use-package yaml-mode)

;;;; ein

(use-package ein
  :bind (:map ein:notebook-mode-map
         ("<C-M-return>" . ein:worksheet-execute-cell)))

;;;; emmet

(use-package emmet-mode
  :hook sgml-mode
  :bind (:map emmet-mode-keymap
         ("C-j" . newline-and-indent)))

;;;; restclient

(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)))

;;;; ruby

(use-package inf-ruby)

(use-package rvm
  :init (rvm-use-default))

(use-package ruby-mode
  :mode "\\.arb\\'"
  :bind (:map ruby-mode-map
         ("C-M-d" . 'mu/kill-parens))) ; displace smie-down-list

(use-package projectile-rails
  :hook ((ruby-mode . rvm-activate-corresponding-ruby))
  :bind-keymap ("C-c r" . projectile-rails-command-map)
  :config (projectile-rails-global-mode))

(use-package rspec-mode
  :hook (ruby-mode . rspec-mode))

(use-package ruby-end)

(use-package rails-i18n
  :bind (:map ruby-mode-map
         ("C-c i" . 'rails-i18n-insert-with-cache)
         ("C-c i" . 'rails-i18n-insert-no-cache)
         :map slim-mode-map
         ("C-c i" . 'rails-i18n-insert-with-cache)
         ("C-c i" . 'rails-i18n-insert-no-cache))
  :init (require 'libyaml)
  :config (advice-add 'rails-i18n--read-lines :override #'yaml-read-file))

;;;; scss

(use-package scss-mode
  :bind ([remap scss-compile] . comment-or-uncomment-region))

;;;; sql

(use-package sql-mode
  :ensure nil
  :mode "\\.sql.erb\\'")

;;;; web

(use-package web-mode
  :mode "\\.svelte\\'")

;;;; global hooks

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(add-hook 'comint-mode-hook 'goto-address-mode)

(dolist (hook '(special-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                minibuffer-setup-hook))
  (add-hook hook
            (lambda () (setq show-trailing-whitespace nil))))

;;;; end
