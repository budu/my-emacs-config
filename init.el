;;;;;;;;
;;;;;;;; This configuration is based on David Wilson's excellent Emacs From Scratch.
;;;;;;;; series. https://github.com/daviwil/emacs-from-scratch
;;;;;;;;
;;;;;;;; Custom variables/functions should be prefixed with "mu/"
;;;;;;;;
;;;;;;;; One guiding principle of this configuration is to make key binding as close as my
;;;;;;;; bash/readline configuration.
;;;;;;;;

;;;; functions

(load-file "functions.el")

;;;; vars

(defvar mu/default-font-size 180)

;;;; minimalism

(setq inhibit-startup-message t)

(column-number-mode)									; show column in mode line
(global-display-line-numbers-mode t)	; show line numbers
(menu-bar-mode -1)										; disable the menu bar
(scroll-bar-mode -1)									; disable visible scrollbar
(set-fringe-mode 10)									; give some breathing room
(tool-bar-mode -1)										; disable the toolbar
(tooltip-mode -1)											; disable tooltips

;;;; personal preference

(setq-default 
 fill-column 92
 indent-tabs-mode nil
 tab-width 2)

;;;; fonts

(set-face-attribute 'default nil :font "Fira Code Retina" :height mu/default-font-size)

;;;; key bindings

(global-set-key "\C-c\C-c" 'comment-or-uncomment-region)
(global-set-key "\C-h"		 'delete-backward-char)         ; displace help-command
(global-set-key "\C-m"		 'mu/kmacro-start-or-end-macro) ; displace newline
(global-set-key "\C-w"		 'backward-kill-word)           ; displace kill-region
(global-set-key "\C-x\C-b" 'mu/switch-to-last-buffer)     ; displace list-buffers
(global-set-key "\C-x\C-c" 'kill-region)                  ; displace save-buffers-kill-terminal
(global-set-key "\C-x\M-q" 'save-buffers-kill-emacs)
(global-set-key "\M-j"		 'next-window-any-frame)        ; displace default-indent-new-line
(global-set-key "\M-k"		 'previous-window-any-frame)    ; displace kill-sentence

(global-set-key (kbd "C-'") 'mu/touch)

(global-set-key "\C-z"    "()\C-b") ; displace suspend-frame
(global-set-key "\M-z"    "[]\C-b") ; displace zap-to-char
(global-set-key "\C-\M-z" "{}\C-b") 

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

;;;; appearance

(use-package all-the-icons) ; M-x all-the-icons-install-fonts

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; ivy

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
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
