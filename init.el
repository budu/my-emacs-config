;;; package --- My Emacs init file
;;;
;;; Commentary:
;;;   This configuration is based on David Wilson's excellent Emacs From Scratch
;;;   series: https://github.com/daviwil/emacs-from-scratch
;;;
;;;   I'm using the "mu/" prefix as namespace for custom variables/functions.
;;;
;;;   One guiding principle of this configuration is to make key binding as close as my
;;;   bash/readline configuration.
;;;
;;;   TODO: check https://github.com/patrickt/emacs
;;;
;;; Update:
;;;   package-upgrade-all
;;;   quelpa-self-upgrade
;;;   quelpa-upgrade-all
;;;   straight-pull-all
;;;   straight-rebuild-all
;;;
;;; Code:

;;;; packages

(define-prefix-command 'mu/cg-map)
(global-set-key (kbd "C-c g") 'mu/cg-map)

(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("emacs-libyaml")))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq use-package-always-ensure t)

(use-package load-relative)
(use-package quelpa)
(use-package quelpa-use-package)

;;;; performance

(setq gc-cons-threshold (* 100 1024 1024) ;; GC sometime after allocating 100Mb
      ;; jit-lock-defer-time nil
      ;; jit-lock-stealth-time 1.25
      ;; jit-lock-stealth-nice 0.5
      jit-lock-chunk-size 4096
      read-process-output-max (* 1024 1024)
      package-native-compile t)
(run-with-idle-timer 2 t (lambda () (garbage-collect))) ;; Trigger a GC after 5s of idle time

;;;; shims

(use-package asdf
  :ensure nil
  :quelpa (asdf :fetcher github
                :repo "tabfugnic/asdf.el"
                :branch "main"
                :files ("asdf.el"))
  :config
  (setq asdf-binary "/opt/asdf-vm/bin/asdf")
  (asdf-enable))

;;;; env vars

(defun load-env-vars ()
  "Load environment variables from the ~/.env file."
  (let ((env-file (expand-file-name "~/.env")))
    (when (file-exists-p env-file)
      (with-temp-buffer
        (insert-file-contents env-file)
        (goto-char (point-min))
        (while (re-search-forward "^\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.*\\)$" nil t)
          (setenv (match-string 1) (match-string 2)))))))

(load-env-vars)

;;;; functions & macros

(defun mu/get-project-dir ()
  (let* ((original-dir default-directory)
         (in-nb-notes (string-match-p "^/.+/nb-notes\\(/\\|$\\)" default-directory))
         (git-dir (if in-nb-notes
                      (file-name-as-directory
                       (car (split-string original-dir "/nb-notes" t)))
                    original-dir)))
    git-dir))

(load-relative "macros.el")
(load-relative "functions.el")

;; List all files in the mu/ directory
(let ((mu-dir (concat (file-name-directory (or load-file-name buffer-file-name)) "mu/")))
  (dolist (file (directory-files mu-dir t "\\.el$"))
    (load file)))

;;;; vars

; set paragraph-start to handle various lists style
(setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] \\|[ \t]*[0-9]+\\. ")
; handle fill-paragraph list indentation
(setq adaptive-fill-regexp "[ \t]*\\([-+*] \\|\\([0-9]+\\.\\)[ \t]+\\|[ \t]*[-+*][ \t]+\\|[ \t]*[0-9]+\\.\\)[ \t]*")

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
 lua-indent-level 2
 js-indent-level 2
 typescript-indent-level 2
 truncate-lines 0)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun my-compilation-mode-hook ()
  (face-remap-add-relative 'default '(:height 0.7)))

(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(defun bugfix-display-line-numbers--turn-on (fun &rest args)
  "Avoid `display-line-numbers-mode' in `image-mode' and related.
Around advice for FUN with ARGS."
  (unless (derived-mode-p 'image-mode 'docview-mode 'pdf-view-mode)
    (apply fun args)))

(advice-add 'display-line-numbers--turn-on :around #'bugfix-display-line-numbers--turn-on)

; list directories first in dired
(setq dired-listing-switches "-alh --group-directories-first")

;;;; advice

; don't ask when saving buffers
(defadvice basic-save-buffer (around auto-confirm compile activate)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
            ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
    ad-do-it))

; don't use tabs in align-regexp
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

(delete-selection-mode 1)

; use aggressive auto-revert
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq auto-revert-use-notify t)
(setq auto-revert-notify-watch-descriptor-list t)
(setq auto-revert-interval 0.1)

;;;; appearance

(set-face-attribute 'default nil
                    :font "Fira Code Retina"
                    :height 80)

; Run these after the package has been installed:
; M-x all-the-icons-install-fonts
; M-x nerd-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :custom ((doom-modeline-height 15))
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :init (load-theme 'doom-dracula t)
  (custom-set-faces
   '(default ((t (:background "#121a1e"))))))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#444444")))))
  )

(use-package emojify
  :hook (after-init . global-emojify-mode)
  :config
  (setq emojify-user-emojis
        '((":memo:" . (("name" . "Memo")
                       ("image" . "~/.emacs.d/emojis/1f4dd.png")
                       ("style" . "github")))
          (":broom:" . (("name" . "Broom")
                        ("image" . "~/.emacs.d/emojis/1f9f9.png")
                        ("style" . "github")))
          (":mirror:" . (("name" . "Mirror")
                        ("image" . "~/.emacs.d/emojis/1fa9e.png")
                        ("style" . "github"))))))

;;;; Open ~/org/index.org by default on load

(find-file "~/org/index.org")

;;;; behavior

(use-package golden-ratio
  :config (golden-ratio-mode 1)
  :custom ((golden-ratio-auto-scale t)
           (golden-ratio-exclude-modes '("ediff-mode" "magit-popup-mode"))
           (golden-ratio-exclude-buffer-names '("*Calendar*"))))

(advice-add 'next-window-any-frame
            :after
            (lambda (&rest args) (golden-ratio)))
(advice-add 'previous-window-any-frame
            :after
            (lambda (&rest args) (golden-ratio)))

;;;; backups

(setq
 backup-by-copying t    ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves/")) ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)     ; use versioned backups


;;;; keyfreq

;; (use-package keyfreq
;;   :config
;;   (keyfreq-mode 1)
;;   (keyfreq-autosave-mode 1))

;;;; ivy

(use-package expand-region
  :bind (("M-h" . er/expand-region))
  :config
  (defun er/add-ruby-mode-expansions ()
    "Adds Ruby-specific expansions for buffers in ruby-mode"
    (set (make-local-variable 'er/try-expand-list)
         (remove 'er/mark-defun
                 (append
                  (default-value 'er/try-expand-list)
                  '(er/mark-ruby-instance-variable
                    er/c-mark-fully-qualified-name
                    er/mark-ruby-block-up
                    er/mark-ruby-heredoc)))))
  (er/enable-mode-expansions 'ruby-mode #'er/add-ruby-mode-expansions))

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

;;;; posframe

(use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  (setq ivy-truncate-lines nil)
  (ivy-posframe-mode 1)
  (set-face-attribute 'ivy-posframe nil
                      :foreground "white"
                      :background "#126"))

;;;; mastodon

(use-package mastodon
  :custom ((mastodon-instance-url "https://ruby.social")
           (mastodon-active-user "budu")))

;;;; screenshot

(use-package screenshot
  :ensure nil
  :quelpa (screenshot :fetcher github
                      :repo "tecosaur/screenshot"
                      :branch "master"
                      :files ("screenshot.el")))

(define-key mu/cg-map (kbd "s") 'screenshot)

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
           #'magit-display-buffer-same-window-except-diff-v1)
  :bind (:map magit-mode-map
              ("<return>" . magit-diff-visit-worktree-file))
  :init
  (setq magit-list-refs-sortby "-creatordate"))

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
  (setq projectile-switch-project-action #'projectile-vc)
  (setq projectile-generic-command "fd . -H -0 --type f")
  (setq projectile-create-missing-test-files t)
  :config
  (setq projectile-project-search-path '("~/cg" "~/projects"))
  (projectile-load-known-projects))

(use-package projectile-ripgrep)

;; TODO: http://localhost:3080/c/b2d69d5c-88d5-4ffb-8c62-b3ad401d18ce
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

;;;; text

(add-hook 'text-mode-hook
          (lambda () (setq fill-column 72)))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(define-key global-map "\M-Q" 'unfill-paragraph)

;;;; folding

;; old but working, might want to look into ts-fold
(use-package origami
  :bind (("C-c v" . origami-recursively-toggle-node)
         ("C-c V" . origami-toggle-all-nodes)
         ("C-c C-v" . origami-toggle-node))
  :hook ((prog-mode . origami-mode)
         (text-mode . origami-mode)))

;;;; prog

(setq scheme-program-name "racket")

(add-hook 'prog-mode-hook 'electric-pair-mode)

(use-package crystal-mode)
(use-package dap-mode)
(use-package dockerfile-mode)
(use-package geiser-racket)
(use-package lua-mode)
(use-package yaml-mode)

;;;; eglot

(use-package eglot
  :init
  (setq eldoc-echo-area-use-multiline-p nil
        eglot-put-doc-in-help-buffer nil) ;'eglot-doc-too-large-for-echo-area)
  :custom
  (eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))

;;;; copilot

(use-package copilot
  :hook ((prog-mode . copilot-mode)
         (text-mode . copilot-mode)
         (git-commit-mode . copilot-mode)
         (copilot-mode . (lambda ()
                           (setq-local copilot--indent-warning-printed-p t))))
  :bind (:map copilot-completion-map
         ("<return>" . copilot-accept-completion)
         ("<M-tab>" . copilot-next-completion))
  :config
  (setq copilot-max-char-warning-disable t)
  (set-face-attribute 'copilot-overlay-face nil :foreground "#585"))

;;;; flycheck

(use-package flycheck
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :custom ((flycheck-check-syntax-automatically '(save mode-enabled))
           (flycheck-display-errors-delay 0.25)
           (flycheck-emacs-lisp-load-path 'inherit)))

;;;; company

(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :custom ((company-idle-delay 0.1)
           (company-minimum-prefix-length 2)
           (company-tooltip-align-annotations t)
           (company-tooltip-limit 10)
           (company-tooltip-flip-when-above t)))

;;;; elisp

;; docs only show usage from command line:
;; emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch *.el
;; https://github.com/gonewest818/elisp-lint
(use-package elisp-lint)

;; Usage from command line:
;; emacs -batch -f package-initialize -L . -f buttercup-run-discover
(use-package buttercup
  :bind (:map buttercup-minor-mode-map
         ("C-c C-t" . buttercup-run-at-point)))

(use-package elisp-mode
  :ensure nil
  :hook ((emacs-lisp-mode . buttercup-minor-mode)))

;;;; restclient

(use-package restclient
  :mode (("\\.rc\\'" . restclient-mode)))

;;;; ruby

(use-package inf-ruby)

(use-package rvm
  :init (rvm-use-default))

(defface my-ruby-paren-face
  '((t (:foreground "#444444")))
  "Face for ruby parentheses"
  :group 'ruby)

(use-package ruby-mode
  :mode ".irbrc\\'"
  :mode "\\.arb\\'"
  :mode "\\.axlsx\\'"
  :hook (; FIXME: object-of-class-p: Lisp nesting exceeds ‘max-lisp-eval-depth’: 1601 [2 times]
         ; (ruby-mode tree-sitter-mode)
         ; (ruby-mode . eglot-ensure) ; this is not working
         (ruby-mode . company-mode))
  :bind (:map ruby-mode-map
         ("M-." . mu/ruby/find-definition)
         ("C-M-." . 'mu/ruby/find-references)
         ("C-c C-e" . "end\C-j")
         ("C-M-h" . er/mark-ruby-block-up)
         ("C-M-p" . er/ruby-backward-up)
         ("C-c l" . "||\C-b")
         ("C-c j" . " \M- do\n\nend\C-p\C-i")
         ("C-M-d" . 'mu/kill-parens)) ; displace smie-down-list
  :config
  (font-lock-add-keywords
   'ruby-mode
   '(("[()]" 0 'my-ruby-paren-face)))
  )

(use-package projectile-rails
  :hook ((ruby-mode . rvm-activate-corresponding-ruby))
  :bind-keymap ("C-c r" . projectile-rails-command-map)
  :config (projectile-rails-global-mode))

(use-package rspec-mode
  :hook (ruby-mode . rspec-mode)
  :bind (:map ruby-mode-map
         ("<f9>" . 'rspec-verify)
         ("C-<f9>" . 'rspec-verify-live))
  :config
  (defun rspec-verify-live ()
    "Run the specified spec, or the spec file for the current buffer."
    (interactive)
    (let ((process-environment (cons "LIVE=t" process-environment)))
      (rspec--autosave-buffer-maybe)
      (rspec-run-single-file (rspec-spec-file-for (buffer-file-name))
                             (rspec-core-options)))))

(use-package robe
  :hook ((ruby-mode . robe-mode))
  :bind (:map ruby-mode-map
         ("C-c C-d" . 'robe-doc)
         ("C-c C-k" . 'robe-rails-refresh)
         ("C-c C-l" . 'robe-load-current-buffer)
         ("C-c C-s" . 'robe-start)
         ("C-c C-t" . 'robe-test)
         ("C-c C-v" . 'robe-jump)))

(use-package rubocop
  :init
  (add-hook 'ruby-mode-hook 'rubocop-mode)
  :diminish rubocop-mode
  :config
  (setq rubocop-autocorrect-on-save nil))

(use-package ruby-end)

(use-package slim-mode
  :bind (:map slim-mode-map
         ("M-." . mu/ruby/find-definition)
         ("C-c '" . 'ruby-toggle-string-quotes)
         ("C-c 9" . 'mu/i18n/goto-translation-file)
         ("C-c i" . 'mu/i18n/extract-translation)
         ("C-c C-i" . 'rails-i18n-insert-with-cache)))

(use-package rails-i18n
  :bind (:map ruby-mode-map
         ("C-c 9" . 'mu/i18n/goto-translation-file)
         ("C-c i" . 'mu/i18n/extract-translation)
         ("C-c C-i" . 'rails-i18n-insert-with-cache))
  :init (require 'libyaml)
  :config (advice-add 'rails-i18n--read-lines :override #'yaml-read-file))

(use-package yari
  :bind (:map ruby-mode-map
         ("C-c C-y" . 'yari)))

;;;; javascript

;; this needs some npm packages to be installed
;; npm install -g javascript-typescript-langserver
;; npm install typescript-eslint-language-service -D
(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . eglot-ensure))
  :bind (:map js2-mode-map
         ("C-c n" . 'eglot-rename))
  :custom
  (js2-mode-assume-strict t)
  (js2-warn-about-unused-function-arguments t)
  (js2-strict-missing-semi-warning nil))

(use-package json-mode)

;;;; coffeescript

(use-package coffee-mode
  :bind (:map coffee-mode-map
         ("C-j" . 'coffee-newline-and-indent))
  :custom (coffee-indent-like-python-mode t))

;;;; rust

(use-package rust-mode
  :mode "\\.rs\\'"
  :custom ((rust-format-on-save t)))

;;;; scss

(use-package scss-mode
  :bind ([remap scss-compile] . comment-or-uncomment-region))

;;;; sql

(use-package sql-mode
  :ensure nil
  :mode "\\.sql.erb\\'")

;;;; web

(use-package web-mode
  :mode "\\.svelte\\'"
  :custom ((web-mode-markup-indent-offset 2)
           (web-mode-css-indent-offset 2)
           (web-mode-code-indent-offset 2)))

;;;; emmet

(use-package emmet-mode
  :hook web-mode
  :bind (:map emmet-mode-keymap
         ("C-j" . newline-and-indent)))

;;;; adoc

(use-package adoc-mode
  :mode "\\.adoc\\'"
  :custom ((adoc-command "asciidoctor"))
  :custom-face (adoc-title-0-face ((t (:height 1.5))))
  :custom-face (adoc-title-1-face ((t (:height 1.3))))
  :custom-face (adoc-title-2-face ((t (:height 1.2))))
  :custom-face (adoc-title-3-face ((t (:height 1.1))))
  :custom-face (adoc-title-4-face ((t (:height 1.0))))
  :custom-face (adoc-title-5-face ((t (:height 0.9)))))

;;;; Polymode

(use-package polymode)

;;;; markdown

;; fix markdown command is not found
(use-package markdown-mode
  :bind (:map markdown-mode-map
         ("C-M-b" . markdown-backward-paragraph)
         ("C-M-f" . markdown-forward-paragraph)
         ("C-c l" . mu/md-link-commit)
         ("C-c '" . mu/md-backquote))
  :hook
  (markdown-mode . markdown-toggle-markup-hiding)
  (markdown-mode . visual-line-mode)
  :config
  (setq markdown-command "~/bin/pandoc"))

;;;; Transient

(use-package transient)

;;;; Ansi-color

(use-package ansi-color
  :config
  (defun my-ansi-color-apply ()
    "Interpret ANSI color escape sequences in the current buffer."
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max)))

  ;; Function to process log files
  (defun my-colorize-compilation-buffer ()
    (when (string-match-p "\\.log\\'" (or buffer-file-name ""))
      (read-only-mode -1)
      (my-ansi-color-apply)
      (read-only-mode 1)))

  ;; Automatically process log files when opened
  (add-hook 'find-file-hook 'my-colorize-compilation-buffer))

;;;; Eat

(use-package eat
  :config
  ;; override these keys explicitly to prevent eat-mode from shadowing global keys
  (with-eval-after-load 'eat
    (define-key eat-semi-char-mode-map (kbd "M-j") #'previous-window-any-frame)
    (define-key eat-semi-char-mode-map (kbd "M-k") #'next-window-any-frame)
    (define-key eat-char-mode-map (kbd "M-j") #'previous-window-any-frame)
    (define-key eat-char-mode-map (kbd "M-k") #'next-window-any-frame)))

;;;; Claude Code

(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :bind-keymap
  ("C-c k" . claude-code-command-map)
  :hook ((claude-code--start . sm-setup-claude-faces))
  :config
  (claude-code-mode))

(defun claude-code-send-region-internal (start end)
  "Call claude-code-send-region from START to END and handle prefix ARG."
  (save-excursion
    (goto-char start)
    (push-mark end nil t)
    (claude-code-send-region)))

(defun mu/get-claude-buffer ()
  "Get the most recent Claude buffer, or nil if none exists."
  (let ((claude-buffers (seq-filter (lambda (buf)
                                      (string-match-p "^\\*claude" (buffer-name buf)))
                                    (buffer-list))))
    (car (last claude-buffers))))

(defun mu/claude-smart-switch ()
  "Smart Claude buffer switching:
- If Claude buffer exists and is displayed, switch to that window
- If Claude buffer exists but not displayed, switch to it and go to end
- If no Claude buffer exists, create one and go to end
- Never starts claude-code inside nb-notes directory, always in containing git project"
  (interactive)
  (let* ((claude-buffer (mu/get-claude-buffer))
         (claude-window (when claude-buffer (get-buffer-window claude-buffer)))
         (current-dir (or (when buffer-file-name
                            (file-name-directory buffer-file-name))
                          default-directory))
         (target-dir (mu/get-project-dir)))
    (cond
     ;; Claude buffer displayed - switch to its window
     (claude-window (select-window claude-window))
     ;; Claude buffer exists but not displayed - switch to it
     (claude-buffer (switch-to-buffer claude-buffer))
     ;; No Claude buffer - create one in the appropriate directory
     (t
      (let ((default-directory target-dir))
        (claude-code)
        ;; Find the newly created Claude buffer
        (->> (mu/get-claude-buffer)
             (get-buffer-window)
             (select-window))))))
    (goto-char (point-max)))

;;;; global hooks

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(add-hook 'comint-mode-hook 'goto-address-mode)

(dolist (hook '(special-mode-hook
                term-mode-hook
                calendar-mode-hook
                comint-mode-hook
                compilation-mode-hook
                minibuffer-setup-hook))
  (add-hook hook
            (lambda () (setq show-trailing-whitespace nil))))

;; TODO: move all use-package out
(global-set-key "\C-cb" 'magit-blame)
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region)

;;;; multiple-cursors

(use-package multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c /") 'mc/mark-all-dwim)

;;;; global key bindings

(define-key input-decode-map [?\C--] [C--])
(define-key input-decode-map [?\M-i] [M-i])

(global-set-key (kbd "M-<mouse-2>") #'mouse-yank-at-click)
(global-set-key (kbd "C-M-y")   #'mouse-yank-primary)

(global-set-key [C-tab]    'mu/cslist-to-indented-lines-and-back)
(global-set-key [C--]      'text-scale-decrease)     ; displace negative-argument
(global-set-key "\C-c1"    'sort-lines)
(global-set-key "\C-c2"    'mu/sort-words)
(global-set-key "\C-c3"    'rubocop-autocorrect-current-file)
(global-set-key "\C-c4"    (lambda () (interactive) (kill-new (buffer-file-name))))
(global-set-key "\C-c5"    'mu/convert-region-to-percent-w-syntax)
(global-set-key "\C-c0"    'mu/goto-personal-notes)
(global-set-key "\C-xl"    'magit-log-buffer-file)
(global-set-key "\C-h"     'delete-backward-char)         ; displace help-command
(global-set-key "\C-w"     'backward-kill-word)           ; displace kill-region
(global-set-key "\C-x\C-b" 'mu/switch-to-last-buffer)     ; displace list-buffers
(global-set-key "\C-x\C-c" 'kill-region)                  ; displace save-buffers-kill-terminal
(global-set-key "\C-x\M-q" 'save-buffers-kill-emacs)
(global-set-key "\M-j"     'previous-window-any-frame)    ; displace default-indent-new-line
(global-set-key "\M-k"     'next-window-any-frame)        ; displace kill-sentence
(global-set-key "\C-\M-h"  'mark-paragraph)               ; displace mark-defun
(global-set-key "\C-\M-g"  'mu/search-gems)

(global-set-key (kbd "C-=")     'text-scale-increase)
(global-set-key (kbd "C-'")     'mu/touch)
(global-set-key (kbd "C-:")     "&:")
(global-set-key (kbd "C-M-S-h") 'mark-defun)
(global-set-key (kbd "C-M-S-k") 'kill-this-buffer)
(global-set-key (kbd "<M-i>")   "<% %>\C-b\C-b\C-b") ; displace tab-to-tab-stop function

(global-set-key (kbd "M-s M-s") 'avy-goto-char-timer)

(global-set-key (kbd "<f2>")    'mastodon-toot)
(global-set-key (kbd "<f5>")    (lambda () (interactive) (find-file "~/org/index.org")))
(global-set-key (kbd "C-<f5>")  'my/org/open-daily-note)
(global-set-key (kbd "<f6>")    'mu/goto-personal-notes)
(global-set-key (kbd "<f7>")    'org-agenda-list)
(global-set-key (kbd "<f8>")    'mu/claude-smart-switch)
(global-set-key (kbd "<f10>")   (lambda () (interactive) (find-file "~/.bashrc")))
(global-set-key (kbd "<f11>")   (lambda () (interactive) (find-file "~/.config/awesome/rc.lua")))
(global-set-key (kbd "<f12>")   (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(global-set-key (kbd "C-c SPC")    'rspec-toggle-spec-and-target)
(global-set-key (kbd "C-<return>") 'mu/open-at-point)
(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "C-<return>") 'mu/open-at-point))

(global-set-key (kbd "C-M-q")
  (lambda () (interactive)
    (setq fill-column (- (window-width) 7))))

;; key binding to wrap region in quote
(global-set-key (kbd "C-\"") 'mu/wrap-in-interpolated-quotes)

(global-set-key "\C-cw"   "%w[")
(global-set-key "\C-ci"   "%i[")
(global-set-key "\C-z"    "()\C-b") ; displace suspend-frame
(global-set-key "\M-z"    "[]\C-b") ; displace zap-to-char
(global-set-key "\C-\M-z" "{}\C-b")
(global-set-key "\C-\M-d" 'mu/kill-parens) ; displace down-list

(add-hook 'sh-mode-hook
 (lambda ()
   (define-key sh-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region)))

(add-hook 'python-mode-hook
 (lambda ()
   (define-key python-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region)))

(define-key mu/cg-map (kbd "w")
  (lambda () (interactive)
    (eww-display-html 'utf-8 (buffer-name) nil (point-min) (current-buffer))
    (setq show-trailing-whitespace nil)))


(define-key mu/cg-map (kbd "c")
  (lambda ()
    (interactive)
    (claude-code--do-send-command "Review the staged changed and commit. Just the already staged changes, nothing else")))

;;; init.el ends here
