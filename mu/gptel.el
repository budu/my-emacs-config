;;; package --- gptel package configuration and helpers
;;; Commentary:
;;;   https://github.com/karthink/gptel
;;;   https://github.com/karthink/gptel-quick
;;; Code:

; TODO: what is the difference between gptel-menu and gptel-rewrite-menu
; => gptel-menu: general command center
; => gptel-rewrite-menu: specialized for rewriting tasks
;    this one is weird, why would they change the key for directive to 'h'?
;    I should study it thoroughly before embarking on my own

; TODO: move to global keybindings
(global-set-key (kbd "C-c g") 'gptel-menu)
; TODO: replace by my own but study and use first
(global-set-key (kbd "C-c t") 'gptel-rewrite-menu)
(global-set-key (kbd "C-c q") 'gptel-quick)

(use-package gptel
  :straight t
  :bind ((:map gptel-mode-map
          ("<C-M-return>" . gptel-send)))
  :hook ((gptel-post-response-functions gptel-end-of-response))
  :config
  (setq
   gptel-api-key chatgpt-api-key
   gptel-backend gptel--openai
   gptel-model   "gpt-4o"))

(use-package gptel-quick
  :straight (gptel-quick :type git
                         :host github
                         :repo "karthink/gptel-quick"))

;;; Customization

(defconst chatgpt-api-key (getenv "CHATGPT_EMACS_KEY"))
(defconst claude-api-key (getenv "CLAUDE_EMACS_KEY"))

(defvar mu/gptel/quick-timeout 10
  "Time in seconds before dismissing the summary.")

;; OpenAI

(require 'gptel)

(defvar mu/gptel/openai-backend gptel--openai)
(defconst mu/gptel/openai-models gptel--openai-models)

;; Anthropic

(require 'gptel-anthropic)

; TODO: review
; see https://docs.anthropic.com/en/docs/about-claude/models
(defconst mu/gptel/anthropic-models
  '((claude-3-5-sonnet-latest
     :description "Claude Sonnet"
     :capabilities (tool)
     :context-window 200
     :input-cost 1.50
     :output-cost 7.00)
    (claude-3-haiku-latest
     :description "Claude Haiku"
     :capabilities (tool)
     :context-window 200
     :input-cost 1.50
     :output-cost 7.00)))

(defconst mu/gptel/anthropic-backend
  (gptel-make-anthropic "Claude"
                        :stream t
                        :key claude-api-key))

;; Ollama

(require 'gptel-ollama)

(defconst mu/gptel/ollama-backend
  (gptel-make-ollama "Ollama"
                     :host "localhost:11434"
                     :stream t
                     :models '("llama3.1")))

; TODO: are input/output-cost relevant here
(defconst mu/gptel/ollama-models
  '((llama3.1
     :description "LLaMA 3.1 model for advanced language tasks"
     :capabilities (tool)
     :context-window 128
     :input-cost 1.00
     :output-cost 5.00)))

;; Actions

(defun mu/gptel/switch-backend-and-model ()
  "Switch between different backends and models for gptel."
  (interactive)
  (let* ((backend-options `(("OpenAI"    . ,mu/gptel/openai-backend)
                            ("Anthropic" . ,mu/gptel/anthropic-backend)
                            ("Ollama"    . ,mu/gptel/ollama-backend)))
         (selected-backend (completing-read "Choose backend: " (mapcar #'car backend-options)))
         (models (cond
                  ((string= selected-backend "OpenAI") mu/gptel/openai-models)
                  ((string= selected-backend "Anthropic") mu/gptel/anthropic-models)
                  ((string= selected-backend "Ollama") mu/gptel/ollama-models)))
         (model-options (mapcar (lambda (model) (symbol-name (car model))) models))
         (selected-model (completing-read "Choose model: " model-options)))
    (setq gptel-backend (cdr (assoc selected-backend backend-options))
          gptel-model selected-model)
    (message "Switched to backend: %s, model: %s" selected-backend selected-model)))

(global-set-key (kbd "C-c C-x l") 'mu/gptel/switch-backend-and-model)

;;; gptel.el ends here
