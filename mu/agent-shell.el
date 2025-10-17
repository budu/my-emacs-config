;;; mu/agent-shell.el --- Agent Shell configuration and helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration and helper functions for agent-shell (Anthropic Claude integration)

;;; Code:

;;;; ACP (agent-shell)

(use-package shell-maker :vc (:url "https://github.com/xenodium/shell-maker" :rev :newest))
(use-package acp :vc (:url "https://github.com/xenodium/acp.el" :rev :newest))
(use-package agent-shell :vc (:url "https://github.com/xenodium/agent-shell" :rev :newest))

(require 'agent-shell)
(require 'agent-shell-anthropic)

(setq agent-shell-anthropic-claude-environment
      (agent-shell-make-environment-variables :inherit-env t))

(setq agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication :login t))

;;;; Helper Functions

(defun mu/get-agent-shell-buffer ()
  "Get the most recent agent-shell buffer, or nil if none exists."
  (let ((agent-buffers (seq-filter (lambda (buf)
                                     (string-match-p "^Claude Code Agent" (buffer-name buf)))
                                   (buffer-list))))
    (car (last agent-buffers))))

(defun mu/agent-shell-send-region-internal (buffer start end)
  "Send region from START to END to agent-shell BUFFER.
Uses agent-shell-add-region internally."
  ;; Activate the region in the current buffer
  (goto-char start)
  (push-mark end nil t)
  (activate-mark)
  ;; Now call agent-shell-add-region which will read the active region
  (agent-shell-add-region))

;;;; Interactive Commands

(defun mu/agent-shell-smart-switch ()
  "Smart agent-shell buffer switching:
- If agent-shell buffer exists and is displayed, switch to that window
- If agent-shell buffer exists but not displayed, switch to it and go to end
- If no agent-shell buffer exists, create one and go to end
- Never starts agent-shell inside nb-notes directory, always in containing git project"
  (interactive)
  (let* ((agent-buffer (mu/get-agent-shell-buffer))
         (agent-window (when agent-buffer (get-buffer-window agent-buffer)))
         (current-dir (or (when buffer-file-name
                            (file-name-directory buffer-file-name))
                          default-directory))
         (target-dir (mu/get-project-dir)))
    (cond
     ;; Agent buffer displayed - switch to its window
     (agent-window (select-window agent-window))
     ;; Agent buffer exists but not displayed - switch to it
     (agent-buffer (switch-to-buffer agent-buffer))
     ;; No agent buffer - create one in the appropriate directory
     (t
      (let ((default-directory target-dir))
        (agent-shell-anthropic-start-claude-code)
        ;; Find the newly created agent buffer
        (->> (mu/get-agent-shell-buffer)
             switch-to-buffer))))))

(defun mu/send-prompt-block-to-agent-shell (&optional arg)
  "Send surrounding prompt block to agent-shell.
Works with both org-mode blocks (#+begin_quote prompt) and markdown code blocks (```).
Does nothing if cursor is not inside such a block.
With prefix ARG, switch to agent-shell buffer after sending."
  (interactive "P")
  (let ((block-start nil)
        (block-end nil)
        (current-pos (point)))
    ;; Try to find surrounding block
    (save-excursion
      (cond
       ;; Check for org-mode #+begin_quote prompt block
       ((save-excursion
          (and (re-search-backward "^#\\+begin_quote[ \t]+prompt[ \t]*$" nil t)
               (let ((begin-pos (line-beginning-position 2)))
                 (when (re-search-forward "^#\\+end_quote[ \t]*$" nil t)
                   (let ((end-pos (1- (line-beginning-position))))
                     (when (and (<= begin-pos current-pos)
                                (>= end-pos current-pos))
                       (setq block-start begin-pos
                             block-end end-pos)
                       t))))))
        ;; Found org block, already set
        )
       ;; Check for markdown ``` block
       ((save-excursion
          (and (re-search-backward "^```" nil t)
               (let ((begin-pos (line-beginning-position 2)))
                 (when (re-search-forward "^```" nil t)
                   (let ((end-pos (1- (line-beginning-position))))
                     (when (and (<= begin-pos current-pos)
                                (>= end-pos current-pos))
                       (setq block-start begin-pos
                             block-end end-pos)
                       t))))))
        ;; Found markdown block, already set
        )))
    ;; Send block if found
    (if (and block-start block-end)
        (let ((agent-buffer (mu/get-agent-shell-buffer)))
          (unless agent-buffer
            (user-error "No agent-shell buffer found. Start one first with M-x agent-shell-anthropic-start-claude-code"))
          (mu/agent-shell-send-region-internal agent-buffer block-start block-end)
          (deactivate-mark)
          (when arg
            (pop-to-buffer agent-buffer)))
      (message "Cursor is not inside a prompt block"))))

(provide 'mu/agent-shell)

;;; agent-shell.el ends here
