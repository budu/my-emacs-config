;;; mu/overmind.el --- Control Overmind from Emacs (Projectile aware) -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Control Overmind from Emacs with these commands (Projectile project root):
;;;     - mu/overmind/quit:    C-c C-o q
;;;     - mu/overmind/restart: C-c C-o r  (choose a process to restart; C-u to restart all)
;;; Code:

(require 'subr-x)

(defgroup mu/overmind nil
  "Overmind helpers."
  :group 'tools
  :prefix "mu/overmind/")

(defcustom mu/overmind/use-compilation-buffer t
  "If non-nil, run commands via `compilation-start' so output is scrollable and ANSI-colored."
  :type 'boolean)

(defcustom mu/overmind/procfile-names-only t
  "If non-nil, list processes by parsing Procfile only.
If nil, and Procfile is missing or empty, fall back to `overmind status'
to try to discover process names."
  :type 'boolean)

(defcustom mu/overmind/command "overmind"
  "Command (or absolute path) to the Overmind binary."
  :type 'string
  :group 'mu/overmind)

(defun mu/overmind--resolve-bin ()
  "Resolve the Overmind binary path or signal a helpful error."
  (let* ((candidate mu/overmind/command)
         (abs (or (and (file-executable-p candidate) candidate)
                  (executable-find candidate))))
    (unless abs
      (user-error
       (concat
        "Cannot find `overmind`.\n\n"
        "- Set `mu/overmind/command` to your full path (e.g., /opt/homebrew/bin/overmind), or\n"
        "- Ensure Emacs PATH includes the directory. For macOS GUI Emacs:\n"
        "  (use-package exec-path-from-shell :config (exec-path-from-shell-initialize))\n\n"
        "Emacs exec-path:\n  %s")
       (mapconcat #'identity exec-path "\n  ")))
    abs))

(defun mu/overmind/doctor ()
  "Show diagnostics for Overmind integration."
  (interactive)
  (let* ((found (executable-find mu/overmind/command))
         (msg (if found
                  (format "OK: overmind found at %s\n\nexec-path:\n  %s"
                          found (mapconcat #'identity exec-path "\n  "))
                (format "NOT FOUND: `%s`\n\nTry one of:\n  - Set `mu/overmind/command` to full path\n  - Add your Homebrew bin to PATH/exec-path\n\nexec-path:\n  %s"
                        mu/overmind/command (mapconcat #'identity exec-path "\n  ")))))
    (with-current-buffer (get-buffer-create "*overmind: doctor*")
      (erase-buffer)
      (insert msg "\n")
      (special-mode)
      (display-buffer (current-buffer)))))

;; Patch mu/overmind--run to use the resolved binary
(defun mu/overmind--run (cmd &optional buffer-name)
  "Run Overmind CMD in the project root.
If `mu/overmind/use-compilation-buffer' is non-nil, use `compilation-start',
otherwise use `async-shell-command'.  Optional BUFFER-NAME selects the target buffer."
  (let* ((default-directory (mu/overmind--project-root))
         (bin (mu/overmind--resolve-bin))
         (full-cmd (if (string-match-p "\\`overmind\\b" cmd)
                       ;; user passed full 'overmind ...' string; replace head with resolved bin
                       (replace-regexp-in-string "\\`overmind\\b" (shell-quote-argument bin) cmd)
                     (format "%s %s" (shell-quote-argument bin) cmd)))
         (bufname (or buffer-name (format "*overmind: %s*" cmd))))
    (if mu/overmind/use-compilation-buffer
        (let ((compilation-scroll-output t))
          (compilation-start full-cmd 'shell-command-mode (lambda (_) bufname)))
      (async-shell-command full-cmd bufname))))

(defun mu/overmind--project-root ()
  "Return the project root using Projectile or project.el, or `default-directory' as a last resort."
  (or
   (when (featurep 'projectile)
     (ignore-errors (projectile-project-root)))
   (when (fboundp 'project-current)
     (let* ((proj (ignore-errors (project-current)))
            (root (ignore-errors (car (project-roots proj)))))
       root))
   default-directory))

(defun mu/overmind--run (cmd &optional buffer-name)
  "Run Overmind CMD in the project root.
If `mu/overmind/use-compilation-buffer' is non-nil, use `compilation-start',
otherwise use `async-shell-command'.  Optional BUFFER-NAME selects the target buffer."
  (let* ((default-directory (mu/overmind--project-root))
         (full-cmd (if (string-match-p "\\`overmind\\b" cmd) cmd (concat "overmind " cmd)))
         (bufname (or buffer-name (format "*overmind: %s*" cmd))))
    (if mu/overmind/use-compilation-buffer
        ;; Use compilation mode for nice paging & clickable errors; also enables ansi-color.
        (let ((compilation-scroll-output t))
          (compilation-start full-cmd 'shell-command-mode (lambda (_) bufname)))
      (async-shell-command full-cmd bufname))))

(defun mu/overmind--procfile-processes ()
  "Return a list of process names parsed from Procfile, if present."
  (let* ((root (mu/overmind--project-root))
         (procfile (and root (expand-file-name "Procfile" root))))
    (when (and procfile (file-readable-p procfile))
      (with-temp-buffer
        (insert-file-contents procfile)
        (let (names)
          (dolist (line (split-string (buffer-string) "\n" t))
            (setq line (string-trim line))
            (unless (or (string-empty-p line)
                        (string-prefix-p "#" line)
                        (not (string-match ":" line)))
              (push (string-trim (substring line 0 (match-beginning 0))) names)))
          (nreverse names))))))

(defun mu/overmind--status-processes ()
  "Try to derive process names from `overmind status'."
  (let* ((default-directory (mu/overmind--project-root))
         (out (ignore-errors (shell-command-to-string "overmind status 2>/dev/null"))))
    (when (and out (not (string-empty-p (string-trim out))))
      ;; Heuristic: lines often look like 'proc_name  RUNNING  (pid ...)'
      (let (names)
        (dolist (line (split-string out "\n" t))
          (setq line (string-trim line))
          (when (and (not (string-empty-p line))
                     ;; First token up to whitespace is commonly the proc name
                     (string-match "\\`\\([^[:space:]]+\\)[[:space:]]+" line))
            (push (match-string 1 line) names)))
        (delete-dups (nreverse names))))))

(defun mu/overmind--choose-process ()
  "Prompt for a process name, using Procfile or `overmind status' as sources."
  (let* ((from-procfile (mu/overmind--procfile-processes))
         (candidates (or from-procfile
                         (unless mu/overmind/procfile-names-only
                           (mu/overmind--status-processes)))))
    (cond
     (candidates
      (completing-read "Overmind process: " candidates nil t))
     (t
      ;; Fall back to free text if we couldn't find any names.
      (read-string "Overmind process (name as in Procfile): ")))))

;;;###autoload
(defun mu/overmind/quit ()
  "Run `overmind quit' in the current Projectile/project root."
  (interactive)
  (mu/overmind--run "quit" "*overmind: quit*"))

;;;###autoload
(defun mu/overmind/restart (arg)
  "Restart a process via Overmind in the current project.
With prefix ARG (C-u), restart all processes (i.e., `overmind restart')."
  (interactive "P")
  (if arg
      (mu/overmind--run "restart" "*overmind: restart all*")
    (let ((proc (mu/overmind--choose-process)))
      (mu/overmind--run (format "restart %s" proc)
                        (format "*overmind: restart %s*" proc)))))

(defun mu/overmind--choose-and (fmt)
  "Select a process and run FMT (a format string like \"restart %s\")."
  (let ((proc (mu/overmind--choose-process)))
    (mu/overmind--run (format fmt proc)
                      (format "*overmind: %s*" (format fmt proc)))))

(defun mu/overmind/status ()
  "Show `overmind status' in the project root."
  (interactive)
  (mu/overmind--run "status" "*overmind: status*"))

(defun mu/overmind/start ()
  "Start a single process via Overmind."
  (interactive)
  (mu/overmind--choose-and "start %s"))

(defun mu/overmind/stop ()
  "Stop a single process via Overmind."
  (interactive)
  (mu/overmind--choose-and "stop %s"))

(defun mu/overmind/run (cmd)
  "Run an arbitrary Overmind subcommand line (after 'overmind ')."
  (interactive "sovermind ")
  (mu/overmind--run cmd (format "*overmind: %s*" cmd)))

(defvar mu/overmind--mode-map
  (let ((m (make-sparse-keymap)))
    ;; originals
    (define-key m (kbd "C-c C-o q") #'mu/overmind/quit)
    (define-key m (kbd "C-c C-o r") #'mu/overmind/restart) ; C-u to restart all

    ;; extras
    (define-key m (kbd "C-c C-o s") #'mu/overmind/status)
    (define-key m (kbd "C-c C-o p") #'mu/overmind/start)
    (define-key m (kbd "C-c C-o x") #'mu/overmind/stop)
    (define-key m (kbd "C-c C-o a")
      (lambda () (interactive)
        (mu/overmind--run "restart" "*overmind: restart all*")))
    (define-key m (kbd "C-c C-o !") #'mu/overmind/run)
    m)
  "Keymap for `mu/overmind-mode'.")

;;;###autoload
(define-minor-mode mu/overmind-mode
  "Minor mode to control Overmind from Emacs."
  :lighter " OM"
  :keymap mu/overmind--mode-map)

;;;###autoload
(defun mu/overmind/enable ()
  "Enable `mu/overmind-mode' in the current buffer."
  (interactive)
  (mu/overmind-mode 1))

(provide 'mu/overmind)

;; --- Auto-enable in projects -------------------------------------------------

(defcustom mu/overmind/auto-enable t
  "If non-nil, automatically enable `mu/overmind-mode' in matching projects."
  :type 'boolean :group 'mu/overmind)

(defcustom mu/overmind/indicator-files '("Procfile" ".overmind.sock" ".overmind.env")
  "Files whose presence in the project root indicates Overmind usage."
  :type '(repeat string) :group 'mu/overmind)

(defun mu/overmind--project-has-indicator-p ()
  "Return non-nil if the current project root contains any indicator files."
  (when-let* ((root (mu/overmind--project-root)))
    (seq-some (lambda (f) (file-exists-p (expand-file-name f root)))
              mu/overmind/indicator-files)))

(defun mu/overmind--maybe-enable ()
  "Enable `mu/overmind-mode' in this buffer when appropriate."
  (when (and mu/overmind/auto-enable
             (mu/overmind--project-root)
             (mu/overmind--project-has-indicator-p))
    (mu/overmind-mode 1)))

;; Turn it into a globalized minor mode so it follows you around
(define-globalized-minor-mode mu/overmind-global-mode
  mu/overmind-mode
  mu/overmind--maybe-enable)

;; Keep it responsive as you open files / switch projects
(add-hook 'find-file-hook #'mu/overmind--maybe-enable)
(with-eval-after-load 'project
  (add-hook 'project-switch-project-hook #'mu/overmind--maybe-enable))
(with-eval-after-load 'projectile
  (add-hook 'projectile-after-switch-project-hook #'mu/overmind--maybe-enable))

;; Enable globally (you can put this in your init file)
(mu/overmind-global-mode 1)

;;; mu/overmind.el ends here
