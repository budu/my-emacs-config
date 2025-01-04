;;; package --- Auto-clean buffers
;;; Commentary:
;;; Code:

(defun mu/auto-clean-buffers/buffer-old-p (buffer)
  "Return t if BUFFER hasn't been accessed in the last 24 hours."
  (let ((time (current-time)))
    (when-let ((buffer-time (buffer-local-value 'buffer-display-time buffer)))
      (> (float-time (time-subtract time buffer-time))
         (* 24 60 60)))))

(defun mu/auto-clean-buffers/get-old-buffers ()
  "Return a list of buffers that haven't been accessed in 24 hours."
  (seq-filter
   (lambda (buf)
     (and (mu/auto-clean-buffers/buffer-old-p buf)
          (not (buffer-modified-p buf))     ; Skip modified buffers
          (not (string-match-p "\\`[ *]" (buffer-name buf))) ; Skip special buffers
          (not (get-buffer-process buf))))  ; Skip buffers with active processes
   (buffer-list)))

(defun mu/auto-clean-buffers/cleanup-old-buffers ()
  "Close buffers that haven't been accessed in 24 hours."
  (interactive)
  (let ((old-buffers (mu/auto-clean-buffers/get-old-buffers)))
    (when old-buffers
      (message "Closing %d unused buffer(s)" (length old-buffers))
      (mapc 'kill-buffer old-buffers))))

(defun mu/auto-clean-buffers/setup-automatic-buffer-cleanup ()
  "Set up automatic buffer cleanup to run every hour."
  (interactive)
  (run-with-timer 0 3600 'mu/auto-clean-buffers/cleanup-old-buffers))  ; Run every hour (3600 seconds)

;; Enable automatic cleanup
(mu/auto-clean-buffers/setup-automatic-buffer-cleanup)

;;; auto-clean-buffers.el ends here
