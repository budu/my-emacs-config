;;; package --- Magit helpers
;;; Commentary:
;;; Code:

(defun mu/magit/quicksave ()
  "Stage everything, commit and push."
  (interactive)
  (let ((default-directory (magit-toplevel)))
    (if default-directory
        (progn
          ;; Stage all changes including untracked files
          (magit-stage-modified t)
          (magit-stage-untracked)

          ;; Create the commit
          (magit-commit-create
           (list "-m" "Quicksave"))

          ;; Push to origin
          (magit-push-current-to-pushremote nil))
      (message "Not in a git repository!"))))

(global-set-key (kbd "C-c q") 'mu/magit/quicksave)

;;; magit.el ends here
