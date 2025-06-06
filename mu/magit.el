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

(defun mu/magit/open-parent ()
  "Open magit for the root of the project."
  (interactive)
  (let* ((toplevel (magit-toplevel))
         (parent-dir (when (and toplevel
                                (string-match-p "/nb-notes/?$" toplevel))
                       (file-name-directory (directory-file-name toplevel)))))
    (if parent-dir
        (magit-status parent-dir)
      (magit-status))))

(global-set-key (kbd "C-x g") 'mu/magit/open-parent)
(global-set-key (kbd "C-x C-g") 'magit-status)

;;; magit.el ends here
