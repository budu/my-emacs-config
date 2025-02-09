;;; package --- CG job-related helpers
;;; Commentary:
;;; Code:

(require 'org)

(defun mu/cg/create-ticket-todo (ticket-number)
  "Create a TODO entry for the specified TICKET-NUMBER."
  (interactive "nTicket number: ")
  (let ((today (format-time-string "%Y-%m-%d"))
        (template-file (expand-file-name "nb-notes/templates/ticket.org")))
    (if (file-exists-p template-file)
        (progn
          (find-file template-file)
          (let ((template-content (buffer-string)))
            (kill-buffer)
            (org-insert-heading)
            (insert (format "TODO [#A] Ticket %d :ticket:\n" ticket-number))
            (org-schedule nil today)
            (insert "\n")
            (insert template-content)
            (save-excursion
              (while (search-forward "NUMBER" nil t)
                (replace-match (number-to-string ticket-number) t t)))))
      (message "Template file not found: %s" template-file))))

;;; cg.el ends here
