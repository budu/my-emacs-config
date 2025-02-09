;;; package --- CG job-related helpers
;;; Commentary:
;;; Code:

(require 'org)

(defun mu/cg/create-ticket-todo (ticket-number)
  "Create a TODO entry for the specified TICKET-NUMBER."
  (interactive "nTicket number: ")
  (let ((today (format-time-string "%Y-%m-%d"))
        (template-file (expand-file-name "templates/ticket.org")))
    (if (file-exists-p template-file)
        (progn
          (find-file template-file)
          (let ((template-content (buffer-string)))
            (kill-buffer)
            (org-insert-heading)
            (insert (format "TODO [#A] [[https://cg-project.codegenome.com/projects/49/stories/%d][Ticket #%d]] :ticket:\n"
                          ticket-number ticket-number))
            (org-schedule nil today)
            (insert "\n")
            (insert template-content)
            (save-excursion
              (while (search-backward "NUMBER" nil t)
                (replace-match (number-to-string ticket-number) t t)))))
            ;; TODO: turn the Ticket #<number> in the heading into a link to the ticket
      (message "Template file not found: %s" template-file))))

(define-key mu/cg-map (kbd "t") 'mu/cg/create-ticket-todo)

;;; cg.el ends here
