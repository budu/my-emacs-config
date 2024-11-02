;;; arrows.el --- Arrow macros for Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;;   https://github.com/tkurtbond/arrow-macros-for-emacs/blob/master/arrows.el
;;; Code:

(defun mu/arrows/simple-inserter (insert-fun)
  "Return an inserter using INSERT-FUN."
  (lambda (acc next)
    (if (listp next)
        (funcall insert-fun acc next)
      (list next acc))))

(defun mu/arrows/insert-first (arg surround)
  "Insert ARG into the list form SURROUND as its first argument."
  (cl-list* (car surround)
            arg
            (cdr surround)))

(defun mu/arrows/insert-last (arg surround)
  "Insert ARG into the list form SURROUND as its last argument."
  (append surround (list arg)))

(defmacro -> (initial-form &rest forms)
  "Insert INITIAL-FORM as first argument into FORMS recursively."
  (cl-reduce (mu/arrows/simple-inserter #'mu/arrows/insert-first)
             forms
             :initial-value initial-form))

(defmacro ->> (initial-form &rest forms)
  "Insert INITIAL-FORM as last argument into FORMS recursively."
  (cl-reduce (mu/arrows/simple-inserter #'mu/arrows/insert-last)
             forms
             :initial-value initial-form))

(provide 'arrows)

;;; arrows.el ends here
