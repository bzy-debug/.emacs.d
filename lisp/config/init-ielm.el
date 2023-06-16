(require 'ielm)

(defun ielm/clear-repl ()
  "Clear current REPL buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (ielm-send-input)))

(define-key inferior-emacs-lisp-mode-map (kbd "M-RET") #'ielm-return)
(define-key inferior-emacs-lisp-mode-map (kbd "C-<return>") #'ielm-return)
(define-key inferior-emacs-lisp-mode-map (kbd "C-c C-q") #'ielm/clear-repl)

(provide 'init-ielm)
