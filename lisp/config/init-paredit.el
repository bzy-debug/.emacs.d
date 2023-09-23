(require 'paredit)

(dolist (hook '(scheme-mode-hook
                lisp-mode-hook
                emacs-lisp-mode-hook
                inferior-emacs-lisp-mode-hook
                racket-mode-hook))
  (add-hook hook #'paredit-mode))

(provide 'init-paredit)
