(require 'magit)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/lisp/extensions/magit/Documentation/"))

(provide 'init-magit)
