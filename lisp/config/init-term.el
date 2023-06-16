(require 'term)

(fset 'term 'ansi-term)

(keymap-global-set "C-c t" #'term)
(define-key term-raw-map (kbd "C-c C-y") #'term-paste)

(provide 'init-term)
