(require 'avy)

(keymap-global-set "C-." #'avy-goto-word-1)
(keymap-global-set "C-;" #'avy-goto-char-2)

(provide 'init-avy)
