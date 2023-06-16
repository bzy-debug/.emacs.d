(require 'multiple-cursors)

(keymap-global-set "C->"           #'mc/mark-next-like-this)
(keymap-global-set "C-<"           #'mc/mark-previous-like-this)
(keymap-global-set "C-S-<mouse-1>" #'mc/add-cursor-on-click)

(setq mc/always-repeat-command t)

(provide 'init-multiple-cursors)
