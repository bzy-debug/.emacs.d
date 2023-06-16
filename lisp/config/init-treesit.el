(require 'treesit-auto)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(setq treesit-auto-install 'prompt)
(global-treesit-auto-mode)

(provide 'init-treesit)
