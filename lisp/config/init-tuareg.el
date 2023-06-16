(require 'tuareg)

(add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.topml$" . tuareg-mode))

(provide 'init-tuareg)
