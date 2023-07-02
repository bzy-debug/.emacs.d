(require 'tuareg)
(require 'ocamlformat)

(defun my-tuareg-mode-hook ()
  (keymap-set tuareg-mode-map "C-M-<tab>" #'ocamlformat)
  (ocamlformat-setup-indent)
  (ocamlformat-set-newline-and-indent)
  (add-hook 'before-save-hook #'ocamlformat-before-save))

(add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.topml$" . tuareg-mode))

(provide 'init-tuareg)
