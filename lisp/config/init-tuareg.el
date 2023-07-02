(require 'tuareg)
(require 'opam-user-setup)
(require 'ocamlformat)
(require 'dune)

(add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.topml$" . tuareg-mode))

(defun my-tuareg-mode-hook ()
  (keymap-set tuareg-mode-map "C-M-<tab>" #'ocamlformat)
  (add-hook 'before-save-hook #'ocamlformat-before-save)
  (ocamlformat-setup-indent)
  (ocamlformat-set-newline-and-indent))

(add-hook 'tuareg-mode-hook #'my-tuareg-mode-hook)

(add-hook 'dune-mode-hook #'paredit-mode)

(provide 'init-tuareg)
