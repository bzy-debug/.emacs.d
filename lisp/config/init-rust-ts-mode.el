(require 'cargo)

(add-hook 'rust-ts-mode-hook #'cargo-minor-mode)

(provide 'init-rust-ts-mode)
