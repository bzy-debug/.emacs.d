(require 'mini-frame)

(setq mini-frame-show-parameters
      '((top   . 0.2)
        (width . 0.8)
        (left  . 0.5)))
(when (eq system-type 'darwin)
  (setq mini-frame-background-color-function
        (lambda ()
          (pcase ns-system-appearance
            ('dark "Black")
            ('light "White")))))
(setq mini-frame-ignore-commands nil)

(mini-frame-mode)

(provide 'init-mini-frame)
