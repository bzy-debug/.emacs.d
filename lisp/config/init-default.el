(setq split-width-threshold 80)                           ;split using left right manner
(setq tramp-default-method "ssh")                         ;faster tramp
(setq tab-always-indent 'complete)                        ;tab to complete
(setq display-line-numbers-type 'relative)                ;relateive line numbers
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-save/" t)))                ;set auto save file directory
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup/")))                    ;set backup directory
(setq backup-by-copying t)
(setq ring-bell-function 'ignore)                         ;no ring bell
(setq use-short-answers t)                                ;y-n instead of yes-no
(setq initial-scratch-message nil)
(setq confirm-kill-processes nil)                         ;stop asking "Active processes..."
(setq-default indent-tabs-mode nil)                       ;use space to indent
(setq-default tab-width 2)                                ;set tab width to 2
(setq-default cursor-type 'bar)                           ;change cursor type

(setq url-proxy-services
      '(("http" . "127.0.0.1:7890")
        ("https" . "127.0.0.1:7890")))                    ;setup proxy

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))                 ;kill buffer with process directly

(set-face-attribute 'default nil :family "Sarasa Term SC Nerd" :height 170)
(set-face-attribute 'fixed-pitch nil :family "Sarasa Term SC Nerd" :height 170)
(set-face-attribute 'variable-pitch nil :family "Charter" :height 170)
(set-fontset-font t '(#x2ff0 . #x9ffc) (font-spec :family "STSong" :size 17 :weight 'bold)) ;set chinese font

(blink-cursor-mode -1)                                    ;no blinking cursor
(mouse-wheel-mode -1)

(pixel-scroll-precision-mode 1)                           ;smooth scroll
(repeat-mode 1)                                           ;repeat command
(recentf-mode 1)                                          ;record recent files
(savehist-mode 1)                                         ;record command hist
(save-place-mode 1)                                       ;record last edit palce
(display-battery-mode 1)
(electric-pair-mode 1)                                    ;auto pair
(global-visual-line-mode 1)                               ;handle line break gracefully
(global-subword-mode 1)                                   ;handle CamelCase gracefully
(global-auto-revert-mode 1)                               ;auto revert buffer
(global-prettify-symbols-mode 1)

(add-hook 'before-save-hook #'delete-trailing-whitespace) ;delete trailing whitespace on save
(add-hook 'prog-mode-hook #'hl-line-mode)                 ;highlight current line on programming
(add-hook 'prog-mode-hook #'display-line-numbers-mode)    ;dispaly line numbers on programming

(keymap-global-set "C-S-h" #'windmove-left)
(keymap-global-set "C-S-j" #'windmove-down)
(keymap-global-set "C-S-k" #'windmove-up)
(keymap-global-set "C-S-l" #'windmove-right)


;;take from vertico
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setq enable-recursive-minibuffers t)

;;mac specific config
(defun ns-system-appearance-changed (new-system-appearance)
  "change theme when system appearance changes"
  (pcase new-system-appearance
    ('light (load-theme 'ef-light :no-confirm))
    ('dark (load-theme 'ef-dark :no-confirm))))

(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen t   ;mac native fullscreen
        mac-command-modifier 'meta   ;command as meta
        mac-option-modifier 'super   ;option as super
        frame-resize-pixelwise t)    ;real fullsize for window manager
  (add-hook 'ns-system-appearance-change-functions
            #'ns-system-appearance-changed)
  (add-to-list 'default-frame-alist
               '(ns-transparent-titlebar . t)))

(provide 'init-default)
