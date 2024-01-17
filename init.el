;; -*- lexical-binding: t -*-

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (setq split-width-threshold 80)                           ;split using left right manner
  (files--ensure-directory "~/.local")
  (files--ensure-directory "~/.local/state")
  (files--ensure-directory "~/.local/state/emacs")
  (files--ensure-directory "~/.local/state/emacs/auto-save")
  (files--ensure-directory "~/.local/state/emacs/backup")
  (setq auto-save-file-name-transforms
        '((".*" "~/.local/state/emacs/auto-save/" t)))                ;set auto save file directory
  (setq backup-directory-alist
        '(("." . "~/.local/state/emacs/backup/")))                    ;set backup directory
  (setq backup-by-copying t)
  (setq tab-always-indent 'complete)                        ;tab to complete
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
  (set-face-attribute 'variable-pitch nil :family "Sarasa Term SC Nerd" :height 170)

  (show-paren-mode -1)
  (setq show-paren-delay 0)
  (show-paren-mode 1)

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
  :bind
  (("C-S-h" . windmove-left)
   ("C-S-j" . windmove-down)
   ("C-S-k" . windmove-up)
   ("C-S-l" . windmove-right))
  :hook
  ((prog-mode . hl-line-mode)
   (before-save . delete-trailing-whitespace)))

(use-package ef-themes
  :config
  (load-theme 'ef-light :no-confirm))

(use-package vertico
  :init
  (vertico-mode 1))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package ctrlf
  :config
  (ctrlf-mode 1))

(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  (yas-global-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(basic partial-completion orderless))
  (setq completion-category-overrides nil)
  (setq completion-category-defaults nil))

(use-package mini-frame
  :config
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

  (mini-frame-mode))

(use-package rainbow-delimiters
  :hook
  ((prog-mode . rainbow-delimiters-mode-enable)))

(use-package magit)

(use-package paredit
  :hook
  ((scheme-mode . paredit-mode)
   (lisp-mode . paredit-mode)
   (emacs-lisp-mode . paredit-mode)))

(use-package which-key
  :init
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1))

(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-files-exclusions '(".git/" ".DS_Store"))
  :hook
  ((prog-mode . company-mode)))

(use-package dashboard
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-banner-logo-title "Welcome to Emacs")
  (setq dashboard-startup-banner (file-truename "~/.emacs.d/icon_128x128.png"))
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-items '((recents . 5)
                          (projects . 5)))
  (dashboard-setup-startup-hook))

(use-package proof-general
  :init
  (setq proof-splash-enable nil))

(use-package company-coq
  :hook
  ((coq-mode . company-coq-mode)))

(use-package term
  :config
  (fset 'term 'ansi-term)
  (keymap-global-set "C-c t" #'term)
  (define-key term-raw-map (kbd "C-c C-y") #'term-paste))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet-snippets ctrlf exec-path-from-shell proof-general proof-site company-coq emacs-surround dashboard company which-key paredit magit rainbow-delimiters orderless mini-frame marginalia vertico ef-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
