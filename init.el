;; -*- lexical-binding: t -*-

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package emacs
  :init
  (setq split-width-threshold 80)
  (unless (file-directory-p "~/.local/state/emacs/auto-save")
    (make-directory "~/.local/state/emacs/auto-save" :parent))
  (unless (file-directory-p "~/.local/state/emacs/backup")
    (make-directory "~/.local/state/emacs/backup" :parent))

  (setq auto-save-file-name-transforms
        '((".*" "~/.local/state/emacs/auto-save/" t)))
  (setq backup-directory-alist
        '(("." . "~/.local/state/emacs/backup/")))
  (setq backup-by-copying t)
  (setq tab-always-indent 'complete)
  (setq ring-bell-function 'ignore)
  (setq use-short-answers t)
  (setq initial-scratch-message nil)
  (setq confirm-kill-processes nil)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default cursor-type 'bar)
  (setq url-proxy-services '(("http" . "http://127.0.0.1:7890")
                             ("https" . "http://127.0.0.1:7890")))
  (setq project-switch-commands '((project-find-file "Find file" "f")
                                  (project-find-dir "Find dir" "d")
                                  (project-dired "Dired" "D")
                                  (consult-ripgrep "ripgrep" "g")
                                  (magit-project-status "Magit" "m")))

  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))

  (set-face-attribute 'default nil :family "Sarasa Term SC Nerd" :height 170)
  (set-face-attribute 'fixed-pitch nil :family "Sarasa Term SC Nerd" :height 170)
  (set-face-attribute 'variable-pitch nil :family "Sarasa Term SC Nerd" :height 170)

  (show-paren-mode -1)
  (setq show-paren-delay 0)
  (show-paren-mode 1)

  (blink-cursor-mode -1)
  (mouse-wheel-mode -1)
  (pixel-scroll-precision-mode 1)
  (repeat-mode 1)
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (display-battery-mode 1)
  (electric-pair-mode 1)
  (global-visual-line-mode 1)
  (global-subword-mode 1)
  (global-auto-revert-mode 1)
  (global-prettify-symbols-mode 1)
  (delete-selection-mode 1)
  (fido-vertical-mode 1)

  (defun ns-system-appearance-changed (new-system-appearance)
    "change theme when system appearance changes"
    (pcase new-system-appearance
      ('light (load-theme 'ef-light :no-confirm))
      ('dark (load-theme 'ef-dark :no-confirm))))

  (when (eq system-type 'darwin)
    (setq ns-use-native-fullscreen t
          mac-command-modifier 'meta
          mac-option-modifier 'super
          frame-resize-pixelwise t)
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

(use-package ctrlf
  :config
  (ctrlf-mode 1))

(use-package rainbow-delimiters
  :hook
  ((prog-mode . rainbow-delimiters-mode-enable)))

(use-package magit)

(use-package paredit
  :hook
  ((scheme-mode     . paredit-mode)
   (lisp-mode       . paredit-mode)
   (emacs-lisp-mode . paredit-mode)))

(use-package which-key
  :init
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1))

(use-package dashboard
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-banner-logo-title "Welcome to Emacs")
  (setq dashboard-startup-banner (file-truename "~/.emacs.d/icon_128x128.png"))
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-items '((recents . 5)
                          (projects . 5)))
  (dashboard-setup-startup-hook))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))
