;; -*- lexical-binding: t -*-

(require 'package)
(setq package-check-signature nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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

(use-package magit
  :init
  (setq magit-define-global-key-bindings 'recommended))

(use-package git-timemachine)

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

(defun magit-help-create-worktree ()
  "Quickly create a worktree from origin/master"
  (interactive)
  (let* ((branch-name (read-string "New branch: "))
         (repo-path (magit-toplevel))
         (new-dir-name (car (last (split-string branch-name "/"))))
         (target-path (expand-file-name new-dir-name (file-name-directory (directory-file-name repo-path)))))
    (magit-worktree-branch target-path branch-name (if (magit-branch-p "origin/main") "origin/main" "origin/master"))))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(git-timemachine yasnippet-snippets which-key vertico tuareg slime rainbow-delimiters proof-general paredit orderless opam-switch-mode multiple-cursors mini-frame marginalia forge fireplace exec-path-from-shell ef-themes dashboard ctrlf company-coq avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
