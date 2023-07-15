;; -*- lexical-binding: t -*-

(require 'cl-lib)

;; take from lazycat emacs
;; https://github.com/manateelazycat/lazycat-emacs/blob/master/site-start.el
(defun add-subdirs-to-load-path (search-dir)
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; 过滤出不必要的目录，提升Emacs启动速度
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   ;; 不是目录的文件都移除
                   (not (file-directory-p (concat dir subdir)))
                   ;; 父目录、 语言相关和版本控制目录都移除
                   (member subdir '("." ".."
                                    "dist" "node_modules" "__pycache__"
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github"))))
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; 目录下有 .el .so .dll 文件的路径才添加到 `load-path' 中，提升Emacs启动速度
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非Elisp语言编写的Emacs动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))

          ;; 注意：`add-to-list' 函数的第三个参数必须为 t ，表示加到列表末尾
          ;; 这样Emacs会从父目录到子目录的顺序搜索Elisp插件，顺序反过来会导致Emacs无法正常启动
          (add-to-list 'load-path subdir-path t))

        ;; 继续递归搜索子目录
        (add-subdirs-to-load-path subdir-path)))))

(add-subdirs-to-load-path "~/.emacs.d/lisp")

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(require 'init-default)
;; UI
(require 'init-ctrlf)
(require 'init-dashboard)
(require 'init-mini-frame)
(require 'init-rainbow-delimiters)
(require 'init-theme)
(require 'init-vertico)
;; Utility
(require 'init-avy)
(require 'init-crux)
(require 'init-emacs-surround)
(require 'init-eshell)
(require 'init-exec-path)
(require 'init-ielm)
(require 'init-magit)
(require 'init-multiple-cursors)
(require 'init-orderless)
(require 'init-paredit)
(require 'init-socks)
(require 'init-term)
(require 'init-treesit)
(require 'init-which-key)
(require 'init-blink-search)
(require 'init-dirvish)
(require 'init-company)
(require 'init-company-coq)
;; Programming
(require 'haskell-mode)
(require 'init-lsp-bridge)
(require 'init-slime)
(require 'init-yasnippet)
(require 'init-tuareg)
(require 'markdown-mode)
(require 'zig-mode)
(require 'init-rust-ts-mode)
(require 'init-PG)
;; Org
;; (require 'init-org)
;; (require 'init-org-roam)
;; (require 'cdlatex)
;; Latex
;; (require 'init-auctex)

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
(setq auto-mode-alist
   (append
     '(("\\.agda\\'" . agda2-mode)
       ("\\.lagda.md\\'" . agda2-mode))
     auto-mode-alist))
