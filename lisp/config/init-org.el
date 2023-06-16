(add-to-list 'load-path "~/.emacs.d/lisp/packages/org/lisp")
(require 'org)
(require 'ox-latex)

(setq org-latex-compiler "pdflatex")
(setq org-latex-pdf-process
      '("latexmk -cd -jobname=%b -auxdir=%b-build -outdir=%o -interaction=nonstopmode -%latex -f %f"))
(setq org-directory "~/org")
(setq org-confirm-babel-evaluate nil)
(setq org-hide-emphasis-markers t)
(setq org-pretty-entities t)

(thread-first
  (alist-get 'dvisvgm org-latex-preview-process-alist)
  (plist-get :image-converter)
  (setf "dvisvgm --page=1- --libgs=/opt/homebrew/bin/gs --optimize --clipjoin --relative --no-fonts --bbox=preview -o %B-%%9p.svg %f"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C          . t)
   (python     . t)))

(defun my/org-mode-hook ()
  (org-indent-mode 1)
  (org-latex-preview-auto-mode 1)
  (org-cdlatex-mode 1)
  (variable-pitch-mode)

  (setq left-margin-width 5
        right-margin-width 5)
  (set-window-buffer nil (current-buffer))

  (setq-local electric-pair-inhibit-predicate
              `(lambda (c)
                 (if (char-equal c ?<) t
                   (,electric-pair-inhibit-predicate c))))

  (dolist (face '(org-block
                  org-table
                  org-code
                  org-verbatim
                  org-block-begin-line
                  org-block-end-line
                  org-meta-line
                  org-date
                  org-drawer
                  org-property-value
                  org-special-keyword
                  org-document-info-keyword))
    (set-face-attribute face nil :inherit 'fixed-pitch)))

(add-hook 'org-mode-hook #'my/org-mode-hook)

(provide 'init-org)
