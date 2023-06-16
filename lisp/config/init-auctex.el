(require 'cdlatex)
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)

(defun my-LaTex-mode-hook ()
  (cdlatex-mode 1)
  (setq preview-scale-function
        (lambda () (* 0.75
                 (funcall (preview-scale-from-face))))))

(add-hook 'LaTeX-mode-hook #'my-LaTex-mode-hook)

(provide 'init-auctex)
