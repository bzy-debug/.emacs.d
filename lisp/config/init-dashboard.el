(require 'dashboard)

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq dashboard-banner-logo-title "Welcome to Emacs")
(setq dashboard-startup-banner (file-truename "~/.emacs.d/icon_128x128.png"))
(setq dashboard-projects-backend 'project-el)
(setq dashboard-items '((recents . 5)
                        (projects . 5)))

(dashboard-setup-startup-hook)

(provide 'init-dashboard)
