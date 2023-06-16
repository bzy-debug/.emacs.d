(require 'org-roam)

(keymap-global-set "C-c n l" #'org-roam-buffer-toggle)
(keymap-global-set "C-c n f" #'org-roam-node-find)
(keymap-global-set "C-c n g" #'org-roam-graph)
(keymap-global-set "C-c n i" #'org-roam-node-insert)
(keymap-global-set "C-c n c" #'org-roam-capture)
(keymap-global-set "C-c n j" #'org-roam-dailies-capture-today)

(setq org-roam-directory (concat org-directory "/roam"))
(org-roam-db-autosync-mode)

(provide 'init-org-roam)
