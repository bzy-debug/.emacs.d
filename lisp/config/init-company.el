(require 'company)

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-files-exclusions '(".git/" ".DS_Store"))

(provide 'init-company)
