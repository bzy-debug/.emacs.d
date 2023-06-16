(require 'awesome-tray)

(setq awesome-tray-mode-line-active-color "#be37a5")
(setq awesome-tray-mode-line-inactive-color "gray80")
(setq awesome-tray-location-format "%p")
(setq awesome-tray-date-format "%m-%d %a")
(setq awesome-tray-active-modules
      '("git" "location" "file-path" "mode-name" "battery" "date"))

(awesome-tray-mode 1)

(provide 'init-awesome-tray)
