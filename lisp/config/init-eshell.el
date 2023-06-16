(require 'eshell)
(require 'with-proxy)

(defun my-proxy-around (&rest args)
  "Wrap a function with proxy configration"
  (with-proxy
   :http-server "127.0.0.1:7890"
   (apply args)))

(defun eshell-other-window ()
    "Open a `eshell' in a new window."
    (interactive)
    (let ((buf (eshell)))
      (switch-to-buffer (other-buffer buf))
      (switch-to-buffer-other-window buf)))

(keymap-global-set "C-c e" #'eshell)
(keymap-global-set "C-x 4 e" #'eshell-other-window)
(advice-add 'eshell :around #'my-proxy-around)

(provide 'init-eshell)
