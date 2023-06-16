(setq package-enable-at-startup nil)                ;disbale package.el
(setq read-process-output-max (* 1024 1024))        ;lsp-mode
(setq gc-cons-threshold 100000000)                  ;lsp-mode
(setq inhibit-startup-screen t)                     ;no gnu hello
(setq native-comp-async-report-warnings-errors nil) ;no native compilation warnings

(tool-bar-mode -1)                                  ;disable tool bar
(scroll-bar-mode -1)                                ;disable scroll bar
