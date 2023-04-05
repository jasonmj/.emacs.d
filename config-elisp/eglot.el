(use-package eldoc-box
  :ensure t
  :custom
  (eldoc-box-offset '(16 16 40))
  (eldoc-idle-delay 1.0)
  :hook (eglot-managed-mode . eldoc-box-hover-mode))

(use-package eglot
  :config
  (setq eglot-extend-to-xref t)
  (setq read-process-output-max (* 1024 1024))
  (push :documentHighlightProvider eglot-ignored-server-capabilities))
