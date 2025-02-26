(use-package eldoc-box
  :ensure t
  :hook (eglot--managed-mode . eldoc-box-hover-mode)
  :bind (("s-TAB" . eldoc-box-help-at-point))
  :config
  (custom-set-faces `(eldoc-box-body ((t (:background ,(face-attribute 'default :background) :foreground ,(face-attribute 'default :foreground))))))
  (custom-set-faces `(eldoc-box-border ((t (:background ,(face-attribute 'default :foreground))))))
  (eval-after-load 'eglot-managed-mode (pop mode-line-misc-info))
  :custom
  (eglot-menu-string "")
  (eldoc-box-offset '(16 16 40))
  (eldoc-box-max-pixel-height 450)
  (eldoc-documentation-strategy 'eldoc-documentation-enthusiast)
  (eldoc-idle-delay 999)
  (eldoc-box-cleanup-interval 1))

(use-package eglot
  :defer t
  :custom
  (eglot-extend-to-xref t)
  (read-process-output-max (* 1024 1024))
  :hook ((elixir-ts-mode . eglot-ensure)
	 (elixir-ts-mode-hook . eglot-ensure)
	 (eglot--managed-mode . manually-activate-flymake))
  :config
  (add-to-list 'exec-path "~/git/lexical-lsp/lexical/bin/")
  (add-to-list 'eglot-stay-out-of 'flymake))

(defun manually-activate-flymake ()
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
  (flymake-mode 1))

(use-package eglot-booster
  :after eglot
  :straight (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
  :config (eglot-booster-mode))
