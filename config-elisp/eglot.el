(use-package eldoc-box
  :ensure t
  :hook (eglot-managed-mode-hook . eldoc-box-hover-mode)
  :config
  (custom-set-faces `(eldoc-box-body ((t (:background ,(face-attribute 'default :background) :foreground ,(face-attribute 'default :foreground))))))
  (custom-set-faces `(eldoc-box-border ((t (:background ,(face-attribute 'default :foreground))))))
  (advice-add 'eglot--mode-line-format :override (lambda () ""))
  :custom
  (eglot-menu-string "")
  (eldoc-box-offset '(16 16 40))
  (eldoc-box-max-pixel-height 450)
  (eldoc-documentation-strategy 'eldoc-documentation-enthusiast)
  (eldoc-idle-delay 1))

(use-package eglot
  :hook ((elixir-ts-mode . eglot-ensure)
	 (c-mode . eglot-ensure)
	 (eglot--managed-mode . manually-activate-flymake)
	 (python-mode . eglot-ensure))
  :custom
  (eglot-extend-to-xref t)
  (read-process-output-max (* 1024 1024))
  :config
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(elixir-ts-mode . ("~/git/elixir-ls/language_server.sh"
							  "~/git/elixir-tools/credo-language-server/bin/credo-language-server")))
  (push :documentHighlightProvider eglot-ignored-server-capabilities))

(defun manually-activate-flymake ()
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
  (flymake-mode 1))
