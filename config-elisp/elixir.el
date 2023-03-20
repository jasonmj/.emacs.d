(use-package dap-mode
  :ensure t
  :config
  (require 'dap-elixir)
  (dap-ui-mode)
  (dap-mode))

(defvar elixir-outline-regexp
  (concat "^[[:space:]]*\\("
	  "@moduledoc\\|@behaviour\\|@callback\\|@type\\|@typedoc\\|@doc\\|@spec"
	  "\\|def\\(\\|p\\|callback\\|delegate\\|impl\\|overridable\\|exception\\|struct\\|guard\\|guardp\\|record\\|recordp\\|macro\\|macrop\\|macrocallback\\|protocol\\)"
	  "\\|describe\\|test\\|setup\\|let\\|it\\|context\\|before"
	  "\\|use\\|alias\\|import\\|require"
	  "\\)\\([[:space:]]\\|(\\)"))
(defun setup-elixir ()
  (add-hook 'before-save-hook 'elixir-format nil 'make-it-local)
  (setq-local outline-regexp elixir-outline-regexp))
(use-package elixir-mode :ensure t)
(use-package elixir-ts-mode
  :ensure t
  :config
  (add-hook 'elixir-ts-mode-hook 'setup-elixir)
  (add-to-list 'auto-mode-alist '("\\.leex\\'" . elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.heex\\'" . elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode)))

(require 'eglot)
(setq eglot-extend-to-xref t)
(dolist (mode '(elixir-ts-mode))
  (add-to-list 'eglot-server-programs `(,mode . ("/home/jasonmj/git/elixir-lsp/elixir-ls-1.14-25.1/language_server.sh"))))
(add-hook 'elixir-ts-mode-hook 'eglot-ensure)

(use-package polymode
  :ensure t
  :config
  (define-hostmode poly-elixir-hostmode :mode 'elixir-ts-mode)
  (define-innermode poly-elixir-doc-innermode
    :mode 'markdown-mode
    :head-matcher "@\\(module\\)?doc *\"\"\""
    :tail-matcher "\"\"\""
    :head-mode 'host
    :tail-mode 'host)
  (define-innermode poly-elixir-template-innermode
    :mode 'web-mode
    :head-matcher "~\\(L\\|E\\)\"\"\""
    :tail-matcher "\"\"\""
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-elixir-mode
    :hostmode 'poly-elixir-hostmode
    :innermodes '(poly-elixir-doc-innermode poly-elixir-template-innermode)))
