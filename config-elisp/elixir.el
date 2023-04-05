(use-package dap-mode
  :ensure t
  :config
  (require 'dap-elixir)
  (dap-ui-mode)
  (dap-mode))

(defvar elixir-outline-regexp
  (concat "^[[:space:]]*\\("
	  "@moduledoc\\|@behaviour\\|@callback\\|@type\\|@typedoc\\|@doc\\|@spec\\|@impl"
	  "\\|def\\(\\|p\\|callback\\|delegate\\|impl\\|overridable\\|exception\\|struct\\|guard\\|guardp\\|record\\|recordp\\|macro\\|macrop\\|macrocallback\\|protocol\\)"
	  "\\|describe\\|test\\|setup\\|let\\|it\\|context\\|before\\|schema"
	  "\\|use\\|alias\\|import\\|require"
	  "\\)\\([[:space:]]\\|(\\)"))
(defun my/elixir-format ()
  (interactive)
  (with-current-buffer (current-buffer)
    (if (f-exists? (concat (project-root (project-current)) "/shell.nix"))
	(call-process-shell-command (concat "cd " (project-root (project-current)) " && " "NIX_SKIP_SHELL_HOOK=true nix-shell --run \"mix format " (buffer-file-name) "\""))
      (elixir-format)))
  (revert-buffer t t))
(defun setup-elixir ()
  (add-hook 'after-save-hook 'my/elixir-format nil 'make-it-local)
  (setq-local outline-regexp elixir-outline-regexp))
(use-package elixir-mode :ensure t)
(use-package elixir-ts-mode
  :ensure t
  :hook (elixir-ts-mode . setup-elixir))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(elixir-ts-mode . ("/home/jasonmj/git/elixir-lsp/elixir-ls-1.14-25.1/language_server.sh")))
  (add-hook 'elixir-ts-mode-hook #'eglot-ensure))

(use-package polymode
  :ensure t
  :custom (polymode-mode-map nil)
  :config
  (define-hostmode poly-elixir-ts-hostmode :mode 'elixir-ts-mode)
  (define-innermode poly-elixir-ts-doc-innermode
    :mode 'markdown-mode
    :head-matcher "@\\(module\\)?doc *\"\"\""
    :tail-matcher "\"\"\""
    :head-mode 'host
    :tail-mode 'host)
  (define-innermode poly-elixir-ts-template-innermode
    :mode 'heex-ts-mode
    :head-matcher "~\\(L\\|E\\|\\H\\)\"\"\""
    :tail-matcher "\"\"\""
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-elixir-ts-mode
    :hostmode 'poly-elixir-ts-hostmode
    :innermodes '(poly-elixir-ts-doc-innermode poly-elixir-ts-template-innermode))
  (define-hostmode poly-heex-ts-hostmode :mode 'heex-ts-mode)
  (define-innermode poly-heex-ts-doc-innermode
    :mode 'markdown-mode
    :head-matcher "@\\(module\\)?doc *\"\"\""
    :tail-matcher "\"\"\""
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-heex-ts-mode
    :hostmode 'poly-heex-ts-hostmode
    :innermodes '(poly-heex-ts-doc-innermode poly-heex-ts-template-innermode))
  (add-to-list 'auto-mode-alist '("\\.leex\\'" . poly-heex-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.heex\\'" . poly-heex-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . poly-elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . poly-elixir-ts-mode)))
