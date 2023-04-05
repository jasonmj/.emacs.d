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
