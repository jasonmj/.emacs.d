(use-package elixir-mode :ensure t)
(use-package elixir-ts-mode
  :ensure t
  :bind (:map elixir-ts-mode-map
	      ("C-S-s" . (lambda () (interactive) (save-buffer) (my/elixir-format)))
	      ("M-RET" . (lambda () (interactive) (newline) (insert "|> ") (indent-for-tab-command))))
  :config
  (defvar elixir-outline-regexp
    (concat "^[[:space:]]*\\("
	    "@moduledoc\\|@behaviour\\|@callback\\|@type\\|@typedoc\\|@doc\\|@spec\\|@impl"
	    "\\|def\\(\\|p\\|callback\\|delegate\\|impl\\|overridable\\|exception\\|struct\\|guard\\|guardp\\|record\\|recordp\\|macro\\|macrop\\|macrocallback\\|protocol\\)"
	    "\\|describe\\|test\\|setup\\|let\\|it\\|context\\|before\\|schema"
	    "\\|use\\|alias\\|import\\|require"
	    "\\)\\([[:space:]]\\|(\\)"))
  (defvar html-outline-regexp "^[[:space:]]*<[^/>]+?\\(>\\|\n\\)")
  (defun my/elixir-format ()
    (interactive)
    (with-current-buffer (current-buffer)
      (if (f-exists? (concat (project-root (project-current)) "/shell.nix"))
	  (call-process-shell-command (concat "cd " (project-root (project-current)) " && " "NIX_SKIP_SHELL_HOOK=true nix-shell --run \"mix format " (buffer-file-name) "\""))
	(elixir-format)))
    (revert-buffer t t))
  :hook ((elixir-ts-mode . (lambda () (setq-local outline-regexp elixir-outline-regexp)))
	 (heex-ts-mode . (lambda () (setq-local outline-regexp html-outline-regexp)))
	 (heex-ts-mode . hungry-delete-mode)
	 (heex-ts-mode . display-line-numbers-mode))
  :mode (("\\.ex\\'" . elixir-ts-mode)
	 ("\\.exs\\'" . elixir-ts-mode)
	 ("\\.heex\\'" . heex-ts-mode)
	 ("\\.leex\\'" . heex-ts-mode)))

(defun format-elixir-region (beg end)
  "Formats the selected region (if any) with Elixir's `Code.format_string!/1`"
  (interactive "r")
  (let* ((column-indent (- beg (point-at-bol)))
	 (region-contents (buffer-substring beg end))
	 (snippet_1 (replace-regexp-in-string "\"" "\\\"" region-contents nil t))
	 (snippet_2 (replace-regexp-in-string "#Ecto.Association.NotLoaded" "" snippet_1 nil t))
	 (snippet_3 (replace-regexp-in-string "#Ecto.Schema.Metadata<:loaded, " "" snippet_2 nil t))
	 (snippet_4 (replace-regexp-in-string "<(.*?)>" "association not loaded" snippet_3 nil t))
	 (snippet (replace-regexp-in-string ">," "," snippet_4 nil t))
	 (command (concat
		   "cd ~/ && mix run --no-mix-exs -e 'Code.format_string!(\""
		   snippet
		   "\") |> IO.iodata_to_binary() |> IO.puts()'"))
	 (result (string-trim (shell-command-to-string command))))
    (kill-region beg end)
    (insert result)
    (set-mark (point))
    (goto-char beg)
    (syntax-overlay-region)
    (indent-rigidly (region-beginning) (region-end) column-indent)))

(global-set-key (kbd "C-x F r") 'format-elixir-region)

(defun copy-test-line ()
  (interactive)
  (let* ((full-path (buffer-file-name))
	 (root (project-root (project-current)))
	 (path-from-home (replace-regexp-in-string "/Users/jjohnson/" "~/" full-path nil t))
	 (path (replace-regexp-in-string root "" path-from-home nil t))
	 (line-number (number-to-string (line-number-at-pos))))
    (kill-new (concat "mix test.watch " path ":" line-number))))
