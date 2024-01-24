(defun copy-test-line ()
  (interactive)
  (let* ((full-path (buffer-file-name))
	 (root (project-root (project-current)))
	 (path-from-home (replace-regexp-in-string (shell-command-to-string "echo $HOME") "~/" full-path nil t))
	 (path (replace-regexp-in-string root "" path-from-home nil t))
	 (line-number (number-to-string (line-number-at-pos))))
    (kill-new (concat "mix test.watch " path ":" line-number))))

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
  :hook ((elixir-ts-mode . (lambda ()
			     (unless (eq system-type 'darwin)
			       (setq indent-tabs-mode nil)
			       (indent-bars-mode t)
			       (setq indent-tabs-mode t))
			     (setq-local outline-regexp elixir-outline-regexp)
			     ))
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

(defun flymake-mix-test--output-filter (output)
  (let ((clean-output (s-trim (strip-ansi-chars output))))
    (if (string-match-p "^Running tests" clean-output) (flymake-mix-test--clear-diags))
    (flymake-mix-test--process-output clean-output))
  nil)

(defun flymake-mix-test--clear-diags ()
  (let ((diags flymake-mix-test-diags))
    (setq flymake-mix-test-diags '())
    (setq flymake-list-only-diagnostics '())
    (mapcar (lambda (diag)
	      (let* ((locus (flymake--diag-locus diag))
		     (locus-buffer (get-buffer (car (last (string-split locus "/"))))))
		(when locus-buffer
		  (with-current-buffer locus-buffer
		    (funcall flymake-mix-test--current-flymake-report-fn '() :region (cons (point-min) (point-max)))
		    (flymake--run-backend 'flymake-mix-test-backend)
		    (setq flymake-mix-test-diags '())
		    (setq flymake-list-only-diagnostics '())
		    (flymake-start)))))
	    diags)))

(defun flymake-mix-test--process-output (output)
  (if (string-match-p "[0-9]+\) test" output)
      (let* ((lines (split-string (escape-parens clean-output) "\n")))
	(mapcar (lambda (line)
		  (if (string-match-p "[:blank:][test/].+ test$" line)
		      (let* ((file (s-trim (car (split-string line ":"))))
			     (full-filepath (concat (project-root (project-current)) file))
			     (line-number (string-to-number (nth 1 (split-string line ":"))))
			     (details (string-join (cdr (cdr (mapcar 's-trim lines))) "\n")))
			(flymake-mix-test--push-diag full-filepath line-number details))))
		lines))))

(defun flymake-mix-test--push-diag (file line msg)
  (let* ((buffer (get-buffer (car (last (split-string file "/")))))
	 (reg (flymake-diag-region buffer line))
	 (beg (car reg))
	 (end (cdr reg)))
    (with-current-buffer buffer
      (push (flymake-make-diagnostic file beg end :error msg) flymake-mix-test-diags)
      (flymake-mix-test--report-to-flymake flymake-mix-test-diags))))

(defun flymake-mix-test--report-to-flymake (diags)
  (save-restriction
    (widen)
    (funcall flymake-mix-test--current-flymake-report-fn diags)))

(defun flymake-mix-test--setup ()
  (setq flymake-mix-test-diags '())
  (add-hook 'flymake-diagnostic-functions 'flymake-mix-test-backend nil t)
  (add-to-list 'comint-output-filter-functions 'flymake-mix-test--output-filter))

(defun flymake-mix-test-backend (report-fn &rest _more)
  (setq flymake-mix-test--current-flymake-report-fn report-fn)
  (flymake-mix-test--report-to-flymake flymake-mix-test-diags))

(defun escape-parens (str)
  (let* ((start (replace-regexp-in-string "[\(]" "" str nil t))
	 (finish (replace-regexp-in-string "[\)]" "" start nil t)))
    finish))

(add-hook 'elixir-ts-mode-hook 'flymake-mix-test--setup)
