(use-package polymode
  :ensure t
  :config
  (define-hostmode poly-markdown-hostmode :mode 'markdown-mode)
  (define-innermode poly-markdown-shell-innermode
    :mode 'sh-mode
    :head-matcher "```shell"
    :tail-matcher "```"
    :head-mode 'host
    :tail-mode 'host)
  (define-innermode poly-markdown-elixir-innermode
    :mode 'elixir-ts-mode
    :head-matcher "```elixir"
    :tail-matcher "```"
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-markdown-mode
    :hostmode 'poly-markdown-hostmode
    :innermodes '(poly-markdown-shell-innermode poly-markdown-elixir-innermode))
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
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . poly-elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . poly-markdown-mode)))
