(use-package polymode
  :ensure t
  :custom (poly-mode-map nil)
  :config
  ;; Markdown
  (define-hostmode poly-markdown-hostmode :mode 'markdown-mode)
  (define-innermode poly-markdown-json-innermode
      :mode 'js-json-mode
      :head-matcher "```json"
      :tail-matcher "```"
      :head-mode 'host
      :tail-mode 'host)
  (define-innermode poly-markdown-shell-innermode
    :mode 'sh-mode
    :head-matcher "```shell"
    :tail-matcher "```"
    :head-mode 'host
    :tail-mode 'host)
  (define-innermode poly-markdown-elixir-innermode
    :mode 'elixir-mode
    :head-matcher "```elixir"
    :tail-matcher "```"
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-markdown-mode
    :hostmode 'poly-markdown-hostmode
    :innermodes '(poly-markdown-json-innermode poly-markdown-shell-innermode poly-markdown-elixir-innermode))
  ;; Elixir
  (define-hostmode poly-elixir-ts-hostmode :mode 'elixir-ts-mode)
  (define-innermode poly-elixir-ts-doc-innermode
    :mode 'markdown-mode
    :head-matcher "@\\(module\\)?doc *\"\"\""
    :tail-matcher "\"\"\""
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-elixir-ts-mode
    :hostmode 'poly-elixir-ts-hostmode
    :innermodes '(poly-elixir-ts-doc-innermode))
  ;; Shell
  (define-hostmode poly-shell-hostmode :mode 'shell-mode)
  (define-innermode poly-shell-elixir-innermode
    :mode 'elixir-ts-mode
    :head-matcher "%[[:ascii:]]+{"
    :tail-matcher "}"
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-shell-mode
    :hostmode 'poly-shell-hostmode
    :innermodes '(poly-shell-elixir-innermode))
  :mode ("\\.md\\'" . poly-markdown-mode))
