(use-package treesit
  :straight (:type built-in)
  :config
  (dolist (mode
           '((bash-mode       . bash-ts-mode)
             (c-mode          . c-ts-mode)
             (elixir-mode     . elixir-ts-mode)))
    (add-to-list 'major-mode-remap-alist mode)))
