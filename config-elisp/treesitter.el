(use-package treesit
  :straight (:type built-in)
  :config
  ;; Replace relevant modes with the treesitter variant
  (dolist (mode
           '((bash-mode       . bash-ts-mode)
             (c-mode          . c-ts-mode)
             (elixir-mode     . elixir-ts-mode)))
    (add-to-list 'major-mode-remap-alist mode)))

;; (use-package combobulate
;;   :hook ((elixir-ts-mode . combobulate-mode))
;;   :load-path ("/home/jasonmj/git/mickeynp/combobulate"))
