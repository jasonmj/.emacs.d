(use-package nix-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)))
