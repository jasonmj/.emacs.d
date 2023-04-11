(use-package prescient
  :ensure t
  :demand t
  :commands prescient-persist-mode
  :config
  (setq prescient-save-file (expand-file-name "cache/prescient-save.el" user-emacs-directory))
  (prescient-persist-mode))

(use-package vertico-prescient
  :ensure t
  :after prescient vertico
  :config
  (setq vertico-prescient-completion-styles '(orderless prescient partial-completion))
  (vertico-prescient-mode))

(use-package corfu-prescient
  :ensure t
  :after prescient corfu
  :config
  (corfu-prescient-mode))
