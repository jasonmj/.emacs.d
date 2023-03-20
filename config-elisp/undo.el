(use-package undo-fu :ensure t)
(use-package undo-fu-session
  :ensure t
  :bind (("C-z" . undo-fu-only-undo)
	 ("C-S-z" . undo-fu-only-redo))
  :hook (after-init . global-undo-fu-session-mode)
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))
