(use-package doom-modeline
  :ensure t
  :custom (doom-modeline-icon nil)
  :config
  (doom-modeline-def-modeline 'my-simple-line
    '(bar matches buffer-info remote-host)
    '(misc-info major-mode process vcs))

  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'my-simple-line 'default))
  :hook ((after-init . setup-custom-doom-modeline)))
