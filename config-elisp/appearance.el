(use-package bar-cursor
  :ensure t
  :hook ((after-init . bar-cursor-mode)
	 (after-init . blink-cursor-mode)))

(use-package circadian
  :ensure t
  :custom
  (circadian-themes '(("7:00"  . modus-operandi)
		      ("17:30" . modus-vivendi)))
  :hook (after-init . circadian-setup))

(use-package fira-code-mode
  :ensure t
  :config (fira-code-mode-set-font)
  :hook prog-mode)

(set-face-attribute 'default nil :font "Iosevka 14")

(setq-default truncate-lines -1)

(use-package modern-fringes
  :ensure t
  :hook (after-init . modern-fringes-mode)
  :config (set-fringe-mode '(7 . 0)))

(global-prettify-symbols-mode t)

(setq display-time-default-load-average nil
      display-time-format "%I:%M%p")
(display-time-mode t)
