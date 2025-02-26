(defun setup-frame-appearance (frame) (interactive)
	 (set-face-attribute 'default nil :font "Iosevka 13")
	 (use-package fira-code-mode
	   :ensure t
	   :config (fira-code-mode-set-font)
	   :hook prog-mode))
(add-hook 'after-make-frame-functions 'setup-frame-appearance)

(use-package circadian
  :ensure t
  :custom
  (circadian-themes '(("7:00"  . modus-operandi-tinted)
			("17:30" . modus-vivendi-tinted)))
  :hook (after-init . circadian-setup))

(use-package emacs
  :init (setq-default cursor-type 'hbar))

(use-package fancy-battery
  :ensure t
  :hook (after-init . fancy-battery-mode)
  :config (setq fancy-battery-show-percentage t))

(use-package fira-code-mode
  :if (display-graphic-p)
  :ensure t
  :config (fira-code-mode-set-font)
  :hook prog-mode)

(set-face-attribute 'default nil :font "Iosevka 15")

(setq-default truncate-lines -1)

(use-package modern-fringes
  :ensure t
  :hook (after-init . modern-fringes-mode)
  :config (set-fringe-mode '(7 . 0)))

(global-prettify-symbols-mode t)

(setq display-time-default-load-average nil
      display-time-format "%I:%M%p")
(display-time-mode t)
