(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '(("7:00" . modus-operandi)
                           ("17:30" . modus-vivendi)))
  (circadian-setup))

(set-face-attribute 'default nil :font "Iosevka 14")

(blink-cursor-mode 1)
(use-package bar-cursor :ensure t :hook ((after-init . bar-cursor-mode)))

(setq display-time-default-load-average nil)
(setq display-time-format "%I:%M%p")
(display-time-mode t)

(use-package modern-fringes
  :ensure t
  :hook (after-init . modern-fringes-mode)
  :config
  (set-fringe-mode '(7 . 0)))

(global-prettify-symbols-mode 1)
(load-file "~/.emacs.d/elisp/pretty-fonts.el")
(pretty-fonts-set-kwds
  '((pretty-fonts-fira-font prog-mode-hook org-mode-hook)))

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(setq-default truncate-lines -1)
