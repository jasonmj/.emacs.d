(defun setup-input-devices()
	(interactive)
	(call-process-shell-command "xmodmap ~/.Xmodmap" nil 0)
	(call-process-shell-command "xinput disable \"SynPS/2 Synaptics TouchPad\"")
	(call-process-shell-command "xset s 00")
	(call-process-shell-command "xset -dpms")
	(call-process-shell-command "xinput --set-button-map \"TPPS/2 Elan TrackPoint\" 3 2 1")
  (call-process-shell-command "nvidia-settings --assign CurrentMetaMode=\"nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }\""))

(unless (eq system-type 'darwin)
  (setup-input-devices)
  (call-process-shell-command "xsetroot -cursor_name left_ptr"))

;; Garbage Collection Magic Hack
(use-package gcmh
  :ensure t
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
	gcmh-auto-idle-delay-factor 10
	gcmh-high-cons-threshold #x1000000)) ; 16MB

(use-package loopy :ensure t :config (require 'loopy-iter))
(use-package key-chord :ensure t)
(use-package use-package-chords
  :ensure t
  :demand t
  :config (key-chord-mode t))
(use-package key-seq :ensure t)

;; Prevent initial flash of display on startup
(when (display-graphic-p)
    (setq-default inhibit-redisplay t
		  inhibit-message t)
    (defun reset-inhibit-vars ()
      (setq-default inhibit-redisplay nil
		    inhibit-message nil)
      (redraw-frame))
    (add-hook 'window-setup-hook #'reset-inhibit-vars)
    (define-advice startup--load-user-init-file (:after (&rest _) reset-inhibit-vars)
      (and init-file-had-error (reset-inhibit-vars))))
