(defun setup-input-devices()
	(interactive)
	(call-process-shell-command "xmodmap ~/.Xmodmap" nil 0)
	(call-process-shell-command "xinput disable \"SynPS/2 Synaptics TouchPad\"")
	(call-process-shell-command "xset s 00")
	(call-process-shell-command "xset -dpms")
	(call-process-shell-command "xinput --set-button-map \"TPPS/2 Elan TrackPoint\" 3 2 1")
  (call-process-shell-command "nvidia-settings --assign CurrentMetaMode=\"nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }\""))

(setup-input-devices)
;;(call-process-shell-command "dunst" nil 0)
(call-process-shell-command "deadd-notification-center" nil 0)
(call-process-shell-command "xsetroot -cursor_name left_ptr")

(use-package loopy :ensure t :config (require 'loopy-iter))
(use-package key-chord :ensure t)
(use-package key-seq :ensure t)
