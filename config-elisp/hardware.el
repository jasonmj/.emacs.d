(exwm-input-set-key (kbd "<S-XF86MonBrightnessDown>") (lambda () (interactive) (call-process-shell-command "xbacklight -set 5")))
(exwm-input-set-key (kbd "<S-XF86MonBrightnessUp>") (lambda () (interactive) (call-process-shell-command "xbacklight -set 100")))
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") (lambda () (interactive) (call-process-shell-command "xbacklight -dec 5")))
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") (lambda () (interactive) (call-process-shell-command "xbacklight -inc 5")))

(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") (lambda()(interactive)(call-process-shell-command "pamixer -d 2")))
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") (lambda()(interactive)(call-process-shell-command "pamixer -i 2")))
(exwm-input-set-key (kbd "<XF86AudioMute>") (lambda()(interactive)(call-process-shell-command "pamixer -t")))
