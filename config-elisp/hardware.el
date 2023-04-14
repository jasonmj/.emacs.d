(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") (lambda () (interactive) (call-process-shell-command "pamixer -d 2")))
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") (lambda () (interactive) (call-process-shell-command "pamixer -i 2")))
(exwm-input-set-key (kbd "<XF86AudioMute>") (lambda () (interactive) (call-process-shell-command "pamixer -t")))

(use-package battery-notifier
  :ensure t
  :after (ednc)
  :config
  (defun my/battery-notifier-notification-function (message)
    (notifications-notify :title "Battery Notifier" :body message :app-name "Emacs" :actions '("default" "default") :urgency 'critical))
  :custom
  (battery-notifier-notification-function 'my/battery-notifier-notification-function)
  :hook ((after-init . battery-notifier-mode)
	 (battery-notifier-capacity-critical . (lambda () (call-process-shell-command "systemctl suspend")))))

(exwm-input-set-key (kbd "<S-XF86PowerOff>") (lambda () (interactive) (shell-command "slock")))

(exwm-input-set-key (kbd "<S-XF86MonBrightnessDown>") (lambda () (interactive) (call-process-shell-command "xbacklight -set 5")))
(exwm-input-set-key (kbd "<S-XF86MonBrightnessUp>") (lambda () (interactive) (call-process-shell-command "xbacklight -set 100")))
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") (lambda () (interactive) (call-process-shell-command "xbacklight -dec 5")))
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") (lambda () (interactive) (call-process-shell-command "xbacklight -inc 5")))

(defun suspend()
  (interactive)
  (shell-command "systemctl suspend"))
(defun suspend-lock()
  (interactive)
  (shell-command "systemctl suspend")
  (shell-command "slock"))
(exwm-input-set-key (kbd "C-x >") 'suspend-lock)
(exwm-input-set-key (kbd "C-x .") 'suspend)
