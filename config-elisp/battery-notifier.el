(use-package battery-notifier
  :ensure t
  :after (ednc)
  :config
  (defun my/battery-notifier-notification-function (message)
    (notifications-notify :title "Battery Notifier" :body message :app-name "Emacs" :app-icon nil
		      :actions '("default" "default") :urgency 'critical))
  :custom
  (battery-notifier-notification-function 'my/battery-notifier-notification-function)
  :hook ((after-init . battery-notifier-mode)
	 (battery-notifier-capacity-critical . (lambda () (call-process-shell-command "systemctl suspend")))))
