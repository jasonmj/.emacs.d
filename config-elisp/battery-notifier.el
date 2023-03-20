(use-package battery-notifier
  :ensure t
  :hook ((after-init . battery-notifier-mode)
	 (battery-notifier-capacity-critical . (lambda () (call-process-shell-command "systemctl suspend")))))
