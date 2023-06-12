(use-package doom-modeline
  :ensure t
  :custom (doom-modeline-icon nil)
  :config
  (doom-modeline-def-segment exwm-workspace
    (propertize (concat " [" (number-to-string exwm-workspace-current-index) "]") 'face '(:weight bold)))

  (doom-modeline-def-segment toggl-timer
    (let* ((description (if (fboundp 'toggl-get-timer) (toggl-get-timer) "No timer")))
      (when description (propertize (concat description " | ") 'face '(:weight normal)))))

  (doom-modeline-def-modeline 'my-simple-line
    '(bar matches buffer-info remote-host)
    '(toggl-timer misc-info exwm-workspace major-mode process vcs checker))

  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'my-simple-line 'default))
  :hook ((after-init . setup-custom-doom-modeline)))
