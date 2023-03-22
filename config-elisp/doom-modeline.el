(require 'parse-time)
(defun my-doom-modeline-init()
  (interactive)
  (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-height 32)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-bar-width 6)
  (setq-default mode-line '((t (:background "#3c3836" :foreground "#d5c4a1" :box nil :height 135))))
  (setq-default mode-line-inactive '((t (:background "#3c3836" :foreground "#a89984" :box nil :height 135))))
  (set-face-attribute 'mode-line nil :family "Iosevka")
  (set-face-attribute 'mode-line-inactive nil :family "Iosevka")
  (setq-local mode-line-format '("%e"
                           (:eval
                            (doom-modeline-format--my-simple-line))))
  (setq-default mode-line-format '("%e"
                           (:eval
                            (doom-modeline-format--my-simple-line))))
  (view-echo-area-messages)
  (delete-other-windows))
(defun toggle-doom-modeline()
  (interactive)
  (if (eq mode-line-format nil)
      (my-doom-modeline-init)
    (progn
      (exwm-workspace-detach-minibuffer)
      (setq-local mode-line-format nil)
      (view-echo-area-messages)
      (delete-other-windows))))
(use-package doom-modeline
  :ensure t
  :config
  (exwm-input-set-key (kbd "<home>") 'toggle-doom-modeline)
  (exwm-input-set-key (kbd "<XF86AudioPlay>") 'toggle-doom-modeline)
  (exwm-input-set-key (kbd "<XF86AudioPause>") 'toggle-doom-modeline)
  :init
  (my-doom-modeline-init))

(doom-modeline-def-segment exwm-workspace
  (propertize (concat " [" (number-to-string exwm-workspace-current-index) "]") 'face '(:weight bold)))

(doom-modeline-def-segment spacer
  (propertize " " 'face '(:weight bold)))

(defun time-number-to-string(number)
  (if (> number 9)
      (number-to-string number)
    (concat "0" (number-to-string number))))

(doom-modeline-def-segment toggl-timer
  (let* ((timer (alist-get 'data toggl-current-time-entry))
         (description (alist-get 'description timer))
         (timer-start-string (alist-get 'start timer)))
         (if timer
             (progn
               (let* ((timer-start (parse-iso8601-time-string timer-start-string))
                      (duration (- (float-time) (float-time timer-start)))
                      (minutes (/ duration 60))
                      (hours (/ minutes 60))
                      (duration-string (concat
                                        "0" (number-to-string (floor hours)) ":"
                                        (time-number-to-string (floor (mod minutes 60))) ":"
                                        (time-number-to-string (floor (mod duration 60))))))
                 (when description (propertize (concat description
                                                       " - "
                                                       (truncate-string-to-width duration-string 35)
                                                       " | ") 'face '(:weight normal)))))
           (propertize "" 'face '(:weight normal)))))

;; Define your custom doom-modeline
(doom-modeline-def-modeline 'my-simple-line
  '(bar matches buffer-info remote-host buffer-position selection-info)
  '(toggl-timer misc-info exwm-workspace major-mode process vcs checker))

;; Add to `doom-modeline-mode-hook` or other hooks
(defun setup-custom-doom-modeline ()
   (doom-modeline-set-modeline 'my-simple-line 'default))
(add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)

(use-package fancy-battery
  :ensure t
  :hook (after-init . fancy-battery-mode)
  :config (setq fancy-battery-show-percentage t))
