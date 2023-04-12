(use-package ednc
  :ensure t
  :init (ednc-mode)
  :config
  (require 'notifications)
  (defun posframe-refposhandler-exwm (&optional frame)
    "EXWM posframe refposhandler."
    (cond
     (exwm--connection
      (or (ignore-errors
	    (posframe-refposhandler-xwininfo frame))
	  (cons 0 0)))
     (t nil)))
  (defun show-posframe-in-buffer (old new)
    ;; Creates a small floating frame with the EDNC notification.
    ;; Test with notification function below:
    ;;
    ;; (notifications-notify :title "EDNC" :body "Testing the notification EDNC notification system.")

    (let ((name (format " *ednc-notification-%d*" (ednc-notification-id (or old new)))))
      (with-current-buffer (get-buffer-create name)
	(if new (let ((inhibit-read-only t))
		  (if old (erase-buffer) (ednc-view-mode))
		  (insert (ednc-format-notification new t))
		  (when (posframe-workable-p)
		    (posframe-show name
				   :accept-focus nil
				   :poshandler '(lambda (_info) '(-6 . 38))
				   :max-width 45
				   :border-width 1
				   :left-fringe 5
				   :right-fringe 5
				   :border-color (face-attribute 'default :foreground)
				   :timeout 5
				   :refposhandler 'posframe-refposhandler-exwm
				   :override-parameters '((alpha-background . 85)))))))))
  (add-hook 'ednc-notification-presentation-functions #'show-posframe-in-buffer))
