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
				   :alpha 92
				   :poshandler '(lambda (_info) '(-6 . 38))
				   :refposhandler 'posframe-refposhandler-exwm
				   :parent-frame nil
				   :override-parameters '((internal-border-width . 15)
							  (internal-border-color . (face-attribute 'default :background))))
		    (run-with-idle-timer 3 nil 'posframe-delete-all)))
	  (kill-buffer)))))
  (add-hook 'ednc-notification-presentation-functions #'show-posframe-in-buffer))
