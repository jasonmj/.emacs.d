(use-package ednc
  :ensure t
  :init
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
                                   :font-height 1.0
                                   :poshandler 'posframe-poshandler-frame-bottom-center
                                   :position 'posframe--last-posframe-pixel-position
                                   :parent-frame-poshandler 'ivy-posframe-parent-frame-poshandler
                                   :parent-frame nil
                                   :internal-border-width 15
                                   :refposhandler 'ivy-posframe-refposhandler-default)
                    (run-with-idle-timer 3 nil 'posframe-hide-all)))
          (kill-buffer)))))
  (add-hook 'ednc-notification-presentation-functions #'show-posframe-in-buffer))
