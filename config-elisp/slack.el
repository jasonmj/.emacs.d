(use-package request
  :ensure t)

(defvar slack-unread-count 0
  "Current Slack unread message count.")

(defvar slack-polling-timer nil
  "Timer for Slack polling.")

(defvar slack-app-token nil
  "Slack app token (xoxb-*). Set via environment or customize.")

(defun slack-load-token ()
  "Load Slack token from environment variable or auth-source."
  (or slack-app-token
      (getenv "SLACK_BOT_TOKEN")
      (when-let* ((auth (auth-source-search :host "slack.com" :max 1))
                  (token (plist-get (car auth) :secret)))
        (if (functionp token) (funcall token) token))))

(defun slack-fetch-unreads ()
  "Fetch unread message count from Slack."
  (let ((token (slack-load-token)))
    (if token
        (request "https://slack.com/api/conversations.unreads"
          :type "GET"
          :headers `(("Authorization" . ,(concat "Bearer " token)))
          :params '(("limit" . "100"))
          :parser 'json-read
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (if (plist-get data :ok)
                          (let ((total 0))
                            (dolist (channel (plist-get data :unreads))
                              (cl-incf total (plist-get channel :unreads)))
                            (setq slack-unread-count total)
                            (force-mode-line-update))
                        (message "Slack API error: %s" (plist-get data :error)))))
          :error (cl-function
                  (lambda (&key error-thrown &allow-other-keys)
                    (message "Slack fetch failed: %s" error-thrown))))
      (message "Slack token not configured"))))

(defun slack-start-polling (&optional interval)
  "Start polling Slack for unreads. INTERVAL in seconds (default 30)."
  (interactive)
  (let ((interval (or interval 30)))
    (when slack-polling-timer
      (cancel-timer slack-polling-timer))
    (slack-fetch-unreads)  ; Fetch immediately
    (setq slack-polling-timer
          (run-at-time nil interval 'slack-fetch-unreads))
    (message "Slack polling started (interval: %ds)" interval)))

(defun slack-stop-polling ()
  "Stop polling Slack."
  (interactive)
  (when slack-polling-timer
    (cancel-timer slack-polling-timer)
    (setq slack-polling-timer nil)
    (setq slack-unread-count 0)
    (force-mode-line-update)
    (message "Slack polling stopped")))

;; Start polling at Emacs startup
(add-hook 'after-init-hook (lambda () (slack-start-polling)))
