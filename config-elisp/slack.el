;; Skip Slack integration if GPG is not available in PATH
;; This prevents "no usable configuration" errors on macOS when GUI Emacs
;; can't find GPG in exec-path (even though it exists in ~/.nix-profile/bin)
(defvar slack-disabled (not (or (executable-find "gpg") (executable-find "gpg2")))
  "Set to t if GPG is not available, disabling Slack integration.")

(unless slack-disabled

(use-package request
  :ensure t)

(defvar slack-unread-count 0
  "Current Slack unread message count.")

(defvar slack-polling-timer nil
  "Timer for Slack polling.")

(defvar slack-app-token nil
  "Slack app token (xoxp-*). Set via auth-source (.authinfo.gpg). Cached after first load.")

(defun slack-load-token ()
  "Load Slack token from auth-source (idiomatic Emacs approach) and cache it.
 
Store your token securely in ~/.authinfo.gpg:
  machine slack.com login slack password xoxp-YOUR-TOKEN-HERE

The token is decrypted once on first use and cached for the session.

Wraps auth-source with error handling to prevent GPG failures during init."
  (or slack-app-token
      (setq slack-app-token
            (condition-case err
                (when-let* ((auth (auth-source-search :host "slack.com" :user "slack" :max 1))
                            (secret (plist-get (car auth) :secret)))
                  (if (functionp secret) (funcall secret) secret))
              (error
               (message "[SLACK] Warning: Could not load token from auth-source: %s" (error-message-string err))
               nil)))))

(defvar slack-unreads nil
  "List of unread conversations. Each element: (channel-id channel-name message-preview).")

(defun slack-fetch-unreads ()
  "Fetch unread DMs and recent activity."
  (let ((token (slack-load-token)))
    (if token
        ;; First check connection
        (request "https://slack.com/api/auth.test"
          :type "POST"
          :headers `(("Authorization" . ,(concat "Bearer " token)))
          :parser 'json-read
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (let ((ok (alist-get 'ok data))
                            (error (alist-get 'error data)))
                        (if ok
                            ;; Now fetch DMs and recent messages
                            (slack-fetch-dm-activity token)
                          (message "[SLACK] Connection failed: %s" error)))))
          :error (cl-function
                  (lambda (&key error-thrown &allow-other-keys)
                    (message "[SLACK] Request error: %s" error-thrown))))
      (message "Slack token not configured"))))

(defun slack-fetch-dm-activity (token)
  "Fetch recent DM messages to detect unread activity."
  (request "https://slack.com/api/users.conversations"
    :type "POST"
    :headers `(("Authorization" . ,(concat "Bearer " token)))
    :params '(("types" . "im,mpim")
              ("limit" . "50")
              ("exclude_archived" . "true"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((ok (alist-get 'ok data))
                      (channels (alist-get 'channels data)))
                  (if ok
                      (slack-process-dm-channels token channels)
                    (message "[SLACK] Failed to fetch DMs: %s" (alist-get 'error data))))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "[SLACK] DM fetch error: %s" error-thrown)))))

(defun slack-process-dm-channels (token channels)
  "Process DM channels to find recent messages."
  (setq slack-unreads nil)
  (setq slack-unread-count 0)
  
  ;; Check first few DMs for recent activity
  (dolist (channel (seq-take channels 10))
    (let ((channel-id (alist-get 'id channel))
          (channel-name (alist-get 'name channel)))
      (request (concat "https://slack.com/api/conversations.history?channel=" channel-id "&limit=1")
        :type "GET"
        :headers `(("Authorization" . ,(concat "Bearer " token)))
        :parser 'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (let ((messages (alist-get 'messages data)))
                      (when messages
                        (let ((last-msg (car messages)))
                          (push (list channel-id channel-name last-msg) slack-unreads)
                          (cl-incf slack-unread-count)))))))))
  
  (force-mode-line-update)
  (message "[SLACK] ✓ Found %d recent DMs" slack-unread-count))

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

;; Start polling on first-use (lazy initialization)
;; This defers GPG auth until Slack is actually needed, preventing init failures
(defvar slack-initialized nil
  "Track whether Slack polling has been initialized.")

(defun slack-ensure-polling ()
  "Ensure Slack polling is running. Safe to call multiple times."
  (unless slack-initialized
    (setq slack-initialized t)
    (slack-start-polling)))

;; Start on first user interaction, not at startup
(add-hook 'focus-in-hook #'slack-ensure-polling)

) ;; end of (unless slack-disabled ...)
