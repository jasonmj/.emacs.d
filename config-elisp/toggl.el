(use-package request :ensure t)

(defun set-toggl-secrets ()
  (setq toggl--headers '(("Authorization" . (auth-source-pick-first-password :host "www.toggl.com" :user "jasonmj"))))
  (setq toggl-auth-token (auth-source-pick-first-password :host "api.toggl.com" :user "jasonmj")))

(setq toggl--headers nil)
(add-hook 'emacs-startup-hook 'set-toggl-secrets)

(defun toggl-stop-timer()
  (interactive)
  (request
    "https://www.toggl.com/api/v8/time_entries/current"
    :parser 'json-read
    :headers 'toggl--headers
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (let ((tid (cdr (pop (cdr (pop data))))))
	 (setq toggl-last-timer tid)
	 (toggl--stop-timer tid))))
    :error
    (cl-function
     (lambda (&rest args &key error-thrown &allow-other-keys)
       (message "Got error: %S" error-thrown)))))

(defun toggl--stop-timer(timer-id)
  (request
    (concat "https://www.toggl.com/api/v8/time_entries/" (number-to-string timer-id) "/stop")
    :parser 'json-read
    :headers 'toggl-headers
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (print (pop data))))
    :error
    (cl-function
     (lambda (&rest args &key error-thrown &allow-other-keys)
       (message "Got error: %S" error-thrown)))))

(defun toggl-restart-timer()
  (interactive)
  (message (concat "Restarting timer " (number-to-string toggl-last-timer)))
  (request
    (concat "https://www.toggl.com/api/v8/time_entries/start")
    :type "POST"
    :data '(("tid" . (number-to-string toggl-last-timer)))
    :parser 'json-read
    :headers 'toggl-headers
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (print (pop data))))
    :error
    (cl-function
     (lambda (&rest args &key error-thrown &allow-other-keys)
       (message "Got error: %S" error-thrown)
       (print error-thrown)))))

(use-package org-toggl
  :straight (:type git :host github :repo "jasonmj/org-toggl")
  :ensure t
  :demand t
  :after org
  :bind (("C-c C-x C-o" . toggl-stop-time-entry)
	 :map org-mode-map
	      ("C-x t s" . org-toggl-set-project)
	      ("C-c C-x TAB" . org-toggl-clock-in))
  :custom
  (toggl-auth-token "")
  (org-toggl-inherit-toggl-properties t)
  :hook ((after-init . toggl-get-projects)
	 (after-init . toggl-timer-watch)
	 (after-init . org-toggl-integration-mode)))
