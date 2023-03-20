;;; org-toggl.el --- A simple Org-mode interface to Toggl  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Marcin Borkowski

;; Author: Marcin Borkowski <mbork@mbork.pl>
;; Keywords: calendar
;; Package-Requires: ((request "0.2.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple Org-mode interface to Toggl, a time-tracking service.
;; Hooks into the Org-mode's clocking mechanism.

;;; Code:

(require 'json)
(require 'request)

(defcustom toggl-auth-token ""
  "Authentication token for Toggl."
  :type 'string
  :group 'toggl)

(defcustom toggl-default-timeout 20
  "Default timeout for HTTP requests."
  :type 'integer
  :group 'toggl)

(defvar toggl-api-url "https://api.track.toggl.com/api/v8/"
  "The URL for making API calls.")

(defun toggl-create-api-url (string)
  "Prepend Toogl API URL to STRING."
  (concat toggl-api-url string))

(defun toggl-prepare-auth-header ()
  "Return a cons to be put into headers for authentication."
  (cons "Authorization"
	      (format "Basic %s" (base64-encode-string (concat toggl-auth-token ":api_token")))))

(defun toggl-request-get (request &optional sync success-fun error-fun timeout)
  "Send a GET REQUEST to api.track.toggl.com, with TIMEOUT.
Add the auth token)."
  (request (toggl-create-api-url request)
	  :parser #'json-read
	  :headers (list (toggl-prepare-auth-header))
	  :success success-fun
	  :error error-fun
	  :sync sync
	  :timeout (or timeout toggl-default-timeout)))

(defun toggl-request-post (request data &optional sync success-fun error-fun timeout)
  "Send a GET REQUEST to api.track.toggl.com, with TIMEOUT.
Add the auth token)."
  (request (toggl-create-api-url request)
	  :type "POST"
	  :data data
	  :parser #'json-read
	  :headers (list (toggl-prepare-auth-header)
			             '("Content-Type" . "application/json"))
	  :success success-fun
	  :error error-fun
	  :sync sync
	  :timeout (or timeout toggl-default-timeout)))

(defun toggl-request-put (request data &optional sync success-fun error-fun timeout)
  "Send a GET REQUEST to api.track.toggl.com, with TIMEOUT.
Add the auth token)."
  (request (toggl-create-api-url request)
	  :type "PUT"
	  :data data
	  :parser #'json-read
	  :headers (list (toggl-prepare-auth-header)
			             '("Content-Type" . "application/json"))
	  :success success-fun
	  :error error-fun
	  :sync sync
	  :timeout (or timeout toggl-default-timeout)))

(defun toggl-request-delete (request &optional sync success-fun error-fun timeout)
  "Send a DELETE REQUEST to api.track.toggl.com, with TIMEOUT.
Add the auth token)."
  (request (toggl-create-api-url request)
	  :type "DELETE"
	  :parser #'json-read
	  :headers (list (toggl-prepare-auth-header))
	  :success success-fun
	  :error error-fun
	  :sync sync
	  :timeout (or timeout toggl-default-timeout)))

(defvar toggl-projects nil
  "A list of available projects.
Each project is a cons cell with car equal to its name and cdr to
its id.")

(defvar toggl-tasks nil
  "A list of available project tasks.
Each task is a cons cell with car equal to its name and cdr to
its id.")

(defvar toggl-current-time-entry nil
  "Data of the current Toggl time entry.")

(defvar toggl-last-time-entry nil
  "Data of the last Toggl time entry.")

(defun toggl-get-projects ()
  "Fill in `toggl-projects' (asynchronously)."
  (interactive)
  (toggl-request-get
   "me?with_related_data=true"
   nil
   (cl-function
    (lambda (&key data &allow-other-keys)
      (setq toggl-projects
	          (mapcar (lambda (project)
		                  (cons (substring-no-properties (alist-get 'name project))
			                      (alist-get 'id project)))
		                (alist-get 'projects (alist-get 'data data))))
      (message "Toggl projects successfully downloaded.")))
   (cl-function
    (lambda (&key error-thrown &allow-other-keys)
      (message "Fetching projects failed because %s" error-thrown)))))

(defun toggl-get-project-tasks (pid)
  "Fill in `toggl-tasks' (asynchronously)."
  (interactive)
  (toggl-request-get
   (concat "projects/" (number-to-string pid) "/tasks")
   nil
   (cl-function
    (lambda (&key data &allow-other-keys)
      ;; (message "Got data: %S" data)
      (setq toggl-tasks
	          (mapcar (lambda (task)
		                  (cons (substring-no-properties (alist-get 'name task))
			                      (alist-get 'id task)))
		                data))
      (when (> (length toggl-tasks) 0) (call-interactively 'org-toggl-set-task))
      (message "Toggl tasks successfully downloaded.")))
   (cl-function
    (lambda (&key error-thrown &allow-other-keys)
      (message "Fetching tasks failed because %s" error-thrown)))))

(defvar toggl-default-project nil
  "Id of the default Toggl project.")

(defun toggl-select-default-project (project)
  "Make PROJECT the default.
It is assumed that no two projects have the same name."
  (interactive (list (completing-read "Default project: " toggl-projects nil t)))
  (setq toggl-default-project (toggl-get-pid project)))

(defvar toggl-timer-watch-timer nil
  "Timer for `toggl-timer-watch' to reschedule itself, or nil.")

(defun toggl-timer-watch ()
  "Start the Toggl timer watch"
  (interactive)
  (setq toggl-timer-watch-timer (run-with-idle-timer 5 t 'toggl-get-running-timer))
  (message "Started watching toggl timer"))

(defun toggl-timer-watch-stop ()
  "Stop the Toggl timer watch"
  (interactive)
  (cancel-timer toggl-timer-watch-timer)
  (setq toggl-timer-watch-timer nil)
  (message "Stopped watching toggl timer"))

(defun toggl-get-running-timer ()
  "Get currently running Toggl time entry."
  (interactive)
  (toggl-request-get
   "time_entries/current"
   nil
   (cl-function
    (lambda (&key data &allow-other-keys)
      (when (alist-get 'data data) (setq toggl-last-time-entry (alist-get 'data data)))
      (setq toggl-current-time-entry data)))))

(defun toggl-start-time-entry (description &optional pid tid show-message)
  "Start Toggl time entry."
  (interactive "MDescription: \ni\np")
  (setq pid (or pid toggl-default-project))
  (toggl-request-post
   "time_entries/start"
   (json-encode `(("time_entry" .
		               (("description" . ,description)
		                ("pid" . ,pid)
		                ("tid" . ,tid)
		                ("created_with" . "Emacs")))))
   nil
   (cl-function
    (lambda (&key data &allow-other-keys)
      (setq toggl-current-time-entry data)
      (when show-message (message "Toggl time entry started."))))
   (cl-function
    (lambda (&key error-thrown &allow-other-keys)
      (when show-message (message "Starting time entry failed because %s" error-thrown))))))

(defun toggl-stop-time-entry (&optional show-message)
  "Stop running Toggl time entry."
  (interactive "p")
  (when toggl-current-time-entry
    (toggl-request-put
     (format "time_entries/%s/stop"
	           (alist-get 'id (alist-get 'data toggl-current-time-entry)))
     nil
     nil
     (cl-function
      (lambda (&key data &allow-other-keys)
	      (when show-message (message "Toggl time entry stopped."))))
     (cl-function
      (lambda (&key error-thrown &allow-other-keys)
	      (when show-message (message "Stopping time entry failed because %s" error-thrown)))))
    (setq toggl-current-time-entry nil)))

(defun toggl-restart-last-entry ()
  (interactive)
  (if (alist-get 'tid toggl-last-time-entry)
      (let ((description (alist-get 'description toggl-last-time-entry))
            (pid (alist-get 'pid toggl-last-time-entry))
            (tid (alist-get 'tid toggl-last-time-entry)))
        (alert (concat "Restarting " description))
        (toggl-start-time-entry description pid tid t))
    (alert "No previous entry")))

(defun toggl-delete-time-entry (&optional tid show-message)
  "Delete a Toggl time entry.
By default, delete the current one."
  (interactive "ip")
  (when toggl-current-time-entry
    (setq tid (or tid (alist-get 'id (alist-get 'data toggl-current-time-entry))))
    (toggl-request-delete
     (format "time_entries/%s" tid)
     nil
     (cl-function
      (lambda (&key data &allow-other-keys)
	      (when (= tid (alist-get 'id (alist-get 'data toggl-current-time-entry)))
	        (setq toggl-current-time-entry nil))
	      (when show-message (message "Toggl time entry deleted."))))
     (cl-function
      (lambda (&key error-thrown &allow-other-keys)
	      (when show-message (message "Deleting time entry failed because %s" error-thrown)))))))

(defun toggl-get-pid (project)
  "Get PID given PROJECT's name."
  (cdr (assoc project toggl-projects)))

(defun toggl-get-tid (task)
  "Get TID given TASK's name."
  (cdr (assoc task toggl-tasks)))

(defcustom org-toggl-inherit-toggl-properties nil
  "Make org-toggl use property inheritance."
  :type 'boolean
  :group 'toggl)

(defun org-toggl-clock-in ()
  "Start a Toggl time entry based on current heading."
  (interactive)
  (let* ((heading (substring-no-properties (org-get-heading t t t t)))
	       (project (org-entry-get (point) "toggl-project" org-toggl-inherit-toggl-properties))
	       (task (org-entry-get (point) "toggl-task" org-toggl-inherit-toggl-properties))
	       (pid (org-entry-get (point) "toggl-project-id" org-toggl-inherit-toggl-properties))
         (tid (org-entry-get (point) "toggl-task-id" org-toggl-inherit-toggl-properties)))
    (setq clock-in-timer (run-with-idle-timer 2 nil (lambda()
                                                      (alert "Starting timer without task")
                                                      (toggl-start-time-entry heading t))))
    (when (and pid tid)
      (cancel-timer clock-in-timer)
      (toggl-start-time-entry heading pid tid t))))

(defun org-toggl-clock-out ()
  "Stop the running Toggle time entry."
  (toggl-stop-time-entry t))

(defun org-toggl-clock-cancel ()
  "Delete the running Toggle time entry."
  (toggl-delete-time-entry nil t))

(defun org-toggl-set-project (project)
  "Save PROJECT in the properties of the current Org headline."
  (interactive (list (completing-read "Toggl project for this headline: " toggl-projects nil t))) ; TODO: dry!
  (org-set-property "toggl-project" project)
  (org-set-property "toggl-project-id" (number-to-string (toggl-get-pid project)))
  (setq toggl-tasks nil)
  (toggl-get-project-tasks (toggl-get-pid project)))

(defun toggl-set-tasks ()
  "Save TASK  in the properties of the current Org headline."
  (interactive)
  (let* ((heading (substring-no-properties (org-get-heading t t t t)))
	       (project (org-entry-get (point) "toggl-project" org-toggl-inherit-toggl-properties))
	       (pid (toggl-get-pid project)))
    (when pid (toggl-get-project-tasks pid))))

(defun org-toggl-set-task (task)
  "Save PROJECT in the properties of the current Org headline."
  (interactive (list (completing-read "Toggl task for this headline: " toggl-tasks nil t))) ; TODO: dry!
  (org-set-property "toggl-task" task)
  (org-set-property "toggl-task-id" (number-to-string (toggl-get-tid task)))
  (call-interactively 'org-toggl-clock-in))

(define-minor-mode org-toggl-integration-mode
  "Toggle a (global) minor mode for Org/Toggl integration.
When on, clocking in and out starts and stops Toggl time entries
automatically."
  :init-value nil
  :global t
  :lighter " T-O"
  (if org-toggl-integration-mode
      (progn
	      (add-hook 'org-clock-in-hook #'org-toggl-clock-in)
	      (add-hook 'org-clock-out-hook #'org-toggl-clock-out)
	      (add-hook 'org-clock-cancel-hook #'org-toggl-clock-cancel))
    (remove-hook 'org-clock-in-hook #'org-toggl-clock-in)
    (remove-hook 'org-clock-out-hook #'org-toggl-clock-out)
    (remove-hook 'org-clock-cancel-hook #'org-toggl-clock-cancel)))

(provide 'org-toggl)
;;; org-toggl.el ends here
