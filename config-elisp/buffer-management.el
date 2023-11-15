(use-package bufler
  :ensure t
  :custom
  (bufler-filter-buffer-name-regexps '("\\*Compile-Log\\*"
				       "\\*Backtrace\\*"
				       "\\*direnv\\*"
				       "\\*Disabled Command\\*"
				       "\\*Org [^z-a]+Output\\*"
				       "\\*xref\\*"
				       "\\*xob\\*"
				       "\\*ednc-log\\*"
				       "\\*straight-process\\*"
				       "\\*blamer\\*"
				       "\\*Messages\\*"
				       "\\*Warnings\\*"))
  :config
  (key-seq-define-global "bf" 'bufler)
  (defun my/bufler-workspace-focus-buffer (&optional buffer)
    (run-with-idle-timer 0.001 nil (lambda () (interactive) (bufler-workspace-focus-buffer (current-buffer)))))
  (add-to-list 'window-selection-change-functions 'my/bufler-workspace-focus-buffer)
  (advice-add 'bufler-workspace-mode-lighter :override (lambda () ""))
  (bufler-workspace-mode t)
  (bufler-workspace-tabs-mode t)
  :hook ((exwm-update-title . my/bufler-workspace-focus-buffer)
	 (exwm-manage-finish . my/bufler-workspace-focus-buffer)
	 (exwm-workspace-switch . my/bufler-workspace-focus-buffer)
	 (kill-buffer .my/bufler-workspace-focus-buffer)))

(key-seq-define-global "xb" 'list-buffers)

(defun my/kill-this-buffer (&optional arg)
  (interactive "P")
  (pcase arg
    ('4 (call-interactively #'kill-buffer))
    (_ (kill-buffer (current-buffer)))))
(global-set-key (kbd "C-x k") 'my/kill-this-buffer)
(key-seq-define-global "gw" 'my/kill-this-buffer)
(key-seq-define-global "fw" (lambda () (interactive)
			      (if (eq (length (window-list)) 1)
				  (my/kill-this-buffer)
				(kill-buffer-and-window))))

(defun hs-process-filter (process output)
  (mapcar (lambda (str)
	    (if (string-match "\"windows\":" str)
		(setq mac-windows-list (nth 1 (split-string str "36m"))))) (split-string output "")))

(when (eq system-type 'darwin)
  (setq hs-process (make-process :name "hs" :command '("hs") :filter 'hs-process-filter)))
(setq mac-windows-list "")

(defun get-windows-string ()
  (process-send-string hs-process "spoon.hs_select_window:list_mac_windows()\n")
  (accept-process-output hs-process 2)
  mac-windows-list)

(defun get-mac-window-list ()
  (let* ((windows-string (get-windows-string))
	 (json-string windows-string)
	 (windows-hash-table (gethash "windows" (json-parse-string json-string)))
	 (window-list '()))
    (mapcar (lambda (v)
	      (let* ((app (gethash "app" v))
		     (title (gethash "title" v))
		     (id (gethash "id" v)))
		(add-to-list 'window-list `(,(concat app " - " title) . ,id))))
	    windows-hash-table)
    window-list))

(defun switch-mac-window ()
  (interactive)
  (let* ((window-list (get-mac-window-list))
	 (window-choice (cdr (assoc (completing-read "Switch window: " window-list) window-list))))
    (focus-mac-window window-choice)))

(defun focus-mac-window (window-id)
  (let* ((command (concat "spoon.hs_select_window:switch_to_mac_window(\"" window-id  "\")\n")))
    (process-send-string hs-process command)))

(defun delayed-switch-mac-window ()
  (interactive)
  (run-with-idle-timer 0.2 nil 'switch-mac-window)
  (get-windows-string))

(emacs-set-key (kbd "C-SPC") 'delayed-switch-mac-window)

(defun get-emacs-buffer-list ()
  (cdr (mapcar #'string-trim (mapcar #'buffer-name (delq nil (delete-dups
							      (flatten-tree (mapcar (lambda (group)
										      (unless (equal (car group) "\*Special")
											(mapcar (lambda (buffer-or-buffers)
												  (let* ((group-buffers (if (eq (type-of buffer-or-buffers) 'buffer) buffer-or-buffers (car (cdr buffer-or-buffers))))
													 (clean-group-buffers (if (eq (type-of group-buffers) 'buffer)
																  group-buffers
																(delq nil (delete-dups group-buffers))))
													 (buffer-list '()))
												    (if (eq (type-of clean-group-buffers) 'buffer) clean-group-buffers
												      (mapcar (lambda (item) (if (eq (type-of item) 'buffer) item)) clean-group-buffers)))) (cdr group)))) (bufler-buffers)))))))))

(key-seq-define-global "xv" (lambda () (interactive) (revert-buffer t t)))

(global-set-key (kbd "C-s") 'save-buffer)

(defun centered-cursor-reset ()
  (interactive)
  (centered-cursor-mode -1)
  (centered-cursor-mode 1))
(defun my/text-scale-increase ()
  (interactive)
  (text-scale-increase 1)
  (run-with-idle-timer 0.01 nil 'centered-cursor-reset))
(defun my/text-scale-decrease ()
  (interactive)
  (text-scale-decrease 1)
  (run-with-idle-timer 0.01 nil 'centered-cursor-reset))
(global-set-key (kbd "C-=") 'my/text-scale-increase)
(global-set-key (kbd "C-+") 'my/text-scale-increase)
(global-set-key (kbd "C--") 'my/text-scale-decrease)
