(use-package bufler
  :ensure t
  :straight (:type git :host github :repo "alphapapa/bufler.el")
  :custom
  (bufler-filter-buffer-name-regexps '("\\*Compile-Log\\*"
					 "\\*Backtrace\\*"
					 "\\*direnv\\*"
					 "\\*Disabled Command\\*"
					 "\\*copilot events\\*"
					 "\\*Org [^z-a]+Output\\*"
					 "\\*xref\\*"
					 "\\*xob\\*"
					 "\\*ednc-log\\*"
					 "\\*straight-process\\*"
					 "\\*blamer\\*"
					 "\\*scratch\\*"
					 "\\*Messages\\*"
					 "\\*Warnings\\*"))
  :config
  (key-chord-define-global "bf" 'bufler-list)
  (defun my/bufler-workspace-focus-buffer (&optional buffer)
    (run-with-idle-timer 0.001 nil (lambda () (interactive) (bufler-workspace-focus-buffer (current-buffer)))))
  (add-to-list 'window-selection-change-functions 'my/bufler-workspace-focus-buffer)
  (advice-add 'bufler-workspace-mode-lighter :override (lambda () ""))
  (bufler-workspace-mode t)
  (load "bufler-workspace-tabs.el")
  (bufler-workspace-workspaces-as-tabs-mode t)
  :hook ((kill-buffer .my/bufler-workspace-focus-buffer)))

(key-seq-define-global "xb" 'list-buffers)

(defun my-kill-this-buffer (&optional arg)
  (interactive "P")
  (pcase arg
    ('4 (call-interactively #'kill-buffer))
    (_ (kill-buffer (current-buffer)))))
(defun my-kill-this-window ()
  (interactive)
  (if (eq (length (window-list)) 1)
	(my-kill-this-buffer)
    (kill-buffer-and-window)))
(global-set-key (kbd "C-x k") 'my-kill-this-buffer)
(global-set-key (kbd "C-w") 'my-kill-this-buffer)
(key-seq-define-global "gw" 'my-kill-this-buffer)
(key-seq-define-global "fw" 'my-kill-this-window)
(key-chord-define-global "fw" 'my-kill-this-window)

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

(if (eq system-type 'darwin)
    (emacs-set-key (kbd "C-SPC") 'delayed-switch-mac-window)
  (emacs-set-key (kbd "C-SPC") 'consult-project-buffer))

(defun get-emacs-buffer-list ()
  (seq-filter 'filter-emacs-buffers-for-hammerspoon
	    (append `(,(with-current-buffer (buffer-name)))
		    (cdr (mapcar #'string-trim (mapcar #'buffer-name (delq nil (delete-dups
										(flatten-tree (mapcar (lambda (group)
													(unless (equal (car group) "\*Special")
													  (mapcar (lambda (buffer-or-buffers)
														    (let* ((group-buffers (if (eq (type-of buffer-or-buffers) 'buffer) buffer-or-buffers (car (cdr buffer-or-buffers))))
															   (clean-group-buffers (if (eq (type-of group-buffers) 'buffer)
																		    group-buffers
																		  (delete-dups group-buffers)))
															   (buffer-list '()))
														      (if (eq (type-of clean-group-buffers) 'buffer) clean-group-buffers
															(mapcar (lambda (item) (if (eq (type-of item) 'buffer) item)) clean-group-buffers)))) (cdr group)))) (bufler-buffers)))))))))))

(defun filter-emacs-buffers-for-hammerspoon (buf)
  (not (string-match-p "magit-process:\\| *server*" (if (bufferp buf) (buffer-name buf) buf))))

(emacs-set-key (kbd "M-S-SPC") 'project-switch-project)

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

(when (not (eq system-type 'darwin))
  ;; Workspaces
  (emacs-set-key (kbd "s-0") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch workspace 10")))
  (emacs-set-key (kbd "s-1") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch workspace 1")))
  (emacs-set-key (kbd "s-2") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch workspace 2")))
  (emacs-set-key (kbd "s-3") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch workspace 3")))
  (emacs-set-key (kbd "s-4") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch workspace 4")))
  (emacs-set-key (kbd "s-5") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch workspace 5")))
  (emacs-set-key (kbd "s-6") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch workspace 6")))
  (emacs-set-key (kbd "s-7") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch workspace 7")))
  (emacs-set-key (kbd "s-8") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch workspace 8")))
  (emacs-set-key (kbd "s-9") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch workspace 9")))

  ;; Move to workspace
  (emacs-set-key (kbd "C-s-0") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch movetoworkspace 10")))
  (emacs-set-key (kbd "C-s-1") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch movetoworkspace 1")))
  (emacs-set-key (kbd "C-s-2") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch movetoworkspace 2")))
  (emacs-set-key (kbd "C-s-3") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch movetoworkspace 3")))
  (emacs-set-key (kbd "C-s-4") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch movetoworkspace 4")))
  (emacs-set-key (kbd "C-s-5") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch movetoworkspace 5")))
  (emacs-set-key (kbd "C-s-6") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch movetoworkspace 6")))
  (emacs-set-key (kbd "C-s-7") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch movetoworkspace 7")))
  (emacs-set-key (kbd "C-s-8") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch movetoworkspace 8")))
  (emacs-set-key (kbd "C-s-9") (lambda() (interactive) (call-process-shell-command "hyprctl dispatch movetoworkspace 9")))
  )
