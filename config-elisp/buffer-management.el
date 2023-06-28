(use-package bufler
  :ensure t
  :custom
  (bufler-filter-buffer-name-regexps '("\\*Compile-Log\\*"
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
