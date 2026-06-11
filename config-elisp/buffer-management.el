(use-package bufler
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
  (bufler-workspace-tabs-mode t)
  ;; Fix: magit-section-mode now enables font-lock, which calls
  ;; font-lock-default-unfontify-region and removes the `face' text
  ;; property that bufler uses for group heading colors.  Prevent
  ;; font-lock from unfontifying bufler buffers.
  (add-hook 'bufler-list-mode-hook
	      (lambda ()
		(setq-local font-lock-unfontify-region-function #'ignore)))
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

(defun project-magit-status ()
  "Open magit-status for the current project root."
  (interactive)
  (magit-status (project-root (project-current t))))

(defun project-ghostel ()
  "Open a new ghostel session at the current project root."
  (interactive)
  (ghostel-project (project-root (project-current t))))

(setq project-switch-commands
      '((project-find-file "Find file" ?f)
        (project-switch-to-buffer "Switch buffer" ?b)
        (project-find-dir "Find directory" ?d)
        (project-find-regexp "Find regexp" ?g)
        (project-magit-status "Magit status" ?m)
        (project-ghostel "Ghostel shell" ?s)
        (project-eshell "Eshell" ?e)))

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
