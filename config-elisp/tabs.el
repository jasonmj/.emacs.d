(use-package tab-bar
  :bind (("M-<tab>" . tab-bar-switch-to-next-tab)
	   ("M-S-<iso-lefttab>" . tab-bar-switch-to-prev-tab))
  :init
  (tab-bar-mode -1)
  :custom
  (tab-bar-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-tab-name-format-function 'my/tab-bar-tab-name-format-default)
  :config
  (advice-add 'tab-bar-switch-to-next-tab :after (lambda () (interactive) (switch-to-buffer (car (funcall tab-line-tabs-function)))))
  (defun my/tab-bar-tab-name-format-default (tab i)
    (concat " " (tab-bar-tab-name-format-default tab i) " ")))

(use-package tab-line
  :bind (("C-<tab>" . tab-line-switch-to-next-tab)
	   ("C-M-<tab>" . tab-line-switch-to-next-tab)
	   ("C-M-S-<tab>" . tab-line-switch-to-prev-tab)
	   ("C-<iso-lefttab>" . tab-line-switch-to-prev-tab)
	   ("C-S-<iso-lefttab>" . tab-line-switch-to-prev-tab))
  :hook ((after-init . global-tab-line-mode)
	   (global-tab-line-mode . init-tab-line-function))
  :custom
  (tab-line-new-button-show nil)
  (tab-line-switch-cycling t)
  (tab-line-close-tab-function 'kill-buffer)
  (tab-line-tab-name-function 'tab-line-tab-name-truncated-buffer)
  (tab-line-tab-name-truncated-max 35)
  (tab-line-separator "")
  (tab-line-close-button-show nil)
  (tab-line-tab-name-format-function 'my/tab-line-tab-name-format-default)
  :config
  (defun my/tabs-project-filter (buffer)
    (let* ((name (buffer-name buffer))
	     (project (project-current)))
	(if (eq project nil)
	    name
	  (if (member name (mapcar 'buffer-name (project-buffers project)))
	      buffer
	    nil))))

  (defun my/tabs-non-project-filter (buffer)
    (not (with-current-buffer buffer (project-current))))

  (defun my/tabs-shell-filter (buffer)
    (eq (with-current-buffer buffer major-mode) 'shell-mode))

  (defun my/tabs-ignore-filter (buffer)
    (let* ((name (buffer-name buffer))
	     (ignored "\\*ednc-log\\|\\*vterm\\|\\*Async-native-compile-log\\|\\*scratch\\|\\straight-process\\|\\*direnv\\|vc\\|Messages\\|copilot events\\|Dired log\\|[a-z]-shell\\|magit-process\\|straight-process\\|direnv\\|\\*gemini\\|info\\|Backtrace\\|EGLOT.+\\*"))
	(not (string-match-p ignored name))))

  (defun my/tabs-function (&optional frame)
    (let* ((buffers (if (eq (with-current-buffer (current-buffer) major-mode) 'shell-mode) (buffer-list frame) (bufler-workspace-buffers frame)))
	     (project-filtered-buffers (seq-filter 'my/tabs-project-filter buffers))
	     (non-project-filtered-buffers (seq-filter 'my/tabs-non-project-filter buffers)))
	(cond ((with-current-buffer (current-buffer) (derived-mode-p 'shell-mode))
	       (seq-filter 'my/tabs-shell-filter buffers))
	      ((with-current-buffer (current-buffer) (project-current))
	       (seq-filter 'my/tabs-ignore-filter project-filtered-buffers))
	      (t
	       (seq-filter 'my/tabs-ignore-filter non-project-filtered-buffers)))))

  (defun my/tab-line-tab-name-format-default (tab tabs)
    "Default function to use as `tab-line-tab-name-format-function', which see."
    (let* ((buffer-p (bufferp tab))
	     (selected-p (if buffer-p
			     (eq tab (window-buffer))
			   (cdr (assq 'selected tab))))
	     (name (if buffer-p
		       (funcall tab-line-tab-name-function tab tabs)
		     (cdr (assq 'name tab))))
	     (face (if selected-p
		       (if (mode-line-window-selected-p)
			   'tab-line-tab-current
			 'tab-line-tab)
		     'tab-line-tab-inactive)))
	(dolist (fn tab-line-tab-face-functions)
	  (setf face (funcall fn tab tabs face buffer-p selected-p)))
	(apply 'propertize
	       (concat " " (propertize (string-replace "%" "%%" name) ;; (bug#57848)
				       'keymap tab-line-tab-map
				       'help-echo (if selected-p "Current tab"
						    "Click to select tab")
				       ;; Don't turn mouse-1 into mouse-2 (bug#49247)
				       'follow-link 'ignore)
		       (or (and (or buffer-p (assq 'buffer tab) (assq 'close tab))
				tab-line-close-button-show
				(not (eq tab-line-close-button-show
					 (if selected-p 'non-selected 'selected)))
				tab-line-close-button)
			   "") " ")
	       `(
		 tab ,tab
		 ,@(if selected-p '(selected t))
		 face ,face
		 mouse-face tab-line-highlight))))
  (key-seq-define-global "jk" 'tab-line-switch-to-next-tab)
  (key-seq-define-global "fd" 'tab-line-switch-to-prev-tab)
  (key-chord-define-global "fd" 'tab-line-switch-to-prev-tab)
  (defun my/tab-line-close-tab ()
    (interactive)
    (tab-line-switch-to-prev-tab)
    (tab-line-switch-to-next-tab)
    (my-kill-this-buffer))
  (key-seq-define-global "gw" 'my/tab-line-close-tab)
  (defun init-tab-line-function () (setq tab-line-tabs-function 'my/tabs-function)))
(add-to-list 'after-init-hook 'init-tab-line-function)
