(use-package tab-bar
  :bind (("M-<tab>" . tab-bar-switch-to-next-tab)
	 ("M-S-<iso-lefttab>" . tab-bar-switch-to-prev-tab))
  :init
  (tab-bar-mode -1)
  :config
  (advice-add 'tab-bar-switch-to-next-tab :after (lambda () (interactive) (switch-to-buffer (car (funcall tab-line-tabs-function)))))
  (defun my/tab-bar-tab-name-format-default (tab i)
    (concat " " (tab-bar-tab-name-format-default tab i) " "))
  (setq tab-bar-show nil
	tab-bar-new-button-show nil
	tab-bar-close-button-show nil	
	tab-bar-tab-name-format-function 'my/tab-bar-tab-name-format-default))

(use-package tab-line
  :bind (("C-<tab>" . tab-line-switch-to-next-tab)
	 ("C-S-<iso-lefttab>" . tab-line-switch-to-prev-tab))
  :init
  (setq global-tab-line-mode t)
  :config
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
  (setq tab-line-new-button-show nil
	tab-line-switch-cycling t
	tab-line-tab-name-function 'tab-line-tab-name-truncated-buffer
	tab-line-tab-name-truncated-max 25
	tab-line-separator ""
	tab-line-close-button-show nil
	tab-line-tab-name-format-function 'my/tab-line-tab-name-format-default)
  (key-seq-define-global "jk" 'tab-line-switch-to-next-tab)
  (key-seq-define-global "fd" 'tab-line-switch-to-prev-tab))
