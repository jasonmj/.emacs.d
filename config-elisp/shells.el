(use-package eshell
  :bind (:map eshell-mode-map
	      ("C-l" . eshell-clear)
	      ("<M-tab>" . tab-bar-switch-to-next-tab))
  :config
  (require 'em-smart)
  (require 'em-tramp)
  (defun eshell-clear ()
    (interactive)
    (let ((eshell-buffer-maximum-lines 0))
      (eshell-truncate-buffer)
      (previous-line)
      (delete-char 1)))
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)
  :custom 
  (eshell-banner-message "")
  (eshell-visual-commands '("bat" "less" "more" "htop" "man" "vim" "fish"))
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-cmpl-autolist t)
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t)
  (eshell-history-size 10000)
  :hook ((eshell-mode-hook . (lambda ()
			       (setq eshell-prefer-lisp-functions t
				     password-cache t
				     password-cache-expiry 900)
			       (setq-local truncate-lines -1)
			       (setenv "TERM" "xterm-256color")))))

(use-package eshell-prompt-extras :ensure t
  :config
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
	  eshell-prompt-function 'epe-theme-lambda)))

(use-package eshell-syntax-highlighting
  :ensure t
  :after eshell-mode
  :hook (eshell-mode . eshell-syntax-highlighting-global-mode))

(defun eshell-buffer (buffer-name)
  (let* ((eshell-buffer-exists (member buffer-name
				       (mapcar (lambda (buf)
						 (buffer-name buf))
					       (buffer-list)))))
    (if eshell-buffer-exists
	(switch-to-buffer buffer-name)
      (progn
	(eshell 99)
	(rename-buffer (concat "Eshell: <" buffer-name ">"))))))

(defun eshell-with-name ()
  (interactive)
  (let* ((eshell-buffers (seq-filter (lambda (buf) (eq (with-current-buffer buf major-mode) 'eshell-mode))
				     (buffer-list)))
	 (eshell-buffer-names (mapcar (lambda (buf)
					(buffer-name buf))
				      eshell-buffers))
	 (buffer-name (completing-read "Eshell buffers: " eshell-buffer-names)))
    (eshell-buffer buffer-name)))
(exwm-input-set-key (kbd "C-`") 'eshell-with-name)

(use-package xterm-color
  :ensure t
  :config
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  :custom
  (eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  :hook ((eshell-mode . (lambda () (setenv "TERM" "xterm-256color")))
	 (eshell-before-prompt . (lambda () (setq xterm-color-preserve-properties t)))))

(use-package shell
  :bind (("C-l" . clear-shell-buffer))
  :config
  (defun clear-shell-buffer () (interactive)
       (erase-buffer)
       (comint-send-input)
       (previous-line)
       (delete-char 1))
  :hook ((shell-mode . compilation-shell-minor-mode)))

(defun clean-compilation-filename (filename)
  (string-trim
       (replace-regexp-in-string "\\(\*\* \\|┃\\)" ""
			     (replace-regexp-in-string "\([^\"]+?\)" ""
						       (string-trim filename)))))
(defun compilation-find-file-fixer (orig-fun marker filename &rest args)
  (message (clean-compilation-filename filename))
  (apply orig-fun marker
	 (clean-compilation-filename filename)
	 args))
(advice-add 'compilation-find-file :around #'compilation-find-file-fixer)

(defun return-to-shell-mode () (interactive) (with-current-buffer (current-buffer) (shell-mode)))

(defun shell-buffer (buffer-name)
  (let* ((shell-buffer-exists (member buffer-name
				      (mapcar (lambda (buf) (buffer-name buf))
					      (buffer-list)))))
    (if shell-buffer-exists
	(switch-to-buffer buffer-name)
      (progn
	(shell "tmp")
	(rename-buffer (concat "Shell: <" buffer-name ">"))))))

(defun shell-with-name ()
  (interactive)
  (let* ((shell-buffers (seq-filter (lambda (buf) (eq (with-current-buffer buf major-mode) 'shell-mode))
				     (buffer-list)))
	 (shell-buffer-names (mapcar (lambda (buf)
					(buffer-name buf))
				      shell-buffers))
	 (buffer-name (completing-read "Shell buffers: " shell-buffer-names)))
    (shell-buffer buffer-name)))
(exwm-input-set-key (kbd "M-`") 'shell-with-name)

(use-package sticky-shell
  :straight (:type git :host github :repo "andyjda/sticky-shell")
  :hook (shell-mode . sticky-shell-mode)
  :custom
  (defun clear-shell-buffer-to-last-prompt () (interactive)
       (end-of-buffer)
       (set-mark (point))
       (comint-previous-prompt 1)
       (next-line)
       (delete-active-region))
  :bind (:map shell-mode-map
	      ("C-S-l" . clear-shell-buffer-to-last-prompt)))

(defun syntax-overlay-region ()
  (interactive)
  (unless (region-active-p)
    (user-error "No region active"))
  (let* ((lang-mode 'elixir-mode)
	 (body-start (region-beginning))
	 (body-end (region-end))
	 (string (buffer-substring-no-properties body-start body-end))
	 (buf (current-buffer))
	 (pos 0)
	 (props)
	 (overlay)
	 (propertized-text))
    (if (fboundp lang-mode)
	(progn
	  (setq propertized-text
		(with-current-buffer
		    (get-buffer-create
		     (format " *fontification:%s*" lang-mode))
		  (let ((inhibit-modification-hooks nil)
			(inhibit-message t))
		    (erase-buffer)
		    ;; Additional space ensures property change.
		    (insert string " ")
		    (funcall lang-mode)
		    (font-lock-ensure))
		  (buffer-string)))
	  (while (< pos (length propertized-text))
	    (setq props (text-properties-at pos propertized-text))
	    (setq overlay (make-overlay (+ body-start pos)
					(+ body-start (1+ pos))
					buf))
	    (overlay-put overlay 'face (plist-get props 'face))
	    (setq pos (1+ pos))))
      (message "%s not found" lang-mode))))

(add-to-list 'load-path (concat "/etc/links/vterm/" (string-trim (shell-command-to-string "ls /etc/links/vterm/"))))
(require 'vterm)
(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")
(defun vterm-startup ()
  (define-key vterm-mode-map (kbd "C-c C-t") 'vterm-copy-mode)
  (define-key vterm-mode-map (kbd "C-p") 'vterm-copy-mode)
  (define-key vterm-mode-map (kbd "C-c C-\\") 'vterm-send-C-c)
  (define-key vterm-mode-map (kbd "M-p") 'vterm-send-up)
  (define-key vterm-mode-map (kbd "M-n") 'vterm-send-down)
  (define-key vterm-mode-map (kbd "C-z") 'vterm-undo)
  (define-key vterm-copy-mode-map (kbd "M-n") 'vterm-next-prompt)
  (define-key vterm-copy-mode-map (kbd "C-z") 'vterm-undo)
  (define-key vterm-copy-mode-map (kbd "M-p") 'vterm-previous-prompt)
  (define-key vterm-copy-mode-map (kbd "C-l") (lambda () (interactive) (vterm-copy-mode -1) (vterm-clear)))
  (with-eval-after-load 'centered-cursor-mode
        (add-hook 'after-change-major-mode-hook
            (lambda ()
              (centered-cursor-mode 0))
            :append
            :local))
  (setq-local global-hi-lock-mode nil)
  (setq-local global-hl-line-mode nil))
(add-hook 'vterm-mode-hook 'vterm-startup)

(use-package vterm-toggle
  :ensure t
  :config
  (exwm-input-set-key (kbd "C-s-t") 'vterm-toggle))
