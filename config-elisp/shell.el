(use-package ansi-color
  :ensure t
  :hook (compilation-filter . colorize-compilation-buffer)
  :preface
  (autoload 'ansi-color-apply-on-region "ansi-color")
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(defun clear-shell-buffer () (interactive)
       (erase-buffer)
       (comint-send-input)
       (previous-line)
       (delete-char 1))
(define-key shell-mode-map (kbd "C-l") 'clear-shell-buffer)
(define-key shell-mode-map (kbd "C-c C-l") 'counsel-shell-history)
(add-hook 'shell-mode-hook (lambda () (compilation-shell-minor-mode 1 )))

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

(use-package bash-completion
  :ensure t
  :init
  (bash-completion-setup))

(use-package fish-completion
  :ensure t
  :config
  (setq fish-completion-fallback-on-bash-p t)
  (global-fish-completion-mode))

(defun shell-buffer (buffer-name)
  (let* ((shell-buffer-exists (member buffer-name
                                      (mapcar (lambda (buf) (buffer-name buf))
                                              (buffer-list)))))
    (if shell-buffer-exists
        (switch-to-buffer buffer-name)
      (progn
        (shell "tmp")
        (rename-buffer (concat "shell: <" buffer-name ">"))))))

(defun shell-with-name ()
  (interactive)
  (let* ((shell-buffers (seq-filter (lambda (buf) (eq (with-current-buffer buf major-mode) 'shell-mode))
				     (buffer-list)))
	 (shell-buffer-names (mapcar (lambda (buf)
					(buffer-name buf))
				      shell-buffers))
	 (buffer-name (completing-read "shell buffers: " shell-buffer-names)))
    (shell-buffer buffer-name)))
(exwm-input-set-key (kbd "M-`") 'shell-with-name)

(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))

(use-package xterm-color :ensure t)
(add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "TERM" "xterm-256color")))
(add-hook 'eshell-before-prompt-hook (lambda () (setq xterm-color-preserve-properties t)))
(setq eshell-output-filter-functions
     (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
