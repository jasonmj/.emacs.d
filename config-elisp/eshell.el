(use-package aweshell :straight (aweshell :type git :host github :repo "manateelazycat/aweshell"))

(require 'eshell)
(require 'em-smart)
(setq eshell-banner-message "")
(setq eshell-visual-commands '("bat" "less" "more" "htop" "man" "vim" "fish"))
(setq eshell-destroy-buffer-when-process-dies t)
(setq eshell-cmpl-autolist t)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(setq eshell-history-size 10000)
(add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)

(add-hook 'eshell-mode-hook (lambda () (interactive) (auto-highlight-symbol-mode -1)))

(defun custom-eshell-init()
  (auto-highlight-symbol-mode 1)
  (setq buffer-face-mode-face '(:height 150))
  (buffer-face-mode)
  (define-key eshell-mode-map (kbd "C-l") 'eshell-clear)
  (define-key eshell-mode-map (kbd "C-c C-;") 'counsel-esh-history)
  (define-key eshell-mode-map (kbd "<M-tab>") 'tab-bar-switch-to-next-tab)
  (setq-local truncate-lines -1))
(add-hook 'eshell-mode-hook 'custom-eshell-init)

(defun eshell-clear ()
  (interactive)
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)
    (previous-line)
    (delete-char 1)))

(defun docker-stop-all ()
  (interactive)
  (mapcar
   (lambda(id)
     (call-process-shell-command (concat "docker stop " id "&")) nil 0)
   (butlast (split-string (shell-command-to-string "docker ps -q") "\n") 1)))

(use-package esh-autosuggest
  :commands esh-autosuggest
  :ensure t
  :config
  :hook
  (eshell-mode . esh-autosuggest-mode))

(use-package eshell-prompt-extras :ensure t
  :config
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :ensure t
  :hook (esh-mode . eshell-syntax-highlighting-global-mode))

(defun eshell-buffer (buffer-name)
  (let* ((eshell-buffer-exists (member buffer-name
                                       (mapcar (lambda (buf)
                                                 (buffer-name buf))
                                               (buffer-list)))))
    (if eshell-buffer-exists
        (switch-to-buffer buffer-name)
      (progn
        (eshell 99)
        (rename-buffer (concat "Aweshell: <" buffer-name ">"))))))

(defun eshell-with-name ()
  (interactive)
  (let* ((eshell-buffers (seq-filter (lambda (buf)
				       (string-match-p "Aweshell" (buffer-name buf)))
				     (buffer-list)))
	 (eshell-buffer-names (mapcar (lambda (buf)
					(buffer-name buf))
				      eshell-buffers))
	 (buffer-name (completing-read "Aweshell buffers: " eshell-buffer-names)))
    (eshell-buffer buffer-name)))
(exwm-input-set-key (kbd "C-`") 'eshell-with-name)

(require 'em-tramp)
(add-hook 'eshell-mode-hook (lambda ()
  (setq eshell-prefer-lisp-functions t)
  (setq password-cache t)
  (setq password-cache-expiry 900)))
