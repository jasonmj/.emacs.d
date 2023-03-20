(use-package alert
  :ensure t
  :commands (alert)
  :init
  (setq alert-fade-time 10
        alert-default-style 'notifications))

(key-seq-define-global "gf" 'keyboard-escape-quit)

(global-set-key (kbd "C-s-e") 'eval-region)

(use-package git-gutter
  :ensure t
  :hook ((c-mode . git-gutter-mode)
	 (sh-mode . git-gutter-mode)
	 (emacs-lisp-mode . git-gutter-mode)
	 (elixir-mode . git-gutter-mode)))
(with-eval-after-load 'git-gutter
  (defun git-gutter+-remote-default-directory (dir file)
    (let* ((vec (tramp-dissect-file-name file))
           (method (tramp-file-name-method vec))
           (user (tramp-file-name-user vec))
           (domain (tramp-file-name-domain vec))
           (host (tramp-file-name-host vec))
           (port (tramp-file-name-port vec)))
      (tramp-make-tramp-file-name method user domain host port dir)))

    (defun git-gutter-remote-file-path (dir file)
      (let ((file (tramp-file-name-localname (tramp-dissect-file-name file))))
	(replace-regexp-in-string (concat "\\`" dir) "" file))))

(global-hl-line-mode +1)

(mouse-avoidance-mode 'banish)
(setq mouse-avoidance-banish-position '((frame-or-window . frame)
                                        (side . right)
                                        (side-pos . 0)
                                        (top-or-bottom . top)
                                        (top-or-bottom-pos . 0)))

(add-hook 'olivetti-mode (lambda () (display-line-numbers-mode -1)))

(use-package pinentry :ensure t :config (pinentry-start))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun suspend()
  (interactive)
  (shell-command "systemctl suspend"))
(defun suspend-lock()
  (interactive)
  (shell-command "systemctl suspend")
  (shell-command "slock"))
(exwm-input-set-key (kbd "C-x >") 'suspend-lock)
(exwm-input-set-key (kbd "C-x .") 'suspend)

(use-package tramp
  :config (put 'tramp-remote-path '(tramp-own-remote-path) nil)
          (add-to-list 'tramp-remote-path "~/.asdf/shims/"))

(use-package which-key :ensure t :hook (after-init . which-key-mode))
