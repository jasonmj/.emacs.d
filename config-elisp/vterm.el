(add-to-list 'load-path "/nix/store/wwvi4a3lqvwaysm5amas4bqrxvxaxq5f-emacs-vterm-20230217.228/share/emacs/site-lisp/elpa/vterm-20230217.228")
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
