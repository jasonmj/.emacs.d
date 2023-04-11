(use-package alert
  :ensure t
  :commands (alert)
  :init
  (setq alert-fade-time 10
        alert-default-style 'notifications))

(use-package app-launcher
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher")
  :bind (("C-s-SPC" . app-launcher-run-app)))

(use-package blamer
  :ensure t
  :bind (("M-I" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-type 'posframe-popup)
  (blamer-show-avatar-p nil)
  (blamer-idle-time 2)
  (blamer-min-offset 70)
  (blamer-posframe-configurations `(:left-fringe 0 :right-fringe 0 :y-pixel-offset 20 :x-pixel-offset -20 :border-width 10 :border-color ,(face-attribute 'default :background) :lines-truncate t :accept-focus nil))
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
		   :background "unspecified"
		   :height 140
		   :italic t)))
  :config
  (global-blamer-mode 1))

(key-seq-define-global "gf" 'keyboard-escape-quit)

(use-package codemetrics
  :straight (codemetrics :type git :host github :repo "jcs-elpa/codemetrics"))

(use-package devdocs :ensure t)

(global-set-key (kbd "C-s-e") 'eval-region)

(use-package git-gutter
  :ensure t
  :hook ((prog-mode . git-gutter-mode)))
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

(use-package grip-mode
  :ensure t
  :config (setq grip-preview-use-webkit nil
		grip-github-user "jasonmj"
		grip-github-password (auth-source-pick-first-password :host "api.github.com" :user "jasonmj^grip")))

(use-package fold-this
  :ensure t
  :config
  (defun expand-and-fold-this () (interactive) (expreg-expand) (fold-this (car (car (region-bounds))) (cdr (car (region-bounds)))))
  :bind (:map shell-mode-map
         ("C-<return>" . expand-and-fold-this)))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-function)
         ("C-h v" . helpful-variable)))

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

(use-package transient-posframe :ensure t :hook (magit-status-mode . transient-posframe-mode))

(use-package wgrep
  :ensure t
  :custom (wgrep-auto-save-buffer t))

(use-package which-key :ensure t :hook (after-init . which-key-mode))

(use-package writeroom-mode
  :ensure t
  :hook ((devdocs-mode . writeroom-mode))
  :config (setq writeroom-width 100))
